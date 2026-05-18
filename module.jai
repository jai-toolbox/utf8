// UTF-8 means "Unicode Transformation Format, 8-bit". The 8 refers to its code
// unit size: UTF-8 text is stored as a stream of 8-bit bytes. It is still a
// variable-length encoding; each Unicode character is stored as 1 to 4 bytes.
//
// ASCII came first historically, and every ASCII character fits in one byte.
// UTF-8 preserves that representation exactly: ASCII bytes are valid UTF-8
// bytes unchanged. This made UTF-8 compatible with a large amount of older text,
// file formats, and software while still allowing all of Unicode to be encoded.
//
// ASCII characters use one byte with a leading 0 bit. Non-ASCII characters
// start with a leading byte that announces the total byte count, followed by
// continuation bytes that all start with 10.
//
// In the patterns below, fixed 0/1 bits are marker bits used to identify the
// byte type and character length. The x bits are payload bits that store the
// Unicode code point's data. The leading byte is not only metadata: after its
// marker bits, it also carries payload bits. Each continuation byte reserves its
// first two bits as 10 and carries six payload bits.
//
//   0xxxxxxx                            -> 1 byte,  7 payload bits
//   110xxxxx 10xxxxxx                   -> 2 bytes, 11 payload bits
//   1110xxxx 10xxxxxx 10xxxxxx          -> 3 bytes, 16 payload bits
//   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx -> 4 bytes, 21 payload bits
//
// Examples:
//   "A"  U+0041 -> 01000001
//                  0 + 1000001
//
//   "£"  U+00A3 -> 11000010 10100011
//                  110 + 00010, 10 + 100011
//                  payload: 00010 100011 = 0b00010100011 = 0x00A3
//
//   "€"  U+20AC -> 11100010 10000010 10101100
//                  1110 + 0010, 10 + 000010, 10 + 101100
//                  payload: 0010 000010 101100 = 0b0010000010101100 = 0x20AC

// startfold binary

#import "Basic";

binary_digit_count :: (value: u8) -> int {
    if value == 0 return 1;

    count := 0;
    while value != 0 {
        value >>= 1;
        count += 1;
    }

    return count;
}

has_binary_prefix :: inline (value: u8, $prefix: u8) -> bool {
    prefix_digit_count := binary_digit_count(prefix);
    return (value >> (8 - prefix_digit_count)) == prefix;
}

// endfold binary

is_continuation_byte :: inline (byte: u8) -> bool {
    return has_binary_prefix(byte, 0b10);
}

is_leading_byte :: inline (byte: u8) -> bool {
    success, byte_count := count_character_bytes(byte);
    return success;
}

is_whitespace_byte :: inline (byte: u8) -> bool {
    if byte == {
        case #char "\0"; #through;
        case #char "\t"; #through;
        case #char "\n"; #through;
        case #char "\x0B"; #through;
        case #char "\x0C"; #through;
        case #char "\r"; #through;
        case #char " ";
            return true;
    }

    return false;
}

count_character_bytes :: inline (character_leading_byte: u8) -> success: bool, byte_count: int {
    if has_binary_prefix(character_leading_byte, 0b0)     return true, 1;
    if has_binary_prefix(character_leading_byte, 0b110)   return true, 2;
    if has_binary_prefix(character_leading_byte, 0b1110)  return true, 3;
    if has_binary_prefix(character_leading_byte, 0b11110) return true, 4;
    
    return false, 0;
}

// Returns the string (using same str.data) truncated to the provided length.
truncate :: (str: string, length: int) -> string {
    
    if str.data == null     then return "";
    assert(length >= 0);

    valid, error_index := is_valid(str);
    assert(valid);

    if str.count < length   then return .{str.count, str.data};
    
    data    := str.data;
    count   := str.count;

    if length == 0 return .{0, str.data};

    idx := length;
    while (idx > 0 && is_continuation_byte(data[idx - 1])) idx -= 1;
    continuation_bytes := length - idx;
    leading_byte_index := idx - 1;

    assert(leading_byte_index >= 0);
    assert(is_leading_byte(data[leading_byte_index]));

    success, character_bytes := count_character_bytes(data[leading_byte_index]);
    assert(success);

    expected_continuation_bytes := character_bytes - 1;
    if continuation_bytes != expected_continuation_bytes {
        length = leading_byte_index;
    }
    
    return .{length, str.data};
}

is_empty :: (str: string) -> bool {
    for str {
        if !is_whitespace_byte(it) return false;
    }

    return true;
}

count_characters :: (str: string, $is_null_terminated := false) -> int {
    characters := 0;
    idx := 0;
    
    #if is_null_terminated {
        while idx < str.count && str[idx] != 0 {
            success, character_bytes := count_character_bytes(str[idx]);
            if !success break;
            idx += character_bytes;
            characters += 1;
        }
    }
    else {
        while idx < str.count {
            success, character_bytes := count_character_bytes(str[idx]);
            if !success break;
            idx += character_bytes;
            characters += 1;
        }
    }
    
    return characters;
}

// Deletes character by its index, and moves tail data to take its place.
delete_character :: (str: *string, character_idx: int) -> success := true {
    found, buffer_idx := get_byte_index(str.*, character_idx);

    if !found return false;
    
    valid_leading_byte, bytes_to_delete := count_character_bytes(str.data[buffer_idx]);
    if !valid_leading_byte return false;
    
    for buffer_idx..str.count-1-bytes_to_delete {
        str.data[it] = str.data[it+bytes_to_delete];
    }
    for str.count-bytes_to_delete..str.count-1 {
        str.data[it] = 0;
    }
    
    str.count -= bytes_to_delete;
    return;
}

// Jai strings are byte arrays, so indexing a string gives a byte, not a full
// Unicode character. For plain ASCII this lines up with intuition: in "hello",
// s[1] is the byte for "e", and that byte is also the second character.
//
// UTF-8 characters can be multiple bytes, so byte index and character index can
// diverge. In "h€llo", the bytes are:
//
//   h        €                          l        l        o
//   01101000 11100010 10000010 10101100 01101100 01101100 01101111
//
// Here character index 1 is "€", but byte index 1 is only the first byte of the
// three-byte sequence for "€". This helper walks complete UTF-8 characters and
// returns the byte index where the requested character begins.
get_byte_index :: (str: string, character_index: int) -> success: bool, buffer_index: int {
    buff_idx := 0;
    char_idx := 0;
    while buff_idx < str.count {
        if char_idx == character_index return true, buff_idx;
        success, character_bytes := count_character_bytes(str[buff_idx]);
        if !success return false, -1;
        buff_idx += character_bytes;
        char_idx += 1;
    }
    return false, -1;
}

// Scans the string for UTF8 encoding errors.
is_valid :: (str: string) -> is_valid := true, error_index: int = -1 {
    remaining_bytes := 0;

    for 0..str.count-1 {
        is_continuation := is_continuation_byte(str[it]);
        
        if (is_continuation && remaining_bytes == 0) || (!is_continuation && remaining_bytes > 0) then return false, it;
        
        if is_continuation {
            remaining_bytes -= 1;
            continue;
        }
        
        success, character_bytes := count_character_bytes(str[it]);
        if !success return false, it;
        remaining_bytes = character_bytes - 1;
    }

    if remaining_bytes > 0 return false, str.count;

    return;
}
