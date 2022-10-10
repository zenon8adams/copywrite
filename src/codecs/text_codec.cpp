#include <string>
#include <cstdint>
#include "copywrite.hpp"
#include "codecs/text_codec.hpp"

size_t TextCodec::byteCount( uint8_t c)
{
    // Check if leading byte is an ASCII character
    if(( c & 0x80) != 0x80)
        return 1;
    // If not, reverse the byte
    c =   ( c & 0x55) << 1 | ( c & 0xAA) >> 1;
    c =   ( c & 0x33) << 2 | ( c & 0xCC) >> 2;
    c = (( c & 0x0F) << 4  | ( c & 0xFF) >> 4);
    // Mask out the continuous runs of ones in the leading byte( now trailing)
    c = ( c ^ ( c + 1)) >> 1;
    // Count the remaining bits in the byte.
    return popcount8( c);
}

uint32_t TextCodec::collate( uint8_t *str, size_t idx, uint8_t count)
{
    if( count == 1 )
        return str[ idx];

    uint32_t copy = count;
    while( copy > 1)
        str[ idx + --copy] &= 0x3FU;

    str[ idx] &= 0xFFU >> count;
    count -= 1;
    size_t i = ( count << 2u ) + ( count << 1u);

    uint32_t value = 0;
    while( (int8_t)count >= 0 )
    {
        value += str[ idx++ ] << i;
        --count;
        i -= 6;
    }

    return value;
}

std::wstring TextCodec::toWString( std::string str)
{
    std::wstring wsRep;
    for( size_t i = 0uL; i < str.size(); )
    {
        size_t byte_count = byteCount( str[ i]);
        wsRep += static_cast<wchar_t>( collate( ( uint8_t *)&str[ 0], i, byte_count));
        i += byte_count;
    }

    return wsRep;
}