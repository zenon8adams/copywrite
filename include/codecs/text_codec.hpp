#ifndef TEXT_CODEC_HPP
#define TEXT_CODEC_HPP

class TextCodec
{
public:
    static size_t byteCount( uint8_t c);

    static uint32_t collate( uint8_t *str, size_t idx, uint8_t count);

    static std::wstring toWString( std::string str);
};

#endif