#ifndef COLOR_SPACE_CONVERTER_HPP
#define COLOR_SPACE_CONVERTER_HPP

#include "copywrite.hpp"

class ColorSpaceConverter
{
public:
    static uint32_t hsvaToRgba( uint32_t hsv);

    static uint32_t rgbaToHsva( uint32_t rgb);

    static uint32_t rgbaToHsla( uint32_t rgba);

    static uint32_t hslaToRgba( uint32_t hsla);

    static XyZColor xyzFromRgb( uint32_t color);

    static uint32_t xyzToRgb( XyZColor color);

private:
    static float hueToSpace( float p, float q, float t);
};

#endif