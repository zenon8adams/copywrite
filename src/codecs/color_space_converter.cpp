#include <cstdint>
#include <algorithm>
#include <cmath>
#include "codecs/color_space_converter.hpp"

uint32_t ColorSpaceConverter::hsvaToRgba( uint32_t hsv)
{
    uint32_t region, p, q, t, remainder,
            h = RED( hsv),
            s = GREEN( hsv),
            v = BLUE( hsv),
            a = ALPHA( hsv);

    if ( s == 0)
        return RGBA( v, v, v, a);

    region = h / 43;
    remainder = ( h - ( region * 43)) * 6;

    p = ( v * ( 0xFFu - s)) >> 8u;
    q = ( v * ( 0xFFu - ((s * remainder) >> 8u))) >> 8u;
    t = ( v * ( 0xFFu - ((s * (0xFFu - remainder)) >> 8u))) >> 8u;

    switch ( region)
    {
        case 0:
            return RGBA( v, t, p, a);
        case 1:
            return RGBA( q, v, p, a);
        case 2:
            return RGBA( p, v, t, a);
        case 3:
            return RGBA( p, q, v, a);
        case 4:
            return RGBA( t, p, v, a);
        default:
            return RGBA( v, p, q, a);
    }
}

uint32_t ColorSpaceConverter::rgbaToHsva( uint32_t rgb)
{
    uint32_t rgb_min, rgb_max, hsv{},
            r = RED( rgb),
            g = GREEN( rgb),
            b = BLUE( rgb),
            a = ALPHA( rgb);

    rgb_max = MAX( r, MAX( g, b));
    rgb_min = MIN( r, MIN( g, b));

    hsv = rgb_max << 8u;
    if ( hsv == 0)
        return hsv | a;

    hsv |= ( 0xFFu * (( int32_t)( rgb_max - rgb_min)) / ( hsv >> 8u)) << 16u;
    if (( hsv >> 16u & 0xFFu) == 0)
        return hsv | a;

    if( rgb_max == r)
        hsv |= ( uint32_t)(( uint8_t)( 0 + 43 * ( int32_t)( g - b) / ( int32_t)( rgb_max - rgb_min))) << 24u;
    else if( rgb_max == g)
        hsv |= ( uint32_t)(( uint8_t)(  85 + 43 * ( int32_t)( b - r) / ( int32_t)( rgb_max - rgb_min))) << 24u;
    else
        hsv |= ( uint32_t)(( uint8_t)( 171 + 43 * ( int32_t)( r - g) / ( int32_t)( rgb_max - rgb_min))) << 24u;

    return hsv | a;
}

uint32_t ColorSpaceConverter::rgbaToHsla( uint32_t rgba)
{
    auto red   = RED( rgba),
            green = GREEN( rgba),
            blue  = BLUE( rgba);
    auto c_max = std::max( std::max( red, green), blue),
            c_min = std::min( std::min( red, green), blue),
            delta = ( uint8_t)( c_max - c_min);

    float hue {}, lum = ( float)( c_max + c_min) / ( 2 * RGB_SCALE),
          sat = delta == 0 ? 0 : ( float)delta / (( 1 - std::abs( 2 * lum - 1)) * RGB_SCALE);
    if( c_max == red)
    {
        auto segment = ( float)( green - blue) / ( float)delta,
                shift   = 0.0f;
        if( segment < 0)
            shift = 360.f / 60.f;
        hue = segment + shift;
    }
    else if( c_max == green)
    {
        auto segment = ( float)( blue - red) / ( float)delta,
                shift = 120.f / 60.f;
        hue = segment + shift;
    }
    else if( c_max == blue)
    {
        auto segment = ( float)( red - green) / ( float)delta,
                shift = 240.f / 60.f;
        hue = segment + shift;
    }

    return HSLA( hue * 60, sat * 100, lum * 100, ALPHA( rgba));
}

uint32_t ColorSpaceConverter::hslaToRgba( uint32_t hsla)
{
    float h = HUE( hsla) / 360.f,
            s = SAT( hsla) / 100.f,
            l = LUMIN( hsla) / 100.f;
    if( ZERO( s))
        return RGBA(( l * RGB_SCALE), ( l * RGB_SCALE), ( l * RGB_SCALE), ALPHA( hsla));

    float q = l < .5f ? l * ( 1 + s) : l + s - l * s;
    float p =  2 * l - q;
    return RGBA(( hueToSpace( p, q, h + 1./3) * RGB_SCALE), ( hueToSpace( p, q, h) * RGB_SCALE),
                ( hueToSpace( p, q, h - 1./3) * RGB_SCALE), ALPHA( hsla));
}

XyZColor ColorSpaceConverter::xyzFromRgb( uint32_t color)
{
    auto r = RED( color),
            g = GREEN( color),
            b = BLUE( color);

    return {
            .x = r * 0.4124564 + g * 0.3575761 + b * 0.1804375,
            .y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750,
            .z = r * 0.0193339 + g * 0.1191920 + b * 0.9503041
    };
}

uint32_t ColorSpaceConverter::xyzToRgb( XyZColor color)
{
    auto r = color.x * 3.2404542  + color.y * -1.5371385 + color.z * -0.4985314,
            g = color.x * -0.9692660 + color.y * 1.8760108  + color.z * 0.0415560,
            b = color.x * 0.0556434  + color.y * -0.2040259 + color.z * 1.0572252;

    r = ZERO( r) ? 0.0 : MIN( (int)std::round( r * XYZ_SCALE), RGB_SCALE);
    g = ZERO( g) ? 0.0 : MIN( (int)std::round( g * XYZ_SCALE), RGB_SCALE);
    b = ZERO( b) ? 0.0 : MIN( (int)std::round( b * XYZ_SCALE), RGB_SCALE);

    return RGB( r, g, b);
}

float ColorSpaceConverter::hueToSpace( float p, float q, float t)
{
    if( t < 0)
        t += 1;
    if( t > 1)
        t -= 1;
    if( t < 1. / 6)
        return p + ( q - p) * 6 * t;
    if( t < 1. / 2)
        return q;
    if( t < 2. / 3)
        return p + ( q - p) * ( 2.f / 3.f - t) * 6.f;

    return p;
}