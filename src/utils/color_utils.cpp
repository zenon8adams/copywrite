#include "copywrite.hpp"
#include "color_utils.hpp"
#include "utils.hpp"
#include "colors_defs.hpp"
#include "codecs/color_space_converter.hpp"

namespace ColorUtil
{
    void insert( std::shared_ptr<KDNode>& node, Color color, size_t index, uint8_t depth)
    {
        if( node == nullptr)
        {
            node = std::make_shared<KDNode>( color, index);
            return;
        }

        uint8_t nchannel = color.rgb[ depth],
                cchannel = node->color.rgb[ depth],
                ndepth   = ( depth + 1) % 3;
        if( nchannel < cchannel)
            insert( node->left, color, index, ndepth);
        else
            insert( node->right, color, index, ndepth);
    }

    std::unordered_map<std::string_view, uint32_t>& colorCodeLookup()
    {
        static std::unordered_map<std::string_view, uint32_t> name_lookup =
                {
                        { COLOR_ALICEBLUE,            COLORHEX_ALICEBLUE},
                        { COLOR_ANTIQUEWHITE,         COLORHEX_ANTIQUEWHITE},
                        { COLOR_AQUA,                 COLORHEX_AQUA},
                        { COLOR_AQUAMARINE,           COLORHEX_AQUAMARINE},
                        { COLOR_AZURE,                COLORHEX_AZURE},
                        { COLOR_BEIGE,                COLORHEX_BEIGE},
                        { COLOR_BISQUE,               COLORHEX_BISQUE},
                        { COLOR_BLACK,                COLORHEX_BLACK},
                        { COLOR_BLANCHEDALMOND,       COLORHEX_BLANCHEDALMOND},
                        { COLOR_BLUE,                 COLORHEX_BLUE},
                        { COLOR_BLUEVIOLET,           COLORHEX_BLUEVIOLET},
                        { COLOR_BROWN,                COLORHEX_BROWN},
                        { COLOR_BURLYWOOD,            COLORHEX_BURLYWOOD},
                        { COLOR_CADETBLUE,            COLORHEX_CADETBLUE},
                        { COLOR_CHARTREUSE,           COLORHEX_CHARTREUSE},
                        { COLOR_CHOCOLATE,            COLORHEX_CHOCOLATE},
                        { COLOR_CORAL,                COLORHEX_CORAL},
                        { COLOR_CORNFLOWERBLUE,       COLORHEX_CORNFLOWERBLUE},
                        { COLOR_CORNSILK,             COLORHEX_CORNSILK},
                        { COLOR_CRIMSON,              COLORHEX_CRIMSON},
                        { COLOR_CYAN,                 COLORHEX_CYAN},
                        { COLOR_DARKBLUE,             COLORHEX_DARKBLUE},
                        { COLOR_DARKCYAN,             COLORHEX_DARKCYAN},
                        { COLOR_DARKGOLDENROD,        COLORHEX_DARKGOLDENROD},
                        { COLOR_DARKGRAY,             COLORHEX_DARKGRAY},
                        { COLOR_DARKGREY,             COLORHEX_DARKGREY},
                        { COLOR_DARKGREEN,            COLORHEX_DARKGREEN},
                        { COLOR_DARKKHAKI,            COLORHEX_DARKKHAKI},
                        { COLOR_DARKMAGENTA,          COLORHEX_DARKMAGENTA},
                        { COLOR_DARKOLIVEGREEN,       COLORHEX_DARKOLIVEGREEN},
                        { COLOR_DARKORANGE,           COLORHEX_DARKORANGE},
                        { COLOR_DARKORCHID,           COLORHEX_DARKORCHID},
                        { COLOR_DARKRED,              COLORHEX_DARKRED},
                        { COLOR_DARKSALMON,           COLORHEX_DARKSALMON},
                        { COLOR_DARKSEAGREEN,         COLORHEX_DARKSEAGREEN},
                        { COLOR_DARKSLATEBLUE,        COLORHEX_DARKSLATEBLUE},
                        { COLOR_DARKSLATEGRAY,        COLORHEX_DARKSLATEGRAY},
                        { COLOR_DARKSLATEGREY,        COLORHEX_DARKSLATEGREY},
                        { COLOR_DARKTURQUOISE,        COLORHEX_DARKTURQUOISE},
                        { COLOR_DARKVIOLET,           COLORHEX_DARKVIOLET},
                        { COLOR_DEEPPINK,             COLORHEX_DEEPPINK},
                        { COLOR_DEEPSKYBLUE,          COLORHEX_DEEPSKYBLUE},
                        { COLOR_DIMGRAY,              COLORHEX_DIMGRAY},
                        { COLOR_DIMGREY,              COLORHEX_DIMGREY},
                        { COLOR_DODGERBLUE,           COLORHEX_DODGERBLUE},
                        { COLOR_FIREBRICK,            COLORHEX_FIREBRICK},
                        { COLOR_FLORALWHITE,          COLORHEX_FLORALWHITE},
                        { COLOR_FORESTGREEN,          COLORHEX_FORESTGREEN},
                        { COLOR_FUCHSIA,              COLORHEX_FUCHSIA},
                        { COLOR_GAINSBORO,            COLORHEX_GAINSBORO},
                        { COLOR_GHOSTWHITE,           COLORHEX_GHOSTWHITE},
                        { COLOR_GOLD,                 COLORHEX_GOLD},
                        { COLOR_GOLDENROD,            COLORHEX_GOLDENROD},
                        { COLOR_GRAY,                 COLORHEX_GRAY},
                        { COLOR_GREY,                 COLORHEX_GREY},
                        { COLOR_GREEN,                COLORHEX_GREEN},
                        { COLOR_GREENYELLOW,          COLORHEX_GREENYELLOW},
                        { COLOR_HONEYDEW,             COLORHEX_HONEYDEW},
                        { COLOR_HOTPINK,              COLORHEX_HOTPINK},
                        { COLOR_INDIANRED,            COLORHEX_INDIANRED},
                        { COLOR_INDIGO,               COLORHEX_INDIGO},
                        { COLOR_IVORY,                COLORHEX_IVORY},
                        { COLOR_KHAKI,                COLORHEX_KHAKI},
                        { COLOR_LAVENDER,             COLORHEX_LAVENDER},
                        { COLOR_LAVENDERBLUSH,        COLORHEX_LAVENDERBLUSH},
                        { COLOR_LAWNGREEN,            COLORHEX_LAWNGREEN},
                        { COLOR_LEMONCHIFFON,         COLORHEX_LEMONCHIFFON},
                        { COLOR_LIGHTBLUE,            COLORHEX_LIGHTBLUE},
                        { COLOR_LIGHTCORAL,           COLORHEX_LIGHTCORAL},
                        { COLOR_LIGHTCYAN,            COLORHEX_LIGHTCYAN},
                        { COLOR_LIGHTGOLDENRODYELLOW, COLORHEX_LIGHTGOLDENRODYELLOW},
                        { COLOR_LIGHTGRAY,            COLORHEX_LIGHTGRAY},
                        { COLOR_LIGHTGREY,            COLORHEX_LIGHTGREY},
                        { COLOR_LIGHTGREEN,           COLORHEX_LIGHTGREEN},
                        { COLOR_LIGHTPINK,            COLORHEX_LIGHTPINK},
                        { COLOR_LIGHTSALMON,          COLORHEX_LIGHTSALMON},
                        { COLOR_LIGHTSEAGREEN,        COLORHEX_LIGHTSEAGREEN},
                        { COLOR_LIGHTSKYBLUE,         COLORHEX_LIGHTSKYBLUE},
                        { COLOR_LIGHTSLATEGRAY,       COLORHEX_LIGHTSLATEGRAY},
                        { COLOR_LIGHTSLATEGREY,       COLORHEX_LIGHTSLATEGREY},
                        { COLOR_LIGHTSTEELBLUE,       COLORHEX_LIGHTSTEELBLUE},
                        { COLOR_LIGHTYELLOW,          COLORHEX_LIGHTYELLOW},
                        { COLOR_LIME,                 COLORHEX_LIME},
                        { COLOR_LIMEGREEN,            COLORHEX_LIMEGREEN},
                        { COLOR_LINEN,                COLORHEX_LINEN},
                        { COLOR_MAGENTA,              COLORHEX_MAGENTA},
                        { COLOR_MAROON,               COLORHEX_MAROON},
                        { COLOR_MEDIUMAQUAMARINE,     COLORHEX_MEDIUMAQUAMARINE},
                        { COLOR_MEDIUMBLUE,           COLORHEX_MEDIUMBLUE},
                        { COLOR_MEDIUMORCHID,         COLORHEX_MEDIUMORCHID},
                        { COLOR_MEDIUMPURPLE,         COLORHEX_MEDIUMPURPLE},
                        { COLOR_MEDIUMSEAGREEN,       COLORHEX_MEDIUMSEAGREEN},
                        { COLOR_MEDIUMSLATEBLUE,      COLORHEX_MEDIUMSLATEBLUE},
                        { COLOR_MEDIUMSPRINGGREEN,    COLORHEX_MEDIUMSPRINGGREEN},
                        { COLOR_MEDIUMTURQUOISE,      COLORHEX_MEDIUMTURQUOISE},
                        { COLOR_MEDIUMVIOLETRED,      COLORHEX_MEDIUMVIOLETRED},
                        { COLOR_MIDNIGHTBLUE,         COLORHEX_MIDNIGHTBLUE},
                        { COLOR_MINTCREAM,            COLORHEX_MINTCREAM},
                        { COLOR_MISTYROSE,            COLORHEX_MISTYROSE},
                        { COLOR_MOCCASIN,             COLORHEX_MOCCASIN},
                        { COLOR_NAVAJOWHITE,          COLORHEX_NAVAJOWHITE},
                        { COLOR_NAVY,                 COLORHEX_NAVY},
                        { COLOR_OLDLACE,              COLORHEX_OLDLACE},
                        { COLOR_OLIVE,                COLORHEX_OLIVE},
                        { COLOR_OLIVEDRAB,            COLORHEX_OLIVEDRAB},
                        { COLOR_ORANGE,               COLORHEX_ORANGE},
                        { COLOR_ORANGERED,            COLORHEX_ORANGERED},
                        { COLOR_ORCHID,               COLORHEX_ORCHID},
                        { COLOR_PALEGOLDENROD,        COLORHEX_PALEGOLDENROD},
                        { COLOR_PALEGREEN,            COLORHEX_PALEGREEN},
                        { COLOR_PALETURQUOISE,        COLORHEX_PALETURQUOISE},
                        { COLOR_PALEVIOLETRED,        COLORHEX_PALEVIOLETRED},
                        { COLOR_PAPAYAWHIP,           COLORHEX_PAPAYAWHIP},
                        { COLOR_PEACHPUFF,            COLORHEX_PEACHPUFF},
                        { COLOR_PERU,                 COLORHEX_PERU},
                        { COLOR_PINK,                 COLORHEX_PINK},
                        { COLOR_PLUM,                 COLORHEX_PLUM},
                        { COLOR_POWDERBLUE,           COLORHEX_POWDERBLUE},
                        { COLOR_PURPLE,               COLORHEX_PURPLE},
                        { COLOR_REBECCAPURPLE,        COLORHEX_REBECCAPURPLE},
                        { COLOR_RED,                  COLORHEX_RED},
                        { COLOR_ROSYBROWN,            COLORHEX_ROSYBROWN},
                        { COLOR_ROYALBLUE,            COLORHEX_ROYALBLUE},
                        { COLOR_SADDLEBROWN,          COLORHEX_SADDLEBROWN},
                        { COLOR_SALMON,               COLORHEX_SALMON},
                        { COLOR_SANDYBROWN,           COLORHEX_SANDYBROWN},
                        { COLOR_SEAGREEN,             COLORHEX_SEAGREEN},
                        { COLOR_SEASHELL,             COLORHEX_SEASHELL},
                        { COLOR_SIENNA,               COLORHEX_SIENNA},
                        { COLOR_SILVER,               COLORHEX_SILVER},
                        { COLOR_SKYBLUE,              COLORHEX_SKYBLUE},
                        { COLOR_SLATEBLUE,            COLORHEX_SLATEBLUE},
                        { COLOR_SLATEGRAY,            COLORHEX_SLATEGRAY},
                        { COLOR_SLATEGREY,            COLORHEX_SLATEGREY},
                        { COLOR_SNOW,                 COLORHEX_SNOW},
                        { COLOR_SPRINGGREEN,          COLORHEX_SPRINGGREEN},
                        { COLOR_STEELBLUE,            COLORHEX_STEELBLUE},
                        { COLOR_TAN,                  COLORHEX_TAN},
                        { COLOR_TEAL,                 COLORHEX_TEAL},
                        { COLOR_THISTLE,              COLORHEX_THISTLE},
                        { COLOR_TOMATO,               COLORHEX_TOMATO},
                        { COLOR_TURQUOISE,            COLORHEX_TURQUOISE},
                        { COLOR_VIOLET,               COLORHEX_VIOLET},
                        { COLOR_WHEAT,                COLORHEX_WHEAT},
                        { COLOR_WHITE,                COLORHEX_WHITE},
                        { COLOR_WHITESMOKE,           COLORHEX_WHITESMOKE},
                        { COLOR_YELLOW,               COLORHEX_YELLOW},
                        { COLOR_YELLOWGREEN,          COLORHEX_YELLOWGREEN},
                };
        return name_lookup;
    }

    uint32_t decodeColorName( const char *&ctx, BKNode *bkroot)
    {
        std::string name;
        do
        {
            name += static_cast<char>( std::tolower( *ctx));
        }
        while( isalpha( *++ctx));

        auto& code_lookup = colorCodeLookup();
        auto pos = code_lookup.find( name);
        if( pos != code_lookup.cend())
            return pos->second;
        else
        {
            fprintf( stderr, "Unable to find match for `%s`\n", name.c_str());
            auto matches = Util::findWordMatch( bkroot, name.c_str(), BKNode::Group::Color, 3);
            if( !matches.empty())
            {
                fprintf( stderr, "The following colors match your search:\n");
                for( size_t i = 0; i < matches.size(); ++i)
                    fprintf( stderr, "%zu]. %s\n", i + 1, matches[ i].c_str());
            }
        }

        return 0xFFFFFFu;
    }

    uint64_t extractColor( const char *&rule, BKNode *bkroot)
    {
        uint64_t ccolor{};
        Util::ltrim( rule);
        int cratio = -1, shift = 0;
        const char *prev = rule;
        if( *rule == '/')
        {
            shift = 32u;
            ++rule;
        }

        if( Util::compareOr<std::equal_to<char>>( *rule, '#', 'x'))
            ++rule;
        else if( strncasecmp( rule, "0x", 2) == 0)
            rule += 2;
        else if( isalpha( *rule))
        {
            uint64_t color_name = decodeColorName( rule, bkroot);
            if( *rule == ':')
            {
                if( *++rule == ':')
                {
                    cratio = INT_CAST( Util::getNumber( ++rule));
                    cratio = cratio < 0 ? 0 : cratio;
                    color_name |= 0xFFu;
                }
                else
                {
                    color_name |= Util::getNumber( rule, 16);
                    if( *rule == ':')
                        cratio = INT_CAST( Util::getNumber( ++rule));
                }

                if( cratio != -1)
                {
                    cratio = MIN( cratio, 100);
                    double cscale = cratio / 100.0;
                    color_name = SCALE_RGB( color_name, cscale) | ALPHA( color_name);
                }

                if( Util::ltrim( rule) && *rule == '/')
                    return MAKE_QWORD( ColorUtil::extractColor( ++rule, bkroot), color_name);

                return color_name << shift;
            }

            if( Util::ltrim( rule) && *rule == '/')
                return MAKE_QWORD( ColorUtil::extractColor( ++rule, bkroot), color_name | 0xFFu);

            return ( color_name | 0xFFu) << shift;
        }
        else
        {
            return {};
//        fprintf( stderr, "Expected hex indicator near -> %s (allowed: `#`, `x`, `0x`)", prev);
//        exit( EXIT_FAILURE);
        }

        prev = rule;
        if( isxdigit( *rule))
        {
            ccolor = Util::getNumber( rule, 16);
            uint8_t ccount = rule - prev;
            if( ccolor == 0 || ( ccount != 6 && ccount != 8))
            {
                fprintf( stderr, ccolor == 0 ? "Invalid color specification %s"
                                             : "Color has to be 6 or 8 hex digits -> %s", prev);
                exit( EXIT_FAILURE);
            }
            else if( ccount == 6)
                ccolor = ccolor << 8u | 0xFFu;

            if( *rule == ':')
            {
                cratio = INT_CAST( Util::getNumber( ++rule));
                cratio = cratio < 0 ? static_cast<uint8_t>( 0) : cratio;
                cratio = MIN( cratio, 100);
                double cscale = cratio / 100.0;
                ccolor = SCALE_RGB( ccolor, cscale);
            }

            if( Util::ltrim( rule) && *rule == '/')
                return MAKE_QWORD( ColorUtil::extractColor( ++rule, bkroot), ccolor);
        }
        else
        {
            fprintf( stderr, "Expected hex digits -> %s", prev);
            exit( EXIT_FAILURE);
        }

        return ccolor << shift;
    }

    uint32_t sumMix(uint32_t lcolor, uint32_t rcolor)
    {
        /*
         * References:
         *  + https://en.wikipedia.org/wiki/CIE_1931_color_space
         *  + http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
         *
         */
        auto lxyz = ColorSpaceConverter::xyzFromRgb( lcolor),
             rxyz = ColorSpaceConverter::xyzFromRgb( rcolor);

        double lsum = lxyz.x + lxyz.y + lxyz.z,
                rsum = rxyz.x + rxyz.y + rxyz.z,
                lx = lxyz.x / lsum,
                ly = lxyz.y / lsum,
                rx = rxyz.x / rsum,
                ry = rxyz.y / rsum,
                xmix = ( lx * lxyz.y / ly + rx * rxyz.y / ry) / ( lxyz.y / ly + rxyz.y / ry),
                ymix = ( lxyz.y + rxyz.y) / ( lxyz.y / ly + rxyz.y / ry),
                zmix = 1 - xmix - ymix;

        return ColorSpaceConverter::xyzToRgb( { xmix, ymix, zmix});
    }

    uint32_t subMix( uint32_t lcolor, uint32_t rcolor)
    {
        auto lred   = RGB_SCALE - RED( lcolor),
                lgreen = RGB_SCALE - GREEN( lcolor),
                lblue  = RGB_SCALE - BLUE( lcolor),
                rred   = RGB_SCALE - RED( rcolor),
                rgreen = RGB_SCALE - GREEN( rcolor),
                rblue  = RGB_SCALE - BLUE( rcolor);

        auto r = RGB_SCALE - std::sqrt( .5 * ( lred * lred + rred * rred)),
                g = RGB_SCALE - std::sqrt( .5 * ( lgreen * lgreen + rgreen * rgreen)),
                b = RGB_SCALE - std::sqrt( .5 * ( lblue * lblue + rblue * rblue));

        return RGB( r, g, b);
    }

    uint64_t mixColor( const char *&ctx, BKNode *bkroot)
    {
        uint64_t lcolor{}, rcolor{};
        uint8_t alpha = 0xFFu;
        uint16_t ops{};
        while( Util::ltrim( ctx) && *ctx != ')')
        {
            if( *ctx && ctx[ 1] == '/')
            {
                ops = MAKE_WORD( ctx[ 2], *ctx);
                ctx += 2;
            }
            else if( Util::compareOr<std::equal_to<char>>( *ctx, '+', '-'))
                ops = INT_CAST( *ctx);
            else
            {
                auto *before = ctx;
                ops ? rcolor = ColorUtil::extractColor( ctx, bkroot) : lcolor = ColorUtil::extractColor( ctx, bkroot);
                ctx -= 1 - ( before == ctx);
            }
            ++ctx;
        }

        ctx += 1;

        if( Util::ltrim( ctx) && *ctx == ':')
            alpha = Util::getNumber( ++ctx, 16);

        return Util::compareOr<std::equal_to<char>>( ops, '+', '-') ?
               MAKE_QWORD( HIGH_BYTE( ops) == '+' ? ColorUtil::sumMix( HIGH_DWORD( lcolor), HIGH_DWORD( rcolor)) | alpha :
                           ColorUtil::subMix( HIGH_DWORD( lcolor), HIGH_DWORD( rcolor)) | alpha,
                           LOW_BYTE( ops) == '+' ?  ColorUtil::sumMix( LOW_DWORD( lcolor), LOW_DWORD( rcolor)) | alpha :
                           ColorUtil::subMix( LOW_DWORD( lcolor), LOW_DWORD( rcolor)) | alpha) : lcolor;
    }

    uint8_t colorClamp( float color)
    {
        return color < 0.f ? 0 : ( color > FLOAT_CAST( RGB_SCALE)) ? RGB_SCALE : INT_CAST( color);
    }

    uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, float progress)
    {
        uint32_t r = RED( lcolor)   * ( 1.0 - progress) + RED( rcolor)   * progress,
                g = GREEN( lcolor) * ( 1.0 - progress) + GREEN( rcolor) * progress,
                b = BLUE( lcolor)  * ( 1.0 - progress) + BLUE( rcolor)  * progress,
                a = ALPHA( lcolor) * ( 1.0 - progress) + ALPHA( rcolor)  * progress;

        return RGBA( r, g, b, a);
    }

    uint32_t interpolateColor( uint32_t scolor, uint32_t ecolor, float progress)
    {
        auto shsv = ColorSpaceConverter::rgbaToHsva( scolor);
        auto ehsv = ColorSpaceConverter::rgbaToHsva( ecolor);

        return ColorSpaceConverter::hsvaToRgba( ColorUtil::colorLerp( shsv, ehsv, progress));
    }

    uint32_t tintColor( uint32_t color, float factor)
    {
        uint8_t red   = ColorUtil::colorClamp( RED( color) * factor),
                green = ColorUtil::colorClamp( GREEN( color) * factor),
                blue  = ColorUtil::colorClamp( BLUE( color) * factor);

        return RGBA( red, green, blue, ALPHA( color));
    }
}