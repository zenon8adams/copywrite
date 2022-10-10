#include <iostream>
#include "copywrite.hpp"
#include <memory>
#include <string>
#include <string_view>
#include <vector>
#include <regex>
#include "utils.hpp"
#include "snap_defs.hpp"
#include "codecs/text_codec.hpp"
#include <fontconfig/fontconfig.h>

namespace Util
{
    void insert( std::shared_ptr<BKNode> &node, std::string_view word, BKNode::Group word_group)
    {
        if( node == nullptr)
        {
            node = std::make_shared<BKNode>( word, word_group);
            return;
        }

        auto dist = editDistance( node->word, word);

        if( dist >= MAX_DIFF_TOLERANCE)
            return;

        if( node->next[ dist] == nullptr)
            insert( node->next[ dist], word, word_group);
        else
        {
            auto ndist = editDistance( node->next[ dist]->word, word);
            insert( node->next[ dist]->next[ ndist], word, word_group);
        }
    }

    bool ltrim( const char*& p)

    {
        while( isspace( *p))
            ++p;
        return true;
    }

    void findWordMatch( BKNode *node, std::string_view word, BKNode::Group word_group,
                        int threshold, std::vector<std::string> &matches)
    {
        if( node == nullptr)
            return;

        int dist = INT_CAST( editDistance( node->word, word)),
                mindist = MAX( dist - threshold, 1),
                maxdist = MIN( dist + threshold, MAX_DIFF_TOLERANCE - 1);

        if( dist <= threshold && node->group == word_group)
            matches.emplace_back( node->word);

        for( int i = mindist; i <= maxdist; ++i)
            findWordMatch( node->next[i].get(), word, word_group, threshold, matches);
    }

    std::vector<std::string> findWordMatch( BKNode *node, std::string_view word, BKNode::Group word_group,
                                            int threshold)
    {
        std::vector<std::string> matches;
        findWordMatch( node, word, word_group, threshold, matches);
        return matches;
    }


float smoothstep( float left, float right, float x)
{
    // Scale, and Util::clamp x to 0..1 range
    x = Util::clamp( ( x - left) / ( right - left), 0.f, 1.f);
    // Evaluate Perlin polynomial( Smoother than Hermite's interpolation)
    return x * x * x * ( x * ( x * 6.f - 15.f) + 10.f);
}

float clamp( float x, float lowerlimit, float upperlimit)
{
    return x < lowerlimit ? lowerlimit : x > upperlimit ? upperlimit : x;
}

KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best, uint8_t depth)
{
    if( node == nullptr)
        return best;

    uint8_t nchannel = search.rgb[ depth],
            cchannel = node->color.rgb[ depth],
            ndepth   = ( depth + 1) % 3;

    int r = search.rgb[ 0] - node->color.rgb[ 0],
            g = search.rgb[ 1] - node->color.rgb[ 1],
            b = search.rgb[ 2] - node->color.rgb[ 2];

    double y = .299    * r + .587   * g + .114   * b,
            u = -.14713 * r - .28886 * g + .436   * b,
            v = .615    * r - .51499 * g - .10001 * b,

            ndist = std::sqrt( y * y + u * u + v * v);
    if( ndist < ldist)
    {
        ldist = ndist;
        best = node;
    }

    bool left = true;
    if( nchannel < cchannel)
    {
        if( node->left != nullptr)
            best = approximate( node->left.get(), search, ldist, best, ndepth);
        else
            return best;
    }
    else
    {
        if( node->right != nullptr)
            best = approximate( node->right.get(), search, ldist, best, ndepth);
        else
            return best;
        left = false;
    }

    if( std::abs( search.rgb[ depth] - node->color.rgb[ depth]) < ldist)
        best = approximate( !left ? node->left.get() : node->right.get(), search, ldist, best, ndepth);

    return best;
}

void write( FrameBuffer<uint32_t> &frame, const char *raster_glyph, FILE *destination, KDNode *root)
{
    bool is_stdout = false;
    auto raster_bytes = destination != stdout ? MAX( TextCodec::byteCount( *raster_glyph) - 1, 1) : is_stdout = true;
    std::string sp( raster_bytes, ' ');
    auto width = frame.width,
         height = frame.height;
    uint8_t fmt[] = { '\x1B', '[', '3', '8', ';', '5', ';', '0', '0', '0', 'm', '\0'};
    constexpr uint8_t offset = 7;

    for ( int j = 0; j < height; ++j)
    {
        for ( int i = 0; i < width; ++i)
        {
            double initial = INFINITY;
            uint32_t color =  frame.buffer.get()[ j * width + i];
            bool is_transparent = ( color & 0xFFu) == 0u;
            auto nmatch = approximate( root, { ( uint8_t)RED( color),
                                               ( uint8_t)GREEN( color),
                                               ( uint8_t)BLUE( color)}, initial);
            fmt[     offset] = nmatch->index / 100 + '0';
            fmt[ offset + 1] = ( nmatch->index - ( fmt[ offset] - '0') * 100) / 10 + '0';
            fmt[ offset + 2] = nmatch->index % 10  + '0';

            if( is_stdout)
                fprintf( destination, "%s", ( const char *)fmt);
            fprintf( destination,"%s", color && !is_transparent ? raster_glyph : sp.c_str());
            if( is_stdout)
                fprintf( destination, "\x1B[0m");
        }
        fputc( '\n', destination);
    }
}

uint64_t getNumber( const char *&ctx, uint8_t base)
{
    uint64_t weight = 0;
    while( isxdigit( *ctx))
    {
        uint8_t character = tolower( *( ctx++));
        int value = character >= 'a' && character <= 'f' ? character - 'a' + 10
                : isdigit( character) ? character - '0' : 0;
        weight = weight * base + value;
    }

    return weight;
}

std::vector<std::string> partition( std::string_view provision, std::string_view regexpr)
{
    std::cmatch search_result;
    std::regex  key( regexpr.data());
    std::cregex_iterator begin( provision.cbegin(), provision.cend(), key),
            end, prev;

    std::vector<std::string> results;
    for( ;begin != end; prev = begin, ++begin)
        results.emplace_back( begin->prefix().str());

    if( prev != end && begin == end)
        results.emplace_back( prev->suffix().str());

    return results;
}

uint32_t editDistance( std::string_view main, std::string_view ref)
{
    auto mlength = main.size() + 1, rlength = ref.size() + 1;
    uint8_t lookup[ mlength][ rlength];

    for( size_t j = 0; j < rlength; ++j)
        lookup[ 0][ j] = j;
    for( size_t i = 0; i < mlength; ++i)
        lookup[ i][ 0] = i;

    for ( size_t j = 1; j < mlength; ++j)
    {
        for ( size_t i = 1; i < rlength; ++i)
        {
            lookup[ j][ i] = std::min( lookup[ j - 1][ i] + 1,
                                       std::min( lookup[ j][ i - 1] + 1,
                                                 lookup[ j - 1][ i - 1] +
                                                 ( tolower( main[ j - 1]) != tolower( ref[ i - 1]))));
        }
    }

    return lookup[ mlength - 1][ rlength - 1];
}

Vec2D<float> getSnapCoordinate( std::string_view given)
{
    size_t view_size = given.size();
    if( !view_size)
        return { INFINITY, INFINITY};

    static const std::unordered_map<std::string_view, Vec2D<float>> possibilities
    {
        { SNAP_TOP_LEFT,       { 0.f,  0.f}},
        { SNAP_TOP_CENTER ,    { .5f,  0.f}},
        { SNAP_TOP_RIGHT ,     { 1.f,  0.f}},
        { SNAP_LEFT_CENTER ,   { 0.f,  .5f}},
        { SNAP_CENTER ,        { 0.5f, .5f}},
        { SNAP_RIGHT_CENTER ,  { 1.f,  .5f}},
        { SNAP_BOTTOM_LEFT ,   { 0.f,  1.f}},
        { SNAP_BOTTOM_CENTER , { 0.5f, 1.f}},
        { SNAP_BOTTOM_RIGHT ,  { 1.f,  1.f}},
    };

    std::string clone( view_size, 0);
    std::transform( given.cbegin(), given.cend(), clone.begin(), tolower);
    auto index = possibilities.find( clone);
    if( index == possibilities.cend())
        return { INFINITY, INFINITY};

    return index->second;
}

void requestFontList()
{
    if( !FcInit())
        return;

    auto locale_name = std::locale("").name();
    if( size_t idx = locale_name.find( '_'); idx != std::string::npos)
        locale_name = locale_name.substr( 0, idx).insert( 0, ":lang=");
    PropertyManager<FcConfig *> config( FcConfigGetCurrent(), FcConfigDestroy);
    FcConfigSetRescanInterval( config.get(), 0);
    PropertyManager<FcPattern *> pattern( FcNameParse((FcChar8 *)locale_name.c_str()), FcPatternDestroy);
    PropertyManager<FcObjectSet *> font_object_set( FcObjectSetBuild ( FC_OUTLINE, FC_STYLE, FC_FAMILY, nullptr),
                                                    FcObjectSetDestroy);
    PropertyManager<FcFontSet *> font_set( FcFontList( config.get(), pattern.get(), font_object_set.get()),
                                           []( auto font_set_local)
                                           {
                                               if( font_set_local)
                                                   FcFontSetDestroy( font_set_local);
                                           });

    if( font_set && font_set->nfont > 0)
    {
        int i = 0;
        do
        {
            PropertyManager<const char *> font_manager( ( const char *)FcNameUnparse( font_set->fonts[ i]),
                                                        []( auto *p) { free( ( FcChar8 *)p); });
            std::string_view font( font_manager.get());
            if( font.empty())
                continue;
            auto parts = Util::partition( font, ":");
            if( parts.size() != 3)
                continue;
            std::smatch sm;
            std::regex key( ".*(?:(normal)|(bold)|(italic)|(regular)).*", std::regex_constants::icase);
            if( std::regex_search( parts[ 1].cbegin(), parts[ 1].cend(), sm, key))
            {
                std::string style;
                if( sm[ 1].matched || sm[ 4].matched) style.append( "Normal");
                if( sm[ 2].matched) style.append( style.empty() ? "Bold" : ", Bold");
                if( sm[ 3].matched) style.append( style.empty() ? "Italic" : ", Italic");
                std::cout << "Family: " << parts[ 0] << ", style(s): " << style <<'\n';
            }
        }
        while( ++i < font_set->nfont);
    }
    FcFini();
}

std::string getFontFile( std::string_view font)
{
    if( !FcInit())
        return {};

    const char *styles[] = { nullptr, "normal", "bold", "italic", "regular"},
            *style    = styles[ 1];
    std::cmatch cm;
    std::regex key( ".*(?:(normal)|(bold)|(italic)|(regular)).*", std::regex_constants::icase);
    if( std::regex_search( font.cbegin(), font.cend(), cm, key))
    {
        auto trim_pos = std::min( { cm.position( 1), cm.position( 2), cm.position( 3), cm.position( 4)});
        style = styles[ cm[ 1].matched * 1];
        style = styles[ cm[ 2].matched * 2];
        style = styles[ cm[ 3].matched * 3];
        style = styles[ cm[ 4].matched * 4];
        if( style == nullptr)
            style = styles[ 1];
        while( trim_pos > 0 && font[ --trim_pos] == ' ')
            ;
        font.remove_suffix( font.size() - trim_pos - 1);
    }

    PropertyManager<FcConfig *> config( FcConfigGetCurrent(), FcConfigDestroy);
    FcConfigSetRescanInterval( config.get(), 0);
    auto parts = Util::partition( font, R"(\s*,\s*)");
    if( parts.empty())
        parts.emplace_back( font);
    for( auto& part : parts)
    {
        PropertyManager<FcPattern *> pattern( FcPatternCreate(), FcPatternDestroy);
        FcPatternAddString( pattern.get(), FC_FAMILY, ( const FcChar8 *)part.data());
        PropertyManager<FcObjectSet *> font_object_set( FcObjectSetBuild ( FC_FILE, nullptr),
                                                        FcObjectSetDestroy);
        PropertyManager<FcFontSet *> font_set( FcFontList( config.get(), pattern.get(), font_object_set.get()),
                                               []( auto font_set_local)
                                               {
                                                   if( font_set_local)
                                                       FcFontSetDestroy( font_set_local);
                                               });
        if( font_set && font_set->nfont > 0)
        {
            int i = 0;
            do
            {
                PropertyManager<const char *> font_manager( ( const char *)FcNameUnparse( font_set->fonts[ i]),
                                                            []( auto *p) { free( ( FcChar8 *)p); });
                if( font_manager.get() == nullptr)
                    continue;
                auto *filename = strchr( font_manager.get(), '/');
                if( strcasestr( filename, style) == nullptr)
                    return filename;
            }
            while( ++i < font_set->nfont);
        }
    }

    return {};
}
}