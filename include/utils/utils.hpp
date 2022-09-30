#ifndef UTILS_HPP
#define UTILS_HPP

#include <vector>
#include "geometry/geo_vector.hpp"
#include "copywrite.hpp"

namespace Util
{
    template <typename T>
    std::pair<std::vector<std::basic_string<T>>, int> expand( std::basic_string_view<T> provision,
                                                              Justification mode, bool pad = true)
    {
        std::vector<std::basic_string<T>> parts;
        int j = 0, max_length = 0, prev = '\0';
        for( int i = 0; i < provision.length(); ++i)
        {
            if( provision[ i] == '\n')
            {
                int length = i - j - ( prev == '\r');
                auto substring = provision.substr( j, length);
                parts.emplace_back( substring);
                j = i + 1;
                max_length = std::max<int>( length, max_length);
            }
            prev = provision[ i];
        }
        parts.emplace_back( provision.substr( j));
        max_length = std::max<int>( parts.back().length(), max_length);
        if( pad)
        {
            for( auto& line : parts)
            {
                int rem   = max_length - line.length(),
                        left  = rem / ENUM_CAST( mode),
                        right = rem - ( left = ( left > 0) * left);
                line.insert( 0, std::basic_string<T>( left, ' '));
                line.append( std::basic_string<T>( right, ' '));
            }
        }
        return { parts, max_length};
    }

    template<typename Pred, typename First, typename... Others>
    bool compareAnd( First base, Others... others)
    {
        return ( ... && Pred()( base, others));
    }

    template<typename Pred, typename First, typename... Others>
    bool compareOr( First base, Others... others)
    {
        return ( ... || Pred()( base, others));
    }


    Vec2D<float> getSnapCoordinate( std::string_view given);

    float clamp( float x, float lowerlimit, float upperlimit);

    float smoothstep( float left, float right, float x);

    void insert( std::shared_ptr<BKNode> &node, std::string_view word, BKNode::Group word_group);

    void write( FrameBuffer<uint32_t> &frame, const char *raster_glyph, FILE *destination, KDNode *root);

    std::vector<std::string> partition( std::string_view provision, std::string_view regexpr);

    KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best = nullptr, uint8_t depth = 0);

    uint32_t editDistance( std::string_view main, std::string_view ref);

    std::vector<std::string> findWordMatch( BKNode *node, std::string_view word,
                                            BKNode::Group word_group, int threshold);

    bool ltrim( const char*& p);

    uint64_t getNumber( const char *&ctx, uint8_t base = 10);

    void requestFontList();

    std::string getFontFile( std::string_view font);

}

#endif