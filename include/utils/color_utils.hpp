#ifndef COLOR_UTILS_HPP
#define COLOR_UTILS_HPP

#include <cstdint>
#include <unordered_map>
#include <memory>
#include <string_view>
#include "copywrite.hpp"

namespace ColorUtil
{
    void insert( std::shared_ptr<KDNode>& node, Color color, size_t index = 0, uint8_t depth = 0);

    std::unordered_map<std::string_view, uint32_t>& colorCodeLookup();

    uint32_t decodeColorName( const char *&ctx, BKNode *bkroot);

    uint64_t extractColor( const char *&rule, BKNode *bkroot);

    uint32_t sumMix(uint32_t lcolor, uint32_t rcolor);

    uint32_t subMix( uint32_t lcolor, uint32_t rcolor);

    uint64_t mixColor( const char *&ctx, BKNode *bkroot);

    uint8_t colorClamp( float color);

    uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, float progress);

    uint32_t interpolateColor( uint32_t scolor, uint32_t ecolor, float progress);

    uint32_t tintColor( uint32_t color, float factor);
}

#endif