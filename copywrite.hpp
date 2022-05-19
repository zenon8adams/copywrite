/***************************************************************************/
/*                                                                         */
/*  copywrite.hpp                                                          */
/*                                                                         */
/*    Forward declaration           									   */
/*                                                                         */
/*  Copyright 2022 by                                                      */
/*  Adesina Meekness                                                       */
/*                                                                         */
/*       ##    ## ##                                                       */
/*       ##    ##  #                                                       */
/*       ###  ###  #  ##                                                   */
/*       # # # ##  # #                                                     */
/* ####  # ### ##  ###                                                     */
/*       #  #  ##  # ##                                                    */
/*       #  #  ##  #  ##                                                   */
/*                                                                         */
/*                                                                         */
/*  This file is part of the Copywrite project, and may only be used,      */
/*  modified, and distributed under the terms of the GNU project           */
/*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
/*  this file you indicate that you have read the license and              */
/*  understand and accept it fully.                                        */
/*                                                                         */
/***************************************************************************/

#ifndef COPYWRITE_HPP
#define COPYWRITE_HPP

#include <ft2build.h>
#include <string>
#include <pngconf.h>
#include <vector>
#include <functional>

#ifndef PNG_STDIO_SUPPORTED
typedef FILE                * png_FILE_p;
#endif

#include  FT_FREETYPE_H

#define MAX_DIFF_TOLERANCE 20

unsigned char *toMonochrome(FT_Bitmap bitmap);

struct ColorRule
{
    int32_t start = 0, end = -1;
    uint32_t scolor = 0x000000FF, ecolor = 0x000000FF,
             font_size_b = UINT32_MAX, font_size_m = UINT32_MAX,
             font_size_e = UINT32_MAX;
    std::function<float(float)> color_easing_fn, font_easing_fn;
};

// Stores the standard red, green, and blue chroma (sRGB)
struct Color
{
    uint8_t rgb[ 3]{};
};

// Stores the CIE 1931 chromaticity values.
struct XyZColor
{
    double x, y, z;
};

struct KDNode
{
    Color color;
    size_t index = -1;
    KDNode *left = nullptr,
            *right = nullptr;
};

struct BKNode
{
    const char *word = nullptr;
    BKNode *next[ MAX_DIFF_TOLERANCE]{};
};

/*
 * Glyph: Structural representation of a character
 */

struct Glyph
{
    FT_Int width{},
            height{},
            xstep{},
            index{};
    unsigned char *pixmap{};
    FT_Vector origin{};
    ColorRule match;
    Glyph *next = nullptr;
};


Glyph extract( FT_GlyphSlot slot);

/*
 * Build up a linked list of characters that
 * make up the sentence
 */

void insert( Glyph *&index, Glyph glyph);

/*
 * Calculate the adjustment distance between
 * two glyphs. E.g `a ,` should be rendered as `a,`
 */

FT_Int kerning( FT_UInt c, FT_UInt prev, FT_Face face);

/*
 * Render the given glyph into the final computed container
 */

void draw( const Glyph& glyph, FT_Vector *pen, uint64_t *out, FT_Int mdescent, FT_Int width, FT_Int height, size_t total);

/*
 * Display the monochrome canvas into stdout
 */

void write(const uint64_t *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination, KDNode *root);

static size_t byteCount( uint8_t c );

static uint32_t collate( uint8_t *str, size_t idx, uint8_t count );

std::vector<ColorRule> parseColorRule(const char *rule, BKNode *bkroot);

uint32_t extractColor(const char *&rule, BKNode *bkroot);

uint32_t mixColor(const char *&ctx, BKNode *bkroot);

uint32_t mixRgb( uint32_t lcolor, uint32_t rcolor);

void fillEasingMode(std::function<float(float)> &function, const char *&rule, BKNode *bkroot, char eoc);

uint32_t interpolateColor( uint32_t scolor, uint32_t ecolor, double progress);

uint32_t decodeColorName(const char *&ctx, BKNode *bkroot);

uint32_t rgbaToHsva( uint32_t rgb);

uint32_t hsvaToRgba( uint32_t hsv);

uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, double progress);

XyZColor xyzFromRgb( uint32_t color);

uint32_t xyzToRgb( XyZColor color);

KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best = nullptr, uint8_t depth = 0);

uint32_t editDistance( const std::string& main, const std::string& ref);

void insert( KDNode *&node, Color color, size_t index = 0, uint8_t depth = 0);

void insert( BKNode *&node, const char *word);

void free( KDNode *&node);

void free( BKNode *&node);

void findWordMatch( BKNode *node, const char *word, int threshold, std::vector<std::string>& matches);

std::vector<std::string> findWordMatch( BKNode *node, const char *word, int threshold = 4);

/*
 * Main dispatcher: Does all the rendering and display
 */

size_t countCharacters( const char *pw);

bool ltrim( const char*& p);

uint32_t getNumber( const char *&ctx, uint8_t base = 10);

void
render(const char *word, FT_Face face, size_t default_font_size, const char *raster_glyph, FILE *destination, bool as_image,
       const char *color_rule, KDNode *root, BKNode *bkroot);

void writePNG( FILE *cfp, const uint64_t *buffer, png_int_32 width, png_int_32 height);

void requestFontList();

#endif //COPYWRITE_HPP
