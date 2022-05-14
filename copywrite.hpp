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
/*  modified, and distributed under the terms of the FreeType project      */
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

unsigned char *to_monochrome( FT_Bitmap bitmap);

struct ColorRule
{
    int32_t start = 0, end = -1;
    uint32_t scolor = 0x000000FF, ecolor = 0x000000FF, font_size_b = UINT32_MAX, font_size_m = UINT32_MAX, font_size_e = UINT32_MAX;
    std::function<float(float)> color_easing_fn, font_easing_fn;
};

struct Color
{
    uint8_t rgb[ 3]{};
};

struct KDNode
{
    Color color;
    size_t index = -1;
    KDNode *left = nullptr,
            *right = nullptr;
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

void fillEasingMode(std::function<float(float)> &function, const char *&rule, char eoc);

/*
 * Main dispatcher: Does all the rendering and display
 */

void
render(const char *word, FT_Face face, const char *raster_glyph, FILE *destination, bool as_image,
       const char *color_rule, KDNode *root);

void writePNG( FILE *cfp, const uint64_t *buffer, png_int_32 width, png_int_32 height);

void requestFontList();

#endif //COPYWRITE_HPP
