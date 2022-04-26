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
#include  FT_FREETYPE_H

unsigned char *to_monochrome( FT_Bitmap bitmap);

/*
 * Glyph: Structural representation of a character
 */

struct Glyph
{
    FT_Int width{},
        height{},
        xstep{};
    unsigned char *pixmap{};
    FT_Vector origin{};

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

void draw( Glyph glyph, FT_Vector *pen, unsigned char *out, FT_Int mdescent, FT_Int width, FT_Int height);

/*
 * Display the monochrome canvas into stdout
 */

void write( const unsigned char *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination);

static size_t byteCount( uint8_t c );

static uint32_t collate( uint8_t *str, size_t idx, uint8_t count );

/*
 * Main dispatcher: Does all the rendering and display
 */

void render( const char *word, FT_Face face, const char *render_glyph, FILE *screen);

void requestFontList();

#endif //COPYWRITE_HPP
