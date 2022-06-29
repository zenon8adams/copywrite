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
#include <memory>
#include <cmath>

#ifndef PNG_STDIO_SUPPORTED
typedef FILE                * png_FILE_p;
#endif

#include FT_FREETYPE_H
#include "geo_vector.hpp"

#define MAX_DIFF_TOLERANCE 20

std::unique_ptr<unsigned char, void( *)( unsigned char *)> toMonochrome(FT_Bitmap bitmap);

enum class GradientType { Linear, Radial};

struct BaseGradient
{
  int32_t startx{}, width{}, height{};
  GradientType gradient_type{ GradientType::Linear};
};

struct RadialGradient : BaseGradient
{
  Vec3D props;
  explicit RadialGradient( float x, float y, float z)
  : props( x, y, z), BaseGradient{ .gradient_type = GradientType::Radial}
  {
  }
};

struct LinearGradient : BaseGradient
{
};

struct ColorRule
{
    int32_t start = 0, end = -1;
    uint32_t scolor = 0x000000FF, ecolor = 0x000000FF,
             font_size_b = UINT32_MAX, font_size_m = UINT32_MAX,
             font_size_e = UINT32_MAX;
    bool soak{ false};
    std::shared_ptr<BaseGradient> gradient;
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
    std::shared_ptr<KDNode> left{}, right{};
    KDNode( Color color, size_t index)
    : color( color), index( index)
    {
    }
};

struct BKNode
{
  std::string_view word;
  std::shared_ptr<BKNode> next[ MAX_DIFF_TOLERANCE]{};
  explicit BKNode( std::string_view word)
  : word( word)
  {
  }
};

float clamp(float x, float lowerlimit, float upperlimit)
{
  return x < lowerlimit ? lowerlimit : x > upperlimit ? upperlimit : x;
}

float smoothstep( float left, float right, float x)
{
  // Scale, and clamp x to 0..1 range
  x = clamp(( x - left) / ( right - left), 0.f, 1.f);
  // Evaluate Perlin polynomial
  return x * x * x * (x * (x * 6.f - 15.f) + 10.f);
}

template<typename Resource>
class PropertyManager
{
 public:
  template <typename Deleter>
   PropertyManager( Resource&& resource, Deleter&& deleter)
      : resource( std::forward<Resource>( resource)),
        destructor( std::forward<Deleter>( deleter))
  {
  }
  template <typename Deleter>
  explicit PropertyManager( Deleter&& deleter)
  : destructor( std::forward<Deleter>( deleter))
  {
  }

  PropertyManager( const PropertyManager&) = delete;
  PropertyManager( PropertyManager&&)      = delete;

  auto& get()
  {
    return resource;
  }

  auto *operator->()
  {
    return resource;
  }

  explicit operator bool() const
  {
    return resource;
  }

  ~PropertyManager()
  {
      std::invoke( destructor, resource);
  }

 private:
  Resource resource;
  std::function<void( Resource)> destructor;
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
    std::unique_ptr<unsigned char, void( *)( unsigned char *)> pixmap;
    FT_Vector origin{};
    std::shared_ptr<ColorRule> match;
    std::unique_ptr<Glyph> next{};
    explicit Glyph( std::unique_ptr<unsigned char, void( *)( unsigned char *)> pixmap)
    : pixmap( std::move( pixmap))
    {
    }
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

uint32_t extractColor( const char *&rule, BKNode *bkroot);

uint32_t mixColor( const char *&ctx, BKNode *bkroot);

uint32_t mixRgb( uint32_t lcolor, uint32_t rcolor);

void fillEasingMode( std::function<float(float)> &function, const char *&rule, BKNode *bkroot, char eoc);

uint32_t interpolateColor( uint32_t scolor, uint32_t ecolor, double progress);

uint32_t decodeColorName(const char *&ctx, BKNode *bkroot);

uint32_t rgbaToHsva( uint32_t rgb);

uint32_t hsvaToRgba( uint32_t hsv);

uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, double progress);

XyZColor xyzFromRgb( uint32_t color);

uint32_t xyzToRgb( XyZColor color);

KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best = nullptr, uint8_t depth = 0);

uint32_t editDistance( std::string_view main, std::string_view ref);

void insert( std::shared_ptr<KDNode> &node, Color color, size_t index = 0, uint8_t depth = 0);

void insert( std::shared_ptr<BKNode> &node, std::string_view word);

void findWordMatch( BKNode *node, const char *word, int threshold, std::vector<std::string>& matches);

std::vector<std::string> findWordMatch( BKNode *node, const char *word, int threshold = 4);

/*
 * Main dispatcher: Does all the rendering and display
 */

size_t countCharacters( std::string_view word);

bool ltrim( const char*& p);

uint32_t getNumber( const char *&ctx, uint8_t base = 10);

void render(std::string_view word, FT_Face face, size_t default_font_size, const char *raster_glyph, FILE *destination, bool as_image,
            const char *color_rule, const std::shared_ptr<KDNode>& root, const std::shared_ptr<BKNode> &bkroot);

void writePNG( FILE *cfp, const uint64_t *buffer, png_int_32 width, png_int_32 height);

void requestFontList();

#endif //COPYWRITE_HPP
