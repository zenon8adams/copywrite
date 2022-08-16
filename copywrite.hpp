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

#include "config.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_STROKER_H
#include <string>
#include <vector>
#include <functional>
#include <memory>
#include <regex>
#include <cmath>
#include <variant>

#if HAVE_SYS_STAT_H
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <unistd.h>
#endif

#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)
    #if defined( __GNUC__) || defined( __clang__)
        #define swap32( x) __builtin_bswap32( x)
    #else
        #define swap32( x)  (( x >> 24) | (( x >> 8) & 0x0000FF00) | (( x << 8) & 0x00FF0000) | ( x << 24));
    #endif
    #if __LITTLE_ENDIAN
        #define PNG_ENDIAN( dword) swap32( dword)
    #else
        #define PNG_ENDIAN( dword) dword
    #endif
    #if defined( PNG_SUPPORTED)
        #include <image.hpp>
    #endif
    #if defined( JPG_SUPPORTED)
        #include <jpeglib.h>
    #endif
#endif

#if defined( CUSTOM_FONT_SUPPORTED)
#include <zip.h>
#endif

#include FT_FREETYPE_H
#include "geo_vector.hpp"

#define MAX_DIFF_TOLERANCE 20

typedef void( *DeleterType)( unsigned char *);

struct BBox
{
    BBox() = default;
    BBox(int left, int top, int right, int bottom)
            : xmin( left), xmax( right), ymin( top), ymax( bottom) { }

    void expandTo( int x, int y)
    {
        xmin = std::min( xmin, x);
        ymin = std::min( ymin, y);
        xmax = std::max( xmax, x);
        ymax = std::max( ymax, y);
    }

    [[nodiscard]] int width() const { return xmax - xmin + 1; }
    [[nodiscard]] int height() const { return ymax - ymin + 1; }

    int xmin{}, xmax{}, ymin{}, ymax{};
};
// A horizontal row of pixels generated by the FreeType renderer.
struct Span
{
    Span( int x, int y, int width, int coverage)
            : x( x), y( y), width( width), coverage( coverage)
    {
    }

    int x, y, width, coverage;
};

struct Figure
{
    std::unique_ptr<uint8_t[]> buffer;
    int left, top, width, height;
};

using Spans = std::vector<Span>;

struct ColorRule;

struct MonoGlyph
{
    std::pair<Figure, Spans> spans;
    std::shared_ptr<ColorRule> match;
    BBox bbox;
    FT_Vector advance;
    Vec2D<int> pos{};
    size_t level{};
    bool is_graph{ false};
};
typedef std::vector<MonoGlyph> MonoGlyphs;

struct RowDetail
{
    int baseline{ INT_MAX}, v_disp{},
            max_descent{}, width{};
    FT_Vector pen{ 0, 0};
};
typedef std::vector<RowDetail> RowDetails;

/*
 * Each row of the output is a span.
 * The `count` argument defines the number of pixels that should be painted `coverage
 * starting from `spans[i].x`
 */
void spansCallback( int y, int count, const FT_Span * spans, void * user);

void renderSpans( FT_Library library, FT_Outline * outline, Spans *spans);

enum class Justification
{
    Left   = -1,
    Right  = 01,
    Center = 02
};

template <typename T>
std::pair<std::vector<std::basic_string<T>>, int> expand( std::basic_string_view<T> provision, Justification mode);
uint32_t easeColor( const MonoGlyph &raster, const RowDetail &row_detail, Vec2D<int> size,
                    Vec2D<int> pos, FT_Vector pen, Vec2D<uint32_t> color_shift, bool is_outline);

enum class GradientType { Linear, Radial, Conic};

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

template <typename T, typename = void>
class PropertyProxy
{
 public:
  template <typename = std::enable_if<std::is_default_constructible_v<T>>>
  PropertyProxy()
  {
  }

  explicit PropertyProxy( T value)
  : value( value)
  {
  }

  operator T() const
  {
    return value;
  }

  [[nodiscard]] T cast()
  {
	return value;
  }

  T operator=( T another)
  {
    if( state_change_callable)
      std::invoke( state_change_callable, value);
    changed_since_initialization = true;
     return value = std::forward<T>( another);
  }

  [[nodiscard]] bool changed() const
  {
    return changed_since_initialization;
  }
private:
  T value;
  std::function<void( T)> state_change_callable;
  bool changed_since_initialization{ false};
};

template <typename T>
class PropertyProxy<T, std::enable_if_t<std::is_class_v<T>>> : public T
{
 public:
  template <typename = std::enable_if<std::is_default_constructible_v<T>>>
  PropertyProxy(): T()
  {
  }

  template <typename... Args>
  explicit PropertyProxy( Args&&... values)
	  : T( std::forward<T>( values)...)
  {
  }

    operator T()
  {
	return *this;
  }

  T& cast()
  {
    return *this;
  }

  T operator=( T another)
  {
    if( state_change_callable)
      std::invoke( state_change_callable, *static_cast<T*>( this));

	changed_since_initialization = true;
	return T::operator=( std::forward<T>( another));
  }

  [[nodiscard]] bool changed() const
  {
	return changed_since_initialization;
  }
 private:

  std::function<void( T)> state_change_callable;
  bool changed_since_initialization{ false};
};

struct ColorRule
{
    Vec2D<int32_t> start{ 1, 1}, end{ -1, -1};
    PropertyProxy<uint64_t> scolor{ 0x000000FFu}, ecolor{ 0x000000FFu};
    uint32_t font_size_b = UINT32_MAX, font_size_m = UINT32_MAX,
             font_size_e = UINT32_MAX;
    bool soak{ false};
    std::shared_ptr<BaseGradient> gradient{ new LinearGradient()};
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

struct ConicGradient : BaseGradient
{
  PropertyProxy<Vec2D<float>> origin{};
  std::vector<std::pair<uint64_t, size_t>> color_variations;
  ConicGradient() : BaseGradient{ .gradient_type = GradientType::Conic}
  {
  }
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
  enum class Group{ Easing, Color} group;
  std::shared_ptr<BKNode> next[ MAX_DIFF_TOLERANCE]{};
  explicit BKNode( std::string_view word, Group group)
  : word( word), group( group)
  {
  }
};

float clamp(float x, float lowerlimit, float upperlimit);

float smoothstep( float left, float right, float x);

template<typename Resource>
class PropertyManager
{
 public:
  template <typename MayBe_Resource, typename Deleter>
   PropertyManager( MayBe_Resource&& resource, Deleter&& deleter)
      : resource( std::forward<MayBe_Resource>( resource)),
        destructor( std::forward<Deleter>( deleter))
  {
  }
  template <typename Deleter>
  explicit PropertyManager( Deleter&& deleter)
  : destructor( std::forward<Deleter>( deleter))
  {
  }

  PropertyManager( const PropertyManager&) = delete;
  PropertyManager( PropertyManager&&) = default;

  auto& get()
  {
    return resource;
  }

  auto operator->()
  {
    return resource;
  }

  explicit operator bool() const
  {
    return resource;
  }

  ~PropertyManager()
  {
    if( destructor)
      std::invoke( destructor, resource);
  }

 private:
  Resource resource;
  std::function<void( Resource)> destructor;
};

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

/*
 * Glyph: Structural representation of a character
 */

struct Glyph
{
    FT_Int width{},
            height{},
            xstep{},
            ystep{},
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

/*
 * Build up a linked list of characters that
 * make up the sentence
 */

void insert( Glyph *&index, Glyph glyph);

/*
 * Render the given glyph into the final computed container
 */

template <typename Size_Class = uint64_t,
	      typename = std::enable_if_t<std::is_integral_v<Size_Class>
	                 || std::is_pointer<Size_Class>::value
	                 || std::is_array<Size_Class>::value>>
struct FrameBuffer
{
  std::shared_ptr<Size_Class>buffer;
  int32_t width, height, n_channel;
  PropertyManager<void *> metadata;
  FrameBuffer( std::shared_ptr<Size_Class> buffer = nullptr,
               int32_t width = {}, int32_t height = {}, int32_t bit_depth = {})
  : buffer( buffer), width( width), height( height), n_channel( bit_depth)
    , metadata( ( void *)nullptr, []( auto *p){ if( p) free( p);})
  {
  }
};

struct ApplicationHyperparameters;

/*
 * Display the monochrome canvas into stdout
 */

void write( FrameBuffer<uint32_t> &frame, const char *raster_glyph, FILE *destination, KDNode *root);

static size_t byteCount( uint8_t c );

static uint32_t collate( uint8_t *str, size_t idx, uint8_t count);

std::wstring toWString( std::string str);

template <size_t count>
std::array<float, count> parseFloats( const char *&rule);

std::vector<std::string> partition( std::string_view provision, std::string_view regexpr);

ConicGradient generateConicGradient( const char *&rule, const ColorRule& color_rule, BKNode *bkroot);

std::vector<ColorRule> parseColorRule( const char *rule, BKNode *bkroot);

void testColor( const char *rule, BKNode *bkroot);

uint64_t extractColor(const char *&rule, BKNode *bkroot);

uint64_t mixColor( const char *&ctx, BKNode *bkroot);

uint32_t sumMix(uint32_t lcolor, uint32_t rcolor);

uint32_t subMix( uint32_t lcolor, uint32_t rcolor);

void fillEasingMode( std::function<float(float)> &function, const char *&rule, BKNode *bkroot, char eoc);

uint32_t interpolateColor( uint32_t scolor, uint32_t ecolor, float progress);

uint32_t decodeColorName( const char *&ctx, BKNode *bkroot);

uint32_t rgbaToHsva( uint32_t rgb);

uint32_t hsvaToRgba( uint32_t hsv);

uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, float progress);

XyZColor xyzFromRgb( uint32_t color);

uint32_t xyzToRgb( XyZColor color);

KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best = nullptr, uint8_t depth = 0);

uint32_t editDistance( std::string_view main, std::string_view ref);

void insert( std::shared_ptr<KDNode> &node, Color color, size_t index = 0, uint8_t depth = 0);

void insert( std::shared_ptr<BKNode> &node, std::string_view word, BKNode::Group word_group);

void findWordMatch( BKNode *node, std::string_view word, BKNode::Group word_group,
				   int threshold, std::vector<std::string> &matches);

std::vector<std::string> findWordMatch( BKNode *node, std::string_view word, BKNode::Group word_group, int threshold);

bool ltrim( const char*& p);

uint64_t getNumber( const char *&ctx, uint8_t base = 10);

std::unordered_map<std::string_view, uint32_t>& colorCodeLookup();

std::string_view getColorNameAt( size_t pos);

struct CompositionRule
{
  enum class CompositionModel
  {
	Copy = 0,
	DestinationAtop,
	DestinationIn,
	DestinationOver,
	DestinationOut,
	Lighter,
	NotApplicable,
	SourceAtop,
	SourceIn,
	SourceOver,
	SourceOut,
	Xor
  }			   c_model{ CompositionModel::NotApplicable};
  enum class BlendModel
  {
    Normal = 0,
    Dissolve,
    Darken,
    Multiply,
    ColorBurn,
    LinearBurn,
    DarkerColor,
    Lighten,
    Screen,
    ColorDodge,
    LinearDodge,
    LighterColor,
    Overlay,
    SoftLight,
    HardLight,
    VividLight,
    LinearLight,
    PinLight,
    HardMix,
    Difference,
    Exclusion,
    Subtract,
    Divide,
    Hue,
    Saturation,
    Color,
    Luminosity
  }            b_model{ BlendModel::Normal};
  Vec2D<float> position{};
  int 	   	   angle{};
};

CompositionRule::CompositionModel selectCompositionModel( std::string_view given);

CompositionRule::BlendModel selectBlendModel( std::string_view given);

CompositionRule parseCompositionRule( std::string_view rule);

enum class OutputFormat
{
    PNG,
    JPEG
};

struct Padding
{
    int left{}, right{},
        top{}, bottom{};
};

struct ApplicationHyperparameters
{
  const char 			 *raster_glyph{ "\u2589"},
             			 *color_rule{ nullptr},
             			 *composition_rule{ nullptr},
             			 *src_filename{ nullptr},
    				     *dest_filename{ nullptr};
  std::shared_ptr<KDNode> kdroot;
  std::shared_ptr<BKNode> bkroot;
  size_t 				  font_size{ 10},
                          thickness{ 0};
  float                   line_height{ 1.15f};
  Justification           j_mode{ Justification::Left};
  PropertyProxy<uint32_t> background_color{};
  bool 					  as_image{ false};
  bool                    ease_col{ false};
  int                     image_quality{ 100},
                          dpi{ 120};
  Padding                 pad{};
  OutputFormat            out_format{ OutputFormat::PNG};
};

void render( FT_Library library, FT_Face face, std::wstring_view text, ApplicationHyperparameters& guide);

void draw( const MonoGlyphs &rasters, RowDetails& row_details,
           FrameBuffer<uint32_t> &frame, ApplicationHyperparameters& guide);

enum FilterMode
{
  SHARPEN,
  BOX_BLUR,
  GAUSSIAN_BLUR,
  B_FILTER_SENTINEL,
  GAUSSIAN_BLUR_x5,
  E_FILTER_SENTINEL
};

bool intersects( std::array<Vec2D<float>, 4> corners, Vec2D<float> test);

void applyFilter( FrameBuffer<uint32_t> &frame, uint8_t filter);

#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)

void composite( ApplicationHyperparameters &guide, FrameBuffer<uint32_t> &s_frame);

#endif

#if defined( JPG_SUPPORTED)

bool isJPEG( std::string_view filename);

FrameBuffer<uint8_t> readJPEG( std::string_view filename);

void writeJPEG( std::string filename, FrameBuffer<uint32_t> &frame, int quality);

#endif

#if defined( PNG_SUPPORTED)

FrameBuffer<uint8_t> readPNG( std::string_view filename);

void writePNG( std::string filename, FrameBuffer<uint32_t> &frame, int quality);

#endif

void requestFontList();

#if defined( CUSTOM_FONT_SUPPORTED)

std::pair<std::string, std::string> requestFontInfo( std::string_view font_file);

void installFont( std::string_view font_file);

enum class FontActivity
{
    Read,
    Delete
};

void executeActionOnFont( std::string_view font_name, FontActivity mode, std::function<void( std::vector<void *>)> fn);

void uninstallFont( std::string_view font);

std::pair<int64_t, std::unique_ptr<uint8_t, DeleterType>> useInstalledFont( std::string_view font_name);

#endif

std::string getFontFile( std::string_view font);

#endif //COPYWRITE_HPP
