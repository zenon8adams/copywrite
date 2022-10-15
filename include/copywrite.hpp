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

#if defined( __GNUC__) || defined( __clang__)
    #define popcount8( x) __builtin_popcount( x)
#else
    #define popcount8(x)                                \
        do {                                            \
              uint8_t v = x;                            \
              v = ( v & 0x55) << 1 | (( v >> 1) & 0x55); \
              v = ( v & 0x33) << 1 | (( v >> 1) & 0x33); \
              v = ( v & 0x0F) << 1 | (( v >> 1) & 0x0F); \
        }while( 0);
#endif

#if PNG_SUPPORTED || JPG_SUPPORTED
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
    #if PNG_SUPPORTED
        #include <png++/rgba_pixel.hpp>
        #include <png++/image.hpp>
    #endif
    #if JPG_SUPPORTED
        #include <jpeglib.h>
    #endif
#endif

#if CUSTOM_FONT_SUPPORTED
#include <zip.h>
#endif

#include FT_FREETYPE_H

#define ALLOWANCE                      2
#define MAX(x, y)                      ((x) ^ (((x) ^ (y)) & -((x) < (y))))
#define MIN(x, y)                      ((x) ^ (((x) ^ (y)) & -((x) > (y))))
#define ZERO( fl)                      ( std::abs( fl) <= EPSILON)
#define EQUAL( al, bl)                 ZERO( ( al) - ( bl))
#define UNSET( x)                      (( x) == -1)
#define ACCESSIBLE( ptr)               (( ptr) != nullptr)
#define RED( color)                    (( uint8_t)(( color) >> 24u))
#define GREEN( color)                  (( uint8_t)((( color) >> 16u) & 0xFFu))
#define BLUE( color)                   (( uint8_t)((( color) >> 8u) & 0xFFu))
#define ALPHA( color)                  (( uint8_t)(( color) & 0xFFu))
#define RGBA( red, green, blue, alpha) (((( uint32_t)(( uint8_t)red))  << 24u) |\
                                       ((( uint32_t)(( uint8_t)green)) << 16u) |\
                                       ((( uint32_t)(( uint8_t)blue))  << 8u) | (( uint8_t)alpha))
#define RGB( red, green, blue)         RGBA( red, green, blue, 0u)
#define LUMIN( color)                  (( uint8_t)(( color) >> 8u)  & 0x7Fu)
#define SAT( color)                    (( uint8_t)(( color) >> 15u) & 0x7Fu)
#define HUE( color)                    (( uint16_t)(( color) >> 22u))
#define HSLA( hue, sat, lum, alpha)    ((( uint32_t)( hue)) << 22u |\
                                       (( uint32_t)( sat))  << 15u |\
                                       (( uint32_t)( lum))  << 8u | alpha)
#define SCALE_RGB( color, scale)       RGB( RED( color) * ( scale), GREEN( color) * ( scale), BLUE( color) * ( scale))
#define XYZ_SCALE                      775
#define RGB_SCALE                      255
#define DEG_MAX                        360
#define HALF_RGB_SCALE                 128
#define ENUM_CAST( idx)				   ( static_cast<uint8_t>( idx))
#define FLOAT_CAST( value)             ( static_cast<float>( value))
#define INT_CAST( value)               ( static_cast<int>( value))
#define From26Dot6( value)             (( value) / 64)
#define To26Dot6( value)               (( value) * 64)
#define MODEL_ENUM( mode)			   CompositionRule::CompositionModel::mode
#define BLEND_ENUM( mode)              CompositionRule::BlendModel::mode
/*
 * The composition table is made up of 2 bit field
 * per flag for CompositionModel.
 * E.g Copy has an index of 1, and a value of 10.
 * The lower zero from the right means that the size of the canvas should be maximum or minimum( 1 or 0).
 * The rightmost zero indicates that if the first bit is maximum, then the size should be that
 * of the source or destination( 1 or 0).
 * The fields are:
 * Clip 		   = 00,
 * Copy            = 01
 * DestinationAtop = 02,
 * DestinationIn   = 03,
 * DestinationOver = 04,
 * DestinationOut  = 05,
 * Lighter         = 06,
 * NotApplicable   = 07,
 * SourceAtop      = 08,
 * SourceIn        = 09,
 * SourceOver      = 10,
 * SourceOut       = 11,
 * Xor             = 12
 * COMPOSITION_TABLE LAYOUT:
 * 01 10 01 00 00 00 01 00 01 10 10 10 10
 * 12 11 10 09 08 07 06 05 04 03 02 01 00
 * 01 1001 0000 0001 0001 1010 1010
 */
#define COMPOSITION_TABLE		       0x19011AAU
#define COMPOSITON_SIZE( idx)          (( COMPOSITION_TABLE >> ( ENUM_CAST( idx) * 2U)) & 1U)
#define COMPOSITION_SIDE( idx)		   (( COMPOSITION_TABLE >> ( ENUM_CAST( idx) * 2U + 1U)) & 1U)
#define LOW_BYTE( value)               (( uint8_t)(( value) & 0xFFu))
#define HIGH_BYTE( value)              (( uint8_t)(( value) >> 8u))
#define MAKE_WORD( high, low)          (( uint16_t)( high) << 8u | ( low))
#define LOW_DWORD( value)              (( uint32_t)(( value) & 0xFFFFFFFFu))
#define HIGH_DWORD( value)             (( uint32_t)(( value) >> 32u))
#define MAKE_QWORD( high, low)         (( uint64_t)( high) << 32u | ( low))

#define FOUND_STRING( xpr)             (( xpr) == 0)

#define STR_LIGHTER                    "lighter"
#define STR_DARKER                     "darker"

#define GLOBAL_TIME_ID                 0

/*
 * CommandLineParser -> ApplicationDirector
 * TextCodec
 * TextColorizer
 * TextRenderer
 * LayerRenderer    <- Plugin
 * ColorSpaceConverter
 * LocalFontManager <- Plugin
 * ImageManager     <- Plugin
 */

#define IMAGE_MANAGER                  "ImageManager"
#define LOCAL_FONT_MANAGER             "LocalFontManager"
#define LAYER_RENDERER                 "LayerRenderer"

#include "geometry/geo_vector.hpp"

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

    int x, y, width,
        coverage;       // Used by Freetype Renderer for antialiasing.
};

struct Figure
{
    std::unique_ptr<uint8_t[]> buffer;
    int left, top, width, height;
};

using Spans = std::vector<Span>;

struct ColorRule;

/*
 * Stores information about each character making up the text.
 */
struct MonoGlyph
{
    std::pair<Figure, Spans> spans;     // Figure object represent fill, Spans represents the outline.
    std::shared_ptr<ColorRule> match;   // The matching rule that will be used in processing the character
    BBox bbox;                          // The tight bounding box of the character.
    FT_Vector advance;                  // The increment in positioning the next character
    Vec2D<int> pos{};                   // The glyph identity. The coordinate of the glyph.
    size_t level{};                     // The current row this glyph belongs.
    bool is_graph{ false};              // Indicates that the glyph is a printable character ( e.g. is not space)
};
typedef std::vector<MonoGlyph> MonoGlyphs;

/*
 *  Stores information common to each row.
 */
struct RowDetail
{
    int       baseline{ INT_MAX},     // The underline position for the text
              v_disp{},               // Used for multiline text to represent the cumulative vertical offset.
              max_descent{},          // The maximum depth from the underline position character in this row can reach.
              width{},                // The sum of with of all glyphs making up the row.
              max_shadow_y{},
              length{};               // Number of characters making up this row.
    FT_Vector pen{ 0, 0};             // The cursor that changes as each character of this row are drawn.
};
typedef std::vector<RowDetail> RowDetails;

enum class Justification
{
    Left   = -1,
    Right  = 01,
    Center = 02
};

/*
 *  Drop in replacement for object with extra notification parameter.
 *  It can detect change in value of the given type since its construction.
 */
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
    T value{};
    std::function<void( T)> state_change_callable;
    bool changed_since_initialization{ false};
};

/*
 * State change detector for class types.
 * It detects if the object has been reinitialized since construction.
 */
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

enum class GradientType { Linear, Radial, Conic};

struct BaseGradient
{
  int startx, width, height;
  GradientType gradient_type;
  BaseGradient( GradientType type = GradientType::Linear, int sx = {}, int w = {}, int h = {})
  : startx( sx), width( w), height( h), gradient_type( type)
  {
  }

  virtual ~BaseGradient()
  {
  }
};

struct RadialGradient : BaseGradient
{
  Vec3D props;
  RadialGradient( float x, float y, float z)
	  : BaseGradient( GradientType::Radial), props( x, y, z)
  {
  }
};

struct LinearGradient : BaseGradient
{
};

struct ConicGradient : BaseGradient
{
    PropertyProxy<Vec2D<float>> origin{};
    std::vector<std::pair<uint64_t, size_t>> color_variations;
    ConicGradient() : BaseGradient( GradientType::Conic)
    {
    }
};

/*
 * Defines the rule to be used to paint a given set of characters.
 */
struct ColorRule
{
    Vec2D<int32_t> start{ 1, 1},    // The start index of the character the rule applies to.
                   end{ -1, -1};   //  The end index of the character the rule applies to.
   /*
    * `scolor` and `ecolor` are used for color based transitions.
    */
    PropertyProxy<uint64_t> scolor{ 0x000000FFu}, // The start color for the outline/fill for each character.
                            ecolor{ 0x000000FFu}; // The end color for the outline/fill for each character.
   /*
    */
    uint32_t shadow_color{ 0x000000FFu};
    /*
     * Font size variations to apply to the characters matching this rule.
     */
    uint32_t font_size_b = UINT32_MAX, font_size_m = UINT32_MAX,
             font_size_e = UINT32_MAX, max_bounce{};
    /*
     * Indicates if the color should apply to each character as a whole or to each pixel making up the character.
     */
    bool soak{ false};
    /*
     */
    Vec3D shadow;
    /*
     * Gradient used in painting.
     */
    std::shared_ptr<BaseGradient> gradient{ new BaseGradient()};
    /*
     * Specifies how the font and color should vary.
     */
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

/*
 *  Stores information about colors in a KDTree.
 *  KDTrees are used in searching in spatial domain for the closest
 *  match of a point to other set of points in space.
 */
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

/*
 *  Stores the closest words in a BKTree.
 *  The MAX_DIFF_TOLERANCE width indicates the closeness metric.
 *  BKTree:: Walter Austin Burkhard and Robert M. Keller
 * is an approximate word search data structure.
 */

struct BKNode
{
  std::string_view word;
  enum class Group{ Easing, Color} group;
  std::shared_ptr<BKNode> next[ MAX_DIFF_TOLERANCE]{};
  explicit BKNode( std::string_view word, Group group)
  : word( word), group( group)
  {
  }
};;

/*
 *  Implements a loose ownership semantics.
 *  Holds this resource. If the resource is still valid when the deleter
 *  is about to be called, delete it.
 */
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
  std::vector<float> metadata;
  FrameBuffer( std::shared_ptr<Size_Class> buffer = nullptr,
               int32_t width = {}, int32_t height = {}, int32_t bit_depth = {})
  : buffer( buffer), width( width), height( height), n_channel( bit_depth)
  {
  }
};

/*
 * Display the monochrome canvas into stdout
 */

enum class SpecialEffect;

/*
 * Predefined positioning on a plain.
 */
enum class SnapPosition
{
    TopLeft,
    TopCenter,
    TopRight,
    LeftCenter,
    Center,
    RightCenter,
    BottomLeft,
    BottomCenter,
    BottomRight
};

struct CompositionRule
{
  enum class CompositionModel
  {
    Clip = 0,
	Copy,
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
  }	                      c_model{ CompositionModel::NotApplicable};
  /*
   * Pixel transformation modes used for blending of one over the other.
   */
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
  };
  /*
   * Indicates the surface to apply the rule on
   * Top::  Top surface
   * Base:: Base surface
   * Both:: Top and Base surface.
   */
  enum class StickyArena { Top, Base, Both};
  /*
   * Collection of BlendModels. e.g. Multiply|ColorBurn
   */
  std::deque<BlendModel>                                  b_models;
  /*
   * Holds the absolute positioning of the top surface.
   */
  Vec2D<float>                                            position{ INFINITY, INFINITY};
  /*
   * Holds the decoded snap positioning.
   */
  Vec2D<float>                                            snap;
  /*
   * Specifies the angle of rotation of the top surface before compositing.
   */
  int 	   	                                              angle{};
  /*
   * The extra surface used for compositing.
   */
  std::string                                             image;
  /*
   * Holds a collection of effects to be applied on this surface.
   */
  std::deque<std::tuple<SpecialEffect, StickyArena, int, std::string>> s_effects;
  /*
   * Specifies the final width of this composited surface. This is used by the **resize** routine.
   */
  int                                                     interpolation[ 3]{};
};

using StickyArena = CompositionRule::StickyArena;

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

enum class SpecialEffect
{
    Blur,
    Sharpen,
    Emboss,
    Oil,
    RequiresKernelSentinel,
    GrayScale,
    Grainy,
    Twirl
};

struct SpecialEffectArgs
{
    // Kernel used by convolution based effect.
    std::vector<float> kernel;
    // Used to specify the area this effect is to affect.
    Vec2D<int> start{ 0, 0}, extent{ -1, -1};
    /*
     * Twirl effect parameters
     */
    Vec2D<float> twirl_center{ .5f, .5f};
    int twirl_strength{ 10}, twirl_radius{ 10};
    float twirl_rotation{};
    /*
     * Oil painting parameters.
     */
    int oil_intensity{ 1}, grain_multiplicity{ 1};
    /*
     */
    bool inplace{};
};

struct ApplicationDirector
{
    const char 			   *raster_glyph{ "\u2589"},
                           *color_rule{ nullptr},
                           *composition_rule{ nullptr},
                           *src_filename{ nullptr};
    std::string             font_profile;
    std::string_view        text;
    std::shared_ptr<KDNode> kdroot;
    std::shared_ptr<BKNode> bkroot;
    size_t 				    font_size{ 10},
                            thickness{ 0};
    float                   line_height{ 1.15f};
    Justification           j_mode{ Justification::Left};
    PropertyProxy<uint32_t> background_color{};
    bool 					as_image{ false};
    bool                    ease_col{ false};
    bool                    shadow_present{ false};
    int                     interpolation[ 3]{};  //[0] - width, [1] - height, [2] -> { 0 - bilinear, 1 - bicubic}
    int                     image_quality{ 100},
                            dpi{ 120};
    Padding                 pad{};
    OutputFormat            out_format{ OutputFormat::PNG};
};

/*
 * Used by **LocalFontManager** to specify
 * action to be performed on installed fonts.
 */
enum class FontActivity
{
    Read,
    Delete
};

#endif //COPYWRITE_HPP
