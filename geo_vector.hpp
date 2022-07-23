/***************************************************************************/
/*                                                                         */
/*  copywrite.cpp                                                          */
/*                                                                         */
/*    Driver program to create stamp					                   */
/*                                                                         */
/*  Copyright 2022 by Adesina Meekness                                     */
/*                                                                         */
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

#ifndef COPYWRITE__GEO_VECTOR_HPP_
#define COPYWRITE__GEO_VECTOR_HPP_

#include <cmath>

#define DEG_SCALE 180.f / M_PI
#define RAD_SCALE M_PI / 180.f
#define EPSILON	  1.e-7f

template <typename T = float, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
struct Vec2D
{
  T x, y;
  Vec2D( T x = {}, T y = {})
  : x( x), y( y)
  {
  }

  bool operator!=( const Vec2D& right) const
  {
    if constexpr ( std::is_floating_point_v<T>)
      return std::abs( x - right.x) >= EPSILON || std::abs( y - right.y) >= EPSILON;
 
	return x != right.x && y != right.y;
  }
  
  Vec2D& operator-()
  {
    x = -x;
    y = -y;
    return *this;
  }
  Vec2D operator+( Vec2D right) const
  {
    return { x + right.x, y + right.y};
  }
  
  Vec2D& operator+=( Vec2D right)
  {
    return *this = *this + right;
  }
  
  Vec2D operator-( Vec2D right) const
  {
    return { x - right.x, y - right.y};
  }

  Vec2D &operator-=( Vec2D right)
  {
    return *this = *this - right;
  }

  Vec2D operator/( Vec2D right) const
  {
    return { x / right.x, y / right.y};
  }
  
  T operator*( Vec2D right) const
  {
    return  x * right.x + y * right.y;
  }
  
  Vec2D& operator*=( Vec2D right)
  {
    return *this = *this * right;
  }

  [[nodiscard]] float length() const
  {
    return std::sqrt( x * x + y * y);
  }
  
  Vec2D unit() const
  {
    return { x / length(), y / length()};
  }
  
  Vec2D scaleBy( float scale) const
  {
    return { x * scale, y * scale};
  }
  
  [[nodiscard]] float angle() const
  {
    if( x < 0)
      return 270.f - ( std::atan2( y, -x) * DEG_SCALE);
    
    return 90 + ( std::atan2( y, x) * DEG_SCALE);
  }
  
  [[nodiscard]] auto rotate( int angle) const
  {
    auto rad = static_cast<float>( -angle * RAD_SCALE);
    return Vec2D<float>{
       std::cos( rad) * x + std::sin( rad) * y,
       
      -std::sin( rad) * x + std::cos( rad) * y
    };
  }
};

struct Vec3D
{
  float x{}, y{}, z{};
  Vec3D( float x, float y, float z);

  Vec3D operator*( float right) const;

  friend Vec3D operator*( float left, Vec3D right);

  Vec3D operator+( Vec3D right) const;

  Vec3D lerp( Vec3D end, float rate);
};

// Line Drawing utilities
struct OutputBuffer
{
  uint64_t *out{ nullptr};
  float width{}, height{};
};

#endif //COPYWRITE__GEO_VECTOR_HPP_
