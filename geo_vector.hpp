#ifndef COPYWRITE__GEO_VECTOR_HPP_
#define COPYWRITE__GEO_VECTOR_HPP_


#include <cmath>
template <typename T = float, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
struct Vec2D
{
  T x{}, y{};
  Vec2D( T x, T y)
  : x( x), y( y)
  {
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

  [[nodiscard]] float length() const
  {
    return std::sqrt( x * x + y * y);
  }
  
  float angle( Vec2D right) const
  {
    auto interm = ( *this * right) / ( length() * right.length());
    return std::acos( interm) + ( interm < 0) * M_PI;
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
