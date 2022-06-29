#ifndef COPYWRITE__GEO_VECTOR_HPP_
#define COPYWRITE__GEO_VECTOR_HPP_

#include <cmath>
struct Vec2D
{
  float x{}, y{};
  Vec2D(float x, float y);

  Vec2D operator-(Vec2D right) const;

  Vec2D &operator-=(Vec2D right);

  Vec2D operator/(Vec2D right) const;

  [[nodiscard]] float length() const;
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

#endif //COPYWRITE__GEO_VECTOR_HPP_
