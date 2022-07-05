#include <cstdint>
#include <iostream>
#include "geo_vector.hpp"

Vec3D::Vec3D( float x, float y, float z)
    : x( x), y( y), z( z)
{
}
Vec3D Vec3D::operator*( float right) const
{
  return { x * right, y * right, z * right};
}

Vec3D operator*( float left, Vec3D right)
{
  return right * left;
}

Vec3D Vec3D::operator+( Vec3D right) const
{
  return { x + right.x, y + right.y, z + right.z};
}

Vec3D Vec3D::lerp( Vec3D end, float rate)
{
  return *this * rate + ( 1.f - rate) * end;
}