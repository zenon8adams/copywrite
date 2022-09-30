/***************************************************************************/
/*                                                                         */
/*  geo_vector.cpp                                                         */
/*                                                                         */
/*    Vec3D class                    					                   */
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

#include <cstdint>
#include <iostream>
#include "geometry/geo_vector.hpp"

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

