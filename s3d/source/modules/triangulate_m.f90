#include "globalDefines.h"
!$Id: triangulate_m.f90,v 1.1.2.6 2007/06/04 22:07:54 rsankar Exp $
!----------------------------------------------------------------------
! To extract iso-surface from volume data
! Triangulation using a variant of marching cubes 
! 
! *********** WRITTEN BY RAMANAN SANKARAN ************
!
! Tables are from the following url 
! http://astronomy.swin.edu.au/~pbourke/modelling/polygonise/
!
! Written for a true 3d data - will not work on 2d data
! 
!----------------------------------------------------------------------
module triangulate_m
implicit none

private

public triangulate_field, triangulate_field_skip
public vertex_coords, vertex_interpolate, vertex_area_weights, vertex_normals

!----------------------------------------------------------------------
! edgetable is not used. But I will leave it in here.
!integer, parameter :: edgetable(256) = (/               &
!  Z'0'  , Z'109', Z'203', Z'30a', Z'406', Z'50f', Z'605', Z'70c', &
!  Z'80c', Z'905', Z'a0f', Z'b06', Z'c0a', Z'd03', Z'e09', Z'f00', &
!  Z'190', Z'99' , Z'393', Z'29a', Z'596', Z'49f', Z'795', Z'69c', &
!  Z'99c', Z'895', Z'b9f', Z'a96', Z'd9a', Z'c93', Z'f99', Z'e90', &
!  Z'230', Z'339', Z'33' , Z'13a', Z'636', Z'73f', Z'435', Z'53c', &
!  Z'a3c', Z'b35', Z'83f', Z'936', Z'e3a', Z'f33', Z'c39', Z'd30', &
!  Z'3a0', Z'2a9', Z'1a3', Z'aa' , Z'7a6', Z'6af', Z'5a5', Z'4ac', &
!  Z'bac', Z'aa5', Z'9af', Z'8a6', Z'faa', Z'ea3', Z'da9', Z'ca0', &
!  Z'460', Z'569', Z'663', Z'76a', Z'66' , Z'16f', Z'265', Z'36c', &
!  Z'c6c', Z'd65', Z'e6f', Z'f66', Z'86a', Z'963', Z'a69', Z'b60', &
!  Z'5f0', Z'4f9', Z'7f3', Z'6fa', Z'1f6', Z'ff' , Z'3f5', Z'2fc', &
!  Z'dfc', Z'cf5', Z'fff', Z'ef6', Z'9fa', Z'8f3', Z'bf9', Z'af0', &
!  Z'650', Z'759', Z'453', Z'55a', Z'256', Z'35f', Z'55' , Z'15c', &
!  Z'e5c', Z'f55', Z'c5f', Z'd56', Z'a5a', Z'b53', Z'859', Z'950', &
!  Z'7c0', Z'6c9', Z'5c3', Z'4ca', Z'3c6', Z'2cf', Z'1c5', Z'cc' , &
!  Z'fcc', Z'ec5', Z'dcf', Z'cc6', Z'bca', Z'ac3', Z'9c9', Z'8c0', &
!  Z'8c0', Z'9c9', Z'ac3', Z'bca', Z'cc6', Z'dcf', Z'ec5', Z'fcc', &
!  Z'cc' , Z'1c5', Z'2cf', Z'3c6', Z'4ca', Z'5c3', Z'6c9', Z'7c0', &
!  Z'950', Z'859', Z'b53', Z'a5a', Z'd56', Z'c5f', Z'f55', Z'e5c', &
!  Z'15c', Z'55' , Z'35f', Z'256', Z'55a', Z'453', Z'759', Z'650', &
!  Z'af0', Z'bf9', Z'8f3', Z'9fa', Z'ef6', Z'fff', Z'cf5', Z'dfc', &
!  Z'2fc', Z'3f5', Z'ff' , Z'1f6', Z'6fa', Z'7f3', Z'4f9', Z'5f0', &
!  Z'b60', Z'a69', Z'963', Z'86a', Z'f66', Z'e6f', Z'd65', Z'c6c', &
!  Z'36c', Z'265', Z'16f', Z'66' , Z'76a', Z'663', Z'569', Z'460', &
!  Z'ca0', Z'da9', Z'ea3', Z'faa', Z'8a6', Z'9af', Z'aa5', Z'bac', &
!  Z'4ac', Z'5a5', Z'6af', Z'7a6', Z'aa' , Z'1a3', Z'2a9', Z'3a0', &
!  Z'd30', Z'c39', Z'f33', Z'e3a', Z'936', Z'83f', Z'b35', Z'a3c', &
!  Z'53c', Z'435', Z'73f', Z'636', Z'13a', Z'33' , Z'339', Z'230', &
!  Z'e90', Z'f99', Z'c93', Z'd9a', Z'a96', Z'b9f', Z'895', Z'99c', &
!  Z'69c', Z'795', Z'49f', Z'596', Z'29a', Z'393', Z'99' , Z'190', &
!  Z'f00', Z'e09', Z'd03', Z'c0a', Z'b06', Z'a0f', Z'905', Z'80c', &
!  Z'70c', Z'605', Z'50f', Z'406', Z'30a', Z'203', Z'109', Z'0'    /)
!------------------------------------------------------------------
integer, parameter :: tritable(16,0:255) = reshape( (/           &
 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  1,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  8,  3,  9,  8,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  3,  1,  2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  9,  2, 10,  0,  2,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  2,  8,  3,  2, 10,  8, 10,  9,  8, -1, -1, -1, -1, -1, -1, -1, &
  3, 11,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0, 11,  2,  8, 11,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  9,  0,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1, 11,  2,  1,  9, 11,  9,  8, 11, -1, -1, -1, -1, -1, -1, -1, &
  3, 10,  1, 11, 10,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0, 10,  1,  0,  8, 10,  8, 11, 10, -1, -1, -1, -1, -1, -1, -1, &
  3,  9,  0,  3, 11,  9, 11, 10,  9, -1, -1, -1, -1, -1, -1, -1, &
  9,  8, 10, 10,  8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  7,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  3,  0,  7,  3,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  1,  9,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  1,  9,  4,  7,  1,  7,  3,  1, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 10,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  3,  4,  7,  3,  0,  4,  1,  2, 10, -1, -1, -1, -1, -1, -1, -1, &
  9,  2, 10,  9,  0,  2,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1, &
  2, 10,  9,  2,  9,  7,  2,  7,  3,  7,  9,  4, -1, -1, -1, -1, &
  8,  4,  7,  3, 11,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 11,  4,  7, 11,  2,  4,  2,  0,  4, -1, -1, -1, -1, -1, -1, -1, &
  9,  0,  1,  8,  4,  7,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1, &
  4,  7, 11,  9,  4, 11,  9, 11,  2,  9,  2,  1, -1, -1, -1, -1, &
  3, 10,  1,  3, 11, 10,  7,  8,  4, -1, -1, -1, -1, -1, -1, -1, &
  1, 11, 10,  1,  4, 11,  1,  0,  4,  7, 11,  4, -1, -1, -1, -1, &
  4,  7,  8,  9,  0, 11,  9, 11, 10, 11,  0,  3, -1, -1, -1, -1, &
  4,  7, 11,  4, 11,  9,  9, 11, 10, -1, -1, -1, -1, -1, -1, -1, &
  9,  5,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  9,  5,  4,  0,  8,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  5,  4,  1,  5,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  8,  5,  4,  8,  3,  5,  3,  1,  5, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 10,  9,  5,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  3,  0,  8,  1,  2, 10,  4,  9,  5, -1, -1, -1, -1, -1, -1, -1, &
  5,  2, 10,  5,  4,  2,  4,  0,  2, -1, -1, -1, -1, -1, -1, -1, &
  2, 10,  5,  3,  2,  5,  3,  5,  4,  3,  4,  8, -1, -1, -1, -1, &
  9,  5,  4,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0, 11,  2,  0,  8, 11,  4,  9,  5, -1, -1, -1, -1, -1, -1, -1, &
  0,  5,  4,  0,  1,  5,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1, &
  2,  1,  5,  2,  5,  8,  2,  8, 11,  4,  8,  5, -1, -1, -1, -1, &
 10,  3, 11, 10,  1,  3,  9,  5,  4, -1, -1, -1, -1, -1, -1, -1, &
  4,  9,  5,  0,  8,  1,  8, 10,  1,  8, 11, 10, -1, -1, -1, -1, &
  5,  4,  0,  5,  0, 11,  5, 11, 10, 11,  0,  3, -1, -1, -1, -1, &
  5,  4,  8,  5,  8, 10, 10,  8, 11, -1, -1, -1, -1, -1, -1, -1, &
  9,  7,  8,  5,  7,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  9,  3,  0,  9,  5,  3,  5,  7,  3, -1, -1, -1, -1, -1, -1, -1, &
  0,  7,  8,  0,  1,  7,  1,  5,  7, -1, -1, -1, -1, -1, -1, -1, &
  1,  5,  3,  3,  5,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  9,  7,  8,  9,  5,  7, 10,  1,  2, -1, -1, -1, -1, -1, -1, -1, &
 10,  1,  2,  9,  5,  0,  5,  3,  0,  5,  7,  3, -1, -1, -1, -1, &
  8,  0,  2,  8,  2,  5,  8,  5,  7, 10,  5,  2, -1, -1, -1, -1, &
  2, 10,  5,  2,  5,  3,  3,  5,  7, -1, -1, -1, -1, -1, -1, -1, &
  7,  9,  5,  7,  8,  9,  3, 11,  2, -1, -1, -1, -1, -1, -1, -1, &
  9,  5,  7,  9,  7,  2,  9,  2,  0,  2,  7, 11, -1, -1, -1, -1, &
  2,  3, 11,  0,  1,  8,  1,  7,  8,  1,  5,  7, -1, -1, -1, -1, &
 11,  2,  1, 11,  1,  7,  7,  1,  5, -1, -1, -1, -1, -1, -1, -1, &
  9,  5,  8,  8,  5,  7, 10,  1,  3, 10,  3, 11, -1, -1, -1, -1, &
  5,  7,  0,  5,  0,  9,  7, 11,  0,  1,  0, 10, 11, 10,  0, -1, &
 11, 10,  0, 11,  0,  3, 10,  5,  0,  8,  0,  7,  5,  7,  0, -1, &
 11, 10,  5,  7, 11,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 10,  6,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  3,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  9,  0,  1,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  8,  3,  1,  9,  8,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1, &
  1,  6,  5,  2,  6,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  6,  5,  1,  2,  6,  3,  0,  8, -1, -1, -1, -1, -1, -1, -1, &
  9,  6,  5,  9,  0,  6,  0,  2,  6, -1, -1, -1, -1, -1, -1, -1, &
  5,  9,  8,  5,  8,  2,  5,  2,  6,  3,  2,  8, -1, -1, -1, -1, &
  2,  3, 11, 10,  6,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 11,  0,  8, 11,  2,  0, 10,  6,  5, -1, -1, -1, -1, -1, -1, -1, &
  0,  1,  9,  2,  3, 11,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1, &
  5, 10,  6,  1,  9,  2,  9, 11,  2,  9,  8, 11, -1, -1, -1, -1, &
  6,  3, 11,  6,  5,  3,  5,  1,  3, -1, -1, -1, -1, -1, -1, -1, &
  0,  8, 11,  0, 11,  5,  0,  5,  1,  5, 11,  6, -1, -1, -1, -1, &
  3, 11,  6,  0,  3,  6,  0,  6,  5,  0,  5,  9, -1, -1, -1, -1, &
  6,  5,  9,  6,  9, 11, 11,  9,  8, -1, -1, -1, -1, -1, -1, -1, &
  5, 10,  6,  4,  7,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  3,  0,  4,  7,  3,  6,  5, 10, -1, -1, -1, -1, -1, -1, -1, &
  1,  9,  0,  5, 10,  6,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1, &
 10,  6,  5,  1,  9,  7,  1,  7,  3,  7,  9,  4, -1, -1, -1, -1, &
  6,  1,  2,  6,  5,  1,  4,  7,  8, -1, -1, -1, -1, -1, -1, -1, &
  1,  2,  5,  5,  2,  6,  3,  0,  4,  3,  4,  7, -1, -1, -1, -1, &
  8,  4,  7,  9,  0,  5,  0,  6,  5,  0,  2,  6, -1, -1, -1, -1, &
  7,  3,  9,  7,  9,  4,  3,  2,  9,  5,  9,  6,  2,  6,  9, -1, &
  3, 11,  2,  7,  8,  4, 10,  6,  5, -1, -1, -1, -1, -1, -1, -1, &
  5, 10,  6,  4,  7,  2,  4,  2,  0,  2,  7, 11, -1, -1, -1, -1, &
  0,  1,  9,  4,  7,  8,  2,  3, 11,  5, 10,  6, -1, -1, -1, -1, &
  9,  2,  1,  9, 11,  2,  9,  4, 11,  7, 11,  4,  5, 10,  6, -1, &
  8,  4,  7,  3, 11,  5,  3,  5,  1,  5, 11,  6, -1, -1, -1, -1, &
  5,  1, 11,  5, 11,  6,  1,  0, 11,  7, 11,  4,  0,  4, 11, -1, &
  0,  5,  9,  0,  6,  5,  0,  3,  6, 11,  6,  3,  8,  4,  7, -1, &
  6,  5,  9,  6,  9, 11,  4,  7,  9,  7, 11,  9, -1, -1, -1, -1, &
 10,  4,  9,  6,  4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4, 10,  6,  4,  9, 10,  0,  8,  3, -1, -1, -1, -1, -1, -1, -1, &
 10,  0,  1, 10,  6,  0,  6,  4,  0, -1, -1, -1, -1, -1, -1, -1, &
  8,  3,  1,  8,  1,  6,  8,  6,  4,  6,  1, 10, -1, -1, -1, -1, &
  1,  4,  9,  1,  2,  4,  2,  6,  4, -1, -1, -1, -1, -1, -1, -1, &
  3,  0,  8,  1,  2,  9,  2,  4,  9,  2,  6,  4, -1, -1, -1, -1, &
  0,  2,  4,  4,  2,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  8,  3,  2,  8,  2,  4,  4,  2,  6, -1, -1, -1, -1, -1, -1, -1, &
 10,  4,  9, 10,  6,  4, 11,  2,  3, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  2,  2,  8, 11,  4,  9, 10,  4, 10,  6, -1, -1, -1, -1, &
  3, 11,  2,  0,  1,  6,  0,  6,  4,  6,  1, 10, -1, -1, -1, -1, &
  6,  4,  1,  6,  1, 10,  4,  8,  1,  2,  1, 11,  8, 11,  1, -1, &
  9,  6,  4,  9,  3,  6,  9,  1,  3, 11,  6,  3, -1, -1, -1, -1, &
  8, 11,  1,  8,  1,  0, 11,  6,  1,  9,  1,  4,  6,  4,  1, -1, &
  3, 11,  6,  3,  6,  0,  0,  6,  4, -1, -1, -1, -1, -1, -1, -1, &
  6,  4,  8, 11,  6,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  7, 10,  6,  7,  8, 10,  8,  9, 10, -1, -1, -1, -1, -1, -1, -1, &
  0,  7,  3,  0, 10,  7,  0,  9, 10,  6,  7, 10, -1, -1, -1, -1, &
 10,  6,  7,  1, 10,  7,  1,  7,  8,  1,  8,  0, -1, -1, -1, -1, &
 10,  6,  7, 10,  7,  1,  1,  7,  3, -1, -1, -1, -1, -1, -1, -1, &
  1,  2,  6,  1,  6,  8,  1,  8,  9,  8,  6,  7, -1, -1, -1, -1, &
  2,  6,  9,  2,  9,  1,  6,  7,  9,  0,  9,  3,  7,  3,  9, -1, &
  7,  8,  0,  7,  0,  6,  6,  0,  2, -1, -1, -1, -1, -1, -1, -1, &
  7,  3,  2,  6,  7,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  2,  3, 11, 10,  6,  8, 10,  8,  9,  8,  6,  7, -1, -1, -1, -1, &
  2,  0,  7,  2,  7, 11,  0,  9,  7,  6,  7, 10,  9, 10,  7, -1, &
  1,  8,  0,  1,  7,  8,  1, 10,  7,  6,  7, 10,  2,  3, 11, -1, &
 11,  2,  1, 11,  1,  7, 10,  6,  1,  6,  7,  1, -1, -1, -1, -1, &
  8,  9,  6,  8,  6,  7,  9,  1,  6, 11,  6,  3,  1,  3,  6, -1, &
  0,  9,  1, 11,  6,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  7,  8,  0,  7,  0,  6,  3, 11,  0, 11,  6,  0, -1, -1, -1, -1, &
  7, 11,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  7,  6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  3,  0,  8, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  1,  9, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  8,  1,  9,  8,  3,  1, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1, &
 10,  1,  2,  6, 11,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 10,  3,  0,  8,  6, 11,  7, -1, -1, -1, -1, -1, -1, -1, &
  2,  9,  0,  2, 10,  9,  6, 11,  7, -1, -1, -1, -1, -1, -1, -1, &
  6, 11,  7,  2, 10,  3, 10,  8,  3, 10,  9,  8, -1, -1, -1, -1, &
  7,  2,  3,  6,  2,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  7,  0,  8,  7,  6,  0,  6,  2,  0, -1, -1, -1, -1, -1, -1, -1, &
  2,  7,  6,  2,  3,  7,  0,  1,  9, -1, -1, -1, -1, -1, -1, -1, &
  1,  6,  2,  1,  8,  6,  1,  9,  8,  8,  7,  6, -1, -1, -1, -1, &
 10,  7,  6, 10,  1,  7,  1,  3,  7, -1, -1, -1, -1, -1, -1, -1, &
 10,  7,  6,  1,  7, 10,  1,  8,  7,  1,  0,  8, -1, -1, -1, -1, &
  0,  3,  7,  0,  7, 10,  0, 10,  9,  6, 10,  7, -1, -1, -1, -1, &
  7,  6, 10,  7, 10,  8,  8, 10,  9, -1, -1, -1, -1, -1, -1, -1, &
  6,  8,  4, 11,  8,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  3,  6, 11,  3,  0,  6,  0,  4,  6, -1, -1, -1, -1, -1, -1, -1, &
  8,  6, 11,  8,  4,  6,  9,  0,  1, -1, -1, -1, -1, -1, -1, -1, &
  9,  4,  6,  9,  6,  3,  9,  3,  1, 11,  3,  6, -1, -1, -1, -1, &
  6,  8,  4,  6, 11,  8,  2, 10,  1, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 10,  3,  0, 11,  0,  6, 11,  0,  4,  6, -1, -1, -1, -1, &
  4, 11,  8,  4,  6, 11,  0,  2,  9,  2, 10,  9, -1, -1, -1, -1, &
 10,  9,  3, 10,  3,  2,  9,  4,  3, 11,  3,  6,  4,  6,  3, -1, &
  8,  2,  3,  8,  4,  2,  4,  6,  2, -1, -1, -1, -1, -1, -1, -1, &
  0,  4,  2,  4,  6,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  9,  0,  2,  3,  4,  2,  4,  6,  4,  3,  8, -1, -1, -1, -1, &
  1,  9,  4,  1,  4,  2,  2,  4,  6, -1, -1, -1, -1, -1, -1, -1, &
  8,  1,  3,  8,  6,  1,  8,  4,  6,  6, 10,  1, -1, -1, -1, -1, &
 10,  1,  0, 10,  0,  6,  6,  0,  4, -1, -1, -1, -1, -1, -1, -1, &
  4,  6,  3,  4,  3,  8,  6, 10,  3,  0,  3,  9, 10,  9,  3, -1, &
 10,  9,  4,  6, 10,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  9,  5,  7,  6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  3,  4,  9,  5, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1, &
  5,  0,  1,  5,  4,  0,  7,  6, 11, -1, -1, -1, -1, -1, -1, -1, &
 11,  7,  6,  8,  3,  4,  3,  5,  4,  3,  1,  5, -1, -1, -1, -1, &
  9,  5,  4, 10,  1,  2,  7,  6, 11, -1, -1, -1, -1, -1, -1, -1, &
  6, 11,  7,  1,  2, 10,  0,  8,  3,  4,  9,  5, -1, -1, -1, -1, &
  7,  6, 11,  5,  4, 10,  4,  2, 10,  4,  0,  2, -1, -1, -1, -1, &
  3,  4,  8,  3,  5,  4,  3,  2,  5, 10,  5,  2, 11,  7,  6, -1, &
  7,  2,  3,  7,  6,  2,  5,  4,  9, -1, -1, -1, -1, -1, -1, -1, &
  9,  5,  4,  0,  8,  6,  0,  6,  2,  6,  8,  7, -1, -1, -1, -1, &
  3,  6,  2,  3,  7,  6,  1,  5,  0,  5,  4,  0, -1, -1, -1, -1, &
  6,  2,  8,  6,  8,  7,  2,  1,  8,  4,  8,  5,  1,  5,  8, -1, &
  9,  5,  4, 10,  1,  6,  1,  7,  6,  1,  3,  7, -1, -1, -1, -1, &
  1,  6, 10,  1,  7,  6,  1,  0,  7,  8,  7,  0,  9,  5,  4, -1, &
  4,  0, 10,  4, 10,  5,  0,  3, 10,  6, 10,  7,  3,  7, 10, -1, &
  7,  6, 10,  7, 10,  8,  5,  4, 10,  4,  8, 10, -1, -1, -1, -1, &
  6,  9,  5,  6, 11,  9, 11,  8,  9, -1, -1, -1, -1, -1, -1, -1, &
  3,  6, 11,  0,  6,  3,  0,  5,  6,  0,  9,  5, -1, -1, -1, -1, &
  0, 11,  8,  0,  5, 11,  0,  1,  5,  5,  6, 11, -1, -1, -1, -1, &
  6, 11,  3,  6,  3,  5,  5,  3,  1, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 10,  9,  5, 11,  9, 11,  8, 11,  5,  6, -1, -1, -1, -1, &
  0, 11,  3,  0,  6, 11,  0,  9,  6,  5,  6,  9,  1,  2, 10, -1, &
 11,  8,  5, 11,  5,  6,  8,  0,  5, 10,  5,  2,  0,  2,  5, -1, &
  6, 11,  3,  6,  3,  5,  2, 10,  3, 10,  5,  3, -1, -1, -1, -1, &
  5,  8,  9,  5,  2,  8,  5,  6,  2,  3,  8,  2, -1, -1, -1, -1, &
  9,  5,  6,  9,  6,  0,  0,  6,  2, -1, -1, -1, -1, -1, -1, -1, &
  1,  5,  8,  1,  8,  0,  5,  6,  8,  3,  8,  2,  6,  2,  8, -1, &
  1,  5,  6,  2,  1,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  3,  6,  1,  6, 10,  3,  8,  6,  5,  6,  9,  8,  9,  6, -1, &
 10,  1,  0, 10,  0,  6,  9,  5,  0,  5,  6,  0, -1, -1, -1, -1, &
  0,  3,  8,  5,  6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 10,  5,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 11,  5, 10,  7,  5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 11,  5, 10, 11,  7,  5,  8,  3,  0, -1, -1, -1, -1, -1, -1, -1, &
  5, 11,  7,  5, 10, 11,  1,  9,  0, -1, -1, -1, -1, -1, -1, -1, &
 10,  7,  5, 10, 11,  7,  9,  8,  1,  8,  3,  1, -1, -1, -1, -1, &
 11,  1,  2, 11,  7,  1,  7,  5,  1, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  3,  1,  2,  7,  1,  7,  5,  7,  2, 11, -1, -1, -1, -1, &
  9,  7,  5,  9,  2,  7,  9,  0,  2,  2, 11,  7, -1, -1, -1, -1, &
  7,  5,  2,  7,  2, 11,  5,  9,  2,  3,  2,  8,  9,  8,  2, -1, &
  2,  5, 10,  2,  3,  5,  3,  7,  5, -1, -1, -1, -1, -1, -1, -1, &
  8,  2,  0,  8,  5,  2,  8,  7,  5, 10,  2,  5, -1, -1, -1, -1, &
  9,  0,  1,  5, 10,  3,  5,  3,  7,  3, 10,  2, -1, -1, -1, -1, &
  9,  8,  2,  9,  2,  1,  8,  7,  2, 10,  2,  5,  7,  5,  2, -1, &
  1,  3,  5,  3,  7,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  7,  0,  7,  1,  1,  7,  5, -1, -1, -1, -1, -1, -1, -1, &
  9,  0,  3,  9,  3,  5,  5,  3,  7, -1, -1, -1, -1, -1, -1, -1, &
  9,  8,  7,  5,  9,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  5,  8,  4,  5, 10,  8, 10, 11,  8, -1, -1, -1, -1, -1, -1, -1, &
  5,  0,  4,  5, 11,  0,  5, 10, 11, 11,  3,  0, -1, -1, -1, -1, &
  0,  1,  9,  8,  4, 10,  8, 10, 11, 10,  4,  5, -1, -1, -1, -1, &
 10, 11,  4, 10,  4,  5, 11,  3,  4,  9,  4,  1,  3,  1,  4, -1, &
  2,  5,  1,  2,  8,  5,  2, 11,  8,  4,  5,  8, -1, -1, -1, -1, &
  0,  4, 11,  0, 11,  3,  4,  5, 11,  2, 11,  1,  5,  1, 11, -1, &
  0,  2,  5,  0,  5,  9,  2, 11,  5,  4,  5,  8, 11,  8,  5, -1, &
  9,  4,  5,  2, 11,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  2,  5, 10,  3,  5,  2,  3,  4,  5,  3,  8,  4, -1, -1, -1, -1, &
  5, 10,  2,  5,  2,  4,  4,  2,  0, -1, -1, -1, -1, -1, -1, -1, &
  3, 10,  2,  3,  5, 10,  3,  8,  5,  4,  5,  8,  0,  1,  9, -1, &
  5, 10,  2,  5,  2,  4,  1,  9,  2,  9,  4,  2, -1, -1, -1, -1, &
  8,  4,  5,  8,  5,  3,  3,  5,  1, -1, -1, -1, -1, -1, -1, -1, &
  0,  4,  5,  1,  0,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  8,  4,  5,  8,  5,  3,  9,  0,  5,  0,  3,  5, -1, -1, -1, -1, &
  9,  4,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4, 11,  7,  4,  9, 11,  9, 10, 11, -1, -1, -1, -1, -1, -1, -1, &
  0,  8,  3,  4,  9,  7,  9, 11,  7,  9, 10, 11, -1, -1, -1, -1, &
  1, 10, 11,  1, 11,  4,  1,  4,  0,  7,  4, 11, -1, -1, -1, -1, &
  3,  1,  4,  3,  4,  8,  1, 10,  4,  7,  4, 11, 10, 11,  4, -1, &
  4, 11,  7,  9, 11,  4,  9,  2, 11,  9,  1,  2, -1, -1, -1, -1, &
  9,  7,  4,  9, 11,  7,  9,  1, 11,  2, 11,  1,  0,  8,  3, -1, &
 11,  7,  4, 11,  4,  2,  2,  4,  0, -1, -1, -1, -1, -1, -1, -1, &
 11,  7,  4, 11,  4,  2,  8,  3,  4,  3,  2,  4, -1, -1, -1, -1, &
  2,  9, 10,  2,  7,  9,  2,  3,  7,  7,  4,  9, -1, -1, -1, -1, &
  9, 10,  7,  9,  7,  4, 10,  2,  7,  8,  7,  0,  2,  0,  7, -1, &
  3,  7, 10,  3, 10,  2,  7,  4, 10,  1, 10,  0,  4,  0, 10, -1, &
  1, 10,  2,  8,  7,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  9,  1,  4,  1,  7,  7,  1,  3, -1, -1, -1, -1, -1, -1, -1, &
  4,  9,  1,  4,  1,  7,  0,  8,  1,  8,  7,  1, -1, -1, -1, -1, &
  4,  0,  3,  7,  4,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  4,  8,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  9, 10,  8, 10, 11,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  3,  0,  9,  3,  9, 11, 11,  9, 10, -1, -1, -1, -1, -1, -1, -1, &
  0,  1, 10,  0, 10,  8,  8, 10, 11, -1, -1, -1, -1, -1, -1, -1, &
  3,  1, 10, 11,  3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  2, 11,  1, 11,  9,  9, 11,  8, -1, -1, -1, -1, -1, -1, -1, &
  3,  0,  9,  3,  9, 11,  1,  2,  9,  2, 11,  9, -1, -1, -1, -1, &
  0,  2, 11,  8,  0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  3,  2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  2,  3,  8,  2,  8, 10, 10,  8,  9, -1, -1, -1, -1, -1, -1, -1, &
  9, 10,  2,  0,  9,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  2,  3,  8,  2,  8, 10,  0,  1,  8,  1, 10,  8, -1, -1, -1, -1, &
  1, 10,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  1,  3,  8,  9,  1,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  9,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
  0,  3,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 /), &
 (/16,256/) )
!----------------------------------------------------------------------
!----------------------------------------------------------------------
integer, public, parameter ::  X_EDGE = 1,  Y_EDGE = 2,  Z_EDGE = 3
integer, parameter :: ARRAY_SIZE_INC = 40000
!----------------------------------------
! Vertex table
real, public, pointer, dimension(:) :: vert_ratio
integer, public, pointer, dimension(:,:) :: vert_loc 
integer, public, pointer, dimension(:) :: vert_dirn
integer, public :: vert_count = 0
!----------------------------------------
! Field size based on which vertex table was created
integer :: vert_nx=0, vert_ny=0, vert_nz=0 
integer :: vert_xskp=1, vert_yskp=1, vert_zskp=1
!----------------------------------------
! Triangle table
integer, public, pointer, dimension(:,:) :: triangle
integer, public :: trng_count = 0
!----------------------------------------------------------------------
!----------------------------------------------------------------------

contains
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Routines to manage the size of the arrays and expand as required
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine expand_triangle_arrays(req_sz)
implicit none
integer, intent(in) :: req_sz
call expand_int3_array(triangle, req_sz)
end subroutine expand_triangle_arrays

!----------------------------------------------------------------------
subroutine expand_vertex_arrays(req_sz)
implicit none
integer, intent(in) :: req_sz
call expand_real_array(vert_ratio, req_sz)
call expand_int3_array(vert_loc, req_sz)
call expand_int_array(vert_dirn, req_sz)
end subroutine expand_vertex_arrays

!----------------------------------------------------------------------
subroutine expand_real_array(A, req_sz)
implicit none

real, pointer, dimension(:) :: A
integer, intent(in) :: req_sz
real, pointer, dimension(:) :: dummy

if(.not.associated(A)) allocate(A(ARRAY_SIZE_INC))
do while (size(A) < req_sz)
  allocate(dummy(size(A)+ARRAY_SIZE_INC))
  dummy(1:size(A)) = A
  deallocate(A)
  A => dummy
  nullify(dummy)
end do 
end subroutine expand_real_array
!----------------------------------------
subroutine expand_int_array(A, req_sz)
implicit none

integer, pointer, dimension(:) :: A
integer, intent(in) :: req_sz
integer, pointer, dimension(:) :: dummy

if(.not.associated(A)) allocate(A(ARRAY_SIZE_INC))
do while (size(A) < req_sz)
  allocate(dummy(size(A)+ARRAY_SIZE_INC))
  dummy(1:size(A)) = A
  deallocate(A)
  A => dummy
  nullify(dummy)
end do 
end subroutine expand_int_array
!----------------------------------------
subroutine expand_int3_array(A, req_sz)
implicit none
integer, pointer, dimension(:,:) :: A
integer, intent(in) :: req_sz
integer, pointer, dimension(:,:) :: dummy
  
if(.not.associated(A)) allocate(A(3, ARRAY_SIZE_INC))
do while (size(A, dim=2) < req_sz)
  allocate(dummy(3, size(A,dim=2)+ARRAY_SIZE_INC))
  dummy(1:3, 1:size(A,dim=2)) = A(1:3,:)
  deallocate(A)
  A => dummy
  nullify(dummy)
end do 
end subroutine expand_int3_array
!----------------------------------------------------------------------
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine reset_surface_data
implicit none

if(associated(vert_ratio)) deallocate(vert_ratio)
if(associated(vert_loc)) deallocate(vert_loc)
if(associated(vert_dirn)) deallocate(vert_dirn)
vert_count = 0

if(associated(triangle)) deallocate(triangle)
trng_count = 0

vert_nx = 0
vert_ny = 0
vert_nz = 0
vert_xskp = 1
vert_yskp = 1
vert_zskp = 1

return
end subroutine reset_surface_data
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine triangulate_field(nx, ny, nz, field, isolevel)
implicit none

integer, intent(in) :: nx, ny, nz
real, intent(in) :: field(nx, ny, nz), isolevel

integer, target, dimension(0:11, nx-1, ny-1) :: plane_a, plane_b
integer, pointer, dimension(:, :, :) :: last_plane, curr_plane, dummy

integer i, j, k
real f(0:1,0:1,0:1)

!wipe it clean before forming new triangles
call reset_surface_data

last_plane => plane_a
curr_plane => plane_b
nullify(dummy)

loop_k: do k = 1, nz
  !swap cell plane pointers
  dummy => last_plane
  last_plane => curr_plane
  curr_plane => dummy
  nullify(dummy)
  !Clear curr_plane for error detection
  curr_plane(:,:,:) = -1
  !----------------------------------------
  !X edges
  xloop_j: do j = 1, ny
    xloop_i: do i = 1, nx-1
      if(vertex_on_edge(i, j, k, X_edge)) then
        if( j>1  ) last_plane(6, i, j-1) = vert_count
        if( j<ny ) last_plane(4, i, j  ) = vert_count
        if( j>1  ) curr_plane(2, i, j-1) = vert_count
        if( j<ny ) curr_plane(0, i, j  ) = vert_count
      end if
    end do xloop_i
  end do xloop_j
  !----------------------------------------
  !Y edges
  yloop_j: do j = 1, ny-1
    yloop_i: do i = 1, nx
      if(vertex_on_edge(i, j, k, Y_edge)) then
        if( i>1  ) last_plane(5, i-1, j) = vert_count
        if( i<nx ) last_plane(7, i  , j) = vert_count
        if( i>1  ) curr_plane(1, i-1, j) = vert_count
        if( i<nx ) curr_plane(3, i  , j) = vert_count
      end if
    end do yloop_i
  end do yloop_j
  !----------------------------------------
  !Z edges
  zloop_j: do j = 1, ny
    zloop_i: do i = 1, nx
      if(k==nz) exit zloop_j
      if(vertex_on_edge(i, j, k, Z_edge)) then
        if( i<nx .and. j<ny ) curr_plane(8 , i  , j  ) = vert_count
        if( i>1  .and. j<ny ) curr_plane(9 , i-1, j  ) = vert_count
        if( i>1  .and. j>1  ) curr_plane(10, i-1, j-1) = vert_count
        if( i<nx .and. j>1  ) curr_plane(11, i  , j-1) = vert_count
      end if
    end do zloop_i
  end do zloop_j
  !----------------------------------------
  ! The last plane is ready to triangulate
  ! but, the current plane is not!
  if(k==1) cycle loop_k
  tloop_j: do j=1, ny-1
    tloop_i: do i=1, nx-1
      f(0:1,0:1,0:1) = field(i:i+1,j:j+1,k-1:k)
      call triangulate_single_cell (cubeindex(f), last_plane(:,i,j))
    end do tloop_i
  end do tloop_j
  !----------------------------------------
end do loop_k
vert_nx = nx
vert_ny = ny
vert_nz = nz

return

contains
!----------------------------------------------------------------------

  !----------------------------------------
  !----------------------------------------
  !----------------------------------------
  logical function vertex_on_edge(i, j, k, dirn) result (exst)
  implicit none
  integer, intent(in) :: i, j, k, dirn
  real :: f0, f1
  
  f0 = field(i, j, k)
  select case(dirn)
    case(X_EDGE)
      f1 = field(i+1,j,k)
    case(Y_EDGE)
      f1 = field(i,j+1,k)
    case(Z_EDGE)
      f1 = field(i,j,k+1)
  end select
  
  if( f0<isolevel .neqv. f1<isolevel) then
    !I found an intersection on this edge :-)
    call expand_vertex_arrays(vert_count+1)
    vert_count = vert_count+1
    vert_loc(1, vert_count) = i
    vert_loc(2, vert_count) = j
    vert_loc(3, vert_count) = k
    vert_dirn(vert_count) = dirn
    vert_ratio(vert_count) = (f1-isolevel)/(f1-f0)
    exst = .true.
  else
    exst = .false.
  end if
  
  return
  end function vertex_on_edge
  !----------------------------------------
  !----------------------------------------
  ! Compute the cubeindex for a single cell
  integer function cubeindex(field) 
  implicit none
  real, intent(in) :: field(0:1,0:1,0:1)
  
  cubeindex = 0
  if (field(0,0,0) < isolevel) cubeindex = ibset(cubeindex, 0)
  if (field(1,0,0) < isolevel) cubeindex = ibset(cubeindex, 1)
  if (field(1,1,0) < isolevel) cubeindex = ibset(cubeindex, 2)
  if (field(0,1,0) < isolevel) cubeindex = ibset(cubeindex, 3)
  if (field(0,0,1) < isolevel) cubeindex = ibset(cubeindex, 4)
  if (field(1,0,1) < isolevel) cubeindex = ibset(cubeindex, 5)
  if (field(1,1,1) < isolevel) cubeindex = ibset(cubeindex, 6)
  if (field(0,1,1) < isolevel) cubeindex = ibset(cubeindex, 7)
  return
  end function cubeindex
  !----------------------------------------
  !----------------------------------------
  ! This subroutine triangulates a single cell
  ! edgvert stores the pointer (just an integer) to the 
  ! intersection vertex that resides on that edge
  
  ! A maximum of 5 triangles are created
  ! ntri is the number of triangles created.
  !----------------------------------------
  subroutine triangulate_single_cell(cubeindex, edgvert)
  implicit none
  integer, intent(in) :: cubeindex, edgvert(0:11)
  integer i
  
  call expand_triangle_arrays(trng_count+5)
  loop: do i = 1, 16, 3
    if(tritable(i, cubeindex) == -1) exit loop
    trng_count = trng_count+1
    triangle(1,trng_count) = edgvert(tritable(i  ,cubeindex))
    triangle(2,trng_count) = edgvert(tritable(i+1,cubeindex))
    triangle(3,trng_count) = edgvert(tritable(i+2,cubeindex))
  end do loop
  return
  end subroutine triangulate_single_cell
  !----------------------------------------
  !----------------------------------------
end subroutine triangulate_field

!----------------------------------------------------------------------
subroutine triangulate_field_skip & 
           (nx, ny, nz, field, isolevel, xskip, yskip, zskip)
implicit none
integer, intent(in) :: nx, ny, nz
real, intent(in) :: field(nx, ny, nz), isolevel
integer, intent(in) :: xskip, yskip, zskip

integer :: mx, my, mz
integer :: xindx(nx), yindx(ny), zindx(nz)
real, allocatable :: field_skipped(:,:,:)
integer i, j, k

if( xskip==1 .and. yskip==1 .and. zskip==1) then
  call triangulate_field(nx, ny, nz, field, isolevel)
  return
end if

call skip_one_dirn(nx, xskip, mx, xindx)
call skip_one_dirn(ny, yskip, my, yindx)
call skip_one_dirn(nz, zskip, mz, zindx)

allocate(field_skipped(mx, my, mz))
do k = 1, mz
  do j = 1, my
    do i = 1, mx
      field_skipped(i, j, k) = field(xindx(i), yindx(j), zindx(k))
    end do
  end do
end do

call triangulate_field(mx, my, mz, field_skipped, isolevel)
!Since the vertex table refers to the reduced field, adjust the indices
vert_nx = nx
vert_ny = ny
vert_nz = nz
vert_xskp = xskip
vert_yskp = yskip
vert_zskp = zskip
do i = 1, vert_count
  vert_loc(1, i) = xindx(vert_loc(1, i))
  vert_loc(2, i) = yindx(vert_loc(2, i))
  vert_loc(3, i) = zindx(vert_loc(3, i))
end do

deallocate(field_skipped)
return

contains
  !----------------------------------------
  subroutine skip_one_dirn(n, skip, m, indx)
  implicit none
  integer, intent(in) :: n, skip
  integer, intent(out) :: m, indx(1:n)

  integer i 
  
  indx(:) = -1 ! To catch errors

  if(skip==1) then
    m = n
    indx(1:n) = (/ (i, i=1, n) /)
    return
  end if

  m = (n-2)/skip + 2
  indx(1:m-1) = (/ ( 1+skip*(i-1), i = 1, m-1) /)
  indx(m) = n

  return
  end subroutine skip_one_dirn
  !----------------------------------------
end subroutine triangulate_field_skip


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Functions to interpolate any function to the triangle vertices
! and to determine the vertex coords
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine vertex_interpolate(field, field_vert)
implicit none
real, intent(in) :: field(1:vert_nx, 1:vert_ny, 1:vert_nz)
real, intent(out) :: field_vert(1:vert_count)

integer n

do n = 1, vert_count
  field_vert(n) = single_vertex_field(n)
end do

return
contains
  !----------------------------------------
  real function single_vertex_field(n) result(f)
  implicit none
  integer, intent(in) :: n
  
  real f0, f1, r
  integer i, j, k
  
  i = vert_loc(1, n)
  j = vert_loc(2, n)
  k = vert_loc(3, n)
  r = vert_ratio(n)
  
  f0 = field(i, j, k)
  
  select case(vert_dirn(n))
    case(X_EDGE)
        f1 = field(min(vert_loc(1, n)+vert_xskp, vert_nx), j, k)
    case(Y_EDGE)
        f1 = field(i, min(vert_loc(2, n)+vert_yskp, vert_ny), k)
    case(Z_EDGE)
        f1 = field(i, j, min(vert_loc(3, n)+vert_zskp, vert_nz))
  end select
  
  f = r*f0 + (1.0-r)*f1
  
  return
  end function single_vertex_field
  !----------------------------------------
end subroutine vertex_interpolate

!----------------------------------------------------------------------
! The following functions assume a cartesian grid to obtain the coords
! For a structured grid, treat x, y and z as any other function
!----------------------------------------------------------------------
subroutine vertex_coords(xg, yg, zg, xv, yv, zv) 
implicit none
real, intent(in) :: xg(1:vert_nx), yg(1:vert_ny), zg(1:vert_nz)
real, intent(out), dimension(1:vert_count) :: xv, yv, zv

integer n

do n = 1, vert_count
  xv(n) = single_vertex_xloc(n, xg)
  yv(n) = single_vertex_yloc(n, yg)
  zv(n) = single_vertex_zloc(n, zg)
end do

return
contains
  !----------------------------------------
  real function single_vertex_xloc(n, xg) result(x)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: xg(1:vert_nx)
  
  real r 
  integer np1
  
  if(vert_dirn(n) .eq. X_EDGE) then
    r = vert_ratio(n)
    np1 = min(vert_loc(1, n)+vert_xskp, vert_nx)
    x = r*xg(vert_loc(1, n)) + (1.0-r)*xg(np1)
  else
    x = xg(vert_loc(1, n))
  end if
  
  return
  end function single_vertex_xloc
  !----------------------------------------
  real function single_vertex_yloc(n, yg) result(y)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: yg(1:vert_ny)
  
  real r 
  integer np1
  
  if(vert_dirn(n) .eq. Y_EDGE) then
    r = vert_ratio(n)
    np1 = min(vert_loc(2, n)+vert_yskp, vert_ny)
    y = r*yg(vert_loc(2, n)) + (1.0-r)*yg(np1)
  else
    y = yg(vert_loc(2, n))
  end if
  
  return
  end function single_vertex_yloc
  !----------------------------------------
  real function single_vertex_zloc(n, zg) result(z)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: zg(1:vert_nz)
  
  real r 
  integer np1
  
  if(vert_dirn(n) .eq. Z_EDGE) then
    r = vert_ratio(n)
    np1 = min(vert_loc(3, n)+vert_zskp, vert_nz)
    z = r*zg(vert_loc(3, n)) + (1.0-r)*zg(np1)
  else
    z = zg(vert_loc(3, n))
  end if
  
  return
  end function single_vertex_zloc
  !----------------------------------------
end subroutine vertex_coords

!----------------------------------------------------------------------
subroutine vertex_area_weights(xg, yg, zg, wt)
implicit none
real, intent(in) :: xg(:), yg(:), zg(:)
real, intent(out) ::wt(vert_count)

real xv(vert_count), yv(vert_count), zv(vert_count)
integer n
real area

wt(:) = 0.0
call vertex_coords(xg, yg, zg, xv, yv, zv)

do n = 1, trng_count
 area = single_triangle_area(triangle(:,n))
 area = area/3.0
 wt(triangle(1,n)) = wt(triangle(1,n))+area
 wt(triangle(2,n)) = wt(triangle(2,n))+area
 wt(triangle(3,n)) = wt(triangle(3,n))+area
end do

return

contains
  !----------------------------------------
  real function single_triangle_area(v) result(area)
  implicit none
  integer, intent(in) :: v(1:3)

  real ax, ay, az
  real bx, by, bz
  real cx, cy, cz

  ax = xv(v(2)) - xv(v(1))
  ay = yv(v(2)) - yv(v(1))
  az = zv(v(2)) - zv(v(1))

  bx = xv(v(3)) - xv(v(1))
  by = yv(v(3)) - yv(v(1))
  bz = zv(v(3)) - zv(v(1))

  cx = ay*bz - az*by
  cy = az*bx - ax*bz
  cz = ax*by - ay*bx

  area = sqrt(cx*cx+cy*cy+cz*cz)
  area = area/2.0

  return
  end function single_triangle_area
  !----------------------------------------

end subroutine vertex_area_weights

!----------------------------------------------------------------------
! The normal will point towards the direction of increasing field
! `flip' will make it point otherwise.
subroutine vertex_normals(xv, yv, zv, flip, vert_norm)
implicit none
real, intent(in), dimension(vert_count) :: xv, yv, zv
logical, intent(in) :: flip
real, intent(out) :: vert_norm(3, vert_count)

real normal(3) 
real mag
integer n

vert_norm(:,:) = 0.0

do n = 1, trng_count
 call single_triangle_normal(triangle(:,n), normal)
 vert_norm(:, triangle(1,n)) = vert_norm(:, triangle(1,n))+normal(:)
 vert_norm(:, triangle(2,n)) = vert_norm(:, triangle(2,n))+normal(:)
 vert_norm(:, triangle(3,n)) = vert_norm(:, triangle(3,n))+normal(:)
end do

do n = 1, vert_count
  mag = sqrt(sum(vert_norm(:,n)**2))
  if(mag .ne.0) vert_norm(:,n) = vert_norm(:,n)/mag
end do

if(flip) normal = -normal

return

contains
  !----------------------------------------
  subroutine single_triangle_normal(v, n)
  implicit none
  integer, intent(in) :: v(1:3)
  real, intent(out) :: n(1:3)

  real ax, ay, az
  real bx, by, bz

  ax = xv(v(2)) - xv(v(1))
  ay = yv(v(2)) - yv(v(1))
  az = zv(v(2)) - zv(v(1))

  bx = xv(v(3)) - xv(v(1))
  by = yv(v(3)) - yv(v(1))
  bz = zv(v(3)) - zv(v(1))

  n(1) = ay*bz - az*by
  n(2) = az*bx - ax*bz
  n(3) = ax*by - ay*bx

  return
  end subroutine single_triangle_normal
  !----------------------------------------

end subroutine vertex_normals

end module triangulate_m
