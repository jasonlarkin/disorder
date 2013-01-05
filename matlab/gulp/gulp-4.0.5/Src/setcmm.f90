  subroutine setcmm
!
!  Sets up boxes for cell multipole method
!
!  nboxcmm = number of boxes in cell multipole method
!  nboxx   = number of boxes in x direction
!  nboxy   = number of boxes in y direction
!  nboxz   = number of boxes in z direction
!  nboxat  = box no. to which atom belongs
!  xboxo   = x box origin - cart coords at which boxes start
!  yboxo   = y box origin
!  zboxo   = z box origin
!  rbox    = box length for electrostatic case with no potentials
!  icmm    = CMM multipole level
!          = 1 => multipole
!          = 2 => dipole
!          = 3 => quadrupole
!          = 4 => octopole
!
!  Conditions of use:
!
!  GULP is available free of charge to academic institutions
!  and non-commerical establishments only. Copies should be
!  obtained from the author only and should not be distributed
!  in any form by the user to a third party without the express
!  permission of the author. This notice applies to all parts
!  of the program, except any library routines which are
!  distributed with the code for completeness. All rights for
!  such routines remain with the original distributor.
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use cellmultipole
  use control
  use current
  use iochannels
  use parallel
  use two
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ind
  integer(i4)        :: nx
  integer(i4)        :: ny
  integer(i4)        :: nz
  logical            :: lcdebug
  real(dp)           :: rbo
  real(dp)           :: rmax
  real(dp)           :: xi
  real(dp)           :: yi
  real(dp)           :: zi
  real(dp)           :: xmax
  real(dp)           :: ymax
  real(dp)           :: zmax
  real(dp)           :: xmid
  real(dp)           :: ymid
  real(dp)           :: zmid
  real(dp)           :: xmin
  real(dp)           :: ymin
  real(dp)           :: zmin
!
  lcdebug = (index(keyword,'dcmm').ne.0)
!*******************************
!  Find dimensions of cluster  *
!*******************************
  xmax =  - 1.0d10
  xmin = 1.0d10
  ymax =  - 1.0d10
  ymin = 1.0d10
  zmax =  - 1.0d10
  zmin = 1.0d10
  do i = 1,numat
    xi = xclat(i)
    yi = yclat(i)
    zi = zclat(i)
    xmax = max(xi,xmax)
    xmin = min(xi,xmin)
    ymax = max(yi,ymax)
    ymin = min(yi,ymin)
    zmax = max(zi,zmax)
    zmin = min(zi,zmin)
  enddo
!
!  Add small amount to sure that atoms lie within box region
!
  xmax = xmax + 0.1_dp
  ymax = ymax + 0.1_dp
  zmax = zmax + 0.1_dp
  xmin = xmin - 0.1_dp
  ymin = ymin - 0.1_dp
  zmin = zmin - 0.1_dp
!*****************
!  Set up boxes  *
!*****************
  xlength = xmax - xmin
  ylength = ymax - ymin
  zlength = zmax - zmin
  xmid = 0.5_dp*(xmax + xmin)
  ymid = 0.5_dp*(ymax + ymin)
  zmid = 0.5_dp*(zmax + zmin)
  rmax = max(rpmax,rbox)
  nboxx = (xlength/rmax) + 1
  nboxy = (ylength/rmax) + 1
  nboxz = (zlength/rmax) + 1
!
!  Make xlength, ylength and zlength into the box side lengths
!
  xlength = rmax
  ylength = rmax
  zlength = rmax
  if (mod(nboxx,2_i4).eq.0) then
!
!  Even number
!
    xboxo = xmid - xlength*(nboxx/2)
  else
!
!  Odd number
!
    rbo = nboxx
    xboxo = xmid - 0.5_dp*xlength*rbo
  endif
  if (mod(nboxy,2_i4).eq.0) then
!
!  Even number
!
    yboxo = ymid - ylength*(nboxy/2)
  else
!
!  Odd number
!
    rbo = nboxy
    yboxo = ymid - 0.5_dp*ylength*rbo
  endif
  if (mod(nboxz,2_i4).eq.0) then
!
!  Even number
!
    zboxo = zmid - zlength*(nboxz/2)
  else
!
!  Odd number
!
    rbo = nboxz
    zboxo = zmid - 0.5_dp*zlength*rbo
  endif
!
!  Set total number of boxes
!
  nboxcmm = nboxx*nboxy*nboxz
!***************************
!  Find box for each atom  *
!***************************
  do i = 1,numat
    xi = xclat(i) - xboxo
    yi = yclat(i) - yboxo
    zi = zclat(i) - zboxo
    nx = int(xi/xlength) + 1
    ny = int(yi/ylength) + 1
    nz = int(zi/zlength) + 1
    ind = nx + (ny - 1)*nboxx + (nz - 1)*nboxy*nboxx
    nboxat(i) = ind
  enddo
!
!  Debug printing
!
  if (lcdebug.and.ioproc) then
    write(ioout,'(/,''  Cell multipole method parameters :'',/)')
    write(ioout,'(''               No. of boxes      Box length     Origin  '',/)')
    write(ioout,'(''    x  '',10x,i4,8x,f9.2,8x,f7.2)') nboxx,xlength,xboxo
    write(ioout,'(''    y  '',10x,i4,8x,f9.2,8x,f7.2)') nboxy,ylength,yboxo
    write(ioout,'(''    z  '',10x,i4,8x,f9.2,8x,f7.2)') nboxz,zlength,zboxo
    write(ioout,'(/)')
  endif
!
  return
  end
