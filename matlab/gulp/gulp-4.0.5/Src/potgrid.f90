  subroutine potgrid
!
!  Subroutine for evaluating electrostatic potential on a grid
!
!   2/97 Put cuts to small value during this option so that
!        potential near nuclei is correct. Also don't add
!        extra point in each direction for molecules.
!   2/99 Extra point when wrapping round the unit cell removed
!        as for a line or plane calculation this is messy.
!   6/00 efgpg properly dimensioned as (6) 
!   8/01 Call to epot0/3 replaced with generic call to epot
!   6/05 Number of steps incremented by 1 if step size > 0.0
!   6/05 Format for output of grid size changed 
!  12/07 Unused variables removed
!   7/11 Modified to handle centred cell symmetry
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, July 2011
!
  use control
  use current
  use iochannels
  use potentialgrid
  use parallel
  use shell
  use symmetry,      only : rop, w1, vit
  use times
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: nx
  integer(i4)                                  :: ny
  integer(i4)                                  :: nz
  logical                                      :: lfirstpg
  logical                                      :: lgrad2
  real(dp)                                     :: cputime
  real(dp)                                     :: efgpg(6)
  real(dp)                                     :: savecuts
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: vpg
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
  real(dp)                                     :: xs
  real(dp)                                     :: ys
  real(dp)                                     :: zs
  real(dp)                                     :: xmax
  real(dp)                                     :: ymax
  real(dp)                                     :: zmax
  real(dp)                                     :: xmin
  real(dp)                                     :: ymin
  real(dp)                                     :: zmin
  real(dp)                                     :: xsite
  real(dp)                                     :: ysite
  real(dp)                                     :: zsite
  real(dp)                                     :: xstep
  real(dp)                                     :: ystep
  real(dp)                                     :: zstep
  real(dp)                                     :: v(3)
  real(dp)                                     :: x(3)
  real(dp)                                     :: xx(3)
!
  time1 = cputime()
  lfirstpg = .true.
  lgrad2 = .false.
!********************************
!  Initialise local parameters  *
!********************************
  nx = nxpg(ncf)
  ny = nypg(ncf)
  nz = nzpg(ncf)
  xmin = xminpg(ncf)
  ymin = yminpg(ncf)
  zmin = zminpg(ncf)
  xmax = xmaxpg(ncf)
  ymax = ymaxpg(ncf)
  zmax = zmaxpg(ncf)
  xstep = (xmax - xmin)/dble(nx)
  ystep = (ymax - ymin)/dble(ny)
  zstep = (zmax - zmin)/dble(nz)
!
!  Increment nx / ny / nz by 1 if the step size is greater than 0
!
  if (abs(xstep).gt.1.0d-12) nx = nx + 1
  if (abs(ystep).gt.1.0d-12) ny = ny + 1
  if (abs(zstep).gt.1.0d-12) nz = nz + 1
!
!  Put cuts to small value
!
  savecuts = cuts
  cuts = 0.0001_dp
!******************
!  Output banner  *
!******************
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''  Electrostatic potential on a grid :'',/)')
    write(ioout,'(''  Number of steps = '',2(i6,''  X ''),i6)') nxpg(ncf),nypg(ncf),nzpg(ncf)
    write(ioout,'(''  Grid size       = '',2(i6,''  X ''),i6,/)') nx,ny,nz
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    write(ioout,'(''    x         y         z       Potential         Derivatives (V/Angs)'')')
    if (ndim.eq.3) then
      write(ioout,'(''         (Fractional)              (eV)            x          y          z'')')
    elseif (ndim.eq.2) then
      write(ioout,'(''  (Frac)     (Frac)    (Angs)      (eV)            x          y          z'')')
    elseif (ndim.eq.1) then
      write(ioout,'(''  (Frac)     (Angs)    (Angs)      (eV)            x          y          z'')')
    else
      write(ioout,'(''         (Angstroms)               (eV)            x          y          z'')')
    endif
    write(ioout,'(''-------------------------------------------------------------------------------'')')
  endif
!***********************************
!  Start of loop over grid points  *
!***********************************
  xs = xmin - xstep
  do ix = 1,nx
    xs = xs + xstep
    ys = ymin - ystep
    do iy = 1,ny
      ys = ys + ystep
      zs = zmin - zstep
      do iz = 1,nz
        zs = zs + zstep
!*****************************************************************
!  New algorithm for large systems where matrix can't be stored  *
!*****************************************************************
        if (ndim.eq.3) then
!
!  Convert input fractional coordinates for centred cell to those for primitive cell
!
          xx(1) = xs
          xx(2) = ys
          xx(3) = zs
          x(1) = 0.0_dp
          x(2) = 0.0_dp
          x(3) = 0.0_dp
          v(1) = vit(1,1)
          v(2) = vit(2,1)
          v(3) = vit(3,1)
          call GULP_mxmb(rop(1,1,1),1_i4,3_i4,xx,1_i4,1_i4,v,1_i4,1_i4,3_i4,3_i4,1_i4)
          call GULP_mxmb(w1(ncbl,1,1),7_i4,21_i4,v,1_i4,1_i4,x,1_i4,1_i4,3_i4,3_i4,1_i4)
!
          xsite = x(1)*r1x + x(2)*r2x + x(3)*r3x
          ysite = x(1)*r1y + x(2)*r2y + x(3)*r3y
          zsite = x(1)*r1z + x(2)*r2z + x(3)*r3z
        elseif (ndim.eq.2) then
          xsite = xs*r1x + ys*r2x
          ysite = xs*r1y + ys*r2y
          zsite = zs
        elseif (ndim.eq.1) then
          xsite = xs*r1x
          ysite = ys
          zsite = zs
        endif
        call epot(lfirstpg,1_i4,vpg,xsite,ysite,zsite,.true.,xd,yd,zd,lgrad2,efgpg,.true.)
        if (ioproc) then
          write(ioout,'(3(f8.4,2x),1x,f12.6,3(1x,f10.5))') xs,ys,zs,vpg,xd,yd,zd
        endif
      enddo
    enddo
  enddo
  if (ioproc) then
    write(ioout,'(''-------------------------------------------------------------------------------'',/)')
  endif
!
!  Restore cuts
!
  cuts = savecuts
!
  time2 = cputime()
  tion = tion + time2 - time1
  if (ioproc) then
    write(ioout,'(''  Total time to end of potential grid = '',f12.4,/)') time2
  endif
!
  return
  end
