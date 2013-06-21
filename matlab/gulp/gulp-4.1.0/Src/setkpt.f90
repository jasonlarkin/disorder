!********
!  3-D  *
!********
  subroutine setkpt3D
!
!  Sets up k points according to shrinking factors
!
!  nkpt = total number of k points across all structures
!  xkpt = fractional x component of k point
!  ykpt = fractional y component of k point
!  zkpt = fractional z component of k point
!  wkpt = weight of each k point
!  nkptcfg = configuration pointer for each k point
!  nxks   = x shrinking factor
!  nyks   = y shrinking factor
!  nzks   = z shrinking factor
!
!  10/04 Modifications for C -1 made
!  12/07 Unused variables removed
!   3/09 lkptdispersion added
!   6/09 lnoksym flag added
!   8/10 lpdf causes a shift of -0.5 -0.5 -0.5 for non-symmetry reduced 3D BZ (ER Cope)
!
!  Now modified to make use of some of the Patterson symmetry
!  As Gulp always converts a unit cell to the primitive form
!  only the primitive Patterson groups are considered here.
!
!  Based on Monkhorst and Pack special points scheme:
!
!  q even : u=(2r-q-1)/2q (r=1,2,3,...q)
!  q odd  : u=(2r-q)/2q   (r=1,2,3,...q)
!
!  Sequence modified for odd shrinking factors as this gives
!  better convergence than even series. Chadi-Cohen is special
!  case of Monkhorst and Pack, based on selecting certain even
!  q values.
!
!  Shrinking factors must have symmetry of unit cell otherwise
!  the symmetry adapted sampling will give different answers
!  to the non-symmetry adapted case.
!
!  iprimgp = pointer to appropriate primitive Patterson group
!
!  pointer numbers:
!			1 = P -1		8  = P -3 1 M
!			2 = P 2/M		9  = P 6/M
!			3 = P M M M		10 = P 6/M M M
!			4 = P 4/M		11 = P M -3
!			5 = P 4/M M M		12 = P M -3 M
!			6 = P -3		13 = R -3
!	       		7 = P -3 M 1            14 = R -3 M
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, August 2010
!
  use control
  use current
  use iochannels,     only : ioout
  use ksample
  use m_pdfneutron,   only : lnoksym, lpdf
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: iprimgp(232)
  integer(i4) :: j
  integer(i4) :: k
  integer(i4) :: kk
  integer(i4) :: ninsert
  integer(i4) :: npgrp
  integer(i4) :: nppgrp
  integer(i4) :: nx
  integer(i4) :: ny
  integer(i4) :: nyy
  integer(i4) :: nz
  integer(i4) :: nzz
  logical     :: ldo
  logical     :: lfound
  logical     :: lksym
  real(dp)    :: delx
  real(dp)    :: dely
  real(dp)    :: delz
  real(dp)    :: diff
  real(dp)    :: fx
  real(dp)    :: fy
  real(dp)    :: fz
  real(dp)    :: fzmax
  real(dp)    :: rdely
  real(dp)    :: rdelz
  real(dp)    :: rx
  real(dp)    :: ry
  real(dp)    :: rz
  real(dp)    :: wght
  real(dp)    :: xadd
  real(dp)    :: yadd
  real(dp)    :: zadd
!  Primitive
  data iprimgp/2*1, &
               2*2,1,2*2,2*1,2*2,1,2*2,1, & !  Monoclinic
               4*3,2*2,3*1,10*3,7*2,5*1,16*3,6*2,6*1, & !  Orthorhombic
               4*4,2*1,4,1,4*4,2*1,8*5,2*1,8*5,4*1,8*5,4*1,16*5,4*1, & !  Tetragonal
               3*6,13,6,13,8,7,8,7,8,7,14,7,8,7,8,2*14,2*8,2*7,2*14, & !  Trigonal
               9*9,18*10, & !  Hexagonal
               11,2*13,11,13,2*11,3*13,11,13,2*12,3*14,2*12,14,12, & !  Cubic
               2*14,12,2*14,4*12,6*14, & 
               2*1/ !  Additional groups!
!
  lksym = (index(keyword,'noksym').eq.0)
  if (lnoksym) lksym = .false.
!
  if (nspcg(ncf).gt.0) then
    npgrp = iprimgp(nspcg(ncf))
  else
    npgrp = 1
  endif
  nx = nxks(ncf)
  ny = nyks(ncf)
  nz = nzks(ncf)
  delx = 1.0_dp/float(nx)
  dely = 1.0_dp/float(ny)
  delz = 1.0_dp/float(nz)
  xadd = 0.5_dp*delx
  yadd = 0.5_dp*dely
  zadd = 0.5_dp*delz
!
!  Find point at which to insert any new k points such that
!  they remain in order of configuration
!
  if (nkpt.gt.0) then
    i = 1
    lfound = .false.
    do while (i.le.nkpt.and..not.lfound)
      if (nkptcfg(i).gt.ncf) then
        lfound = .true.
        ninsert = i
      endif
      i = i + 1
    enddo
    if (.not.lfound) ninsert = nkpt + 1
  else
    ninsert = 1
  endif
!
!  Set pointer to where the gamma point will be located so that
!  it can be corrected afterwards.
!
  if (.not.lksym) then
    if (lpdf.and.ioproc) then
      write(ioout,'(2x,"PDF uses Gamma centred Brillouin zone with no symmetry reduction",/)')
    endif
!*****************************************************
!  Generate k points across the full Brillouin zone  *
!*****************************************************
!
!  Check there is enough space to insert new K points
!
    if (nkpt+nx*ny*nz.gt.maxkpt) then
      maxkpt = nkpt + nx*ny*nz
      call changemaxkpt
    endif
    rx = - delx + xadd
    do i = 1,nx
      rx = rx + delx
      ry = - dely + yadd
      do j = 1,ny
        ry = ry + dely
        rz = - delz + zadd
        do k = 1,nz
          rz = rz + delz
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
          do kk = nkpt,ninsert,-1
            xkpt(kk+1) = xkpt(kk)
            ykpt(kk+1) = ykpt(kk)
            zkpt(kk+1) = zkpt(kk)
            wkpt(kk+1) = wkpt(kk)
            nkptcfg(kk+1) = nkptcfg(kk)
            lkptdispersion(kk+1) = lkptdispersion(kk)
          enddo
          nkpt = nkpt + 1
!
!  Add in new point - PDF uses shifted unit cell
!
          if (lpdf) then
            xkpt(ninsert) = rx - 0.5 
            ykpt(ninsert) = ry - 0.5 
            zkpt(ninsert) = rz - 0.5 
          else
            xkpt(ninsert) = rx
            ykpt(ninsert) = ry
            zkpt(ninsert) = rz
          endif
          wkpt(ninsert) = 1.0_dp
          nkptcfg(ninsert) = ncf
          lkptdispersion(ninsert) = .false.
          ninsert = ninsert + 1
        enddo
      enddo
    enddo
  else
!********************************************************
!  Generate k points across the reduced Brillouin zone  *
!********************************************************
    if (npgrp.eq.12.or.npgrp.eq.13.or.npgrp.eq.14) then
!*****************************
!  P M -3 M : R -3 M : R -3  *
!*****************************
      if (npgrp.eq.12) then
        nx = mod(nx,2_i4) + nx/2
        ny = mod(ny,2_i4) + ny/2
        nz = mod(nz,2_i4) + nz/2
      endif
      fx = - delx + xadd
      rdely = 1.0_dp/dely
      rdelz = 1.0_dp/delz
      do i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        nyy = nint(rdely*(fx-yadd)) + 1
        do j = 1,nyy
          fy = fy + dely
          fz = - delz + zadd
          nzz = nint(rdelz*(fy-zadd)) + 1
          do k = 1,nzz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk+1) = xkpt(kk)
              ykpt(kk+1) = ykpt(kk)
              zkpt(kk+1) = zkpt(kk)
              wkpt(kk+1) = wkpt(kk)
              nkptcfg(kk+1) = nkptcfg(kk)
              lkptdispersion(kk+1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,npgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    elseif (npgrp.eq.11) then
!***********
!  P M -3  *
!***********
      nx = mod(nx,2_i4) + nx/2
      ny = mod(ny,2_i4) + ny/2
      nz = mod(nz,2_i4) + nz/2
      rdelz = 1.0_dp/delz
      fx = - delx + xadd
      do  i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        do j = 1,ny
          fy = fy + dely
          fz = - delz + zadd
          if (fy.le.fx) then
            nzz = nint(rdelz*(fy-zadd)) + 1
          else
            nzz = nint(rdelz*(fx-zadd)) + 1
            fzmax = (nzz-1)*delz + zadd
            if (fzmax.ge.fx) nzz = nzz - 1
          endif
          do k = 1,nzz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk+1) = xkpt(kk)
              ykpt(kk+1) = ykpt(kk)
              zkpt(kk+1) = zkpt(kk)
              wkpt(kk+1) = wkpt(kk)
              nkptcfg(kk+1) = nkptcfg(kk)
              lkptdispersion(kk+1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,npgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    elseif (npgrp.eq.7.or.npgrp.eq.8.or.npgrp.eq.10) then
!************************************
!  P 6/M M M : P -3 M 1 : P -3 1 M  *
!************************************
      if (npgrp.eq.10) nz = mod(nz,2_i4) + nz/2
      ny = mod(ny,2_i4) + ny/2
      fx = - delx + xadd
      do i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        do j = 1,ny
          fy = fy + dely
          if (fx.gt.0.5_dp) then
            if (fy.le.(1.0_dp - fx)) then
              ldo = .true.
            else
              ldo = .false.
            endif
          else
            if (fy.le.fx) then
              ldo = .true.
            else
              ldo = .false.
            endif
          endif
          if (ldo) then
            fz = - delz + zadd
            if (npgrp.eq.7) then
              if (fy.eq.fx) then
                nzz = mod(nz,2_i4) + nz/2
              else
                nzz = nz
              endif
            elseif (npgrp.eq.8) then
              diff = abs(1.0_dp - fx - fy)
              if (fy.eq.0.0_dp.or.diff.lt.1.0d-12) then
                nzz = mod(nz,2_i4) + nz/2
              else
                nzz = nz
              endif
            else
              nzz = nz
            endif
            do k = 1,nzz
              fz = fz + delz
!
!  Check there is enough space to insert new K point
!
              if (nkpt.ge.maxkpt) then
                maxkpt  =  nkpt  +  10
                call changemaxkpt
              endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
              do kk = nkpt,ninsert,-1
                xkpt(kk + 1) = xkpt(kk)
                ykpt(kk + 1) = ykpt(kk)
                zkpt(kk + 1) = zkpt(kk)
                wkpt(kk + 1) = wkpt(kk)
                nkptcfg(kk + 1) = nkptcfg(kk)
                lkptdispersion(kk + 1) = lkptdispersion(kk)
              enddo
              nkpt = nkpt + 1
!
!  Add in new point
!
              xkpt(ninsert) = fx
              ykpt(ninsert) = fy
              zkpt(ninsert) = fz
              nkptcfg(ninsert) = ncf
              lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
              call kweight(fx,fy,fz,wght,npgrp)
              wkpt(ninsert) = wght
              ninsert = ninsert + 1
            enddo
          endif
        enddo
      enddo
    elseif (npgrp.eq.6.or.npgrp.eq.9) then
!*****************
!  P -3 : P 6/M  *
!*****************
      nx = mod(nx,2_i4) + nx/2
      if (npgrp.eq.9) then
        nz = mod(nz,2_i4) + nz/2
      endif
      fx = - delx + xadd
      do i = 1,nx
        fx = fx + delx
        if (fx.eq.0.5_dp) then
          nyy = mod(ny,2_i4) + ny/2
        else
          nyy = ny
        endif
        fy = - dely + yadd
        do j = 1,nyy
          fy = fy + dely
          if (fx.eq.0.5_dp.and.fy.eq.0.5_dp.and.npgrp.eq.6) then
            nzz = mod(nz,2_i4) + nz/2
          else
            nzz = nz
          endif
          fz = - delz + zadd
          do k = 1,nzz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk + 1) = xkpt(kk)
              ykpt(kk + 1) = ykpt(kk)
              zkpt(kk + 1) = zkpt(kk)
              wkpt(kk + 1) = wkpt(kk)
              nkptcfg(kk + 1) = nkptcfg(kk)
              lkptdispersion(kk + 1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,npgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    elseif (npgrp.eq.5) then
!**************
!  P 4/M M M  *
!**************
      nx = mod(nx,2_i4) + nx/2
      ny = mod(ny,2_i4) + ny/2
      nz = mod(nz,2_i4) + nz/2
      rdely = 1.0_dp/dely
      fx = - delx + xadd
      do i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        nyy = nint(rdely*(fx-yadd)) + 1
        do j = 1,nyy
          fy = fy + dely
          fz = - delz + zadd
          do k = 1,nz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk + 1) = xkpt(kk)
              ykpt(kk + 1) = ykpt(kk)
              zkpt(kk + 1) = zkpt(kk)
              wkpt(kk + 1) = wkpt(kk)
              nkptcfg(kk + 1) = nkptcfg(kk)
              lkptdispersion(kk + 1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,npgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    elseif (npgrp.eq.4) then
!**********
!  P 4/M  *
!**********
      nx = mod(nx,2_i4) + nx/2
      ny = mod(ny,2_i4) + ny/2
      nz = mod(nz,2_i4) + nz/2
      fx = - delx + xadd
      do i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        if (fx.eq.0.5_dp) then
          nyy = ny
        else
          if ((ny-1)*dely + yadd.eq.0.5_dp) then
            nyy = ny - 1
          else
            nyy = ny
          endif
        endif
        do j = 1,nyy
          fy = fy + dely
          fz = - delz + zadd
          do k = 1,nz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk+1) = xkpt(kk)
              ykpt(kk+1) = ykpt(kk)
              zkpt(kk+1) = zkpt(kk)
              wkpt(kk+1) = wkpt(kk)
              nkptcfg(kk+1) = nkptcfg(kk)
              lkptdispersion(kk+1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,npgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    elseif (npgrp.eq.2) then
!**********
!  P 2/M  *
!**********
      nx = mod(nx,2_i4) + nx/2
      ny = mod(ny,2_i4) + ny/2
      fx = - delx + xadd
      do i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        do j = 1,ny
          fy = fy + dely
          if (fx.eq.0.0_dp.or.fx.eq.0.5_dp) then
            nzz = mod(nz,2_i4) + nz/2
            fz = - delz + zadd
          else
            nzz = nz
            fz = - delz + zadd
          endif
          do k = 1,nzz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk+1) = xkpt(kk)
              ykpt(kk+1) = ykpt(kk)
              zkpt(kk+1) = zkpt(kk)
              wkpt(kk+1) = wkpt(kk)
              nkptcfg(kk+1) = nkptcfg(kk)
              lkptdispersion(kk+1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,npgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    else
!************************************************************
!  P -1 / P M M M (and other groups as P -1 at the moment)  *
!************************************************************
      nx = mod(nx,2_i4) + nx/2
      if (npgrp.eq.3) then
        nppgrp = 3
        ny = mod(ny,2_i4) + ny/2
        nz = mod(nz,2_i4) + nz/2
      else
        nppgrp = 1
      endif
      fx = - delx + xadd
      do i = 1,nx
        fx = fx + delx
        fy = - dely + yadd
        do j = 1,ny
          fy = fy + dely
          fz = - delz + zadd
          do k = 1,nz
            fz = fz + delz
!
!  Check there is enough space to insert new K point
!
            if (nkpt.ge.maxkpt) then
              maxkpt = nkpt + 10
              call changemaxkpt
            endif
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
            do kk = nkpt,ninsert,-1
              xkpt(kk+1) = xkpt(kk)
              ykpt(kk+1) = ykpt(kk)
              zkpt(kk+1) = zkpt(kk)
              wkpt(kk+1) = wkpt(kk)
              nkptcfg(kk+1) = nkptcfg(kk)
              lkptdispersion(kk+1) = lkptdispersion(kk)
            enddo
            nkpt = nkpt + 1
!
!  Add in new point
!
            xkpt(ninsert) = fx
            ykpt(ninsert) = fy
            zkpt(ninsert) = fz
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
            call kweight(fx,fy,fz,wght,nppgrp)
            wkpt(ninsert) = wght
            ninsert = ninsert + 1
          enddo
        enddo
      enddo
    endif
  endif
  return
  end
!********
!  2-D  *
!********
  subroutine setkpt2D
!
!  Sets up k points according to shrinking factors for 2-D case
!
!  nkpt  =  total number of k points across all structures
!  xkpt  =  fractional x component of k point
!  ykpt  =  fractional y component of k point
!  zkpt  =  fractional z component of k point
!  wkpt  =  weight of each k point
!  nkptcfg  =  configuration pointer for each k point
!  nxks    =  x shrinking factor
!  nyks    =  y shrinking factor
!  nzks    =  z shrinking factor
!
!  Based on Monkhorst and Pack special points scheme:
!
!  q even : u = (2r-q-1)/2q (r=1,2,3,...q)
!  q odd  : u = (2r-q)/2q   (r=1,2,3,...q)
!
!  Sequence modified for odd shrinking factors as this gives
!  better convergence than even series. Chadi-Cohen is special
!  case of Monkhorst and Pack, based on selecting certain even
!  q values.
!
!  Shrinking factors must have symmetry of unit cell otherwise
!  the symmetry adapted sampling will give different answers
!  to the non-symmetry adapted case.
!
!   3/09 lkptdispersion added
!   6/09 lnoksym flag added
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use control
  use current
  use ksample
  use m_pdfneutron, only : lnoksym
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: j
  integer(i4) :: kk
  integer(i4) :: ninsert
  integer(i4) :: nx
  integer(i4) :: ny
  logical     :: lfound
  logical     :: lksym
  real(dp)    :: delx
  real(dp)    :: dely
  real(dp)    :: fx
  real(dp)    :: fy
  real(dp)    :: fz
  real(dp)    :: rx
  real(dp)    :: ry
  real(dp)    :: rz
  real(dp)    :: wght
  real(dp)    :: xadd
  real(dp)    :: yadd
!
  lksym = (index(keyword,'noksym').eq.0)
  if (lnoksym) lksym = .false.
!
  nx = nxks(ncf)
  ny = nyks(ncf)
  delx = 1.0_dp/float(nx)
  dely = 1.0_dp/float(ny)
  xadd = 0.5_dp*delx
  yadd = 0.5_dp*dely
!
!  Find point at which to insert any new k points such that
!  they remain in order of configuration
!
  if (nkpt.gt.0) then
    i = 1
    lfound = .false.
    do while (i.le.nkpt.and..not.lfound)
      if (nkptcfg(i).gt.ncf) then
        lfound = .true.
        ninsert = i
      endif
      i = i + 1
    enddo
    if (.not.lfound) ninsert = nkpt + 1
  else
    ninsert = 1
  endif
!
!  Set pointer to where the gamma point will be located so that
!  it can be corrected afterwards.
!
  if (.not.lksym) then
!*****************************************************
!  Generate k points across the full Brillouin zone  *
!*****************************************************
!
!  Check there is enough space to insert new K points
!
    if (nkpt + nx*ny.gt.maxkpt) then
      maxkpt = nkpt + nx*ny
      call changemaxkpt
    endif
    rx = -delx + xadd
    rz = 0.0_dp
    do i = 1,nx
      rx = rx + delx
      ry = -dely + yadd
      do j = 1,ny
        ry = ry + dely
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
        do kk = nkpt,ninsert,-1
          xkpt(kk+1) = xkpt(kk)
          ykpt(kk+1) = ykpt(kk)
          zkpt(kk+1) = zkpt(kk)
          wkpt(kk+1) = wkpt(kk)
          nkptcfg(kk+1) = nkptcfg(kk)
          lkptdispersion(kk+1) = lkptdispersion(kk)
        enddo
        nkpt = nkpt + 1
!
!  Add in new point
!
        xkpt(ninsert) = rx
        ykpt(ninsert) = ry
        zkpt(ninsert) = rz
        wkpt(ninsert) = 1.0_dp
        nkptcfg(ninsert) = ncf
        lkptdispersion(ninsert) = .false.
        ninsert = ninsert + 1
      enddo
    enddo
  else
!********************************************************
!  Generate k points across the reduced Brillouin zone  *
!********************************************************
!*********
!  P -1  *
!*********
    nx = mod(nx,2_i4) + nx/2
!
!  Check there is enough space to insert new K point
!
    if (nkpt + nx*ny.ge.maxkpt) then
      maxkpt  =  nkpt  +  nx*ny
      call changemaxkpt
    endif
    fx = - delx + xadd
    fz = 0.0_dp
    do i = 1,nx
      fx = fx + delx
      fy = - dely + yadd
      do j = 1,ny
        fy = fy + dely
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
        do kk = nkpt,ninsert,-1
          xkpt(kk+1) = xkpt(kk)
          ykpt(kk+1) = ykpt(kk)
          zkpt(kk+1) = zkpt(kk)
          wkpt(kk+1) = wkpt(kk)
          nkptcfg(kk+1) = nkptcfg(kk)
          lkptdispersion(kk+1) = lkptdispersion(kk)
        enddo
        nkpt = nkpt + 1
!
!  Add in new point
!
        xkpt(ninsert) = fx
        ykpt(ninsert) = fy
        zkpt(ninsert) = fz
        nkptcfg(ninsert) = ncf
        lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
        call kweight(fx,fy,fz,wght,1_i4)
        wkpt(ninsert) = wght
        ninsert = ninsert + 1
      enddo
    enddo
  endif
  return
  end
!********
!  1-D  *
!********
  subroutine setkpt1D
!
!  Sets up k points according to shrinking factors for 1-D case
!
!  nkpt  =  total number of k points across all structures
!  xkpt  =  fractional x component of k point
!  ykpt  =  fractional y component of k point
!  zkpt  =  fractional z component of k point
!  wkpt  =  weight of each k point
!  nkptcfg  =  configuration pointer for each k point
!  nxks    =  x shrinking factor
!  nyks    =  y shrinking factor
!  nzks    =  z shrinking factor
!
!  Based on Monkhorst and Pack special points scheme:
!
!  q even : u = (2r-q-1)/2q (r=1,2,3,...q)
!  q odd  : u = (2r-q)/2q   (r=1,2,3,...q)
!
!  Sequence modified for odd shrinking factors as this gives
!  better convergence than even series. Chadi-Cohen is special
!  case of Monkhorst and Pack, based on selecting certain even
!  q values.
!
!  Shrinking factors must have symmetry of unit cell otherwise
!  the symmetry adapted sampling will give different answers
!  to the non-symmetry adapted case.
!
!   3/09 lkptdispersion added
!   6/09 lnoksym flag added
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use control
  use current
  use ksample
  use m_pdfneutron, only : lnoksym
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: kk
  integer(i4) :: ninsert
  integer(i4) :: nx
  logical     :: lfound
  logical     :: lksym
  real(dp)    :: delx
  real(dp)    :: fx
  real(dp)    :: fy
  real(dp)    :: fz
  real(dp)    :: rx
  real(dp)    :: ry
  real(dp)    :: rz
  real(dp)    :: wght
  real(dp)    :: xadd
!
  lksym = (index(keyword,'noksym').eq.0)
  if (lnoksym) lksym = .false.
!
  nx = nxks(ncf)
  delx = 1.0_dp/float(nx)
  xadd = 0.5_dp*delx
!
!  Find point at which to insert any new k points such that
!  they remain in order of configuration
!
  if (nkpt.gt.0) then
    i = 1
    lfound = .false.
    do while (i.le.nkpt.and..not.lfound)
      if (nkptcfg(i).gt.ncf) then
        lfound = .true.
        ninsert = i
      endif
      i = i + 1
    enddo
    if (.not.lfound) ninsert = nkpt + 1
  else
    ninsert = 1
  endif
!
!  Set pointer to where the gamma point will be located so that
!  it can be corrected afterwards.
!
  if (.not.lksym) then
!*****************************************************
!  Generate k points across the full Brillouin zone  *
!*****************************************************
!
!  Check there is enough space to insert new K points
!
    if (nkpt + nx.gt.maxkpt) then
      maxkpt  =  nkpt  +  nx
      call changemaxkpt
    endif
    rx = - delx + xadd
    ry = 0.0_dp
    rz = 0.0_dp
    do i = 1,nx
      rx = rx + delx
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
      do kk = nkpt,ninsert,-1
        xkpt(kk+1) = xkpt(kk)
        ykpt(kk+1) = ykpt(kk)
        zkpt(kk+1) = zkpt(kk)
        wkpt(kk+1) = wkpt(kk)
        nkptcfg(kk+1) = nkptcfg(kk)
        lkptdispersion(kk+1) = lkptdispersion(kk)
      enddo
      nkpt = nkpt + 1
!
!  Add in new point
!
      xkpt(ninsert) = rx
      ykpt(ninsert) = ry
      zkpt(ninsert) = rz
      wkpt(ninsert) = 1.0_dp
      nkptcfg(ninsert) = ncf
      lkptdispersion(ninsert) = .false.
      ninsert = ninsert + 1
    enddo
  else
!********************************************************
!  Generate k points across the reduced Brillouin zone  *
!********************************************************
!*********
!  P -1  *
!*********
    nx = mod(nx,2_i4) + nx/2
!
!  Check there is enough space to insert new K points
!
    if (nkpt + nx.gt.maxkpt) then
      maxkpt  =  nkpt  +  nx
      call changemaxkpt
    endif
    fx = - delx + xadd
    fy = 0.0_dp
    fz = 0.0_dp
    do i = 1,nx
      fx = fx + delx
!
!  Make space for new k point - needs amending to check existing k points
!  for duplicates
!
      do kk = nkpt,ninsert,-1
        xkpt(kk+1) = xkpt(kk)
        ykpt(kk+1) = ykpt(kk)
        zkpt(kk+1) = zkpt(kk)
        wkpt(kk+1) = wkpt(kk)
        nkptcfg(kk+1) = nkptcfg(kk)
        lkptdispersion(kk+1) = lkptdispersion(kk)
      enddo
      nkpt = nkpt + 1
!
!  Add in new point
!
      xkpt(ninsert) = fx
      ykpt(ninsert) = fy
      zkpt(ninsert) = fz
      nkptcfg(ninsert) = ncf
      lkptdispersion(ninsert) = .false.
!
!  Calculate weighting factor
!
      call kweight(fx,fy,fz,wght,1_i4)
      wkpt(ninsert) = wght
      ninsert = ninsert + 1
    enddo
  endif
!
  return
  end
