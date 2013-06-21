  subroutine cosmoaijzero(ipts,in,jpts,jn,nearsasrptr,npwti,npwtj,adrvi,adrvj,a2drv,ladrv,lgrad2)
!
!  Subroutine zeros the derivatives of the Aij matrix elements
!  only where necessary
!
!  12/04 Created from cosmoaijderv
!  12/08 Migrated to version 3.5 and converted to f90 format
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use datatypes
  use cosmo, only : npwtptr
  implicit none
!
!  Passed variables
!
  integer(i4)                                   :: in
  integer(i4)                                   :: ipts
  integer(i4)                                   :: jn
  integer(i4)                                   :: jpts
  integer(i4)                                   :: nearsasrptr(*)
  integer(i4)                                   :: npwti
  integer(i4)                                   :: npwtj
  logical                                       :: ladrv(*)
  logical                                       :: lgrad2
  real(dp)                                      :: adrvi(3,*)
  real(dp)                                      :: adrvj(3,*)
  real(dp)                                      :: a2drv(3,3,*)
!
!  Local variables
!
  integer(i4)                                   :: ia
  integer(i4)                                   :: ib
  integer(i4)                                   :: in2
  integer(i4)                                   :: ind
  integer(i4)                                   :: jn2
  integer(i4)                                   :: m
  integer(i4)                                   :: mm
  integer(i4)                                   :: mn
  integer(i4)                                   :: n
  integer(i4)                                   :: nn
!
!  Setup constants
!
  in2 = in*(in - 1)/2
  jn2 = jn*(jn - 1)/2
!
!  Weighting factor first derivatives for vectors to i
!
  do n = 1,npwti
    nn = nearsasrptr(npwtptr(n,ipts))
    ladrv(nn) = .false.
    adrvi(1,nn) = 0.0_dp
    adrvi(2,nn) = 0.0_dp
    adrvi(3,nn) = 0.0_dp
  enddo
!
!  Weighting factor first derivatives for vectors to j
!
  do n = 1,npwtj
    nn = nearsasrptr(npwtptr(n,jpts))
    ladrv(nn) = .false.
    adrvj(1,nn) = 0.0_dp
    adrvj(2,nn) = 0.0_dp
    adrvj(3,nn) = 0.0_dp
  enddo                                                                                    
  if (lgrad2) then
    mn = 0
    do m = 1,npwti
      mm = nearsasrptr(npwtptr(m,ipts))
!
!  Weighting factor first derivatives / ij first derivatives for vectors to i only
!
!  M - I
      if (mm.gt.in) then
        ind = mm*(mm - 1)/2 + in
      else
        ind = in2 + mm
      endif
      do ia = 1,3
        a2drv(1,ia,ind) = 0.0_dp
        a2drv(2,ia,ind) = 0.0_dp
        a2drv(3,ia,ind) = 0.0_dp
      enddo
!  M - J
      if (mm.gt.jn) then
        ind = mm*(mm - 1)/2 + jn
      else
        ind = jn2 + mm
      endif
      do ia = 1,3
        a2drv(1,ia,ind) = 0.0_dp
        a2drv(2,ia,ind) = 0.0_dp
        a2drv(3,ia,ind) = 0.0_dp
      enddo
!  J - I
      if (jn.gt.in) then
        ind = jn2 + in
      else
        ind = in2 + jn
      endif
      do ia = 1,3
        a2drv(1,ia,ind) = 0.0_dp
        a2drv(2,ia,ind) = 0.0_dp
        a2drv(3,ia,ind) = 0.0_dp
      enddo
!
!  Weighting factor second derivatives for vectors to i only
!
      do n = 1,m
        nn = nearsasrptr(npwtptr(n,ipts))
        mn = mn + 1
! M - I
        if (in.gt.mm) then
          ind = in2 + mm
        else
          ind = mm*(mm - 1)/2 + in
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
! N - I
        if (in.gt.nn) then
          ind = in2 + nn
        else
          ind = nn*(nn - 1)/2 + in
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
! M - N
        if (mm.gt.nn) then
          ind = mm*(mm - 1)/2 + nn
        else
          ind = nn*(nn - 1)/2 + mm
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
      enddo
    enddo
!
    mn = 0
    do m = 1,npwtj
      mm = nearsasrptr(npwtptr(m,jpts))
!
!  Weighting factor first derivatives / ij first derivatives for vectors to j only
!
!  M - J
      if (mm.gt.jn) then
        ind = mm*(mm - 1)/2 + jn
      else
        ind = jn2 + mm
      endif
      do ia = 1,3
        a2drv(1,ia,ind) = 0.0_dp
        a2drv(2,ia,ind) = 0.0_dp
        a2drv(3,ia,ind) = 0.0_dp
      enddo
!  M - I
      if (mm.gt.in) then
        ind = mm*(mm - 1)/2 + in
      else
        ind = in2 + mm
      endif
      do ia = 1,3
        a2drv(1,ia,ind) = 0.0_dp
        a2drv(2,ia,ind) = 0.0_dp
        a2drv(3,ia,ind) = 0.0_dp
      enddo
!  I - J
      if (in.gt.jn) then
        ind = in2 + jn
      else
        ind = jn2 + in
      endif
      do ia = 1,3
        a2drv(1,ia,ind) = 0.0_dp
        a2drv(2,ia,ind) = 0.0_dp
        a2drv(3,ia,ind) = 0.0_dp
      enddo
!
!  Weighting factor second derivatives for vectors to j only
!
      do n = 1,m
        nn = nearsasrptr(npwtptr(n,jpts))
        mn = mn + 1
!  M - J
        if (jn.gt.mm) then
          ind = jn2 + mm
        else
          ind = mm*(mm - 1)/2 + jn
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
!  N - J
        if (jn.gt.nn) then
          ind = jn2 + nn
        else
          ind = nn*(nn - 1)/2 + jn
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
! M - N
        if (mm.gt.nn) then
          ind = mm*(mm - 1)/2 + nn
        else
          ind = nn*(nn - 1)/2 + mm
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
      enddo
    enddo
!
!  Weighting factor second derivatives for vectors to i and j
!
    do m = 1,npwti
      mm = nearsasrptr(npwtptr(m,ipts))
      do n = 1,npwtj
        nn = nearsasrptr(npwtptr(n,jpts))
!  M - N
        if (mm.gt.nn) then
          ind = mm*(mm - 1)/2 + nn
        else
          ind = nn*(nn - 1)/2 + mm
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
!  I - N
        if (in.gt.nn) then
          ind = in2 + nn
        else
          ind = nn*(nn - 1)/2 + in
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
!  M - J
        if (jn.gt.mm) then
          ind = jn2 + mm
        else
          ind = mm*(mm - 1)/2 + jn
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
!  I - J
        if (jn.gt.in) then
          ind = jn2 + in
        else
          ind = in2 + jn
        endif
        do ia = 1,3
          do ib = 1,3
            a2drv(ib,ia,ind) = 0.0_dp
          enddo
        enddo
      enddo
    enddo
  endif
!
  return
  end
