  subroutine cosmoaiizero(ipts,in,nearsasrptr,npwti,adrv,a2drv,ladrv,lgrad2)
!
!  Subroutine zeros the derivatives of the Aij matrix elements
!  where needed
!
!  12/04 Created from cosmoaiiderv
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
  integer(i4)                                   :: nearsasrptr(*)
  integer(i4)                                   :: npwti
  logical                                       :: ladrv(*)
  logical                                       :: lgrad2
  real(dp)                                      :: adrv(3,*)
  real(dp)                                      :: a2drv(3,3,*)
!
!  Local variables
!
  integer(i4)                                   :: ia
  integer(i4)                                   :: ib
  integer(i4)                                   :: in2
  integer(i4)                                   :: ind
  integer(i4)                                   :: m
  integer(i4)                                   :: mm
  integer(i4)                                   :: mn
  integer(i4)                                   :: n
  integer(i4)                                   :: nn
!
!  Setup constant
!
  in2 = in*(in - 1)/2
!
!  Weighting factor first derivatives for vectors to i
!
  do n = 1,npwti
    nn = nearsasrptr(npwtptr(n,ipts))
    ladrv(nn) = .false.
    adrv(1,nn) = 0.0_dp
    adrv(2,nn) = 0.0_dp
    adrv(3,nn) = 0.0_dp
  enddo
  if (lgrad2) then
    mn = 0
    do m = 1,npwti
      mm = nearsasrptr(npwtptr(m,ipts))
!
!  Weighting factor second derivatives for vectors to i only
!
      do n = 1,m
        nn = nearsasrptr(npwtptr(n,ipts))
        mn = mn + 1
        if (m.ne.n) then
! M - N
          ind = mm*(mm - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = 0.0_dp
            enddo
          enddo
! M - I
          if (in.gt.mm) then
            ind = in2 + mm
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = 0.0_dp
              enddo
            enddo
          else
            ind = mm*(mm - 1)/2 + in
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = 0.0_dp
              enddo
            enddo
          endif
! N - I
          if (in.gt.nn) then
            ind = in2 + nn
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = 0.0_dp
              enddo
            enddo
          else
            ind = nn*(nn - 1)/2 + in
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = 0.0_dp
              enddo
            enddo
          endif
        else
! M - I
          if (in.gt.mm) then
            ind = in2 + mm
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = 0.0_dp
              enddo
            enddo
          else
            ind = mm*(mm - 1)/2 + in
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = 0.0_dp
              enddo
            enddo
          endif
        endif
      enddo
    enddo
  endif
!
  return
  end
