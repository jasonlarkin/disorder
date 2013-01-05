  subroutine cosmoaiiderv(ipts,in,n1,n2,nearsasrptr,npwti,maxnpwt2, &
    rdist,w1,w2,dwti,d2wti,adrv,a2drv,ladrv,lgrad2)
!
!  Subroutine calculates the derivatives of the Aij matrix elements
!  and adds them onto the array of derivatives.
!
!   3/03 Created from cosmoderv
!  12/04 Style updated 
!  12/04 Logicals added to indicate whether adrv/a2drv elements are non-zero
!   1/05 Case where nn > mm handled differently
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
  use cosmo, only : maxnpwt, npwtptr
  implicit none
!
!  Passed variables
!
  integer(i4)                                   :: in
  integer(i4)                                   :: ipts
  integer(i4)                                   :: maxnpwt2
  integer(i4)                                   :: nearsasrptr(*)
  integer(i4)                                   :: n1
  integer(i4)                                   :: n2
  integer(i4)                                   :: npwti
  logical                                       :: ladrv(*)
  logical                                       :: lgrad2
  real(dp)                                      :: adrv(3,*)
  real(dp)                                      :: a2drv(3,3,*)
  real(dp)                                      :: dwti(3,maxnpwt,*)
  real(dp)                                      :: d2wti(3,3,maxnpwt2,*)
  real(dp)                                      :: rdist
  real(dp)                                      :: w1
  real(dp)                                      :: w2
!
!  Local variables
!
  integer(i4)                                   :: ia
  integer(i4)                                   :: ib
  integer(i4)                                   :: ind
  integer(i4)                                   :: m
  integer(i4)                                   :: mm
  integer(i4)                                   :: mn
  integer(i4)                                   :: n
  integer(i4)                                   :: nn
!
!  Weighting factor first derivatives for vectors to i
!
  do n = 1,npwti
    nn = nearsasrptr(npwtptr(n,ipts))
    ladrv(nn) = .true.
    adrv(1,nn) = adrv(1,nn) + rdist*(w2*dwti(1,n,n1) + w1*dwti(1,n,n2))
    adrv(2,nn) = adrv(2,nn) + rdist*(w2*dwti(2,n,n1) + w1*dwti(2,n,n2))
    adrv(3,nn) = adrv(3,nn) + rdist*(w2*dwti(3,n,n1) + w1*dwti(3,n,n2))
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
          if (mm.ge.nn) then
            ind = mm*(mm - 1)/2 + nn
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rdist*(w2*d2wti(ib,ia,mn,n1) + w1*d2wti(ib,ia,mn,n2) + &
                                                      dwti(ib,n,n1)*dwti(ia,m,n2) + dwti(ib,n,n2)*dwti(ia,m,n1))
              enddo
            enddo
          else
            ind = nn*(nn - 1)/2 + mm
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rdist*(w2*d2wti(ib,ia,mn,n1) + w1*d2wti(ib,ia,mn,n2) + &
                                                      dwti(ia,n,n1)*dwti(ib,m,n2) + dwti(ia,n,n2)*dwti(ib,m,n1))
              enddo
            enddo
          endif
! M - I
          if (in.gt.mm) then
            ind = in*(in - 1)/2 + mm
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*(w2*d2wti(ia,ib,mn,n1) + w1*d2wti(ia,ib,mn,n2) + &
                                                      dwti(ia,n,n1)*dwti(ib,m,n2) + dwti(ia,n,n2)*dwti(ib,m,n1))
              enddo
            enddo
          else
            ind = mm*(mm - 1)/2 + in
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*(w2*d2wti(ib,ia,mn,n1) + w1*d2wti(ib,ia,mn,n2) + &
                                                      dwti(ib,n,n1)*dwti(ia,m,n2) + dwti(ib,n,n2)*dwti(ia,m,n1))
              enddo
            enddo
          endif
! N - I
          if (in.gt.nn) then
            ind = in*(in - 1)/2 + nn
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*(w2*d2wti(ib,ia,mn,n1) + w1*d2wti(ib,ia,mn,n2) + &
                                                      dwti(ib,n,n1)*dwti(ia,m,n2) + dwti(ib,n,n2)*dwti(ia,m,n1))
              enddo
            enddo
          else
            ind = nn*(nn - 1)/2 + in
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*(w2*d2wti(ia,ib,mn,n1) + w1*d2wti(ia,ib,mn,n2) + &
                                                      dwti(ia,n,n1)*dwti(ib,m,n2) + dwti(ia,n,n2)*dwti(ib,m,n1))
              enddo
            enddo
          endif
        else
! M - I
          if (in.gt.mm) then
            ind = in*(in - 1)/2 + mm
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*(w2*d2wti(ia,ib,mn,n1) + w1*d2wti(ia,ib,mn,n2) + &
                                                      dwti(ia,n,n1)*dwti(ib,m,n2) + dwti(ia,n,n2)*dwti(ib,m,n1))
              enddo
            enddo
          else
            ind = mm*(mm - 1)/2 + in
            do ia = 1,3
              do ib = 1,3
                a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*(w2*d2wti(ib,ia,mn,n1) + w1*d2wti(ib,ia,mn,n2) + &
                                                      dwti(ib,n,n1)*dwti(ia,m,n2) + dwti(ib,n,n2)*dwti(ia,m,n1))
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
