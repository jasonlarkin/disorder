  subroutine cosmoaijderv(ipts,in,ni,jpts,jn,nj,nearsasrptr,npwti,npwtj,maxnpwt2,x,y,z, &
                          rdist,d1rdist,wi,wj,dwti,dwtj,d2wti,d2wtj,adrvi,adrvj,a2drv, &
                          ladrv,lgrad2)
!
!  Subroutine calculates the derivatives of the Aij matrix elements
!  and adds them onto the array of derivatives. Note that for first
!  derivatives the terms for distances to i and j are kept apart so
!  as to facilitate the generation of dcosmoA, whereas for the 
!  second derivatives they are combined.
!
!   3/03 Created from cosmoderv
!  12/04 Style updated and local scalars introduced
!  12/04 Logicals added to indicate whether adrv/a2drv elements are non-zero
!  12/08 Migrated to version 3.5 and converted to f90 format
!  12/08 ii & nmid no longer passed in as arguments
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
  integer(i4)                                   :: jn
  integer(i4)                                   :: jpts
  integer(i4)                                   :: maxnpwt2
  integer(i4)                                   :: nearsasrptr(*)
  integer(i4)                                   :: ni
  integer(i4)                                   :: nj
  integer(i4)                                   :: npwti
  integer(i4)                                   :: npwtj
  logical                                       :: ladrv(*)
  logical                                       :: lgrad2
  real(dp)                                      :: adrvi(3,*)
  real(dp)                                      :: adrvj(3,*)
  real(dp)                                      :: a2drv(3,3,*)
  real(dp)                                      :: d1rdist
  real(dp)                                      :: dwti(3,maxnpwt,*)
  real(dp)                                      :: dwtj(3,maxnpwt,*)
  real(dp)                                      :: d2wti(3,3,maxnpwt2,*)
  real(dp)                                      :: d2wtj(3,3,maxnpwt2,*)
  real(dp)                                      :: rdist
  real(dp)                                      :: wi
  real(dp)                                      :: wj
  real(dp)                                      :: x
  real(dp)                                      :: y
  real(dp)                                      :: z
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
  real(dp)                                      :: atrm
  real(dp)                                      :: fct
  real(dp)                                      :: fctij
  real(dp)                                      :: rtrm2
!
!  Set weighting for self term
!
  if (ipts.eq.jpts) then
    fctij = 0.5_dp
  else
    fctij = 1.0_dp
  endif
!
!  Weighting factor first derivatives for vectors to i
!
  atrm = fctij*rdist*wj
  do n = 1,npwti
    nn = nearsasrptr(npwtptr(n,ipts))
    ladrv(nn) = .true.
    adrvi(1,nn) = adrvi(1,nn) + atrm*dwti(1,n,ni)
    adrvi(2,nn) = adrvi(2,nn) + atrm*dwti(2,n,ni)
    adrvi(3,nn) = adrvi(3,nn) + atrm*dwti(3,n,ni)
  enddo
!
!  Weighting factor first derivatives for vectors to j
!
  atrm = fctij*rdist*wi
  do n = 1,npwtj
    nn = nearsasrptr(npwtptr(n,jpts))
    ladrv(nn) = .true.
    adrvj(1,nn) = adrvj(1,nn) + atrm*dwtj(1,n,nj)
    adrvj(2,nn) = adrvj(2,nn) + atrm*dwtj(2,n,nj)
    adrvj(3,nn) = adrvj(3,nn) + atrm*dwtj(3,n,nj)
  enddo                                                                                    
  if (lgrad2) then
    mn = 0
    do m = 1,npwti
      mm = nearsasrptr(npwtptr(m,ipts))
!
!  Weighting factor first derivatives / ij first derivatives for vectors to i only
!
      rtrm2 = fctij*d1rdist*wj
!  M - I
      if (mm.gt.in) then
        ind = mm*(mm - 1)/2 + in
        do ia = 1,3
          a2drv(1,ia,ind) = a2drv(1,ia,ind) - rtrm2*dwti(ia,m,ni)*x
          a2drv(2,ia,ind) = a2drv(2,ia,ind) - rtrm2*dwti(ia,m,ni)*y
          a2drv(3,ia,ind) = a2drv(3,ia,ind) - rtrm2*dwti(ia,m,ni)*z
        enddo
      else
        ind = in*(in - 1)/2 + mm
        do ia = 1,3
          a2drv(ia,1,ind) = a2drv(ia,1,ind) - rtrm2*dwti(ia,m,ni)*x
          a2drv(ia,2,ind) = a2drv(ia,2,ind) - rtrm2*dwti(ia,m,ni)*y
          a2drv(ia,3,ind) = a2drv(ia,3,ind) - rtrm2*dwti(ia,m,ni)*z
        enddo
      endif
!  M - J
      if (mm.gt.jn) then
        ind = mm*(mm - 1)/2 + jn
        do ia = 1,3
          a2drv(1,ia,ind) = a2drv(1,ia,ind) + rtrm2*dwti(ia,m,ni)*x
          a2drv(2,ia,ind) = a2drv(2,ia,ind) + rtrm2*dwti(ia,m,ni)*y
          a2drv(3,ia,ind) = a2drv(3,ia,ind) + rtrm2*dwti(ia,m,ni)*z
        enddo
      else
        ind = jn*(jn - 1)/2 + mm
        do ia = 1,3
          a2drv(ia,1,ind) = a2drv(ia,1,ind) + rtrm2*dwti(ia,m,ni)*x
          a2drv(ia,2,ind) = a2drv(ia,2,ind) + rtrm2*dwti(ia,m,ni)*y
          a2drv(ia,3,ind) = a2drv(ia,3,ind) + rtrm2*dwti(ia,m,ni)*z
        enddo
      endif
!  J - I
      if (jn.gt.in) then
        ind = jn*(jn - 1)/2 + in
        do ia = 1,3
          a2drv(ia,1,ind) = a2drv(ia,1,ind) - rtrm2*dwti(ia,m,ni)*x
          a2drv(ia,2,ind) = a2drv(ia,2,ind) - rtrm2*dwti(ia,m,ni)*y
          a2drv(ia,3,ind) = a2drv(ia,3,ind) - rtrm2*dwti(ia,m,ni)*z
        enddo
      else
        ind = in*(in - 1)/2 + jn
        do ia = 1,3
          a2drv(1,ia,ind) = a2drv(1,ia,ind) - rtrm2*dwti(ia,m,ni)*x
          a2drv(2,ia,ind) = a2drv(2,ia,ind) - rtrm2*dwti(ia,m,ni)*y
          a2drv(3,ia,ind) = a2drv(3,ia,ind) - rtrm2*dwti(ia,m,ni)*z
        enddo
      endif
!
!  Weighting factor second derivatives for vectors to i only
!
      rtrm2 = fctij*rdist*wj
      do n = 1,m
        nn = nearsasrptr(npwtptr(n,ipts))
        mn = mn + 1
        if (m.eq.n) then
          fct = 0.5_dp
        else
          fct = 1.0_dp
        endif
! M - I
        if (in.gt.mm) then
          ind = in*(in - 1)/2 + mm
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wti(ia,ib,mn,ni)*fct
            enddo
          enddo
        else
          ind = mm*(mm - 1)/2 + in
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wti(ib,ia,mn,ni)*fct
            enddo
          enddo
        endif
! N - I
        if (in.gt.nn) then
          ind = in*(in - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wti(ib,ia,mn,ni)*fct
            enddo
          enddo
        else
          ind = nn*(nn - 1)/2 + in
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wti(ia,ib,mn,ni)*fct
            enddo
          enddo
        endif
! M - N
        if (mm.gt.nn) then
          ind = mm*(mm - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rtrm2*d2wti(ib,ia,mn,ni)*fct
            enddo
          enddo
        else
          ind = nn*(nn - 1)/2 + mm
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rtrm2*d2wti(ia,ib,mn,ni)*fct
            enddo
          enddo
        endif
      enddo
    enddo
!
    rtrm2 = fctij*d1rdist*wi
    mn = 0
    do m = 1,npwtj
      mm = nearsasrptr(npwtptr(m,jpts))
!
!  Weighting factor first derivatives / ij first derivatives for vectors to j only
!
!  M - J
      if (mm.gt.jn) then
        ind = mm*(mm - 1)/2 + jn
        do ia = 1,3
          a2drv(1,ia,ind) = a2drv(1,ia,ind) + rtrm2*dwtj(ia,m,nj)*x
          a2drv(2,ia,ind) = a2drv(2,ia,ind) + rtrm2*dwtj(ia,m,nj)*y
          a2drv(3,ia,ind) = a2drv(3,ia,ind) + rtrm2*dwtj(ia,m,nj)*z
        enddo
      else
        ind = jn*(jn - 1)/2 + mm
        do ia = 1,3
          a2drv(ia,1,ind) = a2drv(ia,1,ind) + rtrm2*dwtj(ia,m,nj)*x
          a2drv(ia,2,ind) = a2drv(ia,2,ind) + rtrm2*dwtj(ia,m,nj)*y
          a2drv(ia,3,ind) = a2drv(ia,3,ind) + rtrm2*dwtj(ia,m,nj)*z
        enddo
      endif
!  M - I
      if (mm.gt.in) then
        ind = mm*(mm - 1)/2 + in
        do ia = 1,3
          a2drv(1,ia,ind) = a2drv(1,ia,ind) - rtrm2*dwtj(ia,m,nj)*x
          a2drv(2,ia,ind) = a2drv(2,ia,ind) - rtrm2*dwtj(ia,m,nj)*y
          a2drv(3,ia,ind) = a2drv(3,ia,ind) - rtrm2*dwtj(ia,m,nj)*z
        enddo
      else
        ind = in*(in - 1)/2 + mm
        do ia = 1,3
          a2drv(ia,1,ind) = a2drv(ia,1,ind) - rtrm2*dwtj(ia,m,nj)*x
          a2drv(ia,2,ind) = a2drv(ia,2,ind) - rtrm2*dwtj(ia,m,nj)*y
          a2drv(ia,3,ind) = a2drv(ia,3,ind) - rtrm2*dwtj(ia,m,nj)*z
        enddo
      endif
!  I - J
      if (in.gt.jn) then
        ind = in*(in - 1)/2 + jn
        do ia = 1,3
          a2drv(ia,1,ind) = a2drv(ia,1,ind) + rtrm2*dwtj(ia,m,nj)*x
          a2drv(ia,2,ind) = a2drv(ia,2,ind) + rtrm2*dwtj(ia,m,nj)*y
          a2drv(ia,3,ind) = a2drv(ia,3,ind) + rtrm2*dwtj(ia,m,nj)*z
        enddo
      else
        ind = jn*(jn - 1)/2 + in
        do ia = 1,3
          a2drv(1,ia,ind) = a2drv(1,ia,ind) + rtrm2*dwtj(ia,m,nj)*x
          a2drv(2,ia,ind) = a2drv(2,ia,ind) + rtrm2*dwtj(ia,m,nj)*y
          a2drv(3,ia,ind) = a2drv(3,ia,ind) + rtrm2*dwtj(ia,m,nj)*z
        enddo
      endif
!
!  Weighting factor second derivatives for vectors to j only
!
      rtrm2 = fctij*rdist*wi
      do n = 1,m
        nn = nearsasrptr(npwtptr(n,jpts))
        mn = mn + 1
        if (m.eq.n) then
          fct = 0.5_dp
        else
          fct = 1.0_dp
        endif
!  M - J
        if (jn.gt.mm) then
          ind = jn*(jn - 1)/2 + mm
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wtj(ia,ib,mn,nj)*fct
            enddo
          enddo
        else
          ind = mm*(mm - 1)/2 + jn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wtj(ib,ia,mn,nj)*fct
            enddo
          enddo
        endif
!  N - J
        if (jn.gt.nn) then
          ind = jn*(jn - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wtj(ib,ia,mn,nj)*fct
            enddo
          enddo
        else
          ind = nn*(nn - 1)/2 + jn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rtrm2*d2wtj(ia,ib,mn,nj)*fct
            enddo
          enddo
        endif
! M - N
        if (mm.gt.nn) then
          ind = mm*(mm - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rtrm2*d2wtj(ib,ia,mn,nj)*fct
            enddo
          enddo
        else
          ind = nn*(nn - 1)/2 + mm
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rtrm2*d2wtj(ia,ib,mn,nj)*fct
            enddo
          enddo
        endif
      enddo
    enddo
!
!  Weighting factor second derivatives for vectors to i and j
!
    rtrm2 = fctij*rdist*wj
    do m = 1,npwti
      mm = nearsasrptr(npwtptr(m,ipts))
      do n = 1,npwtj
        nn = nearsasrptr(npwtptr(n,jpts))
        if (mm.gt.nn) then
!  M - N
          ind = mm*(mm - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rdist*dwtj(ib,n,nj)*dwti(ia,m,ni)
            enddo
          enddo
        else
          ind = nn*(nn - 1)/2 + mm
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rdist*dwtj(ia,n,nj)*dwti(ib,m,ni)
            enddo
          enddo
        endif
!  I - N
        if (in.gt.nn) then
          ind = in*(in - 1)/2 + nn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*dwtj(ib,n,nj)*dwti(ia,m,ni)
            enddo
          enddo
        else
          ind = nn*(nn - 1)/2 + in
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*dwtj(ia,n,nj)*dwti(ib,m,ni)
            enddo
          enddo
        endif
!  M - J
        if (jn.gt.mm) then
          ind = jn*(jn - 1)/2 + mm
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*dwtj(ia,n,nj)*dwti(ib,m,ni)
            enddo
          enddo
        else
          ind = mm*(mm - 1)/2 + jn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) - rdist*dwtj(ib,n,nj)*dwti(ia,m,ni)
            enddo
          enddo
        endif
!  I - J
        if (jn.gt.in) then
          ind = jn*(jn - 1)/2 + in
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rdist*dwtj(ia,n,nj)*dwti(ib,m,ni)
            enddo
          enddo
        else
          ind = in*(in - 1)/2 + jn
          do ia = 1,3
            do ib = 1,3
              a2drv(ib,ia,ind) = a2drv(ib,ia,ind) + rdist*dwtj(ib,n,nj)*dwti(ia,m,ni)
            enddo
          enddo
        endif
      enddo
    enddo
  endif
!
  return
  end
