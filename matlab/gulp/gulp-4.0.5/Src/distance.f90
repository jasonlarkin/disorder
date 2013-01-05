  subroutine distance
!
!  Subroutine for calculating interatomic distances
!
!   8/98 Cell looping range increased to avoid problems with
!        cells with small angles
!   4/02 Units output added for cutoff distance
!   7/05 Order of deallocations reversed
!   9/07 Modified to avoid out of bounds issues
!  11/07 Unused variables removed
!   5/12 Format statements modified to allow for larger atom
!        numbers.
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use current
  use element, only : maxele
  use general
  use iochannels
  use species
  implicit none
!
!  Local variables
!
  character(len=5)                             :: labi
  character(len=5)                             :: labj
  character(len=5)                             :: stype(2)
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: m
  integer(i4)                                  :: max1
  integer(i4)                                  :: max1l
  integer(i4)                                  :: max2
  integer(i4)                                  :: max2l
  integer(i4)                                  :: max3
  integer(i4)                                  :: max3l
  integer(i4)                                  :: maxtype
  integer(i4)                                  :: n
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nline
  integer(i4)                                  :: nodd
  integer(i4)                                  :: nptr
  integer(i4)                                  :: nsptr1
  integer(i4)                                  :: nsptr2
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntj
  integer(i4)                                  :: nttype
  integer(i4)                                  :: ntype
  integer(i4)                                  :: numt
  integer(i4), dimension(:), allocatable       :: numtype
  integer(i4)                                  :: status
  logical                                      :: lfout
  real(dp)                                     :: cut2
  real(dp)                                     :: diff
  real(dp)                                     :: r
  real(dp)                                     :: rmin
  real(dp)                                     :: rtmp
  real(dp),    dimension(:), allocatable       :: rtype
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcdj
  real(dp)                                     :: ycdj
  real(dp)                                     :: zcdj
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  data stype/'core ','shell'/
!
!  Set up local variables
!
!  Generate maximum looping indices
!
  if (ndim.eq.3) then
    max1 = cutb/a + 2
    max2 = cutb/b + 2
    max3 = cutb/c + 2
    max1l = max1 + 1
    max2l = max2 + 1
    max3l = max3 + 1
  elseif (ndim.eq.2) then
    max1 = cutb/a + 2
    max2 = cutb/b + 2
    max1l = max1 + 1
    max2l = max2 + 1
    max3 = 0
    max3l = 0
  elseif (ndim.eq.1) then
    max1 = cutb/a + 2
    max1l = max1 + 1
    max2 = 0
    max2l = 0
    max3 = 0
    max3l = 0
  else
    max1 = 0
    max1l = 0
    max2 = 0
    max2l = 0
    max3 = 0
    max3l = 0
  endif
  cut2 = cutb*cutb
  maxtype = max(numat,24_i4)
10 continue
  allocate(numtype(maxtype),stat=status)
  if (status/=0) call outofmemory('distance','numtype')
  allocate(rtype(maxtype),stat=status)
  if (status/=0) call outofmemory('distance','rtype')
!
!  Output header
!
  write(ioout,'(/,''  Distance calculation :'',/)')
  write(ioout,'(/,''  Cutoff for distances  =  '',f12.6,'' Angstroms''/)')cutb
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Asymmetric unit site  Full lattice sites    No.  Distance      No.  Distance'')')
  write(ioout,'(''      No.   At.No.            At.No.                (Angs)             (Angs) '')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Loop over sites
!
  do i = 1,nasym
    xcd = xalat(i)
    ycd = yalat(i)
    zcd = zalat(i)
    ni = iatn(i)
    if (ni.gt.maxele) then
      ni = ni - maxele
      nsptr1 = 2
    else
      nsptr1 = 1
    endif
    nti = natype(i)
    call label(ni,nti,labi)
    lfout = .true.
    nttype = 0
!
!  Loop over second site
!
    do j = 1,nspec
      ntype = 0
      nj = natspec(j)
      ntj = ntypspec(j)
      call label(nj,ntj,labj)
      if (nj.gt.maxele) then
        nsptr2 = 2
      else
        nsptr2 = 1
      endif
      do k = 1,numat
        if (nat(k).eq.nj.and.(nftype(k).eq.ntj.or.ntj.eq.0)) then
          xcdi = xclat(k) - xcd - max1l*r1x
          ycdi = yclat(k) - ycd - max1l*r1y
          zcdi = zclat(k) - zcd - max1l*r1z
!
!  Loop over unit cells
!
          do ii = -max1,max1
            xcdi = xcdi + r1x
            ycdi = ycdi + r1y
            zcdi = zcdi + r1z
            xcdj = xcdi - max2l*r2x
            ycdj = ycdi - max2l*r2y
            zcdj = zcdi - max2l*r2z
            do jj = -max2,max2
              xcdj = xcdj + r2x
              ycdj = ycdj + r2y
              zcdj = zcdj + r2z
              xcrd = xcdj - max3l*r3x
              ycrd = ycdj - max3l*r3y
              zcrd = zcdj - max3l*r3z
              do kk = -max3,max3
                xcrd = xcrd + r3x
                ycrd = ycrd + r3y
                zcrd = zcrd + r3z
                r = xcrd**2 + ycrd**2 + zcrd**2
                if (r.le.cut2.and.r.gt.0.0001_dp) then
                  r = sqrt(r)
!
!  Check to see if distance is equivalent to any that have
!  already been characterised
!
                  if (ntype.eq.0) then
                    ntype = ntype + 1
                    numtype(ntype) = 1
                    rtype(ntype) = r
                  else
                    do n = 1,ntype
                      diff = abs(r-rtype(n))
                      if (diff.lt.1d-4) then
                        numtype(n) = numtype(n) + 1
                        goto 20
                      endif
                    enddo
                    ntype = ntype + 1
                    if (ntype.gt.maxtype) then
                      deallocate(rtype,stat=status)
                      if (status/=0) call deallocate_error('distance','rtype')
                      deallocate(numtype,stat=status)
                      if (status/=0) call deallocate_error('distance','numtype')
                      goto 10
                    endif
                    numtype(ntype) = 1
                    rtype(ntype) = r
20                  continue
                  endif
                endif
!
!  End of loops over cell vectors
!
              enddo
            enddo
          enddo
        endif
      enddo
!
!  Shuffle distances into ascending order
!
      do k = 1,ntype
        rmin = 100.0_dp
        do m = k,ntype
          if (rtype(m).lt.rmin) then
            rmin = rtype(m)
            nptr = m
          endif
        enddo
        if (nptr.ne.k) then
          rtmp = rtype(k)
          rtype(k) = rtype(nptr)
          rtype(nptr) = rtmp
          numt = numtype(k)
          numtype(k) = numtype(nptr)
          numtype(nptr) = numt
        endif
      enddo
      nttype = nttype + ntype
!
!  Output details
!
      if (ntype.gt.0) then
        if (lfout) then
          lfout = .false.
          nline = (ntype-1)/2 + 1
          nodd = ntype-2*(nline-1)
          if (nline.gt.0) then
            if (ntype.ge.2) then
              write(ioout,'(1x,i7,3x,a5,1x,a5,7x,a5,1x,a5,2x,2(i7,2x,f8.4,2x))') &
                i,labi,stype(nsptr1),labj,stype(nsptr2),numtype(1),rtype(1),numtype(2),rtype(2)
            else
              write(ioout,'(1x,i7,3x,a5,1x,a5,7x,a5,1x,a5,2x,i7,2x,f8.4)') &
                i,labi,stype(nsptr1),labj,stype(nsptr2),numtype(1),rtype(1)
            endif
            if (nline.gt.1) then
              if (nodd.eq.1) then
                ind = 1
                do m = 2,nline-1
                  ind = ind + 2
                  write(ioout,'(42x,2(i7,2x,f8.4,2x))') &
                    numtype(ind),rtype(ind),numtype(ind + 1),rtype(ind + 1)
                enddo
                ind = ind + 2
                write(ioout,'(42x,i7,2x,f8.4,2x)')numtype(ind),rtype(ind)
              else
                ind = 1
                do m = 2,nline
                  ind = ind + 2
                  write(ioout,'(42x,2(i7,2x,f8.4,2x))')numtype(ind),rtype(ind),numtype(ind + 1),rtype(ind + 1)
                enddo
              endif
            endif
          endif
        else
          nline = (ntype-1)/2 + 1
          nodd = ntype-2*(nline-1)
          if (nline.gt.0) then
            if (ntype.ge.2) then
              write(ioout,'(29x,a5,1x,a5,2x,2(i7,2x,f8.4,2x))') &
                labj,stype(nsptr2),numtype(1),rtype(1),numtype(2),rtype(2)
            else
              write(ioout,'(29x,a5,1x,a5,2x,i7,2x,f8.4)')labj,stype(nsptr2),numtype(1),rtype(1)
            endif
            if (nline.gt.1) then
              if (nodd.eq.1) then
                ind = 1
                do m = 2,nline-1
                  ind = ind + 2
                  write(ioout,'(42x,2(i7,2x,f8.4,2x))')numtype(ind),rtype(ind),numtype(ind + 1),rtype(ind + 1)
                enddo
                ind = ind + 2
                write(ioout,'(42x,i7,2x,f8.4,2x)')numtype(ind),rtype(ind)
              else
                ind = 1
                do m = 2,nline
                  ind = ind + 2
                  write(ioout,'(42x,2(i7,2x,f8.4,2x))')numtype(ind),rtype(ind),numtype(ind + 1),rtype(ind + 1)
                enddo
              endif
            endif
          endif
        endif
      endif
!
!  Zero arrays for next species type
!
      do m = 1,ntype
        numtype(m) = 0
        rtype(m) = 0.0d0
      enddo
!
!  End loop over types of species
!
    enddo
    if (nttype.gt.0) write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  End of asymmetric atom loop
!
  enddo
  write(ioout,'(/)')
  deallocate(rtype,stat=status)
  if (status/=0) call deallocate_error('distance','rtype')
  deallocate(numtype,stat=status)
  if (status/=0) call deallocate_error('distance','numtype')
!
  return
  end
