  subroutine setdsegweight(lgrad1,lgrad2,dsegweight,d2segweight,dtotsegweight,d2totsegweight)
!
!  Calculate derivatives of weighting factors for segments
!
!   5/03 Created
!   1/05 Calculation of normalised total segment weight derivatives
!        added for modified COSMIC scheme
!   1/05 Handling of drsolv/atsrad returned to original scheme
!   2/05 spxyzouter introduced
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
!  Julian Gale, NRI, Curtin University, December 2008
!
  use cosmo
  use current
  use shell
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                     :: lgrad1
  logical,     intent(in)                     :: lgrad2
  real(dp),    intent(out)                    :: dsegweight(3,maxnearseg,*)
  real(dp),    intent(out)                    :: d2segweight(3,3,maxnearseg,maxnearseg,*)
  real(dp),    intent(out)                    :: dtotsegweight(3,maxallnearseg)
  real(dp),    intent(out)                    :: d2totsegweight(3,3,*)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: ii
  integer(i4)                                 :: ii2
  integer(i4)                                 :: ind
  integer(i4)                                 :: ipts
  integer(i4)                                 :: j
  integer(i4)                                 :: jj
  integer(i4)                                 :: k
  integer(i4)                                 :: kk
  integer(i4)                                 :: ll
  integer(i4)                                 :: status
  real(dp)                                    :: d1
  real(dp)                                    :: d1k
  real(dp)                                    :: d2
  real(dp)                                    :: dist
  real(dp),   dimension(:), allocatable, save :: dwdr
  real(dp),   dimension(:), allocatable, save :: d2wdr2
  real(dp)                                    :: r
  real(dp)                                    :: rdist
  real(dp),   dimension(:), allocatable, save :: segw
  real(dp)                                    :: xi
  real(dp)                                    :: yi
  real(dp)                                    :: zi
  real(dp)                                    :: xj
  real(dp)                                    :: yj
  real(dp)                                    :: zj
  real(dp)                                    :: xji
  real(dp)                                    :: yji
  real(dp)                                    :: zji
  real(dp)                                    :: xk
  real(dp)                                    :: yk
  real(dp)                                    :: zk
  real(dp)                                    :: xki
  real(dp)                                    :: yki
  real(dp)                                    :: zki
!
!  Initialise derivatives of segment weighting factor
!
  if (lgrad1) then
    do ipts = 1,npts
      do j = 1,nnearseg(ipts)
        dsegweight(1:3,j,ipts) = 0.0_dp
      enddo
    enddo
    if (lgrad2) then
      do ipts = 1,npts
        do j = 1,nnearseg(ipts)
          do k = 1,nnearseg(ipts)
            d2segweight(1:3,1:3,k,j,ipts) = 0.0_dp
          enddo
        enddo
      enddo
    endif
  endif
!
!  Calculate product of taper functions over all atoms near segment
!
  if (lsegsmooth.and.lgrad1) then
    allocate(segw(maxnearseg),stat=status)
    if (status/=0) call outofmemory('setdsegweight','segw')
    allocate(dwdr(maxnearseg),stat=status)
    if (status/=0) call outofmemory('setdsegweight','dwdr')
    allocate(d2wdr2(maxnearseg),stat=status)
    if (status/=0) call outofmemory('setdsegweight','d2wdr2')
    do ipts = 1,npts
      i = cosmoatomptr(ipts)
      xi = spxyzouter(1,ipts) 
      yi = spxyzouter(2,ipts) 
      zi = spxyzouter(3,ipts) 
!
!  Loop over atom images near segment 
!
      do jj = 1,nnearseg(ipts)
        j = nnearsegptr(jj,ipts)
        ii = nnearsegptrcell(jj,ipts)
!
!  Calculate i - j vector and distance
!
        xj = xclat(j) + xvec1cell(ii)
        yj = yclat(j) + yvec1cell(ii)
        zj = zclat(j) + zvec1cell(ii)
        xji = xj - xi
        yji = yj - yi
        zji = zj - zi
        dist = xji*xji + yji*yji + zji*zji
        dist = sqrt(dist)
        r = dist - atsrad(j) - drsolv - cosmorange 
!
!  Calculate smoothing function and required derivatives
!
        call switch(abs(r),segw(jj),lgrad1,dwdr(jj),lgrad2,d2wdr2(jj))
!
!  Divide dwdr by dist to avoid multiple computation
!
!  Note that because there is an issue with the weight factor distance being
!  in the opposite sense to the interatomic vector, some of the signs of terms
!  below look unconventional!
!
        rdist = 1.0_dp/dist
        dwdr(jj) = - dwdr(jj)*rdist
        d2wdr2(jj) = (d2wdr2(jj) - dwdr(jj))*rdist*rdist
      enddo
!
!  Having collected all the terms loop over atom images again and calculate derivatives
!
      do jj = 1,nnearseg(ipts)
        j = nnearsegptr(jj,ipts)
        ii = nnearsegptrcell(jj,ipts)
!
!  Calculate i - j vector and distance
!
        xj = xclat(j) + xvec1cell(ii)
        yj = yclat(j) + yvec1cell(ii)
        zj = zclat(j) + zvec1cell(ii)
        xji = xj - xi
        yji = yj - yi
        zji = zj - zi
!
!  Calculate first derivatives
!
        d1 = dwdr(jj)
        dsegweight(1,jj,ipts) = dsegweight(1,jj,ipts) + d1*xji
        dsegweight(2,jj,ipts) = dsegweight(2,jj,ipts) + d1*yji
        dsegweight(3,jj,ipts) = dsegweight(3,jj,ipts) + d1*zji
!
!  Calculate second derivatives
!
        if (lgrad2) then
!
!  Second derivatives with respect to same distance
!
          d2 = d2wdr2(jj)
          d2segweight(1,1,jj,jj,ipts) = d2segweight(1,1,jj,jj,ipts) - d2*xji*xji - d1
          d2segweight(2,1,jj,jj,ipts) = d2segweight(2,1,jj,jj,ipts) - d2*yji*xji
          d2segweight(3,1,jj,jj,ipts) = d2segweight(3,1,jj,jj,ipts) - d2*zji*xji
          d2segweight(1,2,jj,jj,ipts) = d2segweight(1,2,jj,jj,ipts) - d2*xji*yji
          d2segweight(2,2,jj,jj,ipts) = d2segweight(2,2,jj,jj,ipts) - d2*yji*yji - d1
          d2segweight(3,2,jj,jj,ipts) = d2segweight(3,2,jj,jj,ipts) - d2*zji*yji
          d2segweight(1,3,jj,jj,ipts) = d2segweight(1,3,jj,jj,ipts) - d2*xji*zji
          d2segweight(2,3,jj,jj,ipts) = d2segweight(2,3,jj,jj,ipts) - d2*yji*zji
          d2segweight(3,3,jj,jj,ipts) = d2segweight(3,3,jj,jj,ipts) - d2*zji*zji - d1
!
!  Second derivatives with respect to a pair of different distances
!
!
!  Having collected all the terms loop over atom images again and calculate derivatives
!
          do kk = 1,nnearseg(ipts) 
            if (kk.ne.jj) then
              k = nnearsegptr(kk,ipts)
              ii2 = nnearsegptrcell(kk,ipts)
!
!  Calculate i - j vector and distance
!
              xk = xclat(k) + xvec1cell(ii2)
              yk = yclat(k) + yvec1cell(ii2)
              zk = zclat(k) + zvec1cell(ii2)
              xki = xk - xi
              yki = yk - yi
              zki = zk - zi
!
!  Calculate first derivatives with respect to this distance
!
              d1k = dwdr(kk)
              d2segweight(1,1,kk,jj,ipts) = d2segweight(1,1,kk,jj,ipts) + d1*d1k*xki*xji
              d2segweight(2,1,kk,jj,ipts) = d2segweight(2,1,kk,jj,ipts) + d1*d1k*yki*xji
              d2segweight(3,1,kk,jj,ipts) = d2segweight(3,1,kk,jj,ipts) + d1*d1k*zki*xji
              d2segweight(1,2,kk,jj,ipts) = d2segweight(1,2,kk,jj,ipts) + d1*d1k*xki*yji
              d2segweight(2,2,kk,jj,ipts) = d2segweight(2,2,kk,jj,ipts) + d1*d1k*yki*yji
              d2segweight(3,2,kk,jj,ipts) = d2segweight(3,2,kk,jj,ipts) + d1*d1k*zki*yji
              d2segweight(1,3,kk,jj,ipts) = d2segweight(1,3,kk,jj,ipts) + d1*d1k*xki*zji
              d2segweight(2,3,kk,jj,ipts) = d2segweight(2,3,kk,jj,ipts) + d1*d1k*yki*zji
              d2segweight(3,3,kk,jj,ipts) = d2segweight(3,3,kk,jj,ipts) + d1*d1k*zki*zji
            endif
          enddo
        endif
      enddo
!
!  Multiply by other weighting factors
!
      do jj = 1,nnearseg(ipts)
        do kk = 1,nnearseg(ipts)
          if (kk.ne.jj) then
            dsegweight(1,jj,ipts) = dsegweight(1,jj,ipts)*segw(kk)
            dsegweight(2,jj,ipts) = dsegweight(2,jj,ipts)*segw(kk)
            dsegweight(3,jj,ipts) = dsegweight(3,jj,ipts)*segw(kk)
          endif
          if (lgrad2) then
            do ll = 1,nnearseg(ipts)
              if (ll.ne.jj.and.ll.ne.kk) then
                d2segweight(1,1,kk,jj,ipts) = d2segweight(1,1,kk,jj,ipts)*segw(ll)
                d2segweight(2,1,kk,jj,ipts) = d2segweight(2,1,kk,jj,ipts)*segw(ll)
                d2segweight(3,1,kk,jj,ipts) = d2segweight(3,1,kk,jj,ipts)*segw(ll)
                d2segweight(1,2,kk,jj,ipts) = d2segweight(1,2,kk,jj,ipts)*segw(ll)
                d2segweight(2,2,kk,jj,ipts) = d2segweight(2,2,kk,jj,ipts)*segw(ll)
                d2segweight(3,2,kk,jj,ipts) = d2segweight(3,2,kk,jj,ipts)*segw(ll)
                d2segweight(1,3,kk,jj,ipts) = d2segweight(1,3,kk,jj,ipts)*segw(ll)
                d2segweight(2,3,kk,jj,ipts) = d2segweight(2,3,kk,jj,ipts)*segw(ll)
                d2segweight(3,3,kk,jj,ipts) = d2segweight(3,3,kk,jj,ipts)*segw(ll)
              endif
            enddo
          endif
        enddo
      enddo
    enddo
    deallocate(d2wdr2,stat=status)
    if (status/=0) call deallocate_error('setdsegweight','d2wdr2')
    deallocate(dwdr,stat=status)
    if (status/=0) call deallocate_error('setdsegweight','dwdr')
    deallocate(segw,stat=status)
    if (status/=0) call deallocate_error('setdsegweight','segw')
  endif
!**************************************
!  COSMIC : Total weight derivatives  *
!**************************************
  if (lcosmic) then
!
!  Zero derivatives
!
    dtotsegweight(1:3,1:nallnearseg) = 0.0_dp
!
!  Compute sum of first derivative weights
!
    do ipts = 1,npts
      i = nallnearsegrptr(cosmoatomptr(ipts))
      do jj = 1,nnearseg(ipts)
        j = nallnearsegrptr(nnearsegptr(jj,ipts))
        dtotsegweight(1,i) = dtotsegweight(1,i) - dsegweight(1,jj,ipts)
        dtotsegweight(2,i) = dtotsegweight(2,i) - dsegweight(2,jj,ipts)
        dtotsegweight(3,i) = dtotsegweight(3,i) - dsegweight(3,jj,ipts)
        dtotsegweight(1,j) = dtotsegweight(1,j) + dsegweight(1,jj,ipts)
        dtotsegweight(2,j) = dtotsegweight(2,j) + dsegweight(2,jj,ipts)
        dtotsegweight(3,j) = dtotsegweight(3,j) + dsegweight(3,jj,ipts)
      enddo
    enddo
    if (lgrad2) then
!
!  Zero derivatives
!
      d2totsegweight(1:3,1:3,1:nallnearseg*(nallnearseg+1)/2) = 0.0_dp
!
!  Compute sum of second derivative weights
!
      do ipts = 1,npts
        i = nallnearsegrptr(cosmoatomptr(ipts))
        do jj = 1,nnearseg(ipts)
          j = nallnearsegrptr(nnearsegptr(jj,ipts))
          if (i.ge.j) then
            ind = i*(i - 1)/2 + j
            d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) - d2segweight(1,1,jj,jj,ipts)
            d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) - d2segweight(2,1,jj,jj,ipts)
            d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) - d2segweight(3,1,jj,jj,ipts)
            d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) - d2segweight(1,2,jj,jj,ipts)
            d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) - d2segweight(2,2,jj,jj,ipts)
            d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) - d2segweight(3,2,jj,jj,ipts)
            d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) - d2segweight(1,3,jj,jj,ipts)
            d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) - d2segweight(2,3,jj,jj,ipts)
            d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) - d2segweight(3,3,jj,jj,ipts)
          else
            ind = j*(j - 1)/2 + i
            d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) - d2segweight(1,1,jj,jj,ipts)
            d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) - d2segweight(1,2,jj,jj,ipts)
            d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) - d2segweight(1,3,jj,jj,ipts)
            d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) - d2segweight(2,1,jj,jj,ipts)
            d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) - d2segweight(2,2,jj,jj,ipts)
            d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) - d2segweight(2,3,jj,jj,ipts)
            d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) - d2segweight(3,1,jj,jj,ipts)
            d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) - d2segweight(3,2,jj,jj,ipts)
            d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) - d2segweight(3,3,jj,jj,ipts)
          endif
        enddo
        do jj = 2,nnearseg(ipts)
          j = nallnearsegrptr(nnearsegptr(jj,ipts))
          do kk = 1,jj-1
            k = nallnearsegrptr(nnearsegptr(kk,ipts))
            if (j.ge.k) then
              ind = j*(j - 1)/2 + k
              d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) - d2segweight(1,1,kk,jj,ipts)
              d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) - d2segweight(2,1,kk,jj,ipts)
              d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) - d2segweight(3,1,kk,jj,ipts)
              d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) - d2segweight(1,2,kk,jj,ipts)
              d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) - d2segweight(2,2,kk,jj,ipts)
              d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) - d2segweight(3,2,kk,jj,ipts)
              d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) - d2segweight(1,3,kk,jj,ipts)
              d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) - d2segweight(2,3,kk,jj,ipts)
              d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) - d2segweight(3,3,kk,jj,ipts)
            else
              ind = k*(k - 1)/2 + j
              d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) - d2segweight(1,1,kk,jj,ipts)
              d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) - d2segweight(1,2,kk,jj,ipts)
              d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) - d2segweight(1,3,kk,jj,ipts)
              d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) - d2segweight(2,1,kk,jj,ipts)
              d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) - d2segweight(2,2,kk,jj,ipts)
              d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) - d2segweight(2,3,kk,jj,ipts)
              d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) - d2segweight(3,1,kk,jj,ipts)
              d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) - d2segweight(3,2,kk,jj,ipts)
              d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) - d2segweight(3,3,kk,jj,ipts)
            endif
!
            if (i.ge.j) then
              ind = i*(i - 1)/2 + j
              d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) + d2segweight(1,1,kk,jj,ipts)
              d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) + d2segweight(1,2,kk,jj,ipts)
              d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) + d2segweight(1,3,kk,jj,ipts)
              d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) + d2segweight(2,1,kk,jj,ipts)
              d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) + d2segweight(2,2,kk,jj,ipts)
              d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) + d2segweight(2,3,kk,jj,ipts)
              d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) + d2segweight(3,1,kk,jj,ipts)
              d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) + d2segweight(3,2,kk,jj,ipts)
              d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) + d2segweight(3,3,kk,jj,ipts)
            else
              ind = j*(j - 1)/2 + i
              d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) + d2segweight(1,1,kk,jj,ipts)
              d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) + d2segweight(2,1,kk,jj,ipts)
              d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) + d2segweight(3,1,kk,jj,ipts)
              d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) + d2segweight(1,2,kk,jj,ipts)
              d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) + d2segweight(2,2,kk,jj,ipts)
              d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) + d2segweight(3,2,kk,jj,ipts)
              d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) + d2segweight(1,3,kk,jj,ipts)
              d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) + d2segweight(2,3,kk,jj,ipts)
              d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) + d2segweight(3,3,kk,jj,ipts)
            endif
!
            if (i.ge.k) then
              ind = i*(i - 1)/2 + k
              d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) + d2segweight(1,1,kk,jj,ipts)
              d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) + d2segweight(2,1,kk,jj,ipts)
              d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) + d2segweight(3,1,kk,jj,ipts)
              d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) + d2segweight(1,2,kk,jj,ipts)
              d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) + d2segweight(2,2,kk,jj,ipts)
              d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) + d2segweight(3,2,kk,jj,ipts)
              d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) + d2segweight(1,3,kk,jj,ipts)
              d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) + d2segweight(2,3,kk,jj,ipts)
              d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) + d2segweight(3,3,kk,jj,ipts)
            else
              ind = k*(k - 1)/2 + i
              d2totsegweight(1,1,ind) = d2totsegweight(1,1,ind) + d2segweight(1,1,kk,jj,ipts)
              d2totsegweight(2,1,ind) = d2totsegweight(2,1,ind) + d2segweight(1,2,kk,jj,ipts)
              d2totsegweight(3,1,ind) = d2totsegweight(3,1,ind) + d2segweight(1,3,kk,jj,ipts)
              d2totsegweight(1,2,ind) = d2totsegweight(1,2,ind) + d2segweight(2,1,kk,jj,ipts)
              d2totsegweight(2,2,ind) = d2totsegweight(2,2,ind) + d2segweight(2,2,kk,jj,ipts)
              d2totsegweight(3,2,ind) = d2totsegweight(3,2,ind) + d2segweight(2,3,kk,jj,ipts)
              d2totsegweight(1,3,ind) = d2totsegweight(1,3,ind) + d2segweight(3,1,kk,jj,ipts)
              d2totsegweight(2,3,ind) = d2totsegweight(2,3,ind) + d2segweight(3,2,kk,jj,ipts)
              d2totsegweight(3,3,ind) = d2totsegweight(3,3,ind) + d2segweight(3,3,kk,jj,ipts)
            endif
          enddo
        enddo
      enddo
    endif
  endif
!
  return
  end
