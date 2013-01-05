  subroutine setsegweight
!
!  Calculate weighting factors for segments
!
!   5/03 Created
!   1/05 Total segweight now computed
!   1/05 Handling of drsolv/atsrad returned to original scheme
!   2/05 spxyzouter introduced
!   3/05 Setting of surface based on both cores and shells removed
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
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: j
  integer(i4)                                 :: jj
  integer(i4)                                 :: ii
  integer(i4)                                 :: ipts
  integer(i4)                                 :: status
  logical,    dimension(:), allocatable, save :: lnearanyseg
  logical                                     :: lnozero
  real(dp)                                    :: dist
  real(dp)                                    :: dwdr
  real(dp)                                    :: d2wdr2
  real(dp)                                    :: switchfn
  real(dp)                                    :: xi
  real(dp)                                    :: yi
  real(dp)                                    :: zi
  real(dp)                                    :: xj
  real(dp)                                    :: yj
  real(dp)                                    :: zj
  real(dp)                                    :: xji
  real(dp)                                    :: yji
  real(dp)                                    :: zji
!
!  Initialise segment weighting factor
!
  segweight(1:npts) = 1.0_dp
  nnearseg(1:npts) = 0
!
!  Calculate product of taper functions over all atoms near segment
!
  if (lsegsmooth) then
    lnozero = .true.
    do ipts = 1,npts
      i = cosmoatomptr(ipts)
      xi = spxyzouter(1,ipts) 
      yi = spxyzouter(2,ipts) 
      zi = spxyzouter(3,ipts) 
      jj = 0
      do while (lnozero.and.jj.lt.ncore)
        jj = jj + 1
        ii = 0
        j = ncoptr(jj)
        do while (lnozero.and.ii.lt.iimax) 
          ii = ii + 1
          if (j.ne.i.or.ii.ne.iimid) then
            xj = xclat(j) + xvec1cell(ii)
            yj = yclat(j) + yvec1cell(ii)
            zj = zclat(j) + zvec1cell(ii)
!
!  Calculate smoothing function
!
            xji = xj - xi
            yji = yj - yi
            zji = zj - zi
            dist = xji*xji + yji*yji + zji*zji
            dist = sqrt(dist) - atsrad(j) - drsolv - cosmorange 
            if (dist .lt. -cosmorange) then
              segweight(ipts) = 0.0_dp
              lnozero = .false.
            elseif (dist .lt. 0.0_dp) then
              call switch(abs(dist),switchfn,.false.,dwdr,.false.,d2wdr2)
              segweight(ipts) = segweight(ipts)*switchfn
              nnearseg(ipts) = nnearseg(ipts) + 1
              if (nnearseg(ipts).gt.maxnearseg) then
                maxnearseg = nnearseg(ipts) + 2
                call changemaxnearseg
              endif
              nnearsegptr(nnearseg(ipts),ipts) = j
              nnearsegptrcell(nnearseg(ipts),ipts) = ii
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
!  Compute total segment weighting factor
!
  totsegweight = 0.0_dp
  do ipts = 1,npts
    totsegweight = totsegweight + segweight(ipts)
  enddo
!****************************
!  COSMIC additional setup  *
!****************************
  if (lcosmic) then 
!
!  Set up pointers to all atoms near all segments
!
    allocate(lnearanyseg(numat),stat=status)
    if (status/=0) call outofmemory('setsegweight','lnearanyseg')
!                 
!  Loop over all segment points setting logical to indicate which atoms are near segments
!                 
    lnearanyseg(1:numat) = .false.
    do ipts = 1,npts
      i = cosmoatomptr(ipts)
      lnearanyseg(i) = .true.
      do ii = 1,nnearseg(ipts)
        lnearanyseg(nnearsegptr(ii,ipts)) = .true.
      enddo   
    enddo     
!               
!  Count number of particles near segments and size arrays
!           
    nallnearseg = 0
    do i = 1,numat
      if (lnearanyseg(i)) then
        nallnearseg = nallnearseg + 1
      endif 
    enddo
    if (nallnearseg.gt.maxallnearseg) then
      maxallnearseg = nallnearseg
      call changemaxallnearseg
    endif   
!             
!  Loop over atoms building lists of those that are near the segments
!               
    nallnearseg = 0
    do i = 1,numat
      if (lnearanyseg(i)) then
        nallnearseg = nallnearseg + 1
        nallnearsegptr(nallnearseg) = i
        nallnearsegrptr(i) = nallnearseg
      endif     
    enddo       
!                   
!  Free local workspace array
!                 
    deallocate(lnearanyseg,stat=status)
    if (status/=0) call deallocate_error('setsegweight','lnearanyseg')
  endif
!
  return
  end
