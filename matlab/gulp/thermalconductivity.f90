  subroutine thermalconductivity(mcv,derv2,eigr,Sij,freq,nphonatc,ncfoc,nphonatptr,maxd2)
!
!  Compute the thermal conductivity in a quasiharmonic supercell approximation
!  according to the method of Allen and Feldman, PRB, 48, 12581 (1993)
!
!   6/12 Created 
!   1/13 JL modified
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
!  Julian Gale, NRI, Curtin University, June 2012
!
!  use constants,     
  use current
  use general,        only : bfactor
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: nphonatptr(*)
  integer(i4), intent(in)                      :: maxd2
  integer(i4), intent(in)                      :: mcv
  integer(i4), intent(in)                      :: ncfoc
  integer(i4), intent(in)                      :: nphonatc
  real(dp),    intent(inout)                   :: derv2(maxd2,*)
  real(dp),    intent(in)                      :: eigr(maxd2,*)
  real(dp),    intent(out)                     :: Sij(mcv,*)
  real(dp),    intent(in)                      :: freq(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: ncfoc2
  integer(i4)                                  :: nfreqmin
  integer(i4)                                  :: status
  logical,        allocatable,            save :: ldone(:)
  real(dp)                                     :: constant
  real(dp)                                     :: Di
  real(dp)                                     :: dwavg
  real(dp)                                     :: dwij
  real(dp),       allocatable,            save :: freqinv(:)
  real(dp)                                     :: rij
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
  real(dp)                                     :: freqsqrt
!
!  Allocate local array ldone to avoid duplicate multiplies in case of partial occupancy
!
  ncfoc2 = ncfoc*(ncfoc+1)/2
  allocate(ldone(ncfoc2),stat=status)
  if (status/=0) call outofmemory('thermalconductivity','ldone')
  ldone(1:ncfoc2) = .false.
! DEBUG
!
!  Write out a file with the eigenvectors
!
  open(91,file='eigvec.gout',status='unknown',form='formatted')
  do i = 1,mcv
    write(91,'(6f12.6)') (eigr(j,i),j=1,mcv)
  enddo
  close(91)
! DEBUG
!
!  Scale dynamical matrix elements by minimum image nearest distance between sites
!
! DEBUG
!
!  Write out a file with rij
!
 open(91,file='rij.gout',status='unknown',form='formatted')
  do i = 1,nphonatc
    ii = nphonatptr(i)
    ix = 3*ii - 2
    iy = ix + 1
    iz = ix + 2
    do j = 1,i-1
      jj = nphonatptr(j)
      ind = ii*(ii-1)/2 + jj
      if (.not.ldone(ind)) then
        jx = 3*jj - 2
        jy = jx + 1
        jz = jx + 2
!
!  Compute initial vector
!
        xd = xclat(jj) - xclat(ii)
        yd = yclat(jj) - yclat(ii)
        zd = zclat(jj) - zclat(ii)
!
!  Find minimum distance between images
!
        call nearestr(ndim,xd,yd,zd,rv,rij)
!  
        derv2(jx,ix) = derv2(jx,ix)*rij
        derv2(jy,ix) = derv2(jy,ix)*rij
        derv2(jz,ix) = derv2(jz,ix)*rij
        derv2(jx,iy) = derv2(jx,iy)*rij
        derv2(jy,iy) = derv2(jy,iy)*rij
        derv2(jz,iy) = derv2(jz,iy)*rij
        derv2(jx,iz) = derv2(jx,iz)*rij
        derv2(jy,iz) = derv2(jy,iz)*rij
        derv2(jz,iz) = derv2(jz,iz)*rij
!  
        derv2(ix,jx) = derv2(ix,jx)*rij
        derv2(iy,jx) = derv2(iy,jx)*rij
        derv2(iz,jx) = derv2(iz,jx)*rij
        derv2(ix,jy) = derv2(ix,jy)*rij
        derv2(iy,jy) = derv2(iy,jy)*rij
        derv2(iz,jy) = derv2(iz,jy)*rij
        derv2(ix,jz) = derv2(ix,jz)*rij
        derv2(iy,jz) = derv2(iy,jz)*rij
        derv2(iz,jz) = derv2(iz,jz)*rij
!
    write(91,'(i6,2x,i6,2x,f12.4)') &
&i,j,rij
! DEBUG
        ldone(ind) = .true.
      endif
    enddo
!
!  Self term is zero
!
    derv2(ix,ix) = 0.0_dp
    derv2(iy,ix) = 0.0_dp
    derv2(iz,ix) = 0.0_dp
    derv2(ix,iy) = 0.0_dp
    derv2(iy,iy) = 0.0_dp
    derv2(iz,iy) = 0.0_dp
    derv2(ix,iz) = 0.0_dp
    derv2(iy,iz) = 0.0_dp
    derv2(iz,iz) = 0.0_dp
  enddo
!
!  Multiply eigenvectors by distance weighted dynamical matrix from both sides
!
  call dgemm('N','N',mcv,mcv,mcv,1.0_dp,derv2,maxd2,eigr,maxd2,0.0_dp,Sij,mcv)
  call dgemm('N','N',mcv,mcv,mcv,1.0_dp,eigr,maxd2,Sij,mcv,0.0_dp,derv2,maxd2)
!
!  Copy results back to Sij
!
  Sij(1:mcv,1:mcv) = derv2(1:mcv,1:mcv)
!
!  Create inverse frequency factors while trapping translations and imaginary modes
!
  allocate(freqinv(mcv),stat=status)
  if (status/=0) call outofmemory('thermalconductivity','freqinv')
  nfreqmin = 0
  do i = 1,mcv
    if (freq(i).gt.1.0_dp) then
      if (nfreqmin.eq.0) nfreqmin = i
      freqinv(i) = 1.0_dp/sqrt(2.0_dp*freq(i))
    else
      freqinv(i) = 1.0_dp
    endif
  enddo
! DEBUG
!
!  Find mean level spacing
!
dwavg = 0.0_dp;
do i = nfreqmin,mcv-1
  dwavg = freq(i+1) - freq(i) + dwavg
enddo
  dwavg = dwavg/(mcv-1 - nfreqmin)
! DEBUG
!
!  Scale by constants and frequency factors to get to Sij
!
  do i = 1,mcv
    do j = 1,mcv
	freqsqrt = sqrt(freqinv(i)*freqinv(j))
      Sij(j,i) = Sij(j,i)*freqsqrt*(freq(i) + freq(j))
    enddo
  enddo
! DEBUG
!
!  Write out a file with Sij
!
  open(91,file='Sij.gout',status='unknown',form='formatted')
  do i = 1,mcv
    write(91,'('' Mode = '',i6)') i
    write(91,'(6f12.6)') (Sij(j,i),j=1,mcv)
  enddo
  close(91)
! DEBUG
!
!  Output banner for thermal conductivity
!
  if (ioproc) then
    write(ioout,'(/,''  Thermal conductivity: '',/)')
    write(ioout,'(''  Lorentzian broadening factor = '',f10.4,/)') bfactor*dwavg
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'('' Mode    : Frequency (cm-1)         Thermal conductivity                        '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  Compute Di values (factors of pi have been cancelled)
!
open(91,file='Dij.gout',status='unknown',form='formatted')
  constant = 1.0_dp/12.0_dp    ! 1/3 convoluted with 1/2 squared from A7
  do i = nfreqmin,mcv
    Di = 0.0_dp
!
!  Sum over coupling with mode j weighted by Lorentzian factor
!
    do j = nfreqmin,i-1
      dwij = (bfactor*dwavg)/(1 + ((bfactor*dwavg)*(freq(j) - freq(i)))**2)
      Di = Di + dwij*Sij(j,i)**2
    enddo
    do j = i+1,mcv
      dwij = (bfactor*dwavg)/(1 + ((bfactor*dwavg)*(freq(j) - freq(i)))**2)
      Di = Di + dwij*Sij(j,i)**2
    enddo
!
!  Scale by constants and inverse frequency squared
!
    Di = Di*constant/freq(i)**2     
    if (ioproc) then
      write(ioout,'(i6,2x,f12.4,10x,f18.8)') i,freq(i),Di
    endif
!
!  Write out Di to separate file
!
   write(91,'(i6,2x,f12.4,10x,f18.8)') i,freq(i),Di
  enddo
!
!  Close output
!
  if (ioproc) then
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Deallocate local memory
!
  deallocate(ldone,stat=status)
  if (status/=0) call deallocate_error('thermalconductivity','ldone')
  deallocate(freqinv,stat=status)
  if (status/=0) call deallocate_error('thermalconductivity','freqinv')
!
  return
  end
