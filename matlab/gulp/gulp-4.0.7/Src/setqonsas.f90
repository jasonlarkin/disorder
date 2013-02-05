  subroutine setqonsas(lgrad2)
!
!  Subroutine calculates the charges on the solvent accessible surface
!
!  12/04 Created from combination of solvation.f and setcosmobmat.f
!  12/04 lgrad2 passed as argument and checks added as to whether
!        iterative method can be used
!   1/05 Modified COSMIC charges now set in which correction is multiplied
!        by segment weight
!   1/05 Output SAS option added
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
  use control,   only : keyword
  use cosmo
  use current
  use files,     only : lsas, sasfile
  use iochannels
  use parallel,  only : ioproc
  implicit none
!
!  Passed variables
!
  logical,      intent(in)                    :: lgrad2
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: info
  integer(i4)                                 :: j
  integer(i4)                                 :: k
  integer(i4)                                 :: l
  integer(i4)                                 :: ipts
  integer(i4)                                 :: kk
  integer(i4)                                 :: kkk
  integer(i4),                           save :: nptslast = 0
  integer(i4)                                 :: status
  real(dp),                 allocatable, save :: Afull(:,:)
  real(dp)                                    :: bhk
  real(dp)                                    :: dqstot(3)
!
  if (.not.lgrad2.and..not.lcosmic.and.index(keyword,'qite').ne.0) then
!************************
!  Iterative algorithm  *
!************************
    allocate(Afull(npts,npts),stat=status)
    if (status/=0) call outofmemory('setqonsas','Afull')
    kk = 0
    do i = 1,npts
      do j = 1,i-1
        kk = kk + 1
        Afull(j,i) = cosmoA(kk)
        Afull(i,j) = cosmoA(kk)
      enddo
      kk = kk + 1
      Afull(i,i) = cosmoA(kk)
    enddo
!
!  On first call initialise charges on sas to zero
!
    if (npts.ne.nptslast) then
      qonsas(1:npts) = 0.0d0
      nptslast = npts
    endif
    call iterinv(npts,Afull,npts,cosmoBq,qonsas,info)
    deallocate(Afull,stat=status)
    if (status/=0) call deallocate_error('setqonsas','Afull')
  else
!*********************
!  Direct algorithm  *
!*********************
!     
!  Invert A matrix
!     
    call setcosmoainv
!
!  Calculate charges on SAS
!
    qonsas(1:npts) = 0.0_dp
    kk = 0
    do k = 1,npts
      bhk = 0.0_dp
      do l = 1,k
        kk = kk + 1
        bhk = bhk + cosmoA(kk)*cosmoBq(l)
      enddo
      kkk = kk 
      do l = k+1,npts
        kkk = kkk + l - 1
        bhk = bhk + cosmoA(kkk)*cosmoBq(l)
      enddo
      qonsas(k) = qonsas(k) - bhk
    enddo
  endif
!
!  Calculate sum of SAS charges  
!
  qonsastot = 0.0_dp
  do ipts = 1,npts
    qonsastot = qonsastot + qonsas(ipts)
  enddo
!**********************
!  COSMIC correction  *
!**********************
  if (lcosmic.and.totsegweight.gt.1.0d-12) then
    deltaq = qonsastot/totsegweight
  else 
    deltaq = 0.0_dp
  endif
!*********************************
!  Print out charges on surface  *
!*********************************
  if (index(keyword,'qsas').ne.0.and.ioproc) then   
!
!  Calculate dipole for output
!
    dqstot(1:3) = 0.0_dp
    do ipts = 1,npts
      dqstot(1) = dqstot(1) + (qonsas(ipts) - deltaq*segweight(ipts))*spxyz(1,ipts)
      dqstot(2) = dqstot(2) + (qonsas(ipts) - deltaq*segweight(ipts))*spxyz(2,ipts)
      dqstot(3) = dqstot(3) + (qonsas(ipts) - deltaq*segweight(ipts))*spxyz(3,ipts)
    enddo
!
    write(ioout,'(/,''  Total charge on Solvent Accessible Surface = '',f12.6)') qonsastot
    if (ndim.eq.0) then
      write(ioout,'(''  Dipole in  X on Solvent Accessible Surface = '',f12.6)') dqstot(1)
      write(ioout,'(''  Dipole in  Y on Solvent Accessible Surface = '',f12.6)') dqstot(2)
      write(ioout,'(''  Dipole in  Z on Solvent Accessible Surface = '',f12.6)') dqstot(3)
    elseif (ndim.eq.1) then
      write(ioout,'(''  Dipole in  Y on Solvent Accessible Surface = '',f12.6)') dqstot(2)
      write(ioout,'(''  Dipole in  Z on Solvent Accessible Surface = '',f12.6)') dqstot(3)
    elseif (ndim.eq.2) then
      write(ioout,'(''  Dipole in  Z on Solvent Accessible Surface = '',f12.6)') dqstot(3)
    endif
    write(ioout,'(/,''  Charges on Solvent Accessible Surface :'',/)')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    write(ioout,'(''  Segment    x (Angs)     y (Angs)     z (Angs)       Charge      Weight'')')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    do ipts = 1,npts
      write(ioout,'(i8,2x,3(1x,f12.5),2(2x,f12.8))') ipts,spxyz(1,ipts),spxyz(2,ipts),spxyz(3,ipts),  &
        (qonsas(ipts) - deltaq*segweight(ipts)),segweight(ipts)
    enddo
    write(ioout,'(''-------------------------------------------------------------------------------'',/)')
  endif
!********************
!  Output SAS file  *
!********************
  if (lsas.and.ioproc) then
!
!  Open file 
!
    if (sasfile(1:1).ne.' ') then
      open(22,file=sasfile,status='unknown')
    else
      open(22,status='unknown')
    endif
!     
!  Output number of points
!
    write(22,'(i8)') npts
!     
!  Output dimensionality
!
    write(22,'(i8)') ndim
!     
!  Output vectors
!
    if (ndim.gt.0) then
      do i = 1,ndim
        write(22,'(3(f14.6,1x))') (rv(j,i),j=1,ndim)
      enddo
    endif
!     
!  Output point information
!     
    do ipts = 1,npts
      write(22,'(i8,1x,i6,1x,3(1x,f12.6),1x,f14.8)') &
        ipts,cosmoatomptr(ipts),spxyz(1,ipts),spxyz(2,ipts),spxyz(3,ipts), &
        (qonsas(ipts) - deltaq*segweight(ipts))
    enddo
!
!  Close file
!
    close(22)
  endif
!
  return
  end
