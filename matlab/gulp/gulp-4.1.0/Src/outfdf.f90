  subroutine outfdf(iout)
!
!  Write out an FDF (Flexible Data Format) file of Garcia
!  and Soler. Can be used to set up a SIESTA job.
!  At this stage the information passed is limited and
!  restricted to one configuration.
!
!   8/98 Created from outarc
!  10/02 Style updated
!   1/05 Memory deallocation order improved
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, January 2005
!
  use configurations
  use current
  use element
  use files
  use general
  use shell
  use species
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: iout
!
!  Local variables
!
  character(len=5)                             :: lab
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ispec
  integer(i4)                                  :: ni
  integer(i4)                                  :: nlspec
  integer(i4)                                  :: nti
  integer(i4), dimension(:), allocatable       :: natlspec
  integer(i4), dimension(:), allocatable       :: ntyplspec
  integer(i4)                                  :: status
!
!  Allocate local memory
!
  allocate(natlspec(nspec),stat=status)
  if (status/=0) call outofmemory('outfdf','natlspec')
  allocate(ntyplspec(nspec),stat=status)
  if (status/=0) call outofmemory('outfdf','ntyplspec')
!
!  Create local arrays of core-only species
!
  nlspec = 0
  do i = 1,nspec
    if (natspec(i).le.maxele) then
      nlspec = nlspec + 1
      natlspec(nlspec)=natspec(i)
      ntyplspec(nlspec)=ntypspec(i)
    endif
  enddo
!
!  If file name has been given then open file
!
  if (fdffile(1:1).ne.' ') then
    open(iout,file=fdffile,status='unknown')
  endif
!
!  Write out first title line and system name
!
  if (ntitle.gt.0) then
    write(iout,'(''SystemName '',a68)') titleword(1)(1:68)
  endif
  if (names(ncf)(1:1).ne.' ') then
    write(iout,'(''SystemLabel '',a20)') names(ncf)(1:20)
  endif
!
!  Number of atoms
!
  write(iout,'(''NumberOfAtoms '',i8,/)') numat-nshell
!
!  Species descriptors
!
  write(iout,'(''NumberOfSpecies  '',i5)') nlspec
  write(iout,'(''%block ChemicalSpeciesLabel'')')
  do i = 1,nlspec
    call label(natlspec(i),ntyplspec(i),lab)
    write(iout,'(i5,1x,i3,1x,a5)') i,natlspec(i),lab
  enddo
  write(iout,'(''%endblock ChemicalSpeciesLabel'',/)')
  if (ndim.eq.3) then
!*******************
!  Bulk structure  *
!*******************
!
!  Lattice constant - use a value of 1 and then cell
!  parameters will be in Angstroms as per usual
!
    write(iout,'(''LatticeConstant 1.0 Ang'')')
    call uncell3D(rv,a,b,c,alpha,beta,gamma)
    write(iout,'(''%block LatticeParameters'')')
    write(iout,'(6f10.4)')a,b,c,alpha,beta,gamma
    write(iout,'(''%endblock LatticeParameters'',/)')
!
!  Write out cores as fractional coordinates
!
    write(iout,'(''AtomicCoordinatesFormat Fractional'')')
    write(iout,'(''%block AtomicCoordinatesAndAtomicSpecies'')')
    do i=1,numat
      ni = nat(i)
      if (ni.le.maxele) then
        nti = nftype(i)
        ispec = 0
        ii = 0
        do while (ispec.eq.0.and.ii.lt.nlspec)
          ii = ii + 1
          if (ni.eq.natlspec(ii).and.(nti.eq.ntyplspec(ii).or.ntyplspec(ii).eq.0)) then
            ispec = ii
          endif
        enddo
        write(iout,'(3f15.9,1x,i5)')xfrac(i),yfrac(i),zfrac(i),ispec
      endif
    enddo
    write(iout,'(''%endblock AtomicCoordinatesAndAtomicSpecies'',/)')
  elseif (ndim.eq.2) then
    nwarn = nwarn + 1
    call outwarning('FDF cannot be output for surfaces yet',0_i4)
  elseif (ndim.eq.1) then
    nwarn = nwarn + 1
    call outwarning('FDF cannot be output for polymers yet',0_i4)
  elseif (ndim.eq.0) then
!*******************
!  Cluster output  *
!*******************
!
!  Write out cores as fractional coordinates
!
    write(iout,'(''AtomicCoordinatesFormat NotScaledCartesianAng'')')
    write(iout,'(''%block AtomicCoordinatesAndAtomicSpecies'')')
    do i = 1,numat
      ni = nat(i)
      if (ni.le.maxele) then
        nti = nftype(i)
        ispec = 0
        ii = 0
        do while (ispec.eq.0.and.ii.lt.nlspec)
          ii = ii + 1
          if (ni.eq.natlspec(ii).and.(nti.eq.ntyplspec(ii).or.ntyplspec(ii).eq.0)) then
            ispec = ii
          endif
        enddo
        write(iout,'(3f15.6,1x,i5)')xclat(i),yclat(i),zclat(i),ispec
      endif
    enddo
    write(iout,'(''%endblock AtomicCoordinatesAndAtomicSpecies'',/)')
  endif
!
!  Free local memory
!
  deallocate(ntyplspec,stat=status)
  if (status/=0) call deallocate_error('outfdf','ntyplspec')
  deallocate(natlspec,stat=status)
  if (status/=0) call deallocate_error('outfdf','natlspec')
!
  return
  end
