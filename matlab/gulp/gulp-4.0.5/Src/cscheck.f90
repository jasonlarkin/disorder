  subroutine cscheck
!
!  Subroutine for checking that each shell has a valid core
!
!   8/96 Correction added to core-shell occupancy sum checking
!        - requirement that type numbers must be the same has 
!          been added in addition to the atom numbers
!   1/09 Core-shell vector array added
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use current
  use element, only : maxele
  use iochannels
  use parallel
  use shell
  implicit none
!
  character(len=5)                             :: labi
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4), dimension(:), allocatable       :: isnoc
  integer(i4)                                  :: isp
  integer(i4)                                  :: j
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: nsnoc
  integer(i4)                                  :: status
  logical                                      :: lfound
  real(dp)                                     :: cut2
  real(dp)                                     :: occ
  real(dp)                                     :: ocs
  real(dp)                                     :: r
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
!  Initialise ncsptr & csvector
!
  ncsptr(1:numat) = 0
  csvector(1:3,1:numat) = 0.0_dp
!
!  If there are no shells give up now!
!
  if (nshell.eq.0) return
!
  nsnoc = 0
  allocate(isnoc(nshell),stat=status)
  if (status/=0) call outofmemory('cscheck','isnoc')
  cut2 = cuts*cuts
!
!  Loop over shells
!
  do i = 1,nshell
    isp = nshptr(i)
    xcd = xclat(isp)
    ycd = yclat(isp)
    zcd = zclat(isp)
    ni = nat(isp)
    ntypi = nftype(isp)
    ocs = occuf(isp)
    lfound = .false.
!
!  Loop over core sites
!
    j = 0
    do while (j.lt.numat.and..not.lfound)
      j = j + 1
      nj = nat(j)
      ntypj = nftype(j)
      if ((ni-nj).eq.maxele.and.ntypi.eq.ntypj) then
        xcdi = xclat(j) - xcd
        ycdi = yclat(j) - ycd
        zcdi = zclat(j) - zcd
!
!  Loop over unit cells
!
        ii = 0
        do while (ii.lt.iimax.and..not.lfound)
          ii = ii + 1
          xcrd = xcdi + xvec1cell(ii)
          ycrd = ycdi + yvec1cell(ii)
          zcrd = zcdi + zvec1cell(ii)
          r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
          if (r.le.cut2) then
            lfound = .true.
            occ = occuf(j)
            ncsptr(isp) = j
            ncsptr(j) = isp
!
!  Store vector between core and shell 
!
            csvector(1,isp) = xcrd
            csvector(2,isp) = ycrd
            csvector(3,isp) = zcrd
            csvector(1,j) = - xcrd
            csvector(2,j) = - ycrd
            csvector(3,j) = - zcrd
          endif
!
!  End of loop over cell vectors
!
        enddo
      endif
!
!  End loop over cores
!
    enddo
    if (.not.lfound) then
      nsnoc = nsnoc + 1
      isnoc(nsnoc) = isp
    elseif (ocs.ne.occ) then
      call outerror('core/shell pairs must have same occupancy',0_i4)
      call stopnow('cscheck')
    endif
!
!  End loop over shells
!
  enddo
!
!  If shells without cores have been found then list shells and stop
!
  if (nsnoc.gt.0) then
    if (ioproc) then
      call outerror('shell has been found with no matching core',0_i4)
      write(ioout,'(/,''  Configuration number = '',i4,/)') ncf
      write(ioout,'(''  List of unmatched shells for full cell : '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      if (ndim.eq.3) then
        write(ioout,'(''    Shell atom number           Label       Fractional coordinates'')')
      elseif (ndim.eq.2) then
        write(ioout,'(''    Shell atom number           Label       Frac/Cart  coordinates'')')
      elseif (ndim.eq.1) then
        write(ioout,'(''    Shell atom number           Label       Frac/Cart  coordinates'')')
      else
        write(ioout,'(''    Shell atom number           Label       Cartesian coordinates'')')
      endif
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nsnoc
        ii = isnoc(i)
        ni = nat(ii)
        nti = nftype(ii)
        call label(ni,nti,labi)
        write(ioout,'(10x,i4,17x,a5,3f12.6)') ii,labi,xfrac(ii),yfrac(ii),zfrac(ii)
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      call outfile
    endif
    call stopnow('cscheck')
  endif
  deallocate(isnoc,stat=status)
  if (status/=0) call deallocate_error('cscheck','isnoc')
!
  return
  end
