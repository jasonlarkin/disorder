  subroutine many12(emany,lgrad1,lgrad2,imode,lsymalg)
!
!  Subroutine for calculating defect many energy and up to second derivatives
!
!  imode = 1 => defective region 1 and region 2a
!  imode = 2 => perfect region 1 and region 2a
!
!  Note that if imode=2 then only energy is needed
!
!  lsymalg => if .true. then dscrhor2d values are stored for
!             asymmetric unit of r2a only
!
!  This approach to many body defects involves the assumption that
!  there are no changes in rho for atoms in region 2 and therefore
!  that the bulk values can be used.
!
!  On entry the array scrho must contain the density at each atomic site.
!
!   4/97 Created from real12a
!   4/97 Symmetry adaption added - only applies to imode=2
!   5/97 Exponential form of Sutton-Chen added
!   7/97 Adapted for density and functional options
!  10/99 Cubic density function added
!   7/00 Region 2a displacements now allowed for
!  11/03 ndennat/ndentyp replaced
!  11/03 Alloy scaling added
!   7/05 Constant scaling added to sqrt and power functionals
!   7/05 Use of EAM species pointers introduced
!   9/05 rhoderv called to get density derivatives
!   9/05 Call to eamfnderv used to replace EAM function derivative code
!  10/05 Call to eamfnderv added for energy
!   4/06 Modified to handle species specific densities
!   3/07 Printing of EAM densities and energies added as an option
!   5/07 Call to rhoderv modified by adding rpot
!  12/07 Unused variables removed
!  11/08 Call to rhoderv updated to include the density
!  11/08 Call to rhoderv modified to include x,y,z Cartesian components
!  11/08 rho arrays changed to 2-D to benefit MEAM
!  11/08 call to rhoderv replaced by calls to meamrho/eamrho according to MEAM vs EAM
!  12/08 rho switched back to 1-D array with condensed components
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!  12/08 MEAM calculation of density added as option
!   1/09 Total density now printed for MEAM case
!   1/09 Derivatives modified to accommodate MEAM : deriv -> deriv(3)
!   2/09 Derivatives for MEAM added
!   3/09 small replaced by global value smallself from general module
!   3/09 Sign of return arguments from EAM/MEAM function routines corrected for
!   4/09 Cross term MEAM derivatives added
!   5/09 MEAM third derivative arguments removed
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use control
  use current
  use defects
  use derivatives
  use eam
  use general,        only : smallself
  use iochannels,     only : ioout
  use sutton
  use region2a
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: imode
  logical                                      :: lgrad1
  logical                                      :: lgrad2
  logical                                      :: lsymalg
  real(dp)                                     :: emany
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: indi
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjj
  integer(i4)                                  :: jmin
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: kkk
  integer(i4)                                  :: m
  integer(i4)                                  :: maxlim
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: natk
  integer(i4)                                  :: neamspeci
  integer(i4)                                  :: neamspecj
  integer(i4)                                  :: neamspeck
  integer(i4)                                  :: neqvi
  integer(i4)                                  :: nloopi
  integer(i4)                                  :: noffset
  integer(i4)                                  :: npot
  integer(i4)                                  :: npotijk
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: npsi
  integer(i4)                                  :: npsj
  integer(i4)                                  :: npsk
  integer(i4)                                  :: nr1
  integer(i4)                                  :: nrelpi
  integer(i4)                                  :: ntot
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: status
  logical                                      :: lanyvalidik
  logical                                      :: lanyvalidjk
  logical                                      :: ldsl
  logical                                      :: lfound
  logical                                      :: lnodisp
  logical                                      :: lregion1j
  logical                                      :: lvalidij
  logical                                      :: lvalidik
  logical                                      :: lvalidjk
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2k
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2rk
  real(dp)                                     :: deriv(3)
  real(dp)                                     :: deriv2(6)
  real(dp)                                     :: drhoij(3,maxmeamcomponent)
  real(dp)                                     :: drhoijs(6,maxmeamcomponent)
  real(dp)                                     :: drhoij2(6,maxmeamcomponent)
  real(dp)                                     :: drhoij2s(21,maxmeamcomponent)
  real(dp)                                     :: drhoij2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhoik(3,maxmeamcomponent)
  real(dp)                                     :: drhoiks(6,maxmeamcomponent)
  real(dp)                                     :: drhoik2(6,maxmeamcomponent)
  real(dp)                                     :: drhoik2s(21,maxmeamcomponent)
  real(dp)                                     :: drhoik2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhoji(3,maxmeamcomponent)
  real(dp)                                     :: drhojis(6,maxmeamcomponent)
  real(dp)                                     :: drhoji2(6,maxmeamcomponent)
  real(dp)                                     :: drhoji2s(21,maxmeamcomponent)
  real(dp)                                     :: drhoji2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhojk(3,maxmeamcomponent)
  real(dp)                                     :: drhojks(6,maxmeamcomponent)
  real(dp)                                     :: drhojk2(6,maxmeamcomponent)
  real(dp)                                     :: drhojk2s(21,maxmeamcomponent)
  real(dp)                                     :: drhojk2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhoki(3,maxmeamcomponent)
  real(dp)                                     :: drhokis(6,maxmeamcomponent)
  real(dp)                                     :: drhoki2(6,maxmeamcomponent)
  real(dp)                                     :: drhoki2s(21,maxmeamcomponent)
  real(dp)                                     :: drhoki2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhokj(3,maxmeamcomponent)
  real(dp)                                     :: drhokjs(6,maxmeamcomponent)
  real(dp)                                     :: drhokj2(6,maxmeamcomponent)
  real(dp)                                     :: drhokj2s(21,maxmeamcomponent)
  real(dp)                                     :: drhokj2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhototij(3)
  real(dp)                                     :: drhototijs(6)
  real(dp)                                     :: drhototij2(6)
  real(dp)                                     :: drhototij2s(21)
  real(dp)                                     :: drhototij2m(6,3)
  real(dp)                                     :: drhototij3(10)
  real(dp)                                     :: drhototik(3)
  real(dp)                                     :: drhototiks(6)
  real(dp)                                     :: drhototik2(6)
  real(dp)                                     :: drhototik2s(21)
  real(dp)                                     :: drhototik2m(6,3)
  real(dp)                                     :: drhototik3(10)
  real(dp)                                     :: drhototji(3)
  real(dp)                                     :: drhototjis(6)
  real(dp)                                     :: drhototji2(6)
  real(dp)                                     :: drhototji2s(21)
  real(dp)                                     :: drhototji2m(6,3)
  real(dp)                                     :: drhototji3(10)
  real(dp)                                     :: drhototjk(3)
  real(dp)                                     :: drhototjks(6)
  real(dp)                                     :: drhototjk2(6)
  real(dp)                                     :: drhototjk2s(21)
  real(dp)                                     :: drhototjk2m(6,3)
  real(dp)                                     :: drhototjk3(10)
  real(dp)                                     :: drhototki(3)
  real(dp)                                     :: drhototkis(6)
  real(dp)                                     :: drhototki2(6)
  real(dp)                                     :: drhototki2s(21)
  real(dp)                                     :: drhototki2m(6,3)
  real(dp)                                     :: drhototki3(10)
  real(dp)                                     :: drhototkj(3)
  real(dp)                                     :: drhototkjs(6)
  real(dp)                                     :: drhototkj2(6)
  real(dp)                                     :: drhototkj2s(21)
  real(dp)                                     :: drhototkj2m(6,3)
  real(dp)                                     :: drhototkj3(10)
  real(dp)                                     :: drhototijk2(3,3)
  real(dp)                                     :: drhototjik2(3,3)
  real(dp)                                     :: drhototkij2(3,3)
  real(dp)                                     :: drhototkij2s(6,6)
  real(dp)                                     :: drhototkij2m(6,3)
  real(dp)                                     :: dt1
  real(dp)                                     :: dt2
  real(dp)                                     :: eeam
  real(dp)                                     :: emanytrm
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ofct
  real(dp)                                     :: ofctijk
  real(dp)                                     :: r
  real(dp)                                     :: r2
  real(dp)                                     :: rhoi
  real(dp)                                     :: rhoj
  real(dp)                                     :: rhok
  real(dp)                                     :: rhoij(maxmeamcomponent)
  real(dp)                                     :: rhoji(maxmeamcomponent)
  real(dp)                                     :: rhoik(maxmeamcomponent)
  real(dp)                                     :: rhoki(maxmeamcomponent)
  real(dp)                                     :: rhojk(maxmeamcomponent)
  real(dp)                                     :: rhokj(maxmeamcomponent)
  real(dp)                                     :: rik
  real(dp)                                     :: rik2
  real(dp)                                     :: rjk
  real(dp)                                     :: rjk2
  real(dp)                                     :: rp
  real(dp)                                     :: rpijk
  real(dp)                                     :: rscrhoi
  real(dp)                                     :: rscrhoi3
  real(dp)                                     :: rscrhoi5
  real(dp)                                     :: rscrhoj
  real(dp)                                     :: rscrhoj3
  real(dp)                                     :: rscrhoj5
  real(dp)                                     :: rscrhok
  real(dp)                                     :: rscrhok3
  real(dp)                                     :: rscrhok5
  real(dp)                                     :: scmax
  real(dp)                                     :: scrhoi(maxmeamcomponent)
  real(dp)                                     :: scrhoj(maxmeamcomponent)
  real(dp)                                     :: scrhok(maxmeamcomponent)
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcd1
  real(dp)                                     :: ycd1
  real(dp)                                     :: zcd1
  real(dp)                                     :: xcd2
  real(dp)                                     :: ycd2
  real(dp)                                     :: zcd2
!
  ldsl = (imode.eq.1.and.ld1sym.and.(.not.lgrad2.or.ld2sym))
  lnodisp = (index(keyword,'r234').eq.0)
!
  time1 = cputime()
!
!  Scale density
!
  call eamscaledscrho(1_i4)
!***********
!  Energy  *
!***********
  emany = 0.0_dp
  if (lPrintEAM) then
!
!  Openning banner for energy decomposition
!
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  EAM : Atom No. (Region 1)     Density                 Atom energy (eV) '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  if (imode.eq.1) then
!**************
!  Defective  *
!**************
!
!  Region 1 contribution
!
    nr1 = nreg1
    if (ldsl) then
      nloopi = ndasym
      do i = 1,nloopi
        ii = ndsptr(i)
        nati = natdefe(ii)
        ntypi = ntypdefe(ii)
        neamspeci = 0
        lfound = .false.
        do while (neamspeci.lt.neamfnspec.and..not.lfound)
          neamspeci = neamspeci + 1
          if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
            lfound = .true.
          endif
        enddo
        rhoi = dscrho(1,i)
        if (lfound.and.rhoi.gt.1.0d-12) then
          if (lMEAMfn) then
            call meamfnderv(neamfn,neamspeci,dscrho(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
          else
            call eamfnderv(neamfn,neamspeci,dscrho(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
            rhoi = dscrho(1,i)
          endif
          emanytrm = ndeqv(i)*occdefe(ii)*eeam
          emany = emany + emanytrm
          if (lPrintEAM) then
            write(ioout,'(7x,i8,4x,f24.10,4x,f24.12)') i,rhoi,emanytrm
          endif
        endif
      enddo
    else
      nloopi = nreg1
      do i = 1,nloopi
        nati = natdefe(i)
        ntypi = ntypdefe(i)
        neamspeci = 0
        lfound = .false.
        do while (neamspeci.lt.neamfnspec.and..not.lfound)
          neamspeci = neamspeci + 1
          if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
            lfound = .true.
          endif
        enddo
        rhoi = dscrho(1,i)
        if (lfound.and.rhoi.gt.1.0d-12) then
          if (lMEAMfn) then
            call meamfnderv(neamfn,neamspeci,dscrho(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
          else
            call eamfnderv(neamfn,neamspeci,dscrho(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
            rhoi = dscrho(1,i)
          endif
          emanytrm = occdefe(i)*eeam
          emany = emany + emanytrm
          if (lPrintEAM) then
            write(ioout,'(7x,i8,4x,f24.10,4x,f24.12)') i,rhoi,emanytrm
          endif
        endif
      enddo
    endif
!
!  Region 2 contribution
!
    if (lsymalg) then
      do i = 1,ndpasym2a
        npsi = nps(ndsptr2a(i))
        nati = nat(npsi)
        ntypi = nftype(npsi)
        neamspeci = 0
        lfound = .false.
        do while (neamspeci.lt.neamfnspec.and..not.lfound)
          neamspeci = neamspeci + 1
          if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
            lfound = .true.
          endif
        enddo
        rhoi = dscrhor2d(1,i)
        if (lfound.and.rhoi.gt.1.0d-12) then
          if (lMEAMfn) then
            call meamfnderv(neamfn,neamspeci,dscrhor2d(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
          else
            call eamfnderv(neamfn,neamspeci,dscrhor2d(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
            rhoi = dscrhor2d(1,i)
          endif
          emany = emany + dble(ndeqv2a(i))*or2a(ndsptr2a(i))*eeam
        endif
      enddo
    else
      do i = 1,npreg2
        npsi = nps(i)
        nati = nat(npsi)
        ntypi = nftype(npsi)
        neamspeci = 0
        lfound = .false.
        do while (neamspeci.lt.neamfnspec.and..not.lfound)
          neamspeci = neamspeci + 1
          if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
            lfound = .true.
          endif
        enddo
        rhoi = dscrhor2d(1,i)
        if (lfound.and.rhoi.gt.1.0d-12) then
          if (lMEAMfn) then
            call meamfnderv(neamfn,neamspeci,dscrhor2d(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
          else
            call eamfnderv(neamfn,neamspeci,dscrhor2d(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
            rhoi = dscrhor2d(1,i)
          endif
          emany = emany + or2a(i)*eeam
        endif
      enddo
    endif
  elseif (imode.eq.2) then
!************
!  Perfect  *
!************
!
!  Region 1 contribution
!
    nloopi = nreg1old
    nr1 = nreg1old
    do i = 1,nr1
      nrelpi = nrelat(npsite(i))
      nati = natp(i)
      ntypi = ntypep(i)
      neamspeci = 0
      lfound = .false.
      do while (neamspeci.lt.neamfnspec.and..not.lfound)
        neamspeci = neamspeci + 1
        if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
          lfound = .true.
        endif
      enddo
      rhoi = scrho(1,nrelpi)
      if (lfound.and.rhoi.gt.1.0d-12) then
        if (lMEAMfn) then
          call meamfnderv(neamfn,neamspeci,scrho(1,nrelpi),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
        else
          call eamfnderv(neamfn,neamspeci,scrho(1,nrelpi),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
          rhoi = scrho(1,nrelpi)
        endif
        emanytrm = occp(i)*eeam
        emany = emany + emanytrm
        if (lPrintEAM) then
          write(ioout,'(7x,i8,4x,f24.10,4x,f24.12)') i,rhoi,emanytrm
        endif
      endif
    enddo
!
!  Region 2 contribution
!
    do i = 1,npreg2
      npsi = nps(i)
      nati = nat(npsi)
      ntypi = nftype(npsi)
      neamspeci = 0
      lfound = .false.
      do while (neamspeci.lt.neamfnspec.and..not.lfound)
        neamspeci = neamspeci + 1
        if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
          lfound = .true.
        endif
      enddo
      rhoi = scrho(1,npsi)
      if (lfound.and.rhoi.gt.1.0d-12) then
        if (lMEAMfn) then
          call meamfnderv(neamfn,neamspeci,scrho(1,npsi),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
        else
          call eamfnderv(neamfn,neamspeci,scrho(1,npsi),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
          rhoi = scrho(1,npsi)
        endif
        emany = emany + or2a(i)*eeam
      endif
    enddo
  endif
  if (lPrintEAM) then
!     
!  Closing banner for energy decomposition
!
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  If no forces are needed then we don't need to do loops
!  over atoms, so we can just return
!
!  In principle, it should never be necessary to go beyond
!  here for imode  =  2
!
  if (.not.lgrad1) goto 1000
!
!  From here on we can assume that lgrad1  =  .true.
!
!  Local variables
!
  ntot = nr1 + npreg2
  noffset = 3*nr1
  if (ldbsm) noffset = noffset + nr1
!
!  Find maximum cut - off radius
!
  scmax = 0.0_dp
  do i = 1,npote
    if (nptype(i).eq.19) then
      if (rpot(i).gt.scmax) scmax = rpot(i)
    endif
  enddo
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('many12','npotl')
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  if (lnoreal) goto 999
!*********************************
!  Region 1  -  region 2 energy  *
!*********************************
!
!  Outer loop over sites
!
  iloop: do i = 1,nloopi
    indi = 3*(i - 1)
    ix = indi + 1
    iy = indi + 2
    iz = indi + 3
!
!  Inner loop over second site
!
    if (imode.eq.1) then
      if (ldsl) then
        ii = ndsptr(i)
        neqvi = ndeqv(i)
      else
        ii = i
        neqvi = 1
      endif
      xal = xdefe(ii)
      yal = ydefe(ii)
      zal = zdefe(ii)
      nati = natdefe(ii)
      ntypi = ntypdefe(ii)
      oci = occdefe(ii)*neqvi
      if (lMEAM) then
        scrhoi(1:maxmeamcomponent) = dscrho(1:maxmeamcomponent,i)
      else
        scrhoi(1) = dscrho(1,i)
      endif
    elseif (imode.eq.2) then
      xal = xperf(i)
      yal = yperf(i)
      zal = zperf(i)
      nati = natp(i)
      ntypi = ntypep(i)
      oci = occp(i)
      npsi = npsite(i)
      if (lMEAM) then
        scrhoi(1:maxmeamcomponent) = scrho(1:maxmeamcomponent,nrelat(i))
      else
        scrhoi(1) = scrho(1,nrelat(i))
      endif
    endif
!
    neamspeci = 0
    lfound = .false.
    do while (neamspeci.lt.neamfnspec.and..not.lfound)
      neamspeci = neamspeci + 1
      if (nati.eq.neamfnnat(neamspeci).and.(ntypi.eq.neamfntyp(neamspeci).or.neamfntyp(neamspeci).eq.0)) then
        lfound = .true.
      endif
    enddo
    if (lfound) then
!
!  Evaluate functional derivatives
!
      if (lMEAMfn) then
        call meamfnderv(neamfn,neamspeci,scrhoi,rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.true.,lgrad2,.false.)
      else
        call eamfnderv(neamfn,neamspeci,scrhoi(1),eeam,rscrhoi,rscrhoi3,rscrhoi5,.true.,lgrad2,.false.)
        rhoi = scrhoi(1)
      endif
    else
      rscrhoi = 0.0_dp
      if (lgrad2) rscrhoi3 = 0.0_dp
    endif
    if (ldsl) then
      jmin = 1
    else
      jmin = i + 1
    endif
!
!  Start of second atom loop  -  avoid double counting
!  when both ions are in region 1
!
    jloop: do j = jmin,ntot
      if (j.le.nr1) then
!
!  Region 1 ion
!
        lregion1j = .true.
        if (imode.eq.1) then
          natj = natdefe(j)
          ntypj = ntypdefe(j)
          xcd = xdefe(j) - xal
          ycd = ydefe(j) - yal
          zcd = zdefe(j) - zal
          ocj = occdefe(j)
          if (ldsl) then
            if (lMEAM) then
              scrhoj(1:maxmeamcomponent) = dscrho(1:maxmeamcomponent,ndrel(j))
            else
              scrhoj(1) = dscrho(1,ndrel(j))
            endif
          else
            if (lMEAM) then
              scrhoj(1:maxmeamcomponent) = dscrho(1:maxmeamcomponent,j)
            else
              scrhoj(1) = dscrho(1,j)
            endif
          endif
        else
          natj = natp(j)
          ntypj = ntypep(j)
          xcd = xperf(j) - xal
          ycd = yperf(j) - yal
          zcd = zperf(j) - zal
          ocj = occp(j)
          npsj = npsite(j)
          if (lMEAM) then
            scrhoj(1:maxmeamcomponent) = scrho(1:maxmeamcomponent,nrelat(npsj))
          else
            scrhoj(1) = scrho(1,nrelat(npsj))
          endif
        endif
        jx = 3*(j - 1) + 1
        jy = jx + 1
        jz = jx + 2
      else
!
!  Region 2 ion
!
        lregion1j = .false.
        jj = j - nr1
        natj = nr2a(jj)
        ntypj = ntr2a(jj)
        if (lnodisp) then
          xcd = xr2a(jj) - xal
          ycd = yr2a(jj) - yal
          zcd = zr2a(jj) - zal
        else
          xcd = xr2a(jj) + xdis(jj) - xal
          ycd = yr2a(jj) + ydis(jj) - yal
          zcd = zr2a(jj) + zdis(jj) - zal
        endif
        ocj = or2a(jj)
        if (imode.eq.1) then
          if (lsymalg) then
            jjj = ndrel2a(jj)
            if (lMEAM) then
              scrhoj(1:maxmeamcomponent) = dscrhor2d(1:maxmeamcomponent,jjj)
            else
              scrhoj(1) = dscrhor2d(1,jjj)
            endif
          else
            if (lMEAM) then
              scrhoj(1:maxmeamcomponent) = dscrhor2d(1:maxmeamcomponent,jj)
            else
              scrhoj(1) = dscrhor2d(1,jj)
            endif
          endif
        else
          npsj = nps(jj)
          if (lMEAM) then
            scrhoj(1:maxmeamcomponent) = scrho(1:maxmeamcomponent,nrelat(npsj))
          else
            scrhoj(1) = scrho(1,nrelat(npsj))
          endif
        endif
        jx = noffset + 1
        jy = noffset + 2
        jz = noffset + 3
      endif
!
      neamspecj = 0
      lfound = .false.
      do while (neamspecj.lt.neamfnspec.and..not.lfound)
        neamspecj = neamspecj + 1
        if (natj.eq.neamfnnat(neamspecj).and.(ntypj.eq.neamfntyp(neamspecj).or.neamfntyp(neamspecj).eq.0)) then
          lfound = .true.
        endif
      enddo
      if (lfound) then
!
!  Evaluate functional derivatives
!
        if (lMEAMfn) then
          call meamfnderv(neamfn,neamspecj,scrhoj,rhoj,eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,lgrad2,.false.)
        else
          call eamfnderv(neamfn,neamspecj,scrhoj(1),eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,lgrad2,.false.)
          rhoj = scrhoj(1)
        endif
      else
        rscrhoj = 0.0_dp
        if (lgrad2) rscrhoj3 = 0.0_dp
      endif
!
!  If no rho then skip
!
      if (rhoi.eq.0.0_dp.and.rhoj.eq.0.0_dp.and..not.lgrad2) cycle jloop
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
      if (nati.eq.natj) then
        nat1 = nati
        nat2 = natj
        if (ntypi.lt.ntypj) then
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
      elseif (nati.lt.natj) then
        nat1 = nati
        nat2 = natj
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        nat1 = natj
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
      ofct = oci*ocj
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      do n = 1,npote
        if (nptype(n).eq.19) then
          if (nat1.eq.nspec1(n).and.nat2.eq.nspec2(n)) then
            if ((ntyp1.eq.nptyp1(n).or.nptyp1(n).eq.0).and.(ntyp2.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
              npots = npots + 1
              npotl(npots) = n
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
!
!  If no valid potentials then there is no need
!  to continue with this pair, unless this is
!  a second derivative calculation, in which
!  case there may be a contribution from triangles
!  of interactions.
!
      if (npots.eq.0.and..not.lgrad2) cycle jloop
      if (lgrad2) then
!
!  Need to make cut - off equal to double the maximum
!  to ensure all triangles are included
!
        rp = 2.0_dp*scmax
        cut2r = rp*rp
        if (cut2r.gt.4.0_dp*cut2p) cut2r = cut2p
      else
        cut2r = rp*rp
        if (cut2r.gt.cut2p) cut2r = cut2p
      endif
      cut2 = cut2r
      r2 = xcd*xcd + ycd*ycd + zcd*zcd
      if (r2.gt.smallself.and.r2.le.cut2) then
!*****************************************************
!  Calculate many - body contribution in real space  *
!*****************************************************
        deriv(1:3) = 0.0_dp
        deriv2(1:6) = 0.0_dp
        r = sqrt(r2)
!*******************************************
!  Valid many - body potentials for i - j  *
!*******************************************
        if (lMEAM) then
          rhoij(1:maxmeamcomponent) = 0.0_dp
          rhoji(1:maxmeamcomponent) = 0.0_dp
          drhoij(1:3,1:maxmeamcomponent) = 0.0_dp
          drhoji(1:3,1:maxmeamcomponent) = 0.0_dp
          if (lgrad2) then
            drhoij2(1:6,1:maxmeamcomponent) = 0.0_dp
            drhoji2(1:6,1:maxmeamcomponent) = 0.0_dp
          endif
        else
          rhoij(1) = 0.0_dp
          rhoji(1) = 0.0_dp
        endif
        drhototij(1:3) = 0.0_dp
        drhototji(1:3) = 0.0_dp
        if (lgrad2) then
          drhototij2(1:6) = 0.0_dp
          drhototji2(1:6) = 0.0_dp
        endif
        lvalidij = .false.
        if (npots.gt.0) then
          do m = 1,npots
            npot = npotl(m)
            if (r.gt.rpot2(npot).and.r.le.rpot(npot).and.r.le.rpmax) then
              lvalidij = .true.
!
!  Calculate density derivatives
!
              if (lMEAMden) then
                call meamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcd,ycd,zcd,rhoij,rhoji,drhoij,drhoji, &
                             drhoijs,drhojis,drhoij2,drhoji2,drhoij2s,drhoji2s,drhoij2m,drhoji2m, &
                             1.0_dp,1.0_dp,.true.,.false.,.true.,lgrad2)
                call meamtotalrhoderv(neamspeci,scrhoi,rhoi,drhoij,drhototij,drhoijs,drhototijs, &
                                      drhoij2,drhototij2,drhoij2s,drhototij2s,drhoij2m,drhototij2m, &
                                      .false.,lgrad2)
                call meamtotalrhoderv(neamspecj,scrhoj,rhoj,drhoji,drhototji,drhojis,drhototjis, &
                                      drhoji2,drhototji2,drhoji2s,drhototji2s,drhoji2m,drhototji2m, &
                                      .false.,lgrad2)
              else
                call eamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcd,ycd,zcd,rhoij,rhoji,drhototij,drhototji, &
                            drhototijs,drhototjis,drhototij2,drhototji2,drhototij2s,drhototji2s, &
                            drhototij2m,drhototji2m,drhototij3,drhototji3,1.0_dp,1.0_dp,.false.,.true.,lgrad2,.false.)
              endif
            endif
          enddo
!
!  Combine derivative terms
!
          deriv(1:3) = deriv(1:3) + (rscrhoi*drhototij(1:3) + rscrhoj*drhototji(1:3))*ofct
          if (lgrad2) then
            deriv2(1:6) = deriv2(1:6) + (rscrhoi*drhototij2(1:6) + rscrhoj*drhototji2(1:6))*ofct
            deriv2(1) = deriv2(1) + ofct*(ocj*rscrhoi3*drhototij(1)*drhototij(1) + oci*rscrhoj3*drhototji(1)*drhototji(1))
            deriv2(2) = deriv2(2) + ofct*(ocj*rscrhoi3*drhototij(1)*drhototij(2) + oci*rscrhoj3*drhototji(1)*drhototji(2))
            deriv2(3) = deriv2(3) + ofct*(ocj*rscrhoi3*drhototij(1)*drhototij(3) + oci*rscrhoj3*drhototji(1)*drhototji(3))
            deriv2(4) = deriv2(4) + ofct*(ocj*rscrhoi3*drhototij(2)*drhototij(2) + oci*rscrhoj3*drhototji(2)*drhototji(2))
            deriv2(5) = deriv2(5) + ofct*(ocj*rscrhoi3*drhototij(2)*drhototij(3) + oci*rscrhoj3*drhototji(2)*drhototji(3))
            deriv2(6) = deriv2(6) + ofct*(ocj*rscrhoi3*drhototij(3)*drhototij(3) + oci*rscrhoj3*drhototji(3)*drhototji(3))
          endif
        endif
        if (lvalidij) then
!******************************
!  Internal first derivatives *
!******************************
          xdrv(i) = xdrv(i) - deriv(1)
          ydrv(i) = ydrv(i) - deriv(2)
          zdrv(i) = zdrv(i) - deriv(3)
          if (lregion1j.and..not.ldsl) then
            xdrv(j) = xdrv(j) + deriv(1)
            ydrv(j) = ydrv(j) + deriv(2)
            zdrv(j) = zdrv(j) + deriv(3)
          endif
!********************************
!  Internal second derivatives  *
!********************************
          if (lgrad2) then
!
!  Generate products for derivatives between i - j
!
            derv2(jx,ix) = derv2(jx,ix) - deriv2(1)
            derv2(jy,ix) = derv2(jy,ix) - deriv2(2)
            derv2(jz,ix) = derv2(jz,ix) - deriv2(3)
            derv2(jx,iy) = derv2(jx,iy) - deriv2(2)
            derv2(jy,iy) = derv2(jy,iy) - deriv2(4)
            derv2(jz,iy) = derv2(jz,iy) - deriv2(5)
            derv2(jx,iz) = derv2(jx,iz) - deriv2(3)
            derv2(jy,iz) = derv2(jy,iz) - deriv2(5)
            derv2(jz,iz) = derv2(jz,iz) - deriv2(6)
          endif
        endif
        if (lgrad2) then
!******************************************************************
!  Start of third atom loop  -  only needed for second derivatives  *
!******************************************************************
          kloop: do k = 1,ntot
            if (k.le.nr1) then
!
!  Region 1 ion
!
              if (imode.eq.1) then
                natk = natdefe(k)
                ntypk = ntypdefe(k)
                xcd1 = xdefe(k) - xal
                ycd1 = ydefe(k) - yal
                zcd1 = zdefe(k) - zal
                ock = occdefe(k)
                if (lMEAM) then
                  scrhok(1:maxmeamcomponent) = dscrho(1:maxmeamcomponent,k)
                else
                  scrhok(1) = dscrho(1,k)
                endif
              else
                natk = natp(k)
                ntypk = ntypep(k)
                xcd1 = xperf(k) - xal
                ycd1 = yperf(k) - yal
                zcd1 = zperf(k) - zal
                ock = occp(k)
                npsk = npsite(k)
                if (lMEAM) then
                  scrhok(1:maxmeamcomponent) = scrho(1:maxmeamcomponent,nrelat(npsk))
                else
                  scrhok(1) = scrho(1,nrelat(npsk))
                endif
              endif
            else
!
!  Region 2 ion
!
              kk = k - nr1
              natk = nr2a(kk)
              ntypk = ntr2a(kk)
              if (lnodisp) then
                xcd1 = xr2a(kk) - xal
                ycd1 = yr2a(kk) - yal
                zcd1 = zr2a(kk) - zal
              else
                xcd1 = xr2a(kk) + xdis(kk) - xal
                ycd1 = yr2a(kk) + ydis(kk) - yal
                zcd1 = zr2a(kk) + zdis(kk) - zal
              endif
              ock = or2a(kk)
              if (imode.eq.1) then
                if (lsymalg) then
                  kkk = ndrel2a(kk)
                  if (lMEAM) then
                    scrhok(1:maxmeamcomponent) = dscrhor2d(1:maxmeamcomponent,kkk)
                  else
                    scrhok(1) = dscrhor2d(1,kkk)
                  endif
                else
                  if (lMEAM) then
                    scrhok(1:maxmeamcomponent) = dscrhor2d(1:maxmeamcomponent,kk)
                  else
                    scrhok(1) = dscrhor2d(1,kk)
                  endif
                endif
              else
                npsk = nps(kk)
                if (lMEAM) then
                  scrhok(1:maxmeamcomponent) = scrho(1:maxmeamcomponent,nrelat(npsk))
                else
                  scrhok(1) = scrho(1,nrelat(npsk))
                endif
              endif
            endif
!
            xcd2 = xcd1 - xcd
            ycd2 = ycd1 - ycd
            zcd2 = zcd1 - zcd
            ofctijk = ofct*ock
!
!  Check whether there are any potentials between i - k or j - k
!
            npotijk = 0
            rpijk = 0.0_dp
            do n = 1,npote
              if (nptype(n).eq.19) then
                if (natk.eq.nspec1(n).and.(ntypk.eq.nptyp1(n).or.nptyp1(n).eq.0)) then
                  npotijk = npotijk + 1
                  if (rpot(n).gt.rpijk) rpijk = rpot(n)
                elseif (natk.eq.nspec2(n).and.(ntypk.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
                  npotijk = npotijk + 1
                  if (rpot(n).gt.rpijk) rpijk = rpot(n)
                endif
              endif
            enddo
!
!  If no valid potentials for i - k or j - k then skip
!
            if (npotijk.eq.0) cycle kloop
            cut2rk = rpijk*rpijk
            if (cut2rk.gt.cut2p) cut2rk = cut2p
            cut2k = cut2rk
            neamspeck = 0
            lfound = .false.
            do while (neamspeck.lt.neamfnspec.and..not.lfound)
              neamspeck = neamspeck + 1
              if (natk.eq.neamfnnat(neamspeck).and.(ntypk.eq.neamfntyp(neamspeck).or.neamfntyp(neamspeck).eq.0)) then
                lfound = .true.
              endif
            enddo
            if (lfound) then
!
!  Evaluate functional derivatives
!
              if (lMEAMfn) then
                call meamfnderv(neamfn,neamspeck,scrhok,rhok,eeam,rscrhok,rscrhok3,rscrhok5,.true.,lgrad2,.false.)
              else
                call eamfnderv(neamfn,neamspeck,scrhok(1),eeam,rscrhok,rscrhok3,rscrhok5,.true.,lgrad2,.false.)
                rhok = scrhok(1)
              endif
            else
              rscrhok = 0.0_dp
              if (lgrad2) rscrhok3 = 0.0_dp
            endif
!
!  If no rho on any of 3 sites then skip
!
            if (abs(rhoi+rhoj+rhok).eq.0.0_dp) cycle kloop
!
            rik2 = xcd1*xcd1 + ycd1*ycd1 + zcd1*zcd1
            rjk2 = xcd2*xcd2 + ycd2*ycd2 + zcd2*zcd2
!
!  Skip if i = k or j=k  -  special cases handled before loop!
!
            if (rik2.le.smallself) cycle kloop
            if (rjk2.le.smallself) cycle kloop
!************************************************************
!  Calculate triangular contribution to second derivatives  *
!************************************************************
            if (lMEAM) then
              rhoik(1:maxmeamcomponent) = 0.0_dp
              rhoki(1:maxmeamcomponent) = 0.0_dp
              rhojk(1:maxmeamcomponent) = 0.0_dp
              rhokj(1:maxmeamcomponent) = 0.0_dp
              drhoik(1:3,1:maxmeamcomponent) = 0.0_dp
              drhoki(1:3,1:maxmeamcomponent) = 0.0_dp
              drhojk(1:3,1:maxmeamcomponent) = 0.0_dp
              drhokj(1:3,1:maxmeamcomponent) = 0.0_dp
            else
              rhoik(1) = 0.0_dp
              rhoki(1) = 0.0_dp
              rhojk(1) = 0.0_dp
              rhokj(1) = 0.0_dp
            endif
            drhototik(1:3) = 0.0_dp
            drhototki(1:3) = 0.0_dp
            drhototjk(1:3) = 0.0_dp
            drhototkj(1:3) = 0.0_dp
            drhototijk2(1:3,1:3) = 0.0_dp
            drhototjik2(1:3,1:3) = 0.0_dp
            drhototkij2(1:3,1:3) = 0.0_dp
!
            lanyvalidik = .false.
            lanyvalidjk = .false.
            if (rik2.le.cut2k) then
!*********************
!  i-k contribution  *
!*********************
              rik = sqrt(rik2)
!
!  Loop over potentials to find many - body ones
!
              do m = 1,npote
                if (nptype(m).eq.19) then
                  lvalidik = .false.
                  if (rik.gt.rpot2(m).and.rik.le.rpot(m)) then
                    if (nati.eq.nspec1(m).and.natk.eq.nspec2(m)) then
                      if (ntypi.eq.nptyp1(m).or.nptyp1(m).eq.0) then
                        if (ntypk.eq.nptyp2(m).or.nptyp2(m).eq.0) lvalidik = .true.
                      endif
                    elseif (nati.eq.nspec2(m).and.natk.eq.nspec1(m)) then
                      if (ntypi.eq.nptyp2(m).or.nptyp2(m).eq.0) then
                        if (ntypk.eq.nptyp1(m).or.nptyp1(m).eq.0) lvalidik = .true.
                      endif
                    endif
                    if (lvalidik) then
!
!  Calculate density derivatives
!
                      if (lMEAMden) then
                        call meamrho(nati,ntypi,natk,ntypk,rik,rpot(m),xcd1,ycd1,zcd1,rhoik,rhoki,drhoik,drhoki, &
                                     drhoiks,drhokis,drhoik2,drhoki2,drhoik2s,drhoki2s,drhoik2m,drhoki2m, &
                                     1.0_dp,1.0_dp,.true.,.false.,.true.,.false.)
                        call meamtotalrhoderv(neamspeci,scrhoi,rhoi,drhoik,drhototik,drhoiks,drhototiks, &
                                              drhoik2,drhototik2,drhoik2s,drhototik2s,drhoik2m,drhototik2m, &
                                              .false.,.false.)
                        call meamtotalrhoderv(neamspeck,scrhok,rhok,drhoki,drhototki,drhokis,drhototkis, &
                                              drhoki2,drhototki2,drhoki2s,drhototki2s,drhoki2m,drhototki2m, &
                                              .false.,.false.)
                      else
                        call eamrho(nati,ntypi,natk,ntypk,rik,rpot(m),xcd1,ycd1,zcd1,rhoik,rhoki,drhototik,drhototki, &
                                    drhototiks,drhototkis,drhototik2,drhototki2,drhototik2s,drhototki2s, &
                                    drhototik2m,drhototki2m,drhototik3,drhototki3,1.0_dp,1.0_dp,.false.,.true.,.false.,.false.)
                      endif
                      lanyvalidik = .true.
                    endif
                  endif
                endif
              enddo
            endif
            if (rjk2.le.cut2) then
!*********************
!  j-k contribution  *
!*********************
              rjk = sqrt(rjk2)
!
!  Loop over potentials to find many - body ones
!
              do m = 1,npote
                if (nptype(m).eq.19) then
                  lvalidjk = .false.
                  if (rjk.gt.rpot2(m).and.rjk.le.rpot(m)) then
                    if (natj.eq.nspec1(m).and.natk.eq.nspec2(m)) then
                      if (ntypj.eq.nptyp1(m).or.nptyp1(m).eq.0) then
                        if (ntypk.eq.nptyp2(m).or.nptyp2(m).eq.0) lvalidjk = .true.
                      endif
                    elseif (natj.eq.nspec2(m).and.natk.eq.nspec1(m)) then
                      if (ntypj.eq.nptyp2(m).or.nptyp2(m).eq.0) then
                        if (ntypk.eq.nptyp1(m).or.nptyp1(m).eq.0) lvalidjk = .true.
                      endif
                    endif
                    if (lvalidjk) then
!
!  Calculate density derivatives
!
                      if (lMEAMden) then
                        call meamrho(natj,ntypj,natk,ntypk,rjk,rpot(m),xcd2,ycd2,zcd2,rhojk,rhokj,drhojk,drhokj, &
                                     drhojks,drhokjs,drhojk2,drhokj2,drhojk2s,drhokj2s,drhojk2m,drhokj2m, &
                                     1.0_dp,1.0_dp,.true.,.false.,.true.,.false.)
                        call meamtotalrhoderv(neamspecj,scrhoj,rhoj,drhojk,drhototjk,drhojks,drhototjks, &
                                              drhojk2,drhototjk2,drhojk2s,drhototjk2s,drhojk2m,drhototjk2m, &
                                              .false.,.false.)
                        call meamtotalrhoderv(neamspeck,scrhok,rhok,drhokj,drhototkj,drhokjs,drhototkjs, &
                                              drhokj2,drhototkj2,drhokj2s,drhototkj2s,drhokj2m,drhototkj2m, &
                                              .false.,.false.)
                      else
                        call eamrho(natj,ntypj,natk,ntypk,rjk,rpot(m),xcd2,ycd2,zcd2,rhojk,rhokj,drhototjk,drhototkj, &
                                    drhototjks,drhototkjs,drhototjk2,drhototkj2,drhototjk2s,drhototkj2s, &
                                    drhototjk2m,drhototkj2m,drhototjk3,drhototkj3,1.0_dp,1.0_dp,.false.,.true.,.false.,.false.)
                      endif
                      lanyvalidjk = .true.
                    endif
                  endif
                endif
              enddo
            endif
!
!  Cross term derivative for k-i / k-j
!
            if (lanyvalidik.and.lanyvalidjk.and.lMEAMden) then
              call meamtotalrhocrossderv(neamspeck,scrhok,rhok,drhoki,drhototki,drhokj,drhototkj,drhokis,drhokjs, &
                                         drhototkij2,drhototkij2s,drhototkij2m,.false.)
            endif
!***************************************************
!  Calculate second derivatives for i - k/j - k terms  *
!***************************************************
!
!  i - k
!
            if (lMEAM) then
              dt1 = rscrhoi3*ofctijk
              dt2 = rscrhoi*ofctijk
              derv2(jx,ix) = derv2(jx,ix) - dt1*drhototik(1)*drhototij(1) - dt2*drhototijk2(1,1)
              derv2(jy,ix) = derv2(jy,ix) - dt1*drhototik(1)*drhototij(2) - dt2*drhototijk2(2,1)
              derv2(jz,ix) = derv2(jz,ix) - dt1*drhototik(1)*drhototij(3) - dt2*drhototijk2(3,1)
              derv2(jx,iy) = derv2(jx,iy) - dt1*drhototik(2)*drhototij(1) - dt2*drhototijk2(1,2)
              derv2(jy,iy) = derv2(jy,iy) - dt1*drhototik(2)*drhototij(2) - dt2*drhototijk2(2,2)
              derv2(jz,iy) = derv2(jz,iy) - dt1*drhototik(2)*drhototij(3) - dt2*drhototijk2(3,2)
              derv2(jx,iz) = derv2(jx,iz) - dt1*drhototik(3)*drhototij(1) - dt2*drhototijk2(1,3)
              derv2(jy,iz) = derv2(jy,iz) - dt1*drhototik(3)*drhototij(2) - dt2*drhototijk2(2,3)
              derv2(jz,iz) = derv2(jz,iz) - dt1*drhototik(3)*drhototij(3) - dt2*drhototijk2(3,3)
            else
              dt1 = rscrhoi3*ofctijk
              derv2(jx,ix) = derv2(jx,ix) - dt1*drhototik(1)*drhototij(1)
              derv2(jy,ix) = derv2(jy,ix) - dt1*drhototik(1)*drhototij(2)
              derv2(jz,ix) = derv2(jz,ix) - dt1*drhototik(1)*drhototij(3)
              derv2(jx,iy) = derv2(jx,iy) - dt1*drhototik(2)*drhototij(1)
              derv2(jy,iy) = derv2(jy,iy) - dt1*drhototik(2)*drhototij(2)
              derv2(jz,iy) = derv2(jz,iy) - dt1*drhototik(2)*drhototij(3)
              derv2(jx,iz) = derv2(jx,iz) - dt1*drhototik(3)*drhototij(1)
              derv2(jy,iz) = derv2(jy,iz) - dt1*drhototik(3)*drhototij(2)
              derv2(jz,iz) = derv2(jz,iz) - dt1*drhototik(3)*drhototij(3)
            endif
!
!  j - k
!
            if (lMEAM) then
              dt1 = rscrhoj3*ofctijk
              dt2 = rscrhoj*ofctijk
              derv2(jx,ix) = derv2(jx,ix) + dt1*drhototjk(1)*drhototji(1) + dt2*drhototjik2(1,1)
              derv2(jy,ix) = derv2(jy,ix) + dt1*drhototjk(2)*drhototji(1) + dt2*drhototjik2(1,2)
              derv2(jz,ix) = derv2(jz,ix) + dt1*drhototjk(3)*drhototji(1) + dt2*drhototjik2(1,3)
              derv2(jx,iy) = derv2(jx,iy) + dt1*drhototjk(1)*drhototji(2) + dt2*drhototjik2(2,1)
              derv2(jy,iy) = derv2(jy,iy) + dt1*drhototjk(2)*drhototji(2) + dt2*drhototjik2(2,2)
              derv2(jz,iy) = derv2(jz,iy) + dt1*drhototjk(3)*drhototji(2) + dt2*drhototjik2(2,3)
              derv2(jx,iz) = derv2(jx,iz) + dt1*drhototjk(1)*drhototji(3) + dt2*drhototjik2(3,1)
              derv2(jy,iz) = derv2(jy,iz) + dt1*drhototjk(2)*drhototji(3) + dt2*drhototjik2(3,2)
              derv2(jz,iz) = derv2(jz,iz) + dt1*drhototjk(3)*drhototji(3) + dt2*drhototjik2(3,3)
            else
              dt1 = rscrhoj3*ofctijk
              derv2(jx,ix) = derv2(jx,ix) + dt1*drhototjk(1)*drhototji(1)
              derv2(jy,ix) = derv2(jy,ix) + dt1*drhototjk(2)*drhototji(1)
              derv2(jz,ix) = derv2(jz,ix) + dt1*drhototjk(3)*drhototji(1)
              derv2(jx,iy) = derv2(jx,iy) + dt1*drhototjk(1)*drhototji(2)
              derv2(jy,iy) = derv2(jy,iy) + dt1*drhototjk(2)*drhototji(2)
              derv2(jz,iy) = derv2(jz,iy) + dt1*drhototjk(3)*drhototji(2)
              derv2(jx,iz) = derv2(jx,iz) + dt1*drhototjk(1)*drhototji(3)
              derv2(jy,iz) = derv2(jy,iz) + dt1*drhototjk(2)*drhototji(3)
              derv2(jz,iz) = derv2(jz,iz) + dt1*drhototjk(3)*drhototji(3)
            endif
!
!  i - k/j - k
!
            if (lMEAM) then
              dt1 = rscrhok3*ofctijk
              dt2 = rscrhok*ofctijk
              derv2(jx,ix) = derv2(jx,ix) + dt1*drhototki(1)*drhototkj(1) + dt2*drhototkij2(1,1)
              derv2(jy,ix) = derv2(jy,ix) + dt1*drhototki(1)*drhototkj(2) + dt2*drhototkij2(1,2)
              derv2(jz,ix) = derv2(jz,ix) + dt1*drhototki(1)*drhototkj(3) + dt2*drhototkij2(1,3)
              derv2(jx,iy) = derv2(jx,iy) + dt1*drhototki(2)*drhototkj(1) + dt2*drhototkij2(2,1)
              derv2(jy,iy) = derv2(jy,iy) + dt1*drhototki(2)*drhototkj(2) + dt2*drhototkij2(2,2)
              derv2(jz,iy) = derv2(jz,iy) + dt1*drhototki(2)*drhototkj(3) + dt2*drhototkij2(2,3)
              derv2(jx,iz) = derv2(jx,iz) + dt1*drhototki(3)*drhototkj(1) + dt2*drhototkij2(3,1)
              derv2(jy,iz) = derv2(jy,iz) + dt1*drhototki(3)*drhototkj(2) + dt2*drhototkij2(3,2)
              derv2(jz,iz) = derv2(jz,iz) + dt1*drhototki(3)*drhototkj(3) + dt2*drhototkij2(3,3)
            else
              dt1 = rscrhok3*ofctijk
              derv2(jx,ix) = derv2(jx,ix) + dt1*drhototki(1)*drhototkj(1)
              derv2(jy,ix) = derv2(jy,ix) + dt1*drhototki(1)*drhototkj(2)
              derv2(jz,ix) = derv2(jz,ix) + dt1*drhototki(1)*drhototkj(3)
              derv2(jx,iy) = derv2(jx,iy) + dt1*drhototki(2)*drhototkj(1)
              derv2(jy,iy) = derv2(jy,iy) + dt1*drhototki(2)*drhototkj(2)
              derv2(jz,iy) = derv2(jz,iy) + dt1*drhototki(2)*drhototkj(3)
              derv2(jx,iz) = derv2(jx,iz) + dt1*drhototki(3)*drhototkj(1)
              derv2(jy,iz) = derv2(jy,iz) + dt1*drhototki(3)*drhototkj(2)
              derv2(jz,iz) = derv2(jz,iz) + dt1*drhototki(3)*drhototkj(3)
            endif
!******************************
!  End of second derivatives  *
!******************************
!***************************
!  End of third atom loop  *
!***************************
          enddo kloop
        endif
!**************************************
!  End of valid distance i-j section  *
!**************************************
      endif
    enddo jloop
  enddo iloop
!
!  End of real space part
!
!****************************************
!  Symmetrise second derivative matrix  *
!****************************************
999 if (lgrad2.and..not.ldsl) then
    maxlim = 3*(nr1 + 1)
    if (ldbsm) maxlim = maxlim + nr1
    do i = 2,maxlim
      do j = 1,i-1
        derv2(j,i) = derv2(i,j)
      enddo
    enddo
  endif
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('many12','npotl')
!
!  Exit point
!
1000 continue
!
!  Unscale density
!
  call eamscaledscrho(-1_i4)
!
!  Timing
!
  time2 = cputime()
  tregm = tregm + time2 - time1
!
  return
  end
