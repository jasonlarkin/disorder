  subroutine realcmm(eatom,ereal,ecmm,eqeq,lgrad1)
!
!  Subroutine for calculating the real space energy of a cluster
!  using the cell multipole method. Designed for large clusters.
!
!  Current algorithm is simplified version of CMM to allow for
!  generality of the program. Each box length is set equal to 
!  the short range potential cutoff so that only adjacent boxes
!  need explcit calculation. The outer box loop is the one for
!  which the multipoles are calculated as this avoids the 
!  storage of the multipoles for all boxes concurrently, particularly
!  as the multipoles for the dispersion terms are species specific.
!
!  Second derivatives are excluded as this requires the specific
!  calculation between particular atoms and is thus contrary to
!  the ethos of the cell multipole method.
!
!  icmm    = CMM multipole level
!          = 1 => multipole
!          = 2 => dipole
!          = 3 => quadrupole
!          = 4 => octopole
!
!  Although correction terms for QEq are available in this routine
!  the derivatives will not be completely correct as the potential
!  in eem.f cannot yet be calculated using the CMM method and so
!  there is an inconsistency depending on the accuracy of CMM.
!
!   2/97 BSM exponential potential added
!   4/97 Sutton-Chen potential modifications added
!  12/97 Modification of energy/derivatives made purely local
!   1/98 QEq modifications added
!   1/99 1-4 interaction scaling added
!   3/99 Parallelisation introduced by simple distribution over
!        outer box loop. Not efficient for large numbers of
!        processors, so a convolution over inner loop will also
!        be necessary.
!   9/01 lmolq calculations accelerated using lneedmol 
!   2/02 lneedmol algorithm corrected
!  10/02 ReaxFF modifications added
!  11/02 Wildcard atoms added
!  11/02 Parallel changes made
!   9/04 Charge first derivatives added
!   4/05 Mods for cosh-spring added
!   7/05 Streitz and Mintmire modifications added
!   2/07 Bonding types added
!   3/07 Printing of twobody energies added as an option
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  11/07 Unused variables cleaned up
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   4/09 MEAM density modifications removed since these are now handled in 
!        a separate routine.
!   6/09 Site energy added
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
!   4/12 xvir, yvir and zvir removed
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
!  Julian Gale, NRI, Curtin University, April 2012
!
  use cellmultipole
  use configurations, only : lbsmat, nregionno
  use constants
  use control
  use current
  use derivatives
  use eam,            only : lMEAMden
  use element
  use energies,       only : siteenergy, eregion2region
  use general,        only : cutw
  use iochannels,     only : ioout
  use molecule
  use optimisation
  use parallel
  use shell
  use sutton
  use times
  use two
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lgrad1
  real(dp),    intent(inout)                   :: eatom
  real(dp),    intent(inout)                   :: ecmm
  real(dp),    intent(inout)                   :: eqeq
  real(dp),    intent(inout)                   :: ereal
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: iar
  integer(i4)                                  :: ii
  integer(i4)                                  :: ilower
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4), dimension(:), allocatable       :: iptri
  integer(i4), dimension(:), allocatable       :: iptrj
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jupper
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: m
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbati
  integer(i4)                                  :: nbatj
  integer(i4)                                  :: nbi
  integer(i4)                                  :: nbj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nix
  integer(i4)                                  :: niy
  integer(i4)                                  :: niz
  integer(i4)                                  :: njx
  integer(i4)                                  :: njy
  integer(i4)                                  :: njz
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lbreathe
  logical                                      :: lcspair
  logical                                      :: ldofull
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lorder12
  logical                                      :: lptrmol
  real(dp)                                     :: apt
  real(dp)                                     :: bpt
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d2i2
  real(dp)                                     :: d2ij
  real(dp)                                     :: d2j2
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: dtrm
  real(dp)                                     :: dx
  real(dp)                                     :: dy
  real(dp)                                     :: dz
  real(dp)                                     :: eatom_before
  real(dp)                                     :: ec6
  real(dp)                                     :: ec6_before
  real(dp)                                     :: eqeq_before
  real(dp)                                     :: ereal_before
  real(dp)                                     :: esum
  real(dp)                                     :: etrm1
  real(dp)                                     :: etrm2
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: foctopole(10)
  real(dp)                                     :: fquadrupole(6)
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qdipolex
  real(dp)                                     :: qdipoley
  real(dp)                                     :: qdipolez
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: qmonopole
  real(dp)                                     :: qoctopole(10)
  real(dp)                                     :: qquadrupole(6)
  real(dp)                                     :: r
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiff
  real(dp)                                     :: rp
  real(dp)                                     :: rrx
  real(dp)                                     :: rrx2
  real(dp)                                     :: rrx3
  real(dp)                                     :: rrx5
  real(dp)                                     :: rtrm
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: rx2
  real(dp)                                     :: rx
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp)                                     :: small2
  real(dp),    dimension(:), allocatable       :: sum
  real(dp),    dimension(:), allocatable       :: sum2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tsum0
  real(dp)                                     :: tsuml
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
  real(dp)                                     :: xj
  real(dp)                                     :: yj
  real(dp)                                     :: zj
  real(dp)                                     :: xmid
  real(dp)                                     :: ymid
  real(dp)                                     :: zmid
!
  time1 = cputime()
!
!  Initialise local variables
!
  small2 = 1.0d-12
  ec6 = 0.0_dp
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realcmm','npotl')
  allocate(iptri(numat),stat=status)
  if (status/=0) call outofmemory('realcmm','iptri')
  allocate(iptrj(numat),stat=status)
  if (status/=0) call outofmemory('realcmm','iptrj')
  allocate(sum(numat),stat=status)
  if (status/=0) call outofmemory('realcmm','sum')
  allocate(sum2(numat),stat=status)
  if (status/=0) call outofmemory('realcmm','sum2')
!
!  Set up cutoffs
!
  if (lwolf) then
    cut2e = cutw*cutw
  else
    cut2e = 1.0d10
  endif
  cut2p = cutp*cutp
  cut2s = cuts*cuts
  if (lnoreal) goto 999
!
!  Openning banner for energy decomposition
!
  if (lPrintTwo) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Two : Atom No. 1  Atom No. 2    Short-range energy (eV)   Coulomb energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!**************************
!  Outer loop over boxes  *
!**************************
!
!  Distribute loop over processors
!
  nbiloop: do nbi = procid+1,nboxcmm,nprocs
!
!  Find no. of atoms in box
!
    nbati = 0
    do i = 1,numat
      if (nboxat(i).eq.nbi) then
        nbati = nbati + 1
        iptri(nbati) = i
      endif
    enddo
!
!  No atoms so skip box
!
    if (nbati.eq.0) cycle nbiloop
!
!  Generate x,y,z indices
!
    call indtoijk(nbi,nix,niy,niz,nboxx,nboxy)
!*******************************
!  Generate multipole moments  *
!*******************************
!
!  Calculate centre of box about which to take moments
!
    rx = dble(nix) - 0.5_dp
    xmid = 0.0_dp
    ymid = 0.0_dp
    zmid = 0.0_dp
    do i = 1,nbati
      ii = iptri(i)
      xmid = xclat(ii) + xmid
      ymid = yclat(ii) + ymid
      zmid = zclat(ii) + zmid
    enddo
    if (nbati.gt.0) then
      xmid = xmid/dble(nbati)
      ymid = ymid/dble(nbati)
      zmid = zmid/dble(nbati)
    endif
!
!  Zero moments - quadupole moments store in linear
!  form:
!
!  1 = xx, 2=xy, 3=xz, 4=yy, 5=yz, 6=zz
!
!  and octopole moments in linear form:
!
!  1 = xxx, 2=xxy, 3=xxz, 4=xyy,  5=xyz
!  6 = zzx, 7=zzy, 8=zzz, 9=yyy, 10=yyz
!
!  Moments beginning with "q" are of the energy,
!  while those starting with "f" 
!
    qmonopole = 0.0_dp
    if (icmm.gt.1) then
      qdipolex = 0.0_dp
      qdipoley = 0.0_dp
      qdipolez = 0.0_dp
      if (icmm.gt.2) then
        do k = 1,6
          qquadrupole(k) = 0.0_dp
        enddo
        if (icmm.gt.3) then
          do k = 1,10
            qoctopole(k) = 0.0_dp
          enddo
        endif
      endif
    endif
    if (lgrad1) then
!
!  Don't need separate storage for the following
!  force terms as they are trivially related to
!  the energy terms (the last factor of two is
!  to compensate for the factor of half for double
!  counting in the energy which doesn't apply to
!  the forces):
!
!  fmonopole=-qmonopole*2.0
!  fdipole=-2.0*qdipole*2.0
!
      if (icmm.gt.2) then
        do k = 1,6
          fquadrupole(k) = 0.0_dp
        enddo
        if (icmm.gt.3) then
          do k = 1,10
            foctopole(k) = 0.0_dp
          enddo
        endif
      endif
      do i = 1,nbati
        ii = iptri(i)
        xi = xclat(ii)
        yi = yclat(ii)
        zi = zclat(ii)
        dx = xi - xmid
        dy = yi - ymid
        dz = zi - zmid
        rx2 = dx*dx + dy*dy + dz*dz
        qli = qf(ii)*occuf(ii)
!
!  Charge terms
!
        qmonopole = qmonopole + qli
        if (icmm.gt.1) then
          qdipolex = qdipolex + qli*dx
          qdipoley = qdipoley + qli*dy
          qdipolez = qdipolez + qli*dz
          if (icmm.gt.2) then
            rtrm = 3.0_dp*qli
            rtrm1 = rtrm*dx
            rx2 = qli*rx2
            qquadrupole(1) = qquadrupole(1) + rtrm1*dx - rx2
            qquadrupole(2) = qquadrupole(2) + rtrm1*dy
            qquadrupole(3) = qquadrupole(3) + rtrm1*dz
            rtrm1 = rtrm*dy
            qquadrupole(4) = qquadrupole(4) + rtrm1*dy - rx2
            qquadrupole(5) = qquadrupole(5) + rtrm1*dz
            rtrm1 = rtrm*dz
            qquadrupole(6) = qquadrupole(6) + rtrm1*dz - rx2
            rtrm = 4.0_dp*qli
            rtrm1 = rtrm*dx
            fquadrupole(1) = fquadrupole(1) + rtrm1*dx - rx2
            fquadrupole(2) = fquadrupole(2) + rtrm1*dy
            fquadrupole(3) = fquadrupole(3) + rtrm1*dz
            rtrm1 = rtrm*dy
            fquadrupole(4) = fquadrupole(4) + rtrm1*dy - rx2
            fquadrupole(5) = fquadrupole(5) + rtrm1*dz
            rtrm1 = rtrm*dz
            fquadrupole(6) = fquadrupole(6) + rtrm1*dz - rx2
            if (icmm.gt.3) then
              rtrm = 5.0_dp*dx*dx*qli
              qoctopole(1) = qoctopole(1) + rtrm*dx - 3.0*dx*rx2
              qoctopole(2) = qoctopole(2) + rtrm*dy - dy*rx2
              qoctopole(3) = qoctopole(3) + rtrm*dz - dz*rx2
              rtrm = 5.0_dp*dx*dy
              qoctopole(4) = qoctopole(4) + rtrm*dy - dx*rx2
              qoctopole(5) = qoctopole(5) + rtrm*dz
              rtrm = 5.0_dp*dz*dz
              qoctopole(6) = qoctopole(6) + rtrm*dx - dx*rx2
              qoctopole(7) = qoctopole(7) + rtrm*dy - dy*rx2
              qoctopole(8) = qoctopole(8) + rtrm*dz - 3.0*dz*rx2
              rtrm = 5.0_dp*dy*dy
              qoctopole(9) = qoctopole(9) + rtrm*dy - 3.0*dy*rx2
              qoctopole(10) = qoctopole(10) + rtrm*dz - dz*rx2
!
              rtrm = 6.0_dp*dx*dx*qli
              foctopole(1) = foctopole(1) + rtrm*dx - 3.0*dx*rx2
              foctopole(2) = foctopole(2) + rtrm*dy - dy*rx2
              foctopole(3) = foctopole(3) + rtrm*dz - dz*rx2
              rtrm = 6.0_dp*dx*dy
              foctopole(4) = foctopole(4) + rtrm*dy - dx*rx2
              foctopole(5) = foctopole(5) + rtrm*dz
              rtrm = 6.0_dp*dz*dz
              foctopole(6) = foctopole(6) + rtrm*dx - dx*rx2
              foctopole(7) = foctopole(7) + rtrm*dy - dy*rx2
              foctopole(8) = foctopole(8) + rtrm*dz - 3.0*dz*rx2
              rtrm = 6.0_dp*dy*dy
              foctopole(9) = foctopole(9) + rtrm*dy - 3.0*dy*rx2
              foctopole(10) = foctopole(10) + rtrm*dz - dz*rx2
            endif
          endif
        endif
      enddo
      if (icmm.gt.2) then
        do k = 1,6
          fquadrupole(k) = fquadrupole(k)*angstoev
        enddo
        if (icmm.gt.3) then
          do k = 1,10
            foctopole(k) = 4.0_dp*foctopole(k)*angstoev/3.0_dp
          enddo
        endif
      endif
    else
      do i = 1,nbati
        ii = iptri(i)
        xi = xclat(ii)
        yi = yclat(ii)
        zi = zclat(ii)
        dx = xi - xmid
        dy = yi - ymid
        dz = zi - zmid
        rx2 = dx*dx + dy*dy + dz*dz
        qli = qf(ii)*occuf(ii)
!
!  Charge terms
!
        qmonopole = qmonopole + qli
        if (icmm.gt.1) then
          qdipolex = qdipolex + qli*dx
          qdipoley = qdipoley + qli*dy
          qdipolez = qdipolez + qli*dz
          if (icmm.gt.2) then
            rtrm = 3.0_dp*qli
            rtrm1 = rtrm*dx
            rx2 = qli*rx2
            qquadrupole(1) = qquadrupole(1) + rtrm1*dx - rx2
            qquadrupole(2) = qquadrupole(2) + rtrm1*dy
            qquadrupole(3) = qquadrupole(3) + rtrm1*dz
            rtrm1 = rtrm*dy
            qquadrupole(4) = qquadrupole(4) + rtrm1*dy - rx2
            qquadrupole(5) = qquadrupole(5) + rtrm1*dz
            rtrm1 = rtrm*dz
            qquadrupole(6) = qquadrupole(6) + rtrm1*dz - rx2
            if (icmm.gt.3) then
              rtrm = 5.0_dp*dx*dx*qli
              qoctopole(1) = qoctopole(1) + rtrm*dx - 3.0*dx*rx2
              qoctopole(2) = qoctopole(2) + rtrm*dy - dy*rx2
              qoctopole(3) = qoctopole(3) + rtrm*dz - dz*rx2
              rtrm = 5.0_dp*dx*dy
              qoctopole(4) = qoctopole(4) + rtrm*dy - dx*rx2
              qoctopole(5) = qoctopole(5) + rtrm*dz
              rtrm = 5.0_dp*dz*dz
              qoctopole(6) = qoctopole(6) + rtrm*dx - dx*rx2
              qoctopole(7) = qoctopole(7) + rtrm*dy - dy*rx2
              qoctopole(8) = qoctopole(8) + rtrm*dz - 3.0*dz*rx2
              rtrm = 5.0_dp*dy*dy
              qoctopole(9) = qoctopole(9) + rtrm*dy - 3.0*dy*rx2
              qoctopole(10) = qoctopole(10) + rtrm*dz - dz*rx2
            endif
          endif
        endif
      enddo
    endif
!
!  Scale moments by factor of half to allow 
!  for double counting. For the quadrupole
!  there is also the factor of half in the
!  normal expression.
!
    qmonopole = 0.5_dp*qmonopole*angstoev
    if (icmm.gt.1) then
      qdipolex = 0.5_dp*qdipolex*angstoev
      qdipoley = 0.5_dp*qdipoley*angstoev
      qdipolez = 0.5_dp*qdipolez*angstoev
      if (icmm.gt.2) then
        do k = 1,6
          qquadrupole(k) = 0.25_dp*qquadrupole(k)*angstoev
        enddo
        if (icmm.gt.3) then
          do k = 1,10
            qoctopole(k) = 0.25_dp*qoctopole(k)*angstoev
          enddo
        endif
      endif
    endif
!**************************
!  Inner loop over boxes  *
!**************************
    nbjloop: do nbj = 1,nboxcmm
!
!  Find no. of atoms in box
!
      nbatj = 0
      do i = 1,numat
        if (nboxat(i).eq.nbj) then
          nbatj = nbatj + 1
          iptrj(nbatj) = i
        endif
      enddo
!
!  No atoms so skip box
!
      if (nbatj.eq.0) cycle nbjloop
!
!  Generate x,y,z indices
!   
      call indtoijk(nbj,njx,njy,njz,nboxx,nboxy)
!
!  Decide whether the box j interacts with box i by
!  multipole moments or explicitly.
!
      ldofull = .true.
      if (abs(njx - nix).gt.1) ldofull = .false.
      if (abs(njy - niy).gt.1) ldofull = .false.
      if (abs(njz - niz).gt.1) ldofull = .false.
      if (ldofull.and.nbi.le.nbj) then
!***************************************************
!  Explicit interaction of atoms in boxes i and j  *
!***************************************************
!
!  Loop over atoms in box i
!
        if (nbi.eq.nbj) then
          ilower = 2
        else
          ilower = 1
        endif
        do i = ilower,nbati
          ii = iptri(i)
          xal = xclat(ii)
          yal = yclat(ii)
          zal = zclat(ii)
          nati = nat(ii)
          ntypi = nftype(ii)
          nregioni = nregionno(nsft+ii)
          qli = qf(ii)
          oci = occuf(ii)
          lopi = (.not.lfreeze.or.lopf(ii))
          if (lbsmat(ii + nsft)) then
            radi = radf(ii)
          else
            radi = 0.0_dp
          endif
!
!  Molecule handling
!
          if (lmol) then
            nmi = natmol(ii)
            indmi = nmolind(ii)
          endif
          if (nbi.eq.nbj) then
            jupper = i - 1
          else
            jupper = nbatj
          endif
!
!  Loop over atoms in box i
!
          do j = 1,jupper
            jj = iptrj(j)
            lopj = (.not.lfreeze.or.lopf(jj))
            if (.not.lopi.and..not.lopj) goto 1110
            natj = nat(jj)
            ntypj = nftype(jj)
            nregionj = nregionno(nsft+jj)
            qlj = qf(jj)
            ocj = occuf(jj)
            if (lbsmat(nsft + jj)) then
              radj = radf(jj)
            else
              radj = 0.0_dp
            endif
            xcrd = xclat(jj) - xal
            ycrd = yclat(jj) - yal
            zcrd = zclat(jj) - zal
            radsum = radi + radj
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
            if (nati.eq.natj) then
              nat1 = nati
              nat2 = natj
              if (ntypi.lt.ntypj) then
                lorder12 = .true.
                ntyp1 = ntypi
                ntyp2 = ntypj
              else
                lorder12 = .false.
                ntyp1 = ntypj
                ntyp2 = ntypi
              endif
            elseif (nati.lt.natj) then
              lorder12 = .true.
              nat1 = nati
              nat2 = natj
              ntyp1 = ntypi
              ntyp2 = ntypj
            else
              lorder12 = .false.
              nat1 = natj
              nat2 = nati
              ntyp1 = ntypj
              ntyp2 = ntypi
            endif
            ofct = oci*ocj
            fct = ofct*angstoev
            factor = qli*qlj*fct
!
!  Possible core - shell flag
!
            lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Molecule handling
!
            if (lmol) then
              nmj = natmol(jj)
              indmj = nmolind(jj)
            endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
            rp = 0.0_dp
            npots = 0
            lneedmol = (lmol.and..not.lmolq)
            do n = 1,npote
              if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
                if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
                  npots = npots + 1
                  npotl(npots) = n
                  if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n)))  &
                    lneedmol = .true.
                  if (nptype(n).eq.8.or.nptype(n).eq.33) then
                    if (cuts.gt.rp) rp = cuts
                  else
                    if (rpot(n).gt.rp) rp = rpot(n)
                  endif
                endif
              endif
            enddo
!
!  Generate looping indices
!
            cut2r = rp*rp
            if (cut2r.gt.cut2p) cut2r = cut2p
            r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule and bonding checks
!
            if (lmol) then
              lmolok = (nmi.eq.nmj.and.nmi.ne.0)
            else
              lmolok = .false.
            endif
!  
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
            if (.not.lneedmol) lmolok = .false.
            if (lmolok.and.(r.gt.cut2s.or..not.lcspair)) then
              ind = indmj - indmi
              lptrmol = (ind.eq.0)
              if (.not.lptrmol) then
                call mindtoijk(indmj,jxx,jyy,jzz)
                call mindtoijk(indmi,ixx,iyy,izz)
                jxx = jxx - ixx
                jyy = jyy - iyy
                jzz = jzz - izz
                call samemol(lptrmol,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
              endif
              if (lptrmol) then
                call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,ii,jj,0_i4,0_i4,0_i4)
              else
                lbonded   = .false.
                l2bonds   = .false.
                l3bonds   = .false.
                nbtypeij  = 0
                nbtypeij2 = 0
              endif
            else
              lptrmol   = .false.
              lbonded   = .false.
              l2bonds   = .false.
              l3bonds   = .false.
              nbtypeij  = 0
              nbtypeij2 = 0
            endif
            if (r.gt.small2) then
!
!  Store vector
!
              nor = 1
              dist = sqrt(r)
            else
              goto 1110
            endif
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
!
!  Set core-shell to true to handle case where defect ion
!  is close to a perfect lattice site
!
            eatom_before = eatom
            ereal_before = ereal
            ec6_before   = ec6
            call twobody1(eatom,ereal,ec6,lgrad1,.false.,.false.,nor,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                          deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                          cut2r,cut2e,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                          sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                          nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
!
            if (lPrintTwo) then
              write(ioout,'(4x,i12,i12,f22.10,1x,f22.10)') i,j,eatom+ec6-eatom_before-ec6_before,ereal-ereal_before
            endif
!
            esum = eatom - eatom_before + ereal - ereal_before + ec6 - ec6_before
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
!
            siteenergy(i) = siteenergy(i) + 0.5_dp*esum
            siteenergy(j) = siteenergy(j) + 0.5_dp*esum
!
            if (lDoQDeriv1.and.lgrad1) then
              d0i = d0i + derive0*qlj
              d0j = d0j + derive0*qli
            endif
            if (leem) then
              eqeq_before = eqeq
              if (lqeq) then
                call qeqbody1(eqeq,lgrad1,.false.,nor,1_i4,dist,deriv,deriv2,fct,qli,qlj,nati,natj, &
                              d1i,d1j,d2i2,d2ij,d2j2)
              elseif (lSandM) then
                call smbody1(eqeq,lgrad1,.false.,nor,1_i4,dist,deriv,deriv2,fct,qli,qlj,nati,natj, &
                              d1i,d1j,d2i2,d2ij,d2j2)
              endif
!
              eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eqeq - eqeq_before
!
              siteenergy(i) = siteenergy(i) + 0.5_dp*(eqeq - eqeq_before)
              siteenergy(j) = siteenergy(j) + 0.5_dp*(eqeq - eqeq_before)
            endif
!
!  Add many-body contribution to rho
!
            if (lsuttonc) then
              if (.not.lMEAMden) then
                if (lorder12) then
                  scrho(1,ii) = scrho(1,ii) + sctrm1*ocj
                  scrho(1,jj) = scrho(1,jj) + sctrm2*oci
                else
                  scrho(1,ii) = scrho(1,ii) + sctrm2*ocj
                  scrho(1,jj) = scrho(1,jj) + sctrm1*oci
                endif
              endif
            endif
!*****************************
!  Charge first derivatives  *
!*****************************
            if (lgrad1.and.lDoQDeriv1) then
              call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
            endif
!***********************
!  Radial derivatives  *
!***********************
            if (lgrad1) then
              if (radi.gt.0.0_dp) then
                raderv(ii) = raderv(ii) + rtrm1
              endif
              if (radj.gt.0.0_dp) then
                raderv(jj) = raderv(jj) + rtrm1
              endif
            endif
!**************************
!  Coordinate Derivatives *
!**************************
!
!  First derivatives
!
            if (lgrad1) then
              xdrv(ii) = xdrv(ii) - deriv*xcrd
              ydrv(ii) = ydrv(ii) - deriv*ycrd
              zdrv(ii) = zdrv(ii) - deriv*zcrd
              xdrv(jj) = xdrv(jj) + deriv*xcrd
              ydrv(jj) = ydrv(jj) + deriv*ycrd
              zdrv(jj) = zdrv(jj) + deriv*zcrd
!
              if (nregioni.ne.nregionj) then
                xregdrv(nregioni) = xregdrv(nregioni) - deriv*xcrd
                yregdrv(nregioni) = yregdrv(nregioni) - deriv*ycrd
                zregdrv(nregioni) = zregdrv(nregioni) - deriv*zcrd
                xregdrv(nregionj) = xregdrv(nregionj) + deriv*xcrd
                yregdrv(nregionj) = yregdrv(nregionj) + deriv*ycrd
                zregdrv(nregionj) = zregdrv(nregionj) + deriv*zcrd
              endif
            endif
1110        continue
          enddo
        enddo
        if (nbi.eq.nbj) then
!
!  Breathing shell self terms
!
          do i = 1,nbati
            ii = iptri(i)
            iar = nsft + nrelat(ii)
            lbreathe = lbsmat(iar)
            nati = nat(ii)
            ntypi = nftype(ii)
            oci = occuf(ii)
            radi = radf(ii)
            if (lbreathe) then
              eatom_before = eatom
              if (nati.gt.maxele) nati = nati - maxele
!******************************
!  Breathing shell self term  *
!******************************
              do m = 1,npote
                if (nptype(m).eq.14) then
                  if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                    apt = twopot(1,m)*oci
                    rdiff = radi - twopot(2,m)
                    eatom = eatom + 0.5_dp*apt*rdiff*rdiff
                    if (lgrad1) then
                      raderv(ii) = raderv(ii) + apt*rdiff
                    endif
                  endif
                elseif (nptype(m).eq.17) then
                  if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                    apt = twopot(1,m)*oci
                    bpt = twopot(2,m)
                    rdiff = radi - twopot(3,m)
                    etrm1 = exp(bpt*rdiff)
                    etrm2 = 1.0_dp/etrm1
                    eatom = eatom + apt*(etrm1 + etrm2)
                    if (lgrad1) then
                      raderv(ii) = raderv(ii) + apt*bpt*(etrm1 - etrm2)
                    endif
                  endif
                elseif (nptype(m).eq.31) then
                  if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                    apt = twopot(1,m)*oci
                    bpt = twopot(2,m)
                    rdiff = radi - twopot(3,m)
                    etrm1 = exp(bpt*rdiff)
                    eatom = eatom + apt*etrm1
                    if (lgrad1) then
                      raderv(ii) = raderv(ii) + apt*bpt*etrm1
                    endif
                  endif
                endif
              enddo
              eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + eatom - eatom_before
              siteenergy(i) = siteenergy(i) + (eatom - eatom_before)
            endif
          enddo
        endif
      elseif (.not.ldofull) then
!**************************
!  Cell multipole method  *
!**************************
!
!  Loop over atoms in box j
!
        do j = 1,nbatj
          jj = iptrj(j)
          xj = xclat(jj)
          yj = yclat(jj)
          zj = zclat(jj)
          nregionj = nregionno(nsft+jj)
!
!  Calculate distance and components to box centre
!
          dx = xj - xmid
          dy = yj - ymid
          dz = zj - zmid
          rx2 = dx*dx + dy*dy + dz*dz
          rx = sqrt(rx2)
          rrx = 1.0_dp/rx
          qli = qf(jj)*occuf(jj)
!*******************************
!  Calculate mulipolar energy  *
!*******************************
!
!  Monopole
!
          rtrm = qmonopole*rrx
          if (lgrad1) dtrm = 2.0d0*rtrm
          rrx2 = rrx*rrx
          if (icmm.gt.1) then
            rrx3 = rrx*rrx2
!
!  Dipole
!
            rtrm1 = (qdipolex*dx + qdipoley*dy + qdipolez*dz)*rrx3
            rtrm = rtrm + rtrm1
            if (lgrad1) dtrm = dtrm + 4.0_dp*rtrm1
            if (icmm.gt.2) then
              rrx5 = rrx2*rrx3
!
!  Quadrupole - off diagonals multiplied by two as we
!  are working in lower-half triangular form
!
              rtrm1 = qquadrupole(1)*dx*dx
              rtrm1 = rtrm1 + 2.0_dp*qquadrupole(2)*dx*dy
              rtrm1 = rtrm1 + 2.0_dp*qquadrupole(3)*dx*dz
              rtrm1 = rtrm1 + qquadrupole(4)*dy*dy
              rtrm1 = rtrm1 + 2.0_dp*qquadrupole(5)*dy*dz
              rtrm1 = rtrm1 + qquadrupole(6)*dz*dz
              rtrm = rtrm + rtrm1*rrx5
              if (lgrad1) then
                rtrm1 = fquadrupole(1)*dx*dx
                rtrm1 = rtrm1 + 2.0_dp*fquadrupole(2)*dx*dy
                rtrm1 = rtrm1 + 2.0_dp*fquadrupole(3)*dx*dz
                rtrm1 = rtrm1 + fquadrupole(4)*dy*dy
                rtrm1 = rtrm1 + 2.0_dp*fquadrupole(5)*dy*dz
                rtrm1 = rtrm1 + fquadrupole(6)*dz*dz
                dtrm = dtrm + rtrm1*rrx5
              endif
              if (icmm.gt.3) then
                rtrm1 = qoctopole(1)*dx*dx*dx
                rtrm1 = rtrm1 + 3.0_dp*qoctopole(2)*dy*dx*dx
                rtrm1 = rtrm1 + 3.0_dp*qoctopole(3)*dz*dx*dx
                rtrm1 = rtrm1 + 3.0_dp*qoctopole(4)*dy*dy*dx
                rtrm1 = rtrm1 + 6.0_dp*qoctopole(5)*dz*dy*dx
                rtrm1 = rtrm1 + 3.0_dp*qoctopole(6)*dz*dz*dx
                rtrm1 = rtrm1 + 3.0_dp*qoctopole(7)*dz*dz*dy
                rtrm1 = rtrm1 + qoctopole(8)*dz*dz*dz
                rtrm1 = rtrm1 + qoctopole(9)*dy*dy*dy
                rtrm1 = rtrm1 + 3.0_dp*qoctopole(10)*dz*dy*dy
                rtrm = rtrm + rtrm1*rrx5*rrx2
                if (lgrad1) then
                  rtrm1 = foctopole(1)*dx*dx*dx
                  rtrm1 = rtrm1 + 3.0_dp*foctopole(2)*dy*dx*dx
                  rtrm1 = rtrm1 + 3.0_dp*foctopole(3)*dz*dx*dx
                  rtrm1 = rtrm1 + 3.0_dp*foctopole(4)*dy*dy*dx
                  rtrm1 = rtrm1 + 6.0_dp*foctopole(5)*dz*dy*dx
                  rtrm1 = rtrm1 + 3.0_dp*foctopole(6)*dz*dz*dx
                  rtrm1 = rtrm1 + 3.0_dp*foctopole(7)*dz*dz*dy
                  rtrm1 = rtrm1 + foctopole(8)*dz*dz*dz
                  rtrm1 = rtrm1 + foctopole(9)*dy*dy*dy
                  rtrm1 = rtrm1 + 3.0_dp*foctopole(10)*dz*dy*dy
                  dtrm = dtrm + rtrm1*rrx5*rrx2
                endif
              endif
            endif
          endif
!
!  Multiply by charge
!
          ecmm = ecmm + rtrm*qli
!
          eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + rtrm*qli
!
          siteenergy(i) = siteenergy(i) + 0.5_dp*rtrm*qli
          siteenergy(j) = siteenergy(j) + 0.5_dp*rtrm*qli
          if (lgrad1) then
            dtrm = dtrm*qli*rrx2
            xdrv(jj) = xdrv(jj) - dtrm*dx
            ydrv(jj) = ydrv(jj) - dtrm*dy
            zdrv(jj) = zdrv(jj) - dtrm*dz
!
            if (nregioni.ne.nregionj) then
              xregdrv(nregionj) = xregdrv(nregionj) - dtrm*dx
              yregdrv(nregionj) = yregdrv(nregionj) - dtrm*dy
              zregdrv(nregionj) = zregdrv(nregionj) - dtrm*dz
            endif
          endif
        enddo
      endif
!
!  End of box loops
!
    enddo nbjloop
  enddo nbiloop
!****************
!  Global sums  *
!****************
  tsum0 = cputime()
  if (lsuttonc.and.nprocs.gt.1) then
    if (.not.lMEAMden) then
      do i = 1,numat
        sum2(i) = scrho(1,i)
      enddo
      call sumall(sum2,sum,numat,"realcmm","scrho")
      do i = 1,numat
        scrho(1,i) = sum(i)
      enddo
    endif
  endif
  tsuml = cputime() - tsum0
  tsum = tsum + tsuml
!
!  Closing banner for energy decomposition
!
  if (lPrintTwo) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(sum2,stat=status)
  if (status/=0) call deallocate_error('realcmm','sum2')
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('realcmm','sum')
  deallocate(iptrj,stat=status)
  if (status/=0) call deallocate_error('realcmm','iptrj')
  deallocate(iptri,stat=status)
  if (status/=0) call deallocate_error('realcmm','iptri')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realcmm','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1 - tsuml
!
  return
  end
