  subroutine realmc3(eatom,ereal,erecip,ec6,ntrialatom,nptrtrialatom,ltrialatom)
!
!  Subroutine for calculating real space energy for a set of trial atoms
!  only during MC
!
!   1/08 Created from realmd3
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody argument list
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
  use configurations, only : lbsmat
  use constants
  use control
  use current
  use datatypes
  use derivatives
  use eam,            only : maxmeamcomponent
  use element
  use general,        only : cutw
  use kspace
  use mdlogic
  use molecule
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use realvectors
  use shell
  use sutton
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: ntrialatom
  integer(i4), intent(in)                      :: nptrtrialatom(ntrialatom)
  logical,     intent(in)                      :: ltrialatom(numat)
  real(dp),    intent(inout)                   :: eatom
  real(dp),    intent(inout)                   :: ec6
  real(dp),    intent(inout)                   :: ereal
  real(dp),    intent(inout)                   :: erecip
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ix
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: j
  integer(i4)                                  :: jx
  integer(i4)                                  :: k
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nt
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lc6loc
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lself
  real(dp)                                     :: c6tot
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: cut2w
  real(dp)                                     :: d2self
  real(dp)                                     :: derive0self
  real(dp)                                     :: eatom2
  real(dp)                                     :: ec62 
  real(dp)                                     :: ereal2
  real(dp)                                     :: erecip2
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rp
  real(dp)                                     :: rtrm1
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
!
!  Local variables
!
  lc6loc = (lc6.and.ndim.eq.3)
!
!  Set the Coulomb term type based on dimensionality :
!
!  1-D         => 1/r
!  2-D and 3-D => erfc(seta*r)/r
!
  lewaldtype = (ndim.ne.1.or.lwolf)
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2e = rmx2
  cut2s = cuts*cuts
  cut2w = cutw*cutw
  if (lwolf) then
    cut2q = cut2w
  else
    cut2q = cut2e
  endif
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realmd3','npotl')
!
  if (.not.lnoreal) then
!***************************************************************
!  Atomistic and real space electrostatic component of energy  *
!***************************************************************
!
!  Outer loop over trial sites
!
    do nt = 1,ntrialatom
      i = nptrtrialatom(nt)
      xal = xclat(i)
      yal = yclat(i)
      zal = zclat(i)
      nati = nat(i)
      ntypi = nftype(i)
      qli = qf(i)
      oci = occuf(i)
      if (lbsmat(nrelat(i)+nsft)) then
        radi = radf(i)
      else
        radi = 0.0_dp
      endif
!
!  Molecule handling
!
      if (lmol) then
        nmi = natmol(i)
        indm = nmolind(i)
        call mindtoijk(indm,ixi,iyi,izi)
      endif
!
!  Start of second atom loop
!
      jloop: do j = procid+1,numat,nprocs
        natj = nat(j)
        ntypj = nftype(j)
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
          nat2 = nat(j)
          ntyp1 = ntypi
          ntyp2 = nftype(j)
        else
          lorder12 = .false.
          nat1 = nat(j)
          nat2 = nati
          ntyp1 = nftype(j)
          ntyp2 = ntypi
        endif
!
        xcrd = xclat(j) - xal
        ycrd = yclat(j) - yal
        zcrd = zclat(j) - zal
        qlj = qf(j)
        ocj = occuf(j)
        if (lbsmat(nsft+nrelat(j))) then
          radj = radf(j)
        else
          radj = 0.0_dp
        endif
        radsum = radi + radj
!
!  Set factor that depends on whether j is a trial atom or not
!
        if (ltrialatom(j)) then
          ofct = 0.5_dp*oci*ocj
        else
          ofct = oci*ocj
        endif
        fct = ofct*angstoev
        factor = qli*qlj*fct
!
!  Possible core-shell flag
!
        lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Molecule handling
!
        if (lmol) then
          nmj = natmol(j)
          indmj = nmolind(j)
          call mindtoijk(indmj,ixj,iyj,izj)
          ixj = ixj - ixi
          iyj = iyj - iyi
          izj = izj - izi
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Locate potential number
!  Check whether potential requires specific types
!  Calculate sum of all dispersion terms for pair
!
        rp = 0.0_dp
        npots = 0
        c6tot = 0.0_dp
        lneedmol = (lmol.and..not.lmolq)
        do n = 1,npote
          if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
            if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
              npots = npots + 1
              npotl(npots) = n
              if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol = .true.
              if (nptype(n).eq.8.or.nptype(n).eq.33) then
                if (cuts.gt.rp) rp = cuts
              elseif (lc6loc) then
                if (nptype(n).eq.1.or.nptype(n).eq.7) then
                  c6tot = c6tot + twopot(3,n)
                  if (repcut(n).gt.rp) rp = repcut(n)
                elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
                  c6tot = c6tot + twopot(2,n)
                  if (repcut(n).gt.rp) rp = repcut(n)
                else
                  if (rpot(n).gt.rp) rp = rpot(n)
                endif
              else
                if (rpot(n).gt.rp) rp = rpot(n)
              endif
            endif
          endif
        enddo
!
!  If no valid potentials and charge product is zero
!  then no need to search for distances
!
        if (npots.eq.0.and.abs(factor).lt.1.0d-8) cycle jloop
        cut2r = rp*rp
        if (cut2r.gt.cut2p) cut2r = cut2p
        cut2 = cut2r
        if (cut2e.gt.cut2.and.lewald) cut2 = cut2e
        if (cut2w.gt.cut2.and.lwolf) cut2 = cut2w
!
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
        if (.not.lneedmol) lmolok = .false.
!
        nmolonly = 0
        d2self = 0.0_dp
!***********************
!  Find valid vectors  *
!***********************
        if (ndim.eq.3) then
          call rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.2) then
          call rsearch2D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.1) then
          call rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        endif
!
        ereal2 = 0.0_dp
        erecip2 = 0.0_dp
        ec62 = 0.0_dp
        derive0self = 0.0_dp
        if (lself) call selfterm(ereal2,erecip2,ec62,derive0self,factor,fct,ofct,1.0_dp,npotl,npots, &
                                 c6tot,d2self,.false.,.false.,i,j,ix,jx,1.0_dp,0.5_dp,lewaldtype,qli,qlj)
        ereal = ereal + ereal2
        erecip = erecip + erecip2
        ec6 = ec6 + ec62
!
        if (nor.eq.0) cycle jloop
!
!  Sqrt distances
!
        do k = 1,nor
          dist(k) = sqrt(dist(k))
        enddo
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
        eatom2 = 0.0_dp
        ereal2 = 0.0_dp
        ec62 = 0.0_dp
        call twobody(eatom2,ereal2,ec62,.false.,.false.,.false.,nor,1_i4,npots,npotl,cut2r, &
                     cut2q,cut2s,nmolonly,factor,ofct,radsum,rtrm1,sctrm1,sctrm2,qli,qlj, &
                     lcspair,lewaldtype,.false.,.false.,.false.,lorder12)
        eatom = eatom + eatom2
        ereal = ereal + ereal2
        ec6 = ec6 + ec62
!*************************************
!  Electrostatic potential on-sites  *
!*************************************
      enddo jloop
    enddo
!
!  Exit point
!
  endif
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realmd3','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1 
!
  return
  end
