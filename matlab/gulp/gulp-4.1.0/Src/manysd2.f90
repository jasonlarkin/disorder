  subroutine manysd2(emany,lgrad1,lgrad2)
!
!  Subroutine for calculating the many-body energy from the
!  Sutton-Chen potential(s). Requires real space routine to
!  be called first to evaluate the repulsive contribution
!  and the rho parameters.
!
!  Symmetry adapted version
!
!  On entry the array scrho must contain the density at each atomic site.
!
!   4/97 Created from realsd2 and many
!   5/97 Exponential form of Sutton-Chen added
!   7/97 Adapted for density and functional options
!   3/99 Re-named from manysd.f
!  10/99 Cubic density function added
!   6/00 nadd default values reduced to save CPU time as they were
!        over cautious and the many-body densities decay rapidly
!        with distance.
!   3/03 Speed up added to second derivative distance checking
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
!  12/07 Call to rfindeither corrected
!  12/07 Unused variables removed
!  12/07 Call to rfindeither modified now that lincludeself is of intent in
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
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 Skipping when density is zero modified to ensure no terms are missed
!   3/09 Sign of return arguments from EAM/MEAM function routines corrected for
!   4/09 MEAM screening function derivatives added
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use control
  use current
  use derivatives
  use eam
  use general
  use iochannels,     only : ioout
  use optimisation
  use realvectors,    only : dist, dist2, xtmp, ytmp, ztmp, xtmp2, ytmp2, ztmp2
  use sutton
  use symmetry
  use times
  use two
  use vectors,        only : vector_pair
  implicit none
!
!  Passed variables
!
  real(dp),    intent(inout)                   :: emany
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixf
  integer(i4)                                  :: iyf
  integer(i4)                                  :: izf
  integer(i4)                                  :: ixfo
  integer(i4)                                  :: iyfo
  integer(i4)                                  :: izfo
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxc
  integer(i4)                                  :: jyc
  integer(i4)                                  :: jzc
  integer(i4)                                  :: k
  integer(i4)                                  :: kl
  integer(i4)                                  :: km
  integer(i4)                                  :: l
  integer(i4)                                  :: la
  integer(i4)                                  :: lvec
  integer(i4)                                  :: m
  integer(i4)                                  :: n  
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: natk
  integer(i4)                                  :: neamspeci
  integer(i4)                                  :: neamspecj
  integer(i4)                                  :: neamspeck
  integer(i4)                                  :: neqi
  integer(i4)                                  :: nfa
  integer(i4)                                  :: nff
  integer(i4)                                  :: nfi
  integer(i4)                                  :: nfif
  integer(i4)                                  :: nfj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4)                                  :: nor2
  integer(i4)                                  :: np
  integer(i4)                                  :: npartial
  integer(i4)                                  :: npot
  integer(i4)                                  :: npotijk
  integer(i4)                                  :: npots
  integer(i4)                                  :: nreli
  integer(i4)                                  :: nrelj
  integer(i4)                                  :: nrelk
  integer(i4)                                  :: ntyp1  
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj    
  integer(i4)                                  :: ntypk
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: nvec
  integer(i4)                                  :: nvec0
  integer(i4)                                  :: status
  logical                                      :: lanyvalidik
  logical                                      :: lanyvalidjk
  logical                                      :: lnonzeroSij
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lpartial
  logical                                      :: lself 
  logical                                      :: lsg1 
  logical                                      :: lsg2
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
  real(dp)                                     :: derivl(3)
  real(dp)                                     :: derivs(6)
  real(dp)                                     :: deriv2(6)
  real(dp)                                     :: deriv2s(21)
  real(dp)                                     :: deriv2m(6,3)
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
  real(dp)                                     :: drhototijk2s(6,6)
  real(dp)                                     :: drhototjik2s(6,6)
  real(dp)                                     :: drhototkij2s(6,6)
  real(dp)                                     :: drhototijk2m(6,3)
  real(dp)                                     :: drhototjik2m(6,3)
  real(dp)                                     :: drhototkij2m(6,3)
  real(dp)                                     :: dt1
  real(dp)                                     :: dt2
  real(dp)                                     :: dt3
  real(dp)                                     :: dt4
  real(dp)                                     :: eeam
  real(dp)                                     :: emanytrm
  real(dp)                                     :: elcom(6,6)
  real(dp)                                     :: oci      
  real(dp)                                     :: ocj  
  real(dp)                                     :: ock
  real(dp)                                     :: ofct
  real(dp)                                     :: ofctijk
  real(dp)                                     :: r
  real(dp)                                     :: r2
  real(dp)                                     :: rcut2
  real(dp)                                     :: rcutfactor
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
  real(dp)                                     :: rjk
  real(dp)                                     :: rik2
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
  real(dp),    dimension(:,:), allocatable     :: scrhofull
  real(dp)                                     :: Sij
  real(dp)                                     :: Silj
  real(dp)                                     :: dSiljdr(3)
  real(dp)                                     :: time1
  real(dp)                                     :: time2  
  real(dp)                                     :: wrk(6)
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
  real(dp)                                     :: xcrd 
  real(dp)                                     :: ycrd    
  real(dp)                                     :: zcrd
  real(dp)                                     :: xcrdik
  real(dp)                                     :: ycrdik
  real(dp)                                     :: zcrdik
  real(dp)                                     :: xcrdjk
  real(dp)                                     :: ycrdjk
  real(dp)                                     :: zcrdjk
  real(dp)                                     :: xil0
  real(dp)                                     :: yil0
  real(dp)                                     :: zil0
  real(dp)                                     :: xjl0
  real(dp)                                     :: yjl0
  real(dp)                                     :: zjl0
  type(screening_atoms)                        :: partial
  type(vector_pair)                            :: vectorpair
!
  time1 = cputime()
!
!  For screened MEAM, set scale factor that determines searching cutoff based on which axis of the ellipse is largest.
!  Note: the cutoff is applied to the mid point of the i-j vector and so this is guaranteed to find all distances.
!
  if (lMEAMscreen) then
    rcutfactor = max(1.0_dp,0.25_dp*(1.0_dp + meam_Cmax))
  endif
!
!  Scale density
!
  call eamscalescrho(1_i4)
  if (lPrintEAM) then
!
!  Openning banner for energy decomposition
!
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  EAM : Atom No.                Density                 Atom energy (eV) '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  Energy calculation
!
  emany = 0.0_dp
  do i = 1,nasym
    neamspeci = neamfnspecptr(nrel2(i))
    rhoi = scrho(1,i)
    if (neamspeci.gt.0.and.rhoi.gt.1.0d-12) then
      if (lMEAMfn) then
        call meamfnderv(neamfn,neamspeci,scrho(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
      else
        call eamfnderv(neamfn,neamspeci,scrho(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
      endif
      emanytrm = dble(neqv(i))*occua(i)*eeam
      emany = emany + emanytrm
      if (lPrintEAM) then                      
        write(ioout,'(7x,i8,4x,f24.10,4x,f24.12)') i,rhoi,emanytrm
      endif
    endif
  enddo
  if (lPrintEAM) then
!     
!  Closing banner for energy decomposition
!
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  If no forces are needed then we don't need to do loops over atoms, so just return
!
  if (.not.lgrad1) goto 1000
!
!  From here on we can assume that lgrad1 = .true.
!
!
!  Local variables
!
  if (lfreeze) then
    nfa = 0
    nff = 0
    do i = 1,nasym
      if (lopf(i)) then
        nfa = nfa + 1
        nff = nff + neqv(i)
      endif
    enddo
  endif
!
  lsg1 = lstr
  lsg2 = (lgrad2.and.lstr)
  if (lsg1) then
    do i = 1,6
      wrk(i) = 0.0_dp
    enddo
    if (lsg2) then
      do i = 1,6
        do j = 1,6
          elcom(j,i) = 0.0_dp
        enddo
      enddo
    endif
  endif
!
!  Find maximum cut-off radius
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
  if (status/=0) call outofmemory('manysd2','npotl')
  allocate(scrhofull(maxmeamcomponent,numat),stat=status)
  if (status/=0) call outofmemory('manysd2','scrhofull')
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
!
  if (lnoreal) goto 999
!
!  If this is MEAM then we need to generate the density components for all atoms in the unit cell
!  since the components are vector quantities and not scalars.
!
  call meamsymdensity(scrho,scrhofull)
!
!  Outer loop over sites
!
  ix = - 2
  iy = - 1
  iz =   0
  ixf = 1
  iyf = 2
  izf = 3
  ixfo = 1
  iyfo = 2
  izfo = 3
  nfi = 0
  nfif = 1
  iloop: do i = 1,nasym
    lopi = (.not.lfreeze.or.lopf(i))
    if (.not.lopi) cycle iloop
    nfi = nfi + 1
    nfif = nfif + neqv(i)
    ix = ix + 3
    iy = iy + 3
    iz = iz + 3
    ixf = ixfo
    iyf = iyfo
    izf = izfo
    ixfo = ixfo + 3*neqv(i)
    iyfo = iyfo + 3*neqv(i)
    izfo = izfo + 3*neqv(i)
    nreli = nrel2(i)
!
!  Find EAM species for i
!
    neamspeci = neamfnspecptr(nrel2(i))
!
!  Evaluate functional derivatives
!
    if (lMEAMfn) then
      call meamfnderv(neamfn,neamspeci,scrho(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.true.,lgrad2,.false.)
    else
      call eamfnderv(neamfn,neamspeci,scrho(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.true.,lgrad2,.false.)
      rhoi = scrho(1,i)
    endif
!
    xal = xalat(i)
    yal = yalat(i)
    zal = zalat(i)
    nati = iatn(i)
    ntypi = natype(i)
    neqi = neqv(i)
    oci = occua(i)
!
!  Start of second atom loop
!
    jxc = - 2
    jyc = - 1
    jzc =   0
    nfj = 0
    jloop: do j = 1,numat
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
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
      elseif (nati.lt.natj) then
        nat1 = nati
        nat2 = nat(j)
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        nat1 = nat(j)
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
!
!  Freeze flag
!
      lopj = (.not.lfreeze.or.lopf(nrelat(j)))
      if (lopj) nfj = nfj + 1
      ocj = occuf(j)
      if (.not.lfreeze.or.lopj) then
        jxc = jxc + 3
        jyc = jyc + 3
        jzc = jzc + 3
        jx = jxc
        jy = jyc
        jz = jzc
      else
        jx = ixf
        jy = iyf
        jz = izf
      endif
      nrelj = nrelat(j)
!
!  Find EAM species for j
!
      neamspecj = neamfnspecptr(j)
!
!  Evaluate functional derivatives
!
      if (lMEAMfn) then
        call meamfnderv(neamfn,neamspecj,scrho(1,nrelj),rhoj,eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,lgrad2,.false.)
      else
        call eamfnderv(neamfn,neamspecj,scrho(1,nrelj),eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,lgrad2,.false.)
        rhoj = scrho(1,nrelj)
      endif
!
!  If there is no density at either centre and this is not a second derivative run then cycle
!
      if (rhoi.eq.0.0_dp.and.rhoj.eq.0.0_dp.and..not.lgrad2) cycle jloop
!
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      ofct = oci*ocj*neqi
!
!  Locate potential number
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
!  Need to make cut-off equal to double the maximum
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
      rp = sqrt(cut2)
!*********************************
!  Find valid vectors for i - j  *
!*********************************
      call rfind(xcrd,ycrd,zcrd,cut2,0.0_dp,0.0_dp,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nmolonly,lself,0_i4,nor)
!
!  Loop over valid vectors
!
      do ii = 1,nor
!***************************************************
!  Calculate many-body contribution in real space  *
!***************************************************
        deriv(1:3) = 0.0_dp
        if (lstr) then
          derivs(1:6) = 0.0_dp
        endif
        if (lgrad2) then
          deriv2(1:6) = 0.0_dp
          if (lstr) then
            deriv2s(1:21) = 0.0_dp
            deriv2m(1:6,1:3) = 0.0_dp
          endif
        endif
        r2 = dist(ii)
        r = sqrt(r2)
        xcd = xtmp(ii)
        ycd = ytmp(ii)
        zcd = ztmp(ii)
!***************************************
!  Valid many-body potentials for i-j  *
!***************************************
        if (lMEAM) then
          rhoij(1:maxmeamcomponent) = 0.0_dp
          rhoji(1:maxmeamcomponent) = 0.0_dp
          drhoij(1:3,1:maxmeamcomponent) = 0.0_dp
          drhoji(1:3,1:maxmeamcomponent) = 0.0_dp
          if (lstr) then
            drhoijs(1:6,1:maxmeamcomponent) = 0.0_dp
            drhojis(1:6,1:maxmeamcomponent) = 0.0_dp
          endif
          if (lgrad2) then
            drhoij2(1:6,1:maxmeamcomponent) = 0.0_dp
            drhoji2(1:6,1:maxmeamcomponent) = 0.0_dp
            if (lstr) then
              drhoij2s(1:21,1:maxmeamcomponent) = 0.0_dp
              drhoji2s(1:21,1:maxmeamcomponent) = 0.0_dp
              drhoij2m(1:6,1:3,1:maxmeamcomponent) = 0.0_dp
              drhoji2m(1:6,1:3,1:maxmeamcomponent) = 0.0_dp
            endif
          endif
        else
          rhoij(1) = 0.0_dp
          rhoji(1) = 0.0_dp
        endif
        drhototij(1:3) = 0.0_dp
        drhototji(1:3) = 0.0_dp
        if (lstr) then
          drhototijs(1:6) = 0.0_dp
          drhototjis(1:6) = 0.0_dp
        endif
        if (lgrad2) then
          drhototij2(1:6) = 0.0_dp
          drhototji2(1:6) = 0.0_dp
          if (lstr) then
            drhototij2s(1:21) = 0.0_dp
            drhototji2s(1:21) = 0.0_dp
            drhototij2m(1:6,1:3) = 0.0_dp
            drhototji2m(1:6,1:3) = 0.0_dp
          endif
        endif
        lvalidij = .false.
        if (npots.gt.0) then
          do m = 1,npots
            npot = npotl(m)
            if (r.gt.rpot2(npot).and.r.le.rpot(npot).and.r.le.rpmax) then
              lvalidij = .true.
!**********************************
!  Calculate density derivatives  *
!**********************************
              if (lMEAMden) then
                call meamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcd,ycd,zcd,rhoij,rhoji,drhoij,drhoji, &
                             drhoijs,drhojis,drhoij2,drhoji2,drhoij2s,drhoji2s,drhoij2m,drhoji2m, &
                             1.0_dp,1.0_dp,.true.,lstr,.true.,lgrad2)
!*******************************
!  Compute screening function  *
!*******************************
!
!  Initialise screening function to 1
!
                lnonzeroSij = .true.
                Sij = 1.0_dp
!
                if (lMEAMscreen) then
!
!  Set cutoffs for possible screening atoms
!
                  rcut2 = rcutfactor*r2
!
!  Loop over atoms to search for images that may contribute to the screening
!
                  l = 0
                  npartial = 0
                  do while (l.lt.numat.and.lnonzeroSij)
                    l = l + 1
!
!  Set basic vectors between atoms
!
                    xil0 = xclat(l) - xal
                    yil0 = yclat(l) - yal
                    zil0 = zclat(l) - zal
                    xjl0 = xil0 - xcd
                    yjl0 = yil0 - ycd
                    zjl0 = zil0 - zcd
!
!  Find images within cutoffs of both atoms - excluding self images
!
                    nvec0 = 0
                    call rfindmid(xil0,yil0,zil0,xjl0,yjl0,zjl0,rcut2,.false.,nvec0,nvec,vectorpair)
!
!  Loop over results of search
!
                    lvec = 0
                    do while (lvec.lt.nvec.and.lnonzeroSij)
                      lvec = lvec + 1
!
!  Compute screening function
!
                      call meamscreen(r2,vectorpair%distance_pair1(lvec),vectorpair%distance_pair2(lvec),Silj, &
                                      dSiljdr,lpartial,.true.)
!
!  If screening function contribution is 0, then no need to continue for this pair
!
                      if (Silj.eq.0.0_dp) then
                        lnonzeroSij = .false.
                        Sij = 0.0_dp
                      else
!
!  Multiply total screening product
!
                        Sij = Sij*Silj
                        if (lpartial) then
!
!  If this atom has a screening factor between 0 and 1, we need to keep track of it since it will generate non-zero derivatives
!
                          npartial = npartial + 1
                          if (npartial.gt.partial%sa_maxdim) then
                            call changemaxsa(partial,npartial)
                          endif
                          partial%sa_atom(npartial) = l
                          partial%sa_xik(npartial) = vectorpair%xvector_pair1(lvec)
                          partial%sa_yik(npartial) = vectorpair%yvector_pair1(lvec)
                          partial%sa_zik(npartial) = vectorpair%zvector_pair1(lvec)
                          partial%sa_xjk(npartial) = vectorpair%xvector_pair2(lvec)
                          partial%sa_yjk(npartial) = vectorpair%yvector_pair2(lvec)
                          partial%sa_zjk(npartial) = vectorpair%zvector_pair2(lvec)
                          partial%sa_Sikj(npartial) = Silj
                          partial%sa_dSikjdr(1:3,npartial) = dSiljdr(1:3)
                        endif
                      endif
!
!  End loop over images of possible screening atoms
!
                    enddo
!
!  End loop over possible screening atoms
!
                  enddo
!
!  End of screening function
!
                endif
!
!  Only do remainder of work if the screening factor is non-zero
!
                if (lnonzeroSij) then
                  if (lMEAMscreen) then
                    if (npartial.gt.0) then
!
!  Compute derivative contributions of the screening function
!
                      call meamtotalrhoscreenderv(neamspeci,npartial,partial,xcd,ycd,zcd,scrho(1,i), &
                                                  rhoi,rscrhoi,rhoij,Sij,lsg1)
!
                      do np = 1,npartial
                        l = partial%sa_atom(np)
                        la = nrelat(l)
!
!  i-j contribution
!
                        xdrv(i) = xdrv(i) - partial%sa_drhototij(1,np)*ofct
                        ydrv(i) = ydrv(i) - partial%sa_drhototij(2,np)*ofct
                        zdrv(i) = zdrv(i) - partial%sa_drhototij(3,np)*ofct
!
!  i-k contribution
!
                        xdrv(i) = xdrv(i) - partial%sa_drhototik(1,np)*ofct
                        ydrv(i) = ydrv(i) - partial%sa_drhototik(2,np)*ofct
                        zdrv(i) = zdrv(i) - partial%sa_drhototik(3,np)*ofct
!
!  Manipulate the contribution to the derivatives of l to those acting on the image of l in the asymmetric unit
!
                        derivl(1) = partial%sa_drhototik(1,np)*ofct
                        derivl(2) = partial%sa_drhototik(2,np)*ofct
                        derivl(3) = partial%sa_drhototik(3,np)*ofct
                        call symdervrot(l,nrel2(la),derivl)
                        xdrv(la) = xdrv(la) + derivl(1)
                        ydrv(la) = ydrv(la) + derivl(2)
                        zdrv(la) = zdrv(la) + derivl(3)
!
                        if (lstr) then
                          wrk(1:6) = wrk(1:6) + partial%sa_drhotots(1:6,np)*ofct
                        endif
                      enddo

                      call meamtotalrhoscreenderv(neamspecj,npartial,partial,xcd,ycd,zcd,scrhofull(1,j), &
                                                  rhoj,rscrhoj,rhoji,Sij,lsg1)
!
                      do np = 1,npartial
                        l = partial%sa_atom(np)
                        la = nrelat(l)
!
!  i-j contribution
!
                        xdrv(i) = xdrv(i) - partial%sa_drhototij(1,np)*ofct
                        ydrv(i) = ydrv(i) - partial%sa_drhototij(2,np)*ofct
                        zdrv(i) = zdrv(i) - partial%sa_drhototij(3,np)*ofct
!
!  i-k contribution
!
                        xdrv(i) = xdrv(i) - partial%sa_drhototik(1,np)*ofct
                        ydrv(i) = ydrv(i) - partial%sa_drhototik(2,np)*ofct
                        zdrv(i) = zdrv(i) - partial%sa_drhototik(3,np)*ofct
!
!  Manipulate the contribution to the derivatives of k to those acting on the image of k in the asymmetric unit
!
                        derivl(1) = partial%sa_drhototik(1,np)*ofct
                        derivl(2) = partial%sa_drhototik(2,np)*ofct
                        derivl(3) = partial%sa_drhototik(3,np)*ofct
                        call symdervrot(l,nrel2(la),derivl)
                        xdrv(la) = xdrv(la) + derivl(1)
                        ydrv(la) = ydrv(la) + derivl(2)
                        zdrv(la) = zdrv(la) + derivl(3)
!
                        if (lstr) then
                          wrk(1:6) = wrk(1:6) + partial%sa_drhotots(1:6,np)*ofct
                        endif
                      enddo
                    endif
                  endif
!
!  Scale density and derivatives by screening factor
!
                  drhoij(1:3,1:maxmeamcomponent) = Sij*drhoij(1:3,1:maxmeamcomponent)
                  drhoji(1:3,1:maxmeamcomponent) = Sij*drhoji(1:3,1:maxmeamcomponent)
                  if (lsg1) then
                    drhoijs(1:6,1:maxmeamcomponent) = Sij*drhoijs(1:6,1:maxmeamcomponent)
                    drhojis(1:6,1:maxmeamcomponent) = Sij*drhojis(1:6,1:maxmeamcomponent)
                  endif
                  if (lgrad2) then
                    drhoij2(1:6,1:maxmeamcomponent) = Sij*drhoij2(1:6,1:maxmeamcomponent)
                    drhoji2(1:6,1:maxmeamcomponent) = Sij*drhoji2(1:6,1:maxmeamcomponent)
                    if (lsg2) then
                      drhoij2s(1:21,1:maxmeamcomponent) = Sij*drhoij2s(1:21,1:maxmeamcomponent)
                      drhoji2s(1:21,1:maxmeamcomponent) = Sij*drhoji2s(1:21,1:maxmeamcomponent)
                      drhoij2m(1:6,1:3,1:maxmeamcomponent) = Sij*drhoij2m(1:6,1:3,1:maxmeamcomponent)
                      drhoji2m(1:6,1:3,1:maxmeamcomponent) = Sij*drhoji2m(1:6,1:3,1:maxmeamcomponent)
                    endif
                  endif
!
!  Compute total derivatives of MEAM density
!
                  call meamtotalrhoderv(neamspeci,scrho(1,i),rhoi,drhoij,drhototij,drhoijs,drhototijs, &
                                        drhoij2,drhototij2,drhoij2s,drhototij2s,drhoij2m,drhototij2m, &
                                        lstr,lgrad2)
                  call meamtotalrhoderv(neamspecj,scrhofull(1,j),rhoj,drhoji,drhototji,drhojis,drhototjis, &
                                        drhoji2,drhototji2,drhoji2s,drhototji2s,drhoji2m,drhototji2m, &
                                        lstr,lgrad2)
                endif
              else
                call eamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcd,ycd,zcd,rhoij,rhoji,drhototij,drhototji, &
                            drhototijs,drhototjis,drhototij2,drhototji2,drhototij2s,drhototji2s, &
                            drhototij2m,drhototji2m,drhototij3,drhototji3,1.0_dp,1.0_dp,lsg1,.true.,lgrad2,.false.)
              endif
            endif
          enddo
!
!  Combine derivative terms
!
          deriv(1:3) = deriv(1:3) + (rscrhoi*drhototij(1:3) + rscrhoj*drhototji(1:3))*ofct
          if (lstr) then
            derivs(1:6) = derivs(1:6) + (rscrhoi*drhototijs(1:6) + rscrhoj*drhototjis(1:6))*ofct
          endif
          if (lgrad2) then
            ind = 0
            do kl = 1,3
              do km = kl,3
                ind = ind + 1
                deriv2(ind) = deriv2(ind) + (rscrhoi*drhototij2(ind) + rscrhoj*drhototji2(ind))*ofct
                deriv2(ind) = deriv2(ind) + ocj*rscrhoi3*drhototij(kl)*drhototij(km)*ofct
                deriv2(ind) = deriv2(ind) + oci*rscrhoj3*drhototji(kl)*drhototji(km)*ofct
              enddo
            enddo
            if (lstr) then
              deriv2s(1:21) = deriv2s(1:21) + ofct*rscrhoi*drhototij2s(1:21) + ofct*rscrhoj*drhototji2s(1:21)
              deriv2m(1:6,1:3) = deriv2m(1:6,1:3) + ofct*rscrhoi*drhototij2m(1:6,1:3) + ofct*rscrhoj*drhototji2m(1:6,1:3)
              ind = 0
              do kl = 1,6
                do km = 1,kl
                  ind = ind + 1
                  deriv2s(ind) = deriv2s(ind) + ocj*rscrhoi3*ofct*drhototijs(kl)*drhototijs(km)
                  deriv2s(ind) = deriv2s(ind) + oci*rscrhoj3*ofct*drhototjis(kl)*drhototjis(km)
                enddo
                deriv2m(kl,1) = deriv2m(kl,1) + ocj*rscrhoi3*ofct*drhototijs(kl)*drhototij(1)
                deriv2m(kl,2) = deriv2m(kl,2) + ocj*rscrhoi3*ofct*drhototijs(kl)*drhototij(2)
                deriv2m(kl,3) = deriv2m(kl,3) + ocj*rscrhoi3*ofct*drhototijs(kl)*drhototij(3)
                deriv2m(kl,1) = deriv2m(kl,1) + oci*rscrhoj3*ofct*drhototjis(kl)*drhototji(1)
                deriv2m(kl,2) = deriv2m(kl,2) + oci*rscrhoj3*ofct*drhototjis(kl)*drhototji(2)
                deriv2m(kl,3) = deriv2m(kl,3) + oci*rscrhoj3*ofct*drhototjis(kl)*drhototji(3)
              enddo
            endif
          endif
        endif
        if (lvalidij) then
!******************************
!  Internal first derivatives *
!******************************
          if (lMEAM) then
            xdrvnr(i) = xdrvnr(i) - deriv(1)
            ydrvnr(i) = ydrvnr(i) - deriv(2)
            zdrvnr(i) = zdrvnr(i) - deriv(3)
          else
            xdrv(i) = xdrv(i) - deriv(1)
            ydrv(i) = ydrv(i) - deriv(2)
            zdrv(i) = zdrv(i) - deriv(3)
          endif
!********************************
!  Internal second derivatives  *
!********************************
          if (lgrad2.and.j.ne.nreli) then
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
!*****************************
!  Mixed strain derivatives  *
!*****************************
          if (lsg2) then
            do kl = 1,nstrains
              derv3(ix,kl) = derv3(ix,kl) - deriv2m(kl,1)
              derv3(iy,kl) = derv3(iy,kl) - deriv2m(kl,2)
              derv3(iz,kl) = derv3(iz,kl) - deriv2m(kl,3)
            enddo
          endif
!*****************************
!  Strain first derivatives  *
!*****************************
          if (lsg1.or.lgrad2) then
            wrk(1:6) = wrk(1:6) + derivs(1:6)
!******************************
!  Strain second derivatives  *
!******************************
            if (lsg2) then
              do kl = 1,nstrains
                do km = 1,nstrains
                  if (kl.ge.km) then
                    ind = kl*(kl - 1)/2 + km
                  else
                    ind = km*(km - 1)/2 + kl
                  endif
                  elcom(km,kl) = elcom(km,kl) + deriv2s(ind)
                enddo
              enddo
            endif
          endif
        endif
        if (lgrad2) then
!******************************************************************
!  Start of third atom loop - only needed for second derivatives  *
!******************************************************************
          kloop: do k = 1,numat
!
            natk = nat(k)
            ntypk = nftype(k)
            xcrdik = xclat(k) - xal
            ycrdik = yclat(k) - yal
            zcrdik = zclat(k) - zal
            xcrdjk = xcrdik - xcd
            ycrdjk = ycrdik - ycd
            zcrdjk = zcrdik - zcd
            ock = occuf(k)
            ofctijk = ofct*ock
!
!  Check whether there are any potentials between i-k or j-k
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
!  If no valid potentials for i-k or j-k then skip
!
            if (npotijk.eq.0) cycle kloop
            cut2rk = rpijk*rpijk
            if (cut2rk.gt.cut2p) cut2rk = cut2p
            cut2k = cut2rk
!
!  Set up constants for k
!
            nrelk = nrelat(k)
!
!  Find EAM species for k
!
            neamspeck = neamfnspecptr(k)
!
!  Evaluate functional derivatives
!
            if (lMEAMfn) then
              call meamfnderv(neamfn,neamspeck,scrho(1,nrelk),rhok,eeam,rscrhok,rscrhok3,rscrhok5,.true.,lgrad2,.false.)
            else
              call eamfnderv(neamfn,neamspeck,scrho(1,nrelk),eeam,rscrhok,rscrhok3,rscrhok5,.true.,lgrad2,.false.)
              rhok = scrho(1,nrelk)
            endif
!
!  If no rho then skip
!
            if (rhoi.eq.0.0_dp.and.rhoj.eq.0.0_dp.and.rhok.eq.0.0_dp) cycle kloop
!***********************
!  General cell loops  *
!***********************
            call rfindeither(xcrdik,ycrdik,zcrdik,xcrdjk,ycrdjk,zcrdjk,cut2k,cut2k,.false.,nor,nor2)
            do jj = 1,nor2
              rik2 = dist(nor+jj)
              rjk2 = dist2(nor+jj)
              xcd1 = xtmp(nor+jj)
              ycd1 = ytmp(nor+jj)    
              zcd1 = ztmp(nor+jj)  
              xcd2 = xtmp2(nor+jj)
              ycd2 = ytmp2(nor+jj)
              zcd2 = ztmp2(nor+jj)
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
                if (lstr) then
                  drhoiks(1:6,1:maxmeamcomponent) = 0.0_dp
                  drhokis(1:6,1:maxmeamcomponent) = 0.0_dp
                  drhojks(1:6,1:maxmeamcomponent) = 0.0_dp
                  drhokjs(1:6,1:maxmeamcomponent) = 0.0_dp
                endif
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
              if (lstr) then
                drhototiks(1:6) = 0.0_dp
                drhototkis(1:6) = 0.0_dp
                drhototjks(1:6) = 0.0_dp
                drhototkjs(1:6) = 0.0_dp
                drhototijk2s(1:6,1:6) = 0.0_dp
                drhototjik2s(1:6,1:6) = 0.0_dp
                drhototkij2s(1:6,1:6) = 0.0_dp
                drhototijk2m(1:6,1:3) = 0.0_dp
                drhototjik2m(1:6,1:3) = 0.0_dp
                drhototkij2m(1:6,1:3) = 0.0_dp
              endif
!
              lanyvalidik = .false.
              lanyvalidjk = .false.
              if (rik2.le.cut2k) then
!*********************
!  i-k contribution  *
!*********************
                rik = sqrt(rik2)
!
!  Loop over potentials to find many-body ones
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
                                       1.0_dp,1.0_dp,.true.,lstr,.true.,.false.)
                          call meamtotalrhoderv(neamspeci,scrho(1,i),rhoi,drhoik,drhototik,drhoiks,drhototiks, &
                                                drhoik2,drhototik2,drhoik2s,drhototik2s,drhoik2m,drhototik2m, &
                                                lstr,.false.)
                          call meamtotalrhoderv(neamspeck,scrhofull(1,k),rhok,drhoki,drhototki,drhokis,drhototkis, &
                                                drhoki2,drhototki2,drhoki2s,drhototki2s,drhoki2m,drhototki2m, &
                                                lstr,.false.)
                          call meamtotalrhocrossderv(neamspeci,scrho(1,i),rhoi,drhoij,drhototij,drhoik,drhototik,drhoijs,drhoiks, &
                                                     drhototijk2,drhototijk2s,drhototijk2m,lstr)
                        else
                          call eamrho(nati,ntypi,natk,ntypk,rik,rpot(m),xcd1,ycd1,zcd1,rhoik,rhoki,drhototik,drhototki, &
                                      drhototiks,drhototkis,drhototik2,drhototki2,drhototik2s,drhototki2s, &
                                      drhototik2m,drhototki2m,drhototik3,drhototki3,1.0_dp,1.0_dp,lsg1,.true.,.false.,.false.)
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
!  Loop over potentials to find many-body ones
!
                do m = 1,npote
                  if (nptype(m).eq.19)  then
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
                                       1.0_dp,1.0_dp,.true.,lstr,.true.,.false.)
                          call meamtotalrhoderv(neamspecj,scrhofull(1,j),rhoj,drhojk,drhototjk,drhojks,drhototjks, &
                                                drhojk2,drhototjk2,drhojk2s,drhototjk2s,drhojk2m,drhototjk2m, &
                                                lstr,.false.)
                          call meamtotalrhoderv(neamspeck,scrhofull(1,k),rhok,drhokj,drhototkj,drhokjs,drhototkjs, &
                                                drhokj2,drhototkj2,drhokj2s,drhototkj2s,drhokj2m,drhototkj2m, &
                                                lstr,.false.)
                          call meamtotalrhocrossderv(neamspecj,scrhofull(1,j),rhoj,drhoji,drhototji,drhojk,drhototjk, &
                                                     drhojis,drhojks,drhototjik2,drhototjik2s,drhototjik2m,lstr)
                        else
                          call eamrho(natj,ntypj,natk,ntypk,rjk,rpot(m),xcd2,ycd2,zcd2,rhojk,rhokj,drhototjk,drhototkj, &
                                      drhototjks,drhototkjs,drhototjk2,drhototkj2,drhototjk2s,drhototkj2s, &
                                      drhototjk2m,drhototkj2m,drhototjk3,drhototkj3,1.0_dp,1.0_dp,lsg1,.true.,.false.,.false.)
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
                call meamtotalrhocrossderv(neamspeck,scrhofull(1,k),rhok,drhoki,drhototki,drhokj,drhototkj,drhokis,drhokjs, &
                                           drhototkij2,drhototkij2s,drhototkij2m,lstr)
              endif
!
!  Only need to do internal derivatives if i not equals j
!
              if (j.ne.nreli) then
!***************************************************
!  Calculate second derivatives for i-k/j-k terms  *
!***************************************************
!
!  i-k
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
!  j-k
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
!  i-k/j-k
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
              endif
              if (lsg2) then
!
!  Mixed internal-strain derivatives
!
                if (lMEAM) then
                  dt1 = rscrhoi3*ofctijk
                  dt2 = rscrhoj3*ofctijk
                  dt3 = rscrhoi*ofctijk
                  dt4 = rscrhoj*ofctijk
                  if (j.ne.nreli) then
                    do kl = 1,nstrains
                      derv3(ix,kl) = derv3(ix,kl) - dt1*drhototiks(kl)*drhototij(1) - dt2*drhototjks(kl)*drhototji(1)
                      derv3(iy,kl) = derv3(iy,kl) - dt1*drhototiks(kl)*drhototij(2) - dt2*drhototjks(kl)*drhototji(2)
                      derv3(iz,kl) = derv3(iz,kl) - dt1*drhototiks(kl)*drhototij(3) - dt2*drhototjks(kl)*drhototji(3)
!
                      derv3(ix,kl) = derv3(ix,kl) - dt3*drhototijk2m(kl,1) - dt4*drhototjik2m(kl,1)
                      derv3(iy,kl) = derv3(iy,kl) - dt3*drhototijk2m(kl,2) - dt4*drhototjik2m(kl,2)
                      derv3(iz,kl) = derv3(iz,kl) - dt3*drhototijk2m(kl,3) - dt4*drhototjik2m(kl,3)
                    enddo
                  endif
                else
                  dt1 = rscrhoi3*ofctijk
                  dt2 = rscrhoj3*ofctijk
                  if (j.ne.nreli) then
                    do kl = 1,nstrains
                      derv3(ix,kl) = derv3(ix,kl) - dt1*drhototiks(kl)*drhototij(1) - dt2*drhototjks(kl)*drhototji(1)
                      derv3(iy,kl) = derv3(iy,kl) - dt1*drhototiks(kl)*drhototij(2) - dt2*drhototjks(kl)*drhototji(2)
                      derv3(iz,kl) = derv3(iz,kl) - dt1*drhototiks(kl)*drhototij(3) - dt2*drhototjks(kl)*drhototji(3)
                    enddo
                  endif
                endif
!
!  Strain-strain second derivatives
!
                if (lMEAM) then
                  do kl = 1,nstrains
                    do km = 1,nstrains
                      elcom(km,kl) = elcom(km,kl) + 0.5_dp*(dt1*drhototiks(km)*drhototijs(kl) + dt2*drhototjks(km)*drhototjis(kl))
                      elcom(km,kl) = elcom(km,kl) + 0.5_dp*(dt1*drhototiks(kl)*drhototijs(km) + dt2*drhototjks(kl)*drhototjis(km))
                      elcom(km,kl) = elcom(km,kl) + 0.5_dp*(dt3*drhototijk2s(km,kl) + dt4*drhototjik2s(km,kl))
                      elcom(km,kl) = elcom(km,kl) + 0.5_dp*(dt3*drhototijk2s(kl,km) + dt4*drhototjik2s(kl,km))
                    enddo
                  enddo
                else
                  do kl = 1,nstrains
                    do km = 1,nstrains
                      elcom(km,kl) = elcom(km,kl) + 0.5_dp*(dt1*drhototiks(km)*drhototijs(kl) + dt2*drhototjks(km)*drhototjis(kl))
                      elcom(km,kl) = elcom(km,kl) + 0.5_dp*(dt1*drhototiks(kl)*drhototijs(km) + dt2*drhototjks(kl)*drhototjis(km))
                    enddo
                  enddo
                endif
              endif
!******************************
!  End of second derivatives  *
!******************************
            enddo
!***************************
!  End of third atom loop  *
!***************************
          enddo kloop
        endif
!**************************************
!  End of valid distance i-j section  *
!**************************************
      enddo
    enddo jloop
!
!  Skip to here if i is frozen
!
  enddo iloop
!
!  Double counting correction
!
  if (.not.lfreeze) then
    if (lsg1) then
      if (lMEAM) then
        do i = 1,6
          rstrdnr(i) = rstrdnr(i) + 0.5_dp*wrk(i)
        enddo
      else
        do i = 1,6
          rstrd(i) = rstrd(i) + 0.5_dp*wrk(i)
        enddo
      endif
    endif
    if (lsg2) then
      do i = 1,6
        do j = 1,i
          sderv2(i,j) = sderv2(i,j) + 0.5_dp*elcom(i,j)
        enddo
      enddo
    endif
  else
    if (lsg1) then
      if (lMEAM) then
        do i = 1,6
          rstrdnr(i) = rstrdnr(i) + wrk(i)
        enddo
      else
        do i = 1,6
          rstrd(i) = rstrd(i) + wrk(i)
        enddo
      endif
    endif
    if (lsg2) then
      do i = 1,6
        do j = 1,i
          sderv2(i,j) = sderv2(i,j) + elcom(i,j)
        enddo
      enddo
    endif
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(scrhofull,stat=status)
  if (status/=0) call deallocate_error('manysd2','scrhofull')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('manysd2','npotl')
!
!  Exit point 
!
1000 continue
!
!  Unscale density
!
  call eamscalescrho(-1_i4)
!
!  Timing
!
  time2 = cputime()
  tmany = tmany + time2 - time1
!
  return
  end
