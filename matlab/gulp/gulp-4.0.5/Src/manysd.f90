  subroutine manysd(emany,lgrad1)
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
!   3/99 Created from manysd2.f
!   3/99 Parallel modifications added
!  10/99 Cubic density function added
!   6/00 nadd default values reduced to save CPU time as they were
!        over cautious and the many-body densities decay rapidly
!        with distance.
!  11/02 Parallel changes made
!   3/03 Faster search algorithm included
!  11/03 ndennat/ndentyp replaced
!  11/03 Alloy scaling added
!   7/05 Constant scaling added to sqrt and power functionals
!   7/05 Use of EAM species pointers introduced
!   9/05 rhoderv called to get density derivatives
!   9/05 Call to eamfnderv used to replace EAM function derivative code
!  10/05 Call to eamfnderv added for energy
!   4/06 Modified to handle species specific densities
!   3/07 Printing of EAM densities and energies added as an option
!   3/07 Calculation of emany parallelised
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
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 Skipping when density is zero modified to ensure no terms are missed
!   3/09 Sign of return arguments from EAM/MEAM function routines corrected for
!   4/09 MEAM screening function derivatives added
!   5/09 MEAM third derivative arguments removed
!  10/09 Potential integer overflow trapped
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
!  Julian Gale, NRI, Curtin University, October 2009
!
  use control
  use current
  use datatypes,      only : i4_limit
  use derivatives
  use eam
  use general
  use iochannels,     only : ioout
  use optimisation
  use parallel
  use realvectors,    only : dist, xtmp, ytmp, ztmp
  use sutton
  use symmetry
  use times
  use two
  use vectors,        only : vector_pair
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lgrad1
  real(dp),    intent(out)                     :: emany
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifree
  integer(i4)                                  :: ii
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: ka
  integer(i4)                                  :: kvec
  integer(i4)                                  :: m
  integer(i4)                                  :: n  
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: neamspeci
  integer(i4)                                  :: neamspecj
  integer(i4)                                  :: neqi
  integer(i4)                                  :: nfree
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4)                                  :: nout
  integer(i4)                                  :: nouterloop
  integer(i4)                                  :: np
  integer(i4)                                  :: npartial
  integer(i4)                                  :: npot
  integer(i4)                                  :: npots
  integer(i4)                                  :: nrelj
  integer(i4)                                  :: ntyp1  
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj    
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4), dimension(:), allocatable       :: nptr
  integer(i4)                                  :: nvec
  integer(i4)                                  :: nvec0
  integer(i4)                                  :: status
  logical                                      :: lnonzeroSij
  logical                                      :: lpartial
  logical                                      :: lself 
  logical                                      :: lsg1 
  logical                                      :: lvalidij
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2r
  real(dp)                                     :: deriv(3)
  real(dp)                                     :: derivk(3)
  real(dp)                                     :: derivs(6)
  real(dp)                                     :: drhoij(3,maxmeamcomponent)
  real(dp)                                     :: drhoijs(6,maxmeamcomponent)
  real(dp)                                     :: drhoij2(6,maxmeamcomponent)
  real(dp)                                     :: drhoij2s(21,maxmeamcomponent)
  real(dp)                                     :: drhoij2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhoji(3,maxmeamcomponent)
  real(dp)                                     :: drhojis(6,maxmeamcomponent)
  real(dp)                                     :: drhoji2(6,maxmeamcomponent)
  real(dp)                                     :: drhoji2s(21,maxmeamcomponent)
  real(dp)                                     :: drhoji2m(6,3,maxmeamcomponent)
  real(dp)                                     :: drhototij(3)
  real(dp)                                     :: drhototijs(6)
  real(dp)                                     :: drhototij2(6)
  real(dp)                                     :: drhototij2s(21)
  real(dp)                                     :: drhototij2m(6,3)
  real(dp)                                     :: drhototij3(10)
  real(dp)                                     :: drhototji(3)
  real(dp)                                     :: drhototjis(6)
  real(dp)                                     :: drhototji2(6)
  real(dp)                                     :: drhototji2s(21)
  real(dp)                                     :: drhototji2m(6,3)
  real(dp)                                     :: drhototji3(10)
  real(dp)                                     :: eeam
  real(dp)                                     :: emanytrm
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: r
  real(dp)                                     :: r2
  real(dp)                                     :: rcut2
  real(dp)                                     :: rcutfactor
  real(dp)                                     :: rhoi
  real(dp)                                     :: rhoj
  real(dp)                                     :: rhoij(maxmeamcomponent)
  real(dp)                                     :: rhoji(maxmeamcomponent)
  real(dp)                                     :: rp   
  real(dp)                                     :: rscrhoi
  real(dp)                                     :: rscrhoi3
  real(dp)                                     :: rscrhoi5
  real(dp)                                     :: rscrhoj
  real(dp)                                     :: rscrhoj3
  real(dp)                                     :: rscrhoj5
  real(dp)                                     :: scmax
  real(dp),    dimension(:,:), allocatable     :: scrhofull
  real(dp)                                     :: Sij
  real(dp)                                     :: Sikj
  real(dp)                                     :: dSikjdr(3)
  real(dp)                                     :: strfct
  real(dp)                                     :: time1
  real(dp)                                     :: time2  
  real(dp)                                     :: xal 
  real(dp)                                     :: yal    
  real(dp)                                     :: zal
  real(dp)                                     :: xcd 
  real(dp)                                     :: ycd    
  real(dp)                                     :: zcd
  real(dp)                                     :: xcrd 
  real(dp)                                     :: ycrd    
  real(dp)                                     :: zcrd
  real(dp)                                     :: xik0
  real(dp)                                     :: yik0
  real(dp)                                     :: zik0
  real(dp)                                     :: xjk0
  real(dp)                                     :: yjk0
  real(dp)                                     :: zjk0
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
    call mpbarrier
    if (ioproc) then
!
!  Openning banner for energy decomposition
!
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  EAM : Atom No.                Density                 Atom energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  Energy calculation
!
  emany = 0.0_dp
  do i = procid+1,nasym,nprocs
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
    call mpbarrier
    if (ioproc) then
!     
!  Closing banner for energy decomposition
!
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  If no forces are needed then we don't need to do loops over atoms, so just return
!
  if (.not.lgrad1) goto 1000
!
!  From here on we can assume that lgrad1  =  .true.
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('manysd','npotl')
  allocate(nptr(nasym),stat=status)
  if (status/=0) call outofmemory('manysd','nptr')
  allocate(scrhofull(maxmeamcomponent,numat),stat=status)
  if (status/=0) call outofmemory('manysd','scrhofull')
!
!  Initialise local variables
!
  lsg1 = lstr
!
!  Double counting correction
!
  if (.not.lfreeze.and.lsg1) then
    strfct = 0.5_dp
  else
    strfct = 1.0_dp
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
!  Set up cutoffs
!
  cut2p = cutp*cutp
  if (lnoreal) goto 999
!
!  Generate pointer to non - frozen atoms
!
  if (lfreeze) then
    nfree = 0
    do i = 1,nasym
      if (lopf(i).and.scrho(1,i).ne.0.0_dp) then
        nfree = nfree + 1
        nptr(nfree) = i
      endif
    enddo
  else
    nfree = 0
    do i = 1,nasym
      if (scrho(1,i).ne.0.0_dp) then
        nfree = nfree + 1
        nptr(nfree) = i
      endif
    enddo
  endif
!
!  If this is MEAM then we need to generate the density components for all atoms in the unit cell
!  since the components are vector quantities and not scalars.
!
  call meamsymdensity(scrho,scrhofull)
!
!  Outer loop over sites
!
!  Combine i/j loops into one for improved parallel efficiency
!
  if (nfree.gt.i4_limit.and.numat.gt.i4_limit) then
    call outerror('integer overflow in manysd - change i4 to i8',0_i4)
    call stopnow('manysd')
  endif
  nouterloop = nfree*numat
  ijloop: do nout  =  procid + 1,nouterloop,nprocs
    ifree  =  ((nout - 1)/numat) + 1
    i  =  nptr(ifree)
!
!  Inner loop over second site
!  Skip if scrho  =  0
!
!  Find EAM species for i
!  
    neamspeci = neamfnspecptr(nrel2(i))
!
!  Evaluate functional derivatives
!
    if (lMEAMfn) then
      call meamfnderv(neamfn,neamspeci,scrho(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.true.,.false.,.false.)
    else
      call eamfnderv(neamfn,neamspeci,scrho(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.true.,.false.,.false.)
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
    j = nout - (ifree - 1)*numat
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
    ocj = occuf(j)
!
!  If no rho then skip
!
    nrelj = nrelat(j)
!
!  Find EAM species for i
!
    neamspecj = neamfnspecptr(j)
!
!  Evaluate functional derivatives
!
    if (lMEAMfn) then
      call meamfnderv(neamfn,neamspecj,scrho(1,nrelj),rhoj,eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,.false.,.false.)
    else
      call eamfnderv(neamfn,neamspecj,scrho(1,nrelj),eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,.false.,.false.)
      rhoj = scrho(1,nrelj)
    endif
!
!  If there is no density at either centre then cycle
!
    if (rhoi.eq.0.0_dp.and.rhoj.eq.0.0_dp) cycle ijloop
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
    if (npots.eq.0) cycle ijloop
    cut2r = rp*rp
    if (cut2r.gt.cut2p) cut2r = cut2p
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
      if (lsg1) derivs(1:6) = 0.0_dp
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
        if (lsg1) then
          drhoijs(1:6,1:maxmeamcomponent) = 0.0_dp
          drhojis(1:6,1:maxmeamcomponent) = 0.0_dp
        endif
      else
        rhoij(1) = 0.0_dp
        rhoji(1) = 0.0_dp
      endif
      drhototij(1:3) = 0.0_dp
      drhototji(1:3) = 0.0_dp
      if (lsg1) then
        drhototijs(1:6) = 0.0_dp
        drhototjis(1:6) = 0.0_dp
      endif
!
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
                           1.0_dp,1.0_dp,.true.,lsg1,.true.,.false.)
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
                k = 0
                npartial = 0
                do while (k.lt.numat.and.lnonzeroSij)
                  k = k + 1
!
!  Set basic vectors between atoms
!
                  xik0 = xclat(k) - xal
                  yik0 = yclat(k) - yal
                  zik0 = zclat(k) - zal
                  xjk0 = xik0 - xcd
                  yjk0 = yik0 - ycd
                  zjk0 = zik0 - zcd
!
!  Find images within cutoffs of both atoms - excluding self images
!
                  nvec0 = 0
                  call rfindmid(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,.false.,nvec0,nvec,vectorpair)
!
!  Loop over results of search
!
                  kvec = 0
                  do while (kvec.lt.nvec.and.lnonzeroSij)
                    kvec = kvec + 1
!
!  Compute screening function
!
                    call meamscreen(r2,vectorpair%distance_pair1(kvec),vectorpair%distance_pair2(kvec),Sikj, &
                                    dSikjdr,lpartial,.true.)
!
!  If screening function contribution is 0, then no need to continue for this pair
!
                    if (Sikj.eq.0.0_dp) then
                      lnonzeroSij = .false.
                      Sij = 0.0_dp
                    else
!
!  Multiply total screening product
!
                      Sij = Sij*Sikj
                      if (lpartial) then
!
!  If this atom has a screening factor between 0 and 1, we need to keep track of it since it will generate non-zero derivatives
!
                        npartial = npartial + 1
                        if (npartial.gt.partial%sa_maxdim) then
                          call changemaxsa(partial,npartial)
                        endif
                        partial%sa_atom(npartial) = k
                        partial%sa_xik(npartial) = vectorpair%xvector_pair1(kvec)
                        partial%sa_yik(npartial) = vectorpair%yvector_pair1(kvec)
                        partial%sa_zik(npartial) = vectorpair%zvector_pair1(kvec)
                        partial%sa_xjk(npartial) = vectorpair%xvector_pair2(kvec)
                        partial%sa_yjk(npartial) = vectorpair%yvector_pair2(kvec)
                        partial%sa_zjk(npartial) = vectorpair%zvector_pair2(kvec)
                        partial%sa_Sikj(npartial) = Sikj
                        partial%sa_dSikjdr(1:3,npartial) = dSikjdr(1:3)
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
                      k = partial%sa_atom(np)
                      ka = nrelat(k)
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
                      derivk(1) = partial%sa_drhototik(1,np)*ofct
                      derivk(2) = partial%sa_drhototik(2,np)*ofct
                      derivk(3) = partial%sa_drhototik(3,np)*ofct
                      call symdervrot(k,nrel2(ka),derivk)
                      xdrv(ka) = xdrv(ka) + derivk(1)
                      ydrv(ka) = ydrv(ka) + derivk(2)
                      zdrv(ka) = zdrv(ka) + derivk(3)
!
                      if (lstr) then
                        rstrd(1:6) = rstrd(1:6) + partial%sa_drhotots(1:6,np)*ofct*strfct
                      endif
                    enddo

                    call meamtotalrhoscreenderv(neamspecj,npartial,partial,xcd,ycd,zcd,scrhofull(1,j), &
                                                rhoj,rscrhoj,rhoji,Sij,lsg1)
!
                    do np = 1,npartial
                      k = partial%sa_atom(np)
                      ka = nrelat(k)
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
                      derivk(1) = partial%sa_drhototik(1,np)*ofct
                      derivk(2) = partial%sa_drhototik(2,np)*ofct
                      derivk(3) = partial%sa_drhototik(3,np)*ofct
                      call symdervrot(k,nrel2(ka),derivk)
                      xdrv(ka) = xdrv(ka) + derivk(1)
                      ydrv(ka) = ydrv(ka) + derivk(2)
                      zdrv(ka) = zdrv(ka) + derivk(3)
!
                      if (lstr) then
                        rstrd(1:6) = rstrd(1:6) + partial%sa_drhotots(1:6,np)*ofct*strfct
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
!
!  Compute total derivatives of MEAM density
!
                call meamtotalrhoderv(neamspeci,scrho(1,i),rhoi,drhoij,drhototij,drhoijs,drhototijs, &
                                      drhoij2,drhototij2,drhoij2s,drhototij2s,drhoij2m,drhototij2m, &
                                      lsg1,.false.)
                call meamtotalrhoderv(neamspecj,scrhofull(1,j),rhoj,drhoji,drhototji,drhojis,drhototjis, &
                                      drhoji2,drhototji2,drhoji2s,drhototji2s,drhoji2m,drhototji2m, &
                                      lsg1,.false.)
              endif
            else
              call eamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcd,ycd,zcd,rhoij,rhoji,drhototij,drhototji, &
                          drhototijs,drhototjis,drhototij2,drhototji2,drhototij2s,drhototji2s, &
                          drhototij2m,drhototji2m,drhototij3,drhototji3,1.0_dp,1.0_dp,lsg1,.true.,.false.,.false.)
            endif
          endif
        enddo
!
!  Combine derivative terms
!
        deriv(1:3) = deriv(1:3) + (rscrhoi*drhototij(1:3) + rscrhoj*drhototji(1:3))*ofct
        if (lsg1) then
          derivs(1:6) = derivs(1:6) + (rscrhoi*drhototijs(1:6) + rscrhoj*drhototjis(1:6))*ofct
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
!*****************************
!  Strain first derivatives  *
!*****************************
        if (lsg1) then
          if (lMEAM) then
            rstrdnr(1:6) = rstrdnr(1:6) + strfct*derivs(1:6)
          else
            rstrd(1:6) = rstrd(1:6) + strfct*derivs(1:6)
          endif
        endif
      endif
!**************************************
!  End of valid distance i-j section  *
!**************************************
    enddo
!
!  Skip to here if i is frozen
!
  enddo ijloop
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(scrhofull,stat=status)
  if (status/=0) call deallocate_error('manysd','scrhofull')
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('manysd','nptr')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('manysd','npotl')
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
