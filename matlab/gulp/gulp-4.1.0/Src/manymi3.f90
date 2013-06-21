  subroutine manymi3(emany,esregion12,esregion2,eattach,lgrad1)
!
!  Subroutine for calculating the many-body energy from the
!  Sutton-Chen potential(s). Requires real space routine to
!  be called first to evaluate the repulsive contribution
!  and the rho parameters.
!
!  Minimum image version.
!
!  On entry the array scrho must contain the density at each atomic site.
!
!  lfreeze = if .true. only do derivatives for atoms with a degree of
!            freedom
!
!   4/01 Created from manymd3 / realmi3
!   9/01 Marvin compatibility option for SE added
!  11/01 Attachment energy added
!   8/02 Surface energy calculation algorithm changed
!  11/02 Parallel changes made
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
!   5/07 QM/MM schemes added
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
!   3/09 Skipping when density is zero modified to ensure no terms are missed
!   3/09 small replaced by global value smallself from general module
!   3/09 Sign of return arguments from EAM/MEAM function routines corrected for
!   4/09 MEAM screening function derivatives added
!   5/09 MEAM third derivative arguments removed
!  11/09 Region derivatives added
!   3/10 Extra virial contributions added
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stresses added
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : nregions, nregionno, nregiontype, QMMMmode
  use control
  use current
  use derivatives
  use eam
  use general
  use iochannels,     only : ioout
  use mdlogic
  use numbers,        only : third
  use optimisation
  use parallel
  use sutton
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lgrad1
  real(dp),    intent(inout)                   :: eattach
  real(dp),    intent(out)                     :: emany  
  real(dp),    intent(inout)                   :: esregion12
  real(dp),    intent(inout)                   :: esregion2
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: iimx
  integer(i4)                                  :: iimn
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjmx
  integer(i4)                                  :: jjmn
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: kkmx
  integer(i4)                                  :: kkmn
  integer(i4)                                  :: kl
  integer(i4)                                  :: klp
  integer(i4)                                  :: m
  integer(i4)                                  :: mj
  integer(i4)                                  :: n  
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: neamspeci
  integer(i4)                                  :: neamspecj
  integer(i4)                                  :: noff
  integer(i4)                                  :: noffm1
  integer(i4)                                  :: noffset
  integer(i4)                                  :: np
  integer(i4)                                  :: npartial
  integer(i4)                                  :: npot
  integer(i4)                                  :: npots
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: ntyp1  
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj    
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: status
  logical                                      :: lnonzeroSij
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lopk
  logical                                      :: lpartial
  logical                                      :: lQMMMok
  logical                                      :: lreg2ok
  logical                                      :: lsg1 
  logical                                      :: lvalidij
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2r
  real(dp)                                     :: deltarho(maxmeamcomponent)
  real(dp)                                     :: deriv(3)
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
  real(dp)                                     :: r2ik
  real(dp)                                     :: r2jk
  real(dp)                                     :: rcut2
  real(dp)                                     :: rcutfactor
  real(dp)                                     :: r2min
  real(dp)                                     :: rhoi
  real(dp)                                     :: rhoj
  real(dp)                                     :: rhoij(maxmeamcomponent)
  real(dp)                                     :: rhoji(maxmeamcomponent)
  real(dp)                                     :: rhorr  
  real(dp)                                     :: rp   
  real(dp)                                     :: rscrhoi
  real(dp)                                     :: rscrhoi3
  real(dp)                                     :: rscrhoi5
  real(dp)                                     :: rscrhoj
  real(dp)                                     :: rscrhoj3
  real(dp)                                     :: rscrhoj5
  real(dp)                                     :: scmax
  real(dp)                                     :: Sij
  real(dp)                                     :: Sikj
  real(dp)                                     :: dSikjdr(3)
  real(dp)                                     :: time1
  real(dp)                                     :: time2  
  real(dp)                                     :: xal 
  real(dp)                                     :: yal    
  real(dp)                                     :: zal
  real(dp)                                     :: xcd 
  real(dp)                                     :: ycd    
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi   
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcdj
  real(dp)                                     :: ycdj   
  real(dp)                                     :: zcdj
  real(dp)                                     :: xcrd 
  real(dp)                                     :: ycrd    
  real(dp)                                     :: zcrd
  real(dp)                                     :: xmin 
  real(dp)                                     :: ymin    
  real(dp)                                     :: zmin
  real(dp)                                     :: xiji
  real(dp)                                     :: yiji
  real(dp)                                     :: ziji
  real(dp)                                     :: xijj
  real(dp)                                     :: yijj
  real(dp)                                     :: zijj
  real(dp)                                     :: xijk
  real(dp)                                     :: yijk
  real(dp)                                     :: zijk
  real(dp)                                     :: xij0
  real(dp)                                     :: yij0
  real(dp)                                     :: zij0
  real(dp)                                     :: xik0
  real(dp)                                     :: yik0
  real(dp)                                     :: zik0
  real(dp)                                     :: xjk0
  real(dp)                                     :: yjk0
  real(dp)                                     :: zjk0
  type(screening_atoms)                        :: partial
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
  lreg2ok = (lseok.and.nregions(ncf).gt.1)
  do i = procid+1,numat,nprocs
    neamspeci = neamfnspecptr(i)
    rhoi = scrho(1,i)
    nregioni = nregionno(nsft+nrelat(i))
    nregiontypi = nregiontype(nregioni,ncf)
!
!  QM/MM handling : i is a QM atom => exclude
!
    lQMMMok = .true.
    if (QMMMmode(ncf).gt.0) then
      if (nregiontypi.eq.1) lQMMMok = .false.
    endif
    if (neamspeci.gt.0.and.rhoi.gt.1.0d-12.and.lQMMMok) then
      if (lMEAMfn) then
        call meamfnderv(neamfn,neamspeci,scrho(1,i),rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
      else
        call eamfnderv(neamfn,neamspeci,scrho(1,i),eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
      endif
      emanytrm = occuf(i)*eeam
      if (lreg2ok.and.nregionno(nsft+nrelat(i)).gt.1) then
        esregion2 = esregion2 + emanytrm
      else
        emany = emany + emanytrm
      endif
      if (lPrintEAM) then                      
        write(ioout,'(7x,i8,4x,f24.10,4x,f24.12)') i,rhoi,emanytrm
      endif
      eattach = eattach + emanytrm
      if (lMEAMfn) then
        deltarho(1:maxmeamcomponent) = scrho(1:maxmeamcomponent,i) - scrho12(1:maxmeamcomponent,i)
        call meamfnderv(neamfn,neamspeci,deltarho,rhoi,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
      else
        rhorr = rhoi - scrho12(1,i)
        call eamfnderv(neamfn,neamspeci,rhorr,eeam,rscrhoi,rscrhoi3,rscrhoi5,.false.,.false.,.false.)
      endif
      emanytrm = occuf(i)*eeam
      eattach = eattach - emanytrm
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
!  From here on we can assume that lgrad1 = .true.
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('manymi3','npotl')
!
!  Initialise local variables
!
  lsg1 = (lstr.or.lmd)
!***************************
!  Set up local variables  *
!***************************
  if (ndim.eq.3) then
    iimn = -1
    iimx =  1
    jjmn = -1
    jjmx =  1
    kkmn = -1
    kkmx =  1
  elseif (ndim.eq.2) then
    iimn = -1
    iimx =  1
    jjmn = -1
    jjmx =  1
    kkmn =  0
    kkmx =  0
  elseif (ndim.eq.1) then
    iimn = -1
    iimx =  1
    jjmn =  0
    jjmx =  0
    kkmn =  0
    kkmx =  0
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
!  Outer loop over sites
!
!  Use Brode-Ahlrichs Algorithm
!  modified to include i=j
  noff = numat/2
  noffset = noff
  if (mod(numat,2_i4).eq.0) then
    noffm1  =  noff - 1
  else
    noffm1  =  noff
  endif
  iloop: do i  =  procid+1,numat,nprocs
    if (i.gt.noff) then
      noffset  =  noffm1
    endif
    lopi = (.not.lfreeze.or.lopf(nrelat(i)))
!     
!  Find EAM species for i
!  
    neamspeci = neamfnspecptr(i)
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
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    ntypi = nftype(i)
    nregioni = nregionno(nsft+nrelat(i))
    nregiontypi = nregiontype(nregioni,ncf)
    oci = occuf(i)
!
!  Start of second atom loop
!
    jloop: do mj  =  0,noffset
      j = mod(i+mj-1_i4,numat)+1
      lopj = (.not.lfreeze.or.lopf(nrelat(j)))
      if (.not.lopi.and..not.lopj) cycle jloop
!     
!  Find EAM species for j
!  
      neamspecj = neamfnspecptr(j)
!
!  Evaluate functional derivatives
!
      if (lMEAMfn) then
        call meamfnderv(neamfn,neamspecj,scrho(1,j),rhoj,eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,.false.,.false.)
      else
        call eamfnderv(neamfn,neamspecj,scrho(1,j),eeam,rscrhoj,rscrhoj3,rscrhoj5,.true.,.false.,.false.)
        rhoj = scrho(1,j)
      endif
!
      if (rhoi.eq.0.0_dp.and.rhoj.eq.0.0_dp) cycle jloop
!
      natj = nat(j)
      ntypj = nftype(j)
      nregionj = nregionno(nsft+nrelat(j))
      nregiontypj = nregiontype(nregionj,ncf)
!
!  QM/MM handling : i & j are both QM atoms => no forces to compute
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop
      endif
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
        ntyp2 = nftype(j)
      else
        nat1 = nat(j)
        nat2 = nati
        ntyp1 = nftype(j)
        ntyp2 = ntypi
      endif
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      ocj = occuf(j)
      if (i.eq.j) then
        ofct = 0.5_dp*oci*ocj
      else
        ofct = oci*ocj
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!  Calculate sum of all dispersion terms for pair
!
      npots = 0
      rp = 0.0_dp
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
      if (npots.eq.0) cycle jloop
      cut2r = rp*rp
      if (cut2r.gt.cut2p) cut2r = cut2p
      cut2 = cut2r
      rp = sqrt(cut2)
!***********************
!  Find minimum image  *
!***********************
      if (lra) then
        if (ndim.eq.3) then
          xcrd  =  xcrd - r1x*dnint(xcrd/r1x)
          ycrd  =  ycrd - r2y*dnint(ycrd/r2y)
          zcrd  =  zcrd - r3z*dnint(zcrd/r3z)
        elseif (ndim.eq.2) then
          xcrd  =  xcrd - r1x*dnint(xcrd/r1x)
          ycrd  =  ycrd - r2y*dnint(ycrd/r2y)
        elseif (ndim.eq.1) then
          xcrd  =  xcrd - r1x*dnint(xcrd/r1x)
        endif
        r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      else
        r2min = 1.0d12
        xcdi = xcrd + (iimn-1)*r1x
        ycdi = ycrd + (iimn-1)*r1y
        zcdi = zcrd + (iimn-1)*r1z
        do ii = iimn,iimx
          xcdi = xcdi + r1x
          ycdi = ycdi + r1y
          zcdi = zcdi + r1z
          xcdj = xcdi + (jjmn-1)*r2x
          ycdj = ycdi + (jjmn-1)*r2y
          zcdj = zcdi + (jjmn-1)*r2z
          do jj = jjmn,jjmx
            xcdj = xcdj + r2x
            ycdj = ycdj + r2y
            zcdj = zcdj + r2z
            xcd = xcdj + (kkmn-1)*r3x
            ycd = ycdj + (kkmn-1)*r3y
            zcd = zcdj + (kkmn-1)*r3z
            do kk = kkmn,kkmx
              xcd = xcd + r3x
              ycd = ycd + r3y
              zcd = zcd + r3z
              r2 = xcd*xcd + ycd*ycd + zcd*zcd
              if (r2.lt.r2min) then
                r2min = r2
                xmin = xcd
                ymin = ycd
                zmin = zcd
              endif
            enddo
          enddo
        enddo
        r2 = r2min
        xcrd = xmin
        ycrd = ymin
        zcrd = zmin
      endif
      if (r2.gt.smallself.and.r2.le.cut2) then
!***************************************************
!  Calculate many-body contribution in real space  *
!***************************************************
        deriv(1:3) = 0.0_dp
        if (lsg1) derivs(1:6) = 0.0_dp
        r = sqrt(r2)
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
                call meamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcrd,ycrd,zcrd,rhoij,rhoji,drhoij,drhoji, &
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
                    xij0 = 0.5_dp*(xik0 + xjk0)
                    yij0 = 0.5_dp*(yik0 + yjk0)
                    zij0 = 0.5_dp*(zik0 + zjk0)
!***************************
!  Find minimum image of k *
!***************************
                    if (lra) then
                      if (ndim.eq.3) then
                        xij0  =  xij0 - r1x*dnint(xcrd/r1x)
                        yij0  =  yij0 - r2y*dnint(ycrd/r2y)
                        zij0  =  zij0 - r3z*dnint(zcrd/r3z)
                      elseif (ndim.eq.2) then
                        xij0  =  xij0 - r1x*dnint(xcrd/r1x)
                        yij0  =  yij0 - r2y*dnint(ycrd/r2y)
                      elseif (ndim.eq.1) then
                        xij0  =  xij0 - r1x*dnint(xcrd/r1x)
                      endif
                      r2 = xij0*xij0 + yij0*yij0 + zij0*zij0
                    else
                      r2min = 1.0d12
                      xiji = xij0 + (iimn-1)*r1x
                      yiji = yij0 + (iimn-1)*r1y
                      ziji = zij0 + (iimn-1)*r1z
                      do ii = iimn,iimx
                        xiji = xiji + r1x
                        yiji = yiji + r1y
                        ziji = ziji + r1z
                        xijj = xiji + (jjmn-1)*r2x
                        yijj = yiji + (jjmn-1)*r2y
                        zijj = ziji + (jjmn-1)*r2z
                        do jj = jjmn,jjmx
                          xijj = xijj + r2x
                          yijj = yijj + r2y
                          zijj = zijj + r2z
                          xijk = xijj + (kkmn-1)*r3x
                          yijk = yijj + (kkmn-1)*r3y
                          zijk = zijj + (kkmn-1)*r3z
                          do kk = kkmn,kkmx
                            xijk = xijk + r3x
                            yijk = yijk + r3y
                            zijk = zijk + r3z
                            r2 = xijk*xijk + yijk*yijk + zijk*zijk
                            if (r2.lt.r2min) then
                              r2min = r2
                              xmin = xijk
                              ymin = yijk
                              zmin = zijk
                            endif
                          enddo
                        enddo
                      enddo
                      r2 = r2min
                      xij0 = xmin
                      yij0 = ymin
                      zij0 = zmin
                    endif
                    if (r2.lt.rcut2) then
!
!  Compute remaining vectors and distances
!
                      xik0 = xij0 + 0.5_dp*xcrd
                      yik0 = yij0 + 0.5_dp*ycrd
                      zik0 = zij0 + 0.5_dp*zcrd
                      xjk0 = xij0 - 0.5_dp*xcrd
                      yjk0 = yij0 - 0.5_dp*ycrd
                      zjk0 = zij0 - 0.5_dp*zcrd
!
                      r2ik = xik0*xik0 + yik0*yik0 + zik0*zik0
                      r2jk = xjk0*xjk0 + yjk0*yjk0 + zjk0*zjk0
!
!  Compute screening function
!
                      call meamscreen(r2,r2ik,r2jk,Sikj,dSikjdr,lpartial,.true.,.false.)
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
                          partial%sa_xik(npartial) = xik0
                          partial%sa_yik(npartial) = yik0
                          partial%sa_zik(npartial) = zik0
                          partial%sa_xjk(npartial) = xjk0
                          partial%sa_yjk(npartial) = yjk0
                          partial%sa_zjk(npartial) = zjk0
                          partial%sa_Sikj(npartial) = Sikj
                          partial%sa_dSikjdr(1:3,npartial) = dSikjdr(1:3)
                        endif
                      endif
                    endif
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
                        lopk = (.not.lfreeze.or.lopf(nrelat(k)))
                        nregionk = nregionno(nsft+nrelat(k))
!
!  i-j contribution
!
                        if (lopi) then
                          xdrv(i) = xdrv(i) - partial%sa_drhototij(1,np)*ofct
                          ydrv(i) = ydrv(i) - partial%sa_drhototij(2,np)*ofct
                          zdrv(i) = zdrv(i) - partial%sa_drhototij(3,np)*ofct
                        endif
                        if (lopj) then
                          xdrv(j) = xdrv(j) + partial%sa_drhototij(1,np)*ofct
                          ydrv(j) = ydrv(j) + partial%sa_drhototij(2,np)*ofct
                          zdrv(j) = zdrv(j) + partial%sa_drhototij(3,np)*ofct
                        endif
                        if (nregioni.ne.nregionj) then
                          xregdrv(nregioni) = xregdrv(nregioni) - partial%sa_drhototij(1,np)*ofct
                          yregdrv(nregioni) = yregdrv(nregioni) - partial%sa_drhototij(2,np)*ofct
                          zregdrv(nregioni) = zregdrv(nregioni) - partial%sa_drhototij(3,np)*ofct
                          xregdrv(nregionj) = xregdrv(nregionj) + partial%sa_drhototij(1,np)*ofct
                          yregdrv(nregionj) = yregdrv(nregionj) + partial%sa_drhototij(2,np)*ofct
                          zregdrv(nregionj) = zregdrv(nregionj) + partial%sa_drhototij(3,np)*ofct
                        endif
!
!  i-k contribution
!
                        if (lopi) then
                          xdrv(i) = xdrv(i) - partial%sa_drhototik(1,np)*ofct
                          ydrv(i) = ydrv(i) - partial%sa_drhototik(2,np)*ofct
                          zdrv(i) = zdrv(i) - partial%sa_drhototik(3,np)*ofct
                        endif
                        if (lopk) then
                          xdrv(k) = xdrv(k) + partial%sa_drhototik(1,np)*ofct
                          ydrv(k) = ydrv(k) + partial%sa_drhototik(2,np)*ofct
                          zdrv(k) = zdrv(k) + partial%sa_drhototik(3,np)*ofct
                        endif
                        if (nregioni.ne.nregionk) then
                          xregdrv(nregioni) = xregdrv(nregioni) - partial%sa_drhototik(1,np)*ofct
                          yregdrv(nregioni) = yregdrv(nregioni) - partial%sa_drhototik(2,np)*ofct
                          zregdrv(nregioni) = zregdrv(nregioni) - partial%sa_drhototik(3,np)*ofct
                          xregdrv(nregionk) = xregdrv(nregionk) + partial%sa_drhototik(1,np)*ofct
                          yregdrv(nregionk) = yregdrv(nregionk) + partial%sa_drhototik(2,np)*ofct
                          zregdrv(nregionk) = zregdrv(nregionk) + partial%sa_drhototik(3,np)*ofct
                        endif
!
!  j-k contribution
!
                        if (lopj) then
                          xdrv(j) = xdrv(j) - partial%sa_drhototjk(1,np)*ofct
                          ydrv(j) = ydrv(j) - partial%sa_drhototjk(2,np)*ofct
                          zdrv(j) = zdrv(j) - partial%sa_drhototjk(3,np)*ofct
                        endif
                        if (lopk) then
                          xdrv(k) = xdrv(k) + partial%sa_drhototjk(1,np)*ofct
                          ydrv(k) = ydrv(k) + partial%sa_drhototjk(2,np)*ofct
                          zdrv(k) = zdrv(k) + partial%sa_drhototjk(3,np)*ofct
                        endif
                        if (nregionj.ne.nregionk) then
                          xregdrv(nregionj) = xregdrv(nregionj) - partial%sa_drhototjk(1,np)*ofct
                          yregdrv(nregionj) = yregdrv(nregionj) - partial%sa_drhototjk(2,np)*ofct
                          zregdrv(nregionj) = zregdrv(nregionj) - partial%sa_drhototjk(3,np)*ofct
                          xregdrv(nregionk) = xregdrv(nregionk) + partial%sa_drhototjk(1,np)*ofct
                          yregdrv(nregionk) = yregdrv(nregionk) + partial%sa_drhototjk(2,np)*ofct
                          zregdrv(nregionk) = zregdrv(nregionk) + partial%sa_drhototjk(3,np)*ofct
                        endif
                        if (lstr) then
                          do kl = 1,nstrains
                            klp = nstrptr(kl)
                            rstrd(kl) = rstrd(kl) + partial%sa_drhotots(klp,np)*ofct
                          enddo
                          if (latomicstress) then
                            do kl = 1,nstrains
                              klp = nstrptr(kl)
                              atomicstress(kl,i) = atomicstress(kl,i) + third*partial%sa_drhotots(klp,np)*ofct
                              atomicstress(kl,j) = atomicstress(kl,j) + third*partial%sa_drhotots(klp,np)*ofct
                              atomicstress(kl,k) = atomicstress(kl,k) + third*partial%sa_drhotots(klp,np)*ofct
                            enddo
                          endif
                        endif
                      enddo

                      call meamtotalrhoscreenderv(neamspecj,npartial,partial,xcd,ycd,zcd,scrho(1,j), &
                                                  rhoj,rscrhoj,rhoji,Sij,lsg1)
!
                      do np = 1,npartial
                        k = partial%sa_atom(np)
                        lopk = (.not.lfreeze.or.lopf(nrelat(k)))
                        nregionk = nregionno(nsft+nrelat(k))
!
!  i-j contribution
!
                        if (lopi) then
                          xdrv(i) = xdrv(i) - partial%sa_drhototij(1,np)*ofct
                          ydrv(i) = ydrv(i) - partial%sa_drhototij(2,np)*ofct
                          zdrv(i) = zdrv(i) - partial%sa_drhototij(3,np)*ofct
                        endif
                        if (lopj) then
                          xdrv(j) = xdrv(j) + partial%sa_drhototij(1,np)*ofct
                          ydrv(j) = ydrv(j) + partial%sa_drhototij(2,np)*ofct
                          zdrv(j) = zdrv(j) + partial%sa_drhototij(3,np)*ofct
                        endif
                        if (nregioni.ne.nregionj) then
                          xregdrv(nregioni) = xregdrv(nregioni) - partial%sa_drhototij(1,np)*ofct
                          yregdrv(nregioni) = yregdrv(nregioni) - partial%sa_drhototij(2,np)*ofct
                          zregdrv(nregioni) = zregdrv(nregioni) - partial%sa_drhototij(3,np)*ofct
                          xregdrv(nregionj) = xregdrv(nregionj) + partial%sa_drhototij(1,np)*ofct
                          yregdrv(nregionj) = yregdrv(nregionj) + partial%sa_drhototij(2,np)*ofct
                          zregdrv(nregionj) = zregdrv(nregionj) + partial%sa_drhototij(3,np)*ofct
                        endif
!
!  i-k contribution
!
                        if (lopi) then
                          xdrv(i) = xdrv(i) - partial%sa_drhototik(1,np)*ofct
                          ydrv(i) = ydrv(i) - partial%sa_drhototik(2,np)*ofct
                          zdrv(i) = zdrv(i) - partial%sa_drhototik(3,np)*ofct
                        endif
                        if (lopk) then
                          xdrv(k) = xdrv(k) + partial%sa_drhototik(1,np)*ofct
                          ydrv(k) = ydrv(k) + partial%sa_drhototik(2,np)*ofct
                          zdrv(k) = zdrv(k) + partial%sa_drhototik(3,np)*ofct
                        endif
                        if (nregioni.ne.nregionk) then
                          xregdrv(nregioni) = xregdrv(nregioni) - partial%sa_drhototik(1,np)*ofct
                          yregdrv(nregioni) = yregdrv(nregioni) - partial%sa_drhototik(2,np)*ofct
                          zregdrv(nregioni) = zregdrv(nregioni) - partial%sa_drhototik(3,np)*ofct
                          xregdrv(nregionk) = xregdrv(nregionk) + partial%sa_drhototik(1,np)*ofct
                          yregdrv(nregionk) = yregdrv(nregionk) + partial%sa_drhototik(2,np)*ofct
                          zregdrv(nregionk) = zregdrv(nregionk) + partial%sa_drhototik(3,np)*ofct
                        endif
!
!  j-k contribution
!
                        if (lopj) then
                          xdrv(j) = xdrv(j) - partial%sa_drhototjk(1,np)*ofct
                          ydrv(j) = ydrv(j) - partial%sa_drhototjk(2,np)*ofct
                          zdrv(j) = zdrv(j) - partial%sa_drhototjk(3,np)*ofct
                        endif
                        if (lopk) then
                          xdrv(k) = xdrv(k) + partial%sa_drhototjk(1,np)*ofct
                          ydrv(k) = ydrv(k) + partial%sa_drhototjk(2,np)*ofct
                          zdrv(k) = zdrv(k) + partial%sa_drhototjk(3,np)*ofct
                        endif
                        if (nregionj.ne.nregionk) then
                          xregdrv(nregionj) = xregdrv(nregionj) - partial%sa_drhototjk(1,np)*ofct
                          yregdrv(nregionj) = yregdrv(nregionj) - partial%sa_drhototjk(2,np)*ofct
                          zregdrv(nregionj) = zregdrv(nregionj) - partial%sa_drhototjk(3,np)*ofct
                          xregdrv(nregionk) = xregdrv(nregionk) + partial%sa_drhototjk(1,np)*ofct
                          yregdrv(nregionk) = yregdrv(nregionk) + partial%sa_drhototjk(2,np)*ofct
                          zregdrv(nregionk) = zregdrv(nregionk) + partial%sa_drhototjk(3,np)*ofct
                        endif
                        if (lstr) then
                          do kl = 1,nstrains
                            klp = nstrptr(kl)
                            rstrd(kl) = rstrd(kl) + partial%sa_drhotots(klp,np)*ofct
                          enddo
                          if (latomicstress) then
                            do kl = 1,nstrains
                              klp = nstrptr(kl)
                              atomicstress(kl,i) = atomicstress(kl,i) + third*partial%sa_drhotots(klp,np)*ofct
                              atomicstress(kl,j) = atomicstress(kl,j) + third*partial%sa_drhotots(klp,np)*ofct
                              atomicstress(kl,k) = atomicstress(kl,k) + third*partial%sa_drhotots(klp,np)*ofct
                            enddo
                          endif
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
                  call meamtotalrhoderv(neamspecj,scrho(1,j),rhoj,drhoji,drhototji,drhojis,drhototjis, &
                                        drhoji2,drhototji2,drhoji2s,drhototji2s,drhoji2m,drhototji2m, &
                                        lsg1,.false.)
                endif
              else
                call eamrho(nati,ntypi,natj,ntypj,r,rpot(npot),xcrd,ycrd,zcrd,rhoij,rhoji,drhototij,drhototji, &
                            drhototijs,drhototjis,drhototij2,drhototji2,drhototij2s,drhototji2s, &
                            drhototij2m,drhototji2m,drhototij3,drhototji3,1.0_dp,1.0_dp,lsg1,.true.,.false.,.false.)
              endif
            endif
          enddo
!
!  Combine derivative terms
!
          if (QMMMmode(ncf).gt.0) then
            if (nregiontypi.ne.1) then
              deriv(1:3) = deriv(1:3) + rscrhoi*drhototij(1:3)*ofct
              if (lsg1) then
                derivs(1:6) = derivs(1:6) + rscrhoi*drhototijs(1:6)*ofct
              endif
            endif
            if (nregiontypj.ne.1) then
              deriv(1:3) = deriv(1:3) + rscrhoj*drhototji(1:3)*ofct
              if (lsg1) then
                derivs(1:6) = derivs(1:6) + rscrhoj*drhototjis(1:6)*ofct
              endif
            endif
          else
            deriv(1:3) = deriv(1:3) + (rscrhoi*drhototij(1:3) + rscrhoj*drhototji(1:3))*ofct
            if (lsg1) then
              derivs(1:6) = derivs(1:6) + (rscrhoi*drhototijs(1:6) + rscrhoj*drhototjis(1:6))*ofct
            endif
          endif
        endif
        if (lvalidij) then
!
!  Only need to do internal derivatives if i not equals j
!
          if (i.ne.j) then
!******************************
!  Internal first derivatives *
!******************************
            if (lMEAM) then
              if (lopi) then
                xdrvnr(i) = xdrvnr(i) - deriv(1)
                ydrvnr(i) = ydrvnr(i) - deriv(2)
                zdrvnr(i) = zdrvnr(i) - deriv(3)
              endif
              if (lopj) then
                xdrvnr(j) = xdrvnr(j) + deriv(1)
                ydrvnr(j) = ydrvnr(j) + deriv(2)
                zdrvnr(j) = zdrvnr(j) + deriv(3)
              endif
            else
              if (lopi) then
                xdrv(i) = xdrv(i) - deriv(1)
                ydrv(i) = ydrv(i) - deriv(2)
                zdrv(i) = zdrv(i) - deriv(3)
              endif
              if (lopj) then
                xdrv(j) = xdrv(j) + deriv(1)
                ydrv(j) = ydrv(j) + deriv(2)
                zdrv(j) = zdrv(j) + deriv(3)
              endif
            endif
          endif
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) - deriv(1)
            yregdrv(nregioni) = yregdrv(nregioni) - deriv(2)
            zregdrv(nregioni) = zregdrv(nregioni) - deriv(3)
            xregdrv(nregionj) = xregdrv(nregionj) + deriv(1)
            yregdrv(nregionj) = yregdrv(nregionj) + deriv(2)
            zregdrv(nregionj) = zregdrv(nregionj) + deriv(3)
          endif
!*****************************
!  Strain first derivatives  *
!*****************************
          if (lstr) then
            if (lMEAM) then
              do kl = 1,nstrains
                klp = nstrptr(kl)
                rstrdnr(kl) = rstrdnr(kl) + derivs(klp)
              enddo
            else
              do kl = 1,nstrains
                klp = nstrptr(kl)
                rstrd(kl) = rstrd(kl) + derivs(klp)
              enddo
            endif
            if (latomicstress) then
              do kl = 1,nstrains
                klp = nstrptr(kl)
                atomicstress(kl,i) = atomicstress(kl,i) + 0.5_dp*derivs(klp)
                atomicstress(kl,j) = atomicstress(kl,j) + 0.5_dp*derivs(klp)
              enddo
            endif
          endif
        endif
!**************************************
!  End of valid distance i-j section  *
!**************************************
      endif
    enddo jloop
  enddo iloop
!
!  End of real space part - perform general tasks
!
999 continue
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('manymi3','npotl')
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
