  subroutine property(lprint)
!
!  Calculate the properties that depend on the second derivatives
!
!  Calculate high freq dielectric constant first so that
!  on return the inverse second derivative matrix is
!  still available in dervi for use by defect calculations.
!  If optical relaxation then the order must be reversed!
!
!  Modified for breathing shell model
!
!  nbs   = no. of full cell atoms with breathing shells
!  nbss  = no. of full cell shells with breathing shells
!  nbsptr= pointer to atoms with breathing shells
!
!   8/98 Breathing shell pointer generation moved to subroutine
!   8/98 Calculation of refractive index added
!   5/99 Calculation of Young's moduli and Poisson's ratios added
!  10/01 Pressure correction for C44,55,66 corrected by a half
!   5/02 All three conventions for bulk modulus now present
!   5/02 Shear modulus added
!   8/02 S and P wave velocities added for polycrystalline average
!   8/02 Made implicit none
!   9/02 Output of elastic compliances added
!   9/02 conve now defined using parameters
!  10/02 Problems with negative shear moduli in sqrt handled
!   2/03 All mechanical properties now internally in GPa
!   2/03 Labels for piezoelectric strain/stress switched to be correct
!   2/03 Matrix inversion accelerated
!   3/03 Partial occupancies with multiple ions on the same site handled
!   6/03 Calculation of compressibility corrected for switch to GPa
!   4/04 Dimensions of tmp now set to a minimum of 12 to avoid error
!  10/04 Eispack calls replaced by lapack
!   5/06 Mass now taken from species masses
!   5/07 Partial occupancy data moved to module
!  12/07 Unused variables removed
!   1/09 Integer datatypes all explicitly declared
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   4/09 Calling of changemaxd2 modified to handle case where no internal
!        second derivatives are required.
!   6/09 Extra space added ahead of "Mechanical properties"
!   1/10 Young's moduli and Poisson's ratios store in module arrays
!   1/10 Incorrect factor of 10 in velocities corrected
!  10/11 Stress tensor output moved to property from optout
!   5/12 Stresses moved to separate routine
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, October 2011
!
  use configurations, only : lanisotropicpresscfg
  use constants
  use control
  use current
  use derivatives
  use element
  use gulp_cml,       only : lcml
  use gulp_cml_props, only : gulp_cml_output_derivs, gulp_cml_output_dielectric
  use general
  use iochannels
  use parallel
  use partial
  use properties
  use shell
  use species,        only : massspec
  use symmetry
  use times
  implicit none
!
!  Local variables
!
  real(dp),    dimension(:,:), allocatable       :: d3
  real(dp),    dimension(:),   allocatable       :: qfo
  real(dp),    dimension(:),   allocatable       :: qshell
  real(dp),    dimension(:),   allocatable       :: tmp
  integer(i4)                                    :: i
  integer(i4)                                    :: ifail
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: ixs
  integer(i4)                                    :: iys
  integer(i4)                                    :: izs
  integer(i4)                                    :: indk
  integer(i4)                                    :: indl
  integer(i4), dimension(:),   allocatable       :: ipivot
  integer(i4)                                    :: iptr
  integer(i4)                                    :: j
  integer(i4)                                    :: jptr
  integer(i4)                                    :: jx
  integer(i4)                                    :: jy
  integer(i4)                                    :: jz
  integer(i4)                                    :: jxs
  integer(i4)                                    :: jys
  integer(i4)                                    :: jzs
  integer(i4)                                    :: k
  integer(i4)                                    :: ki
  integer(i4)                                    :: l
  integer(i4)                                    :: mint
  integer(i4)                                    :: msvar
  integer(i4)                                    :: n
  integer(i4)                                    :: n3f
  integer(i4)                                    :: nbsc
  integer(i4)                                    :: ni
  integer(i4)                                    :: nj
  integer(i4)                                    :: status
  logical                                        :: lcompliances
  logical                                        :: ldlch
  logical                                        :: ldlcs
  logical                                        :: lelastic
  logical                                        :: linternal
  logical                                        :: lmoduli
  logical                                        :: lpiezo
  logical                                        :: lprint
  real(dp)                                       :: bulkmod_hill
  real(dp)                                       :: bulkmod_reuss
  real(dp)                                       :: bulkmod_voigt
  real(dp)                                       :: cfactor
  real(dp)                                       :: compliances(6,6)
  real(dp)                                       :: conve
  real(dp)                                       :: cputime
  real(dp)                                       :: dlcfactor
  real(dp),    dimension(:),   allocatable       :: dpacked
  real(dp)                                       :: epv
  real(dp)                                       :: piefactor
  real(dp)                                       :: qak
  real(dp)                                       :: r3(3)
  real(dp)                                       :: r33(3,3)
  real(dp)                                       :: r43
  real(dp)                                       :: rvol
  real(dp)                                       :: rmolfct
  real(dp)                                       :: shearmod_hill
  real(dp)                                       :: shearmod_reuss
  real(dp)                                       :: shearmod_voigt
  real(dp)                                       :: sum
  real(dp)                                       :: t1p
  real(dp)                                       :: t2p
  real(dp)                                       :: tdum
  real(dp)                                       :: totmass
  real(dp)                                       :: vol
  real(dp)                                       :: volume
  real(dp)                                       :: vs_hill
  real(dp)                                       :: vs_reuss
  real(dp)                                       :: vs_voigt
  real(dp)                                       :: vp_hill
  real(dp)                                       :: vp_reuss
  real(dp)                                       :: vp_voigt
  real(dp)                                       :: w1l(9)
  real(dp),    dimension(:),   allocatable       :: wrk
  real(dp)                                       :: zero
!
  data cfactor/6.241460893d-3/
!
  t1p = cputime()
!
  lcompliances = .true.
  lelastic = .true.
  ldlch = .true.
  ldlcs = .true.
  lmoduli = .true.
  lpiezo = .true.
  conve = evtoj*1.0d20
  zero = 1.0d-12
  vol = volume(rv)
  n3f = 3*numat
  mint = n3f - 3
!
!  Check second derivative memory
!
  if (n3f+6.gt.maxd2.or.n3f+6.gt.maxd2u) then
    maxd2  = max(n3f + 6,maxd2)
    maxd2u = max(n3f + 6,maxd2u)
    call changemaxd2
  endif
!
!  Allocate local memory
!
  allocate(qfo(numat),stat=status)
  if (status/=0) call outofmemory('property','qfo')
  allocate(qshell(numat),stat=status)
  if (status/=0) call outofmemory('property','qshell')
  allocate(tmp(max(8*numat,12)),stat=status)
  if (status/=0) call outofmemory('property','tmp')
  if (lpocc) then
    allocate(d3(maxd2,6),stat=status)
    if (status/=0) call outofmemory('property','d3')
  endif
!
!  Set logical as to whether internal contribution to
!  elastic constants are needed or wanted
!
  linternal = (index(keyword,'noin').eq.0)
  if (mint+nbs.eq.0) linternal = .false.
!
  if (ldlcs.or.ldlch.or.lpiezo) then
    dlcfactor = 4.0_dp*pi/vol
    piefactor = dlcfactor*conve
    dlcfactor = angstoev*dlcfactor
  endif
!
!  Calculate density
!
  totmass = 0.0_dp
  do i = 1,nasym
    ni = nspecptr(i)
    totmass = totmass + massspec(ni)*occua(i)*dble(neqv(i))
  enddo
  if (vol.gt.1d-12) then
    rmolfct = avogadro*1.0d-23
    density = (10.0d0*totmass)/(vol*rmolfct)
  endif
!**************************************
!  High frequency dielectric constant *
!**************************************
  if (.not.lshello) then
    if (ldlch.and.nshell.gt.0) then
!
!  Collect shell second derivative terms
!
      msvar = 3*nshell
      do i = 1,nshell
        ni = nshptr(i)
        qshell(i) = qf(ni)*occuf(ni)
        ix = 3*(ni-1) + 1
        iy = ix + 1
        iz = ix + 2
        ixs = 3*(i-1) + 1
        iys = ixs + 1
        izs = ixs + 2
        do j = 1,nshell
          nj = nshptr(j)
          jx = 3*(nj-1) + 1
          jy = jx + 1
          jz = jx + 2
          jxs = 3*(j-1) + 1
          jys = jxs + 1
          jzs = jxs + 2
          dervi(jxs,ixs) = derv2(jx,ix)
          dervi(jys,ixs) = derv2(jy,ix)
          dervi(jzs,ixs) = derv2(jz,ix)
          dervi(jxs,iys) = derv2(jx,iy)
          dervi(jys,iys) = derv2(jy,iy)
          dervi(jzs,iys) = derv2(jz,iy)
          dervi(jxs,izs) = derv2(jx,iz)
          dervi(jys,izs) = derv2(jy,iz)
          dervi(jzs,izs) = derv2(jz,iz)
        enddo
      enddo
      if (nbss.gt.0) then
!
!  Collect radial second derivatives
!
        if (lpocc) then
          do i = 1,nshell
            ni = nshptr(i)
            do j = 1,nshell
              nj = nshptr(j)
              dervi(msvar+j,msvar+i) = derv2(n3f+nj,n3f+ni)
            enddo
            do j = 1,nshell
              nj = nshptr(j)
              jx = 3*(nj-1) + 1
              jy = jx + 1
              jz = jx + 2
              jxs = 3*(j-1) + 1
              jys = jxs + 1
              jzs = jxs + 2
              dervi(jxs,msvar+i) = derv2(jx,n3f+ni)
              dervi(jys,msvar+i) = derv2(jy,n3f+ni)
              dervi(jzs,msvar+i) = derv2(jz,n3f+ni)
              dervi(msvar+i,jxs) = derv2(n3f+ni,jx)
              dervi(msvar+i,jys) = derv2(n3f+ni,jy)
              dervi(msvar+i,jzs) = derv2(n3f+ni,jz)
            enddo
          enddo
        else
          nbsc = nbs - nbss
          do i = 1,nbss
            iptr = n3f + nbsptr(nbsc+i)
            do j = 1,nbss
              jptr = n3f + nbsptr(nbsc+j)
              dervi(msvar+j,msvar+i) = derv2(jptr,iptr)
            enddo
            do j = 1,nshell
              nj = nshptr(j)
              jx = 3*(nj-1) + 1
              jy = jx + 1
              jz = jx + 2
              jxs = 3*(j-1) + 1
              jys = jxs + 1
              jzs = jxs + 2
              dervi(jxs,msvar+i) = derv2(jx,iptr)
              dervi(jys,msvar+i) = derv2(jy,iptr)
              dervi(jzs,msvar+i) = derv2(jz,iptr)
            enddo
          enddo
        endif
      endif
!
!  Compress derivatives for partial occupancies
!
      if (lpocc) then
        call compressd1(qshell,0_i4,nsfoc,nshell,iocshptr)
        call compressd2(dervi,maxd2,0_i4,nsfoc,nbsfoc,nshell,iocshptr,ibocshptr)
        n = 3*nsfoc + nbsfoc
      else
        n = msvar + nbss
      endif
!
!  Invert second derivative matrix
!
      ifail = 1
!
!  Allocate workspace for inversion
!
      allocate(dpacked(n*(n+1)/2),stat=status)
      if (status/=0) call outofmemory('property','dpacked')
      allocate(ipivot(n),stat=status)
      if (status/=0) call outofmemory('property','ipivot')
      allocate(wrk(3*n),stat=status)
      if (status/=0) call outofmemory('property','wrk')
!
!  Transfer data to packed storage
!
      k = 0
      do i = 1,n
        do j = 1,i
          k = k + 1
          dpacked(k) = dervi(j,i)
        enddo
      enddo
!
!  Factorise matrix
!
      call dsptrf('U',n,dpacked,ipivot,ifail)
      if (ifail.eq.0) then
!
!  Form inverse
!
        call dsptri('U',n,dpacked,ipivot,wrk,ifail)
!
!  Transfer data back
!
        k = 0
        do i = 1,n
          do j = 1,i
            k = k + 1
            dervi(j,i) = dpacked(k)
            dervi(i,j) = dpacked(k)
          enddo
        enddo
      endif
!
!  Free workspace
!
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('property','wrk')
      deallocate(ipivot,stat=status)
      if (status/=0) call deallocate_error('property','ipivot')
      deallocate(dpacked,stat=status)
      if (status/=0) call deallocate_error('property','dpacked')
!
      if (ifail.ne.0) then
        nwarn = nwarn + 1
        call outwarning('Properties cannot be calculated - matrix is singular',0_i4)
        goto 999
      endif
!
!  Multiply inverse second derivatives by charge vectors
!
      do i = 1,3
        do j = 1,3
          sum = 0.0_dp
          do k = 1,nsfoc
            qak = qshell(k)
            indk = 3*(k-1) + i
            do l = 1,nsfoc
              indl = 3*(l-1) + j
              sum = sum + dervi(indl,indk)*qak*qshell(l)
            enddo
          enddo
          diconh(j,i) = dlcfactor*sum
        enddo
        diconh(i,i) = diconh(i,i) + 1.0_dp
      enddo
!
!  Calculate refractive indices
!
      do i = 1,3
        do j = 1,3
          r33(j,i) = diconh(j,i)
        enddo
      enddo
      call dsyev('N','U',3_i4,r33,3_i4,r3,w1l,9_i4,ifail)
      hfrefind(1) = sqrt(dabs(r3(1)))
      hfrefind(2) = sqrt(dabs(r3(2)))
      hfrefind(3) = sqrt(dabs(r3(3)))
      hfrefind(1) = sign(hfrefind(1),r3(1))
      hfrefind(2) = sign(hfrefind(2),r3(2))
      hfrefind(3) = sign(hfrefind(3),r3(3))
    else
      do i = 1,3
        do j = 1,3
          diconh(j,i) = 0.0_dp
        enddo
        diconh(i,i) = 1.0_dp
        hfrefind(i) = 1.0_dp
      enddo
    endif
  endif
!
!  Set up constants and invert second derivative matrix
!
  if (lelastic.or.ldlcs.or.lpiezo) then
    if (linternal) then
      if (lpocc) then
!
!  Copy derv2 into dummy array as matinv overwrites it
!
        do i = 1,mint+3
          do j = 1,mint+3
            dervi(j,i) = derv2(j,i)
          enddo
        enddo
        do i = 1,6
          do j = 1,mint
            d3(j,i) = derv3(j,i)
          enddo
        enddo
!
!  Radial component
!
        if (nbs.gt.0) then
          do i = 1,numat
            do j = 1,numat
              dervi(n3f+j,n3f+i) = derv2(n3f+j,n3f+i)
            enddo
            do j = 1,mint+3
              dervi(j,n3f+i) = derv2(j,n3f+i)
              dervi(n3f+i,j) = derv2(n3f+i,j)
            enddo
          enddo
          do i = 1,6
            do j = 1,numat
              d3(n3f+j,i) = derv3(n3f+j,i)
            enddo
          enddo
        endif
!
!  Compress partial occupancy second derivatives to full sites
!
        do i = 1,numat
          qfo(i) = qf(i)*occuf(i)
        enddo
        call compressd1(qfo,ncfoc,nsfoc,numat,iocptr)
        call compressd2(dervi,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
        call compressd3(d3,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
!
        n = 3*(ncsfoc - 1) + nbfoc
!
!  Now remove last atom from list to avoid singular second derivatives
!
        if (nbfoc.gt.0) then
          indk = 3*(ncsfoc - 1)
          do i = 1,3*ncsfoc+nbfoc
            do j = 1,nbfoc
              dervi(indk+j,i) = dervi(indk+3+j,i)
            enddo
          enddo
          do i = 1,6
            do j = 1,nbfoc
              d3(indk+j,i) = d3(indk+3+j,i)
            enddo
          enddo
          do i = 1,nbfoc
            do j = 1,n
              dervi(j,indk+i) = dervi(j,indk+3+i)
            enddo
          enddo
          do i = 1,nbfoc
            do j = 1,nbfoc
              dervi(indk+j,indk+i) = dervi(indk+3+j,indk+3+i)
            enddo
          enddo
        endif
      else
!
!  Copy derv2 into dummy array as matinv overwrites it
!
        do i = 1,mint
          do j = 1,mint
            dervi(j,i) = derv2(j,i)
          enddo
        enddo
!
!  Radial component
!
        if (nbs.gt.0) then
          do i = 1,nbs
            iptr = n3f + nbsptr(i)
            do j = 1,nbs
              jptr = n3f + nbsptr(j)
              dervi(mint+j,mint+i) = derv2(jptr,iptr)
            enddo
            do j = 1,mint
              dervi(j,mint+i) = derv2(j,iptr)
            enddo
          enddo
        endif
        do i = 1,numat
          qfo(i) = qf(i)*occuf(i)
        enddo
        n = mint + nbs
      endif
!
!  Invert internal derivative matrix
!  Ignore second derivatives of last atom
!  to prevent a singularity - corresponds
!  to removing the 3 translational degrees 
!  of freedom of the lattice
!
      ifail = 1
!
!  Allocate workspace memory
!
      allocate(dpacked(n*(n+1)/2),stat=status)
      if (status/=0) call outofmemory('property','dpacked')
      allocate(ipivot(n),stat=status)
      if (status/=0) call outofmemory('property','ipivot')
      allocate(wrk(3*n),stat=status)
      if (status/=0) call outofmemory('property','wrk')
!
!  Transfer data to workspace
!
      k = 0
      do i = 1,n
        do j = 1,i
          k = k + 1
          dpacked(k) = dervi(j,i)
        enddo
      enddo
!
!  Factorise matrix
!
      call dsptrf('U',n,dpacked,ipivot,ifail)
      if (ifail.eq.0) then
!
!  Form inverse
!
        call dsptri('U',n,dpacked,ipivot,wrk,ifail)
!
!  Transfer data back
!
        k = 0
        do i = 1,n
          do j = 1,i
            k = k + 1
            dervi(j,i) = dpacked(k)
            dervi(i,j) = dpacked(k)
          enddo
        enddo
      endif
!
!  Free workspace memory
!
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('property','wrk')
      deallocate(ipivot,stat=status)
      if (status/=0) call deallocate_error('property','ipivot')
      deallocate(dpacked,stat=status)
      if (status/=0) call deallocate_error('property','dpacked')
!
      if (ifail.gt.0) then
        nwarn = nwarn + 1
        call outwarning('Properties cannot be calculated - matrix is singular',0_i4)
        goto 999
      endif
    endif
  endif
!*********************
!  Elastic constants *
!*********************
  if (lelastic) then
    elcon(1:6,1:6) = sderv2(1:6,1:6)
!
!  Correct for pressure term in the second derivatives
!
!  First remove enthalpy terms.
!  Second apply non-zero pressure corrections
!
    if (abs(press).gt.0.0_dp) then
      epv = press*vol*cfactor
      elcon(1,1) = elcon(1,1) - epv
      elcon(2,2) = elcon(2,2) - epv
      elcon(3,3) = elcon(3,3) - epv
      elcon(2,1) = elcon(2,1) - epv
      elcon(3,1) = elcon(3,1) - epv
      elcon(3,2) = elcon(3,2) - epv
      elcon(1,2) = elcon(1,2) - epv
      elcon(1,3) = elcon(1,3) - epv
      elcon(2,3) = elcon(2,3) - epv
      elcon(4,4) = elcon(4,4) - 0.5_dp*epv
      elcon(5,5) = elcon(5,5) - 0.5_dp*epv
      elcon(6,6) = elcon(6,6) - 0.5_dp*epv
! non-zero pressure correction
! infinitesimal (small) strain correction at finite hydostatic pressure
! Barron & Klein Proc. Phys. Soc.vol 85 1965 Eqn.5.5
! 1/2P(2DijDkl-DilDjk-DikDjl      
! Cijkl=1/V(d2U/dEijdEkl)+P/2*(2DijDkl-DilDjk-DikDjl) pressure correction
      elcon(2,1) = elcon(2,1) + epv
      elcon(3,1) = elcon(3,1) + epv
      elcon(3,2) = elcon(3,2) + epv
      elcon(1,2) = elcon(1,2) + epv
      elcon(1,3) = elcon(1,3) + epv
      elcon(2,3) = elcon(2,3) + epv
      elcon(4,4) = elcon(4,4) - 0.5_dp*epv
      elcon(5,5) = elcon(5,5) - 0.5_dp*epv
      elcon(6,6) = elcon(6,6) - 0.5_dp*epv
    endif
!
    if (linternal) then
      if (lpocc) then
        do i = 1,6
          do j = 1,n
            tmp(j) = 0.0_dp
            do k = 1,n
              tmp(j) = tmp(j) + dervi(k,j)*d3(k,i)
            enddo
          enddo
          do j = 1,6
            do k = 1,n
              elcon(j,i) = elcon(j,i) - d3(k,j)*tmp(k)
            enddo
          enddo
        enddo
      else
        do i = 1,6
          do j = 1,mint+nbs
            tmp(j) = 0.0_dp
            do k = 1,mint+nbs
              if (k.gt.mint) then
                ki = n3f + nbsptr(k-mint)
              else
                ki = k
              endif
              tmp(j) = tmp(j) + dervi(k,j)*derv3(ki,i)
            enddo
          enddo
          do j = 1,6
            do k = 1,mint+nbs
              if (k.gt.mint) then
                ki = n3f + nbsptr(k-mint)
              else
                ki = k
              endif
              elcon(j,i) = elcon(j,i) - derv3(ki,j)*tmp(k)
            enddo
          enddo
        enddo
      endif
      rvol = 10.0_dp*conve/vol
      do i = 1,6
        do j = 1,6
          elcon(j,i) = rvol*elcon(j,i)
        enddo
      enddo
    else
      rvol = 10.0_dp*conve/vol
      do i = 1,6
        do j = 1,6
          elcon(j,i) = rvol*elcon(j,i)
        enddo
      enddo
    endif
  endif
  if (lpiezo.or.lmoduli) then
!**********************
!  Compliance tensor  *
!**********************
    do i = 1,6
      do j = 1,6
        compliances(j,i) = elcon(j,i)
      enddo
    enddo
    ifail = 1
    call matinv(compliances,6_i4,6_i4,tmp,ifail)
    if (ifail.gt.0) lcompliances = .false.
  endif
!****************************
!  Piezoelectric constants  *
!****************************
  if (lpiezo) then
    if (linternal) then
!
!  Multiply inverse second derivatives by mixed strain cartesian matrix
!
      if (lpocc) then
        do i = 1,6
          do j = 1,n
            tdum = 0.0_dp
            do k = 1,n
              tdum = tdum + dervi(k,j)*d3(k,i)
            enddo
            dervi(j,i+n) = tdum
          enddo
        enddo
      else
        do i = 1,6
          do j = 1,mint+nbs
            tdum = 0.0_dp
            do k = 1,mint+nbs
              if (k.gt.mint) then
                ki = n3f + nbsptr(k-mint)
              else
                ki = k
              endif
              tdum = tdum + dervi(k,j)*derv3(ki,i)
            enddo
            dervi(j,i+mint+nbs) = tdum
          enddo
        enddo
      endif
!
!  Piezoelectric stress constants
!
      do i = 1,6
        do j = 1,3
          sum = 0.0_dp
          do k = 1,ncsfoc-1
            indk = 3*(k-1) + j
            sum = sum + dervi(indk,i+n)*qfo(k)
          enddo
          piezo(i,j) = - piefactor*sum
        enddo
      enddo
    else
!
!  If not linternal then piezo must be zero
!
      piezo(1:6,1:3) = 0.0_dp
    endif
!
!  Piezoelectric strain constants
!
    do i = 1,3
      do j = 1,6
        piezs(j,i) = 0.0_dp
        do k = 1,6
          piezs(j,i) = piezs(j,i) + compliances(k,j)*piezo(k,i)
        enddo
      enddo
    enddo
  endif
  if (lmoduli) then
!
!  Bulk modulus - calculate all 3 possible definitions
!
    bulkmod_hill = 0.0_dp
    bulkmod_reuss = 0.0_dp
    bulkmod_voigt = 0.0_dp
    do i = 1,3
      bulkmod_voigt = bulkmod_voigt + elcon(1,i) + elcon(2,i) + elcon(3,i)
    enddo
    bulkmod_voigt = bulkmod_voigt/9.0_dp
    do i = 1,3
      bulkmod_reuss = bulkmod_reuss + compliances(1,i) + compliances(2,i) + compliances(3,i)
    enddo
    if (abs(bulkmod_reuss).gt.1.0d-8) then
      bulkmod_reuss = 1.0_dp/bulkmod_reuss
    else
      bulkmod_reuss = 0.0_dp
    endif
    bulkmod_hill = 0.5_dp*(bulkmod_reuss + bulkmod_voigt)
!
!  Shear modulus - calculate all 3 possible definitions
!
    shearmod_voigt = elcon(1,1) + elcon(2,2) + elcon(3,3) &
      + 3.0_dp*(elcon(4,4) + elcon(5,5) + elcon(6,6)) &
      - elcon(1,2) - elcon(1,3) - elcon(2,3)
    shearmod_voigt = shearmod_voigt/15.0_dp
    shearmod_reuss = 4.0_dp*(compliances(1,1) + compliances(2,2) + compliances(3,3)) &
                   - 4.0_dp*(compliances(1,2) + compliances(1,3) + compliances(2,3)) &
                   + 3.0_dp*(compliances(4,4) + compliances(5,5) + compliances(6,6))
    if (abs(shearmod_reuss).gt.1.0d-8) then
      shearmod_reuss = 15.0_dp/shearmod_reuss
    else
      shearmod_reuss = 0.0_dp
    endif
    shearmod_hill = 0.5_dp*(shearmod_reuss + shearmod_voigt)
!
!  Return the requested moduli values for fitting
!
    if (index(keyword,'voi').ne.0) then
      bulkmod = bulkmod_voigt
      shearmod = shearmod_voigt
    elseif (index(keyword,'hill').ne.0) then
      bulkmod = bulkmod_hill
      shearmod = shearmod_hill
    else
      bulkmod = bulkmod_reuss
      shearmod = shearmod_reuss
    endif
!
!  Acoustic wave velocities - polycrystalline average
!
    r43 = 4.0_dp/3.0_dp
    vs_hill = sign(sqrt(abs(shearmod_hill)/density),shearmod_hill)
    vs_reuss = sign(sqrt(abs(shearmod_reuss)/density),shearmod_reuss)
    vs_voigt = sign(sqrt(abs(shearmod_voigt)/density),shearmod_voigt)
    vp_hill = sqrt(abs(bulkmod_hill + r43*shearmod_hill)/density)
    vp_reuss = sqrt(abs(bulkmod_reuss + r43*shearmod_reuss)/density)
    vp_voigt = sqrt(abs(bulkmod_voigt + r43*shearmod_voigt)/density)
!
!  Young's Moduli
!
    if (abs(compliances(1,1)).gt.1.0d-8) then
      ym(1) = 1.0_dp/compliances(1,1)
    else
      ym(1) = 0.0_dp
    endif
    if (abs(compliances(2,2)).gt.1.0d-8) then
      ym(2) = 1.0_dp/compliances(2,2)
    else
      ym(2) = 0.0_dp
    endif
    if (abs(compliances(3,3)).gt.1.0d-8) then
      ym(3) = 1.0_dp/compliances(3,3)
    else
      ym(3) = 0.0_dp
    endif
!
!  Poisson's ratios
!
    poissonratio(1) = - compliances(2,1)*ym(2)
    poissonratio(2) = - compliances(3,1)*ym(3)
    poissonratio(3) = - compliances(3,2)*ym(3)
  endif
!******************************
!  Static dielectric constant *
!******************************
  if (ldlcs) then
    do i = 1,3
      do j = 1,3
        sum = 0.0_dp
        do k = 1,ncsfoc-1
          qak = qfo(k)
          indk = 3*(k-1) + i
          do l = 1,ncsfoc-1
            indl = 3*(l-1) + j
            sum = sum + dervi(indl,indk)*qak*qfo(l)
          enddo
        enddo
        dicons(j,i) = dlcfactor*sum
      enddo
      dicons(i,i) = dicons(i,i) + 1.0_dp
    enddo
!
!  Calculate refractive indices
!
    do i = 1,3
      do j = 1,3
        r33(j,i) = dicons(j,i)
      enddo
    enddo
    call dsyev('N','U',3_i4,r33,3_i4,r3,w1l,9_i4,ifail)
    srefind(1) = sqrt(abs(r3(1)))
    srefind(2) = sqrt(abs(r3(2)))
    srefind(3) = sqrt(abs(r3(3)))
    srefind(1) = sign(srefind(1),r3(1))
    srefind(2) = sign(srefind(2),r3(2))
    srefind(3) = sign(srefind(3),r3(3))
  endif
!**************************************
!  High frequency dielectric constant *
!**************************************
  if (lshello) then
    if (ldlch.and.nshell.gt.0) then
!
!  Collect shell second derivative terms
!
      msvar = 3*nshell
      do i = 1,nshell
        ni = nshptr(i)
        qshell(i) = qf(ni)*occuf(ni)
        ix = 3*(ni-1) + 1
        iy = ix + 1
        iz = ix + 2
        ixs = 3*(i-1) + 1
        iys = ixs + 1
        izs = ixs + 2
        do j = 1,i
          nj = nshptr(j)
          jx = 3*(nj-1) + 1
          jy = jx + 1
          jz = jx + 2
          jxs = 3*(j-1) + 1
          jys = jxs + 1
          jzs = jxs + 2
          dervi(jxs,ixs) = derv2(jx,ix)
          dervi(jys,ixs) = derv2(jy,ix)
          dervi(jzs,ixs) = derv2(jz,ix)
          dervi(jxs,iys) = derv2(jx,iy)
          dervi(jys,iys) = derv2(jy,iy)
          dervi(jzs,iys) = derv2(jz,iy)
          dervi(jxs,izs) = derv2(jx,iz)
          dervi(jys,izs) = derv2(jy,iz)
          dervi(jzs,izs) = derv2(jz,iz)
        enddo
      enddo
      if (nbss.gt.0) then
!
!  Collect radial second derivatives
!
        nbsc = nbs - nbss
        do i = 1,nbss
          iptr = n3f + nbsptr(nbsc+i)
          do j = 1,i
            jptr = n3f + nbsptr(nbsc+j)
            dervi(msvar+j,msvar+i) = derv2(jptr,iptr)
          enddo
          do j = 1,nshell
            nj = nshptr(j)
            jx = 3*(nj-1) + 1
            jy = jx + 1
            jz = jx + 2
            jxs = 3*(j-1) + 1
            jys = jxs + 1
            jzs = jxs + 2
            dervi(jxs,msvar+i) = derv2(jx,iptr)
            dervi(jys,msvar+i) = derv2(jy,iptr)
            dervi(jzs,msvar+i) = derv2(jz,iptr)
          enddo
        enddo
      endif
!
!  Compress derivatives for partial occupancies
!
      if (lpocc) then
        call compressd1(qshell,0_i4,nsfoc,nshell,iocshptr)
        call compressd2(dervi,maxd2,0_i4,nsfoc,nbsfoc,nshell,iocshptr,ibocshptr)
        n = 3*nsfoc + nbsfoc
      else
        n = msvar + nbss
      endif
!
!  Invert second derivative matrix
!
      ifail = 1
!
!  Allocate workspace memory
!
      allocate(dpacked(n*(n+1)/2),stat=status)
      if (status/=0) call outofmemory('property','dpacked')
      allocate(ipivot(n),stat=status)
      if (status/=0) call outofmemory('property','ipivot')
      allocate(wrk(3*n),stat=status)
      if (status/=0) call outofmemory('property','wrk')
!
!  Transfer data to workspace
!
      k = 0
      do i = 1,n
        do j = 1,i
          k = k + 1
          dpacked(k) = dervi(j,i)
        enddo
      enddo
!
!  Factorise matrix
!
      call dsptrf('U',n,dpacked,ipivot,ifail)
      if (ifail.eq.0) then
!
!  Form inverse
!
        call dsptri('U',n,dpacked,ipivot,wrk,ifail)
!
!  Transfer data back
!
        k = 0
        do i = 1,n
          do j = 1,i
            k = k + 1
            dervi(j,i) = dpacked(k)
            dervi(i,j) = dpacked(k)
          enddo
        enddo
      endif
!
!  Free workspace memory
!
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('property','wrk')
      deallocate(ipivot,stat=status)
      if (status/=0) call deallocate_error('property','ipivot')
      deallocate(dpacked,stat=status)
      if (status/=0) call deallocate_error('property','dpacked')
!
      if (ifail.gt.0) then
        nwarn = nwarn + 1
        call outwarning('Properties cannot be calculated - matrix is singular',0_i4)
        goto 999
      endif
!
!  Multiply inverse second derivatives by charge vectors
!
      do i = 1,3
        do j = 1,3
          sum = 0.0_dp
          do k = 1,nsfoc
            qak = qshell(k)
            indk = 3*(k-1) + i
            do l = 1,nsfoc
              indl = 3*(l-1) + j
              sum = sum + dervi(indl,indk)*qak*qshell(l)
            enddo
          enddo
          diconh(j,i) = dlcfactor*sum
        enddo
        diconh(i,i) = diconh(i,i) + 1.0_dp
      enddo
!
!  Calculate refractive indices
!
      do i = 1,3
        do j = 1,3
          r33(j,i) = diconh(j,i)
        enddo
      enddo
      call dsyev('N','U',3_i4,r33,3_i4,r3,w1l,9_i4,ifail)
      hfrefind(1) = sqrt(abs(r3(1)))
      hfrefind(2) = sqrt(abs(r3(2)))
      hfrefind(3) = sqrt(abs(r3(3)))
      hfrefind(1) = sign(hfrefind(1),r3(1))
      hfrefind(2) = sign(hfrefind(2),r3(2))
      hfrefind(3) = sign(hfrefind(3),r3(3))
    else
      do i = 1,3
        do j = 1,3
          diconh(j,i) = 0.0_dp
        enddo
        diconh(i,i) = 1.0_dp
        hfrefind(i) = 1.0_dp
      enddo
    endif
  endif
!**********************
!  Output properties  *
!**********************
  if (lprint.and.ioproc) then
    if (lelastic.and.abs(elcon(1,1)).gt.zero) then
      write(ioout,'(/)')
      if (index(keyword,'oldu').ne.0) then
        write(ioout,'(''  Elastic Constant Matrix: (Units=10**11 Dyne/cm**2= 10 GPa)'',/)')
      else
        write(ioout,'(''  Elastic Constant Matrix: (Units=GPa)'',/)')
      endif
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Indices      1         2         3         4         5         6    '')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      if (index(keyword,'oldu').ne.0) then
        do i = 1,6
          write(ioout,'(7x,i1,2x,6f10.5)')i,(0.1_dp*elcon(j,i),j=1,6)
        enddo
      else
        do i = 1,6
          write(ioout,'(7x,i1,2x,6f10.4)')i,(elcon(j,i),j=1,6)
        enddo
      endif
      write(ioout,'(''-------------------------------------------------------------------------------'')')
    endif
    if (lcompliances) then
      write(ioout,'(/)')
      write(ioout,'(''  Elastic Compliance Matrix: (Units=1/GPa)'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Indices      1         2         3         4         5         6    '')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      do i = 1,6
        write(ioout,'(7x,i1,2x,6f10.6)')i,(compliances(j,i),j=1,6)
      enddo
      write(ioout,'(''-------------------------------------------------------------------------------'')')
    endif
    if (lelastic) then
      write(ioout,'(/,''  Mechanical properties :'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Convention : '',19x,''Reuss'',9x,''Voigt'',9x,''Hill'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Bulk  Modulus (GPa)     = '',3(f13.5,1x))') &
        bulkmod_reuss,bulkmod_voigt,bulkmod_hill
      write(ioout,'(''  Shear Modulus (GPa)     = '',3(f13.5,1x))') &
        shearmod_reuss,shearmod_voigt,shearmod_hill
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Velocity S-wave (km/s)  = '',3(f13.5,1x))') vs_reuss,vs_voigt,vs_hill
      write(ioout,'(''  Velocity P-wave (km/s)  = '',3(f13.5,1x))') vp_reuss,vp_voigt,vp_hill
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      if (abs(bulkmod_reuss).gt.1.0d-8) then
        write(ioout,'(''  Compressibility (1/GPa) = '',f13.8)') 1.0_dp/bulkmod_reuss
      endif
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Stress axis :'',21x,''x'',13x,''y'',13x,''z'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Youngs Moduli (GPa)     = '',3(f13.5,1x))') (ym(i),i=1,3)
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Poissons Ratio (x)      = '',14x,2(f13.5,1x))')-compliances(2,1)*ym(2),-compliances(3,1)*ym(3)
      write(ioout,'(''  Poissons Ratio (y)      = '',f13.5,15x,f13.5)')-compliances(2,1)*ym(1),-compliances(3,2)*ym(3)
      write(ioout,'(''  Poissons Ratio (z)      = '',2(f13.5,1x))')-compliances(3,1)*ym(1),-compliances(3,2)*ym(2)
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(/)')
      if (lcml) then
        call gulp_cml_output_derivs(elcon, compliances, bulkmod_reuss, bulkmod_voigt, bulkmod_hill,  &
             shearmod_reuss, shearmod_voigt, shearmod_hill, vs_reuss, vs_voigt, vs_hill,  &
             vp_reuss, vp_voigt, vp_hill)
      endif
    endif
    if (lpiezo) then
      write(ioout,'(''  Piezoelectric Strain Matrix: (Units=C/m**2)'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Indices      1         2         3         4         5         6    '')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(7x,''x'',2x,6f10.5)')(piezo(i,1),i=1,6)
      write(ioout,'(7x,''y'',2x,6f10.5)')(piezo(i,2),i=1,6)
      write(ioout,'(7x,''z'',2x,6f10.5)')(piezo(i,3),i=1,6)
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(/)')
      write(ioout,'(''  Piezoelectric Stress Matrix: (Units=10**-11 C/N)'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Indices      1         2         3         4         5         6    '')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(7x,''x'',2x,6f10.5)')(100.0_dp*piezs(i,1),i=1,6)
      write(ioout,'(7x,''y'',2x,6f10.5)')(100.0_dp*piezs(i,2),i=1,6)
      write(ioout,'(7x,''z'',2x,6f10.5)')(100.0_dp*piezs(i,3),i=1,6)
      write(ioout,'(''-------------------------------------------------------------------------------'')')
    endif
    if (ldlcs) then
      write(ioout,'(/)')
      write(ioout,'(''  Static dielectric constant tensor : '',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(10x,4x,''x'',9x,''y'',9x,''z'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(7x,''x'',2x,3f10.5)')(dicons(1,i),i=1,3)
      write(ioout,'(7x,''y'',2x,3f10.5)')(dicons(2,i),i=1,3)
      write(ioout,'(7x,''z'',2x,3f10.5)')(dicons(3,i),i=1,3)
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
    if (ldlch.and.(nshell.gt.0)) then
      write(ioout,'(''  High frequency dielectric constant tensor : '',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(10x,4x,''x'',9x,''y'',9x,''z'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(7x,''x'',2x,3f10.5)')(diconh(1,i),i=1,3)
      write(ioout,'(7x,''y'',2x,3f10.5)')(diconh(2,i),i=1,3)
      write(ioout,'(7x,''z'',2x,3f10.5)')(diconh(3,i),i=1,3)
      write(ioout,'(''-------------------------------------------------------------------------------'',/)')
    endif
    if (ldlcs) then
      write(ioout,'(''  Static refractive indices : '',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(4x,''1 = '',f10.5,6x,''2 = '',f10.5,6x,''3 = '',f10.5)')(srefind(i),i=1,3)
      write(ioout,'(''-------------------------------------------------------------------------------'',/)')
    endif
    if (ldlch.and.(nshell.gt.0)) then
      write(ioout,'(''  High frequency refractive indices : '',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(4x,''1 = '',f10.5,6x,''2 = '',f10.5,6x,''3 = '',f10.5)')(hfrefind(i),i=1,3)
      write(ioout,'(''-------------------------------------------------------------------------------'',/)')
    endif
    if (lcml) then
      if (ldlch.and.(nshell.gt.0).and.(ldlcs).and.(lpiezo)) then
          call gulp_cml_output_dielectric(sdct=dicons, srind=srefind, &
                 & hfct=diconh, hrind=hfrefind, piezo=piezo, piezs=piezs)
      elseif (ldlch.and.(nshell.gt.0).and.(ldlcs)) then
          call gulp_cml_output_dielectric(sdct=dicons, srind=srefind, &
                 & hfct=diconh, hrind=hfrefind)
      elseif ( (ldlcs).and.(lpiezo) ) then
          call gulp_cml_output_dielectric(sdct=dicons, srind=srefind, piezo=piezo, piezs=piezs)
      elseif (ldlcs) then
          call gulp_cml_output_dielectric(sdct=dicons, srind=srefind)
      endif
    endif
  endif
!
!  Flush the output buffer
!
  call gflush(ioout)
!***************
!  Exit tasks  *
!***************
999 continue
!
!  Timings
!
  t2p = cputime()
  tprop = t2p - t1p + tprop
!
!  Deallocate memory
!
  if (lpocc) then
    deallocate(d3,stat=status)
    if (status/=0) call deallocate_error('property','d3')
  endif
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('property','tmp')
  deallocate(qshell,stat=status)
  if (status/=0) call deallocate_error('property','qshell')
  deallocate(qfo,stat=status)
  if (status/=0) call deallocate_error('property','qfo')
!
  return
  end
