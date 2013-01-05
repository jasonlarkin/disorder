  subroutine fenergy3(fc,lgrad1,hessian,lkeepd2)
!
!  Supplies the function and first derivatives of the free energy
!  for a solid.
!
!  NOTE : BSM not allowed for!
!
!   8/97 Created from phonon to replace old routine from 
!        numerical version.
!   9/97 Check on change of number of active frequencies added
!        across all k points
!   9/97 Modified to handle quasiharmonic approximation
!   9/97 Separate array for storage of on-diagonal blocks to
!        avoid overwriting of derv3
!   9/97 Hessian now used to save half of derv2 instead of disk
!        and tmat is recalculated instead of stored - OK for
!        .not.lsymderv2 case
!   7/98 Exclusion of zero point energy add
!   8/98 Four-body modifications added
!   6/99 Partial occupancy modifications added
!   8/99 Size of d34 increased to allow for manybody case
!   6/00 iocptr/ibocptr made local to avoid overwriting
!   6/00 Dimensions of d33s in f90 allocation corrected as this
!        was leading to an out of bounds write occuring.
!   6/00 nword increased for numat < 4 to avoid overwriting with 
!        fourbody potentials 
!   9/01 Code modified to handle small temperatures correctly
!   5/02 freq now referenced as a 2-D array
!   8/02 cmat usage changed to be compatable with phonon.f mods
!   3/03 compressd2/3 now used for compactness of code
!  11/03 Automatic resizing of maxmany/2 added
!   5/06 Mass now uses species values
!   5/07 Partial occupancy data moved to module
!  11/07 Unused variables removed
!   1/08 lgrad2 removed as an argument
!   6/09 Module name changed from three to m_three
!   6/12 Calls to phoncopy routines changed to add extra argument
!
!  nkpt    = total number of k points across all structures
!  nlkpt   = pointer to lowest k point
!  nukpt   = pointer to upper k point
!  xkpt    = fractional x component of k point
!  ykpt    = fractional y component of k point
!  zkpt    = fractional z component of k point
!  wkpt    = weight of each k point
!  nkptcfg = configuration pointer for each k point
!  sumwkpt = sum over weights of k points
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
!  Julian Gale, NRI, Curtin University, June 2012
!
  use constants
  use control
  use current
  use derivatives
  use element
  use energies
  use feworkspace
  use four
  use frequencies
  use general,     only : nwarn
  use iochannels
  use ksample
  use m_three
  use parallel
  use partial
  use shell
  use species,     only : massspec, natspec, ntypspec
  use sutton
  use times
  implicit none
!
!  Passed variables
!
  logical,      intent(in)                      :: lgrad1
  logical,      intent(in)                      :: lkeepd2
  real(dp),     intent(inout)                   :: fc
  real(dp),     intent(out)                     :: hessian(*)
!
!  Local variables
!
  character(len=5)                              :: lab1
  complex(dpc), dimension(:), allocatable       :: ctmp
  integer(i4)                                   :: i
  integer(i4)                                   :: ifail
  integer(i4)                                   :: ii
  integer(i4)                                   :: ind
  integer(i4)                                   :: indi
  integer(i4)                                   :: inert(3)
  integer(i4)                                   :: ix
  integer(i4)                                   :: iy
  integer(i4)                                   :: iz
  integer(i4)                                   :: j
  integer(i4)                                   :: job
  integer(i4)                                   :: k
  integer(i4)                                   :: l
  integer(i4)                                   :: maxlim
  integer(i4)                                   :: mcv
  integer(i4)                                   :: mcvmax
  integer(i4)                                   :: mcvmin
  integer(i4)                                   :: mcvmintot
  integer(i4)                                   :: mint
  integer(i4)                                   :: msv
  integer(i4)                                   :: nk
  integer(i4)                                   :: nlkpt
  integer(i4)                                   :: nsi
  integer(i4)                                   :: nukpt
  integer(i4)                                   :: nwords
  integer(i4)                                   :: status
  logical                                       :: lfirstkpt
  logical                                       :: lmanybodyderv
  logical                                       :: lnozeropt
  real(dp),     dimension(:), allocatable       :: cmat
  real(dp)                                      :: cmfact
  real(dp)                                      :: cputime
  real(dp)                                      :: det(2)
  real(dp),     dimension(:), allocatable       :: dsqh
  real(dp),     dimension(:), allocatable       :: dfdw2
  real(dp)                                      :: exptrm
  real(dp)                                      :: fetrm
  real(dp)                                      :: fscale
  real(dp)                                      :: rkptcmfct
  real(dp)                                      :: rkptfct
  real(dp)                                      :: rmassi
  real(dp)                                      :: rnokpt
  real(dp)                                      :: sumwkpt
  real(dp)                                      :: t1i
  real(dp)                                      :: t2i
  real(dp),     dimension(:), allocatable       :: w1
  real(dp),     dimension(:), allocatable       :: w2
  real(dp)                                      :: wi
  real(dp)                                      :: wk
  real(dp)                                      :: wr
  real(dp)                                      :: zpe
!
!  Check that there are no breathing shells
!
  if (nbsmat.gt.0) then
    call outerror('breathing shells not allowed for FEM',0_i4)
    call stopnow('fenergy3')
  endif
!**********************************
!  Set local variables and flags  *
!**********************************
  mint = 3*numat
  maxlim = mint
  msv = 3*nsfoc
  mcv = 3*ncfoc
!
!  Allocate memory that depends on mcv
!
  allocate(dfdw2(mcv),stat=status)
  if (status/=0) call outofmemory('fenergy3','dfdw2')
!
  fscale = sqrt(1.0d23*evtoj*avogadro)
  fscale = fscale/(2.0_dp*pi*speedl)
  if (temperature.gt.1.0d-6) then
    cmfact = planck*speedl/(boltz*temperature)
  endif
  evib = 0.0_dp
  lnozeropt = (index(keyword,'noze').ne.0)
  lmanybodyderv = ((nthb+nfor).gt.0.or.lsuttonc)
!*******************************************************
!  Store contents of second derivative arrays on disk  *
!*******************************************************
!
!  Second derivatives of U needed to approximate hessian
!
  if (lkeepd2) then
    ind = 0
    do i = 1,maxlim
      do j = 1,i
        ind = ind + 1
        hessian(ind) = derv2(j,i)
      enddo
    enddo
  endif
!******************************************
!  Allocate memory for 3-body 3rd derivs  *
!******************************************
  if (lmanybodyderv) then
    maxmany = max(2*numat,54)
    if (lsuttonc) then
      maxmany2 = numat*(numat + 1)
    else
      maxmany2 = maxmany
    endif
    call changemaxmany
  endif
!************************************
!  Find K points for configuration  *
!************************************
  nlkpt = 0
  do i = 1,nkpt
    nk = nkptcfg(i)
    if (nlkpt.eq.0.and.nk.eq.ncf) nlkpt = i
    if (nk.eq.ncf) nukpt = i
  enddo
!
!  No k points found for current structure, so return
!
  if (nlkpt.eq.0) goto 999
!********************************************
!  Calculate inverse square root of masses  *
!********************************************
  do i = 1,ncfoc
    mass(i) = 0.0_dp
  enddo
  do i = 1,ncore
    ii = iocptr(i)
    nsi = nspecptr(nrelat(i))
    rmassi = massspec(nsi)*occuf(i)
    if (abs(rmassi).lt.1.0d-12) then
      call label(natspec(nsi),ntypspec(nsi),lab1)
      call outerror('mass of species '//lab1//' is zero',0_i4)
      goto 999
    endif
    mass(ii) = mass(ii) + rmassi
  enddo
  do i = 1,ncfoc
    if (mass(i).eq.0.0_dp) then
      call outerror('site has total mass of zero in phonon',0_i4)
      call stopnow('fenergy3')
    endif
    rmass(i) = 1.0_dp/sqrt(mass(i))
  enddo
!
!  Check normalisation of weighting factors 
!
  sumwkpt = 0.0_dp
  do i = nlkpt,nukpt
    sumwkpt = sumwkpt + wkpt(i)
  enddo
  rnokpt = 1.0_dp/sumwkpt
  rkptfct = rnokpt*boltz*temperature/evtoj
  rkptcmfct = rnokpt*planck*speedl/evtoj
!***************************************
!  Allocate memory for complex matrix  *
!***************************************
  nwords = 2*maxd2*maxd2
  allocate(cmat(nwords),stat=status)
  if (status/=0) call outofmemory('fenergy3','cmat')
  if (lzsisa) then
    allocate(dsqh(nstrains*maxd2),stat=status)
    if (status/=0) call outofmemory('fenergy3','dsqh')
  endif
  mcvmintot = 0
!*************************************************
!  Store diagonal blocks to avoid recalculation  *
!*************************************************
  do i = 1,numat
    indi = 3*(i-1)
    diagblock(indi+1,1) = derv2(indi+1,indi+1)
    diagblock(indi+2,1) = derv2(indi+2,indi+1)
    diagblock(indi+3,1) = derv2(indi+3,indi+1)
    diagblock(indi+1,2) = derv2(indi+1,indi+2)
    diagblock(indi+2,2) = derv2(indi+2,indi+2)
    diagblock(indi+3,2) = derv2(indi+3,indi+2)
    diagblock(indi+1,3) = derv2(indi+1,indi+3)
    diagblock(indi+2,3) = derv2(indi+2,indi+3)
    diagblock(indi+3,3) = derv2(indi+3,indi+3)
  enddo
!****************************************************************
!  For ZSISA approximation mode build strain correction matrix  *
!****************************************************************
  ncsfoc = ncfoc + nsfoc
  if (lzsisa) then
    if (lpocc) then
!
!  Compress second derivatives according to partial occupancies
!
      call compressd2(derv2,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
      call compressd3(derv3,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
    endif
    call setquasiharm(dsqh,3_i4*ncsfoc)
  endif
!***********************
!  Loop over k points  *
!***********************
  do k = nlkpt,nukpt
!***************************************
!  Generate phased second derivatives  *
!***************************************
    lfirstkpt = (k.eq.nlkpt)
    call dynamic(k)
!
!  Change sign of imaginary part of dynamical matrix
!
    do i = 1,mint
      do j = 1,mint
        dervi(j,i) = - dervi(j,i)
      enddo
    enddo
    wk = wkpt(k)
!
!  Include diagonal blocks, stored in diagblock
!
    do i = 1,numat
      indi = 3*(i-1)
      derv2(indi+1,indi+1) = derv2(indi+1,indi+1) + diagblock(indi+1,1)
      derv2(indi+2,indi+1) = derv2(indi+2,indi+1) + diagblock(indi+2,1)
      derv2(indi+3,indi+1) = derv2(indi+3,indi+1) + diagblock(indi+3,1)
      derv2(indi+1,indi+2) = derv2(indi+1,indi+2) + diagblock(indi+1,2)
      derv2(indi+2,indi+2) = derv2(indi+2,indi+2) + diagblock(indi+2,2)
      derv2(indi+3,indi+2) = derv2(indi+3,indi+2) + diagblock(indi+3,2)
      derv2(indi+1,indi+3) = derv2(indi+1,indi+3) + diagblock(indi+1,3)
      derv2(indi+2,indi+3) = derv2(indi+2,indi+3) + diagblock(indi+2,3)
      derv2(indi+3,indi+3) = derv2(indi+3,indi+3) + diagblock(indi+3,3)
    enddo
!*****************************************************************
!  Compress second derivatives according to partial occupancies  *
!*****************************************************************
    if (lpocc) then
      ncsfoc = ncfoc + nsfoc
      call compressd2(derv2,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
      call compressd2(dervi,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
    endif
!**********************************
!  Eliminate shell contributions  *
!**********************************
    if (msv.gt.0) then
      allocate(w1(msv),stat=status)
      if (status/=0) call outofmemory('fenergy3','w1')
      allocate(w2(msv),stat=status)
      if (status/=0) call outofmemory('fenergy3','w2')
!*****************************
!  Complex Matrix Inversion  *
!*****************************
!
!  Transfer real and imaginary components to complex matrix 
!
      call phoncopy1(derv2,dervi,cmat,maxd2,mcv,mcv,msv,msv)
!
!  Invert complex matrix
!
      job = 1
      t1i = cputime()
      call zhifa(cmat,msv,msv,w1,ifail)
      if (ifail.ne.0) then
        call outerror('inversion of shell 2nd derivatives failed',0_i4)
        goto 999
      endif
      allocate(ctmp(msv),stat=status)
      if (status/=0) call outofmemory('fenergy3','ctmp')
      call zhidi(cmat,msv,msv,w1,det,inert,ctmp,job)
      deallocate(ctmp,stat=status)
      if (status/=0) call deallocate_error('fenergy3','ctmp')
      t2i = cputime()
      tmati = tmati + t2i - t1i
!
!  Return inverse complex matrix to separate real and imaginary matrices
!  and resymmetrise
!
      call phoncopy2e(derv2,dervi,cmat,maxd2,mcv,mcv,msv,msv)
!***********************************************
!  Corrected second derivatives = R - T*S-1*T  *
!***********************************************
!
!  First pass : S-1*T stored in diagonally opposite copy of T
!
      do i = 1,mcv
        do j = 1,msv
          wr = 0.0_dp
          wi = 0.0_dp
          do l = 1,msv
            wr = wr + derv2(l+mcv,j+mcv)*derv2(l+mcv,i)
            wr = wr + dervi(l+mcv,j+mcv)*dervi(l+mcv,i)
            wi = wi - dervi(l+mcv,j+mcv)*derv2(l+mcv,i)
            wi = wi + derv2(l+mcv,j+mcv)*dervi(l+mcv,i)
          enddo
          w1(j) = wr
          w2(j) = wi
        enddo
        do j = 1,msv
          derv2(mcv+j,i) = w1(j)
          dervi(mcv+j,i) = w2(j)
        enddo
      enddo
!
!  Second pass : T*(S-1*T) - for imaginary case sign has to be adjusted
!  to allow for the fact that T(conj) and T are of opposite signs
!
      do i = 1,mcv
        do j = 1,mcv
          wr = 0.0_dp
          wi = 0.0_dp
          do l = 1,msv
            wr = wr - derv2(j,l+mcv)*derv2(mcv+l,i)
            wr = wr + dervi(j,l+mcv)*dervi(mcv+l,i)
            wi = wi - derv2(j,l+mcv)*dervi(mcv+l,i)
            wi = wi - dervi(j,l+mcv)*derv2(mcv+l,i)
          enddo
          derv2(j,i) = derv2(j,i) + wr
          dervi(j,i) = dervi(j,i) + wi
        enddo
      enddo
      deallocate(w2,stat=status)
      if (status/=0) call deallocate_error('fenergy3','w2')
      deallocate(w1,stat=status)
      if (status/=0) call deallocate_error('fenergy3','w1')
    endif
!****************************
!  End of shell correction  *
!****************************
!
!  Multiply by mass-factors
!
    allocate(w1(3*ncfoc),stat=status)
    if (status/=0) call outofmemory('fenergy3','w1')
    ix = - 2
    iy = - 1
    iz =   0
    do i = 1,ncfoc
      ix = ix + 3
      iy = iy + 3
      iz = iz + 3
      w1(ix) = rmass(i)
      w1(iy) = rmass(i)
      w1(iz) = rmass(i)
    enddo
    do i = 1,mcv
      rmassi = w1(i)
      do j = 1,mcv
        derv2(j,i) = rmassi*w1(j)*derv2(j,i)
        dervi(j,i) = rmassi*w1(j)*dervi(j,i)
      enddo
    enddo
    deallocate(w1,stat=status)
    if (status/=0) call deallocate_error('fenergy3','w1')
!*********************
!  Debugging output  *
!*********************
    if (index(keyword,'dynam').ne.0.and.ioproc) then
      write(ioout,'(/,''  Real Dynamical matrix :'',/)')
      do i = 1,mcv
        write(ioout,'(9f12.8)')(derv2(i,j),j=1,mcv)
      enddo
      write(ioout,'(/,''  Imaginary Dynamical matrix :'',/)')
      do i = 1,mcv
        write(ioout,'(9f12.8)')(dervi(i,j),j=1,mcv)
      enddo
    endif
!************************************
!  Diagonalise dynamical matrix     *
!  => Eigenvectors and eigenvalues  *
!************************************
    ifail = 0
    call diagdyn(mcv,maxd2,mcvmin,freq(1,k-nlkpt+1),fscale,cmat(1),cmat(maxd2*maxd2+1),ifail)
    mcvmintot = mcvmintot + mcvmin
    mcvmax = mcv
    if (minmode.ne.1) mcvmin = minmode
    if (maxmode.ne.0) mcvmax = maxmode
    if (index(keyword,'verb').ne.0.and.ioproc) then
      write(ioout,'(/,''  Minimum mode number for K point '',i5,'' = '',i5)') k-nlkpt+1,mcvmin
      write(ioout,'(''  Maximum mode number for K point '',i5,'' = '',i5,/)') k-nlkpt+1,mcvmax
    endif
!**************************************
!  Calculate transformation matrices  *
!  and contributions to free energy   *
!**************************************
    fetrm = 0.0_dp
    zpe = 0.0_dp
!
!  Calculate (1/2w).(dG/dw) => dfdw2
!
    if (lnozeropt) then
      do i = mcvmin,mcvmax
        if (temperature.gt.1.0d-6) then
          exptrm = exp(-freq(i,k-nlkpt+1)*cmfact)
        else
          exptrm = 0.0_dp
        endif
        if (exptrm.lt.1.0_dp) then
          fetrm = fetrm + log(1.0_dp-exptrm)
          dfdw2(i) = wk*rkptcmfct*(exptrm/(1.0_dp-exptrm))/(2.0_dp*freq(i,k-nlkpt+1))
!
!  Sqrt dfdw2 so that it can be convoluted into the eigenvectors 
!  to save time on the projection of the third derivatives
!
          dfdw2(i) = sqrt(dfdw2(i))*fscale
        else
          dfdw2(i) = 0.0_dp
        endif
      enddo
    else
      do i = mcvmin,mcvmax
        if (temperature.gt.1.0d-6) then
          exptrm = exp(-freq(i,k-nlkpt+1)*cmfact)
        else
          exptrm = 0.0_dp
        endif
        zpe = zpe + wk*freq(i,k-nlkpt+1)*rkptcmfct
        if (exptrm.lt.1.0_dp) then
          fetrm = fetrm + log(1.0_dp-exptrm)
          dfdw2(i) = wk*rkptcmfct*(0.5_dp+exptrm/(1.0_dp-exptrm))/(2.0_dp*freq(i,k-nlkpt+1))
!
!  Sqrt dfdw2 so that it can be convoluted into the eigenvectors 
!  to save time on the projection of the third derivatives
!
          dfdw2(i) = sqrt(dfdw2(i))*fscale
        else
          dfdw2(i) = 0.0_dp
        endif
      enddo
    endif
    zpe = 0.5_dp*zpe
    evib = evib + zpe + fetrm*wk*rkptfct
    if (lgrad1) then
!
!  Scale eigenvectors by sqrt(dfdw2)
!
      call scaleevec(mcv,cmat(1),maxd2,dfdw2)
      call scaleevec(mcv,cmat(maxd2*maxd2+1),maxd2,dfdw2)
!
!  Calculate shell projection matrices
!
!  Two components to store real/imaginary
!  
!  Msc => already stored in derv2(mcv+j,i)/dervi(mcv+j,i)
!  Pns => store in derv2(i,mcv+j)/dervi(i,mcv+j)
!      =  Enc.Mcs, where Enc = eigenvector for mode n
!
      if (msv.gt.0) then
!
!  Scale Msc by mass factor for use in derivatives
!
        ix = - 2
        iy = - 1
        iz =   0
        do i = 1,ncfoc
          rmassi = rmass(i)
          ix = ix + 3
          iy = iy + 3
          iz = iz + 3
          do j = 1,msv
            derv2(mcv+j,ix) = rmassi*derv2(mcv+j,ix)
            derv2(mcv+j,iy) = rmassi*derv2(mcv+j,iy)
            derv2(mcv+j,iz) = rmassi*derv2(mcv+j,iz)
            dervi(mcv+j,ix) = rmassi*dervi(mcv+j,ix)
            dervi(mcv+j,iy) = rmassi*dervi(mcv+j,iy)
            dervi(mcv+j,iz) = rmassi*dervi(mcv+j,iz)
          enddo
        enddo
        call setfeshtmat(mcv,msv,derv2,dervi,cmat(1),cmat(maxd2*maxd2+1),maxd2)
      endif
!*********************
!  Debugging output  *
!*********************
      if (index(keyword,'dynam').ne.0.and.msv.gt.0.and.ioproc) then
        write(ioout,'(/,''  Msc (real) :'',/)')
        do i = 1,mcv
          write(ioout,'(12f11.6)')(derv2(mcv+j,i),j=1,msv)
        enddo
        write(ioout,'(/,''  Msc (imag) :'',/)')
        do i = 1,mcv
          write(ioout,'(12f11.6)')(dervi(mcv+j,i),j=1,msv)
        enddo
        write(ioout,'(/,''  Pns (real) :'',/)')
        do i = 1,mcv
          write(ioout,'(12f11.6)')(derv2(i,mcv+j),j=1,msv)
        enddo
        write(ioout,'(/,''  Pns (imag) :'',/)')
        do i = 1,mcv
          write(ioout,'(12f11.6)')(dervi(i,mcv+j),j=1,msv)
        enddo
      endif
!********************************
!  Evaluate phonon derivatives  *
!********************************
      call realrecip3d3(k,mcvmin,mcvmax,cmat(1),cmat(maxd2*maxd2+1),dsqh,maxd2,lfirstkpt,iocptr)
    endif
!***************************
!  End loop over K points  *
!***************************
  enddo
  if (nummode.eq.0) then
    nummode = mcvmintot
  elseif (nummode.ne.mcvmintot) then
    if (ioproc) then
      write(ioout,'(''**** Warning - number of modes has changed to '',i8,'' ****'')')(mcv+1)*nkpt-mcvmintot
    endif
    nwarn = nwarn + 1
    nummode = mcvmintot
  endif
!
!  Sum vibration contribution and internal energy
!
  fc = fc + evib
!***************
!  Exit point  *
!***************
999 continue
!****************
!  Free memory  *
!****************
  if (lzsisa.and.allocated(dsqh)) then
    deallocate(dsqh,stat=status)
    if (status/=0) call deallocate_error('fenergy3','dsqh')
  endif
  if (allocated(cmat)) then
    deallocate(cmat,stat=status)
    if (status/=0) call deallocate_error('fenergy3','cmat')
  endif
!
!  Free local memory
!
  deallocate(dfdw2,stat=status)
  if (status/=0) call deallocate_error('fenergy3','dfdw2')
!*************************************************
!  Recover contents of second derivative arrays  *
!*************************************************
!
!  Re-generate transformation matrix if needed for optimisation
!  This is generally faster than saving it to disk
!
  if ((lopt.or.lrelax).and.lkeepd2) then
    call transmat
  endif
!
!  Second derivatives of U needed to approximate hessian
!
  if (lkeepd2) then
    ind = 0
    do i = 1,maxlim
      do j = 1,i
        ind = ind + 1
        derv2(j,i) = hessian(ind)
        derv2(i,j) = hessian(ind)
      enddo
    enddo
  endif
!
  return
  end
