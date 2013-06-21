  subroutine fenergy0(fc,lgrad1,hessian,lkeepd2)
!
!  Supplies the function and first derivatives of the free energy
!  for a molecule.
!
!   8/97 Created from fenergy3
!   9/97 Array add for storage of three body third derivative terms
!   9/97 Hessian now used to save half of derv2 instead of disk
!        and tmat is recalculated instead of stored - OK for
!        .not.lsymderv2 case
!   5/98 Three-body derivatives completed
!   7/98 Four-body modifications added
!   7/98 Exclusion of zero point energy add
!   8/98 Array d34 added for four-body terms
!   6/99 Partial occupancy modifications added
!   8/99 Size of d34 increased to allow for manybody case
!   6/00 iocptr/ibocptr made local to avoid overwriting
!   6/00 nword increased for numat < 4 to avoid overwriting with
!        fourbody potentials
!   6/00 breathing shell phonon calculation re-enabled, but FE
!        derivatives not yet available
!   9/01 Code modified to handle small temperatures correctly
!   5/02 freq now referenced as a 2-D array
!   3/03 compressd2 used for compactness of code
!  11/03 Workspace arrays moved into array for resizing
!  10/04 Eispack calls replaced with lapack
!   5/06 Mass now uses species values
!   5/07 Partial occupancy data moved to module
!  11/07 Unused variables removed
!   1/08 lgrad2 removed as an argument
!   6/09 Module name changed from three to m_three
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
  use constants
  use control
  use current
  use derivatives
  use element
  use energies
  use feworkspace
  use four
  use frequencies
  use general,      only : nwarn
  use iochannels
  use m_three
  use parallel
  use partial
  use shell
  use species,      only : massspec, natspec, ntypspec
  use sutton
  use times
  implicit none
!
!  Passed variables
!
  logical                                      :: lgrad1
  logical                                      :: lkeepd2
  real(dp)                                     :: fc
  real(dp)                                     :: hessian(*)
!
!  Local variables
!
  character(len=5)                             :: lab1
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4)                                  :: inert(3)
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: job
  integer(i4)                                  :: k
  integer(i4)                                  :: l
  integer(i4)                                  :: maxlim
  integer(i4)                                  :: mcv
  integer(i4)                                  :: mcvmax
  integer(i4)                                  :: mcvmin
  integer(i4)                                  :: mint
  integer(i4)                                  :: msv
  integer(i4)                                  :: nimag
  integer(i4)                                  :: nsi
  integer(i4)                                  :: status
  logical                                      :: linear
  logical                                      :: lmanybodyderv
  logical                                      :: lnozeropt
  real(dp)                                     :: cmfact
  real(dp)                                     :: cputime
  real(dp)                                     :: det(2)
  real(dp),    dimension(:), allocatable       :: dfdw2
  real(dp)                                     :: exptrm
  real(dp)                                     :: fetrm
  real(dp)                                     :: fscale
  real(dp)                                     :: rkptcmfct
  real(dp)                                     :: rkptfct
  real(dp)                                     :: rmassi
  real(dp)                                     :: rt
  real(dp)                                     :: t1d
  real(dp)                                     :: t1i
  real(dp)                                     :: t2d
  real(dp)                                     :: t2i
  real(dp)                                     :: wr
  real(dp),    dimension(:), allocatable       :: w1
  real(dp),    dimension(:), allocatable       :: w2
  real(dp)                                     :: zpe
!
!  Check that there are no breathing shells
!
  if (nbsmat.gt.0) then
    call outerror('breathing shells not allowed for FEM',0_i4)
    call stopnow('fenergy0')
  endif
!**********************************
!  Set local variables and flags  *
!**********************************
  mint = 3*numat
  maxlim = mint
  if (nbsmat.gt.0) maxlim = maxlim + numat
  msv = 3*nsfoc + nbfoc
  mcv = 3*ncfoc
!
!  Allocate array that depends on mcv
!
  allocate(dfdw2(mcv),stat=status)
  if (status/=0) call outofmemory('fenergy0','dfdw2')
!
  fscale = sqrt(1.0d23*evtoj*avogadro)
  fscale = fscale/(2.0_dp*pi*speedl)
  if (temperature.gt.1.0d-6) then
    cmfact = planck*speedl/(boltz*temperature)
  endif
  rkptfct = boltz*temperature/evtoj
  rkptcmfct = planck*speedl/evtoj
  lmanybodyderv = ((nthb+nfor).gt.0.or.lsuttonc)
  lnozeropt = (index(keyword,'noze').ne.0)
!***********************************************
!  Store contents of second derivative arrays  *
!***********************************************
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
!*********************************************
!  Allocate memory for many-body 3rd derivs  *
!*********************************************
  if (lmanybodyderv) then
    maxmany = numat
    if (lsuttonc) then
      maxmany2 = numat*(numat - 1)/2
    else
      maxmany2 = numat
    endif
    call changemaxmany
  endif
!*********************
!  Debugging output  *
!*********************
  if (index(keyword,'dynam').ne.0.and.ioproc) then
    write(ioout,'(/,''  Second derivative matrix:'',/)')
    do i = 1,mcv+msv
      write(ioout,'(12f11.6)')(derv2(i,j),j=1,mcv+msv)
    enddo
  endif
!********************************************
!  Calculate inverse square root of masses  *
!********************************************
  do i = 1,ncfoc
    mass(i) = 0.0_dp
  enddo
  do i = 1,ncore
    ii = iocptr(i)
    nsi = nspecptr(i)
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
      call stopnow('fenergy0')
    endif
    rmass(i) = 1.0_dp/sqrt(mass(i))
  enddo
!*****************************************************************
!  Compress second derivatives according to partial occupancies  *
!*****************************************************************
  if (lpocc) then
    ncsfoc = ncfoc + nsfoc
    call compressd2(derv2,maxd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
  endif
!**********************************
!  Eliminate shell contributions  *
!**********************************
  if (msv.gt.0) then
!*********************
!  Matrix Inversion  *
!*********************
!
!  Copy shell-shell matrix
!
    do i = 1,msv
      do j = 1,msv
        dervi(j,i) = derv2(mcv+j,mcv+i)
      enddo
    enddo
!
!  Invert matrix
!
    job = 1
    t1i = cputime()
    allocate(itmp(msv),stat=status)
    if (status/=0) call outofmemory('fenergy0','itmp')
    call dsifa(dervi,maxd2,msv,itmp,ifail)
    if (ifail.ne.0) then
      call outerror('inversion of shell 2nd derivatives failed',0_i4)
      goto 999
    endif
    allocate(w2(msv),stat=status)
    if (status/=0) call outofmemory('fenergy0','w2')
    call dsidi(dervi,maxd2,msv,itmp,det,inert,w2,job)
    deallocate(w2,stat=status)
    if (status/=0) call deallocate_error('fenergy0','w2')
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('fenergy0','itmp')
    t2i = cputime()
    tmati = tmati + t2i - t1i
!
!  Symmetrise inverse matrix
!
    do i = 1,msv
      do j = 1,i
        dervi(i,j) = dervi(j,i)
      enddo
    enddo
!***********************************************
!  Corrected second derivatives = R - T*S-1*T  *
!***********************************************
!
!  First pass : S-1*T stored in diagonally opposite copy of T
!
    do i = 1,mcv
      do j = 1,msv
        wr = 0.0_dp
        do l = 1,msv
          wr = wr + dervi(j,l)*derv2(i,l+mcv)
        enddo
        derv2(mcv+j,i) = wr
      enddo
    enddo
!
!  Second pass : T*(S-1*T) 
!
    do i = 1,mcv
      do j = 1,mcv
        wr = 0.0_dp
        do l = 1,msv
          wr = wr - derv2(j,l+mcv)*derv2(mcv+l,i)
        enddo
        derv2(j,i) = derv2(j,i) + wr
      enddo
    enddo
  endif
!****************************
!  End of shell correction  *
!****************************
!
!  Multiply by mass-factors
!
  do ii = 1,3
    indi = ii - 3
    do jj = 1,3
      indj = jj - 3
      do i = 1,ncfoc
        rmassi = rmass(i)
        do j = 1,ncfoc
          derv2(indj+3*j,indi+3*i) = rmassi*rmass(j)*derv2(indj+3*j,indi+3*i)
        enddo
      enddo
    enddo
  enddo
!*********************
!  Debugging output  *
!*********************
  if (index(keyword,'dynam').ne.0.and.ioproc) then
    write(ioout,'(/,''  Real Dynamical matrix :'',/)')
    do i = 1,mcv
      write(ioout,'(12f11.6)')(derv2(i,j),j=1,mcv)
    enddo
  endif
!*****************************************************************
!  Diagonalise dynamical matrix => eigenvectors and eigenvalues  *
!*****************************************************************
  ifail = 0
  t1d = cputime()
  allocate(w1(3*mcv),stat=status)
  if (status/=0) call outofmemory('fenergy0','w1')
  dervi(1:mcv,1:mcv) = derv2(1:mcv,1:mcv)
  if (lgrad1) then
    call dsyev('V','U',mcv,dervi,maxd2,freq,w1,3_i4*mcv,ifail)
  else
    call dsyev('N','U',mcv,dervi,maxd2,freq,w1,3_i4*mcv,ifail)
  endif
  deallocate(w1,stat=status)
  if (status/=0) call deallocate_error('fenergy0','w1')
  t2d = cputime()
  tdiag = tdiag + t2d - t1d
!***************************************
!  Convert eigenvalues to frequencies  *
!***************************************
!
!  For molecule exclude rotations
!
  call moltype(linear)
  if (linear) then
    mcvmin = 6
  else
    mcvmin = 7
  endif
!
!  Check for imaginary modes and exclude them
!
!  Note that rotations tend to be imaginary modes 
!
  nimag = 0
  do i = 1,mcv
    if (freq(i,1).lt.-1.0d-3) nimag = nimag + 1
  enddo
  if (nimag.gt.3) mcvmin = mcvmin + (nimag - 3)
  mcvmax = mcv
  if (minmode.ne.1) mcvmin = minmode
  if (maxmode.ne.0) mcvmax = maxmode
  do i = mcvmin,mcvmax
    rt = freq(i,1)
    if (rt.ge.0.0_dp) then
      freq(i,1) = sqrt(rt)*fscale
    else
      rt = abs(rt)
      freq(i,1) = - sqrt(rt)*fscale
    endif
  enddo
  if (nummode.eq.0) then
    nummode = mcvmin
  elseif (nummode.ne.mcvmin) then
    nwarn = nwarn + 1
    if (ioproc) then
      write(ioout,'(''**** Warning - number of modes has changed during free energy minimisation ****'')')
    endif
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
        exptrm = exp(-freq(i,1)*cmfact)   
      else
        exptrm = 0.0_dp
      endif
      if (exptrm.lt.1.0_dp) then
        fetrm = fetrm + log(1.0_dp - exptrm)
        dfdw2(i) = rkptcmfct*(exptrm/(1.0_dp - exptrm))/(2.0_dp*freq(i,1))
        dfdw2(i) = sqrt(dfdw2(i))*fscale
      else
        dfdw2(i) = 0.0_dp
      endif
    enddo
  else 
    do i = mcvmin,mcvmax
      if (temperature.gt.1.0d-6) then
        exptrm = exp(-freq(i,1)*cmfact)   
      else
        exptrm = 0.0_dp
      endif
      zpe = zpe + freq(i,1)*rkptcmfct
      if (exptrm.lt.1.0_dp) then
        fetrm = fetrm + log(1.0_dp-exptrm)
        dfdw2(i) = rkptcmfct*(0.5_dp + exptrm/(1.0_dp - exptrm))/(2.0_dp*freq(i,1))
        dfdw2(i) = sqrt(dfdw2(i))*fscale
      else
        dfdw2(i) = 0.0_dp
      endif
    enddo
  endif
  zpe = 0.5_dp*zpe
  evib = zpe + fetrm*rkptfct
  fc = fc + evib
  if (lgrad1) then
!********************************
!  Evaluate phonon derivatives  *
!********************************
!
!  Scale eigenvectors by sqrt(dfdw2)
!
    call scaleevec(mcv,dervi,maxd2,dfdw2)
!
!  Calculate shell projection matrices 
!
!  Msc => already stored in derv2(mcv+j,i)
!  Pns => store in derv2(i,mcv+j)
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
        enddo
      enddo
!
!  Generate Pns from Msc and eigenvectors
!
      do i = 1,mcv
        do j = 1,msv
          derv2(i,mcv+j) = 0.0_dp
          do k = 1,mcv
            derv2(i,mcv+j) = derv2(i,mcv+j) + dervi(k,i)*derv2(mcv+j,k)
          enddo
        enddo
      enddo
    endif
!*********************
!  Debugging output  *
!*********************
    if (index(keyword,'dynam').ne.0.and.ioproc) then
      write(ioout,'(/,''  Eigenvectors :'',/)')
      do i = mcvmin,mcvmax
        write(ioout,'(12f11.6)')(dervi(j,i),j=1,mcv)
      enddo
      if (msv.gt.0) then
        write(ioout,'(/,''  Msc :'',/)')
        do i = 1,mcv
          write(ioout,'(12f11.6)')(derv2(mcv+j,i),j=1,msv)
        enddo
        write(ioout,'(/,''  Pns :'',/)')
        do i = 1,mcv
          write(ioout,'(12f11.6)')(derv2(i,mcv+j),j=1,msv)
        enddo
      endif
    endif
!
!  Real space component
!
    call real0d3(mcvmin,mcvmax,iocptr)
  endif
!***************
!  Exit point  *
!***************
999 continue
!****************
!  Free memory  *
!****************
  deallocate(dfdw2,stat=status)
  if (status/=0) call deallocate_error('fenergy0','dfdw2')
!*************************************************
!  Recover contents of second derivative arrays  *
!*************************************************
!
!  Re-generate transformation matrix if needed for optimisation
!  This is in general faster than saving to disk
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
