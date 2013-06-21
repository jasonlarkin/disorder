  subroutine densitysd
!
!  Subroutine for calculating MEAM density using symmetry
!
!  Freezing now included
!
!   4/09 Created from realsd
!   4/09 Screening function added
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
  use datatypes
  use eam,            only : lMEAM, lMEAMscreen, maxmeamcomponent, meam_Cmax
  use element
  use optimisation
  use parallel
  use realvectors
  use sutton
  use symmetry
  use times
  use two
  use vectors,        only : vector_pair
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifree
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: kvec
  integer(i4)                                  :: l
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nfree
  integer(i4)                                  :: nout
  integer(i4)                                  :: nouterloop
  integer(i4), dimension(:), allocatable       :: nptr
  integer(i4)                                  :: nreli
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: nvec
  integer(i4)                                  :: nvec0
  integer(i4)                                  :: status
  logical                                      :: lcspair
  logical                                      :: lmatch
  logical                                      :: lnonzeroSij
  logical                                      :: lorder12
  logical                                      :: lpartial
  logical                                      :: lself
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: dSikjdr(3)       ! Dummy argument for call to meamscreen - not used here
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: rcut2
  real(dp)                                     :: rcutfactor
  real(dp)                                     :: rp
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: Sij
  real(dp)                                     :: Sikj
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
  real(dp)                                     :: xik0
  real(dp)                                     :: yik0
  real(dp)                                     :: zik0
  real(dp)                                     :: xjk0
  real(dp)                                     :: yjk0
  real(dp)                                     :: zjk0
  type(vector_pair)                            :: vectorpair
!
!  Check that this call is needed
!
  if (.not.lsuttonc) return
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
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realsd','npotl')
  allocate(nptr(nasym),stat=status)
  if (status/=0) call outofmemory('realsd','nptr')
  allocate(sum(max(nstrains,numat)),stat=status)
  if (status/=0) call outofmemory('realsd','sum')
  allocate(sum2(max(nstrains,numat)),stat=status)
  if (status/=0) call outofmemory('realsd','sum2')
!
!  Generate pointer to non-frozen atoms
!
  if (lfreeze) then
    nfree = 0
    do i = 1,nasym
      if (lopf(i)) then
        nfree = nfree + 1
        nptr(nfree) = i
      endif
    enddo
  else
    nfree = nasym
    do i = 1,nasym
      nptr(i) = i
    enddo
  endif
!***************************************************************
!  Atomistic and real space electrostatic component of energy  *
!***************************************************************
!
!  Combine i/j loops into one for improved parallel efficiency
!
  if (nfree.gt.i4_limit.and.numat.gt.i4_limit) then
    call outerror('integer overflow in densitysd - change i4 to i8',0_i4)
    call stopnow('densitysd')
  endif
  nouterloop = nfree*numat
  outerloop: do nout = procid+1,nouterloop,nprocs
    ifree = ((nout-1)/numat)+1
    i = nptr(ifree)
!
!  Inner loop over second site
!
    xal = xalat(i)
    yal = yalat(i)
    zal = zalat(i)
    nati = iatn(i)
    ntypi = natype(i)
    oci = occua(i)
    nreli = nrel2(i)
!
!  Start of second atom loop
!
    j = nout - (ifree-1)*numat
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
      ntyp2 = ntypj
    else
      lorder12 = .false.
      nat1 = nat(j)
      nat2 = nati
      ntyp1 = ntypj
      ntyp2 = ntypi
    endif
!
!  Freeze flag
!
    xcrd = xclat(j) - xal
    ycrd = yclat(j) - yal
    zcrd = zclat(j) - zal
    ocj = occuf(j)
!
!  Possible core-shell pair flag
!
    lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Locate potential number
!
    rp = 0.0_dp
    npots = 0
    do n = 1,npote
      if (nptype(n).eq.19) then
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (rpot(n).gt.rp) rp = rpot(n)
          endif
        endif
      endif
    enddo
!
!  If no valid potentials and charge product is zero
!  then no need to search for distances
!
    if (npots.eq.0) cycle outerloop
    cut2 = rp*rp
!***********************
!  Find valid vectors  *
!***********************
    if (ndim.eq.3) then
      call rsearch3D(xcrd,ycrd,zcrd,.false.,lcspair,nreli,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
    elseif (ndim.eq.2) then
      call rsearch2D(xcrd,ycrd,zcrd,.false.,lcspair,nreli,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
    elseif (ndim.eq.1) then
      call rsearch1D(xcrd,ycrd,zcrd,.false.,lcspair,nreli,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
    endif
!
    if (nor.eq.0) cycle outerloop
!
!  Sqrt distances
!
    do l = 1,nor
      dist(l) = sqrt(dist(l))
    enddo
!*******************************
!  Loop over vectors from i-j  *
!*******************************
    norloop: do l = 1,nor
!*******************************
!  Compute unscreened density  *
!*******************************
      call twoden(l,l,npots,npotl,sctrm1,sctrm2,lorder12)
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
        rcut2 = rcutfactor*(dist(l))**2
!
!  Loop over atoms to search for images that may contribute to the screening
!
        k = 0
        do while (k.lt.numat.and.lnonzeroSij)
          k = k + 1
!
!  Set basic vectors between atoms
!
          xik0 = xclat(k) - xal
          yik0 = yclat(k) - yal
          zik0 = zclat(k) - zal
          xjk0 = xik0 - xtmp(l)
          yjk0 = yik0 - ytmp(l)
          zjk0 = zik0 - ztmp(l)
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
            call meamscreen(dist(l)**2,vectorpair%distance_pair1(kvec),vectorpair%distance_pair2(kvec),Sikj, &
                            dSikjdr,lpartial,.false.)
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
            endif
!
!  End loop over images of possible screening atoms
!
          enddo
!
!  End loop over possible screening atoms
!
        enddo
      endif
!
      if (lnonzeroSij) then
        if (lMEAM) then
          if (lorder12) then
            scrho(1:maxmeamcomponent,i) = scrho(1:maxmeamcomponent,i) + sctrm1(1:maxmeamcomponent)*ocj*Sij
          else
            scrho(1:maxmeamcomponent,i) = scrho(1:maxmeamcomponent,i) + sctrm2(1:maxmeamcomponent)*ocj*Sij
          endif
        else
          if (lorder12) then
            scrho(1,i) = scrho(1,i) + sctrm1(1)*ocj*Sij
          else
            scrho(1,i) = scrho(1,i) + sctrm2(1)*ocj*Sij
          endif
        endif
      endif
    enddo norloop
  enddo outerloop
!****************
!  Global sums  *
!****************
  tsum0 = cputime()
  if (nprocs.gt.1) then
    if (lMEAM) then
      do j = 1,maxmeamcomponent
        do i = 1,numat
          sum2(i) = scrho(j,i)
        enddo
        call sumall(sum2,sum,numat,"realsd","scrho")
        do i = 1,numat
          scrho(j,i) = sum(i)
        enddo
      enddo
    else
      do i = 1,numat
        sum2(i) = scrho(1,i)
      enddo
      call sumall(sum2,sum,numat,"realsd","scrho")
      do i = 1,numat
        scrho(1,i) = sum(i)
      enddo
    endif
  endif
  tsuml = cputime() - tsum0
  tsum = tsum + tsuml
!
!  Free local memory
!
  deallocate(sum2,stat=status)
  if (status/=0) call deallocate_error('realsd','sum2')
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('realsd','sum')
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('realsd','nptr')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realsd','npotl')
!
!  Timing
!
  time2 = cputime()
  trls = trls + time2 - time1 - tsuml
!
  return
  end
