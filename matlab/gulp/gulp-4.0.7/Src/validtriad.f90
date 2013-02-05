  subroutine validtriad(i,j,k,nati,ntypi,natj,ntypj,natk,ntypk,r21,r31,r32,nthbpot,ndim, &
                        ii,jj,imax,jmax,kmax,lvalid)
!
!  Checks to see if this is a valid triad with this permutation of atoms.
!
!   6/00 Created from three0d3
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   5/01 Minimum cut-offs added
!   9/01 lmolq calculations accelerated using lneedmol 
!  11/02 Wildcard atom type added
!  12/02 Modified for correct handling of bonding above 0-D case
!  11/04 Style update & intent added
!   2/07 Bonding types added
!  12/07 Unused variables removed
!   6/08 Checking of bond numbers added
!   6/09 Module name changed from three to m_three
!
!  On entry : nthbpot = three-body potential being tested
!
!  On exit  : lvalid - if true then this is a valid triad in the
!             present atom order.
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
  use current,   only : nbonds
  use m_three
  use molecule
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)     :: i
  integer(i4), intent(in)     :: ii
  integer(i4), intent(in)     :: imax
  integer(i4), intent(in)     :: j
  integer(i4), intent(in)     :: jj
  integer(i4), intent(in)     :: jmax
  integer(i4), intent(in)     :: k
  integer(i4), intent(in)     :: kmax
  integer(i4), intent(in)     :: nati
  integer(i4), intent(in)     :: natj
  integer(i4), intent(in)     :: natk
  integer(i4), intent(in)     :: ndim
  integer(i4), intent(in)     :: nthbpot
  integer(i4), intent(in)     :: ntypi
  integer(i4), intent(in)     :: ntypj
  integer(i4), intent(in)     :: ntypk
  logical,     intent(out)    :: lvalid
  real(dp),    intent(in)     :: r21
  real(dp),    intent(in)     :: r31
  real(dp),    intent(in)     :: r32
!
!  Local variables
!
  integer(i4)                 :: in3
  integer(i4)                 :: ixx
  integer(i4)                 :: iyy
  integer(i4)                 :: izz
  integer(i4)                 :: nbtypeij
  integer(i4)                 :: nbtypeik
  integer(i4)                 :: nbtypeij2
  integer(i4)                 :: nbtypeik2
  integer(i4)                 :: nmi
  integer(i4)                 :: nmj
  integer(i4)                 :: nmk
  integer(i4)                 :: nt1
  integer(i4)                 :: nt2
  integer(i4)                 :: nt3
  integer(i4)                 :: ntyp1
  integer(i4)                 :: ntyp2
  integer(i4)                 :: ntyp3
  logical                     :: l2bonds
  logical                     :: lbonded
  logical                     :: lbondnoOK
  logical                     :: lbtyp
  logical                     :: lintra_only
  logical                     :: linter_only
  logical                     :: lmatch
  logical                     :: lmolok
  logical                     :: lneedmol 
  real(dp)                    :: tr1
  real(dp)                    :: tr2
  real(dp)                    :: tr3
  real(dp)                    :: tr1m
  real(dp)                    :: tr2m
  real(dp)                    :: tr3m
!
  lvalid = .true.
!*****************************
!  Get potential parameters  *
!*****************************
  nt1 = ntspec1(nthbpot)
  nt2 = ntspec2(nthbpot)
  nt3 = ntspec3(nthbpot)
  ntyp1 = ntptyp1(nthbpot)
  ntyp2 = ntptyp2(nthbpot)
  ntyp3 = ntptyp3(nthbpot)
  tr1m = thr1min(nthbpot)
  tr2m = thr2min(nthbpot)
  tr3m = thr3min(nthbpot)
  tr1 = thr1(nthbpot)
  tr2 = thr2(nthbpot)
  tr3 = thr3(nthbpot)
  lbtyp = (mmtexc(nthbpot).eq.1)
  lintra_only = (ltintra(nthbpot).and..not.ltinter(nthbpot))
  linter_only = (ltinter(nthbpot).and..not.ltintra(nthbpot))
  lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Check atom types with fixed order
!
  lvalid = lmatch(nati,ntypi,nt1,ntyp1,.true.)
  if (.not.lvalid) return
  lvalid = lmatch(natj,ntypj,nt2,ntyp2,.true.)
  if (.not.lvalid) return
  lvalid = lmatch(natk,ntypk,nt3,ntyp3,.true.)
  if (.not.lvalid) return
!
!  Check number of bonds if necessary
!
  if (n3bondnono(1,nthbpot).gt.0) then
    lbondnoOK = .false.
    do in3 = 1,n3bondnono(1,nthbpot)
      if (nbonds(i).eq.n3bondno(in3,1,nthbpot)) lbondnoOK = .true.
    enddo
    if (.not.lbondnoOK) lvalid = .false.
  endif
  if (n3bondnono(2,nthbpot).gt.0) then
    lbondnoOK = .true.
    do in3 = 1,n3bondnono(2,nthbpot)
      if (nbonds(i).eq.n3bondno(in3,2,nthbpot)) lbondnoOK = .false.
    enddo
    if (.not.lbondnoOK) lvalid = .false.
  endif
  if (.not.lvalid) return
!
!  Check molecule attributes for i-j
!
  lbonded = .false.
  if (lmol.and.lneedmol) then
    nmi = natmol(i)
    nmj = natmol(j)
    lmolok = (nmi.eq.nmj.and.nmi.gt.0)
    if (lintra_only.and..not.lmolok) lvalid = .false.
  else
    lmolok = .false.
  endif
  if (.not.lvalid) return
  if (lmolok) then
    if (ndim.eq.0) then
      if (linter_only) lvalid = .false.
      if (lbtyp) then
        call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
        if (.not.lbonded) lvalid = .false.
      endif
    else
      call lintoijk(ixx,iyy,izz,ii,imax,jmax,kmax)
      if (lbtyp) then
        call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
        if (.not.lbonded) lvalid = .false.
      endif
    endif
  endif
  if (lbtyp.and..not.lmolok) lvalid = .false.
  if (.not.lvalid) return
!
!  Check r21 is OK
!
  if ((r21.gt.tr1.or.r21.lt.tr1m).and.(.not.lbtyp.or..not.lbonded)) lvalid = .false.
  if (.not.lvalid) return
!
!  Check molecule attributes for 1-3 and 2-3
!
  lbonded = .false.
  if (lmol.and.lneedmol) then
    nmk = natmol(k)
    lmolok = (nmi.eq.nmk.and.nmi.gt.0)
    if (lmolok.and.linter_only) lvalid = .false.
    if (.not.lmolok.and.lintra_only) lvalid = .false.
  else
    lmolok = .false.
  endif
  if (.not.lvalid) return
  if (lmolok) then
    if (ndim.eq.0) then
      if (lbtyp) then
        call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,0_i4,0_i4,0_i4)
        if (.not.lbonded) lvalid = .false.
      endif
    else
      call lintoijk(ixx,iyy,izz,jj,imax,jmax,kmax)
      if (lbtyp) then
        call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,ixx,iyy,izz)
        if (.not.lbonded) lvalid = .false.
      endif
    endif
  endif
  if (lbtyp.and..not.lmolok) lvalid = .false.
  if (.not.lvalid) return
!
!  Check r31 is OK
!
  if ((r31.gt.tr2.or.r31.lt.tr2m).and.(.not.lbtyp.or..not.lbonded)) lvalid = .false.
  if (.not.lvalid) return
!
!  Check r32 is OK
!
  if (r32.gt.tr3.and..not.lbtyp) lvalid = .false.
  if (r32.lt.tr3m) lvalid = .false.
  if (.not.lvalid) return
!
  return
  end
