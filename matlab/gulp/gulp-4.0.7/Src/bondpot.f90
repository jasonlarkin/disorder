  subroutine bondpot(iout,nbontot,nbptype,nbptypeptr)
!
!  Outputs bond potentials for Lammps file I/O
!
!  On entry:
!
!  iout      = channel number for I/O - must be open already
!
!  On exit:
!
!  nbontot   = total number of bond potential terms over all potentials
!  nbptype   = total number of bond potential types 
!  nbptypeptr= pointer from nbptype number to npote number
!
!  10/12 Created from angle for benefit of Lammps file I/O
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
!  Julian Gale, NRI, Curtin University, October 2012
!
  use configurations, only : nregionno, nregiontype, QMMMmode
  use constants
  use current
  use two,            only : npote, nspec1, nspec2, nptyp1, nptyp2
  use two,            only : nptype, mmexc
  implicit none
!
!  Passed variables
!
  integer(i4),                     intent(in)  :: iout
  integer(i4),                     intent(out) :: nbontot
  integer(i4),                     intent(out) :: nbptype
  integer(i4),                     intent(out) :: nbptypeptr(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: n
  integer(i4)                                  :: n1
  integer(i4)                                  :: n2
  integer(i4)                                  :: n2ty
  integer(i4)                                  :: nbon
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  logical                                      :: lbtyp
  logical                                      :: lmatch
  logical                                      :: lpotOK
  logical                                      :: lswap
!
!  Initialise total number of valid bond potentials
!
  nbontot = 0
!
!  Initialise total number of types of bond potentials
!
  nbptype = 0
!
!  Loop over potentials
!
  pots: do n = 1,npote
!
!  Check whether this is a suitable potential for bonding atoms
!
    n2ty = nptype(n)
    lpotOK = .false.
    if (n2ty.eq.3)  lpotOK = .true.            ! Morse
    if (n2ty.eq.4)  lpotOK = .true.            ! Morse
    if (n2ty.eq.5)  lpotOK = .true.            ! Harmonic
    if (n2ty.eq.6)  lpotOK = .true.            ! Harmonic
!
!  Currently not supported by Lammps
!
!    if (n2ty.eq.28) lpotOK = .true.            ! Squared harmonic
!    if (n2ty.eq.29) lpotOK = .true.            ! Squared harmonic
!    if (n2ty.eq.39) lpotOK = .true.            ! Morse with etaper
!    if (n2ty.eq.40) lpotOK = .true.            ! Morse with etaper
!
!  If potential is not of the right type then cycle
!
    if (.not.lpotOK) cycle pots
!
!  Increment the bond potential type
!
    nbptype = nbptype + 1
    nbptypeptr(nbptype) = n
!
    n1 = nspec1(n)
    n2 = nspec2(n)
    ntyp1 = nptyp1(n)
    ntyp2 = nptyp2(n)
    lbtyp = (mmexc(n).eq.1)
!
!  Initialise number of valid bond potentials for this type
!
    nbon = 0
!
!  Outer loop over sites
!
    iloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
      nregioni = nregionno(nsft+nrelat(i))
      nregiontypi = nregiontype(nregioni,ncf)
!
!  Check i is allowed for n
!
      lswap = .false.
      if (.not.lmatch(ni,ntypi,n1,ntyp1,.true.)) then
        lswap = .true.
        if (.not.lmatch(ni,ntypi,n2,ntyp2,.true.)) cycle iloop
      endif
!
!  QM/MM handling
!     
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle iloop
      endif
!
!  Look for match in bonds of i
!
      if (nbonds(i).eq.0) cycle iloop
      kloop: do k = 1,nbonds(i)
        j = nbonded(k,i)
!
!  Exclude i < j to avoid duplication 
!
        if (i.lt.j) cycle kloop
!
!  Get properties of atom bonded to i -> j
!
        nj = nat(j)
        ntypj = nftype(j)
!
!  Check j is allowed for n
!
        if (lswap) then
          if (.not.lmatch(nj,ntypj,n1,ntyp1,.true.)) cycle kloop
        else
          if (.not.lmatch(nj,ntypj,n2,ntyp2,.true.)) cycle kloop
        endif
!
!  Valid bond potential
!
        nbontot = nbontot + 1
        write(iout,'(4i8)') nbontot,nbptype,i,j
      enddo kloop
    enddo iloop
!
!  End of outer loops
!
  enddo pots
!
  return
  end
