  subroutine speclibw(iin,word,lwordok,iline,line,l55,l1000)
!
!  Processes input for species from library
!
!  iin = input fortran channel
!
!   6/01 Initialisation of line added for benefit of some compilers
!  11/02 Charge handling fixed
!  11/04 linspec handling modified to be species-wise
!  10/05 Modified to handle ilp = -1 for wildcard
!  12/05 lqinspec set to true where charges have been set by library
!   8/06 ltype0 added to call to okspec
!   8/06 iin passed to linepro
!  12/08 Module input renamed to gulpinput
!   7/09 Mass setting now added
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
!  Julian Gale, NRI, Curtin University, July 2009
!
  use element
  use gulpinput
  use library
  use species
  implicit none
!
!  Passed variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: iin
  integer(i4)                  :: iline
  logical                      :: l55
  logical                      :: l1000
  logical                      :: lwordok
!
!  Local variables
!
  integer(i4)                  :: i
  integer(i4)                  :: ilp
  integer(i4)                  :: inat
  integer(i4)                  :: ind2
  integer(i4)                  :: itype
  logical                      :: lbri
  logical                      :: lok
  logical                      :: lqinput
  logical                      :: lsymbol
  logical                      :: ltype0
  real(dp)                     :: qli
  real(dp)                     :: radi
!
!  Initialise local variables
!
  if (index(word,'spec').eq.1) goto 100
  return
!********************************************************
!  Species info - at the moment charge and radius only  *
!********************************************************
100 continue
105 line = '  '
  read(iin,'(a)',end=108) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 105
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 105
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      call stolc(word,20_i4)
      if (index(word,'end').eq.1) then
        lwordok = .true.
        return
      else
        l55 = .true.
        goto 108
      endif
    endif
  endif
  call okspec(lok,words(1),ilp,.false.,ltype0)
  if (.not.lok) goto 105
!
!  Atomic number and type assigned on the basis of species which matches the library type
!  as the old method of a call to ltont will fail with unusual symbols
!
  if (ilp.gt.0.and.ilp.le.nspec) then
    inat = natspec(ilp)
    if (ltype0) then
      itype = 0
    else
      itype = ntypspec(ilp)
    endif
  else
    inat = maxele
    itype = 0
  endif
  lbri = .false.
  if (nword.ge.2) then
    call stolc(words(2),maxword)
    if (index(words(2),'bco').eq.1.or.index(words(2),'bsh').eq.1) then
      lbri = .true.
      ind2 = 2
    else
      ind2 = 1
    endif
    if (index(words(2),'she').eq.ind2) inat = inat + maxele
  endif
!
!  Handle charges and radii
!
  lqinput = .false.
  if (nfloat.ge.2) then
    lqinput = .true.
    qli = floats(1)
    radi = floats(2)
  elseif (nfloat.eq.1) then
    lqinput = .true.
    qli = floats(1)
    if (lbri) then
      radi = rion(inat)
    else
      radi = 0.0_dp
    endif
  else
    if (lbri) then
      radi = rion(inat)
    else
      radi = 0.0_dp
    endif
  endif
  do i = 1,nspec
    if (natspec(i).eq.inat.and.(ntypspec(i).eq.itype.or.itype.eq.0)) then
      if (lqinput) then
        qlspec(i) = qli
        linspec(i) = .true.
        lqinspec(i) = .true.
      endif
      radspec(i) = radi
      lbrspec(i) = lbri
      lmask(i) = .true.
!
!  Set species dependent mass
!
      if (.not.lmassinspec(i)) then
        if (inat.le.maxele) then
          massspec(i) = atmass(inat)
        else
          massspec(i) = 0.0_dp
        endif
      endif
    endif
  enddo
  goto 105
108 if (.not.l55) l1000 = .true.
  lwordok = .true.
!
  return
  end
