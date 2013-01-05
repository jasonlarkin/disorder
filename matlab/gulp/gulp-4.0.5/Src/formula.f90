  subroutine formula(iout)
!
!  Output formula string to unit iout. String generated in calc_formula 
!  (below). This is split in two to allow XML output of formula.
!
!  11/00 maximum number of atoms of a single type increased 
!  11/00 too many atoms of one type trapped
!   6/03 XML output added
!   3/04 Extended to cope with a million atoms
!   7/05 Order of deallocations reversed
!   3/08 Added option to allow formular to be returned and removed XML 
!   1/09 New version adopted that splits formula into 2 parts
!   6/09 Modified to prevent out of bounds write on form
!   5/10 Interface declaration added for calc_formula to avoid problems
!        with PGI compiler.
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, May 2010
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: iout
!
!  Parameters
!
  integer(i4), parameter                       :: nformsize = 60
!
!  Local variables
!
  character(len=nformsize)                     :: form
!
!  Interface to avoid problems with PGI compiler
!
  interface
    function calc_formula(nformsize) result(xx)
      character(len=nformsize) :: xx
      integer, intent(in)      :: nformsize
    end function calc_formula
  end interface
!
!  Get the formula
!
  form = calc_formula(nformsize)

!*******************
!  Output formula  *
!*******************
   write(iout,'(/,''  Formula = '',a60)') form
!
  return
  end subroutine formula


  function calc_formula (nformsize) 
!
!  Generate and return string containing the chemical formula 
!
!  11/00 maximum number of atoms of a single type increased 
!  11/00 too many atoms of one type trapped
!   6/03 XML output added
!   3/04 Extended to cope with a million atoms
!   7/05 Order of deallocations reversed
!   3/08 Added option to allow formular to be returned and removed XML 
!   6/09 Modified to prevent out of bounds write on form
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
  use current
  use element
  implicit none
!
!  Passed variables
!
  integer(i4),                   intent(in)    :: nformsize
!
!  Result variable
!
  character(len=nformsize)                     :: calc_formula
!
!  Local variables
!
  character(len=1)                             :: numstring(10)
  character(len=2)                             :: lab
  character(len=nformsize)                     :: form
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4),                            save :: maxtype
  integer(i4), dimension(:), allocatable       :: natt
  integer(i4)                                  :: nlen
  integer(i4)                                  :: nptr
  integer(i4)                                  :: nt
  integer(i4)                                  :: ntot
  integer(i4)                                  :: num(10)
  integer(i4)                                  :: status
  logical                                      :: lfractional
  logical                                      :: lfound
  logical                                      :: sizeOK
  real(dp)                                     :: rnnt
  real(dp)                                     :: rnt
  real(dp),    dimension(:), allocatable       :: rnumt
!
  data maxtype/100/
  data numstring/'0','1','2','3','4','5','6','7','8','9'/
!
!  Initialise formula string
!
  do i = 1,nformsize
    form(i:i) = ' '
  enddo
!
!  Determine how many distinct atomic numbers there are and
!  how many of each type
!
  sizeOK = .false.
  do while (.not.sizeOK)
    ntot = 0
    allocate(natt(maxtype),stat=status)
    if (status/=0) call outofmemory('formula','natt')
    allocate(rnumt(maxtype),stat=status)
    if (status/=0) call outofmemory('formula','rnumt')
    natt(1:maxtype) = 0
    rnumt(1:maxtype) = 0.0_dp
    do i = 1,numat
      if (nat(i).le.maxele) then
        j = 1
        lfound = .false.
        do while (j.le.ntot.and..not.lfound)
          if (nat(i).eq.natt(j)) lfound = .true.
          j = j + 1
        enddo
        if (lfound) then
          j = j - 1
          rnumt(j) = rnumt(j) + occuf(i)
        else
          ntot = ntot + 1
          if (ntot.gt.maxtype) then
!
!  Dimensions exceed - increase then start again
!
            maxtype = 2*maxtype
            deallocate(rnumt,stat=status)
            if (status/=0) call deallocate_error('formula','rnumt')
            deallocate(natt,stat=status)
            if (status/=0) call deallocate_error('formula','natt')
!
!  Go back to start of formula build.....
!
            cycle
          endif
          rnumt(ntot) = occuf(i)
          natt(ntot) = nat(i)
        endif
      endif
    enddo
    sizeOK = .true.
  enddo
!****************************
!  Generate formula string  *
!****************************
  nptr = 0
  do i = 1,ntot
!
!  Atomic label
!
    lab = atsym(natt(i))
    if (lab(2:2).eq.' ') then
      nptr = nptr + 1
      if (nptr.le.nformsize) form(nptr:nptr) = lab(1:1)
    else
      nptr = nptr + 1
      if (nptr.le.nformsize) form(nptr:nptr) = lab(1:1)
      nptr = nptr + 1
      if (nptr.le.nformsize) form(nptr:nptr) = lab(2:2)
    endif
!
!  Number of atoms
!
    rnt = rnumt(i)
    nt = nint(rnt)
    rnnt = dble(nt)
    lfractional = (abs(rnt - rnnt).gt.1.0d-3)
    if (lfractional) nt = rnt
    if (nt.ge.10000000) then
      call outerror('too many atoms of one type for formula',0_i4)
      call stopnow('formula')
    elseif (nt.ge.1000000) then
      nlen = 7
      num(1) = nt/1000000
      num(2) = (nt - 1000000*num(1))/100000
      num(3) = (nt - 1000000*num(1) - 100000*num(2))/10000
      num(4) = (nt - 1000000*num(1) - 100000*num(2) - 10000*num(3))/1000
      num(5) = (nt - 1000000*num(1) - 100000*num(2) - 10000*num(3) - 1000*num(4))/100
      num(6) = (nt - 1000000*num(1) - 100000*num(2) - 10000*num(3) - 1000*num(4) - 100*num(5))/10
      num(7) = (nt - 1000000*num(1) - 100000*num(2) - 10000*num(3) - 1000*num(4) - 100*num(5) - 10*num(6))
    elseif (nt.ge.100000) then
      nlen = 6
      num(1) = nt/100000
      num(2) = (nt - 100000*num(1))/10000
      num(3) = (nt - 100000*num(1) - 10000*num(2))/1000
      num(4) = (nt - 100000*num(1) - 10000*num(2) - 1000*num(3))/100
      num(5) = (nt - 100000*num(1) - 10000*num(2) - 1000*num(3) - 100*num(4))/10
      num(6) = (nt - 100000*num(1) - 10000*num(2) - 1000*num(3) - 100*num(4) - 10*num(5))
    elseif (nt.ge.10000) then
      nlen = 5
      num(1) = nt/10000
      num(2) = (nt - 10000*num(1))/1000
      num(3) = (nt - 10000*num(1) - 1000*num(2))/100
      num(4) = (nt - 10000*num(1) - 1000*num(2) - 100*num(3))/10
      num(5) = (nt - 10000*num(1) - 1000*num(2) - 100*num(3) - 10*num(4))
    elseif (nt.ge.1000) then
      nlen = 4
      num(1) = nt/1000
      num(2) = (nt - 1000*num(1))/100
      num(3) = (nt - 1000*num(1) - 100*num(2))/10
      num(4) = (nt - 1000*num(1) - 100*num(2) - 10*num(3))
    elseif (nt.ge.100) then
      nlen = 3
      num(1) = nt/100
      num(2) = (nt - 100*num(1))/10
      num(3) = (nt - 100*num(1) - 10*num(2))
    elseif (nt.ge.10) then
      nlen = 2
      num(1) = nt/10
      num(2) = nt - 10*num(1)
    elseif (nt.eq.1.and..not.lfractional) then
      nlen = 0
    else
      nlen = 1
      num(1) = nt
    endif
    if (nptr+nlen.le.nformsize) then
      do j = 1,nlen
        form(nptr+j:nptr+j) = numstring(num(j)+1)
      enddo
    endif
    nptr = nptr + nlen
    if (lfractional) then
      if (nptr+1.le.nformsize) form(nptr+1:nptr+1) = '.'
      nptr = nptr + 1
      nt = nint(1000.0_dp*(rnt - nt))
      nlen = 3
      num(1) = nt/100
      num(2) = (nt - 100*num(1))/10
      num(3) = (nt - 100*num(1) - 10*num(2))
      if (nptr+1.le.nformsize) form(nptr+1:nptr+1) = numstring(num(1)+1)
      if (num(3).eq.0) then
        if (num(2).ne.0) then
          if (nptr+2.le.nformsize) form(nptr+2:nptr+2) = numstring(num(2)+1)
          nptr = nptr + 2
        else
          nptr = nptr + 1
        endif
      else
        if (nptr+2.le.nformsize) form(nptr+2:nptr+2) = numstring(num(2)+1)
        if (nptr+3.le.nformsize) form(nptr+3:nptr+3) = numstring(num(3)+1)
        nptr = nptr + 3
      endif
    endif
  enddo
!
! form now has the formula to be returned.
!
  calc_formula = form
!
  deallocate(rnumt,stat=status)
  if (status/=0) call deallocate_error('formula','rnumt')
  deallocate(natt,stat=status)
  if (status/=0) call deallocate_error('formula','natt')
!
  return
  end function calc_formula
