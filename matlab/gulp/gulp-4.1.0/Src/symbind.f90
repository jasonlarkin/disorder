  subroutine symbind(symbol_in,hbr,nctyp,nsymb,nsymtype)
!
!  Works out symbol type index numbers.
!
!  nctyp   = pointer to type of centring
!  nsymb   = number of non-centring parts to symbol
!  nsymtype= type numbers for parts of symbol
!
!  12/07 Unused variables removed
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, December 2007
!
  use datatypes
  implicit none
!
!  Passed variables
!
  character(len=1)   :: hbr(8)
  character(len=16)  :: symbol_in
  integer(i4)        :: nctyp
  integer(i4)        :: nsymb
  integer(i4)        :: nsymtype(3)
!
!  Local variables
!
  character(len=4)   :: blank
  character(len=1)   :: c1
  character(len=1)   :: c2
  character(len=1)   :: parts(7)
  character(len=1)   :: space
  character(len=4)   :: string
  character(len=16)  :: symbol
  integer(i4)        :: i
  integer(i4)        :: ilower
  integer(i4)        :: iupper
  integer(i4)        :: k
  integer(i4)        :: nend
  integer(i4)        :: nstart
  integer(i4)        :: ntmp
  logical            :: lfrst
  logical            :: lspace
!
  data blank/'    '/
  data parts/'1','2','M','N','A','B','C'/
  space = ' '
  symbol = symbol_in
  lfrst = (symbol(1:1).ne.space)
  nsymb = - 1
  nsymtype(1) = 0
  nsymtype(2) = 0
  nsymtype(3) = 0
!*******************
!  Locate strings  *
!*******************
  ilower = 1
  iupper = 16
  do while (ilower.lt.iupper)
    lspace = .false.
    i = ilower-1
!
!  Find space before alphanumeric character
!
    if (lfrst) then
      i = 0
      nstart = 1
      lfrst = .false.
      lspace = .true.
    else
      do while (.not.lspace.and.i.lt.iupper-1)
        i = i + 1
        c1 = symbol(i:i)
        c2 = symbol(i+1:i+1)
        if (c1.eq.space.and.c2.ne.space) then
          lspace = .true.
        endif
      enddo
      nstart = i + 1
    endif
!
!  Find last alphanumeric character
!
    do while (lspace.and.i.lt.iupper-1)
      i = i + 1
      c1 = symbol(i:i)
      c2 = symbol(i+1:i+1)
      if (c1.ne.space.and.c2.eq.space) then
        lspace = .false.
      endif
    enddo
    nend = i
    if (nstart.lt.iupper) then
!**********************************************
!  Decide what type of symbol has been found  *
!**********************************************
      string = blank
      string = symbol(nstart:nend)
      nsymb = nsymb + 1
      if (nsymb.gt.3) goto 100
      if (nsymb.eq.0) then
        c1 = string(1:1)
        do k = 1,7
          if (hbr(k).eq.c1) nctyp = k
        enddo
        if (nctyp.eq.3) nctyp = 2
        if (nctyp.eq.4) nctyp = 2
      else
        c1 = string(1:1)
        do k = 1,7
          if (parts(k).eq.c1) nsymtype(nsymb) = k
        enddo
        if (nsymtype(nsymb).gt.4) nsymtype(nsymb) = 4
        c1 = string(2:2)
        c2 = string(3:3)
        if (c1.eq.'1') then
          nsymtype(nsymb) = nsymtype(nsymb)+10
          c1 = string(3:3)
          c2 = string(4:4)
        endif
        if (c1.eq.'/') then
          do k = 1,7
            if (parts(k).eq.c2) nsymtype(nsymb) = nsymtype(nsymb) + 100*min(k,4)
          enddo
        endif
      endif
    endif
!
!  Set values for next search
!
    ilower = nend + 1
  enddo
100 continue
!
!  Order values before return
!
  if (nsymtype(1).gt.nsymtype(2)) then
    ntmp = nsymtype(1)
    nsymtype(1) = nsymtype(2)
    nsymtype(2) = ntmp
  endif
  if (nsymtype(2).gt.nsymtype(3)) then
    ntmp = nsymtype(2)
    nsymtype(2) = nsymtype(3)
    nsymtype(3) = ntmp
  endif
  if (nsymtype(1).gt.nsymtype(2)) then
    ntmp = nsymtype(1)
    nsymtype(1) = nsymtype(2)
    nsymtype(2) = ntmp
  endif
!
  return
  end
