  subroutine linepro(iounit,line_in,iline)
!
!  Subroutine searches a line for words and numbers
!  Now amended to handle full 80 character string.
!
!   2/95 Ability to handle continuation lines added
!   4/98 Now reads from channel 4 as input is preprocessed
!   5/01 Line length generalised in terms of 80
!   6/01 Note - line length returned to 80 since there are
!        problems under Absoft compiler otherwise
!   8/06 iounit now passed in for general control
!  12/08 Module input renamed to gulpinput
!   8/10 floatwords added
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
!  Julian Gale, NRI, Curtin University, August 2010
!
  use control
  use general, only : nwarn
  use gulpinput
  use iochannels
  implicit none
!
!  Passed variables
!
  character(len=maxlinelength)   :: line_in
  integer(i4),        intent(in) :: iounit
  integer(i4)                    :: iline
!
!  Local variables
!
  character(len=1)               :: c1
  character(len=1)               :: c2
  character(len=1)               :: contchar
  character(len=1)               :: decipoint
  character(len=1)               :: hash
  character(len=maxlinelength+1) :: line
  character(len=1)               :: slash
  character(len=1)               :: space
  character(len=maxlinelength)   :: string
  character(len=1)               :: tab
  integer(i4)                    :: i
  integer(i4)                    :: i0
  integer(i4)                    :: i9
  integer(i4)                    :: ia
  integer(i4)                    :: ic
  integer(i4)                    :: ica
  integer(i4)                    :: icontin
  integer(i4)                    :: icz
  integer(i4)                    :: ii
  integer(i4)                    :: ilower
  integer(i4)                    :: ineg
  integer(i4)                    :: iorder
  integer(i4)                    :: ipos
  integer(i4)                    :: islash
  integer(i4)                    :: iupper
  integer(i4)                    :: iz
  integer(i4)                    :: ndecipoint
  integer(i4)                    :: nend
  integer(i4)                    :: nlen
  integer(i4)                    :: nstart
  integer(i4)                    :: nsw
  logical                        :: lcontinuation
  logical                        :: lfrst
  logical                        :: lread
  logical                        :: lspace
  logical                        :: lword
  real(dp)                       :: rnum
!
  contchar = '&'
  decipoint = '.'
  hash = '#'
  slash = '/'
  space = ' '
  tab = achar(9)
  i0 = ichar('0')
  i9 = ichar('9')
  ineg = ichar('-')
  ipos = ichar('+')
  ia = ichar('a')
  iz = ichar('z')
  ica = ichar('A')
  icz = ichar('Z')
  nword = 0
  nfloat = 0
  iorder = 0
  lread = .false.
10 if (lread) then
    line_in  =  ' '
    read(iounit,'(a)',end = 100) line_in
    iline = iline + 1
  endif
  line(1:maxlinelength) = line_in(1:maxlinelength)
  line(maxlinelength+1:maxlinelength+1) = ' '
  lfrst = (line_in(1:1).ne.space.and.line_in(1:1).ne.tab)
  lread = .true.
  icontin = index(line,contchar)
  lcontinuation = (icontin.ne.0)
  if (lcontinuation) then
!
!  Blank out anything after continuation character to 
!  avoid processing comments
!
    do i = icontin,maxlinelength
      line(i:i) = ' '
    enddo
  endif
!*******************
!  Locate strings  *
!*******************
  ilower = 1
  iupper = index(line,'#')
  if (iupper.eq.0) iupper = maxlinelength
  do while (ilower.lt.iupper)
    lspace = .false.
    i = ilower - 1
!
!  Find space before alphanumeric character
!
    if (lfrst) then
      i = 0
      nstart = 1
      lfrst = .false.
      lspace = .true.
    else
      do while (.not.lspace.and.i.lt.iupper)
        i = i + 1
        c1 = line(i:i)
        c2 = line(i+1:i+1)
        if ((c1.eq.space.or.c1.eq.tab).and.(c2.ne.space.and.c2.ne.tab)) then
          lspace = .true.
        endif
      enddo
      nstart = i + 1
    endif
!
!  Find last alphanumeric character
!
    do while (lspace.and.i.lt.iupper)
      i = i + 1
      c1 = line(i:i)
      c2 = line(i+1:i+1)
      if ((c1.ne.space.and.c1.ne.tab).and.(c2.eq.space.or.c2.eq.hash.or.c2.eq.tab)) then
        lspace = .false.
      endif
    enddo
    nend = i
    nlen = nend - nstart + 1
    if (nstart.lt.iupper) then
!**********************************************
!  Decide what type of string has been found  *
!**********************************************
      string = ' '
      string = line(nstart:nend)
      c1 = string(1:1)
      if (c1.eq.decipoint) then
        c1 = string(2:2)
      endif
      ic = ichar(c1)
      lword = ((ic.lt.i0.or.ic.gt.i9).and.ic.ne.ineg.and.ic.ne.ipos)
!
!  Are there more than two letters in the string - if so this is a word
!
      nsw = 0
      do i = 1,nlen
        ii = ichar(string(i:i))
        if (ii.ge.ia.and.ii.le.iz) nsw = nsw + 1
        if (ii.ge.ica.and.ii.le.icz) nsw = nsw + 1
      enddo
      if (nsw.ge.2) lword = .true.
!
      if (lword) then
!*********
!  Word  *
!*********
        nword = nword + 1
        if (nword.gt.maxword) then
          maxword = nword + 10
          call changemaxword
        endif
        words(nword) = string
        iorder = iorder + 1
        nlorder(iorder) = 1
      else
!***********
!  Number  *
!***********
        nfloat = nfloat + 1
        if (nfloat.gt.maxword) then
          maxword = nfloat + 10
          call changemaxword
        endif
        islash = (index(string,slash))
        if (islash.ne.0) then
!
!  Fractions
!
          ndecipoint = islash
        else
!
!  Floating point
!
          ndecipoint = index(string,'.')
        endif
        floatwords(nfloat) = string
        call wtof(string,rnum,nlen,ndecipoint)
        floats(nfloat) = rnum
        iorder = iorder + 1
        nlorder(iorder) = 2
      endif
    endif
!
!  Set values for next search
!
    ilower = nend + 1
  enddo
  if (lcontinuation) then
    goto 10
  else
    return
  endif
100 write(ioout,'(/,''  **** Warning - continuation character on last line of input ****'',/)')
  nwarn = nwarn + 1
  return
  end
!
  subroutine linepronc(line_in,iline)
!
!  Subroutine searches a line for words and numbers
!  Now amended to handle full maxlinelength character string.
!
!  Version of linepro which ignores continuation characters for
!  use by firstpass.f
!
!   2/95 Ability to handle continuation lines added
!   5/01 Line length generalised in terms of 80
!  12/08 Module input renamed to gulpinput
!   8/10 floatwords added
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
!  Julian Gale, NRI, Curtin University, August 2010
!
  use gulpinput
  implicit none
!
!  Passed variables
!
  character(len=maxlinelength)   :: line_in
  integer(i4)                    :: iline
!
!  Local variables
!
  character(len=1)               :: c1
  character(len=1)               :: c2
  character(len=1)               :: decipoint
  character(len=1)               :: hash
  character(len=maxlinelength+1) :: line
  character(len=1)               :: slash
  character(len=1)               :: space
  character(len=maxlinelength)   :: string
  character(len=1)               :: tab
  integer(i4)                    :: i
  integer(i4)                    :: i0
  integer(i4)                    :: i9
  integer(i4)                    :: ia
  integer(i4)                    :: ic
  integer(i4)                    :: ica
  integer(i4)                    :: icz
  integer(i4)                    :: ii
  integer(i4)                    :: ilower
  integer(i4)                    :: ineg
  integer(i4)                    :: iorder
  integer(i4)                    :: ipos
  integer(i4)                    :: islash
  integer(i4)                    :: iupper
  integer(i4)                    :: iz
  integer(i4)                    :: ndecipoint
  integer(i4)                    :: nend
  integer(i4)                    :: nlen
  integer(i4)                    :: nstart
  integer(i4)                    :: nsw
  logical                        :: lfrst
  logical                        :: lspace
  logical                        :: lword
  real(dp)                       :: rnum
!
  decipoint = '.'
  hash = '#'
  slash = '/'
  space = ' '
  tab = achar(9)
  i0 = ichar('0')
  i9 = ichar('9')
  ineg = ichar('-')
  ipos = ichar('+')
  ia = ichar('a')
  iz = ichar('z')
  ica = ichar('A')
  icz = ichar('Z')
  nword = 0
  nfloat = 0
  iorder = 0
  line(1:maxlinelength) = line_in(1:maxlinelength)
  line(maxlinelength+1:maxlinelength+1) = ' '
  lfrst = (line_in(1:1).ne.space.and.line_in(1:1).ne.tab)
!*******************
!  Locate strings  *
!*******************
  ilower = 1
  iupper = index(line,'#')
  if (iupper.eq.0) iupper = maxlinelength
  do while (ilower.lt.iupper)
    lspace = .false.
    i = ilower - 1
!
!  Find space before alphanumeric character
!
    if (lfrst) then
      i = 0
      nstart = 1
      lfrst = .false.
      lspace = .true.
    else
      do while (.not.lspace.and.i.lt.iupper)
        i = i + 1
        c1 = line(i:i)
        c2 = line(i + 1:i+1)
        if ((c1.eq.space.or.c1.eq.tab).and.(c2.ne.space.and.c2.ne.tab)) then
          lspace = .true.
        endif
      enddo
      nstart = i + 1
    endif
!
!  Find last alphanumeric character
!
    do while (lspace.and.i.lt.iupper)
      i = i + 1
      c1 = line(i:i)
      c2 = line(i + 1:i+1)
      if ((c1.ne.space.and.c1.ne.tab).and.(c2.eq.space.or.c2.eq.hash.or.c2.eq.tab)) then
        lspace = .false.
      endif
    enddo
    nend = i
    nlen = nend - nstart + 1
    if (nstart.lt.iupper) then
!**********************************************
!  Decide what type of string has been found  *
!**********************************************
      string = ' '
      string = line(nstart:nend)
      c1 = string(1:1)
      if (c1.eq.decipoint) then
        c1 = string(2:2)
      endif
      ic = ichar(c1)
      lword = ((ic.lt.i0.or.ic.gt.i9).and.ic.ne.ineg.and.ic.ne.ipos)
!
!  Are there more than two letters in the string - if so this is a word
!
      nsw = 0
      do i = 1,nlen
        ii = ichar(string(i:i))
        if (ii.ge.ia.and.ii.le.iz) nsw = nsw + 1
        if (ii.ge.ica.and.ii.le.icz) nsw = nsw + 1
      enddo
      if (nsw.ge.2) lword  =  .true.
!
      if (lword) then
!*********
!  Word  *
!*********
        nword = nword + 1
        if (nword.gt.maxword) then
          maxword  =  nword  +  10
          call changemaxword
        endif
        words(nword) = string
        iorder = iorder + 1
        nlorder(iorder) = 1
      else
!***********
!  Number  *
!***********
        nfloat = nfloat + 1
        if (nfloat.gt.maxword) then
          maxword  =  nfloat  +  10
          call changemaxword
        endif
        islash = (index(string,slash))
        if (islash.ne.0) then
!
!  Fractions
!
          ndecipoint = islash
        else
!
!  Floating point
!
          ndecipoint = index(string,'.')
        endif
        floatwords(nfloat) = string
        call wtof(string,rnum,nlen,ndecipoint)
        floats(nfloat) = rnum
        iorder = iorder + 1
        nlorder(iorder) = 2
      endif
    endif
!
!  Set values for next search
!
    ilower = nend + 1
  enddo
!
  return
  end
