  subroutine getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,nword1,nbeg,lvalidpot)
!
!  Gets species symbols for one-body input
!
!   7/06 Created from getpotsymbol2
!   7/06 Error in non-library call corrected
!   8/06 ltype0 flag added to okspec call
!   9/06 Literal symbols now returned 
!  11/06 Words are saved for literal symbols before corruption by okspec call
!  12/08 Module input renamed to gulpinput
!   3/09 Breathing shells now handled when setting atomic number
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
  use datatypes  
  use element,     only : maxele
  use gulpinput,   only : nfloat, nword, floats, words
  use species,     only : natspec, ntypspec, nspec
  implicit none
!
!  Passed variables
!
  character(len=5), intent(out) :: sym1
  integer(i4),      intent(in)  :: iline
  integer(i4),      intent(out) :: itype1
  integer(i4),      intent(out) :: nbeg
  integer(i4),      intent(out) :: nvar1
  integer(i4),      intent(in)  :: nword1
  logical,          intent(in)  :: llibrary
  logical,          intent(out) :: lvalidpot
!
!  Local variables
!
  character(len=20)             :: wordsave
  integer(i4)                   :: ilp1
  logical                       :: lok1
  logical                       :: ltype0
!
  lvalidpot = .true.
  nbeg = 0
  sym1 = ' '
  if (nword.gt.0) then
!
!  Symbols used in input
!
    wordsave = words(nword1+1)(1:20)
    if (nword.eq.nword1+1) then
      if (llibrary) then
        call okspec(lok1,words(nword1+1),ilp1,.true.,ltype0)
        if (.not.lok1) then
          lvalidpot = .false.
        endif
        if (ilp1.gt.0.and.ilp1.le.nspec) then
          nvar1 = natspec(ilp1)
          if (ltype0) then
            itype1 = 0
          else
            itype1 = ntypspec(ilp1)
          endif
        elseif (ilp1.eq.-1) then
          nvar1 = maxele
          itype1 = 0
        endif
      else
        call ltont(words(nword1+1),nvar1,itype1)
      endif
    elseif (nword.eq.nword1+2) then
      if (llibrary) then
        call okspec(lok1,words(nword1+1),ilp1,.true.,ltype0)
        if (.not.lok1) then
          lvalidpot = .false.
        endif
        if (ilp1.gt.0.and.ilp1.le.nspec) then
          nvar1 = natspec(ilp1)
          if (ltype0) then
            itype1 = 0
          else
            itype1 = ntypspec(ilp1)
          endif
        else 
          nvar1 = maxele
          itype1 = 0
        endif
      else
        call ltont(words(nword1+1),nvar1,itype1)
      endif
      if ((index(words(nword1+2),'s').eq.1).or.(index(words(nword1+2),'S').eq.1)) then
        nvar1 = nvar1 + maxele
      elseif ((index(words(nword1+2),'bs').eq.1).or.(index(words(nword1+2),'BS').eq.1)) then
        nvar1 = nvar1 + maxele
      elseif ((index(words(nword1+2),'bS').eq.1).or.(index(words(nword1+2),'Bs').eq.1)) then
        nvar1 = nvar1 + maxele
      endif
    else
      call outerror('Incorrect species input for one-body term',iline)
      call stopnow('getpotsymbol1')
    endif
    nbeg = 0
    sym1 = wordsave(1:5)
  else
!
!  Numeric input
!
    nvar1 = int(floats(1))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    itype1 = 0
    nbeg = 1
    nfloat = nfloat - 1
    call label(nvar1,itype1,sym1)
  endif
!
  return
  end

  subroutine getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
!
!  Gets species symbols for two-body potential input
!
!  10/04 Created from potword21
!  10/05 Correction to handling of atom type added
!   8/06 ltype0 flag added to okspec call
!   9/06 Literal symbols now returned 
!  11/06 Words are saved for literal symbols before corruption by okspec call
!  12/08 Module input renamed to gulpinput
!   3/09 Breathing shells now handled when setting atomic number
!   5/11 Incorrect reference to ilp1 instead of ilp2 corrected
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
!  Julian Gale, NRI, Curtin University, May 2011
!
  use datatypes  
  use element,     only : maxele
  use gulpinput,   only : nfloat, nword, floats, words
  use species,     only : natspec, ntypspec, nspec
  implicit none
!
!  Passed variables
!
  character(len=5), intent(out) :: sym1
  character(len=5), intent(out) :: sym2
  integer(i4),      intent(in)  :: iline
  integer(i4),      intent(out) :: itype1
  integer(i4),      intent(out) :: itype2
  integer(i4),      intent(out) :: nbeg
  integer(i4),      intent(out) :: nvar1
  integer(i4),      intent(out) :: nvar2
  logical,          intent(in)  :: llibrary
  logical,          intent(out) :: lvalidpot
!
!  Local variables
!
  character(len=20)             :: word
  character(len=20)             :: wordsave1
  character(len=20)             :: wordsave2
  integer(i4)                   :: ilp1
  integer(i4)                   :: ilp2
  logical                       :: lok1
  logical                       :: lok2
  logical                       :: ltype01
  logical                       :: ltype02
!
  lvalidpot = .true.
  nbeg = 0
  sym1 = ' '
  sym2 = ' '
  if (nword.gt.0) then
!
!  Symbols used in input
!
    if (nword.eq.2) then
      wordsave1 = words(1)(1:20)
      wordsave2 = words(2)(1:20)
      if (llibrary) then
        call okspec(lok1,words(1),ilp1,.true.,ltype01)
        call okspec(lok2,words(2),ilp2,.true.,ltype02)
        if (.not.lok1.or..not.lok2) then
          lvalidpot = .false.
        endif
        if (ilp1.gt.0.and.ilp1.le.nspec) then
          nvar1 = natspec(ilp1)
          if (ltype01) then
            itype1 = 0
          else
            itype1 = ntypspec(ilp1)
          endif
        elseif (ilp1.eq.-1) then
          nvar1 = maxele
          itype1 = 0
        endif
        if (ilp2.gt.0.and.ilp2.le.nspec) then
          nvar2 = natspec(ilp2)
          if (ltype02) then
            itype2 = 0
          else
            itype2 = ntypspec(ilp2)
          endif
        elseif (ilp2.eq.-1) then
          nvar2 = maxele
          itype2 = 0
        endif
      else
        call ltont(words(1),nvar1,itype1)
        call ltont(words(2),nvar2,itype2)
      endif
      sym1 = wordsave1(1:5)
      sym2 = wordsave2(1:5)
    elseif (nword.eq.3) then
      wordsave1 = words(1)(1:20)
      if (llibrary) then
        call okspec(lok1,words(1),ilp1,.true.,ltype01)
        if (.not.lok1) then
          lvalidpot = .false.
        endif
        if (ilp1.gt.0.and.ilp1.le.nspec) then
          nvar1 = natspec(ilp1)
          if (ltype01) then
            itype1 = 0
          else
            itype1 = ntypspec(ilp1)
          endif
        elseif (ilp1.eq.-1) then
          nvar1 = maxele
          itype1 = 0
        endif
      else
        call ltont(words(1),nvar1,itype1)
      endif
      sym1 = wordsave1(1:5)
      word = words(2)(1:20)
      call stolc(word,20_i4)
      if (index(word,'cor').eq.1) then
        wordsave2 = words(3)(1:20)
        if (llibrary) then
          call okspec(lok2,words(3),ilp2,.true.,ltype02)
          if (.not.lok2) then
            lvalidpot = .false.
          endif
          if (ilp2.gt.0.and.ilp2.le.nspec) then
            nvar2 = natspec(ilp2)
            if (ltype02) then
              itype2 = 0
            else
              itype2 = ntypspec(ilp2)
            endif
          else
            nvar2 = maxele
            itype2 = 0
          endif
        else
          call ltont(words(3),nvar2,itype2)
        endif
        sym2 = wordsave2(1:5)
      elseif (index(word,'she').eq.1) then
        nvar1 = nvar1 + maxele
        wordsave2 = words(3)(1:20)
        if (llibrary) then
          call okspec(lok2,words(3),ilp2,.true.,ltype02)
          if (.not.lok2) then
            lvalidpot = .false.
          endif
          if (ilp2.gt.0.and.ilp2.le.nspec) then
            nvar2 = natspec(ilp2)
            if (ltype02) then
              itype2 = 0
            else
              itype2 = ntypspec(ilp2)
            endif
          else
            nvar2 = maxele
            itype2 = 0
          endif
        else
          call ltont(words(3),nvar2,itype2)
        endif
        sym2 = wordsave2(1:5)
      else
        wordsave2 = words(2)(1:20)
        if (llibrary) then
          call okspec(lok2,words(2),ilp2,.true.,ltype02)
          if (.not.lok2) then
            lvalidpot = .false.
          endif
          if (ilp2.gt.0.and.ilp2.le.nspec) then
            nvar2 = natspec(ilp2)
            if (ltype02) then
              itype2 = 0
            else
              itype2 = ntypspec(ilp2)
            endif
          else
            nvar2 = maxele
            itype2 = 0
          endif
        else
          call ltont(words(2),nvar2,itype2)
        endif
        word = words(3)(1:20)
        call stolc(word,20_i4)
        if (index(word,'she').eq.1) then
          nvar2 = nvar2 + maxele
        endif
        sym2 = wordsave2(1:5)
      endif
    elseif (nword.eq.4) then
      wordsave1 = words(1)(1:20)
      wordsave2 = words(3)(1:20)
      if (llibrary) then
        call okspec(lok1,words(1),ilp1,.true.,ltype01)
        call okspec(lok2,words(3),ilp2,.true.,ltype02)
        if (.not.lok1.or..not.lok2) then
          lvalidpot = .false.
        endif
        if (ilp1.gt.0.and.ilp1.le.nspec) then
          nvar1 = natspec(ilp1)
          if (ltype01) then
            itype1 = 0
          else
            itype1 = ntypspec(ilp1)
          endif
        else 
          nvar1 = maxele
          itype1 = 0
        endif
        if (ilp2.gt.0.and.ilp2.le.nspec) then
          nvar2 = natspec(ilp2)
          if (ltype02) then
            itype2 = 0
          else
            itype2 = ntypspec(ilp2)
          endif
        else
          nvar2 = maxele
          itype2 = 0
        endif
      else
        call ltont(words(1),nvar1,itype1)
        call ltont(words(3),nvar2,itype2)
      endif
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) then
        nvar1 = nvar1 + maxele
      elseif ((index(words(2),'bs').eq.1).or.(index(words(2),'BS').eq.1)) then
        nvar1 = nvar1 + maxele
      elseif ((index(words(2),'bS').eq.1).or.(index(words(2),'Bs').eq.1)) then
        nvar1 = nvar1 + maxele
      endif
      if ((index(words(4),'s').eq.1).or.(index(words(4),'S').eq.1)) then
        nvar2 = nvar2 + maxele
      elseif ((index(words(4),'bs').eq.1).or.(index(words(4),'BS').eq.1)) then
        nvar2 = nvar2 + maxele
      elseif ((index(words(4),'bS').eq.1).or.(index(words(4),'Bs').eq.1)) then
        nvar2 = nvar2 + maxele
      endif
      sym1 = wordsave1(1:5)
      sym2 = wordsave2(1:5)
    else
      call outerror('Incorrect species input for two-body potential',iline)
      call stopnow('getpotsymbol2')
    endif
    nbeg = 0
  else
!
!  Numeric input
!
    nvar1 = int(floats(1))
    nvar2 = int(floats(2))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    itype1 = 0
    itype2 = 0
    nbeg = 2
    nfloat = nfloat - 2
    call label(nvar1,itype1,sym1)
    call label(nvar2,itype2,sym2)
  endif
!
  return
  end
!
  subroutine getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
!
!  Gets species symbols for three-body potential input
!
!  10/04 Created from potword3
!  10/05 Handling of atom types modified
!   8/06 ltype0 flag added to okspec call
!   9/06 Literal symbols now returned 
!  11/06 Words are saved for literal symbols before corruption by okspec call
!  12/08 Module input renamed to gulpinput
!   3/10 Modified so that all three species types are return arguments
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
!  Julian Gale, NRI, Curtin University, March 2010
!
  use datatypes  
  use element,     only : maxele
  use gulpinput,   only : nfloat, nword, floats, words
  use species,     only : natspec, ntypspec, nspec
  implicit none
!
!  Passed variables
!
  character(len=5), intent(out) :: sym1
  character(len=5), intent(out) :: sym2
  character(len=5), intent(out) :: sym3
  integer(i4),      intent(in)  :: iline
  integer(i4),      intent(out) :: itype1
  integer(i4),      intent(out) :: itype2
  integer(i4),      intent(out) :: itype3
  integer(i4),      intent(out) :: nbeg
  integer(i4),      intent(out) :: nvar1
  integer(i4),      intent(out) :: nvar2
  integer(i4),      intent(out) :: nvar3
  logical,          intent(in)  :: llibrary
  logical,          intent(out) :: lvalidpot
!
!  Local variables
!
  character(len=20)             :: word
  character(len=20)             :: wordsave
  integer(i4)                   :: i
  integer(i4)                   :: ilp1
  integer(i4)                   :: ityp(3)
  integer(i4)                   :: nptr
  integer(i4)                   :: nvr(3)
  logical                       :: lok1
  logical                       :: ltype0
!
  lvalidpot = .true.
  nbeg = 0
!
  if (nword.gt.0) then
    if (nword.ge.3) then
      nptr = 0
      do i = 1,nword
        wordsave = words(i)(1:20)
        word = words(i)(1:20)
        call stolc(word,20_i4)
        if (index(word,'she').eq.1.and.nptr.gt.0) then
          nvr(nptr) = nvr(nptr) + maxele
        elseif (index(word,'cor').eq.0) then
          nptr = nptr + 1
          if (llibrary) then
            call okspec(lok1,words(i),ilp1,.true.,ltype0)
            if (.not.lok1) then
              lvalidpot = .false.
            endif
            if (ilp1.gt.0.and.ilp1.le.nspec) then
              nvr(nptr) = natspec(ilp1)
              if (ltype0) then
                ityp(nptr) = 0
              else
                ityp(nptr) = ntypspec(ilp1)
              endif
            else 
              nvr(nptr) = maxele
              ityp(nptr) = 0
            endif
          else
            call ltont(words(i),nvr(nptr),ityp(nptr))
          endif
          if (nptr.eq.1) then
            sym1 = wordsave(1:5)
          elseif (nptr.eq.2) then
            sym2 = wordsave(1:5)
          elseif (nptr.eq.3) then
            sym3 = wordsave(1:5)
          endif
        endif
      enddo
      if (nptr.ne.3) then
        call outerror('Incorrect potential species input for three-body potential',iline)
        call stopnow('getpotsymbol3')
      endif
    else
      call outerror('Incorrect potential species input for three-body potential',iline)
      call stopnow('getpotsymbol3')
    endif
    nbeg = 0
    nvar1 = nvr(1)
    nvar2 = nvr(2)
    nvar3 = nvr(3)
    itype1 = ityp(1)
    itype2 = ityp(2)
    itype3 = ityp(3)
  else
!
!  Numeric input
!
    nvar1 = int(floats(1))
    nvar2 = int(floats(2))
    nvar3 = int(floats(3))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    if (nvar3.gt.100) nvar3 = nvar3 - 100 + maxele
    itype1 = 0
    itype2 = 0
    itype3 = 0
    nbeg = 3
    nfloat = nfloat - 3
    call label(nvar1,itype1,sym1)
    call label(nvar2,itype2,sym2)
    call label(nvar3,itype3,sym3)
  endif
!
  return
  end
!
  subroutine getpotsymbol4(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2, &
                           nvar3,itype3,sym3,nvar4,itype4,sym4,nbeg,lvalidpot)
!
!  Gets species symbols for four-body potential input
!
!  10/04 Created from potword4
!  10/05 Correction to handling of atom type added
!   8/06 ltype0 flag added to okspec call
!   9/06 Literal symbols now returned
!  11/06 Words are saved for literal symbols before corruption by okspec call
!  12/08 Module input renamed to gulpinput
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use datatypes  
  use element,     only : maxele
  use gulpinput,   only : nfloat, nword, floats, words
  use species,     only : natspec, ntypspec, nspec
  implicit none
!
!  Passed variables
!
  character(len=5), intent(out) :: sym1
  character(len=5), intent(out) :: sym2
  character(len=5), intent(out) :: sym3
  character(len=5), intent(out) :: sym4
  integer(i4),      intent(in)  :: iline
  integer(i4),      intent(out) :: itype1
  integer(i4),      intent(out) :: itype2
  integer(i4),      intent(out) :: itype3
  integer(i4),      intent(out) :: itype4
  integer(i4),      intent(out) :: nbeg
  integer(i4),      intent(out) :: nvar1
  integer(i4),      intent(out) :: nvar2
  integer(i4),      intent(out) :: nvar3
  integer(i4),      intent(out) :: nvar4
  logical,          intent(in)  :: llibrary
  logical,          intent(out) :: lvalidpot
!
!  Local variables
!
  character(len=20)             :: word
  character(len=20)             :: wordsave
  integer(i4)                   :: i
  integer(i4)                   :: ilp1
  integer(i4)                   :: ityp(4)
  integer(i4)                   :: nptr
  integer(i4)                   :: nvr(4)
  logical                       :: lok1
  logical                       :: ltype0
!
  lvalidpot = .true.
  nbeg = 0
!
  if (nword.gt.0) then
    if (nword.ge.4) then
      nptr = 0
      do i = 1,nword
        wordsave = words(i)(1:20)
        word = words(i)(1:20)
        call stolc(word,20_i4)
        if (index(word,'she').eq.1.and.nptr.gt.0) then
          nvr(nptr) = nvr(nptr) + maxele
        elseif (index(word,'cor').eq.0) then
          nptr = nptr + 1
          if (llibrary) then
            call okspec(lok1,words(i),ilp1,.true.,ltype0)
            if (.not.lok1) then
              lvalidpot = .false.
            endif
            if (ilp1.gt.0.and.ilp1.le.nspec) then
              nvr(nptr) = natspec(ilp1)
              if (ltype0) then
                ityp(nptr) = 0
              else
                ityp(nptr) = ntypspec(ilp1)
              endif
            else 
              nvr(nptr) = maxele
              ityp(nptr) = 0
            endif
          else
            call ltont(words(i),nvr(nptr),ityp(nptr))
          endif
          if (nptr.eq.1) then
            sym1 = wordsave(1:5)
          elseif (nptr.eq.2) then
            sym2 = wordsave(1:5)
          elseif (nptr.eq.3) then
            sym3 = wordsave(1:5)
          elseif (nptr.eq.4) then
            sym4 = wordsave(1:5)
          endif
        endif
      enddo
      if (nptr.ne.4) then
        call outerror('Incorrect potential species input for four-body potential',iline)
        call stopnow('getpotsymbol4')
      endif
    else
      call outerror('Incorrect potential species input for four-body potential',iline)
      call stopnow('getpotsymbol4')
    endif
    nbeg = 0
    nvar1 = nvr(1)
    nvar2 = nvr(2)
    nvar3 = nvr(3)
    nvar4 = nvr(4)
    itype1 = ityp(1)
    itype2 = ityp(2)
    itype3 = ityp(3)
    itype4 = ityp(4)
  else
!
!  Numeric input
!
    nvar1 = int(floats(1))
    nvar2 = int(floats(2))
    nvar3 = int(floats(3))
    nvar4 = int(floats(4))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    if (nvar3.gt.100) nvar3 = nvar3 - 100 + maxele
    if (nvar4.gt.100) nvar4 = nvar4 - 100 + maxele
    itype1 = 0
    itype2 = 0
    itype3 = 0
    itype4 = 0
    nbeg = 4
    nfloat = nfloat - 4
    call label(nvar1,itype1,sym1)
    call label(nvar2,itype2,sym2)
    call label(nvar3,itype3,sym3)
    call label(nvar4,itype4,sym4)
  endif
!
  return
  end
!
  subroutine getpotsymbol6(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3, &
                           nvar4,itype4,sym4,nvar5,itype5,sym5,nvar6,itype6,sym6,nbeg,lvalidpot)
!
!  Gets species symbols for four-body potential input
!
!  11/04 Created from getpotsymbol4
!  10/05 Correction to handling of atom types added
!   8/06 ltype0 flag added to okspec call
!   9/06 Literal symbols now returned
!  11/06 Words are saved for literal symbols before corruption by okspec call
!  12/08 Module input renamed to gulpinput
!   3/10 ltype0 case corrected to set type number to zero
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
!  Julian Gale, NRI, Curtin University, March 2010
!
  use datatypes  
  use element,     only : maxele
  use gulpinput,   only : nfloat, nword, floats, words
  use species,     only : natspec, ntypspec, nspec
  implicit none
!
!  Passed variables
!
  character(len=5), intent(out) :: sym1
  character(len=5), intent(out) :: sym2
  character(len=5), intent(out) :: sym3
  character(len=5), intent(out) :: sym4
  character(len=5), intent(out) :: sym5
  character(len=5), intent(out) :: sym6
  integer(i4),      intent(in)  :: iline
  integer(i4),      intent(out) :: itype1
  integer(i4),      intent(out) :: itype2
  integer(i4),      intent(out) :: itype3
  integer(i4),      intent(out) :: itype4
  integer(i4),      intent(out) :: itype5
  integer(i4),      intent(out) :: itype6
  integer(i4),      intent(out) :: nbeg
  integer(i4),      intent(out) :: nvar1
  integer(i4),      intent(out) :: nvar2
  integer(i4),      intent(out) :: nvar3
  integer(i4),      intent(out) :: nvar4
  integer(i4),      intent(out) :: nvar5
  integer(i4),      intent(out) :: nvar6
  logical,          intent(in)  :: llibrary
  logical,          intent(out) :: lvalidpot
!
!  Local variables
!
  character(len=20)             :: word
  character(len=20)             :: wordsave
  integer(i4)                   :: i
  integer(i4)                   :: ilp1
  integer(i4)                   :: ityp(6)
  integer(i4)                   :: nptr
  integer(i4)                   :: nvr(6)
  logical                       :: lok1
  logical                       :: ltype0
!
  lvalidpot = .true.
  nbeg = 0
!
  if (nword.gt.0) then
    if (nword.ge.6) then
      nptr = 0
      do i = 1,nword
        wordsave = words(i)(1:20)
        word = words(i)(1:20)
        call stolc(word,20_i4)
        if (index(word,'she').eq.1.and.nptr.gt.0) then
          nvr(nptr) = nvr(nptr) + maxele
        elseif (index(word,'cor').eq.0) then
          nptr = nptr + 1
          if (llibrary) then
            call okspec(lok1,words(i),ilp1,.true.,ltype0)
            if (.not.lok1) then
              lvalidpot = .false.
            endif
            if (ilp1.gt.0.and.ilp1.le.nspec) then
              nvr(nptr) = natspec(ilp1)
              if (ltype0) then
                ityp(nptr) = 0
              else
                ityp(nptr) = ntypspec(ilp1)
              endif
            else 
              nvr(nptr) = maxele
              ityp(nptr) = 0
            endif
          else
            call ltont(words(i),nvr(nptr),ityp(nptr))
          endif
          if (nptr.eq.1) then
            sym1 = wordsave(1:5)
          elseif (nptr.eq.2) then
            sym2 = wordsave(1:5)
          elseif (nptr.eq.3) then
            sym3 = wordsave(1:5)
          elseif (nptr.eq.4) then
            sym4 = wordsave(1:5)
          elseif (nptr.eq.5) then
            sym5 = wordsave(1:5)
          elseif (nptr.eq.6) then
            sym6 = wordsave(1:5)
          endif
        endif
      enddo
      if (nptr.ne.6) then
        call outerror('Incorrect potential species input for six-body potential',iline)
        call stopnow('getpotsymbol6')
      endif
    else
      call outerror('Incorrect potential species input for six-body potential',iline)
      call stopnow('getpotsymbol6')
    endif
    nbeg = 0
    nvar1 = nvr(1)
    nvar2 = nvr(2)
    nvar3 = nvr(3)
    nvar4 = nvr(4)
    nvar5 = nvr(5)
    nvar6 = nvr(6)
    itype1 = ityp(1)
    itype2 = ityp(2)
    itype3 = ityp(3)
    itype4 = ityp(4)
    itype5 = ityp(5)
    itype6 = ityp(6)
  else
!
!  Numeric input
!
    nvar1 = int(floats(1))
    nvar2 = int(floats(2))
    nvar3 = int(floats(3))
    nvar4 = int(floats(4))
    nvar5 = int(floats(5))
    nvar6 = int(floats(6))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    if (nvar3.gt.100) nvar3 = nvar3 - 100 + maxele
    if (nvar4.gt.100) nvar4 = nvar4 - 100 + maxele
    if (nvar5.gt.100) nvar5 = nvar5 - 100 + maxele
    if (nvar6.gt.100) nvar6 = nvar6 - 100 + maxele
    itype1 = 0
    itype2 = 0
    itype3 = 0
    itype4 = 0
    itype5 = 0
    itype6 = 0
    nbeg = 6
    nfloat = nfloat - 6
    call label(nvar1,itype1,sym1)
    call label(nvar2,itype2,sym2)
    call label(nvar3,itype3,sym3)
    call label(nvar4,itype4,sym4)
    call label(nvar5,itype5,sym5)
    call label(nvar6,itype6,sym6)
  endif
!
  return
  end
