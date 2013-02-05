  subroutine setlib(ncurr)
!
!  Read in library potentials
!
!  nspecold = original number of species in input deck
!  ncurr    = pointer to current configuration - should be
!             returned unchanged, but needed for lower calls.
!
!   6/01 Initialisation of line added for benefit of some compilers
!   3/04 Call to boword added
!  11/04 Six-body potentials added
!   6/05 Library argument added to genword call
!   5/06 Call to setkeyword added to update keyword flags for 
!        instant effect
!   8/06 I/O unit now passed to linepro
!   8/06 Option to read fitting flags from library added
!   3/07 Adding of backslash on to gulpdir handled
!   3/07 Keyword to preserve individual charges added
!   6/07 Ignore/erongi option added
!  12/08 Module input renamed to gulpinput
!  12/09 Check added to prevent duplication of keywords
!   9/11 Argument list to boword corrected
!   6/12 File handling changed to allow for spaces in names
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
  use control
  use general
  use gulpinput
  use library
  use parallel
  use species
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)      :: ncurr
!
!  Local variables
!
  character(len=20)            :: word
  character(len=136)           :: lib
  character(len=maxlinelength) :: line
  integer(i4)                  :: i
  integer(i4)                  :: iblank
  integer(i4)                  :: ilen
  integer(i4)                  :: ilend
  integer(i4)                  :: iline
  integer(i4)                  :: ind
  integer(i4)                  :: j
  integer(i4)                  :: nptr
  logical                      :: l45
  logical                      :: l50
  logical                      :: lfflags
  logical                      :: lignore
  logical                      :: linr
  logical                      :: lint
  logical                      :: lwordok
!***************************************
!  Set pointer to end of keyword line  *
!***************************************
  lignore = .false.
  do i = 400,1,-1
    if (keyword(i:i).ne.' ') goto 5
  enddo
5 nptr = i + 2
!
!  Initialise charge mask
!
  do i = 1,nspec
    lmask(i) = .false.
  enddo
!
!  Loop over libraries
!
  do i = 1,nlib
!
!  If file name doesn't include .lib extension then add
!
    do j = 133,136
      lib(j:j) = ' '
    enddo
    lib = libname(i)
    ind = index(lib,'.lib')
    if (ind.eq.0) then
      call endstring(lib,len(lib),ilen)
      lib(ilen:ilen+3) = '.lib'
    endif
    iline = 0
    lint = .true.
    linr = .true.
!
!  Try opening library file in current directory
!
    open(7,file=lib,status='old',err=30)
    goto 40
!
!  Try opening library file in default directory
!
30  call endstring(gulpdir,len(gulpdir),ilend)
    ilend = ilend - 1
    if (ilend.eq.-1) ilend = len(gulpdir)
!
!  Check whether backslash is one end of directory
!
    if (ilend.gt.0) then
      if (gulpdir(ilend:ilend).ne.'/') then
        ilend = ilend + 1
        gulpdir(ilend:ilend) = '/'
      endif
    endif
    call endstring(lib,len(lib),ilen)
    ilen = ilen - 1
    do j = 1,ilen
      lib(j+ilend:j+ilend) = lib(j:j)
    enddo
    lib(1:ilend) = gulpdir(1:ilend)
    open(7,file=lib,status='old',err=100)
    goto 40
40  line = '  '
    read(7,'(a)',end=50) line
    iline = iline + 1
    call linepro(7_i4,line,iline)
    if (nword.eq.0) goto 40
45  word = words(1)(1:20)
    call stolc(word,20_i4)
    lwordok = .false.
    l45 = .false.
    l50 = .false.
!*********************
!  Get any keywords  *
!*********************
    if (index(word,'key').ne.0) then
!
!  Keyword option line - add to keyword line
!
      do j = 2,nword
        word = words(j)(1:20)
        iblank = index(word,' ')
        if (iblank.eq.0) iblank = 21
        if (index(keyword,word(1:iblank-1)).eq.0) then
          if (nptr+iblank.gt.400) then
            call outerror('keyword line has exceeded maximum length',0_i4)
            call stopnow('setlib')
          endif
          keyword(nptr:nptr+iblank-2) = word(1:iblank-1)
          nptr = nptr + iblank
        endif
      enddo
!
!  Call to update keyword flags before reading the rest of the library
!
      call setkeyword
      goto 40
    endif
!
!  Ignore/erongi construct
!
    if (lignore) then
      if (index(word,'eron').eq.1) then
        lignore = .false.
      endif
      goto 40
    endif
    if (index(word,'igno').eq.1) then
      lignore = .true.
      goto 40
    endif
!
!  Set flag for library fitting flags
!
    lfflags = (index(keyword,'libff').ne.0)
!
    call genword(7_i4,word,lwordok,iline,line,l45,l50,.true.,lfflags,ncurr)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call potword21(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags,ncurr)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call potword22(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags,ncurr)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call potword3(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call potword4(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call potword6(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call potwordm(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags,ncurr)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call boword(7_i4,word,lwordok,iline,line,l45,l50,linr,lint,.true.,lfflags)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
    call speclibw(7_i4,word,lwordok,iline,line,l45,l50)
    if (l45) goto 45
    if (l50) goto 50
    if (lwordok) goto 40
50  continue
  enddo
  close(7)
!
  if (.not.lpreserveQ) call qupdate
  return
!
!  Error - library file could not be found
!
100 call outerror('could not open library file = '//libname(i),0_i4)
  call stopnow('setlib')
  end
