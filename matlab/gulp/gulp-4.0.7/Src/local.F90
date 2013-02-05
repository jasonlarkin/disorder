  subroutine local
!
!  This routine should be edited prior to installation to contain the
!  following items of local information:
!
!  (1) elefile  = default location of element information file (eledata)
!  (2) helpfile = default location of help text file (help.txt)
!
!   3/07 Location of files now controlled by environmental variable
!  11/08 Checking for backslash on end of environment variables added
!   6/12 File handling changed to allow for spaces in names
!
!  Julian Gale, NRI, Curtin University, November 2008
!
  use general
#ifdef NAG
!For NAG compiler only:  
use f90_unix_env
#endif
!
!  Local variables
!
  integer(i4) :: i
!*************************
!  Start of definitions  *
!*************************
!
!  GULP directory for location of library files
!  Should end with a slash so that filenames can be concatenated
!
#ifdef ACCELRYS
  gulpdir=' '
  call getenv('GULP_DATA',gulpdir)
#else
  gulpdir=' '
  gulpdoc=' '
  call getenv('GULP_LIB',gulpdir)
  call getenv('GULP_DOC',gulpdoc)
#endif
!
!  Ensure names from environment variables in / if not blank
!
#ifdef ACCELRYS
  call endstring(gulpdir,len(gulpdir),i)
  if (i.gt.1) then
    if (gulpdir(i-1:i-1).ne.'/') then
      gulpdir(i:i) = '/'
    endif
  endif
#else
  call endstring(gulpdir,len(gulpdir),i)
  if (i.gt.1) then
    if (gulpdir(i-1:i-1).ne.'/') then
      gulpdir(i:i) = '/'
    endif
  endif
  call endstring(gulpdoc,len(gulpdoc),i)
  if (i.gt.1) then
    if (gulpdoc(i-1:i-1).ne.'/') then
      gulpdoc(i:i) = '/'
    endif
  endif
#endif
!
!  Element information file
!
#ifdef ACCELRYS
  elefile = trim(gulpdir) // 'eledata'
#else
  elefile = trim(gulpdir) // 'eledata'
#endif
!
!  Help text file
!
#ifdef ACCELRYS
  helpfile=trim(gulpdir)//'help.txt'
#else
  helpfile=trim(gulpdoc)//'help.txt'
#endif
!***********************
!  End of definitions  *
!***********************
  return
  end
