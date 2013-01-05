  subroutine okspec(lok,labelin,i,lpotcall,ltype0)
!
!  Check whether label matches one found in libspec list
!  If match is found then lok=.true., else lok=.false.
!  This routine is used to screen out unwanted potentials
!  from library.
!
!  On return, if lok then i contains species no. corresponding
!  to label, except for wildcard X, in which case i is -1.
!  If ltype0 is true, then the atomic symbol should be taken
!  from i, but the type number should be set to zero.
!
!   7/98 Set correct atomic symbol in labelin on return
!  10/02 Style updated
!  10/02 lpotcall passed through to worsy
!  10/05 Case sensitivity of libspec checking removed
!  10/05 Handling of wildcard species added
!  10/05 Modified handling of underscores added
!   8/06 Modified to handle the case where atoms in a structure
!        all have types > 0, but library has potentials for 
!        type = 0
!   8/06 ltype0 flag added to indicate whether type should be
!        set to 0 on return
!   5/11 Check for multiple uses of the same library symbol added
!   8/11 Check on multiple uses of same library symbol corrected
!   9/11 Length of strings standardised to 20 characters
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
!  Julian Gale, NRI, Curtin University, September 2011
!
  use element,     only : maxele
  use library
  use species
  implicit none
!
!  Passed variables
!
  character(len=20), intent(inout)  :: labelin
  integer(i4),       intent(out)    :: i
  logical,           intent(out)    :: lok
  logical,           intent(in)     :: lpotcall
  logical,           intent(out)    :: ltype0
!
!  Local variables
!
  character(len=10)                 :: blank
  character(len=20)                 :: labeltmp
  character(len=20)                 :: libsymtmp
  integer(i4)                       :: ind
  integer(i4)                       :: j
  integer(i4)                       :: ni
  integer(i4)                       :: nfoundc
  integer(i4)                       :: nfounds
  integer(i4)                       :: nti
  logical                           :: lsymbol
!
  data blank/'          '/
  lok = .false.
  ltype0 = .false.
  labeltmp = labelin
  call stolc(labeltmp,20_i4)
!
!  Check for wildcard symbol
!
  if (index(labeltmp,'x ').eq.1) then
    lok = .true.
    i = -1
    return
  endif
!
!  Search for symbol match
!
  i = 0
  nfoundc = 0
  nfounds = 0
  do while (.not.lok.and.i.lt.nlibsp)
    i = i + 1
    libsymtmp = libspec(i)
    call stolc(libsymtmp,20_i4)
    if (libsymtmp.eq.labeltmp) then
      lok = .true.
      if (natspec(i).le.maxele) then
        nfoundc = nfoundc + 1
      else
        nfounds = nfounds + 1
      endif
    endif
  enddo
  if (lok) then
!
!  Check for additional species matches
!
    do j = i+1,nlibsp
      libsymtmp = libspec(j)
      call stolc(libsymtmp,20_i4)
      if (libsymtmp.eq.labeltmp) then
        if (natspec(j).le.maxele) then
          nfoundc = nfoundc + 1
        else
          nfounds = nfounds + 1
        endif
      endif
    enddo
  endif
!
!  Error if species is ambiguous as multiple matches can't be handled at present
!
  if (nfoundc.gt.1.or.nfounds.gt.1) then
    call outerror('multiple species with same library symbol not yet supported',0_i4)
    call stopnow('okspec')
  endif
!
  ind = index(labeltmp,'_')
  if (ind.ne.0) then
    labelin = ' '
    labelin = labeltmp(1:ind-1)
  endif
  if (lok) return
!
!  If no symbol match is found check for species with no symbol
!  but matching atomic number
!
  if (ind.eq.0) then
    call worsy(labeltmp,lsymbol,lpotcall)
    if (lsymbol) then
      call ltont(labelin,ni,nti)
      i = 0
      do while (.not.lok.and.i.lt.nlibsp)
        i = i + 1
        if (libspec(i).eq.blank) then
          if (natspec(i).eq.ni.and.ntypspec(i).eq.nti) then
            lok = .true.
          endif
        endif
      enddo
      if (.not.lok.and.nti.eq.0) then
!
!  We still haven't found a symbol, but if there is no type number check
!  whether it matches any species that does have a type number. If so, 
!  it still needs to be included.
!
        i = 0
        do while (.not.lok.and.i.lt.nlibsp)
          i = i + 1
          if (libspec(i).eq.blank) then
            if (natspec(i).eq.ni) then
              lok = .true.
              ltype0 = .true.
            endif
          endif
        enddo
      endif
    endif
  endif
!
  return
  end
