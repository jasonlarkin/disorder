  subroutine wtof(string,rnum,nlen,ndp)
!
!  Convert string to floating point number
!
!  nlen = length of string
!  ndp  = position of decimal point
!
!   9/92 Created
!   3/97 Handling of fractions added
!  12/07 Modified so that nlen & ndp are unmodified on return
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
  character(len=*), intent(in)    :: string
  integer(i4),      intent(in)    :: ndp
  integer(i4),      intent(in)    :: nlen
  real(dp),         intent(out)   :: rnum
!
!  Local variables
!
  character(len=1)                :: c
  integer(i4)                     :: i
  integer(i4)                     :: i0
  integer(i4)                     :: ic
  integer(i4)                     :: ineg
  integer(i4)                     :: ipos
  integer(i4)                     :: n
  integer(i4)                     :: ndploc
  integer(i4)                     :: nend
  integer(i4)                     :: nexp
  integer(i4)                     :: nfct
  integer(i4)                     :: nlenloc
  integer(i4)                     :: npower
  integer(i4)                     :: nse
  logical                         :: lneg
  logical                         :: lnegp
  real(dp)                        :: factor
  real(dp)                        :: rnum2
!
  nlenloc = nlen
  ndploc  = ndp
!
  npower = 0
  i0 = ichar('0')
  ineg = ichar('-')
  ipos = ichar('+')
!
!  Look for exponentiation
!
  lnegp = .false.
  nexp = index(string,'e')
  if (nexp.eq.0) nexp = index(string,'E')
  if (nexp.eq.0) nexp = index(string,'d')
  if (nexp.eq.0) nexp = index(string,'D')
  if (nexp.gt.0.and.nexp.lt.nlenloc) then
!
!  Exponential found
!
    nse = nexp + 1
    if (index(string(nse:nse),'+').eq.1) then
      nse = nse + 1
    elseif (index(string(nse:nse),'-').eq.1) then
      nse = nse + 1
      lnegp = .true.
    endif
    nfct = 1
    do i = nlenloc,nse,-1
      c = string(i:i)
      ic = ichar(c)
      n = ic - i0
      npower = npower + nfct*n
      nfct = nfct*10
    enddo
    nlenloc = nexp - 1
    if (lnegp) npower = -npower
  endif
  rnum = 0.0_dp
  if (nlenloc.eq.0) return
  factor = 0.1_dp
  if (ndploc.eq.0) ndploc = nlenloc + 1
  nend = 1
  c = string(1:1)
  ic = ichar(c)
  if (ic.eq.ineg.or.ic.eq.ipos) nend=2
  lneg = (ic.eq.ineg)
  if (lneg) nend = 2
!
!  In front of decimal point
!
  do i = ndploc-1,nend,-1
    c = string(i:i)
    ic = ichar(c)
    n = ic - i0
    factor = factor*10.0_dp
    rnum = rnum + factor*n
  enddo
!
!  After decimal point
!
  factor = 1.0_dp
  if (string(ndploc:ndploc).eq.'/') then
    rnum2 = 0.0_dp
    do i = nlenloc,ndploc+1,-1
      c = string(i:i)
      ic = ichar(c)
      n = ic - i0
!
!  The following condition is to avoid problems with tab characters
!
      if (n.ge.0.and.n.le.9) then
        rnum2 = rnum2 + factor*n
        factor = factor*10.0_dp
      else
!
!  If non-numeric character has been found then exit as this is really
!  a string
!
        return
      endif
    enddo
    if (rnum2.ne.0.0_dp) then
      rnum = rnum/rnum2
    else
      call outerror('zero in fraction',0_i4)
      call stopnow('wtof')
    endif
  else
    do i = ndploc+1,nlenloc
      c = string(i:i)
      ic = ichar(c)
      n = ic - i0
!
!  The following condition is to avoid problems with tab characters
!
      if (n.ge.0.and.n.le.9) then
        factor = factor*0.1_dp
        rnum = rnum + factor*n
      endif
    enddo
  endif
  if (lneg) rnum = -rnum
  rnum = rnum*(10.0_dp**npower)
!
  return
  end
