  subroutine addframe2arc(iout,fc,lmdcall,nstep)
!
!  Adds a frame to an arc file in order to make a movie.
!  Note: the arc file should have already been initialised
!  prior to the call to this routine using initarc.
!
!   7/08 Created from mdwrite
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
!  Julian Gale, NRI, Curtin University, July 2008
!
  use control,      only : lconp
  use current
  use element,      only : maxele, atsym
  use files,        only : loutshell
  use general,      only : titleword, timesofar
  use moldyn,       only : xcell
  use parallel,     only : ioproc
  use shell,        only : ncsptr
  use symmetry,     only : hmssg
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)  :: fc         ! Current energy for output
  integer(i4), intent(in)  :: iout       ! Output channel for arc file
  integer(i4), intent(in)  :: nstep      ! Step number for output
  logical,     intent(in)  :: lmdcall    ! If true add time so far to title
!
!  Local variables
!
  character(len=4)         :: lab
  character(len=18)        :: spaceg
  character(len=25)        :: dattim
  character(len=64)        :: title
  integer(i4)              :: i
  integer(i4)              :: idum(8)
  integer(i4)              :: inat
  integer(i4)              :: isp
  integer(i4)              :: itype
  integer(i4)              :: j
  real(dp)                 :: aloc
  real(dp)                 :: alp
  real(dp)                 :: bloc
  real(dp)                 :: bet
  real(dp)                 :: cloc
  real(dp)                 :: gam
  real(dp)                 :: q
!
!  Only write from main node in parallel
!
  if (.not.ioproc) return
!
  title = titleword(1)(1:64)
  if (lmdcall) then
    write(title,'(f10.5,'' ps'',51x)') timesofar
  elseif (nstep.gt.0) then
    write(title,'(i20,44x)') nstep
  endif
  write(iout,'(a64,f16.6)') title,fc*23.0603618346030599163_dp
!
! Set date and time
!
  dattim = ' '
  call date_and_time(dattim(1:8),dattim(10:20),dattim(21:25),idum)
!
  write(iout,'(''!DATE: '',a25)') dattim
  if (ndim.eq.3) then
    spaceg(1:1) = '('
    do i = 1,16
      spaceg(i+1:i+1) = hmssg(i,ncf)
    enddo
    do i = 17,1,-1
      if (spaceg(i:i).ne.' ') then
        spaceg(i+1:i+1)=')'
        exit
      endif
    enddo
    if (lconp) then
      call uncell3D(xcell,aloc,bloc,cloc,alp,bet,gam)
      write(iout,'(''PBC'',6f10.4,1x,a)') aloc,bloc,cloc,alp,bet,gam,spaceg(1:i+1)
    else
      write(iout,'(''PBC'',6f10.4,1x,a)') a,b,c,alpha,beta,gamma,spaceg(1:i+1)
    endif
  elseif (ndim.eq.2) then
    if (lconp) then
      call uncell2D(xcell,aloc,bloc,alp)
      write(iout,'(''PBC'',3f10.4)') aloc,bloc,alp
    else
      write(iout,'(''PBC'',3f10.4)') a,b,alpha
    endif
  elseif (ndim.eq.1) then
    if (lconp) then
      write(iout,'(''PBC'',f10.4)') xcell(1)
    else
      write(iout,'(''PBC'',f10.4)') a
    endif
  endif
  do i = 1,numat
    inat = nat(i)
    if (inat.le.maxele) then
      itype = natype(i)
      call label(inat,itype,lab)
      if (lab(2:2).ge.'a'.and.lab(2:2).le.'z') lab(2:2) = char(ichar(lab(2:2))-32)
      if (lab(2:2).eq.' ') then
        j = 2
      else
        j = 3
      endif
      if (i.le.9) write(lab(j:j),'(i1)') i
      if (i.gt.9.and.i.le.99) write(lab(j:j+1),'(i2)') i
      if (.not.loutshell) then
        isp = ncsptr(i)
        if (isp.ne.0) then
          q = qa(i) + qa(isp)
        else
          q = qa(i)
        endif
      else
        q = qa(i)
      endif
      write(iout,'(a4,1x,3f15.9,'' CORE Z0A    '',a2,''      '',a2,1x,f6.3)')  &
        lab,xalat(i),yalat(i),zalat(i),atsym(inat),atsym(inat),q
    endif
  enddo
  if (loutshell) then
    write(iout,'(''end'')')
    do i = 1,numat
      inat = nat(i)
      if (inat.gt.maxele) then
        itype = natype(i)
        call label(inat,itype,lab)
        if (lab(2:2).ge.'a'.and.lab(2:2).le.'z') lab(2:2) = char(ichar(lab(2:2))-32)
        if (lab(2:2).eq.' ') then
          j = 2
        else
          j = 3
        endif
        if (i.le.9) write(lab(j:j),'(i1)') i
        if (i.gt.9.and.i.le.99) write(lab(j:j+1),'(i2)') i
        write(iout,'(a4,1x,3(f15.9),'' SHEL Z0A    '',a2,''      '',a2,1x,f6.3)')  &
          lab,xalat(i),yalat(i),zalat(i),atsym(inat),atsym(inat),qa(i)
      endif
    enddo
  endif
  write(iout,'(''end'',/,''end'')')
!
  return
  end
