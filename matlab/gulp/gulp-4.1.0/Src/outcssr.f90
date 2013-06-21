  subroutine outcssr(iout)
!
!  Write out Standard CSSR file (for input to Cerius)
!
!   1/99 Cell parameters corrected to be those of the centred cell
!   5/01 Handling of multiple configurations added
!   1/05 Style updated
!   6/07 lall set to false in calls to setup to avoid
!        potential recursive call issue
!  12/07 Unused variables removed
!   3/09 0-D case added
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
  use configurations
  use control
  use current
  use element
  use files
  use general
  use iochannels
  use shell
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: iout
!
!  Local variables
!
  character(len=80)  :: cssrfile2
  character(len=80)  :: cssrfilel
  character(len=5)   :: lab
  character(len=1)   :: numbers(10)
  integer(i4)        :: i
  integer(i4)        :: i0(8)
  integer(i4)        :: ii
  integer(i4)        :: inat
  integer(i4)        :: incf
  integer(i4)        :: ind
  integer(i4)        :: itype
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: nc
  integer(i4)        :: nshasym
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!
  do i = 1,8
    i0(i) = 0
  enddo
!
!  If dumpfile name has been given then open file
!
  if (cssrfile(1:1).ne.' ') then
    open(iout,file=cssrfile,status='unknown')
  endif
!*****************************
!  Loop over configurations  *
!*****************************
  do nc = 1,ncfg
    ncf = nc
    if (ndimen(ncf).eq.3.or.ndimen(ncf).eq.0) then
!***************************
!  Initialisation of file  *
!***************************
      cssrfile2 = ' '
      cssrfilel = cssrfile
!
!  If cssr file name has been given then open file
!
      if (cssrfilel(1:1).eq.' ') then
        cssrfilel = 'gulp.cssr'
      endif
      ind = index(cssrfilel,'.cssr')
      if (ind.eq.0) then
        call endstring(cssrfilel,len(cssrfilel),ind)
        cssrfilel(ind:ind+3) = '.cssr'
      endif
      cssrfile2(1:ind-1) = cssrfilel(1:ind-1)
      if (ncf.ge.100) then
        cssrfile2(ind+4:ind+7) = cssrfilel(ind:ind+3)
        cssrfile2(ind:ind) = '_'
        incf = ncf
        ii = incf/10
        incf = incf - ii*10
        jj = incf/10
        incf = incf - jj*10
        cssrfile2(ind+1:ind+1) = numbers(ii+1)
        cssrfile2(ind+2:ind+2) = numbers(jj+1)
        cssrfile2(ind+3:ind+3) = numbers(incf+1)
      elseif (ncf.ge.10) then
        cssrfile2(ind+3:ind+6) = cssrfilel(ind:ind+3)
        cssrfile2(ind:ind) = '_'
        incf = ncf
        ii = incf/10
        incf = incf - ii*10
        cssrfile2(ind+1:ind+1) = numbers(ii+1)
        cssrfile2(ind+2:ind+2) = numbers(incf+1)
      elseif (ncfg.gt.1) then
        cssrfile2(ind+2:ind+5) = cssrfilel(ind:ind+3)
        cssrfile2(ind:ind) = '_'
        cssrfile2(ind+1:ind+1) = numbers(ncf+1)
      else
        cssrfile2(ind:ind+3) = cssrfilel(ind:ind+3)
      endif
      open(iout,file = cssrfile2,status='unknown')
      if (ncfg.gt.1) then
        write(ioout,'(''  CSSR file written for configuration '',i4,'' as '',a30)')ncf,cssrfile2(1:30)
      else
        write(ioout,'(''  CSSR file written as '',a30)') cssrfile2(1:30)
      endif
!
      call setup(.false.)
!
!  3-D cell handling
!
      if (ndimen(ncf).eq.3) then
!
!  Transform primitive cell back to original cell
!
        do i = 1,3
          rv(1,i) = rvcfg(1,i,ncf)
          rv(2,i) = rvcfg(2,i,ncf)
          rv(3,i) = rvcfg(3,i,ncf)
        enddo
        if (nspcg(ncf).gt.1) then
          if (ncbl.gt.1.and.(ifhr(ncf).eq.0.or.lhex)) then
            call uncentre(rv)
          endif
        endif
        call uncell3D(rv,a,b,c,alpha,beta,gamma)
!
!  Unit cell information
!
        write(iout,'(38x,3f8.3)') a,b,c
        if (nspcg(ncf).eq.0) then
          write(iout,'(21x,3f8.3,4x,''SPGR =    1 P 1'')') alpha,beta,gamma
        else
          write(iout,'(21x,3f8.3,4x,''SPGR =  '',i3,1x,11a1)') alpha,beta,gamma,nspcg(ncf),(hmssg(i,ncf),i=1,11)
        endif
      else
!
!  0-D - leave cell lines blank
!
        write(iout,'('' '')') 
        write(iout,'('' '')') 
      endif
!
!  Number of atoms and title - remove number of shells
!
      nshasym = 0
      if (nshell.gt.0) then
        do i = 1,nasym
          if (iatn(i).ge.maxele) nshasym = nshasym + 1
        enddo
      endif
      if (ntitle.ge.1) then
        write(iout,'(i4,''   0'',1x,a60)') nasym - nshasym,titleword(1)(1:60)
      else
        write(iout,'(i4,''   0'')') nasym - nshasym
      endif
!
!  Title line
!
      if (ndimen(ncf).eq.3) then
        write(iout,'(''bulk CSSR file written from GULP'')')
      else
        write(iout,'(''cluster CSSR file written from GULP'')')
      endif
!
!  Atom data - only write out cores : fractional for 3-D and Cartesian for 0-D
!
      if (ndimen(ncf).eq.3) then
        do i = 1,nasym
          inat = iatn(i)
          if (inat.le.maxele) then
            itype = natype(i)
            call label(inat,itype,lab)
            write(iout,'(i4,1x,a4,2x,3(f9.5,1x),8i4,1x,f7.3)') &
              i,lab,xcfg(i+nsft),ycfg(i+nsft),zcfg(i+nsft),(i0(j),j = 1,8),qa(i)
          endif
        enddo
      else
        do i = 1,nasym
          inat = iatn(i)
          if (inat.le.maxele) then
            itype = natype(i)
            call label(inat,itype,lab)
            write(iout,'(i4,1x,a4,2x,3(f9.5,1x),8i4,1x,f7.3)') &
              i,lab,xalat(i),yalat(i),zalat(i),(i0(j),j = 1,8),qa(i)
          endif
        enddo
      endif
      close(iout)
    endif
!******************************
!  End of configuration loop  *
!******************************
  enddo
!
  return
  end
