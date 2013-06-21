  subroutine outxyz(iout,lappend,ldefectloc)
!
!  Write out XYZ file for Molden/Unichem
!
!  lappend = if .true. then structure should be appended
!            to file (for optimisation sequence)
!  ldefectloc = if .true. then output is for defect section of
!            bulk run
!
!   5/98 Created from outarc
!   2/08 Corrected for change from iatn to natdefe for atomic
!        numbers in a defective region 1.
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
  use defects
  use element
  use energies
  use files
  use iochannels
  use shell
  use species
  implicit none
!
!  Passed variables
!
  integer(i4)       :: iout
  logical           :: lappend
  logical           :: ldefectloc
!
!  Local variables
!
  character(len=5)  :: lab
  character(len=4)  :: lab2
  character(len=1)  :: numbers(10)
  character(len=80) :: xyzfile2
  character(len=80) :: xyzfilel
  integer(i4)       :: i
  integer(i4)       :: ii
  integer(i4)       :: incf
  integer(i4)       :: ind
  integer(i4)       :: jj
  integer(i4)       :: ni
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!***************************
!  Initialisation of file  *
!***************************
  if (.not.lappend) then
    xyzfile2 = ' '
    xyzfilel = xyzfile
!
!  If xyz file name has been given then open file
!
    if (xyzfilel(1:1).eq.' ') then
      xyzfilel = 'gulp.xyz'
    endif
    ind = index(xyzfilel,'.xyz')
    if (ind.eq.0) then
      call endstring(xyzfilel,len(xyzfilel),ind)
      xyzfilel(ind:ind+3) = '.xyz'
    endif
    xyzfile2(1:ind-1) = xyzfilel(1:ind-1)
    if (ncf.ge.100) then
      xyzfile2(ind+4:ind+7) = xyzfilel(ind:ind+3)
      xyzfile2(ind:ind) = '_'
      incf = ncf
      ii = incf/10
      incf = incf - ii*10
      jj = incf/10
      incf = incf - jj*10
      xyzfile2(ind+1:ind+1) = numbers(ii+1)
      xyzfile2(ind+2:ind+2) = numbers(jj+1)
      xyzfile2(ind+3:ind+3) = numbers(incf+1)
    elseif (ncf.ge.10) then
      xyzfile2(ind+3:ind+6) = xyzfilel(ind:ind+3)
      xyzfile2(ind:ind) = '_'
      incf = ncf
      ii = incf/10
      incf = incf - ii*10
      xyzfile2(ind+1:ind+1) = numbers(ii+1)
      xyzfile2(ind+2:ind+2) = numbers(incf+1)
    elseif (ncfg.gt.1) then
      xyzfile2(ind+2:ind+5) = xyzfilel(ind:ind+3)
      xyzfile2(ind:ind) = '_'
      xyzfile2(ind+1:ind+1) = numbers(ncf+1)
    else
      xyzfile2(ind:ind+3) = xyzfilel(ind:ind+3)
    endif
    open(iout,file=xyzfile2,status='unknown')
!
!  Write information to output
!
    if (lxyzmovie) then
      write(ioout,'(/,''  XYZ File open for Movie as '',a40,/)') xyzfile2(1:40)
    else
      write(ioout,'(/,''  XYZ File written as '',a40,/)') xyzfile2(1:40)
    endif
  endif
!***********************
!  Configuration dump  *
!***********************
!
!  Write out number of atoms
!
  if (ldefectloc) then
    write(iout,'(i8)') ncoreg1
  else
    write(iout,'(i8)') numat - nshell
  endif
!
!  Write out the energy line
!
  write(iout,'(''SCF Done '',f24.8)') fcsave
  lab = ' '
  if (ldefectloc) then
!******************
!  Defect output  *
!******************
!
!  Cores only
!
    do i = 1,nreg1
      if (natdefe(i).le.maxele) then
        ni = natdefe(i)
        call label(ni,0_i4,lab)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f20.9)') lab2,xdefe(i),ydefe(i),zdefe(i)
      endif
    enddo
  else
!*******************
!  Cluster output  *
!*******************
!
!  Cores only
!
    do i = 1,numat
      if (nat(i).le.maxele) then
        ni = nat(i)
        call label(ni,0_i4,lab)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f20.9)') lab2,xclat(i),yclat(i),zclat(i)
      endif
    enddo
  endif
!
  return
  end
