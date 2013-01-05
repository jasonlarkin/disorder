  subroutine outbiograf(iout)
!
!  Write out biograf format input files suitable for use with ReaxFF code
!
!  11/07 Created
!  12/07 Unused variables removed
!  12/07 Cell parameter output added
!   3/12 Use of iatn replaced by nat for accessing atomic number
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
!  Julian Gale, NRI, Curtin University, March 2012
!
  use control
  use configurations
  use current
  use element
  use files
  use general
  use iochannels
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4)              :: iout
!
!  Local variables
!
  character(len=1)         :: numbers(10)
  character(len=80)        :: biofile2
  character(len=80)        :: biofilel
  character(len=80)        :: line
  integer(i4)              :: i
  integer(i4)              :: ii
  integer(i4)              :: inat
  integer(i4)              :: incf
  integer(i4)              :: ind
  integer(i4)              :: jj
  integer(i4)              :: nc
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!
!*****************************
!  Loop over configurations  *
!*****************************
  do nc = 1,ncfg
    ncf = nc
!***************************
!  Initialisation of file  *
!***************************
    biofile2 = ' '
    biofilel = biofile
!
!  If bio file name has been given then open file
!
    if (biofilel(1:1).eq.' ') then
      biofilel = 'gulp.bio'
    endif
    ind = index(biofilel,'.bio')
    if (ind.eq.0) then
      ind = index(biofilel,' ')
      biofilel(ind:ind+3) = '.bio'
    endif
    biofile2(1:ind-1) = biofilel(1:ind-1)
    if (ncf.ge.100) then
      biofile2(ind+4:ind+7) = biofilel(ind:ind+3)
      biofile2(ind:ind) = '_'
      incf = ncf
      ii = incf/10
      incf = incf - ii*10
      jj = incf/10
      incf = incf - jj*10
      biofile2(ind+1:ind+1) = numbers(ii+1)
      biofile2(ind+2:ind+2) = numbers(jj+1)
      biofile2(ind+3:ind+3) = numbers(incf+1)
    elseif (ncf.ge.10) then
      biofile2(ind+3:ind+6) = biofilel(ind:ind+3)
      biofile2(ind:ind) = '_'
      incf = ncf
      ii = incf/10
      incf = incf - ii*10
      biofile2(ind+1:ind+1) = numbers(ii+1)
      biofile2(ind+2:ind+2) = numbers(incf+1)
    elseif (ncfg.gt.1) then
      biofile2(ind+2:ind+5) = biofilel(ind:ind+3)
      biofile2(ind:ind) = '_'
      biofile2(ind+1:ind+1) = numbers(ncf+1)
    else
      biofile2(ind:ind+3) = biofilel(ind:ind+3)
    endif
    open(iout,file=biofile2,status='unknown')
    if (ncfg.gt.1) then
      write(ioout,'(''  Biograf file written for configuration '',i4,'' as '',a30)') ncf,biofile2(1:30)
    else
      write(ioout,'(''  Biograf file written as '',a30)') biofile2(1:30)
    endif
!**********************
!  Write out headers  *
!**********************
    write(iout,'(''BIOGRF 200'')')
    if (names(ncf)(1:1).eq.' ') then
      write(iout,'(''DESCRP gulp'')') 
    else
      write(iout,'(''DESCRP '',a73)') names(ncf)(1:73)
    endif
    write(iout,'(''REMARK'')')
!**************************
!  Cell parameter output  *
!**************************
    if (ndim.eq.3) then
      write(iout,'(''CRYSTX'',1x,6f11.5)') a,b,c,alpha,beta,gamma
    endif
!****************
!  Atom output  *
!****************
    write(iout,'(''RUTYPE NORMAL RUN'')')
    line = 'FORMAT ATOM   (a6,1x,i5,1x,a5,1x,a3,1x,a1,1x,a5,3f10.5,1x,a5,i3,i2,1x,f8.5)'
    write(iout,'(a80)') line
    call setup(.false.)
    do i = 1,numat
      inat = mod(nat(i),maxele)
      write(iout,'(''HETATM'',1x,i5,1x,a2,15x,3f10.5,1x,a2,3x,i3,i2,1x,f8.5)') &
        i,atsym(inat),xclat(i),yclat(i),zclat(i),atsym(inat),1,1,0.0_dp
    enddo
    write(iout,'(''END'')')
    close(iout)
!******************************
!  End of configuration loop  *
!******************************
  enddo
!
  return
  end
