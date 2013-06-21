  subroutine outdlv(iout)
!
!  Write out CRYSTAL STR input files for DLV
!
!   8/02 Created from outxtl
!   6/07 lall set to false in calls to setup to avoid
!        potential recursive call issue
!  12/07 Unused variables removed
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
  integer(i4)          :: iout
!
!  Local variables
! 
  character(len=80)    :: dlvfile2
  character(len=80)    :: dlvfilel
  character(len=1)     :: numbers(10)
  integer(i4)          :: i
  integer(i4)          :: ifail
  integer(i4)          :: ii
  integer(i4)          :: inat
  integer(i4)          :: incf
  integer(i4)          :: ind
  integer(i4)          :: j
  integer(i4)          :: jj
  integer(i4)          :: na
  integer(i4)          :: nc
  integer(i4)          :: nasymcore
  integer(i4)          :: nr
  real(dp)             :: rrl(6)
  real(dp)             :: rvi(3,3)
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!
  do nc = 1,ncfg
    ncf = nc
!***************************
!  Initialisation of file  *
!***************************
    dlvfile2 = ' '
    dlvfilel = dlvfile
!
!  If dlv file name has been given then open file
!
    if (dlvfilel(1:1).eq.' ') then
      dlvfilel = 'gulp.str'
    endif
    ind = index(dlvfilel,'.str')
    if (ind.eq.0) then
      call endstring(dlvfilel,len(dlvfilel),ind)
      dlvfilel(ind:ind+3) = '.str'
    endif
    dlvfile2(1:ind-1) = dlvfilel(1:ind-1)
    if (ncf.ge.100) then
      dlvfile2(ind+4:ind+7) = dlvfilel(ind:ind+3)
      dlvfile2(ind:ind) = '_'
      incf = ncf
      ii = incf/10
      incf = incf-ii*10
      jj = incf/10
      incf = incf-jj*10
      dlvfile2(ind+1:ind+1) = numbers(ii+1)
      dlvfile2(ind+2:ind+2) = numbers(jj+1)
      dlvfile2(ind+3:ind+3) = numbers(incf+1)
    elseif (ncf.ge.10) then
      dlvfile2(ind+3:ind+6) = dlvfilel(ind:ind+3)
      dlvfile2(ind:ind) = '_'
      incf = ncf
      ii = incf/10
      incf = incf-ii*10
      dlvfile2(ind+1:ind+1) = numbers(ii+1)
      dlvfile2(ind+2:ind+2) = numbers(incf+1)
    elseif (ncfg.gt.1) then
      dlvfile2(ind+2:ind+5) = dlvfilel(ind:ind+3)
      dlvfile2(ind:ind) = '_'
      dlvfile2(ind+1:ind+1) = numbers(ncf+1)
    else
      dlvfile2(ind:ind+3) = dlvfilel(ind:ind+3)
    endif
    open(iout,file = dlvfile2,status='unknown')
    if (ncfg.gt.1) then
      write(ioout,'(''  STR file written for configuration '',i4,'' as '',a30)')ncf,dlvfile2(1:30)
    else
      write(ioout,'(''  STR file written as '',a30)') dlvfile2(1:30)
    endif
!*****************************
!  Loop over configurations  *
!*****************************
    if (ndimen(ncf).eq.3) then
      call setup(.false.)
!
!  Transform primitive cell back to original cell
!
      do i = 1,3
        rv(1,i) = rvcfg(1,i,ncf)
        rv(2,i) = rvcfg(2,i,ncf)
        rv(3,i) = rvcfg(3,i,ncf)
      enddo
      do i = 1,3
        rvi(1,i) = rv(1,i)
        rvi(2,i) = rv(2,i)
        rvi(3,i) = rv(3,i)
      enddo
      ifail = 0
      call matinv(rvi,3_i4,3_i4,rrl,ifail)
!
!  Transform hexagonal fractional coordinates back to 
!  rhombohedral if ifhr2 = 1, unless hexagonal final cell
!  has been requested (lhex)
!
      if (ifhr(ncf).eq.1.and.(.not.lhex)) then
        do na = 1,nasym
          nr = nrel2(na)
          xcfg(nsft+na) = xfrac(nr)
          ycfg(nsft+na) = yfrac(nr)
          zcfg(nsft+na) = zfrac(nr)
        enddo
      endif
!
!  Crystal structure and symmetry info first
!
      write(iout,'(3i2)') ndim,ncbl,nccs
      write(iout,'(3f15.6)')(rv(j,1),j = 1,3)
      write(iout,'(3f15.6)')(rv(j,2),j = 1,3)
      write(iout,'(3f15.6)')(rv(j,3),j = 1,3)
!
!  Space group operators
!
      if (ngo.gt.0) then
        write(iout,'(i3)') ngo
        do i = 1,ngo
          write(iout,'(3f15.6)') (rop(j,1,i),j = 1,3)
          write(iout,'(3f15.6)') (rop(j,2,i),j = 1,3)
          write(iout,'(3f15.6)') (rop(j,3,i),j = 1,3)
          write(iout,'(3f15.6)') (vit(j,i),j = 1,3)
        enddo
      else
        write(iout,'(''  1'')')
        write(iout,'(''1.0 0.0 0.0'')')
        write(iout,'(''0.0 1.0 0.0'')')
        write(iout,'(''0.0 0.0 1.0'')')
        write(iout,'(''0.0 0.0 0.0'')')
      endif
    elseif (ndimen(ncf).eq.2) then
      call setup(.false.)
!
!  Surface cell info first
!
      write(iout,'('' 2 1 1'')')
      write(iout,'(3f15.6)')(rv(j,1),j = 1,3)
      write(iout,'(3f15.6)')(rv(j,2),j = 1,3)
      write(iout,'(3f15.6)') 0.0_dp,0.0_dp,0.0_dp
      write(iout,'(''  1'')')
      write(iout,'(''1.0 0.0 0.0'')')
      write(iout,'(''0.0 1.0 0.0'')')
      write(iout,'(''0.0 0.0 1.0'')')
      write(iout,'(''0.0 0.0 0.0'')')
    elseif (ndimen(ncf).eq.1) then
      call setup(.false.)
!
!  Polymer cell info first
!
      write(iout,'('' 1 1 1'')')
      write(iout,'(3f15.6)')(rv(j,1),j = 1,3)
      write(iout,'(3f15.6)') 0.0_dp,0.0_dp,0.0_dp
      write(iout,'(3f15.6)') 0.0_dp,0.0_dp,0.0_dp
      write(iout,'('' 0'')')
    elseif (ndimen(ncf).eq.0) then
      call setup(.false.)
!
!  Cluster
!
      write(iout,'('' 0 1 1'')')
      write(iout,'(3f15.6)') 0.0_dp,0.0_dp,0.0_dp
      write(iout,'(3f15.6)') 0.0_dp,0.0_dp,0.0_dp
      write(iout,'(3f15.6)') 0.0_dp,0.0_dp,0.0_dp
      write(iout,'(''  1'')')
      write(iout,'(''1.0 0.0 0.0'')')
      write(iout,'(''0.0 1.0 0.0'')')
      write(iout,'(''0.0 0.0 1.0'')')
      write(iout,'(''0.0 0.0 0.0'')')
    endif
!
!  Find cores in asymmetric unit
!
    nasymcore = 0
    do i = 1,nasym
      if (iatn(i).le.maxele) then
        nasymcore = nasymcore + 1
      endif
    enddo
!
!  Atoms
!
    write(iout,'(i6)') nasymcore
    do i  =  1,nasym
      if (iatn(i).le.maxele) then
        inat = iatn(i)
        write(iout,'(i3,1x,3(f12.6,1x))') inat,xalat(i),yalat(i),zalat(i)
      endif
    enddo
    close(iout)
!******************************
!  End of configuration loop  *
!******************************
  enddo
!
  return
  end
