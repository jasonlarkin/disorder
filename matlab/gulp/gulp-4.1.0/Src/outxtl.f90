  subroutine outxtl(iout)
!
!  Write out MSI XTL input files
!
!  12/00 Modified to handle 2-D systems
!   5/01 Modified to handle multiple configurations by
!        using multiple file names
!   6/07 lall set to false in calls to setup to avoid
!        potential recursive call issue
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
  integer(i4)              :: iout
!
!  Local variables
!
  character(len=5)         :: lab
  character(len=1)         :: numbers(10)
  character(len=16)        :: spacegroup
  character(len=80)        :: xtlfile2
  character(len=80)        :: xtlfilel
  integer(i4)              :: i
  integer(i4)              :: ii
  integer(i4)              :: inat
  integer(i4)              :: incf
  integer(i4)              :: ind
  integer(i4)              :: itype
  integer(i4)              :: jj
  integer(i4)              :: na
  integer(i4)              :: nc
  integer(i4)              :: nr
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!
  do nc = 1,ncfg
    ncf = nc
    if (ndimen(ncf).gt.0) then
!***************************
!  Initialisation of file  *
!***************************
      xtlfile2 = ' '
      xtlfilel = xtlfile
!
!  If xtl file name has been given then open file
!
      if (xtlfilel(1:1).eq.' ') then
        xtlfilel = 'gulp.xtl'
      endif
      ind = index(xtlfilel,'.xtl')
      if (ind.eq.0) then
        call endstring(xtlfilel,len(xtlfilel),ind)
        xtlfilel(ind:ind+3) = '.xtl'
      endif
      xtlfile2(1:ind-1) = xtlfilel(1:ind-1)
      if (ncf.ge.100) then
        xtlfile2(ind+4:ind+7) = xtlfilel(ind:ind+3)
        xtlfile2(ind:ind) = '_'
        incf = ncf
        ii = incf/10
        incf = incf - ii*10
        jj = incf/10
        incf = incf - jj*10
        xtlfile2(ind+1:ind+1) = numbers(ii+1)
        xtlfile2(ind+2:ind+2) = numbers(jj+1)
        xtlfile2(ind+3:ind+3) = numbers(incf+1)
      elseif (ncf.ge.10) then
        xtlfile2(ind+3:ind+6) = xtlfilel(ind:ind+3)
        xtlfile2(ind:ind) = '_'
        incf = ncf
        ii = incf/10
        incf = incf - ii*10
        xtlfile2(ind+1:ind+1) = numbers(ii+1)
        xtlfile2(ind+2:ind+2) = numbers(incf+1)
      elseif (ncfg.gt.1) then
        xtlfile2(ind+2:ind+5) = xtlfilel(ind:ind+3)
        xtlfile2(ind:ind) = '_'
        xtlfile2(ind+1:ind+1) = numbers(ncf+1)
      else
        xtlfile2(ind:ind+3) = xtlfilel(ind:ind+3)
      endif
      open(iout,file=xtlfile2,status='unknown')
      if (ncfg.gt.1) then
        write(ioout,'(''  XTL file written for configuration '',i4,'' as '',a30)') ncf,xtlfile2(1:30)
      else
        write(ioout,'(''  XTL file written as '',a30)') xtlfile2(1:30)
      endif
!**********************************************************************
!  Write out title or name
!**********************************************************************
      if (ntitle.gt.0) then
        write(iout,'(''TITLE '',a74)')titleword(1)(1:74)
      elseif (names(ncf).ne.' ') then
        write(iout,'(''TITLE '',a74)')names(ncf)(1:74)
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
        if (nspcg(ncf).gt.1) then
          if (ncbl.gt.1.and.(ifhr(ncf).eq.0.or.lhex)) then
            call uncentre(rv)
          endif
        endif
        call uncell3D(rv,a,b,c,alpha,beta,gamma)
!
!  Transform hexagonal fractional coordinates back to 
!  rhombohedral if ifhr2=1, unless hexagonal final cell
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
!  Crystal structure info first
!
        write(iout,'(''CELL'')')
        write(iout,'(6f11.6)') a,b,c,alpha,beta,gamma
        if (lsymopt) then
!
!  Reduce symbol
!
          spacegroup = '         '
          ind = 0
          do i = 1,16
            if (hmssg(i,ncf).ne.' ') then
              ind = ind + 1
              spacegroup(ind:ind) = hmssg(i,ncf)
            endif
          enddo
          write(iout,'(''SYMMETRY NUMBER '',i3)') nspcg(ncf)
          write(iout,'(''SYMMETRY LABEL  '',a16)') spacegroup
          if (ifso(ncf).eq.1) then
            write(iout,'(''SYMMETRY QUALIFIER origin_2'')')
          endif
          if (nspcg(ncf).ge.3.and.nspcg(ncf).le.15) then
            if (beta.ne.90.0) then
              write(iout,'(''SYMMETRY QUALIFIER b_unique'')')
            elseif (alpha.ne.90.0) then
              write(iout,'(''SYMMETRY QUALIFIER a_unique'')')
            elseif (gamma.ne.90.0) then
              write(iout,'(''SYMMETRY QUALIFIER c_unique'')')
            endif
          endif
        endif
        write(iout,'(''ATOMS'')')
        write(iout,'(''NAME'',9x,''X'',11x,''Y'',11x,''Z'')')
        do i = 1,nasym
          if (iatn(i).le.maxele) then
            inat = iatn(i)
            itype = natype(i)
            call label(inat,itype,lab)
            write(iout,'(a5,2x,3f12.5)') lab,xcfg(i+nsft),ycfg(i+nsft),zcfg(i+nsft)
          endif
        enddo
      elseif (ndimen(ncf).eq.2) then
        call setup(.false.)
!
!  Surface cell info first
!
        write(iout,'(''DIMENSION 2'')')
        write(iout,'(''CELL'')')
        write(iout,'(3f11.6)')a,b,alpha
        write(iout,'(''ATOMS'')')
        write(iout,'(''NAME'',9x,''X'',11x,''Y'',11x,''Z'')')
        do i = 1,nasym
          if (iatn(i).le.maxele) then
            inat = iatn(i)
            itype = natype(i)
            call label(inat,itype,lab)
            write(iout,'(a5,2x,3f12.5)') lab,xcfg(i+nsft),ycfg(i+nsft),zcfg(i+nsft)
          endif
        enddo
      elseif (ndimen(ncf).eq.1) then
        call setup(.false.)
!
!  Polymer cell info first
!
        write(iout,'(''DIMENSION 1'')')
        write(iout,'(''CELL'')')
        write(iout,'(f11.6)') a
        write(iout,'(''ATOMS'')')
        write(iout,'(''NAME'',9x,''X'',11x,''Y'',11x,''Z'')')
        do i=1,nasym
          if (iatn(i).le.maxele) then
            inat = iatn(i)
            itype = natype(i)
            call label(inat,itype,lab)
            write(iout,'(a5,2x,3f12.5)') lab,xcfg(i+nsft),ycfg(i+nsft),zcfg(i+nsft)
          endif
        enddo
      endif
      write(iout,'(''EOF'',/)')
      close(iout)
    endif
!******************************
!  End of configuration loop  *
!******************************
  enddo
!
  return
  end
