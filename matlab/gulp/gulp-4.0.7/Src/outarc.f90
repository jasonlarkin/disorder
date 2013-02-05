  subroutine outarc(iout,lappend,ldefectloc)
!
!  Write out MSI arc file
!
!  lappend = if .true. then structure should be appended
!            to file (for optimisation sequence)
!  ldefectloc = if .true. then output is for defect section of
!            bulk run
!
!  species symbols have to be truncated to 4 characters due
!  to fixed format of arc files
!
!  20/6/97 Correction added to orientation of cartesian coords for
!          periodic case to match standard cell orientation.
! 31/10/02 Defect data altered to use new arrays
!  6/10/04 Labels _clus etc changed to 0D/1D/2D/3D
!  8/03/05 2D arc file modifications added
!  9/08/05 1 & 2-D output corrected
! 18/10/05 Modified to output arc 3 format by Paul Strodel 
!          (Accelrys)
! 12/11/06 Charges for defect output corrected to qdefe instead of qa
! 28/11/06 xfracimage etc introduced
! 29/06/12 File handling changed to allow for spaces in names
! 29/09/12 Residue field changed to be the molecule number
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
!  Julian Gale, NRI, Curtin University, September 2012
!
  use configurations
  use constants
  use control
  use current
  use defects
  use element
  use energies
  use files
  use general
  use iochannels
  use molecule,         only : natmol
  use shell
  use species
  use symmetry
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
  character(len=1)  :: blank
  character(len=1)  :: numbers(10)
  character(len=2)  :: asym
  character(len=4)  :: wtype1
  character(len=4)  :: lab2
  character(len=5)  :: lab
  character(len=6)  :: resid
  character(len=16) :: spacegroup 
  character(len=80) :: arcfilel
  character(len=80) :: arcfile2
  integer(i4)       :: i
  integer(i4)       :: iarcv
  integer(i4)       :: ii
  integer(i4)       :: incf
  integer(i4)       :: ind
  integer(i4)       :: jj
  integer(i4)       :: ncc
  integer(i4)       :: ni
  integer(i4)       :: ns
  integer(i4)       :: ntp
  real(dp)          :: xci
  real(dp)          :: yci
  real(dp)          :: zci
  real(dp)          :: rvl(3,3)
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!
  blank = ' '
  iarcv = 3
!***************************
!  Initialisation of file  *
!***************************
  if (.not.lappend) then
    arcfile2 = ' '
    arcfilel = arcfile
!
!  If arc file name has been given then open file
!
    if (arcfilel(1:1).eq.' ') then
      arcfilel = 'gulp.arc'
    endif
    ind = index(arcfilel,'.arc')
    if (ind.eq.0) then
      ind = index(arcfilel,'.car')
      if (ind.eq.0) then
        call endstring(arcfilel,len(arcfilel),ind)
        arcfilel(ind:ind+3) = '.arc'
      endif
    endif
    arcfile2(1:ind-1) = arcfilel(1:ind-1)
    if (ndim.eq.3) then
      if (ldefectloc) then
        arcfile2(ind:ind+2) = '_0D'
      else
        arcfile2(ind:ind+2) = '_3D'
      endif
    elseif (ndim.eq.2) then
      arcfile2(ind:ind+2) = '_2D'
    elseif (ndim.eq.1) then
      arcfile2(ind:ind+2) = '_1D'
    else
      arcfile2(ind:ind+2) = '_0D'
    endif
    if (ncf.ge.100) then
      arcfile2(ind+7:ind+10) = arcfilel(ind:ind+3)
      arcfile2(ind+3:ind+3) = '_'
      incf = ncf
      ii = incf/10
      incf = incf - ii*10
      jj = incf/10
      incf = incf - jj*10
      arcfile2(ind+4:ind+4) = numbers(ii+1)
      arcfile2(ind+5:ind+5) = numbers(jj+1)
      arcfile2(ind+6:ind+6) = numbers(incf+1)
    elseif (ncf.ge.10) then
      arcfile2(ind+6:ind+9) = arcfilel(ind:ind+3)
      arcfile2(ind+3:ind+3) = '_'
      incf = ncf
      ii = incf/10
      incf = incf - ii*10
      arcfile2(ind+4:ind+4) = numbers(ii+1)
      arcfile2(ind+5:ind+5) = numbers(incf+1)
    elseif (ncfg.gt.1) then
      arcfile2(ind+5:ind+8) = arcfilel(ind:ind+3)
      arcfile2(ind+3:ind+3) = '_'
      arcfile2(ind+4:ind+4) = numbers(ncf+1)
    else
      arcfile2(ind+3:ind+6) = arcfilel(ind:ind+3)
    endif
    open(iout,file=arcfile2,status='unknown')
!
!  Write out header
!
    write(iout,'(''!BIOSYM archive '',i1)') iarcv
    if (ndim.eq.3.and..not.ldefectloc) then
      write(iout,'(''PBC=ON'')')
    elseif (ndim.eq.2) then
      write(iout,'(''PBC=2D'')')
    elseif (ndim.eq.1) then
      write(iout,'(''PBC=1D'')')
    else
      write(iout,'(''PBC=OFF'')')
    endif
!
!  Write information to output
!
    if (lmovie) then
      write(ioout,'(/,''  MSI Archive (version '',i1,'') open for Movie as '',a40,/)')iarcv,arcfile2(1:40)
    else
      write(ioout,'(/,''  MSI Archive (version '',i1,'') written as '',a40,/)')iarcv,arcfile2(1:40)
    endif
    if (loutshell) then
      write(ioout,'(''  Shells to be included in the archive '',/)')
    endif
  endif
!***********************
!  Configuration dump  *
!***********************
!
!  Write out first title line
!
  if (ntitle.gt.0) then
    write(iout,'(64a1,f16.6)')(titleword(1)(i:i),i=1,64),fcsave*evtokcal
  else
    write(iout,'(64a1,f16.6)')(blank,i=1,64),fcsave*evtokcal
  endif
  write(iout,'(''!DATE'')')
  if (ldefectloc) then
!******************
!  Defect output  *
!******************
    wtype1 = 'CORE'
!
!  Cores
!
    ncc = 0
    do i = 1,nreg1
      ni = natdefe(i)
      if (ni.le.maxele) then
        ncc = ncc + 1
!
!  PS: Need to provide left adjusted resid
!
        write(resid,'(i6)') natmol(ncc)
        resid = adjustl(resid)
        ntp = ntypdefe(i)
        call label(ni,ntp,lab)
        call labellc(asym,ni)
!
!  PS: Need to provide left-adjusted element symbol
!
        asym = adjustl(asym)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f15.9,1x,a4,1x,a6,1x,a2,6x,a2,f7.3)') &
          lab2,xdefe(i),ydefe(i),zdefe(i),wtype1,resid,asym,atsym(ni),qdefe(i)
      endif
    enddo
    write(iout,'(''end'')')
!
!  Shells
!
    if (nshreg1.gt.0.and.loutshell) then
      wtype1 = 'SHEL'
      ns = 0
      do i = 1,nreg1
        ni = natdefe(i)
        if (ni.gt.maxele) then
          ns = ns + 1
          write(resid,'(i6)') natmol(ns + ncc)
          resid = adjustl(resid)
          ni = ni - maxele
          ntp = ntypdefe(i)
          call label(ni,ntp,lab)
          call labellc(asym,ni)
          asym = adjustl(asym)
          lab2 = lab(1:4)
          write(iout,'(a4,1x,3f15.9,1x,a4,1x,a6,1x,a2,6x,a2,f7.3)') &
            lab2,xdefe(i),ydefe(i),zdefe(i),wtype1,resid,asym,atsym(ni),qdefe(i)
        endif
      enddo
      write(iout,'(''end'')')
    endif
  elseif (ndim.gt.0) then
!****************
!  Bulk output  *
!****************
    wtype1 = 'CORE'
!
!  PS: Need spacegroup symbol
!
    spacegroup = '         '
    ind = 0
    do i = 1,16
      if (hmssg(i,1).ne.' ') then
        ind = ind + 1
        spacegroup(ind:ind) = hmssg(i,ncf)
      endif
    enddo
    if (ndim.eq.3) then
      call uncell3D(rv,a,b,c,alpha,beta,gamma)
      write(iout,'(''PBC'',6f10.4,1x,a15)') a,b,c,alpha,beta,gamma,spacegroup
!
!  Generate cell in standard orientation for coordinate output
!
      call cell3D(rvl,a,b,c,alpha,beta,gamma)
    elseif (ndim.eq.2) then
      call uncell2D(rv,a,b,alpha)
      write(iout,'(''PBC'',3f10.4,1x,''P 1'')') a,b,alpha
      call cell2D(rvl,a,b,alpha)
    elseif (ndim.eq.1) then
      write(iout,'(''PBC'',f10.4)') a
    endif
!
!  Cores
!
    ncc = 0
    do i = 1,numat
      if (nat(i).le.maxele) then
        ncc = ncc + 1
        write(resid,'(i6)') natmol(ncc)
        resid = adjustl(resid)
        if (ndim.eq.3) then
          xci = (xfrac(i) - xfracimage(i))*rvl(1,1) + (yfrac(i) - yfracimage(i))*rvl(1,2) + (zfrac(i) - zfracimage(i))*rvl(1,3)
          yci = (xfrac(i) - xfracimage(i))*rvl(2,1) + (yfrac(i) - yfracimage(i))*rvl(2,2) + (zfrac(i) - zfracimage(i))*rvl(2,3)
          zci = (xfrac(i) - xfracimage(i))*rvl(3,1) + (yfrac(i) - yfracimage(i))*rvl(3,2) + (zfrac(i) - zfracimage(i))*rvl(3,3)
        elseif (ndim.eq.2) then
          xci = xfrac(i)*rvl(1,1) + yfrac(i)*rvl(1,2) 
          yci = xfrac(i)*rvl(2,1) + yfrac(i)*rvl(2,2) 
          zci = zfrac(i)
        elseif (ndim.eq.1) then
          xci = xfrac(i)*rvl(1,1) 
          yci = yfrac(i)
          zci = zfrac(i)
        endif
        ni = nat(i)
        ntp = nftype(i)
        call label(ni,ntp,lab)
        call labellc(asym,ni)
        asym = adjustl(asym)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f15.9,1x,a4,1x,a6,1x,a2,6x,a2,f7.3)') &
          lab2,xci,yci,zci,wtype1,resid,asym,atsym(nat(i)),qf(i)
      endif
    enddo
    write(iout,'(''end'')')
!
!  Shells
!
    if (nshell.gt.0.and.loutshell) then
      wtype1 = 'SHEL'
      ns = 0
      do i = 1,nshell
        ns = ns + 1
        write(resid,'(i6)') natmol(ns + ncc)
        resid = adjustl(resid)
        ii = nshptr(i)
        if (ndim.eq.3) then
          xci = (xfrac(ii) - xfracimage(ii))*rvl(1,1) + (yfrac(ii) - yfracimage(ii))*rvl(1,2) +  &
                (zfrac(ii) - zfracimage(ii))*rvl(1,3)
          yci = (xfrac(ii) - xfracimage(ii))*rvl(2,1) + (yfrac(ii) - yfracimage(ii))*rvl(2,2) +  &
                (zfrac(ii) - zfracimage(ii))*rvl(2,3)
          zci = (xfrac(ii) - xfracimage(ii))*rvl(3,1) + (yfrac(ii) - yfracimage(ii))*rvl(3,2) +  &
                (zfrac(ii) - zfracimage(ii))*rvl(3,3)
        elseif (ndim.eq.2) then
          xci = xfrac(ii)*rvl(1,1) + yfrac(ii)*rvl(1,2) 
          yci = xfrac(ii)*rvl(2,1) + yfrac(ii)*rvl(2,2) 
          zci = zfrac(ii)
        elseif (ndim.eq.1) then
          xci = xfrac(ii)*rvl(1,1) 
          yci = yfrac(ii)
          zci = zfrac(ii)
        endif
        ni = nat(ii) - maxele
        ntp = nftype(ii)
        call label(ni,ntp,lab)
        call labellc(asym,ni)
        asym = adjustl(asym)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f15.9,1x,a4,1x,a6,1x,a2,6x,a2,f7.3)') &
          lab2,xci,yci,zci,wtype1,resid,asym,atsym(ni),qf(ii)
      enddo
      write(iout,'(''end'')')
    endif
  else
!*******************
!  Cluster output  *
!*******************
    wtype1 = 'CORE'
!
!  Cores
!
    ncc = 0
    do i = 1,numat
      if (nat(i).le.maxele) then
        ncc = ncc + 1
        write(resid,'(i6)') natmol(ncc)
        resid = adjustl(resid)
        ni = nat(i)
        ntp = nftype(i)
        call label(ni,ntp,lab)
        call labellc(asym,ni)
        asym = adjustl(asym)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f15.9,1x,a4,1x,a6,1x,a2,6x,a2,f7.3)') &
          lab2,xclat(i),yclat(i),zclat(i),wtype1,resid,asym,atsym(nat(i)),qf(i)
      endif
    enddo
    write(iout,'(''end'')')
!
!  Shells
!
    if (nshell.gt.0.and.loutshell) then
      wtype1 = 'SHEL'
      ns = 0
      do i = 1,nshell
        ns = ns + 1
        write(resid,'(i6)') natmol(ns + ncc)
        resid = adjustl(resid)
        ii = nshptr(i)
        ni = nat(ii) - maxele
        ntp = nftype(ii)
        call label(ni,ntp,lab)
        call labellc(asym,ni)
        asym = adjustl(asym)
        lab2 = lab(1:4)
        write(iout,'(a4,1x,3f15.9,1x,a4,1x,a6,1x,a2,1x,a2,f7.3)') &
          lab2,xclat(ii),yclat(ii),zclat(ii),wtype1,resid,asym,atsym(ni),qf(ii)
      enddo
      write(iout,'(''end'')')
    endif
  endif
  write(iout,'(''end'')')
!
  return
  end
