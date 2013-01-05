  subroutine outxr(iout)
!
!  Write out Oxford Materials XR input files
!
!   5/01 Modified to handle multiple configurations by using multiple file names
!   6/07 lall set to false in calls to setup to avoid potential recursive call issue
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
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
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: iout
!
!  Local variables
!
  character(len=5)   :: lab
  character(len=1)   :: numbers(10)
  character(len=80)  :: xrfile2
  character(len=80)  :: xrfilel
  integer(i4)        :: i
  integer(i4)        :: i0(8)
  integer(i4)        :: iend
  integer(i4)        :: ii
  integer(i4)        :: iii
  integer(i4)        :: inat
  integer(i4)        :: incf
  integer(i4)        :: ind
  integer(i4)        :: isp
  integer(i4)        :: ityp
  integer(i4)        :: itype
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: n
  integer(i4)        :: nc
  integer(i4)        :: ni
  logical            :: lfound
  real(dp)           :: cut2
  real(dp)           :: r
  real(dp)           :: rkcs
  real(dp)           :: xcd
  real(dp)           :: ycd
  real(dp)           :: zcd
  real(dp)           :: xcdi
  real(dp)           :: ycdi
  real(dp)           :: zcdi
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
!
  data numbers/'0','1','2','3','4','5','6','7','8','9'/
!
  do i = 1,8
    i0(i) = 0
  enddo
!
!  If file name has been given then open file
!
  if (xrfile(1:1).ne.' ') then
    open(iout,file=xrfile,status='unknown')
  endif
  cut2 = cuts*cuts
!*****************************
!  Loop over configurations  *
!*****************************
  do nc = 1,ncfg
    ncf = nc
    if (ndimen(ncf).eq.3) then
!***************************
!  Initialisation of file  *
!***************************
      xrfile2 = ' '
      xrfilel = xrfile
!
!  If xr file name has been given then open file
!
      if (xrfilel(1:1).eq.' ') then
        xrfilel = 'gulp.xr'
      endif
      ind = index(xrfilel,'.xr')
      if (ind.eq.0) then
        ind = index(xrfilel,' ')
        xrfilel(ind:ind+3) = '.xr'
      endif
      xrfile2(1:ind-1) = xrfilel(1:ind-1)
      if (ncf.ge.100) then
        xrfile2(ind+4:ind+7) = xrfilel(ind:ind+3)
        xrfile2(ind:ind) = '_'
        incf = ncf
        ii = incf/10
        incf = incf - ii*10
        jj = incf/10
        incf = incf - jj*10
        xrfile2(ind+1:ind+1) = numbers(ii+1)
        xrfile2(ind+2:ind+2) = numbers(jj+1)
        xrfile2(ind+3:ind+3) = numbers(incf+1)
      elseif (ncf.ge.10) then
        xrfile2(ind+3:ind+6) = xrfilel(ind:ind+3)
        xrfile2(ind:ind) = '_'
        incf = ncf
        ii = incf/10
        incf = incf - ii*10
        xrfile2(ind+1:ind+1) = numbers(ii+1)
        xrfile2(ind+2:ind+2) = numbers(incf+1)
      elseif (ncfg.gt.1) then
        xrfile2(ind+2:ind+5) = xrfilel(ind:ind+3)
        xrfile2(ind:ind) = '_'
        xrfile2(ind+1:ind+1) = numbers(ncf+1)
      else
        xrfile2(ind:ind+3) = xrfilel(ind:ind+3)
      endif
      open(iout,file = xrfile2,status='unknown')
      if (ncfg.gt.1) then
        write(ioout,'(''  XR file written for configuration '',i4,'' as '',a30)') ncf,xrfile2(1:30)
      else
        write(ioout,'(''  XR file written as '',a30)') xrfile2(1:30)
      endif
!
      call setup(.false.)
!
!  Transform primitive cell back to original cell
!
      do i = 1,3
        rv(1,i) = rvcfg(1,i,ncf)
        rv(2,i) = rvcfg(2,i,ncf)
        rv(3,i) = rvcfg(3,i,ncf)
      enddo
      call uncell3D(rv,a,b,c,alpha,beta,gamma)
!
!  Unit cell information
!
      write(iout,'(22x,''crysalis'',8x,3f8.4)') a,b,c
      write(iout,'(21x,3f8.3)') alpha,beta,gamma
!
!  Number of atoms and title
!
      if (ntitle.ge.1) then
        write(iout,'(i4,''   1'',1x,a60)') numat,titleword(1)(1:60)
      else
        write(iout,'(i4,''   1'')') numat
      endif
!
!  Bulk line
!
      write(iout,'(5x,''1'',1x,''bulk'')')
!
!  Atom data
!
      if (nshell.gt.0) then
!
!  Shell model case
!
        iii = 0
        do i = 1,numat
          ni = nat(i)
          if (ni.le.maxele) then
            itype = 0
            ityp = nftype(i)
            call label(ni,itype,lab)
            iend = index(lab,' ')
            rkcs = 0.0_dp
            do n = 1,npote
              if (nptype(n).eq.5.or.nptype(n).eq.8) then
                if (abs(nspec1(n)-nspec2(n)).eq.maxele) then
                  if (ni.eq.nspec1(n).and.(ityp.eq.nptyp1(n).or.nptyp1(n).eq.0)) then
                    rkcs = twopot(1,n)
                  elseif (ni.eq.nspec2(n).and.(ityp.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
                    rkcs = twopot(1,n)
                  endif
                endif
              endif
            enddo
!
!  Search for corresponding shell
!
            xcd = xclat(i)
            ycd = yclat(i)
            zcd = zclat(i)
            lfound = .false.
!
!  Loop over shells
!
            j = 0
            do while (j.lt.nshell.and..not.lfound)
              j = j + 1
              isp = nshptr(j)
              xcdi = xclat(isp) - xcd
              ycdi = yclat(isp) - ycd
              zcdi = zclat(isp) - zcd
!
!  Loop over unit cells
!
              ii = 0
              do while (ii.lt.iimax.and..not.lfound)
                ii = ii + 1
                xcrd = xcdi + xvec1cell(ii)
                ycrd = ycdi + yvec1cell(ii)
                zcrd = zcdi + zvec1cell(ii)
                r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
                if (r.le.cut2) then
                  lfound = .true.
                endif
!
!  End of loop over cell vectors
!
              enddo
            enddo
            if (lfound) then
              lab(iend:iend+1) = '_c'
              iii = iii + 1
              write(iout,'(i4,1x,a4,2x,3(f9.5,1x),8i4,1x,f7.3,1x,f12.3)') &
                iii,lab,xcd,ycd,zcd,(i0(j),j = 1,8),qf(i),rkcs
              lab(iend:iend+1) = '_s'
              iii = iii + 1
              write(iout,'(i4,1x,a4,2x,3(f9.5,1x),8i4,1x,f7.3,1x,f12.3)') &
                iii,lab,xclat(isp),yclat(isp),zclat(isp),(i0(j),j = 1,8),qf(isp),rkcs
            else
              iii = iii + 1
              write(iout,'(i4,1x,a4,2x,3(f9.5,1x),8i4,1x,f7.3,1x,f12.3)') &
                iii,lab,xcd,ycd,zcd,(i0(j),j = 1,8),qf(i),rkcs
            endif
          endif
        enddo
      else
!
!  Core only case
!
        rkcs = 0.0_dp
        do i = 1,numat
          inat = nat(i)
          itype = nftype(i)
          call label(inat,itype,lab)
          write(iout,'(i4,1x,a4,2x,3(f9.5,1x),8i4,1x,f7.3,1x,f12.3)') &
            i,lab,xclat(i),yclat(i),zclat(i),(i0(j),j = 1,8),qf(i),rkcs
        enddo
      endif
!
!  Unit cell vectors
!
      do i = 1,3
        write(iout,'(3f12.6)')(rv(j,i),j = 1,3)
      enddo
      do i = 1,3
        write(iout,'(3f12.6)')(rv(j,i),j = 1,3)
      enddo
      close(iout)
    endif
!******************************
!  End of configuration loop  *
!******************************
  enddo
!
  return
  end
