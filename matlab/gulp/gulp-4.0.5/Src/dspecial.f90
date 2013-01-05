  subroutine dspecial(iop,iopptr,ndsops)
!
!  Identify special positions for region 1 ions.
!
!  lopx,lopy and lopz indicate whether iop was 1 on entry
!
!   3/95 Bug fixed in flag setting - lopx,lopy and lopz introduced
!   6/95 Automatic generation of constraints for related partial
!        occupancy sites added
!   1/99 Value of shift used for constraint testing has been increased
!        to 0.001 from 0.0001 to avoid problems where shift was too
!        close to coordinate precision.
!  11/06 Calls to ftow replaced with itow
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006.
!
  use control
  use current
  use defects
  use general,    only : nwarn
  use iochannels
  use parallel
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)      :: iop(*)
  integer(i4)      :: iopptr(*)
  integer(i4)      :: ndsops
!
!  Local variables
!
  character(len=6) :: atomno
  integer(i4)      :: i
  integer(i4)      :: ii
  integer(i4)      :: indi
  integer(i4)      :: indx
  integer(i4)      :: indy
  integer(i4)      :: indz
  integer(i4)      :: j
  integer(i4)      :: jj
  integer(i4)      :: k
  integer(i4)      :: mv
  integer(i4)      :: na
  integer(i4)      :: ncv
  integer(i4)      :: neqvi
  integer(i4)      :: nfk
  integer(i4)      :: ngen
  integer(i4)      :: nposs
  integer(i4)      :: nvk
  integer(i4)      :: nvk1
  integer(i4)      :: nvk2
  integer(i4)      :: nxy1
  integer(i4)      :: nxz1
  integer(i4)      :: nyz1
  logical          :: lfound
  logical          :: lfoundxy
  logical          :: lfoundyz
  logical          :: lfoundxz
  logical          :: lgeneral
  logical          :: lopx
  logical          :: lopy
  logical          :: lopz
  logical          :: lxposs
  logical          :: lyposs
  logical          :: lzposs
  real(dp)         :: coeff
  real(dp)         :: cputime
  real(dp)         :: diff
  real(dp)         :: rxp
  real(dp)         :: ryp
  real(dp)         :: rzp
  real(dp)         :: thresh
  real(dp)         :: time1
  real(dp)         :: time2
  real(dp)         :: vx
  real(dp)         :: vy
  real(dp)         :: vz
  real(dp)         :: x(3)
  real(dp)         :: xx(3)
  real(dp)         :: xorig
  real(dp)         :: yorig
  real(dp)         :: zorig
  real(dp)         :: xposs(10)
  real(dp)         :: yposs(10)
  real(dp)         :: zposs(10)
  real(dp)         :: xstor(48)
  real(dp)         :: ystor(48)
  real(dp)         :: zstor(48)
!
  time1 = cputime()
  thresh = 1.0d-4
  ncfst = 0
!********************************************
!  Setup combination shifts of coordinates  *
!********************************************
  nposs = 10
  xposs(1) = 0.001_dp
  yposs(1) = 0.001_dp
  zposs(1) = 0.001_dp
  xposs(2) = 0.001_dp
  yposs(2) = 0.001_dp
  zposs(2) = - 0.001_dp
  xposs(3) = 0.001_dp
  yposs(3) = - 0.001_dp
  zposs(3) = 0.001_dp
  xposs(4) = - 0.001_dp
  yposs(4) = 0.001_dp
  zposs(4) = 0.001_dp
  xposs(5) = 0.001_dp
  yposs(5) = 0.001_dp
  zposs(5) = 0.0_dp
  xposs(6) = 0.001_dp
  yposs(6) = - 0.001_dp
  zposs(6) = 0.0_dp
  xposs(7) = 0.001_dp
  yposs(7) = 0.0_dp
  zposs(7) = 0.001_dp
  xposs(8) = 0.001_dp
  yposs(8) = 0.0_dp
  zposs(8) = - 0.001_dp
  xposs(9) = 0.0_dp
  yposs(9) = 0.001_dp
  zposs(9) = 0.001_dp
  xposs(10) = 0.0_dp
  yposs(10) = 0.001_dp
  zposs(10) = - 0.001_dp
!**************************************
!  Test for each atom and coordinate  *
!**************************************
  do na = 1,ndasym
    i = ndsptr(na)
    xorig = xdefe(i) - xdc
    yorig = ydefe(i) - ydc
    zorig = zdefe(i) - zdc
    lgeneral = .false.
    indi = 3*(na - 1)
    lopx = (iop(indi+1).eq.1)
    lopy = (iop(indi+2).eq.1)
    lopz = (iop(indi+3).eq.1)
    if (.not.lopx.and..not.lopy.and..not.lopz) goto 5
    if (.not.lgeneral) then
      neqvi = ndeqv(na)
!***************************************
!  First pass - individual x, y and z  *
!***************************************
      do 10 ii = 1,3
        indi = indi + 1
        xx(1) = xorig
        xx(2) = yorig
        xx(3) = zorig
        xx(ii) = xx(ii) + 0.0001_dp
!
!  Loop over symmetry operators
!
        ngen = 0
        do 20 mv = 1,ndsops
          jj = iopptr(mv)
!
!  Roto-translation
!
          x(1) = xx(1)*dsymop(1,1,jj) + xx(2)*dsymop(1,2,jj)+ xx(3)*dsymop(1,3,jj)
          x(2) = xx(1)*dsymop(2,1,jj) + xx(2)*dsymop(2,2,jj)+ xx(3)*dsymop(2,3,jj)
          x(3) = xx(1)*dsymop(3,1,jj) + xx(2)*dsymop(3,2,jj)+ xx(3)*dsymop(3,3,jj)
!
!  Compare atom with previously generated equivalent
!
          do j = 1,ngen
            vx = xstor(j) - x(1)
            vy = ystor(j) - x(2)
            vz = zstor(j) - x(3)
            if ((abs(vx)+abs(vy)+abs(vz)).lt.thresh) goto 20
          enddo
!
!  Atom is not equivalent to any previous atom.
!
          ngen = ngen + 1
          if (ngen.gt.neqvi) then
            iop(indi) = 0
            goto 10
          endif
          xstor(ngen) = x(1)
          ystor(ngen) = x(2)
          zstor(ngen) = x(3)
20       continue
10     continue
!*********************************************
!  Second pass - combinations of x, y and z  *
!*********************************************
      indx = indi - 2
      indy = indi - 1
      indz = indi
      do 30 ii = 1,nposs
        xx(1) = xorig + xposs(ii)
        xx(2) = yorig + yposs(ii)
        xx(3) = zorig + zposs(ii)
!
!  Only test combinations where shifted coordinates haven't
!  already been flagged for optimisation
!
        lxposs = (abs(xposs(ii)).gt.1.0d-6)
        lyposs = (abs(yposs(ii)).gt.1.0d-6)
        lzposs = (abs(zposs(ii)).gt.1.0d-6)
        if (iop(indx).eq.1.and.lxposs) goto 30
        if (iop(indy).eq.1.and.lyposs) goto 30
        if (iop(indz).eq.1.and.lzposs) goto 30
!****************************
!  First symmetry operator  *
!****************************
        ngen = 0
        do 40 mv = 1,ndsops
          jj = iopptr(mv)
          x(1) = xx(1)*dsymop(1,1,jj) + xx(2)*dsymop(1,2,jj) + xx(3)*dsymop(1,3,jj)
          x(2) = xx(1)*dsymop(2,1,jj) + xx(2)*dsymop(2,2,jj) + xx(3)*dsymop(2,3,jj)
          x(3) = xx(1)*dsymop(3,1,jj) + xx(2)*dsymop(3,2,jj) + xx(3)*dsymop(3,3,jj)
!
!  Compare atom with previously generated equivalent
!
          do j = 1,ngen
            vx = xstor(j) - x(1)
            vy = ystor(j) - x(2)
            vz = zstor(j) - x(3)
            if ((abs(vx)+abs(vy)+abs(vz)).lt.thresh) goto 40
          enddo
!
!  Atom is not equivalent to any previous atom.
!
          ngen = ngen + 1
          if (ngen.gt.neqvi) goto 30
          xstor(ngen) = x(1)
          ystor(ngen) = x(2)
          zstor(ngen) = x(3)
40       continue
!**************************************************
!  Valid combination - set flags and constraints  *
!**************************************************
        if (lxposs.and.lyposs.and.lzposs) then
!**********************
!  x y z combination  *
!**********************
          rxp = 1.0_dp/xposs(ii)
          ryp = 1.0_dp/yposs(ii)
          rzp = 1.0_dp/zposs(ii)
!
!  Check to see if any constraints already exist for these variables
!
          lfound = .false.
          lfoundxy = .false.
          lfoundyz = .false.
          lfoundxz = .false.
          do k = 1,ndcon
            nvk = ncdvar(ncfst+k)
            nfk = ncdfix(ncfst+k)
            if (indx.eq.nvk.and.indy.eq.nfk) then
              lfoundxy = .true.
              nxy1 = k
            elseif (indy.eq.nvk.and.indx.eq.nfk) then
              lfoundxy = .true.
              nxy1 = k
            elseif (indy.eq.nvk.and.indz.eq.nfk) then
              lfoundyz = .true.
              nyz1 = k
            elseif (indz.eq.nvk.and.indy.eq.nfk) then
              lfoundyz = .true.
              nyz1 = k
            elseif (indx.eq.nvk.and.indz.eq.nfk) then
              lfoundxz = .true.
              nxz1 = k
            elseif (indz.eq.nvk.and.indx.eq.nfk) then
              lfoundxz = .true.
              nxz1 = k
            endif
          enddo
          if (lfoundxy.and.lfoundyz.and.lfoundxz) then
!
!  All pairs constrained
!
            lfound = .true.
            if (ioproc) then
              write(ioout,'(''  **** Advice - redundant constraint supplied for atom '',i4,'' ****'')')na
            endif
!
!  If constraints have been found for both pairs
!
          elseif (lfoundxy.and.lfoundxz) then
            nvk1 = ncdvar(nxy1)
            nvk2 = ncdvar(nxz1)
            if (nvk1.ne.nvk2) then
              call itow(atomno,na,6_i4)
              call outerror('Badly defined constraints for atom '//atomno,0_i4)
              call stopnow('dspecial')
            endif
            lfound = .true.
          elseif (lfoundxy.and.lfoundyz) then
            nvk1 = ncdvar(nxy1)
            nvk2 = ncdvar(nyz1)
            if (nvk1.ne.nvk2) then
              call itow(atomno,na,6_i4)
              call outerror('Badly defined constraints for atom '//atomno,0_i4)
              call stopnow('dspecial')
            endif
            lfound = .true.
          elseif (lfoundxz.and.lfoundyz) then
            nvk1 = ncdvar(nxz1)
            nvk2 = ncdvar(nyz1)
            if (nvk1.ne.nvk2) then
              call itow(atomno,na,6_i4)
              call outerror('Badly defined constraints for atom '//atomno,0_i4)
              call stopnow('dspecial')
            endif
            lfound = .true.
!
!  If constraints have been found for one pair
!
          elseif (lfoundxy) then
            ncv = ncdvar(nxy1)
            if (ncv.eq.indx) then
              iop(indx) = 1
              ndcon = ndcon + 1
              if (ndcon.ge.maxdcon) then
                maxdcon  =  ndcon + 10
                call changemaxdcon
              endif
              ncdvar(ncfst+ndcon) = indx
              ncdfix(ncfst+ndcon) = indz
              dconco(ncfst+ndcon) = zposs(ii)*rxp
              lfound = .true.
            else
              iop(indy) = 1
              ndcon = ndcon + 1
              if (ndcon.ge.maxdcon) then
                maxdcon  =  ndcon + 10
                call changemaxdcon
              endif
              ncdvar(ncfst+ndcon) = indy
              ncdfix(ncfst+ndcon) = indz
              dconco(ncfst+ndcon) = zposs(ii)*ryp
              lfound = .true.
            endif
          elseif (lfoundyz) then
            ncv = ncdvar(nyz1)
            if (ncv.eq.indy) then
              iop(indy) = 1
              ndcon = ndcon + 1
              if (ndcon.ge.maxdcon) then
                maxdcon  =  ndcon + 10
                call changemaxdcon
              endif
              ncdvar(ncfst+ndcon) = indy
              ncdfix(ncfst+ndcon) = indx
              dconco(ncfst+ndcon) = xposs(ii)*ryp
              lfound = .true.
            else
              iop(indz) = 1
              ndcon = ndcon + 1
              if (ndcon.ge.maxdcon) then
                maxdcon  =  ndcon + 10
                call changemaxdcon
              endif
              ncdvar(ncfst+ndcon) = indz
              ncdfix(ncfst+ndcon) = indx
              dconco(ncfst+ndcon) = xposs(ii)*rzp
              lfound = .true.
            endif
          elseif (lfoundxz) then
            ncv = ncdvar(nxz1)
            if (ncv.eq.indx) then
              iop(indx) = 1
              ndcon = ndcon + 1
              if (ndcon.ge.maxdcon) then
                maxdcon  =  ndcon + 10
                call changemaxdcon
              endif
              ncdvar(ncfst+ndcon) = indx
              ncdfix(ncfst+ndcon) = indy
              dconco(ncfst+ndcon) = yposs(ii)*rxp
              lfound = .true.
            else
              iop(indz) = 1
              ndcon = ndcon + 1
              if (ndcon.ge.maxdcon) then
                maxdcon  =  ndcon + 10
                call changemaxdcon
              endif
              ncdvar(ncfst+ndcon) = indz
              ncdfix(ncfst+ndcon) = indy
              dconco(ncfst+ndcon) = yposs(ii)*rzp
              lfound = .true.
            endif
          endif
!
!  No constraints set already
!
          if (.not.lfound.and.lopx.and.lopy.and.lopz) then
            iop(indx) = 1
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            ncdvar(ncfst+ndcon) = indx
            ncdfix(ncfst+ndcon) = indy
            dconco(ncfst+ndcon) = yposs(ii)*rxp
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            ncdvar(ncfst+ndcon) = indx
            ncdfix(ncfst+ndcon) = indz
            dconco(ncfst+ndcon) = zposs(ii)*rxp
          endif
        elseif (lxposs.and.lyposs) then
!********************
!  x y combination  *
!********************
          rxp = 1.0_dp/xposs(ii)
          ryp = 1.0_dp/yposs(ii)
!
!  Check to see if any constraints already exist for these variables
!
          lfound = .false.
          do k = 1,ndcon
            nvk = ncdvar(ncfst+k)
            nfk = ncdfix(ncfst+k)
            if (indx.eq.nvk.and.indy.eq.nfk) then
              lfound = .true.
              iop(indx) = 1
              coeff = yposs(ii)*rxp
              diff = abs(coeff-dconco(ncfst+k))
              if (diff.gt.1.0d-4) then
                nwarn = nwarn+1
                if (ioproc) then
                  write(ioout,'(''  **** Warning - coefficient for constraint '',i3,'' is being reset ****'')') k
                endif
              endif
              dconco(ncfst+k) = coeff
            elseif (indy.eq.nvk.and.indx.eq.nfk) then
              lfound = .true.
              iop(indy) = 1
              coeff = xposs(ii)*ryp
              diff = abs(coeff-dconco(ncfst+k))
              if (diff.gt.1.0d-4) then
                nwarn = nwarn + 1
                if (ioproc) then
                  write(ioout,'(''  **** Warning - coefficient for constraint '',i3,'' is being reset ****'')') k
                endif
              endif
              dconco(ncfst+k) = coeff
            endif
          enddo
          if (.not.lfound.and.lopx.and.lopy) then
            iop(indx) = 1
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            ncdvar(ncfst+ndcon) = indx
            ncdfix(ncfst+ndcon) = indy
            dconco(ncfst+ndcon) = yposs(ii)*rxp
          endif
        elseif (lyposs.and.lzposs) then
!********************
!  y z combination  *
!********************
          ryp = 1.0_dp/yposs(ii)
          rzp = 1.0_dp/zposs(ii)
!
!  Check to see if any constraints already exist for these variables
!
          lfound = .false.
          do k = 1,ndcon
            nvk = ncdvar(ncfst+k)
            nfk = ncdfix(ncfst+k)
            if (indy.eq.nvk.and.indz.eq.nfk) then
              lfound = .true.
              iop(indy) = 1
              coeff = zposs(ii)*ryp
              diff = abs(coeff-dconco(ncfst+k))
              if (diff.gt.1.0d-4) then
                nwarn = nwarn+1
                if (ioproc) then
                  write(ioout,'(''  **** Warning - coefficient for constraint '',i3,'' is being reset ****'')')k
                endif
              endif
              dconco(ncfst+k) = coeff
            elseif (indz.eq.nvk.and.indy.eq.nfk) then
              lfound = .true.
              iop(indz) = 1
              coeff = yposs(ii)*rzp
              diff = abs(coeff-dconco(ncfst+k))
              if (diff.gt.1.0d-4) then
                nwarn = nwarn+1
                if (ioproc) then
                  write(ioout,'(''  **** Warning - coefficient for constraint '',i3,'' is being reset ****'')')k
                endif
              endif
              dconco(ncfst+k) = coeff
            endif
          enddo
          if (.not.lfound.and.lopy.and.lopz) then
            iop(indy) = 1
            ndcon = ndcon+1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            ncdvar(ncfst+ndcon) = indy
            ncdfix(ncfst+ndcon) = indz
            dconco(ncfst+ndcon) = zposs(ii)*ryp
          endif
        elseif (lxposs.and.lzposs) then
!********************
!  x z combination  *
!********************
          rxp = 1.0_dp/xposs(ii)
          rzp = 1.0_dp/zposs(ii)
!
!  Check to see if any constraints already exist for these variables
!
          lfound = .false.
          do k = 1,ndcon
            nvk = ncdvar(ncfst+k)
            nfk = ncdfix(ncfst+k)
            if (indx.eq.nvk.and.indz.eq.nfk) then
              lfound = .true.
              iop(indx) = 1
              coeff = zposs(ii)*rxp
              diff = abs(coeff-dconco(ncfst+k))
              if (diff.gt.1.0d-4) then
                nwarn = nwarn + 1
                if (ioproc) then
                  write(ioout,'(''  **** Warning - coefficient for constraint '',i3,'' is being reset ****'')')k
                endif
              endif
              dconco(ncfst+k) = coeff
            elseif (indz.eq.nvk.and.indx.eq.nfk) then
              lfound = .true.
              iop(indz) = 1
              coeff = xposs(ii)*rzp
              diff = abs(coeff-dconco(ncfst+k))
              if (diff.gt.1.0d-4) then
                nwarn = nwarn + 1
                if (ioproc) then
                  write(ioout,'(''  **** Warning - coefficient for constraint '',i3,'' is being reset ****'')')k
                endif
              endif
              dconco(ncfst+k) = coeff
            endif
          enddo
          if (.not.lfound.and.lopx.and.lopz) then
            iop(indx) = 1
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            ncdvar(ncfst+ndcon) = indx
            ncdfix(ncfst+ndcon) = indz
            dconco(ncfst+ndcon) = zposs(ii)*rxp
          endif
        endif
30     continue
    endif
5   continue
!
!  End of loop over atoms
!
  enddo
  time2 = cputime()
  tsym = tsym + time2 - time1
!
  return
  end
