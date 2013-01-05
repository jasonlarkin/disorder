  subroutine defout(ifail,fc,gc,xdstore,ydstore,zdstore)
!
!  Output final point of optimisation for defect
!
!   2/01 Correct handling of a gradient calculation introduced
!   6/09 Modules removed that are unused
!  12/10 Hide shells option added
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, December 2010
!
  use control
  use current
  use defects
  use element, only : maxele
  use iochannels
  use optimisation
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)                  :: ifail
  real(dp)                     :: fc
  real(dp)                     :: gc(*)
  real(dp)                     :: xdstore(*)
  real(dp)                     :: ydstore(*)
  real(dp)                     :: zdstore(*)
!
!  Local variables
!
  character(len = 2)           :: cstype
  character(len = 5)           :: lab
  integer(i4)                  :: i
  integer(i4)                  :: ii
  integer(i4)                  :: inat
  integer(i4)                  :: ind
  integer(i4)                  :: itype
  integer(i4)                  :: j
  integer(i4)                  :: nloop
  real(dp)                     :: pdiff(3)
  real(dp)                     :: sdiff(3)
  real(dp)                     :: rd
  real(dp)                     :: xci
  real(dp)                     :: yci
  real(dp)                     :: zci
  real(dp)                     :: xd
  real(dp)                     :: yd
  real(dp)                     :: zd
  real(dp)                     :: xsi
  real(dp)                     :: ysi
  real(dp)                     :: zsi
!
!  Check gradient norm
!
  gnorm = 0.0_dp
  do i = 1,nvar
    gnorm = gnorm + gc(i)*gc(i)
  enddo
  if (nvar.gt.0) gnorm = sqrt(gnorm)/nvar
!
!  Has the calculation been successful?
!
  if (ioproc) write(ioout,'(/)')
  if (ifail.eq.3.and.gnorm.lt.0.001_dp) ifail = 0
  if (ifail.lt.0) then
    if (ioproc) then
      write(ioout,'(''  **** CPU limit has been exceeded - restart optimisation ****'',/)')
    endif
  elseif (ifail.eq.1) then
    if (ioproc) then
      write(ioout,'(''  **** Too many failed attempts to optimise ****'')')
    endif
  elseif (ifail.eq.2) then
    if (ioproc) then
      write(ioout,'(''  **** Maximum number of function calls has been reached ****'',/)')
    endif
  elseif (ifail.eq.3) then
    if (ioproc) then
      write(ioout,'(''  **** Conditions for a minimum have not been satisfied. However ****'')')
      write(ioout,'(''  **** no lower point can be found - treat results with caution  ****'')')
      write(ioout,'(''  **** unless gradient norm is small (less than 0.1)             ****'',/)')
    endif
  elseif (ifail.eq.5) then
    return
  elseif (ifail.eq.0) then
    if (ioproc) then
      write(ioout,'(''  **** Optimisation achieved ****'',/)')
    endif
  elseif (ifail.ne.4) then
    if (ioproc) then
      write(ioout,'(''  **** Unexpected termination of optimisation ****'',/)')
    endif
  endif
!
!  Output final energy breakdown
!
  if (ioproc.and.ifail.ne.4) then
!
    write(ioout,'(/,''  Final defect energy  =  '',f16.8)') fc
    write(ioout,'(''  Final defect Gnorm   =  '',f16.8)') gnorm
!
    call outdener
!
!  Output final geometry and derivatives
!
    write(ioout,'(/,''  Final coordinates of region 1 :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''   No.  Atomic        x           y           z          Radius      Charge'')')
    write(ioout,'(''        Label       (Angs)      (Angs)      (Angs)       (Angs)       (e) '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nreg1
      inat = natdefe(i)
      itype = ntypdefe(i)
!
!  Hide shells?
!
      if (inat.le.maxele.or..not.lhideshells) then
        call label(inat,itype,lab)
        if (ldefbsmat(i)) then
          cstype = 'bc'
          if (inat.gt.maxele) cstype = 'bs'
        else
          cstype = 'c '
          if (inat.gt.maxele) cstype = 's '
        endif
        write(ioout,'(2x,i4,2x,a5,1x,a2,6f12.6)')i,lab,cstype,xdefe(i),ydefe(i),zdefe(i),radefe(i),qdefe(i)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if ((abs(xdc)+abs(ydc)+abs(zdc)).gt.1.0d-4.and..not.ldsym) then
      write(ioout,'(/,''  Final coordinates of region 1 relative to defect centre :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''   No.  Atomic        x           y           z          Charge'')')
      write(ioout,'(''        Label       (Angs)      (Angs)      (Angs)         (e) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nreg1
        inat = natdefe(i)
        itype = ntypdefe(i)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (ldefbsmat(i)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          write(ioout,'(2x,i4,2x,a5,1x,a2,5f12.6)')i,lab,cstype,(xdefe(i)-xdc),(ydefe(i)-ydc),(zdefe(i)-zdc),qdefe(i)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    elseif (ldsym) then
      write(ioout,'(/,''  Final coordinates of symmetry reduced region 1 (relative to defect centre):'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''   No.  Atomic        x           y           z          Charge'')')
      write(ioout,'(''        Label       (Angs)      (Angs)      (Angs)         (e) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,ndasym
        ii = ndsptr(i)
        inat = natdefe(ii)
        itype = ntypdefe(ii)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (ldefbsmat(ii)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          write(ioout,'(2x,i4,2x,a5,1x,a2,5f12.6)') i,lab,cstype, &
            (xdefe(ii)-xdc),(ydefe(ii)-ydc),(zdefe(ii)-zdc),qdefe(ii)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''  Final derivatives for region 1 :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''   No.  Atomic          x             y             z            Radius'')')
    write(ioout,'(''        Label       (eV/Angs)     (eV/Angs)     (eV/Angs)       (eV/Angs)'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if (ldsym) then
      do i = 1,ndasym
        ii = ndsptr(i)
        inat = natdefe(ii)
        itype = ntypdefe(ii)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (ldefbsmat(ii)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          xd = 0.0_dp
          yd = 0.0_dp
          zd = 0.0_dp
          rd = 0.0_dp
          ind = 3*i-2
          do j = 1,nvar
            if (idopt(j).eq.ind) xd = gc(j)
            if (idopt(j).eq.ind+1) yd = gc(j)
            if (idopt(j).eq.ind+2) zd = gc(j)
            if (idopt(j).eq.3*ndasym+i) rd = gc(j)
          enddo
          write(ioout,'(2x,i4,2x,a5,1x,a2,4f14.6)') i,lab,cstype,xd,yd,zd,rd
        endif
      enddo
    else
      do i = 1,nreg1
        inat = natdefe(i)
        itype = ntypdefe(i)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (ldefbsmat(i)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          xd = 0.0_dp
          yd = 0.0_dp
          zd = 0.0_dp
          rd = 0.0_dp
          ind = 3*i - 2
          do j = 1,nvar
            if (idopt(j).eq.ind) xd = gc(j)
            if (idopt(j).eq.ind+1) yd = gc(j)
            if (idopt(j).eq.ind+2) zd = gc(j)
            if (idopt(j).eq.3*nreg1+i) rd = gc(j)
          enddo
          write(ioout,'(2x,i4,2x,a5,1x,a2,4f14.6)') i,lab,cstype,xd,yd,zd,rd
        endif
      enddo
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
!
!  Option to compare initial and final structures
!
    if (lcomp) then
      write(ioout,'(''  Comparison of initial and final region 1 : '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Parameter   Initial value   Final value   Difference    Units      Percent'')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      if (ldsym) then
        nloop = ndasym
      else
        nloop = nreg1
      endif
      do i = 1,nloop
        if (ldsym) then
          ii = ndsptr(i)
          inat = natdefe(ii)
        else
          ii = i
          inat = natdefe(i)
        endif
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          xci = xdefe(ii)
          yci = ydefe(ii)
          zci = zdefe(ii)
          xsi = xdstore(i)
          ysi = ydstore(i)
          zsi = zdstore(i)
          sdiff(1) = xci - xsi
          sdiff(2) = yci - ysi
          sdiff(3) = zci - zsi
          if (abs(xsi).gt.0.0_dp) then
            pdiff(1) = 100.0_dp*sdiff(1)/xsi
          else
            pdiff(1) = 0.0_dp
          endif
          if (abs(ysi).gt.0.0_dp) then
            pdiff(2) = 100.0_dp*sdiff(2)/ysi
          else
            pdiff(2) = 0.0_dp
          endif
          if (abs(zsi).gt.0.0_dp) then
            pdiff(3) = 100.0_dp*sdiff(3)/zsi
          else
            pdiff(3) = 0.0_dp
          endif
          write(ioout,'(3x,i4,1x,''x'',6x,f12.6,2x,f12.6,3x,f10.6,4x,''Cartesian '',f7.2)') &
            i,xdstore(i),xci,sdiff(1),pdiff(1)
          write(ioout,'(3x,i4,1x,''y'',6x,f12.6,2x,f12.6,3x,f10.6,4x,''Cartesian '',f7.2)') &
            i,ydstore(i),yci,sdiff(2),pdiff(2)
          write(ioout,'(3x,i4,1x,''z'',6x,f12.6,2x,f12.6,3x,f10.6,4x,''Cartesian '',f7.2)') &
            i,zdstore(i),zci,sdiff(3),pdiff(3)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
  return
  end
