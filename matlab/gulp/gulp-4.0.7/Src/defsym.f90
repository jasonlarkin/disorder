  subroutine defsym
!
!  Find the valid symmetry operators for a defect and reduce
!  the full region 1 and 2a to their asymmetric units.
!  Only uses rotations and reflections about principal axes
!  which coincide with cartesian axes for simplicity and
!  speed.
!
!  dsymop   = array of 48 possible 3 x 3 symmetry operators
!  ndasym   = no. of defect asymmetric unit ions
!  ndeqv    = no. of equivalent defect ions
!  ndrel    = no. of ion to which ion is related by symmetry
!  ndsptr   = pointer to asymmetric unit ions
!
!  10/95 Modification for qm atom pointer added as qm specification
!        needs to be consistent with overall symmetry
!   7/97 Modified so that all atoms in region 2 are symmetrised
!        including those used for potentials and manybody terms
!        if EAM is being used.
!   7/05 Order of deallocations reversed
!   3/09 small replaced by global value smallself from general module
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
!  Julian Gale, NRI, Curtin University, March 2009
!
  use control
  use current
  use defects
  use general,       only : smallself
  use iochannels
  use parallel
  use region2a
  use symmetry
  use sutton
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ii1
  integer(i4)                                  :: ii2
  integer(i4)                                  :: ii3
  integer(i4)                                  :: iii
  integer(i4)                                  :: itt
  integer(i4), dimension(:), allocatable       :: iop
  integer(i4)                                  :: iopptr(48)
  integer(i4)                                  :: j
  integer(i4)                                  :: j1
  integer(i4)                                  :: j2
  integer(i4)                                  :: j3
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: l
  integer(i4)                                  :: m
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: natk
  integer(i4)                                  :: ndsops
  integer(i4)                                  :: ndlasym2a
  integer(i4)                                  :: nlreg2
  integer(i4)                                  :: nop
  integer(i4)                                  :: nop2
  integer(i4)                                  :: nr
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: leqat
  logical                                      :: lfound
  logical                                      :: lqmi
  logical                                      :: lqmk
  logical                                      :: lvop(48)
  real(dp)                                     :: diff
  real(dp)                                     :: r
  real(dp)                                     :: radi
  real(dp)                                     :: radr1
  real(dp)                                     :: rm(3,3)
  real(dp)                                     :: rtt
  real(dp)                                     :: tm(3,3)
  real(dp)                                     :: volr1
  real(dp)                                     :: voluc
  real(dp)                                     :: volume
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
  real(dp)                                     :: xis
  real(dp)                                     :: yis
  real(dp)                                     :: zis
!
!  Allocate local memory
!
  allocate(iop(4*nreg1),stat=status)
  if (status/=0) call outofmemory('defsym','iop')
  allocate(leqat(nreg1),stat=status)
  if (status/=0) call outofmemory('defsym','leqat')
!
  if (.not.ldsym) then
!
!  Initialise quantities for no-symmetry case to be safe
!
    ndasym = nreg1
    do i = 1,nreg1
      ndsptr(i) = i
    enddo
    ndasym2a = nreg2
    ndpasym2a = npreg2
    nlreg2 = max(nreg2,npreg2)
    do i = 1,nlreg2
      ndrel2a(i) = i
    enddo
!
!  Reset idopt to allow for partial occupancy constraints
!
    do i = 1,4*nreg1
      iop(i) = 0
    enddo
    do i = 1,nvar
      iop(idopt(i)) = 1
    enddo
    call dpoccon(iop)
    nvar = 0
    do i = 1,3*nreg1
      if (iop(i).eq.1) then
        nvar = nvar + 1
        idopt(nvar) = i
      endif
    enddo
    if (ldbsm) then
      do i = 1,nreg1
        if (ldefbsmat(i)) then
          nvar = nvar + 1
          idopt(nvar) = 3*nreg1 + i
        endif
      enddo
    endif
    goto 999
  endif
!
!  Find upper limit for region 2 search
!
  if (lsuttonc) then
    nlreg2 = max(nreg2,npreg2)
  else
    nlreg2 = nreg2
  endif
!********************************
!  Generate symmetry operators  *
!********************************
  dsymop(1:3,1:3,1:48) = 0.0_dp
  nop = 0
  do i = 1,3
    do j = 1,8
      dsymop(i,1,nop + j) = 1.0_dp
    enddo
    nop2 = nop
    do j = 1,3
      if (j.ne.i) then
        do k = 1,2
          dsymop(j,2,nop2 + k) = 1.0_dp
        enddo
        do k = 1,3
          if (k.ne.i.and.k.ne.j) then
            dsymop(k,3,nop2 + 1) = 1.0_dp
            dsymop(k,3,nop2 + 2) = -1.0_dp
          endif
        enddo
        nop2 = nop2 + 2
        do k = 1,2
          dsymop(j,2,nop2 + k) = -1.0_dp
        enddo
        do k = 1,3
          if (k.ne.i.and.k.ne.j) then
            dsymop(k,3,nop2 + 1) = 1.0_dp
            dsymop(k,3,nop2 + 2) = -1.0_dp
          endif
        enddo
        nop2 = nop2 + 2
      endif
    enddo
    nop = nop + 8
    do j = 1,8
      dsymop(i,1,nop + j) = -1.0_dp
    enddo
    nop2 = nop
    do j = 1,3
      if (j.ne.i) then
        do k = 1,2
          dsymop(j,2,nop2 + k) = 1.0_dp
        enddo
        do k = 1,3
          if (k.ne.i.and.k.ne.j) then
            dsymop(k,3,nop2 + 1) = 1.0_dp
            dsymop(k,3,nop2 + 2) = -1.0_dp
          endif
        enddo
        nop2 = nop2 + 2
        do k = 1,2
          dsymop(j,2,nop2 + k) = -1.0_dp
        enddo
        do k = 1,3
          if (k.ne.i.and.k.ne.j) then
            dsymop(k,3,nop2 + 1) = 1.0_dp
            dsymop(k,3,nop2 + 2) = -1.0_dp
          endif
        enddo
        nop2 = nop2 + 2
      endif
    enddo
    nop = nop + 8
  enddo
!**********************************
!  Find valid symmetry operators  *
!**********************************
!
!  Checks for valid operator include:
!    (1) Coordinates
!    (2) Atomic number
!    (3) Atom type
!    (4) Radius
!    (5) Flags
!
!  Convert idopt to flags for use here
!
  do i = 1,4*nreg1
    iop(i) = 0
  enddo
  do i = 1,nvar
    iop(idopt(i)) = 1
  enddo
!************************
!  Loop over operators  *
!************************
  voluc = volume(rv)
  radr1 = reg1(ncf)
  volr1 = 4.0_dp*radr1**3
  do ii = 1,48
!
!  Test on region 1
!
    i = 0
    lvop(ii) = .true.
    do while (i.lt.nreg1.and.lvop(ii))
      i = i + 1
      xi = xdefe(i)-xdc
      yi = ydefe(i)-ydc
      zi = zdefe(i)-zdc
      xis = xi*dsymop(1,1,ii) + yi*dsymop(1,2,ii)+zi*dsymop(1,3,ii)
      yis = xi*dsymop(2,1,ii) + yi*dsymop(2,2,ii)+zi*dsymop(2,3,ii)
      zis = xi*dsymop(3,1,ii) + yi*dsymop(3,2,ii)+zi*dsymop(3,3,ii)
      ii1 = iop(3*(i-1) + 1)
      ii2 = iop(3*(i-1) + 2)
      ii3 = iop(3*(i-1) + 3)
      j1  =  nint(ii1*dsymop(1,1,ii)  +  ii2*dsymop(1,2,ii) + ii3*dsymop(1,3,ii))
      j2  =  nint(ii1*dsymop(2,1,ii)  +  ii2*dsymop(2,2,ii) + ii3*dsymop(2,3,ii))
      j3  =  nint(ii1*dsymop(3,1,ii)  +  ii2*dsymop(3,2,ii) + ii3*dsymop(3,3,ii))
      j1  =  abs(j1)
      j2  =  abs(j2)
      j3  =  abs(j3)
      nati = natdefe(i)
      ntypi = ntypdefe(i)
      radi = radefe(i)
      lvop(ii) = .false.
      j = 0
      do while (j.lt.nreg1.and..not.lvop(ii))
        j = j + 1
        natj = natdefe(j)
        ntypj = ntypdefe(j)
        rtt = radefe(j)-radi
        if (nati.eq.natj.and.ntypi.eq.ntypj.and.abs(rtt).lt.1.0d-6) then
          xcd = xdefe(j) - xis - xdc
          ycd = ydefe(j) - yis - ydc
          zcd = zdefe(j) - zis - zdc
          r = xcd*xcd + ycd*ycd + zcd*zcd
          itt = abs(iop(3*(j-1) + 1)-j1)
          itt = abs(iop(3*(j-1) + 2)-j2) + itt
          itt = abs(iop(3*(j-1) + 3)-j3) + itt
          lvop(ii) = (r.lt.smallself.and.itt.eq.0)
        endif
      enddo
    enddo
!
!  Test on region 2a if region 1 is small - criteria used
!  is that volume of region 1 must be greater than that of
!  the unit cell.
!
    if (volr1.lt.voluc) then
      i = 0
      do while (i.lt.nlreg2.and.lvop(ii))
        i = i + 1
        xi = xr2a(i)-xdc
        yi = yr2a(i)-ydc
        zi = zr2a(i)-zdc
        xis = xi*dsymop(1,1,ii) + yi*dsymop(1,2,ii)+zi*dsymop(1,3,ii)
        yis = xi*dsymop(2,1,ii) + yi*dsymop(2,2,ii)+zi*dsymop(2,3,ii)
        zis = xi*dsymop(3,1,ii) + yi*dsymop(3,2,ii)+zi*dsymop(3,3,ii)
        nati = nr2a(i)
        ntypi = ntr2a(i)
        radi = rr2a(i)
        lvop(ii) = .false.
        j = 0
        do while (j.lt.nlreg2.and..not.lvop(ii))
          j = j + 1
          natj = nr2a(j)
          ntypj = ntr2a(j)
          rtt = rr2a(j)-radi
          if (nati.eq.natj.and.ntypi.eq.ntypj.and.abs(rtt).lt.1.0d-6) then
            xcd = xr2a(j) - xis - xdc
            ycd = yr2a(j) - yis - ydc
            zcd = zr2a(j) - zis - zdc
            r = xcd*xcd + ycd*ycd + zcd*zcd
            lvop(ii) = (r.lt.smallself)
          endif
        enddo
      enddo
    endif
  enddo
!**************************************
!  Create pointer to valid operators  *
!**************************************
  ndsops = 0
  do i = 1,48
    if (lvop(i)) then
      ndsops = ndsops + 1
      iopptr(ndsops) = i
    endif
  enddo
!****************************
!  Generate product tables  *
!****************************
  do i = 1,ndsops
    ii = iopptr(i)
    do j = 1,3
      rm(1,j) = dsymop(1,j,ii)
      rm(2,j) = dsymop(2,j,ii)
      rm(3,j) = dsymop(3,j,ii)
    enddo
    do j = 1,ndsops
      jj = iopptr(j)
      diff = 0.0_dp
      do l = 1,3
        do m = 1,3
          diff = diff + abs(rm(l,m)-dsymop(m,l,jj))
        enddo
      enddo
      if (diff.lt.1.0d-6) inverse(ii) = jj
    enddo
    do j = 1,ndsops
      jj = iopptr(j)
      do k  =  1,3
        do l  =  1,3
          tm(l,k)  =  rm(l,1)*dsymop(1,k,jj)  +  rm(l,2)*dsymop(2,k,jj) + rm(l,3)*dsymop(3,k,jj)
        enddo
      enddo
      lfound = .false.
      k = 0
      do while (.not.lfound.and.k.lt.ndsops)
        k = k + 1
        kk = iopptr(k)
        diff = 0.0_dp
        do l = 1,3
          do m = 1,3
            diff = diff + abs(tm(m,l)-dsymop(m,l,kk))
          enddo
        enddo
        if (diff.lt.1.0d-6) then
          iptab(ii,jj) = kk
          lfound = .true.
        endif
      enddo
      if (.not.lfound) then
        if (ioproc) then
          write(ioout,'(/,''  **** Operator is missing ****'',/)')
        endif
        call stopnow('defsym')
      endif
    enddo
  enddo
!*************************
!  Find asymmetric unit  *
!*************************
!*************
!  Region 1  *
!*************
  do i = 1,nreg1
    leqat(i) = .false.
  enddo
  ndasym = 0
  do i = 1,nreg1
    if (.not.leqat(i)) then
!
!  Atom found which is not already equivalent to another atom
!  therefore add it to the defect asymmetric unit
!
      ndasym = ndasym + 1
      ndsptr(ndasym) = i
      xi = xdefe(i) - xdc
      yi = ydefe(i) - ydc
      zi = zdefe(i) - zdc
      nati = natdefe(i)
      ntypi = ntypdefe(i)
      radi = radefe(i)
      lqmi = ldqmatom(i)
      ndeqv(ndasym) = 0
      ii1 = iop(3*(i-1) + 1)
      ii2 = iop(3*(i-1) + 2)
      ii3 = iop(3*(i-1) + 3)
!
!  Try each symmetry operator in turn
!
      do j = 1,ndsops
        jj = iopptr(j)
        xis = xi*dsymop(1,1,jj) + yi*dsymop(1,2,jj) + zi*dsymop(1,3,jj)
        yis = xi*dsymop(2,1,jj) + yi*dsymop(2,2,jj) + zi*dsymop(2,3,jj)
        zis = xi*dsymop(3,1,jj) + yi*dsymop(3,2,jj) + zi*dsymop(3,3,jj)
        j1 = nint(ii1*dsymop(1,1,jj) + ii2*dsymop(1,2,jj) + ii3*dsymop(1,3,jj))
        j2 = nint(ii1*dsymop(2,1,jj) + ii2*dsymop(2,2,jj) + ii3*dsymop(2,3,jj))
        j3 = nint(ii1*dsymop(3,1,jj) + ii2*dsymop(3,2,jj) + ii3*dsymop(3,3,jj))
        j1 = abs(j1)
        j2 = abs(j2)
        j3 = abs(j3)
!
!  Find symmetry related atom
!
        k = i - 1
        lfound = .false.
        do while (k.lt.nreg1.and..not.lfound)
          k = k + 1
          natk = natdefe(k)
          ntypk = ntypdefe(k)
          lqmk = ldqmatom(k)
          rtt = radefe(k) - radi
          if (nati.eq.natk.and.ntypi.eq.ntypk.and.abs(rtt).lt.1.0d-6.and. &
              ((lqmi.and.lqmk).or.(.not.lqmi.and..not.lqmk))) then
            xcd = xdefe(k) - xis - xdc
            ycd = ydefe(k) - yis - ydc
            zcd = zdefe(k) - zis - zdc
            r = xcd*xcd + ycd*ycd + zcd*zcd
            itt = abs(iop(3*(k-1) + 1) - j1)
            itt = abs(iop(3*(k-1) + 2) - j2) + itt
            itt = abs(iop(3*(k-1) + 3) - j3) + itt
            lfound = (r.lt.smallself.and.itt.eq.0)
          endif
        enddo
        if (lfound.and..not.leqat(k)) then
!
!  Atom has been found which has not been previously generated
!
          ndeqv(ndasym) = ndeqv(ndasym) + 1
          leqat(k) = .true.
          ndrel(k) = ndasym
          ndrelop(k) = jj
        endif
      enddo
    endif
  enddo
  deallocate(leqat,stat=status)
  if (status/=0) call deallocate_error('defsym','leqat')
!**************
!  Region 2a  *
!**************
  allocate(leqat(nlreg2),stat=status)
  if (status/=0) call outofmemory('defsym','leqat')
  do i = 1,nlreg2
    leqat(i) = .false.
  enddo
  ndasym2a = 0
  ndpasym2a = 0
  ndlasym2a = 0
  do i = 1,nlreg2
    if (.not.leqat(i)) then
!
!  Atom found which is not already equivalent to another atom
!  therefore add it to the defect asymmetric unit
!
      ndlasym2a = ndlasym2a + 1
      if (i.le.nreg2) ndasym2a = ndasym2a + 1
      if (i.le.npreg2) ndpasym2a = ndpasym2a + 1
      ndsptr2a(ndlasym2a) = i
      xi = xr2a(i) - xdc
      yi = yr2a(i) - ydc
      zi = zr2a(i) - zdc
      nati = nr2a(i)
      ntypi = ntr2a(i)
      radi = rr2a(i)
      ndeqv2a(ndlasym2a) = 0
      ndrelop2a(i) = 1
!
!  Try each symmetry operator in turn
!
      do j = 1,ndsops
        jj = iopptr(j)
        xis = xi*dsymop(1,1,jj) + yi*dsymop(1,2,jj) + zi*dsymop(1,3,jj)
        yis = xi*dsymop(2,1,jj) + yi*dsymop(2,2,jj) + zi*dsymop(2,3,jj)
        zis = xi*dsymop(3,1,jj) + yi*dsymop(3,2,jj) + zi*dsymop(3,3,jj)
!
!  Find symmetry related atom
!
        k = i-1
        lfound = .false.
        do while (k.lt.nlreg2.and..not.lfound)
          k = k + 1
          natk = nr2a(k)
          ntypk = ntr2a(k)
          rtt = rr2a(k) - radi
          if (nati.eq.natk.and.ntypi.eq.ntypk.and.abs(rtt).lt.1.0d-6) then
            xcd = xr2a(k) - xis - xdc
            ycd = yr2a(k) - yis - ydc
            zcd = zr2a(k) - zis - zdc
            r = xcd*xcd + ycd*ycd + zcd*zcd
            lfound = (r.lt.smallself)
          endif
        enddo
        if (lfound.and..not.leqat(k)) then
!
!  Atom has been found which has not been previously generated
!
          ndeqv2a(ndlasym2a) = ndeqv2a(ndlasym2a) + 1
          leqat(k) = .true.
          ndrel2a(k) = ndlasym2a
          ndrelop2a(k) = jj
        endif
      enddo
    endif
  enddo
!
!  Sort region 1 ions so that all symmetry generated
!  images are contiguous
!
  call sort1sym(iop)
!**************************************
!  Symmetrise optimisation variables  *
!**************************************
!
!  Collect flags for asymmetric unit
!
  do i = 1,ndasym
    ii = ndsptr(i)
    iii = 3*(i-1)
    jjj = 3*(ii-1)
    iop(iii+1) = iop(jjj+1)
    iop(iii+2) = iop(jjj+2)
    iop(iii+3) = iop(jjj+3)
  enddo
!
!  Find constraints
!
  call dspecial(iop,iopptr,ndsops)
!
!  Find constraints due to partial occupancy
!
  call dpoccon(iop)
!
!  Reset idopt
!
  nvar = 0
  do i = 1,3*ndasym
    if (iop(i).eq.1) then
      nvar = nvar + 1
      idopt(nvar) = i
    endif
  enddo
  if (ldbsm) then
    do i = 1,ndasym
      nr = ndsptr(i)
      if (ldefbsmat(nr)) then
        nvar = nvar + 1
        idopt(nvar) = 3*ndasym + i
      endif
    enddo
  endif
!**************
!  Exit point *
!**************
999 continue
!
!  Free local memory
!
  deallocate(leqat,stat=status)
  if (status/=0) call deallocate_error('defsym','leqat')
  deallocate(iop,stat=status)
  if (status/=0) call deallocate_error('defsym','iop')
!
  return
  end
