  subroutine sitepot
!
!  Subroutine for evaluating electrostatic potential at general sites
!  specified by the user in the input.
!
!   6/03 Created 
!  10/04 Eispack call replaced by lapack
!  12/07 Unused variables removed
!  12/09 For consistency, apply same trick as potgrid of putting cuts to 
!        a small value during this routine
!   7/11 Modified to handle centred cell symmetry
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, July 2011
!
  use control
  use current
  use element,       only : maxele
  use iochannels
  use parallel
  use potentialsites
  use shell,         only : cuts
  use symmetry,      only : rop, w1, vit
  use times
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4)                                  :: ind
  integer(i4)                                  :: j
  integer(i4)                                  :: nsite
  integer(i4)                                  :: status
  logical                                      :: lgrad2
  logical                                      :: lzero
  real(dp)                                     :: asymefg
  real(dp)                                     :: cputime
  real(dp)                                     :: cqq
  real(dp)                                     :: defg(3,3)
  real(dp),  dimension(:,:), allocatable       :: efg
  real(dp)                                     :: refg1
  real(dp)                                     :: refg2
  real(dp)                                     :: refg3
  real(dp)                                     :: savecuts
  real(dp)                                     :: sumv
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp),  dimension(:),   allocatable       :: xsite
  real(dp),  dimension(:),   allocatable       :: ysite
  real(dp),  dimension(:),   allocatable       :: zsite
  real(dp),  dimension(:),   allocatable       :: vsite
  real(dp),  dimension(:),   allocatable       :: vsitex
  real(dp),  dimension(:),   allocatable       :: vsitey
  real(dp),  dimension(:),   allocatable       :: vsitez
  real(dp)                                     :: v(3)
  real(dp)                                     :: vefg(3)
  real(dp)                                     :: vefg1
  real(dp)                                     :: vefg2
  real(dp)                                     :: vefg3
  real(dp)                                     :: vtmp
  real(dp)                                     :: wl1(9)
  real(dp)                                     :: x(3)
  real(dp)                                     :: xx(3)
  real(dp),                               save :: qconfct = 2.41796781_dp
!
  time1 = cputime()
  lgrad2 = (index(keyword,'efg').ne.0)
  lzero = (index(keyword,'zer').eq.1.or.index(keyword,' zer').ne.0)
!
!  Put cuts to small value
!
  savecuts = cuts
  cuts = 0.0001_dp
!
!  Count number of sites
!
  nsite = 0
  do i = 1,npotsites
    if (npotsitecfg(i).eq.ncf) nsite = nsite + 1
  enddo
  if (nsite.gt.0) then
!
!  Allocate memory
!
    allocate(xsite(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','xsite')
    allocate(ysite(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','ysite')
    allocate(zsite(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','zsite')
    allocate(vsite(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','vsite')
    allocate(vsitex(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','vsitex')
    allocate(vsitey(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','vsitey')
    allocate(vsitez(nsite),stat=status)
    if (status/=0) call outofmemory('sitepot','vsitez')
    if (lgrad2) then
      allocate(efg(6,nsite),stat=status)
    else
      allocate(efg(6,1),stat=status)
    endif
    if (status/=0) call outofmemory('sitepot','efg')
!
!  Setup coordinates by converting fractional to Cartesian
!
    ind = 0
    do i = 1,npotsites
!
      if (npotsitecfg(i).eq.ncf) then
        ind = ind + 1
        if (ndim.eq.3) then
!
!  Convert input fractional coordinates for centred cell to those for primitive cell
!
          xx(1) = xpotsite(i)
          xx(2) = ypotsite(i)
          xx(3) = zpotsite(i)
          x(1) = 0.0_dp
          x(2) = 0.0_dp
          x(3) = 0.0_dp
          v(1) = vit(1,1)
          v(2) = vit(2,1)
          v(3) = vit(3,1)
          call GULP_mxmb(rop(1,1,1),1_i4,3_i4,xx,1_i4,1_i4,v,1_i4,1_i4,3_i4,3_i4,1_i4)
          call GULP_mxmb(w1(ncbl,1,1),7_i4,21_i4,v,1_i4,1_i4,x,1_i4,1_i4,3_i4,3_i4,1_i4)
!
          xsite(ind) = x(1)*r1x + x(2)*r2x + x(3)*r3x
          ysite(ind) = x(1)*r1y + x(2)*r2y + x(3)*r3y
          zsite(ind) = x(1)*r1z + x(2)*r2z + x(3)*r3z
        elseif (ndim.eq.2) then
          xsite(ind) = xpotsite(i)*r1x + ypotsite(i)*r2x
          ysite(ind) = xpotsite(i)*r1y + ypotsite(i)*r2y
          zsite(ind) = zpotsite(i)
        elseif (ndim.eq.1) then
          xsite(ind) = xpotsite(i)*r1x
          ysite(ind) = ypotsite(i)
          zsite(ind) = zpotsite(i)
        else
          xsite(ind) = xpotsite(i)
          ysite(ind) = ypotsite(i)
          zsite(ind) = zpotsite(i)
        endif
      endif
    enddo
!*****************************************************************
!  New algorithm for large systems where matrix can't be stored  *
!*****************************************************************
    call epot(.true.,nsite,vsite,xsite,ysite,zsite,.true.,vsitex,vsitey,vsitez,lgrad2,efg,.true.)
    time2 = cputime()
    tion = tion + time2 - time1
!
!  Option to zero average potential across sites
!
    if (lzero) then
      sumv = 0.0_dp
      do i = 1,nsite
        sumv = sumv + vsite(i)
      enddo
      sumv = sumv / dble(nsite)
      do i = 1,nasym
        vsite(i) = vsite(i) - sumv
      enddo
    endif
!
!  Output potential
!
    if (ioproc) then
      write(ioout,'(/)')
      write(ioout,'(''  Electrostatic potential at input sites :'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'('' Site        Coordinates            Potential        Derivatives (V/Angs)'')')
      write(ioout,'('' No.     x        y        z           (V)          x          y          z'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      ind = 0
      do i = 1,npotsites
        if (npotsitecfg(i).eq.ncf) then
          ind = ind + 1
          write(ioout,'(1x,i4,3(1x,f8.4),f13.6,1x,3(1x,f10.4))') i,xpotsite(i),ypotsite(i),zpotsite(i),vsite(ind), &
            vsitex(ind),vsitey(ind),vsitez(ind)
        endif
      enddo
      write(ioout,'(''-------------------------------------------------------------------------------'',/)')
!
!  Output electric field gradients
!
      if (index(keyword,'efg').gt.0) then
        write(ioout,'(/,''  Electric Field Gradients at input sites :'',/)')
        write(ioout,'(''-------------------------------------------------------------------------------'')')
        write(ioout,'(''   Site no.                      EFGs (V/Angs**2)'')')
        write(ioout,'(''                xx        xy        yy        xz        yz        zz'')')
        write(ioout,'(''-------------------------------------------------------------------------------'')')
        ind = 0
        do i = 1,npotsites
          if (npotsitecfg(i).eq.ncf) then
            ind = ind + 1
            write(ioout,'(4x,i4,4x,6(f9.4,1x))') i,(efg(j,ind),j=1,6)
          endif
        enddo
        write(ioout,'(''-------------------------------------------------------------------------------'',/)')
        write(ioout,'(/,''  EFG Tensor properties :'',/)')
        write(ioout,'(''-------------------------------------------------------------------------------'')')
        write(ioout,'(''   Site no.    Diagonalised EFGs (V/Angs**2)          eVzz/h        Asymmetry'')')
        write(ioout,'(''                 xx        yy        zz             (MHz/barn)      Parameter'')')
        write(ioout,'(''-------------------------------------------------------------------------------'')')
        ind = 0
        do i = 1,npotsites
          if (npotsitecfg(i).eq.ncf) then
            ind = ind + 1
            defg(1,1) = efg(1,i)
            defg(2,1) = efg(2,i)
            defg(3,1) = efg(4,i)
            defg(1,2) = efg(2,i)
            defg(2,2) = efg(3,i)
            defg(3,2) = efg(5,i)
            defg(1,3) = efg(4,i)
            defg(2,3) = efg(5,i)
            defg(3,3) = efg(6,i)
!
            call dsyev('N','U',3_i4,defg,3_i4,vefg,wl1,9_i4,ifail)
!
            vefg1 = abs(vefg(1))
            vefg2 = abs(vefg(2))
            vefg3 = abs(vefg(3))
            refg1 = vefg(1)
            refg2 = vefg(2)
            refg3 = vefg(3)
            if (vefg1.gt.vefg2) then
              vtmp = vefg1
              vefg1 = vefg2
              vefg2 = vtmp
              vtmp = refg1
              refg1 = refg2
              refg2 = vtmp
            endif
            if (vefg2.gt.vefg3) then
              vtmp = vefg2
              vefg2 = vefg3
              vefg3 = vtmp
              vtmp = refg2
              refg2 = refg3
              refg3 = vtmp
            endif
            if (vefg1.gt.vefg2) then
              vtmp = vefg1
              vefg1 = vefg2
              vefg2 = vtmp
              vtmp = refg1
              refg1 = refg2
              refg2 = vtmp
            endif
            if (vefg3.ne.0.0_dp) then
              asymefg = (vefg2 - vefg1)/vefg3
            else
              asymefg = 0.0_dp
            endif
            cqq = vefg3*qconfct
            write(ioout,'(4x,i4,5x,3(f9.4,1x),5x,f13.4,3x,f11.4)') i,refg1,refg2,refg3,cqq,asymefg
          endif
        enddo
        write(ioout,'(''-------------------------------------------------------------------------------'',/)')
      endif
    endif
!
!  Free local memory
!
    deallocate(efg,stat=status)
    if (status/=0) call deallocate_error('sitepot','efg')
    deallocate(vsitez,stat=status)
    if (status/=0) call deallocate_error('sitepot','vsitez')
    deallocate(vsitey,stat=status)
    if (status/=0) call deallocate_error('sitepot','vsitey')
    deallocate(vsitex,stat=status)
    if (status/=0) call deallocate_error('sitepot','vsitex')
    deallocate(vsite,stat=status)
    if (status/=0) call deallocate_error('sitepot','vsite')
    deallocate(zsite,stat=status)
    if (status/=0) call deallocate_error('sitepot','zsite')
    deallocate(ysite,stat=status)
    if (status/=0) call deallocate_error('sitepot','ysite')
    deallocate(xsite,stat=status)
    if (status/=0) call deallocate_error('sitepot','xsite')
  endif
!
!  Restore cuts
!
  cuts = savecuts
!
  return
  end
