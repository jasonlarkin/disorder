  subroutine latpot
!
!  Subroutine for evaluating electrostatic potential
!
!   7/95 Trap added for maxone being too small -> change
!        algorithm to use epot call.
!   7/97 Sympot/genpot efg's are wrong so use large system
!        algorithm now for all cases
!  12/97 Sympot/genpot algorithm removed as it is restricted
!        my memory to small numbers of atoms
!   7/98 Calculation of asymmetry parameter added
!   6/00 efg dimensions changed to (6,maxat)
!   6/00 storage of field moved into vx,vy,vz
!   3/01 option to set the zero of potential added
!   8/01 Call to epot0/3 replaced with generic call to epot
!  12/02 Signs put back for efgs in diagonalised form
!   1/03 Check on charges being non-zero added
!   6/03 Call of sitepot added if there are non-atomic positions
!        specified
!   4/04 efg array substituted by v2xyz
!  10/04 Eispack call replaced by lapack
!   9/07 Format of site number output now allows for up to 5 digits
!  12/07 Unused variables removed
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   5/12 Call to reaxFFpot added
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : lbsmat
  use control
  use current
  use element,        only : maxele
  use general,        only : time0
  use gulp_cml,       only : lcml, gulp_cml_print_epot_lattice
  use iochannels
  use kspace
  use parallel
  use potentialxyz
  use symmetry
  use times
  implicit none
!
!  Local variables
!
  character(len=2)                             :: cstype
  character(len=5)                             :: lab
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4)                                  :: j
  integer(i4)                                  :: inat
  integer(i4)                                  :: itype
  integer(i4)                                  :: nr
  integer(i4)                                  :: status
  logical                                      :: lgrad2
  logical                                      :: lzero
  real(dp)                                     :: asymefg
  real(dp)                                     :: cputime
  real(dp)                                     :: cqq
  real(dp)                                     :: defg(3,3)
  real(dp)                                     :: qsum
  real(dp)                                     :: refg1
  real(dp)                                     :: refg2
  real(dp)                                     :: refg3
  real(dp)                                     :: sumv
  real(dp)                                     :: time
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp),  dimension(:),   allocatable       :: v
  real(dp)                                     :: vefg(3)
  real(dp)                                     :: vefg1
  real(dp)                                     :: vefg2
  real(dp)                                     :: vefg3
  real(dp)                                     :: vtmp
  real(dp)                                     :: wl1(9)
  real(dp),                               save :: qconfct = 2.41796781_dp
!
  time1 = cputime()
  lgrad2 = (index(keyword,'efg').ne.0)
  lzero = (index(keyword,'zer').eq.1.or.index(keyword,' zer').ne.0)
!
!  Setup coordinates
!
  qsum = 0.0_dp
  if (lsymopt) then
    do i = 1,nasym
      nr = nrel2(i)
      xalat(i) = xclat(nr)
      yalat(i) = yclat(nr)
      zalat(i) = zclat(nr)
      qsum = qsum + abs(qa(i)*occua(i))
    enddo
  else
    do i = 1,numat
      xalat(i) = xclat(i)
      yalat(i) = yclat(i)
      zalat(i) = zclat(i)
      qsum = qsum + abs(qf(i)*occuf(i))
    enddo
  endif
!
!  Allocate local memory
!
  allocate(v(nasym),stat=status)
  if (status/=0) call outofmemory('latpot','v')
!*****************************************************************
!  New algorithm for large systems where matrix can't be stored  *
!*****************************************************************
  if (lreaxff) then
!
!  Potential from ReaxFF
!
    call reaxFFpot(v)
  elseif (qsum.lt.1.0d-8) then
!
!  If charges are zero then terms will all be zero
!
    do i = 1,nasym
      v(i) = 0.0_dp
      vx(i) = 0.0_dp
      vy(i) = 0.0_dp
      vz(i) = 0.0_dp
      if (lgrad2) then
        v2xyz(1:6,i) = 0.0_dp
      endif
    enddo
  else
    call epot(.true.,nasym,v,xalat,yalat,zalat,.true.,vx,vy,vz,lgrad2,v2xyz,.true.)
  endif
  time2 = cputime()
  tion = tion + time2 - time1
!
!  Option to zero average potential across sites
!
  if (lzero) then
    sumv = 0.0_dp
    do i = 1,nasym
      sumv = sumv + v(i)*dble(neqv(i))
    enddo
    sumv = sumv / dble(numat)
    do i = 1,nasym
      v(i) = v(i) - sumv
    enddo
  endif
!
!  Output potential
!
  if (ioproc) then
    if (lcml) call gulp_cml_print_epot_lattice(v)
    write(ioout,'(/)')
    write(ioout,'(''  Electrostatic potential at atomic positions :'',/)')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    if (lreaxff) then
      write(ioout,'('' Site  Atomic      Potential                                   )'')')
      write(ioout,'('' No.   Label          (V)                                      )'')')
    else
      write(ioout,'('' Site  Atomic      Potential                Derivatives (V/Angs)'')')
      write(ioout,'('' No.   Label          (V)                 x           y           z'')')
    endif
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    do i = 1,nasym
      inat = iatn(i)
      itype = natype(i)
      call label(inat,itype,lab)
      if (lbsmat(nsft+i)) then
        cstype = 'bc'
        if (inat.gt.maxele) cstype = 'bs'
      else
        cstype = 'c '
        if (inat.gt.maxele) cstype = 's '
      endif
      if (lreaxff) then
        write(ioout,'(1x,i5,1x,a5,1x,a2,f13.6)') i,lab,cstype,v(i)
      else
        write(ioout,'(1x,i5,1x,a5,1x,a2,f13.6,7x,3f12.6)') i,lab,cstype,v(i),vx(i),vy(i),vz(i)
      endif
    enddo
    write(ioout,'(''-------------------------------------------------------------------------------'',/)')
!
!  Output electric field gradients
!
    if (index(keyword,'efg').gt.0.and..not.lreaxff) then
      write(ioout,'(/,''  Electric Field Gradients at atomic positions :'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''   Site no.                      EFGs (V/Angs**2)'')')
      write(ioout,'(''                xx        xy        yy        xz        yz        zz'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      do i = 1,nasym
        write(ioout,'(4x,i5,3x,6(f9.4,1x))') i,(v2xyz(j,i),j=1,6)
      enddo
      write(ioout,'(''-------------------------------------------------------------------------------'',/)')
      write(ioout,'(/,''  EFG Tensor properties :'',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''   Site no.    Diagonalised EFGs (V/Angs**2)          eVzz/h        Asymmetry'')')
      write(ioout,'(''                 xx        yy        zz             (MHz/barn)      Parameter'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      do i = 1,nasym
        defg(1,1) = v2xyz(1,i)
        defg(2,1) = v2xyz(2,i)
        defg(3,1) = v2xyz(4,i)
        defg(1,2) = v2xyz(2,i)
        defg(2,2) = v2xyz(3,i)
        defg(3,2) = v2xyz(5,i)
        defg(1,3) = v2xyz(4,i)
        defg(2,3) = v2xyz(5,i)
        defg(3,3) = v2xyz(6,i)
        call dsyev('N','U',3_i4,defg,3_i4,vefg,wl1,9_i4,ifail)
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
        write(ioout,'(4x,i5,4x,3(f9.4,1x),5x,f13.4,3x,f11.4)') i,refg1,refg2,refg3,cqq,asymefg
      enddo
      write(ioout,'(''-------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  Free local memory
!
  deallocate(v,stat=status)
  if (status/=0) call deallocate_error('latpot','v')
!
!  Calculate the potential at any non-atomic sites requested by the user
!
  call sitepot
!
  if (ioproc) then
    time = cputime() - time0
    write(ioout,'(''  Total time to end of lattice properties = '',f12.4,'' s'',/)') time
  endif
!
  return
  end
