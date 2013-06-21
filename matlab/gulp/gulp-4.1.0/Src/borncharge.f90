  subroutine borncharge(lprint)
!
!  Calculates the Born effective charges for a system
!
!   4/02 Created from property
!  11/02 Basic code for calculation of Born charges for EEM/QEq added
!        but needs phase issue to be addressed
!   1/03 Full charge tensor now output
!   2/03 Matrix inversion accelerated through packed storage
!   3/03 Partial occupancy correction added
!   7/03 Handling of symmetry for output added
!   6/05 Style updated for continuations
!   5/07 Shell pointers moved to module
!   6/07 Modified to handle 2-D case where charges are not to be computed
!        for region 2.
!   2/09 Modified to accommodate new version of FoX and gulp_cml
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
!  Julian Gale, NRI, Curtin University, February 2009
!
  use control,        only : leem
  use current
  use derivatives
  use element
  use gulp_cml,       only : lcml
  use iochannels
  use parallel
  use partial
  use properties
  use shell
  use times
  use gulp_cml_props, only : gulp_cml_print_born_charges
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lprint
!
!  Local variables
!
  character(len=5)                             :: lab
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4)                                  :: ii
  integer(i4)                                  :: inat
  integer(i4), dimension(:), allocatable       :: ipivot
  integer(i4)                                  :: itype
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixs
  integer(i4)                                  :: iys
  integer(i4)                                  :: izs
  integer(i4)                                  :: j
  integer(i4)                                  :: jfoc
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxs
  integer(i4)                                  :: jys
  integer(i4)                                  :: jzs
  integer(i4)                                  :: k
  integer(i4)                                  :: kx
  integer(i4)                                  :: ky
  integer(i4)                                  :: kz
  integer(i4)                                  :: msvar
  integer(i4)                                  :: n
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: n3f
  integer(i4)                                  :: numatr1
  integer(i4)                                  :: status
  logical                                      :: lbornloc
  real(dp)                                     :: cputime
  real(dp),    dimension(:), allocatable       :: dpacked
  real(dp)                                     :: t1p
  real(dp)                                     :: t2p
  real(dp),    dimension(:), allocatable       :: qshell
  real(dp),    dimension(:), allocatable       :: wrk
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
!
  t1p = cputime()
  lbornloc = (.not.leem)
!
!  Set numatr1 to be sum of relevant cores and shells
!
  numatr1 = ncorer1 + nshellr1
!
!  Check second derivative memory
!
  n3f = 3*numat
  if (n3f.gt.maxd2u) then
    maxd2u = n3f
    call changemaxd2
  endif
  if (n3f.gt.maxd2) then
    maxd2 = n3f
    call changemaxd2
  endif
!
!  Allocate local memory
!
  allocate(qshell(numatr1),stat=status)
  if (status/=0) call outofmemory('borncharge','qshell')
!***************************
!  Born effective charges  *
!***************************
  if (lbornloc) then
    if (nshell.eq.0) then
!
!  Born charges equal normal charges if there are no shells present
!
      do i = 1,numatr1
        bornq(1,1,i) = qf(i)*occuf(i)
        bornq(2,1,i) = 0.0_dp
        bornq(3,1,i) = 0.0_dp
        bornq(1,2,i) = 0.0_dp
        bornq(2,2,i) = qf(i)*occuf(i)
        bornq(3,2,i) = 0.0_dp
        bornq(1,3,i) = 0.0_dp
        bornq(2,3,i) = 0.0_dp
        bornq(3,3,i) = qf(i)*occuf(i)
      enddo
    else
!
!  Initialise Born charges using core contribution
!
      n = 0
      do i = 1,numatr1
        if (nat(i).le.maxele) then
          n = n + 1
          bornq(1,1,n) = qf(i)*occuf(i)
          bornq(2,1,n) = 0.0_dp
          bornq(3,1,n) = 0.0_dp
          bornq(1,2,n) = 0.0_dp
          bornq(2,2,n) = qf(i)*occuf(i)
          bornq(3,2,n) = 0.0_dp
          bornq(1,3,n) = 0.0_dp
          bornq(2,3,n) = 0.0_dp
          bornq(3,3,n) = qf(i)*occuf(i)
        endif
      enddo
!
!  Collect shell second derivative terms
!
      msvar = 3*nshellr1
      do i = 1,nshellr1
        ni = nshptr(i)
        qshell(i) = qf(ni)*occuf(ni)
        ix = 3*(ni-1) + 1
        iy = ix + 1
        iz = ix + 2
        ixs = 3*(i-1) + 1
        iys = ixs + 1
        izs = ixs + 2
        do j = 1,nshellr1
          nj = nshptr(j)
          jx = 3*(nj-1) + 1
          jy = jx + 1
          jz = jx + 2
          jxs = 3*(j-1) + 1
          jys = jxs + 1
          jzs = jxs + 2
          dervi(jxs,ixs) = derv2(jx,ix)
          dervi(jys,ixs) = derv2(jy,ix)
          dervi(jzs,ixs) = derv2(jz,ix)
          dervi(jxs,iys) = derv2(jx,iy)
          dervi(jys,iys) = derv2(jy,iy)
          dervi(jzs,iys) = derv2(jz,iy)
          dervi(jxs,izs) = derv2(jx,iz)
          dervi(jys,izs) = derv2(jy,iz)
          dervi(jzs,izs) = derv2(jz,iz)
        enddo
      enddo
      if (lpocc) then  
        call compressd1(qshell,0_i4,nsfoc,nshellr1,iocshptr)
        call compressd2(dervi,maxd2,0_i4,nsfoc,0_i4,nshellr1,iocshptr,ibocshptr)
        n = 3*nsfoc
      else  
        n = msvar
      endif
!
!  Invert second derivative matrix
!
      ifail = 0
!
!  Allocate workspace for inversion
!
      allocate(dpacked(n*(n+1)/2),stat=status)
      if (status/=0) call outofmemory('borncharge','dpacked')
      allocate(ipivot(n),stat=status)
      if (status/=0) call outofmemory('borncharge','ipivot')
      allocate(wrk(3*n),stat=status)
      if (status/=0) call outofmemory('borncharge','wrk')
!
!  Transfer data to packed storage
!     
      k = 0                                    
      do i = 1,n
        do j = 1,i
          k = k + 1                            
          dpacked(k) = dervi(j,i)
        enddo    
      enddo  
!     
!  Factorise matrix
!     
      call dsptrf('U',n,dpacked,ipivot,ifail)  
      if (ifail.eq.0) then
!
!  Form inverse
!
        call dsptri('U',n,dpacked,ipivot,wrk,ifail)
!
!  Transfer data back
!
        k = 0
        do i = 1,n
          do j = 1,i
            k = k + 1
            dervi(j,i) = dpacked(k)            
            dervi(i,j) = dpacked(k)
          enddo   
        enddo 
      endif
!           
!  Free workspace
!     
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('borncharge','wrk')
      deallocate(ipivot,stat=status)
      if (status/=0) call deallocate_error('borncharge','ipivot')
      deallocate(dpacked,stat=status)
      if (status/=0) call deallocate_error('borncharge','dpacked')
!
      if (ifail.gt.0) then
        call outwarning('Born charges cannot be calculated - matrix is singular',0_i4)
        goto 999
      endif
!
!  Multiply inverse shell second derivatives by core-shell second derivatives
!
      ix = - 2
      iy = - 1
      iz =   0
      n = 0
      do i = 1,numatr1
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        if (nat(i).le.maxele) then
          n = n + 1
          do j = 1,nshellr1
            nj = nshptr(j)
            jfoc = iocshptr(j)
            jx = 3*(jfoc - 1) + 1
            jy = jx + 1
            jz = jy + 1
            jxs = 3*(nj - 1) + 1
            jys = jxs + 1
            jzs = jxs + 2
            kx = - 2
            ky = - 1
            kz =   0
            do k = 1,nsfoc
              kx = kx + 3
              ky = ky + 3
              kz = kz + 3
              bornq(1,1,n) = bornq(1,1,n) -  &
                qshell(k)*(dervi(kx,jx)*derv2(jxs,ix) + &
                           dervi(kx,jy)*derv2(jys,ix) + &
                           dervi(kx,jz)*derv2(jzs,ix))
              bornq(1,2,n) = bornq(1,2,n) -  &
                qshell(k)*(dervi(ky,jx)*derv2(jxs,ix) + &
                           dervi(ky,jy)*derv2(jys,ix) + &
                           dervi(ky,jz)*derv2(jzs,ix))
              bornq(1,3,n) = bornq(1,3,n) -  &
                qshell(k)*(dervi(kz,jx)*derv2(jxs,ix) + &
                           dervi(kz,jy)*derv2(jys,ix) + &
                           dervi(kz,jz)*derv2(jzs,ix))
              bornq(2,1,n) = bornq(2,1,n) -  &
                qshell(k)*(dervi(kx,jx)*derv2(jxs,iy) + &
                           dervi(kx,jy)*derv2(jys,iy) + &
                           dervi(kx,jz)*derv2(jzs,iy))
              bornq(2,2,n) = bornq(2,2,n) -  &
                qshell(k)*(dervi(ky,jx)*derv2(jxs,iy) + &
                           dervi(ky,jy)*derv2(jys,iy) + &
                           dervi(ky,jz)*derv2(jzs,iy))
              bornq(2,3,n) = bornq(2,3,n) -  &
                qshell(k)*(dervi(kz,jx)*derv2(jxs,iy) + &
                           dervi(kz,jy)*derv2(jys,iy) + &
                           dervi(kz,jz)*derv2(jzs,iy))
              bornq(3,1,n) = bornq(3,1,n) -  &
                qshell(k)*(dervi(kx,jx)*derv2(jxs,iz) + &
                           dervi(kx,jy)*derv2(jys,iz) + &
                           dervi(kx,jz)*derv2(jzs,iz))
              bornq(3,2,n) = bornq(3,2,n) -  &
                qshell(k)*(dervi(ky,jx)*derv2(jxs,iz) + &
                           dervi(ky,jy)*derv2(jys,iz) + &
                           dervi(ky,jz)*derv2(jzs,iz))
              bornq(3,3,n) = bornq(3,3,n) -  &
                qshell(k)*(dervi(kz,jx)*derv2(jxs,iz) + &
                           dervi(kz,jy)*derv2(jys,iz) + &
                           dervi(kz,jz)*derv2(jzs,iz))
            enddo
          enddo
        endif
      enddo
    endif
!**************************************************************
!  Charge equilibration scheme contributions to Born charges  *
!**************************************************************
    if (leem) then
      do i = 1,numatr1
        kx = - 2
        ky = - 1
        kz =   0
        xi = xclat(i)
        yi = yclat(i)
        zi = zclat(i)
        do k = 1,numatr1
          kx = kx + 3
          ky = ky + 3
          kz = kz + 3
          bornq(1,1,k) = bornq(1,1,k) + dqdxyz(kx,i)*xi
          bornq(2,1,k) = bornq(2,1,k) + dqdxyz(kx,i)*yi
          bornq(3,1,k) = bornq(3,1,k) + dqdxyz(kx,i)*zi
          bornq(1,2,k) = bornq(1,2,k) + dqdxyz(ky,i)*xi
          bornq(2,2,k) = bornq(2,2,k) + dqdxyz(ky,i)*yi
          bornq(3,2,k) = bornq(3,2,k) + dqdxyz(ky,i)*zi
          bornq(1,3,k) = bornq(1,3,k) + dqdxyz(kz,i)*xi
          bornq(2,3,k) = bornq(2,3,k) + dqdxyz(kz,i)*yi
          bornq(3,3,k) = bornq(3,3,k) + dqdxyz(kz,i)*zi
        enddo
      enddo
    endif
  endif
!**********************
!  Output properties  *
!**********************
  if (lprint.and.ioproc) then
    if (lbornloc) then
      if (lcml) call gulp_cml_print_born_charges(bornq)
      write(ioout,'(/,''  Born effective charge tensors : '',/)')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom             x           y             z'')')
      write(ioout,'(''-------------------------------------------------------------------------------'')')
      n = 0
      if (ndim.eq.2) then
        do i = 1,numatr1
          inat = nat(i)  
          if (inat.le.maxele) then
            n = n + 1
            itype = nftype(i)
            call label(inat,itype,lab)
            write(ioout,'(i4,1x,a5,1x,''x '',3(2x,f10.4))') n,lab,bornq(1,1,i),bornq(2,1,i),bornq(3,1,i)
            write(ioout,'(11x,''y '',3(2x,f10.4))') bornq(1,2,i),bornq(2,2,i),bornq(3,2,i)
            write(ioout,'(11x,''z '',3(2x,f10.4))') bornq(1,3,i),bornq(2,3,i),bornq(3,3,i)
            write(ioout,'(''-------------------------------------------------------------------------------'')')
          endif
        enddo
      else
        do i = 1,nasym
          ii = nrel2(i)
          inat = nat(ii)  
          if (inat.le.maxele) then
            n = n + 1
            itype = nftype(ii)
            call label(inat,itype,lab)
            write(ioout,'(i4,1x,a5,1x,''x '',3(2x,f10.4))') n,lab,bornq(1,1,ii),bornq(2,1,ii),bornq(3,1,ii)
            write(ioout,'(11x,''y '',3(2x,f10.4))') bornq(1,2,ii),bornq(2,2,ii),bornq(3,2,ii)
            write(ioout,'(11x,''z '',3(2x,f10.4))') bornq(1,3,ii),bornq(2,3,ii),bornq(3,3,ii)
            write(ioout,'(''-------------------------------------------------------------------------------'')')
          endif
        enddo
      endif
      write(ioout,'(/)')
    endif
    call gflush(ioout)
  endif
!***************
!  Exit tasks  *
!***************
999 continue
!
!  Timings
!
  t2p = cputime()
  tprop = t2p - t1p + tprop
!
!  Deallocate memory
!
  deallocate(qshell,stat=status)
  if (status/=0) call deallocate_error('borncharge','qshell')
!
  return
  end
