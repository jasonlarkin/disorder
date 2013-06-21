  subroutine setatomnodesbo(numat,nprocs,procid,lspatial)
!
!  Sets up the distribution of atoms over nodes for a 
!  spatial decomposition or otherwise in parallel. Version specifically
!  for bond order potentials.
!
!   9/04 Created from setatomnodes
!  11/06 Modified in handling of npgridxptr setting
!   6/07 Structure of arrays for storing distribution changed to 1-D
!  12/07 Unused variables removed
!   7/09 y/z interchange error in parallel spatial decomposition fixed
!
!  On entry :
!
!  numat         = total number of atoms
!  nprocs        = total number of nodes
!  procid        = local node number
!  lspatial      = if .true. then use a spatial decomposition
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
!  Julian Gale, NRI, Curtin University, July 2009
!
  use datatypes
  use control,   only : keyword
  use iochannels
  use parallel,  only : ioproc
  use spatialbo, only : lbuffercell => lbuffercellbo
  use spatialbo, only : maxatompernode => maxatompernodebo
  use spatialbo, only : maxcellpernode => maxcellpernodebo
  use spatialbo, only : natomcell => natomcellbo
  use spatialbo, only : natomnodeptr => natomnodeptrbo
  use spatialbo, only : natompernode => natompernodebo
  use spatialbo, only : nbufferx => nbufferxbo
  use spatialbo, only : nbuffery => nbufferybo
  use spatialbo, only : nbufferz => nbufferzbo
  use spatialbo, only : ncellpernode => ncellpernodebo
  use spatialbo, only : ncellnodeptr => ncellnodeptrbo
  use spatialbo, only : nspcell => nspcellbo
  use spatialbo, only : nspcellat => nspcellatbo
  use spatialbo, only : nspcellatptr => nspcellatptrbo
  use spatialbo, only : nspcellat1ptr => nspcellat1ptrbo
  use spatialbo, only : nspmax => nspmaxbo
  use spatialbo, only : nspmin => nspminbo
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: nprocs
  integer(i4), intent(in)                      :: numat
  integer(i4), intent(in)                      :: procid
  logical,     intent(in)                      :: lspatial
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: icx
  integer(i4)                                  :: icy
  integer(i4)                                  :: icz
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: maxxy
  integer(i4)                                  :: maxx
  integer(i4)                                  :: n
  integer(i4)                                  :: n2
  integer(i4)                                  :: n3
  integer(i4)                                  :: n5
  integer(i4)                                  :: n235(3)
  integer(i4)                                  :: nn235(3)
  integer(i4)                                  :: natomnow
  integer(i4)                                  :: natomold
  integer(i4)                                  :: nmax
  integer(i4)                                  :: nmin
  integer(i4)                                  :: npgrid(3)
  integer(i4), dimension(:), allocatable       :: npgridxptr
  integer(i4), dimension(:), allocatable       :: npgridyptr
  integer(i4), dimension(:), allocatable       :: npgridzptr
  integer(i4)                                  :: nrem
  integer(i4)                                  :: nspdiff
  integer(i4)                                  :: nspnobuff
  integer(i4)                                  :: nstep
  integer(i4)                                  :: ntotcell
  integer(i4)                                  :: status
  logical                                      :: ldebug
  logical                                      :: lfixx
  logical                                      :: lfixy
  logical                                      :: lnon235
  logical                                      :: lok
  logical                                      :: lokx
  logical                                      :: loky
  logical                                      :: lokz
  real(dp)                                     :: diffxy
  real(dp)                                     :: diffxz
  real(dp)                                     :: diffyz
  real(dp)                                     :: pratio
  real(dp)                                     :: ratiox
  real(dp)                                     :: ratioy
  real(dp)                                     :: ratioz
  real(dp)                                     :: ratioxy
  real(dp)                                     :: ratioxz
  real(dp)                                     :: ratioyz
  real(dp)                                     :: rnow
  real(dp)                                     :: spcelltot
  real(dp)                                     :: targetx
  real(dp)                                     :: targety
  real(dp)                                     :: targetz
  real(dp)                                     :: targetratioxy
  real(dp)                                     :: targetratioxz
  real(dp)                                     :: targetratioyz
!
  if (nprocs.eq.1) then
!**********************
!  Non-parallel case  *
!**********************
    natompernode = numat
    if (natompernode.gt.maxatompernode) then
      maxatompernode = natompernode
      call changemaxatompernodebo
    endif
    do i = 1,natompernode
       natomnodeptr(i) = i
    enddo
    if (lspatial) then
!
!  For spatial algorithm set pointers to cells
!
      ncellpernode = nspcell(1)*nspcell(2)*nspcell(3)
      if (ncellpernode.gt.maxcellpernode) then
        maxcellpernode = ncellpernode
        call changemaxcellpernodebo
      endif
      nspmax(1) = nspcell(1) - nbufferx
      nspmin(1) = nbufferx
      nspmax(2) = nspcell(2) - nbuffery
      nspmin(2) = nbuffery
      nspmax(3) = nspcell(3) - nbufferz
      nspmin(3) = nbufferz
      maxxy = nspcell(1)*nspcell(2)
      maxx  = nspcell(1)
      n = 0
      do iz = 1,nspcell(3)
        lokz = (iz.gt.nbufferz.and.iz.lt.(nspcell(3)-nbufferz+1))
        do iy = 1,nspcell(2)
          loky = (iy.gt.nbuffery.and.iy.lt.(nspcell(2)-nbuffery+1))
          do ix = 1,nspcell(1)
            lokx = (ix.gt.nbufferx.and.ix.lt.(nspcell(1)-nbufferx+1))
            ind = (iz-1)*maxxy + (iy-1)*maxx + ix
            n = n + 1
            ncellnodeptr(n) = ind
            if (lokx.and.loky.and.lokz) then
              lbuffercell(n) = .false.
            else
              lbuffercell(n) = .true.
            endif
          enddo
        enddo
      enddo
    endif
  elseif (.not.lspatial) then
!******************************
!  Non-spatial parallel case  *
!******************************
    nstep = numat/nprocs
    nrem  = numat - nprocs*nstep
    nmax = (procid + 1)*nstep + min(nrem,procid+1)
    nmin = procid*nstep + 1 + min(nrem,procid)
    natompernode = nmax - nmin + 1
    if (natompernode.gt.maxatompernode) then
      maxatompernode = natompernode
      call changemaxatompernodebo
    endif
    do i = 1,natompernode
       natomnodeptr(i) = i + nmin - 1
    enddo
  else
!**************************
!  Spatial parallel case  *
!**************************
!
!  Set debugging flag
!
    ldebug = (ioproc.and.index(keyword,'debu').ne.0)
!
!  Find factors of number of processors (2,3,5)
!
    n2 = 0
    n3 = 0
    n5 = 0
    n = nprocs
    lok = .true.
    do while (lok.and.n.gt.1)
      if (mod(n,2_i4).eq.0) then
        n2 = n2 + 1
        n = n/2
      elseif (mod(n,3_i4).eq.0) then
        n3 = n3 + 1
        n = n/3
      elseif (mod(n,5_i4).eq.0) then
        n5 = n5 + 1
        n = n/5
      else
        lok = .false.
      endif
    enddo
!
!  Set flag to indicate whether there is a factor other than 2,3 or 5
!
    lnon235 = (n.ne.1)
    if (ldebug) then
      write(ioout,'(/,''  Spatial decomposition of processors :'',/)')
      write(ioout,'(''  No. of multiples of 2 = '',i4)') n2
      write(ioout,'(''  No. of multiples of 3 = '',i4)') n3
      write(ioout,'(''  No. of multiples of 5 = '',i4)') n5
      if (lnon235) then
        write(ioout,'(''  Other factor          = '',i4)') n
      endif
    endif
!
!  If non 2,3,5, decide which direction to fix
!
    lfixx = .false.
    lfixy = .false.
    if (lnon235) then
      spcelltot = dble(nspcell(1)-2*nbufferx)*dble(nspcell(2)-2*nbuffery)*dble(nspcell(3)-2*nbufferz)
      pratio = dble(n)/dble(nprocs)
      ratiox = pratio - dble(nspcell(1)-2*nbufferx)/spcelltot 
      ratioy = pratio - dble(nspcell(2)-2*nbuffery)/spcelltot
      ratioz = pratio - dble(nspcell(3)-2*nbufferz)/spcelltot
      ratiox = abs(ratiox)
      ratioy = abs(ratioy)
      ratioz = abs(ratioz)
      if (ratiox.lt.ratioy.and.ratiox.lt.ratioz) then
        lfixx = .true.
        npgrid(1) = n
      elseif (ratioy.lt.ratioz) then
        lfixy = .true.
        npgrid(2) = n
      else
        npgrid(3) = n
      endif
    endif
!
!  Calculate optimal target ratios for x-y and x-z
!
    targetratioxy = dble(nspcell(2)-2*nbuffery)/dble(nspcell(1)-2*nbufferx)
    targetratioxz = dble(nspcell(3)-2*nbufferz)/dble(nspcell(1)-2*nbufferx)
    targetratioyz = dble(nspcell(3)-2*nbufferz)/dble(nspcell(2)-2*nbuffery)
!
!  Group factors in proportion to numbers of cells
!
    npgrid(1:3) = 1
    n235(1) = n5
    n235(2) = n3
    n235(3) = n2
    nn235(1) = 5
    nn235(2) = 3
    nn235(3) = 2
    do ind = 1,3
      ii = n235(ind)
      n = nn235(ind)
      do i = 1,ii
        if (lnon235) then
          if (lfixx) then
            ratioyz = dble(npgrid(3))/dble(npgrid(2))
            diffyz = targetratioyz - ratioyz
            if (diffyz.lt.0.0_dp) then
              npgrid(2) = n*npgrid(2)
            else
              npgrid(3) = n*npgrid(3)
            endif
          elseif (lfixy) then
            ratioxz = dble(npgrid(3))/dble(npgrid(1))
            diffxz = targetratioxz - ratioxz
            if (diffxz.lt.0.0_dp) then
              npgrid(1) = n*npgrid(1)
            else
              npgrid(3) = n*npgrid(3)
            endif
          else
            ratioxy = dble(npgrid(2))/dble(npgrid(1))
            diffxy = targetratioxy - ratioxy
            if (diffxy.lt.0.0_dp) then
              npgrid(1) = n*npgrid(1)
            else
              npgrid(2) = n*npgrid(2)
            endif
          endif
        else
          ratioxy = dble(npgrid(2))/dble(npgrid(1))
          ratioxz = dble(npgrid(3))/dble(npgrid(1))
          diffxy = targetratioxy - ratioxy
          diffxz = targetratioxz - ratioxz
          if (diffxy.lt.0.0_dp.and.diffxz.lt.0.0_dp) then
            npgrid(1) = n*npgrid(1)
          elseif (diffxy.gt.diffxz) then
            npgrid(2) = n*npgrid(2)
          else
            npgrid(3) = n*npgrid(3)
          endif
        endif
      enddo
    enddo
    if (ldebug) then
      write(ioout,'(/,''  Processor grid = '',3(i4,1x))') (npgrid(n),n=1,3)
    endif
!
!  Calculate target number of atoms per node in each direction
!
    targetx = dble(numat)/dble(npgrid(1))
    targety = dble(numat)/dble(npgrid(2))
    targetz = dble(numat)/dble(npgrid(3))
!
!  Allocate local memory
!
    allocate(npgridxptr(0:npgrid(1)),stat=status)
    if (status/=0) call outofmemory('setatomnodesbo','npgridxptr')
    allocate(npgridyptr(0:npgrid(2)),stat=status)
    if (status/=0) call outofmemory('setatomnodesbo','npgridyptr')
    allocate(npgridzptr(0:npgrid(3)),stat=status)
    if (status/=0) call outofmemory('setatomnodesbo','npgridzptr')
!
    npgridxptr(0) = nbufferx
    npgridyptr(0) = nbuffery
    npgridzptr(0) = nbufferz
!
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!
!  Assign cells to nodes based on cumulative numbers of atoms : X
!
    natomnow = 0
    natomold = 0
    npgridxptr(1:npgrid(1)) = 0
    ii = 0
    do ix = nbufferx+1,nspcell(1) - nbufferx
      do iy = nbuffery+1,nspcell(2) - nbuffery
        do iz = nbufferz+1,nspcell(3) - nbufferz
          ind = (iz-1)*maxxy + (iy-1)*maxx + ix
          natomnow = natomnow + nspcellat(ind)
        enddo
      enddo
      rnow = dble(natomnow - natomold)
      if (rnow.ge.targetx.and.ii.lt.npgrid(1)) then
        ii = ii + 1
        npgridxptr(ii) = ix
        natomold = natomnow
      endif
    enddo
    npgridxptr(npgrid(1)) = nspcell(1) - nbufferx
!
!  Assign cells to nodes based on cumulative numbers of atoms : Y
!
    natomnow = 0
    natomold = 0
    npgridyptr(1:npgrid(2)) = 0
    ii = 0
    do iy = nbuffery+1,nspcell(2) - nbuffery
      do iz = nbufferz+1,nspcell(3) - nbufferz
        do ix = nbufferx+1,nspcell(1) - nbufferx
          ind = (iz-1)*maxxy + (iy-1)*maxx + ix
          natomnow = natomnow + nspcellat(ind)
        enddo
      enddo
      rnow = dble(natomnow - natomold)
      if (rnow.ge.targety.and.ii.lt.npgrid(2)) then
        ii = ii + 1
        npgridyptr(ii) = iy
        natomold = natomnow
      endif
    enddo
    npgridyptr(npgrid(2)) = nspcell(2) - nbuffery
!
!  Assign cells to nodes based on cumulative numbers of atoms : Z
!
    natomnow = 0
    natomold = 0
    npgridzptr(0:npgrid(3)) = nbufferz
    ii = 0
    do iz = nbufferz+1,nspcell(3) - nbufferz
      do iy = nbuffery+1,nspcell(2) - nbuffery
        do ix = nbufferx+1,nspcell(1) - nbufferx
          ind = (iz-1)*maxxy + (iy-1)*maxx + ix
          natomnow = natomnow + nspcellat(ind)
        enddo
      enddo
      rnow = dble(natomnow - natomold)
      if (rnow.ge.targetz.and.ii.lt.npgrid(3)) then
        ii = ii + 1
        npgridzptr(ii) = iz
        natomold = natomnow
      endif
    enddo
    npgridzptr(npgrid(3)) = nspcell(3) - nbufferz
!
!  Build lists linking cells to nodes
!
    n = 0
    ncellpernode = 0
    do iz = 1,npgrid(3)
      do iy = 1,npgrid(2)
        do ix = 1,npgrid(1)
          n = n + 1
          if (procid.eq.n-1) then
            nspmax(1) = npgridxptr(ix) 
            nspmin(1) = npgridxptr(ix-1)
            nspmax(2) = npgridyptr(iy)
            nspmin(2) = npgridyptr(iy-1)
            nspmax(3) = npgridzptr(iz)
            nspmin(3) = npgridzptr(iz-1)
!
            nspnobuff = nspcell(1) - 2*nbufferx
            nspdiff = min(nspnobuff-nspmax(1)+nspmin(1),nbufferx)
            if (nspdiff.gt.0) nspmin(1) = nspmin(1) - nspdiff
            nspdiff = min(nspnobuff-nspmax(1)+nspmin(1),nbufferx)
            if (nspdiff.gt.0) nspmax(1) = nspmax(1) + nspdiff
            nspnobuff = nspcell(2) - 2*nbuffery
            nspdiff = min(nspnobuff-nspmax(2)+nspmin(2),nbuffery)
            if (nspdiff.gt.0) nspmin(2) = nspmin(2) - nspdiff
            nspdiff = min(nspnobuff-nspmax(2)+nspmin(2),nbuffery)
            if (nspdiff.gt.0) nspmax(2) = nspmax(2) + nspdiff
            nspnobuff = nspcell(3) - 2*nbufferz
            nspdiff = min(nspnobuff-nspmax(3)+nspmin(3),nbufferz)
            if (nspdiff.gt.0) nspmin(3) = nspmin(3) - nspdiff
            nspdiff = min(nspnobuff-nspmax(3)+nspmin(3),nbufferz)
            if (nspdiff.gt.0) nspmax(3) = nspmax(3) + nspdiff
!
            nspmax(1) = min(nspmax(1),nspcell(1)-1)
            nspmin(1) = max(nspmin(1),1)
            nspmax(2) = min(nspmax(2),nspcell(2)-1)
            nspmin(2) = max(nspmin(2),1)
            nspmax(3) = min(nspmax(3),nspcell(3)-1)
            nspmin(3) = max(nspmin(3),1)
!
            ntotcell = (npgridzptr(iz) - npgridzptr(iz-1) + 2*nbufferz)* &
                       (npgridyptr(iy) - npgridyptr(iy-1) + 2*nbuffery)* &
                       (npgridxptr(ix) - npgridxptr(ix-1) + 2*nbufferx)
            if (ncellpernode+ntotcell.gt.maxcellpernode) then
              maxcellpernode = ncellpernode + ntotcell
              call changemaxcellpernodebo
            endif
!
            do icz = npgridzptr(iz-1)+1-nbufferz,npgridzptr(iz)+nbufferz
              lokz = (icz.gt.npgridzptr(iz-1).and.icz.le.npgridzptr(iz))
              do icy = npgridyptr(iy-1)+1-nbuffery,npgridyptr(iy)+nbuffery
                loky = (icy.gt.npgridyptr(iy-1).and.icy.le.npgridyptr(iy))
                do icx = npgridxptr(ix-1)+1-nbufferx,npgridxptr(ix)+nbufferx
                  lokx = (icx.gt.npgridxptr(ix-1).and.icx.le.npgridxptr(ix))
                  ind = (icz-1)*maxxy + (icy-1)*maxx + icx
                  ncellpernode = ncellpernode + 1
                  ncellnodeptr(ncellpernode) = ind
                  if (lokx.and.loky.and.lokz) then
                    lbuffercell(ncellpernode) = .false.
                  else
                    lbuffercell(ncellpernode) = .true.
                  endif
                enddo
              enddo
            enddo
          endif
        enddo
      enddo
    enddo
    if (index(keyword,'debu').ne.0) then
      write(ioout,'(''  Cells per Processor = '',2(i8,1x))') procid,ncellpernode
    endif
!
!  Build lists linking atoms to nodes
!
    natompernode = 0
    do ii = 1,ncellpernode
      if (.not.lbuffercell(ii)) then
        ind = ncellnodeptr(ii)
        if (natompernode+nspcellat(ind).gt.maxatompernode) then
          maxatompernode = natompernode + nspcellat(ind)
          call changemaxatompernodebo
        endif
        do n = 1,nspcellat(ind)
          natompernode = natompernode + 1
          natomnodeptr(natompernode) = nspcellatptr(nspcellat1ptr(ind)+n)
        enddo
      endif
    enddo
    if (index(keyword,'debu').ne.0) then
      write(ioout,'(''  Atoms per Processor = '',2(i8,1x))') procid,natompernode
    endif
!
!  Free local memory
!
    deallocate(npgridzptr,stat=status)
    if (status/=0) call deallocate_error('setatomnodesbo','npgridzptr')
    deallocate(npgridyptr,stat=status)
    if (status/=0) call deallocate_error('setatomnodesbo','npgridyptr')
    deallocate(npgridxptr,stat=status)
    if (status/=0) call deallocate_error('setatomnodesbo','npgridxptr')
  endif
!
!  Set pointer from atom to cell containing image in non-buffer region
!
  if (lspatial) then
    do iz = nbufferz+1,nspcell(3) - nbufferz
      do iy = nbuffery+1,nspcell(2) - nbuffery
        do ix = nbufferx+1,nspcell(1) - nbufferx
          ind = (iz-1)*maxxy + (iy-1)*maxx + ix
          do n = 1,nspcellat(ind)
            natomcell(nspcellatptr(nspcellat1ptr(ind)+n)) = ind
          enddo
        enddo
      enddo
    enddo
  endif
!
  return
  end
