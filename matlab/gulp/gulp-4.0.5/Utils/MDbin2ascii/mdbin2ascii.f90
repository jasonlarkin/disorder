  program mdbin2ascii
!
!  Converts a binary trajectory file from GULP to ASCII format.
!
!   4/08 Created based on mdwrite
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, April 2008
!
  implicit none
!
!  Local variables
!
  character(len=64)   :: trjasc
  character(len=64)   :: trjbin
  character(len=64)   :: conp
  integer*4           :: i
  integer*4           :: j
  integer*4           :: ndim
  integer*4           :: nstrains
  integer*4           :: numat
  logical             :: lconp
  logical             :: leof
  real*8              :: ekin
  real*8              :: fc
  real*8              :: temperature
  real*8              :: timesofar
  real*8              :: version
  real*8              :: volume
  real*8              :: velc(6)
  real*8              :: xcell(9)
  real*8, allocatable :: velx(:)
  real*8, allocatable :: vely(:)
  real*8, allocatable :: velz(:)
  real*8, allocatable :: xalat(:)
  real*8, allocatable :: yalat(:)
  real*8, allocatable :: zalat(:)
!
!  Get file names
!
  read(5,'(a)') trjbin
  read(5,'(a)') trjasc
!
!  Constant pressure?
!
  read(5,'(a)') conp
  lconp = (index(conp,'t').ne.0.or.index(conp,'T').ne.0)
!
!  Open binary file
!
  open(31,file=trjbin,status='unknown',form='unformatted')
!
!  Open ASCII
!
  open(32,file=trjasc,status='unknown',form='formatted')
!
!  Read header from binary
!
  read(31) version
  read(31) numat, ndim
!
!  Write header to ASCII
!
  write(32,'(f5.2)') version
  write(32,'(i8,1x,i1)') numat, ndim
!
  if (ndim.eq.3) then
    nstrains = 6
  elseif (ndim.eq.2) then
    nstrains = 3
  elseif (ndim.eq.1) then
    nstrains = 1
  endif
!
!  Allocate workspace
!
  allocate(velx(numat))
  allocate(vely(numat))
  allocate(velz(numat))
  allocate(xalat(numat))
  allocate(yalat(numat))
  allocate(zalat(numat))
!
  leof = .false.
  do while (.not.leof)
!
!  Read and write data from MD loop
!
    read(31,end=999,err=999) timesofar,ekin,fc,temperature
    write(32,'(''#  Time/KE/E/T'')')
    write(32,'(4(g25.15,1x))') timesofar,ekin,fc,temperature
!
    read(31,end=999,err=999) (xalat(i),i=1,numat)
    read(31,end=999,err=999) (yalat(i),i=1,numat)
    read(31,end=999,err=999) (zalat(i),i=1,numat)
    write(32,'(''#  Coordinates'')')
    do i = 1,numat
      write(32,'(3(g25.15,1x))') xalat(i),yalat(i),zalat(i)
    enddo
!
    read(31,end=999,err=999) (velx(i),i=1,numat)
    read(31,end=999,err=999) (vely(i),i=1,numat)
    read(31,end=999,err=999) (velz(i),i=1,numat)
    write(32,'(''#  Velocities'')')
    do i = 1,numat
      write(32,'(3(g25.15,1x))') velx(i),vely(i),velz(i)
    enddo
    if (lconp) then
      read(31,end=999,err=999) (xcell(i),i=1,9)
      read(31,end=999,err=999) (velc(i),i=1,nstrains)
      write(32,'(''#  Cell'')')
      do i = 1,3
        write(32,'(3(g25.15,1x))') (xcell(3*(i-1)+j),j=1,3)
      enddo
      write(32,'(''#  Cell strain velocities'')')
      do i = 1,2
        write(32,'(3(g25.15,1x))') (velc(3*(i-1)+j),j=1,3)
      enddo
    endif
  enddo
999 continue
!
!  Deallocate workspace
!
  deallocate(zalat)
  deallocate(yalat)
  deallocate(xalat)
  deallocate(velz)
  deallocate(vely)
  deallocate(velx)
!
!  Close files
!
  close(32)
  close(31)
!
  end
