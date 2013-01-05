  program arc2his
!
!  Converts an archive file to a DLPOLY history file format
!  NB - not completely general at present but might be useful!
!
!   8/08 Created
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
!  Julian Gale, NRI, Curtin University, August 2008
!
  implicit none
!
  integer              :: maxat = 100000
!
!  Local variables
!
  character(len=2)     :: atsym
  character(len=2)     :: atsym2
  character(len=4), allocatable :: lab(:)
  character(len=8)     :: lab8
  character(len=80)    :: line
  integer              :: i
  integer              :: j
  integer              :: keytrj
  integer              :: ncstep
  integer              :: ndim
  integer              :: nformat
  integer              :: nline
  integer              :: numat
  logical              :: lfirstframe
  logical              :: leof
  logical              :: lend
  real*8               :: a
  real*8               :: alpha
  real*8               :: b
  real*8               :: beta
  real*8               :: c
  real*8               :: gamma
  real*8               :: rv(3,3)
  real*8               :: timesofar
  real*8               :: timestep
  real*8,  allocatable :: q(:)
  real*8,  allocatable :: x(:)
  real*8,  allocatable :: y(:)
  real*8,  allocatable :: z(:)
!
!  Allocate large atom arrays
!
  allocate(lab(maxat))
  allocate(q(maxat))
  allocate(x(maxat))
  allocate(y(maxat))
  allocate(z(maxat))
  nline = 0
!
!  Read title line for archive file and find out whether this is archive format 2 or 3
!
  read(5,'(a16,i1)') line(1:16),nformat
  nline = nline + 1
!
!  Read PBC line for archive file
!
  read(5,'(a)') line
  nline = nline + 1
  if (index(line(5:6),'ON').ne.0) then
    ndim = 3
  elseif (index(line(5:6),'2D').ne.0) then
    ndim = 2
  elseif (index(line(5:6),'1D').ne.0) then
    ndim = 1
  elseif (index(line(5:6),'OF').ne.0) then
    ndim = 0
  endif
!
!  Write header to history file
!
  write(6,'(''History file written from GULP'',a50)')
  lfirstframe = .true.
  leof = .false.
!
!  Loop over frames
!
  ncstep = 0
  do while (.not.leof)
    ncstep = ncstep + 1
!
!  Read frame of archive file
!
    if (nformat.eq.3) then
      read(5,'(f10.5)',end=999) timesofar
      nline = nline + 1
    else
      read(5,'(a)',end=999) line
      nline = nline + 1
      timesofar = 0.001d0*dble(ncstep)
    endif
    read(5,'(a)') line
    nline = nline + 1
!
!  Read cell line
!
    if (ndim.eq.3) then
      read(5,'(3x,6f10.4)') a,b,c,alpha,beta,gamma
      nline = nline + 1
    elseif (ndim.eq.2) then
      read(5,'(3x,3f10.4)') a,b,alpha
      nline = nline + 1
    elseif (ndim.eq.1) then
      read(5,'(3x,f10.4)') a
      nline = nline + 1
    endif
    lend = .false.
    numat = 0
    do while (.not.lend)
      read(5,'(a)') line
      nline = nline + 1
      if (index(line,'end').eq.1) then
        lend = .true.
        read(5,'(a)') line
        nline = nline + 1
      else
        numat = numat + 1
        if (numat.gt.maxat) then
          write(6,'('' Error - maxat exceeded! '')')
          stop
        endif
        read(line,'(a4,1x,3f15.9,13x,a2,6x,a2,1x,f6.3)')  &
          lab(numat),x(numat),y(numat),z(numat),atsym,atsym2,q(numat)
      endif
    enddo
!
!  First frame needs delayed writing of header since number of atoms is not known in advance
!
    if (lfirstframe) then
      lfirstframe = .false.
      timestep = timesofar
      keytrj = 1
      write(6,'(3i10)') keytrj,ndim,numat
    endif
!
!  History file output
!
    keytrj = 1
    write(6,'(''timestep'',4i10,f12.6)') ncstep,numat,keytrj,ndim,timestep
    if (ndim.eq.3) then
      call cell3D(rv,a,b,c,alpha,beta,gamma)
      do i = 1,3
        write(6,'(3g12.4)')(rv(j,i),j=1,3)
      enddo
    endif
    do i = 1,numat
      lab8 = ' '
      lab8(1:4) = lab(i)
      write(6,'(a8,i10,2f12.6)') lab8,i,1.0d0,q(i)
      write(6,'(1p,3e12.4)') x(i),y(i),z(i)
      write(6,'(1p,3e12.4)') 0.0d0,0.0d0,0.0d0
    enddo
!
!  End of loop over frames
!
  enddo
!
!  Exit point
!
999 continue
!
  end
