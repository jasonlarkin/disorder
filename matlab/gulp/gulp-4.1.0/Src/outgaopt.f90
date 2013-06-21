  subroutine outgaopt
!
!  Output final configurations from genetic algorithm optimisation
!
!  11/93 First created
!   6/95 Modified for additive constraints
!  12/07 Parallel error in setting nga fixed
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
!  Copyright Curtin University 2007
!  
!  Julian Gale, NRI, Curtin University, December 2007
!
  use configurations
  use current
  use gaconf
  use genetic
  use iochannels
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4),      dimension(:), allocatable       :: ncount
  character(len=5)                                  :: namecell(6)
  character(len=1)                                  :: namecrd(3)
  integer(i4)                                       :: i
  integer(i4)                                       :: idj
  integer(i4)                                       :: ii
  integer(i4)                                       :: ip
  integer(i4)                                       :: j
  integer(i4)                                       :: jbase
  integer(i4)                                       :: jmax
  integer(i4)                                       :: k
  integer(i4)                                       :: ng
  integer(i4)                                       :: nga
  integer(i4)                                       :: ngroup
  integer(i4)                                       :: nj
  integer(i4)                                       :: status
  logical                                           :: lfound
  real(dp)                                          :: cellp(6,4)
  real(dp)                                          :: diff
  real(dp)                                          :: rvp(3,3)
  real(dp)                                          :: strloc(6)
!
  data namecell/'a    ','b    ','c    ','alpha','beta','gamma'/
  data namecrd/'x','y','z'/
!***************************
!  Decide type of results  *
!***************************
  if (ioproc) then
    write(ioout,'(/)')
    if (ngabest.eq.0) then
      write(ioout,'(''  Final configurations from genetic algoritthm optimisation :'',/)')
      nga = ngacfg
    else
      write(ioout,'(''  Best configurations from genetic algorithm optimisation :'',/)')
      nga = ngabest
    endif
  else
    if (ngabest.eq.0) then
      nga = ngacfg
    else
      nga = ngabest
    endif
  endif
!
!  How many lots of 4?
!
  ngroup = ((nga-1)/4) + 1
  mvar = 3*nasym + 6
!***********************
!  Table of variables  *
!***********************
  do ng = 1,ngroup
    if (ng.eq.ngroup) then
      jmax = nga - 4*(ngroup-1)
    else
      jmax = 4
    endif
    jbase = 4*(ng - 1)
    if (ngabest.gt.0) jbase = jbase + mgacfg
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(25x,4(5x,i3,5x))')(4*(ng-1)+j,j=1,jmax)
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Sum of squares =  '',5x,4(1x,f12.5))') (fconf(jbase+j),j=1,jmax)
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
    do i = 1,jmax
      do j = 1,3
        rv(1,j) = rvcfg(1,j,ncf)
        rv(2,j) = rvcfg(2,j,ncf)
        rv(3,j) = rvcfg(3,j,ncf)
      enddo
      do j = 1,6
        x0(j) = 1.0_dp
      enddo
      do j = 1,nvar
        x0(iopt(j)) = xconf(j,i+jbase)
      enddo
!**********************
!  Apply constraints  *
!**********************
      if (ncon.gt.0) then
        do j = 1,ncon
          x0(ncfix(j)) = 0.0_dp
        enddo
        do j = 1,ncon
          x0(ncfix(j)) = x0(ncvar(j))*conco(j) + conadd(j) + x0(ncfix(j))
        enddo
!
!  Handle additive constraints for fractional coordinates
!  - take nearest pair of images
!
        if (ndimen(ncf).eq.3) then
          mvar = 3*nasym + 6
          allocate(ncount(mvar),stat=status)
          if (status/=0) call outofmemory('outgaopt','ncount')
          do j = 1,mvar
            ncount(j) = 0
          enddo
          do j = 1,ncon
            ii = ncfix(j)
            ncount(ii) = ncount(ii) + 1
          enddo
          do ii = 7,mvar
            if (ncount(ii).ge.2) then
              lfound = .false.
              j = 0
              do while (.not.lfound.and.j.lt.ncon-1)
                j = j + 1
                if (ncfix(j).eq.ii) then
                  k = j
                  do while (.not.lfound.and.k.lt.ncon) 
                    k = k + 1
                    lfound = (ncfix(k).eq.ii)
                  enddo
                endif
              enddo
              if (lfound) then
                diff = abs(x0(ncvar(j)) - x0(ncvar(k)))
                if (diff.gt.0.5) then
                  x0(ii) = x0(ii) + 0.5_dp
                  x0(ii) = mod(x0(ii),1.0_dp)
                endif
              endif
            endif
          enddo
          deallocate(ncount,stat=status)
          if (status/=0) call deallocate_error('outgaopt','ncount')
        endif
      endif
      if (ncell.gt.0) then
!******************
!  Apply strains  *
!******************
        do j = 1,6
          strloc(j) = x0(j) - 1.0_dp
        enddo
        call strain3D(strloc,rv)
!
!  Generate non-primitive cell
!
        do j = 1,3
          rvp(1,j) = rv(1,j)
          rvp(2,j) = rv(2,j)
          rvp(3,j) = rv(3,j)
        enddo
        if (ncbl.gt.1) call uncentre(rvp)
        call uncell3D(rvp,cellp(1,i),cellp(2,i),cellp(3,i),cellp(4,i),cellp(5,i),cellp(6,i))
      endif
    enddo
    if (ioproc) then
      do i = 1,nvar
        ip = iopt(i)
        if (i.le.ncell) then
!
!  Cell parameter
!
          write(ioout,'(i3,2x,''Unit cell'',4x,a5,2x,4(1x,f12.5))') i,namecell(ip),(cellp(ip,j),j=1,jmax)
        elseif (i.le.(nvar-nbsm)) then
!
!  Internal fractional coordinate
!
          nj = (ip - 4)/3
          idj = ip - (3*nj + 3)
          write(ioout,'(i3,2x,''Fractional'',2x,i3,1x,a1,2x,4(1x,f12.5))') i,nj,namecrd(idj),(xconf(i,j+jbase),j=1,jmax)
        elseif (i.le.(nvar-nbsm)) then
!
!  Radius
!
          nj = (ip - mvar)
          write(ioout,'(i3,2x,''Radius    '',2x,i3,4x,4(1x,f12.5))') i,nj,(xconf(i,j+jbase),j=1,jmax)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  enddo
!
  return
  end
