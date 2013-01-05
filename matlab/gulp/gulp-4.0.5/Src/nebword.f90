  subroutine nebword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!
!  Processes input for Nudged Elastic Band method
!
!  nru = fortran channel for reading input
!
!   2/03 Created from surfword.f
!   8/06 nru passed to linepro
!  11/06 Modified for GULP3.2
!  11/06 Radii added
!   6/08 mod of ffractional coordinates added unless nomod is input
!  12/08 Module input renamed to gulpinput
!  12/08 Separate parameters for synchronous transit added
!  10/11 Call to cart2frac modified by adding cell indices
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
!  Julian Gale, NRI, Curtin University, October 2011
!
  use constants
  use control
  use configurations
  use current
  use element, only : maxele
  use general
  use gulpinput
  use neb
  use parallel
  use reallocate
  use synchro
  implicit none
!
!  Passed variables
!
  character(len=20)                        :: word
  character(len=maxlinelength)             :: line
  integer(i4)                              :: iline
  integer(i4)                              :: ncurr
  integer(i4)                              :: nru
  logical                                  :: l55
  logical                                  :: l1000
  logical                                  :: lwordok
!
!  Local variables
!
  integer(i4)                              :: i
  integer(i4)                              :: icx
  integer(i4)                              :: icy
  integer(i4)                              :: icz
  real(dp)                                 :: alpharep
  real(dp)                                 :: arep
  real(dp)                                 :: betarep
  real(dp)                                 :: brep
  real(dp)                                 :: crep
  real(dp)                                 :: gammarep
  real(dp)                                 :: rvrep(3,3)
  real(dp)                                 :: units
  real(dp)                                 :: xci
  real(dp)                                 :: yci
  real(dp)                                 :: zci
!
!  Search for valid option
!
  if (index(word,'rcar').eq.1) goto 110
  if (index(word,'rcel').eq.1) goto 120
  if (index(word,'rfra').eq.1) goto 130
  if (index(word,'nebi').eq.1) goto 140
  if (index(word,'nebre').eq.1) goto 150
  if (index(word,'nebto').eq.1) goto 160
  if (index(word,'fcar').eq.1) goto 170
  if (index(word,'fcel').eq.1) goto 180
  if (index(word,'ffra').eq.1) goto 190
  if (index(word,'nebs').eq.1) goto 200
  if (index(word,'nebra').eq.1) goto 220
  if (index(word,'nebm').eq.1) goto 230
  if (index(word,'nebta').eq.1) goto 240
  if (index(word,'synci').eq.1) goto 250
  if (index(word,'syncs').eq.1) goto 260
  if (index(word,'synct').eq.1) goto 270
  return
!***************************************
!  Read replica Cartesian coordinates  *
!***************************************
110 if (nnebreplicatot+1.gt.maxnebreplicatot) then
    maxnebreplicatot = nnebreplicatot + 10
    call changemaxnebreplicatot
  endif
!
!  Increment replica counters
!
  nnebreplicatot = nnebreplicatot + 1
  nnebreplica(ncurr) = nnebreplica(ncurr) + 1
  nebreplicacfgptr(nnebreplicatot) = ncurr
!
!  Get replica number from input line
!
  if (nfloat.gt.0) then
    nnebreplicano(nnebreplicatot) = nint(floats(1))
  else
    call outerror('Replica number missing from rcartesian input',iline)
    call stopnow('nebword')
  endif
!
!  Set number of atoms based on current configuration
!
  nasym = nascfg(ncurr)
!
!  Loop over atoms within replica reading coordinates
!
  do i = 1,nasym
115 line = '  '
    read(nru,'(a)',end=118) line
    iline = iline + 1
    call linepro(nru,line,iline)
!
!  Check whether the line is blank
!
    if ((nword+nfloat).eq.0) goto 115
!
!  Check whether number of coordinates is correct
!
    if (nfloat.lt.3) then
      call outerror('Insufficient data in replica coordinate input',iline)
      call stopnow('nebword')
    endif
!
!  Assign coordinates to array
!
    nebreplicaxyz(1,i,nnebreplicatot) = floats(1)
    nebreplicaxyz(2,i,nnebreplicatot) = floats(2)
    nebreplicaxyz(3,i,nnebreplicatot) = floats(3)
    if (nfloat.ge.4) then
      nebreplicaradius(i,nnebreplicatot) = abs(floats(4))
    else
      nebreplicaradius(i,nnebreplicatot) = 0.0_dp
    endif
!
!  End of input loop
!
  enddo
!
!  Convert coordinates from Cartesian to fractional
!
  if (ndimen(ncurr).eq.3) then
    do i = 1,nasym
      nebreplicaxyz(1,i,nnebreplicatot) = nebreplicaxyz(1,i,nnebreplicatot)*scalefactor
      nebreplicaxyz(2,i,nnebreplicatot) = nebreplicaxyz(2,i,nnebreplicatot)*scalefactor
      nebreplicaxyz(3,i,nnebreplicatot) = nebreplicaxyz(3,i,nnebreplicatot)*scalefactor
    enddo
    arep = nebreplicacell(1,nnebreplicatot)
    brep = nebreplicacell(2,nnebreplicatot)
    crep = nebreplicacell(3,nnebreplicatot)
    alpharep = nebreplicacell(4,nnebreplicatot)
    betarep  = nebreplicacell(5,nnebreplicatot)
    gammarep = nebreplicacell(6,nnebreplicatot)
    call cell3D(rvrep,arep,brep,crep,alpharep,betarep,gammarep)
  elseif (ndimen(ncurr).eq.2) then
    do i = 1,nasym
      nebreplicaxyz(1,i,nnebreplicatot) = nebreplicaxyz(1,i,nnebreplicatot)*scalefactor
      nebreplicaxyz(2,i,nnebreplicatot) = nebreplicaxyz(2,i,nnebreplicatot)*scalefactor
    enddo
    arep = nebreplicacell(1,nnebreplicatot)
    brep = nebreplicacell(2,nnebreplicatot)
    alpharep = nebreplicacell(3,nnebreplicatot)
    call cell2D(rvrep,arep,brep,alpharep)
  elseif (ndimen(ncurr).eq.1) then
    do i = 1,nasym
      nebreplicaxyz(1,i,nnebreplicatot) = nebreplicaxyz(1,i,nnebreplicatot)*scalefactor
    enddo
    arep = nebreplicacell(1,nnebreplicatot)
    call cell1D(rvrep,arep)
  endif
  if (ndimen(ncurr).gt.0) then
    do i = 1,nasym
      xci = nebreplicaxyz(1,i,nnebreplicatot)
      yci = nebreplicaxyz(2,i,nnebreplicatot)
      zci = nebreplicaxyz(3,i,nnebreplicatot)
      call cart2frac(ndimen(ncurr),xci,yci,zci,rvrep,nebreplicaxyz(1,i,nnebreplicatot), &
        nebreplicaxyz(2,i,nnebreplicatot),nebreplicaxyz(3,i,nnebreplicatot),icx,icy,icz)
    enddo
  endif
118 lwordok = .true.
  return
!********************************
!  Read replica cell parameters *
!********************************
120 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nnebreplicatot+1.gt.maxnebreplicatot) then
    maxnebreplicatot = nnebreplicatot + 1
    call changemaxnebreplicatot
  endif
  nebreplicacfgptr(nnebreplicatot+1) = ncurr
  if (ndimen(ncurr).eq.3) then
    if (nfloat.lt.6) then
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    if (nfloat.lt.6) then
      call outerror('Insufficient cell parameters in input',iline)
      call stopnow('nebword')
    endif
    nebreplicacell(1,nnebreplicatot+1) = abs(floats(1))*units
    nebreplicacell(2,nnebreplicatot+1) = abs(floats(2))*units
    nebreplicacell(3,nnebreplicatot+1) = abs(floats(3))*units
    nebreplicacell(4,nnebreplicatot+1) = abs(floats(4))
    nebreplicacell(5,nnebreplicatot+1) = abs(floats(5))
    nebreplicacell(6,nnebreplicatot+1) = abs(floats(6))
  elseif (ndimen(ncurr).eq.2) then
    if (nfloat.lt.3) then
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    if (nfloat.lt.3) then
      call outerror('Insufficient cell parameters in input',iline)
      call stopnow('nebword')
    endif
    nebreplicacell(1,nnebreplicatot+1) = abs(floats(1))*units
    nebreplicacell(2,nnebreplicatot+1) = abs(floats(2))*units
    nebreplicacell(3,nnebreplicatot+1) = abs(floats(3))
  elseif (ndimen(ncurr).eq.1) then
    if (nfloat.lt.1) then
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    if (nfloat.lt.1) then
      call outerror('Insufficient cell parameters in input',iline)
      call stopnow('nebword')
    endif
    nebreplicacell(1,nnebreplicatot+1) = abs(floats(1))*units
  endif
  lwordok = .true.
  return
!****************************************
!  Read replica fractional coordinates  *
!****************************************
130 if (nnebreplicatot+1.gt.maxnebreplicatot) then
    maxnebreplicatot = nnebreplicatot + 10
    call changemaxnebreplicatot
  endif
!
!  Increment replica counters
!
  nnebreplicatot = nnebreplicatot + 1
  nnebreplica(ncurr) = nnebreplica(ncurr) + 1
  nebreplicacfgptr(nnebreplicatot) = ncurr
!
!  Get replica number from input line
!
  if (nfloat.gt.0) then
    nnebreplicano(nnebreplicatot) = nint(floats(1))
  else
    call outerror('Replica number missing from rfractional input',iline)
    call stopnow('nebword')
  endif
!
!  Set number of atoms based on current configuration
!
  nasym = nascfg(ncurr)
!
!  Loop over atoms within replica reading coordinates
!
  do i = 1,nasym
135 line = '  '
    read(nru,'(a)',end=138) line
    iline = iline + 1
    call linepro(nru,line,iline)
!
!  Check whether the line is blank
!
    if ((nword+nfloat).eq.0) goto 135
!
!  Check whether number of coordinates is correct
!
    if (nfloat.lt.3) then
      call outerror('Insufficient data in replica coordinate input',iline)
      call stopnow('nebword')
    endif
!
!  Assign coordinates to array
!
    nebreplicaxyz(1,i,nnebreplicatot) = floats(1)
    nebreplicaxyz(2,i,nnebreplicatot) = floats(2)
    nebreplicaxyz(3,i,nnebreplicatot) = floats(3)
    if (nfloat.ge.4) then
      nebreplicaradius(i,nnebreplicatot) = abs(floats(4))
    else
      nebreplicaradius(i,nnebreplicatot) = 0.0_dp
    endif
!
!  End of input loop
!
  enddo
138 lwordok = .true.
  return
!************************************
!  NEB maximum number of iterations *
!************************************
140 if (nfloat.gt.0) then
    nnebiter = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!**************************
!  NEB number of replicas *
!**************************
150 if (nfloat.gt.0) then
    nnebreplica(ncurr) = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!*********************************
!  NEB tolerance for convergence *
!*********************************
160 if (nfloat.gt.0) then
    nebtol = abs(floats(1))
  endif
  lwordok = .true.
  return
!*****************************************
!  Read NEB final Cartesian coordinates  *
!*****************************************
!
!  Set number of atoms based on current configuration
!
170 nasym = nascfg(ncurr)
!
!  Loop over atoms within replica reading coordinates
!
  do i = 1,nasym
175 line = '  '
    read(nru,'(a)',end=178) line
    iline = iline + 1
    call linepro(nru,line,iline)
!
!  Check whether the line is blank
!
    if ((nword+nfloat).eq.0) goto 175
!
!  Check whether number of coordinates is correct
!
    if (nfloat.lt.3) then
      call outerror('Insufficient data in replica coordinate input',iline)
      call stopnow('nebword')
    endif
!
!  Assign coordinates to array
!
    nebfinalxyz(1,i,ncurr) = floats(1)
    nebfinalxyz(2,i,ncurr) = floats(2)
    nebfinalxyz(3,i,ncurr) = floats(3)
    if (nfloat.ge.4) then
      nebfinalradius(i,ncurr) = abs(floats(4))
    else
      nebfinalradius(i,ncurr) = 0.0_dp
    endif
!
!  End of input loop
!
  enddo
!
!  Convert coordinates from Cartesian to fractional
!
  if (ndimen(ncurr).eq.3) then
    do i = 1,nasym
      nebfinalxyz(1,i,ncurr) = nebfinalxyz(1,i,ncurr)*scalefactor
      nebfinalxyz(2,i,ncurr) = nebfinalxyz(2,i,ncurr)*scalefactor
      nebfinalxyz(3,i,ncurr) = nebfinalxyz(3,i,ncurr)*scalefactor
    enddo
    arep = nebfinalcell(1,ncurr)
    brep = nebfinalcell(2,ncurr)
    crep = nebfinalcell(3,ncurr)
    alpharep = nebfinalcell(4,ncurr)
    betarep  = nebfinalcell(5,ncurr)
    gammarep = nebfinalcell(6,ncurr)
    call cell3D(rvrep,arep,brep,crep,alpharep,betarep,gammarep)
  elseif (ndimen(ncurr).eq.2) then
    do i = 1,nasym
      nebfinalxyz(1,i,ncurr) = nebfinalxyz(1,i,ncurr)*scalefactor
      nebfinalxyz(2,i,ncurr) = nebfinalxyz(2,i,ncurr)*scalefactor
    enddo
    arep = nebfinalcell(1,ncurr)
    brep = nebfinalcell(2,ncurr)
    alpharep = nebfinalcell(3,ncurr)
    call cell2D(rvrep,arep,brep,alpharep)
  elseif (ndimen(ncurr).eq.1) then
    do i = 1,nasym
      nebfinalxyz(1,i,ncurr) = nebfinalxyz(1,i,ncurr)*scalefactor
    enddo
    arep = nebfinalcell(1,ncurr)
    call cell1D(rvrep,arep)
  endif
  do i = 1,nasym
    xci = nebfinalxyz(1,i,ncurr)
    yci = nebfinalxyz(2,i,ncurr)
    zci = nebfinalxyz(3,i,ncurr)
    call cart2frac(ndimen(ncurr),xci,yci,zci,rvrep,nebfinalxyz(1,i,ncurr), &
      nebfinalxyz(2,i,ncurr),nebfinalxyz(3,i,ncurr),icx,icy,icz)
  enddo
178 lwordok = .true.
  return
!**********************************
!  Read NEB final cell parameters *
!**********************************
180 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (ndimen(ncurr).eq.3) then
    if (nfloat.lt.6) then
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    if (nfloat.lt.6) then
      call outerror('Insufficient cell parameters in input',iline)
      call stopnow('nebword')
    endif
    nebfinalcell(1,ncurr) = abs(floats(1))*units
    nebfinalcell(2,ncurr) = abs(floats(2))*units
    nebfinalcell(3,ncurr) = abs(floats(3))*units
    nebfinalcell(4,ncurr) = abs(floats(4))
    nebfinalcell(5,ncurr) = abs(floats(5))
    nebfinalcell(6,ncurr) = abs(floats(6))
  elseif (ndimen(ncurr).eq.2) then
    if (nfloat.lt.3) then
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    if (nfloat.lt.3) then
      call outerror('Insufficient cell parameters in input',iline)
      call stopnow('nebword')
    endif
    nebfinalcell(1,ncurr) = abs(floats(1))*units
    nebfinalcell(2,ncurr) = abs(floats(2))*units
    nebfinalcell(3,ncurr) = abs(floats(3))
  elseif (ndimen(ncurr).eq.1) then
    if (nfloat.lt.1) then
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    if (nfloat.lt.1) then
      call outerror('Insufficient cell parameters in input',iline)
      call stopnow('nebword')
    endif
    nebfinalcell(1,ncurr) = abs(floats(1))*units
  endif
  lwordok = .true.
  return
!******************************************
!  Read NEB final fractional coordinates  *
!******************************************
!
!  Set number of atoms based on current configuration
!
190 nasym = nascfg(ncurr)
!
!  Loop over atoms within replica reading coordinates
!
  do i = 1,nasym
195 line = '  '
    read(nru,'(a)',end=198) line
    iline = iline + 1
    call linepro(nru,line,iline)
!
!  Check whether the line is blank
!
    if ((nword+nfloat).eq.0) goto 195
!
!  Check whether number of coordinates is correct
!
    if (nfloat.lt.3) then
      call outerror('Insufficient data in replica coordinate input',iline)
      call stopnow('nebword')
    endif
!
!  Assign coordinates to array
!
    nebfinalxyz(1,i,ncurr) = floats(1)
    nebfinalxyz(2,i,ncurr) = floats(2)
    nebfinalxyz(3,i,ncurr) = floats(3)
    if (lmodco) then
      if (ndimen(ncurr).eq.3) then
        nebfinalxyz(1,i,ncurr) = mod(nebfinalxyz(1,i,ncurr)+10.0_dp,1.0_dp)
        nebfinalxyz(2,i,ncurr) = mod(nebfinalxyz(2,i,ncurr)+10.0_dp,1.0_dp)
        nebfinalxyz(3,i,ncurr) = mod(nebfinalxyz(3,i,ncurr)+10.0_dp,1.0_dp)
      elseif (ndimen(ncurr).eq.2) then
        nebfinalxyz(1,i,ncurr) = mod(nebfinalxyz(1,i,ncurr)+10.0_dp,1.0_dp)
        nebfinalxyz(2,i,ncurr) = mod(nebfinalxyz(2,i,ncurr)+10.0_dp,1.0_dp)
      elseif (ndimen(ncurr).eq.1) then
        nebfinalxyz(1,i,ncurr) = mod(nebfinalxyz(1,i,ncurr)+10.0_dp,1.0_dp)
      endif
    endif
    if (nfloat.ge.4) then
      nebfinalradius(i,ncurr) = abs(floats(4))
    else
      nebfinalradius(i,ncurr) = 0.0_dp
    endif
!
!  End of input loop
!
  enddo
198 lwordok = .true.
  return
!*****************************************
!  NEB spring constant between replicas  *
!*****************************************
200 if (nfloat.gt.0) then
    nebspring(ncurr) = abs(floats(1))
    if (nfloat.gt.1) then
      nebspringmin(ncurr) = abs(floats(2))
      if (nebspringmin(ncurr).gt.nebspring(ncurr)) then
        call outerror('minimum spring const greater than maximum for NEB',iline)
        call stopnow('nebword')
      endif
    endif
  endif
  if (nword.gt.1) then
    do i = 2,nword
      if (index(words(i),'var').eq.1) lnebvaryspring(ncurr) = .true.
    enddo
  endif
  lwordok = .true.
  return
  return
!**********************************************
!  NEB random displacement about linear path  *
!**********************************************
220 if (nfloat.gt.0) then
    nebrandom = abs(floats(1))
  endif
  lwordok = .true.
  return
!*****************************************
!  NEB maximum displacement in one step  *
!*****************************************
230 if (nfloat.gt.0) then
    nebmaxdisp = abs(floats(1))
  endif
  lwordok = .true.
  return
!*********************
!  NEB tangent type  *
!*********************
240 if (nfloat.gt.0) then
    nebtangent = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!*************************************
!  Sync maximum number of iterations *
!*************************************
250 if (nfloat.gt.0) then
    maxsynciter = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!*********************************
!  Sync maximum number of steps  *
!*********************************
260 if (nfloat.gt.0) then
    maxsyncstep = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!**********************************
!  Sync tolerance for convergence *
!**********************************
270 if (nfloat.gt.0) then
    synctol = abs(floats(1))
  endif
  lwordok = .true.
  return
!
  end
