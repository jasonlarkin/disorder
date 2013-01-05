  subroutine move2a1
!
!  Moves ions from region 2a to region 1 at the end of a defect
!  run using the relaxed positions.
!
!  12/07 Unused variables removed
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
  use control
  use current
  use defects
  use parallel
  use region2a
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ind
  integer(i4)        :: it1
  integer(i4)        :: it2
  integer(i4)        :: it3
  integer(i4)        :: j
  integer(i4)        :: n2last
  integer(i4)        :: nam
  integer(i4)        :: nati
  integer(i4)        :: nc
  integer(i4)        :: ndc
  integer(i4)        :: ni
  integer(i4)        :: nmind
  integer(i4)        :: nr1
  integer(i4)        :: nreg1o
  integer(i4)        :: nv
  logical            :: lbre
  logical            :: ldqm
  real(dp)           :: oci
  real(dp)           :: qai
  real(dp)           :: r2
  real(dp)           :: r2last
  real(dp)           :: r2max
  real(dp)           :: rai
  real(dp)           :: small
  real(dp)           :: xa
  real(dp)           :: ya
  real(dp)           :: za
  real(dp)           :: xi
  real(dp)           :: yi
  real(dp)           :: zi
!
!  Local variables
!
  r2max = reg2a1(ncf)
  r2max = r2max*r2max
  small = 1.0d-6
!
!  Read in region 2a ions to scratch
!
  rewind(48)
  do i = 1,nreg2
    read(48) xdis(i),ydis(i),zdis(i)
  enddo
  nreg1o = nreg1
!*******************************
!  Move region 2a to region 1  *
!*******************************
  r2last = reg1(ncf)*reg1(ncf)
  n2last = 0
  i = 0
  do while (r2last.le.r2max.and.i.lt.nreg2) 
    i = i + 1
    nreg1 = nreg1 + 1
    if (nreg1.gt.maxr1at) then
      maxr1at = nreg1 + 20
      call changemaxr1at
    endif
!
!  Transfer data between arrays
!
    xa = xr2a(i)
    ya = yr2a(i)
    za = zr2a(i)
    qdefe(nreg1) = qr2a(i)
    occdefe(nreg1) = or2a(i)
    radefe(nreg1) = rr2a(i)
    natdefe(nreg1) = nr2a(i)
    ntypdefe(nreg1) = ntr2a(i)
    ndefmol(nreg1) = nmr2a(i)
    ndefind(nreg1) = nmir2a(i)
    ldefbsmat(nreg1) = ldbr2a(i)
    xdefe(nreg1) = xa + xdis(i)
    ydefe(nreg1) = ya + ydis(i)
    zdefe(nreg1) = za + zdis(i)
!
!  Check distance to see if new shell has started
!
    xi = xa - xdc
    yi = ya - ydc
    zi = za - zdc
    r2 = xi*xi + yi*yi + zi*zi
    if ((r2 - r2last).gt.small) then
      n2last = nreg1
      r2last = r2
    endif
  enddo
  if (r2last.gt.r2max) then
!
!  Exited because of radius being reached
!
    nreg1 = n2last - 1
    reg1(ncf) = reg2a1(ncf)
  endif
!
!  Add optimisation flags
!
  do i = nreg1o + 1,nreg1
    ind = 3*(i - 1)
    idopt(nvar + 1) = ind + 1
    idopt(nvar + 2) = ind + 2
    idopt(nvar + 3) = ind + 3
    nvar = nvar + 3
  enddo
!*********************************************
!  Write out new region 1 info to disk file  *
!*********************************************
!
!  Copy region 1 
!
  rewind(41)
  rewind(42)
  ndc = 0
  do while (ndc.lt.ncfg)
    read(41,end = 10)nc,nr1
    if (ioproc) write(42)nc,nr1
    ndc = ndc + 1
    do j = 1,nr1
      read(41) ni,nati,xa,ya,za,qai,oci,rai,nam,nmind,lbre,ldqm
      if (ioproc) write(42) ni,nati,xa,ya,za,qai,oci,rai,nam,nmind,lbre,ldqm
      if (lflags) then
        read(41) it1,it2,it3
        if (ioproc) write(42) it1,it2,it3
      endif
    enddo
    if (ldeflin(nc)) then
      read(41)nv,ni
      if (ioproc) write(42)nv,ni
      do j = 1,nv + ni
        read(41)it1
        if (ioproc) write(42)it1
      enddo
    endif
    if (lreldin(nc)) then
      do j = 1,nr1
        read(41)it1
        if (ioproc) write(42)it1
      enddo
    endif
  enddo
10 rewind(41)
  rewind(42)
  do i = 1,ndc
    read(42)nc,nr1
    if (nc.eq.ncf) then
      if (ioproc) write(41)ncf,nreg1
      do j = 1,nreg1
        if (ioproc) write(41) natdefe(j),ntypdefe(j),xdefe(j),ydefe(j),zdefe(j),qdefe(j), &
                    occdefe(j),radefe(j),ndefmol(j),ndefind(j),ldefbsmat(j),ldqmatom(j)
        if (j.le.nreg1o) then
          read(42) ni,nati,xa,ya,za,qai,oci,rai,nam,nmind,lbre,ldqm
        endif
        if (lflags) then
          if (j.le.nreg1o) then
            read(42)it1,it2,it3
            if (ioproc) write(41)it1,it2,it3
          else
            it1 = 1
            it2 = 1
            it3 = 1
            if (ioproc) write(41)it1,it2,it3
          endif
        endif
      enddo
    else
      do j = 1,nr1
        read(42) ni,nati,xa,ya,za,qai,oci,rai,nam,nmind,lbre,ldqm
        if (ioproc) write(41) ni,nati,xa,ya,za,qai,oci,rai,nam,nmind,lbre,ldqm
        if (lflags) then
          read(42) it1,it2,it3
          if (ioproc) write(41) it1,it2,it3
        endif
      enddo
    endif
    if (ldeflin(nc)) then
      read(42)nv,ni
      if (ioproc) write(41)nv,ni
      do j = 1,nv + ni
        read(42)it1
        if (ioproc) write(41)it1
      enddo
    endif
    if (lreldin(nc)) then
      do j = 1,nr1
        read(42)it1
        if (ioproc) write(41)it1
      enddo
    endif
  enddo
!
!  Set move radius to zero as move has been performed
!
  reg2a1(ncf) = 0.0_dp
!
  return
  end
