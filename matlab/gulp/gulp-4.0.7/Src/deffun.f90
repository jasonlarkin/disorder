  subroutine deffun(iflag,n,xc,fc,gc)
!
!  Supplies the function and derivatives for defects
!
!   6/95 Modified to allow for additive defect constraints
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use control
  use current
  use defects
  use general
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)    :: iflag
  integer(i4)    :: n
  real(dp)       :: fc
  real(dp)       :: xc(*)
  real(dp)       :: gc(*)
!
!  Local variables
!
  integer(i4)    :: i
  integer(i4)    :: iff
  integer(i4)    :: ii
  integer(i4)    :: ij
  integer(i4)    :: ind
  integer(i4)    :: indf
  integer(i4)    :: indv
  integer(i4)    :: iv
  integer(i4)    :: ivv
  integer(i4)    :: j
  integer(i4)    :: n3r1
  integer(i4)    :: neq
  integer(i4)    :: nf
  integer(i4)    :: nj
  integer(i4)    :: nr
  integer(i4)    :: nv
  logical        :: lfound
  logical        :: lgrad1
  logical        :: lgrad2
  real(dp)       :: csft(3)
  real(dp)       :: cputime
  real(dp)       :: t1
  real(dp)       :: t2
  real(dp), save :: tdmax = 0.0_dp
!
  t1 = cputime()
  lgrad1 = (iflag.ge.1)
  lgrad2 = (iflag.ge.2)
!
!  First substitute parameters into place
!
  do i = 1,n
    x0(idopt(i)) = xc(i)
  enddo
!
!  Now apply constraints
!
  if (ndcon.gt.0) then
    csft(1) = xdc
    csft(2) = ydc
    csft(3) = zdc
    do i = 1,ndcon
      nf = ncdfix(i)
      iff = mod(nf,3_i4)
      if (iff.eq.0) iff = 3
      x0(nf) = csft(iff)
    enddo
    do i = 1,ndcon
      nf = ncdfix(i)
      nv = ncdvar(i)
      ivv = mod(nv,3_i4)
      if (ivv.eq.0) ivv = 3
      x0(nf) = (x0(nv) - csft(ivv))*dconco(i) + x0(nf)
    enddo
  endif
!*****************************************
!  Return variables to structure arrays  *
!*****************************************
  ind = 0
  if (ldsym) then
    do i = 1,ndasym
      ii = ndsptr(i)
      xdefe(ii) = x0(ind+1)
      ydefe(ii) = x0(ind+2)
      zdefe(ii) = x0(ind+3)
      ind = ind + 3
    enddo
    if (ldbsm) then
      do i = 1,ndasym
        ii = ndsptr(i)
        radefe(ii) = x0(ind+i)
      enddo
    endif
    call defequ
  else
    do i = 1,nreg1
      xdefe(i) = x0(ind+1)
      ydefe(i) = x0(ind+2)
      zdefe(i) = x0(ind+3)
      ind = ind + 3
    enddo
    if (ldbsm) then
      do i = 1,nreg1
        radefe(i) = x0(ind+i)
      enddo
    endif
  endif
  lfirst = .true.
!********************************************
!  Evaluate function and first derivatives  *
!********************************************
  call defener(fc,lgrad1,lgrad2)
  if (lgrad1) then
!***************************************************************
!  Collect derivatives from xdrv,ydrv,zdrv and raderv into gc  *
!***************************************************************
    if (ldsym.and.((lgrad2.and..not.ld2sym).or..not.ld1sym)) then
!
!  If lgrad2, gradients were generated for full cell so they
!  must now be symmetrised
!
      do i = 1,ndasym
        nr = ndsptr(i)
        neq = ndeqv(i)
        xdrv(i) = neq*xdrv(nr)
        ydrv(i) = neq*ydrv(nr)
        zdrv(i) = neq*zdrv(nr)
        if (ldefbsmat(nr)) raderv(i) = neq*raderv(nr)
      enddo
    endif
    if (ldsym) then
      n3r1 = 3*ndasym
    else
      n3r1 = 3*nreg1
    endif
    do i = 1,n
      ind = idopt(i)
      if (ind.gt.n3r1) then
!
!  Radial derviative
!
        ind = ind - n3r1
        gc(i) = raderv(ind)
      else
!
!  Cartesian derivative
!
        nj = (ind+2)/3
        ij = ind - (3*nj-3)
        if (ij.eq.1) then
          gc(i) = xdrv(nj)
        elseif (ij.eq.2) then
          gc(i) = ydrv(nj)
        else
          gc(i) = zdrv(nj)
        endif
      endif
    enddo
!****************************
!  Constrained derivatives  *
!****************************
    if (ndcon.gt.0) then
      do i = 1,ndcon
        indf = ncdfix(i)
        indv = ncdvar(i)
        if (indf.gt.n3r1) then
!
!  Radial derivatives
!
          nv = indf - n3r1
          lfound = .false.
          j = 0
          do while (.not.lfound.and.j.lt.n)
            j = j + 1
            if (indv.eq.idopt(j)) lfound = .true.
          enddo
          gc(j) = gc(j) + raderv(nv)*dconco(i)
        else
!
!  Cartesian derivatives
!
          nv = (indf + 2)/3
          iv = indf - (3*nv - 3)
          lfound = .false.
          j = 0
          do while (.not.lfound.and.j.lt.n)
            j = j + 1
            if (indv.eq.idopt(j)) lfound = .true.
          enddo
          if (iv.eq.1) then
            gc(j) = gc(j) + xdrv(nv)*dconco(i)
          elseif (iv.eq.2) then
            gc(j) = gc(j) + ydrv(nv)*dconco(i)
          else
            gc(j) = gc(j) + zdrv(nv)*dconco(i)
          endif
        endif
      enddo
    endif
  endif
  t2 = cputime()
  if (t2-t1.gt.tdmax) tdmax = t2 - t1
  if (timmax.gt.0.0_dp) then
    if ((timmax-t2).lt.tdmax.and..not.lrelax) iflag = - 1
  endif
!
  return
  end
