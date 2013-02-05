  subroutine setmaxcell1D(maxcell)
!
!  This subroutine finds the maximum number of cells required for
!  convergence of the electrostatic energy in 1-D. Used in the
!  setup for potential calculation.
!
!   5/02 Created from real1D
!   5/02 hfunc call modified for third derivatives
!  12/02 maxcell forced to be at least 1
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
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, June 2005
!
  use constants,    only : angstoev
  use control,      only : lnoreal
  use current
  use element,      only : maxele
  use general,      only : accuracy, nmaxcells, nemorder
  use shell,        only : cuts
  use times
  implicit none
!
!  Passed arguments
!
  integer(i4) :: maxcell
!
!  Local variables
!
  integer(i4) :: i,j 
  integer(i4) :: m
  integer(i4) :: nati
  integer(i4) :: natj
  logical     :: lconverged
  logical     :: lcspair
  real(dp)    :: accf
  real(dp)    :: acell
  real(dp)    :: cputime
  real(dp)    :: cut2s
  real(dp)    :: d0
  real(dp)    :: dh1(3)
  real(dp)    :: dh2(3)
  real(dp)    :: dh1s
  real(dp)    :: dh2s
  real(dp)    :: d2h1(6)
  real(dp)    :: d2h2(6)
  real(dp)    :: d2h1m(3)
  real(dp)    :: d2h2m(3)
  real(dp)    :: d2h1s
  real(dp)    :: d2h2s
  real(dp)    :: d3h1(10)
  real(dp)    :: d3h1m(6)
  real(dp)    :: ediff
  real(dp)    :: elast
  real(dp)    :: ereal
  real(dp)    :: esum
  real(dp)    :: esumem
  real(dp)    :: esumh
  real(dp)    :: e1
  real(dp)    :: e2
  real(dp)    :: h1
  real(dp)    :: h2
  real(dp)    :: lna
  real(dp)    :: oci
  real(dp)    :: ocj
  real(dp)    :: qi
  real(dp)    :: qj
  real(dp)    :: qii
  real(dp)    :: qij
  real(dp)    :: r
  real(dp)    :: rcut
  real(dp)    :: rr
  real(dp)    :: t1, t2
  real(dp)    :: u
  real(dp)    :: x
  real(dp)    :: y
  real(dp)    :: z
!
  t1 = cputime()
!
!  If noreal specified, return
!
  if (lnoreal) then
    ereal = 0.0_dp
    return
  endif
!********************************************************
!  Calculate Coulomb sum converged to desired accuracy  *
!********************************************************
!
!  Loop over number of cells in sum
!
  accf = 10.0**(-accuracy)
  lna = log(a)
  cut2s = cuts*cuts
  m = - 1
  lconverged = .false.
  elast = 0.0_dp
  esum = 0.0_dp
  do while (m.lt.nmaxcells.and..not.lconverged) 
    m = m + 1
!
!  Direct sum component over neutral cells
!
    acell = dble(m)*a
    do i = 2,numat
      nati = nat(i)
      oci = occuf(i)
      qi = qf(i)*oci
      do j = 1,i-1
        natj = nat(j)
        ocj = occuf(j)
        qj = qf(j)*ocj
        lcspair=(abs(nati-natj).eq.maxele.or.(oci+ocj).lt.1.0001d0)
        if (lcspair) then
          rcut = cut2s
        else
          rcut = 1.0d-15
        endif
        x = acell + xclat(j) - xclat(i)
        y = yclat(j) - yclat(i)
        z = zclat(j) - zclat(i)
        r = x*x + y*y + z*z
        if (r.gt.rcut) then
          r = sqrt(r)
          rr = 1.0_dp/r
          d0 = qi*qj*rr
          esum = esum + d0
        endif
        if (m.gt.0) then
          x = - acell + xclat(j) - xclat(i)
          r = x*x + y*y + z*z
          if (r.gt.rcut) then
            r = sqrt(r)
            rr = 1.0_dp/r
            d0 = qi*qj*rr
            esum = esum + d0
          endif
        endif
      enddo
    enddo
!
!  Self interactions
!
    do i = 1,numat
      oci = occuf(i)
      qi = qf(i)*oci
      r = acell*acell
      if (r.gt.1.0d-15) then
        r = sqrt(r)
        rr = 1.0_dp/r
        d0 = qi*qi*rr
        esum = esum + d0
      endif
    enddo
!
!  Neutralising terms
!
!  Background
!
!  and
!
!  Euler-MacLaurin component
!
    u = (dble(m)+0.5_dp)*a
    esumh = 0.0_dp
    esumem = 0.0_dp
    do i = 2,numat
      oci = occuf(i)
      qi = qf(i)*oci
      do j = 1,i-1
        ocj = occuf(j)
        qj = qf(j)*ocj
        qij = qi*qj
        x = xclat(j) - xclat(i)
        y = yclat(j) - yclat(i)
        z = zclat(j) - zclat(i)
        call hfunc(u,+x,1.0_dp,y,z,h1,dh1,d2h1,d3h1,.false.,.false.,.false.)
        call hfunc(u,-x,-1.0_dp,y,z,h2,dh2,d2h2,d3h1,.false.,.false.,.false.)
        esumh = esumh - qij*(h1 + h2 - 2.0_dp*lna)/a
        call emfunc(nemorder,u,+x,1.0_dp,y,z,a,e1,dh1,d2h1,dh1s, &
          d2h1s,d2h1m,d3h1,d3h1m,.false.,.false.,.false.)
        call emfunc(nemorder,u,-x,-1.0_dp,y,z,a,e2,dh2,d2h2,dh2s, &
          d2h2s,d2h2m,d3h1,d3h1m,.false.,.false.,.false.)
        esumem = esumem + qij*(e1 + e2)
      enddo
    enddo
    do i = 1,numat
      oci = occuf(i)
      qi = qf(i)*oci
      qii = qi*qi
      call hfunc(u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,h1,dh1,d2h1,d3h1,.false.,.false.,.false.)
      esumh = esumh - qii*(h1 - lna)/a
      call emfunc(nemorder,u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,a,e1, &
        dh1,d2h1,dh1s,d2h1s,d2h1m,d3h1,d3h1m,.false.,.false.,.false.)
      esumem = esumem + qii*e1
    enddo
!
!  Sum up terms
!
    ereal = esum + esumh + esumem
!
!  Compare to energy with previous number of cells
!  and check for convergence to the required
!  accuracy.
!
    if (abs(ereal).lt.accf) lconverged = .true.
    if (.not.lconverged) then
      ediff = abs((ereal - elast)/ereal)
      lconverged = (ediff.lt.accf)
    endif
    elast = ereal
  enddo
  ereal = ereal*angstoev
!
!  Set return value ensuring that it is at least 1
!
  maxcell = max(m,1_i4)
!
  t2 = cputime()
  tatom = tatom + t2 - t1
!
  return
  end
