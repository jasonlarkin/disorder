  subroutine fefunct(iflag,n,xc,fc,gc,hessian)
!
!  Supplies the function and first derivatives of the free energy
!
!   8/97 Created from funct to replace old routine from 
!        numerical version.
!   9/97 If iflag=2 then need to keep second derivatives for
!        hessian calculation
!  11/06 xc now passed getderv1
!   1/08 lgrad2 removed
!   5/08 lgeometryOK added as argument to xctox0
!   5/09 Calls to x0tostr routines moved from energy to calling routine
!  11/09 Region derivatives added
!   4/10 Setting of lkeepd2 modified to allow for relaxed fitting
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, April 2010
!
  use configurations, only : maxregion
  use control
  use current
  use derivatives
  use four
  use general
  use six
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)    :: iflag
  integer(i4)    :: n
  real(dp)       :: fc
  real(dp)       :: xc(*)
  real(dp)       :: gc(*)
  real(dp)       :: hessian(*)
!
!  Local variables
!
  integer(i4)    :: i
  logical        :: lgeometryOK
  logical        :: lgrad1
  logical        :: lkeepd2
  logical        :: lpartfehess
  logical        :: lvibonly
  real(dp)       :: cputime
  real(dp), save :: tdmax = 0.0_dp
  real(dp)       :: t1
  real(dp)       :: t2
  real(dp)       :: tf1
  real(dp)       :: tf2
!
  t1 = cputime()
  lpartfehess = (index(keyword,'feh').ne.0)
  lvibonly = (index(keyword,'vibo').ne.0)
!
!  Derivative flags
!
  lgrad1 = (iflag.ge.1)
  lfirst = .true.
  lkeepd2 = (iflag.ge.2.and.(lopt.or.lrelax))
!**************************************************
!  Convert optimisation array to structure array  *
!**************************************************
  call xctox0(n,xc,lgeometryOK)
!************************************************************
!  Convert linear structure array to main structure arrays  *
!************************************************************
  if (lx0centroid) then
    call x0tostrcentroid
  else
    call x0tostr
  endif
!************************************
!  Build potential lists if needed  *
!************************************
  if (nfor.gt.0) call setlist4
  if (nsix.gt.0) call setlist6
!********************************************************
!  Calculate the first and second derivatives w.r.t. U  *
!********************************************************
  call energy(fc,.true.,.true.)
!**********************************************************
!  Complete strain derivatives if based only on d2U/dede  *
!**********************************************************
  if (lstr.and..not.lpartfehess) call strfin(.true.)
!
  if (lvibonly) then
    fc = 0.0_dp
    do i = 1,numat
      xdrv(i) = 0.0_dp
      ydrv(i) = 0.0_dp
      zdrv(i) = 0.0_dp
    enddo
    xregdrv(1:maxregion) = 0.0_dp
    yregdrv(1:maxregion) = 0.0_dp
    zregdrv(1:maxregion) = 0.0_dp
    do i = 1,nstrains
      strderv(i) = 0.0_dp
    enddo
  endif
!******************************************
!  Calculate free energy and derivatives  *
!******************************************
  tf1 = cputime()
  if (ndim.gt.0) then
    select case(ndim)
    case(3)
      call kvector3D
    case(2)
      call kvector2D
    case(1)
      call kvector1D
    end select
    call fenergy3(fc,lgrad1,hessian,lkeepd2)
  else
    call fenergy0(fc,lgrad1,hessian,lkeepd2)
  endif
  tf2 = cputime()
  tfederiv = tfederiv + tf2 - tf1
!*******************************************
!  For a surface, get surface free energy  *
!*******************************************
  if (lseok) call surfaceenergy(fc)
!************************************************************
!  Complete strain derivatives if based partly on d2A/dede  *
!************************************************************
  if (lstr.and.lpartfehess) then
    call strfin(.true.)
!*************************************************************
!  Symmetrise strains to allow for distribution of K points  *
!*************************************************************
  elseif (lstr.and.lsymderv) then
    call strsym
  endif
!*******************************
!  Complete First Derivatives  *
!*******************************
  if (lgrad1) then
    if (.not.lzsisa) then
      call getderv1(n,xc,gc,.false.,.true.)
    else
      call getderv1(n,xc,gc,.false.,.false.)
    endif
  endif
!*******************
!  CPU time check  *
!*******************
  t2 = cputime()
  if (t2-t1.gt.tdmax) tdmax = t2 - t1
  if (timmax.gt.0.0d0) then
    if ((timmax-t2).lt.tdmax.and..not.lrelax) iflag = -1
  endif
!
  return
  end
