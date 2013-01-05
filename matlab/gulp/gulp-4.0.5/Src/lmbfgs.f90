  subroutine lmbfgssub(iter,n,m,stp,x,g,diag,w,maxw,pvect)
!
!  Subroutine for Limited Memory BFGS algorithm
!
!   1/09 Integer datatypes all explicitly declared
!  12/09 Support for defect calculations added
!
  use datatypes
  use iochannels
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(in)    :: iter
  integer(i4),  intent(in)    :: m
  integer(i4),  intent(in)    :: n
  integer(i4),  intent(in)    :: maxw
  real(dp),     intent(inout) :: diag(n)
  real(dp),     intent(in)    :: g(n)
  real(dp),     intent(inout) :: pvect(n)
  real(dp),     intent(inout) :: stp
  real(dp),     intent(inout) :: x(n)
  real(dp),     intent(inout) :: w(maxw)    ! Size should be at least w(n*(2*m+1)+2*m)
!
!  Local variables
!
  integer(i4)       :: bound
  integer(i4)       :: CP
  integer(i4)       :: I
  integer(i4)       :: inmc
  integer(i4)       :: iscn
  integer(i4)       :: ispt
  integer(i4)       :: iycn
  integer(i4)       :: iypt
  integer(i4)       :: NPT
  integer(i4), save :: point = 0
  real(dp)          :: beta
  real(dp)          :: ddot
  real(dp)          :: gnorm
  real(dp)          :: SQ
  real(dp)          :: YR
  real(dp)          :: YS
  real(dp)          :: YY
!
! The work vector w is divided as follows:
! ---------------------------------------
! The first N locations are used to store the gradient and other temporary information.
! Locations (N+1)...(N+M) store the scalars rho.
! Locations (N+M+1)...(N+2M) store the numbers alpha used in the formula that computes H*G.
! Locations (N+2M+1)...(N+2M+NM) store the last M search steps.
! Locations (N+2M+NM+1)...(N+2M+2NM) store the last M gradient differences.
!
! The search steps and gradient differences are stored in a
! circular order controlled by the parameter point.
!
  ispt = N + 2*M
  iypt = ispt + N*M     
  bound = iter - 1
  if (iter.eq.1) then
    do I = 1,N
      W(ispt+I) = - G(I)*diag(I)
      pvect(I) = - G(I)*diag(I)
    enddo
    gnorm = ddot(N,G,1_i4,G,1_i4)
    stp = 1.0_dp/gnorm**2
    point = 0
  else
    if (iter .gt. M) bound = M
!
! Compute the new step and gradient change 
!
    npt = point*N
    do I = 1,N
      W(ispt+npt+I) = stp*W(ispt+npt+I)
      W(iypt+npt+I) = G(I) - W(I)
    enddo
    point = point + 1
    if (point.eq.M) point = 0

    YS = ddot(N,W(iypt+npt+1),1_i4,W(ispt+npt+1),1_i4)
    YY = ddot(N,W(iypt+npt+1),1_i4,W(iypt+npt+1),1_i4)
    do I = 1,N
      diag(I) = YS/YY
    enddo
!
!  Compute -H*G using the formula given in: Nocedal, J. 1980,
!  "Updating quasi-Newton matrices with limited storage",
!  Mathematics of Computation, Vol.24, No.151, pp. 773-782.
!
    CP = point
    if (point.eq.0) CP = M
    W(N+CP) = 1.0_dp/YS
    do I = 1,N
      W(I) = - G(I)
    enddo
    CP = point
    do I = 1,bound
      CP = CP - 1
      if (CP.eq. -1) CP = M - 1
      sq = ddot(N,W(ispt+CP*N+1),1_i4,W,1_i4)
      inmc = N + M + CP + 1
      iycn = iypt + CP*N
      W(inmc) = W(N+CP+1)*SQ
      call daxpy(N,-W(inmc),W(iycn+1),1_i4,W,1_i4)
    enddo
!
    do I = 1,N
      W(I) = diag(I)*W(I)
    enddo
!
    do I = 1,bound
      YR = ddot(N,W(iypt+CP*N+1),1_i4,W,1_i4)
      beta = W(N+CP+1)*YR
      inmc = N + M +CP + 1
      beta = W(inmc) - beta
      iscn = ispt + CP*N
      call daxpy(N,beta,W(iscn+1),1_i4,W,1_i4)
      CP = CP + 1
      if (CP.eq.M) CP = 0
    enddo
!
! Store the new search direction
!
    do I = 1,N
      W(ispt+point*N+I) = W(I)
      pvect(I) = W(I)
    enddo
    stp = 1.0_dp
  endif
!
!  Copy gradients to W
!
  call dcopy(n,g,1_i4,w,1_i4)
!
  return
  end
!*******************************
!  Line search routine mcsrch  *
!*******************************
  subroutine MCSRCH3(N,X,F,G,S,stp,INFO,WA,ldefect)
!
  use datatypes
  use iochannels
  use optimisation
!
  integer(i4),  intent(in)    :: N
  integer(i4),  intent(inout) :: INFO
  logical,      intent(in)    :: ldefect
  real(dp)    :: F,stp
  real(dp)    :: X(N),G(N),S(N),WA(N)
!                
!  A slight modification of the subroutine CSRCH of More' and Thuente.
!  The changes are to allow reverse communication, and do not affect
!  the performance of the routine. 
!
!  The purpose of mcsrch is to find a step which satisfies
!  a sufficient decrease condition and a curvature condition.
!
!  At each stage the subroutine updates an interval of
!  uncertainty with endpoints stx and sty. The interval of
!  uncertainty is initially chosen so that it contains a
!  minimizer of the modified function
!
!    F(X+stp*S) - F(X) - ftol*stp*(GRADF(X)'S).
!
!  If a step is obtained for which the modified function
!  has a nonpositive function value and nonnegative derivative,
!  then the interval of uncertainty is chosen so that it
!  contains a minimizer of F(X+stp*S).
!
!  The algorithm is designed to find a step which satisfies
!  the sufficient decrease condition
!
!    F(X+stp*S) .LE. F(X) + ftol*stp*(GRADF(X)'S),
!
!  and the curvature condition
!
!    abs(GRADF(X+stp*S)'S)) .le. GTOL*abs(GRADF(X)'S).
!
!  If ftol is less than gtol and if, for example, the function
!  is bounded below, then there is always a step which satisfies
!  both conditions. If no step can be found which satisfies both
!  conditions, then the algorithm usually stops when rounding
!  errors prevent further progress. In this case stp only
!  satisfies the sufficient decrease condition.
!
!  The subroutine statement is
!
!    SUBROUTINE MCSRCH(N,X,F,G,S,stp,ftol,XTOL, INFO, WA)
!
!  where
!
!    N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
!      OF VARIABLES.
!
!    X IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!      BASE POINT FOR THE LINE SEARCH. ON OUTPUT IT CONTAINS
!      X + stp*S.
!
!    F IS A VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F
!      AT X. ON OUTPUT IT CONTAINS THE VALUE OF F AT X + stp*S.
!
!    G IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!      GRADIENT OF F AT X. ON OUTPUT IT CONTAINS THE GRADIENT
!      OF F AT X + stp*S.
!
!    S IS AN INPUT ARRAY OF LENGTH N WHICH SPECIFIES THE
!      SEARCH DIRECTION.
!
!    stp is a nonnegative variable. On input stp contains an
!      initial estimate of a satisfactory step. On output
!      stp contains the final estimate.
!
!    ftol and gtol are nonnegative input variables. (In this reverse
!      communication implementation GTOL is defined in a COMMON
!      statement.) Termination occurs when the sufficient decrease
!      condition and the directional derivative condition are
!      satisfied.
!
!    xtol is a nonnegative input variable. termination occurs
!      when the relative WIDTH of the interval of uncertainty
!      is at most xtol.
!
!    stpmin and stpmax are nonnegative input variables which
!      specify lower and upper bounds for the step. (In this reverse
!      communication implementatin they are defined in a common
!      statement).
!
!    INFO is an integer output variable set as follows:
!
!      INFO = 0  IMPROPER INPUT PARAMETERS.
!
!      INFO =-1  A return IS MADE TO COMPUTE THE FUNCTION AND GRADIENT.
!
!      INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
!                DIRECTIONAL DERIVATIVE CONDITION HOLD.
!
!      INFO = 2  RELATIVE width OF THE INTERVAL OF UNCERTAINTY
!                IS AT MOST XTOL.
!
!      INFO = 4  THE STEP IS AT THE LOWER bound stpMIN.
!
!      INFO = 5  THE STEP IS AT THE UPPER bound stpMAX.
!
!      INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
!                THERE MAY NOT BE A STEP WHICH SATISFIES THE
!                SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
!                TOLERANCES MAY BE TOO SMALL.
!
!      WA IS A WORK ARRAY OF LENGTH N.
!
!  Subprograms called
!
!    MCSTEP
!
!    FORTRAN-SUPPLIED...ABS,MAX,MIN
!
!    ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!    JORGE J. MORE', DAVID J. THUENTE
!
  integer(i4) :: INFOC,J,iflag
  logical     :: BRACKT,STAGE1
  real(dp)    :: DG,DGM,dginit,dgtest,DGX,DGXM,DGY,DGYM, &
                 FINIT,FTEST1,FM,FX,FXM,FY,FYM,P5,P66,STX, &
                 STY,STMIN,STMAX,width,width1,XTRAPF,zero
  data P5,P66,XTRAPF,zero /0.5D0,0.66D0,4.0D0,0.0D0/
  INFOC = 1
!
!  Check the input parameters for errors.
!
  if (N .LE. 0 .OR. stp .LE. zero .OR. ftol .LT. zero .OR. &
      lmgtol .LT. zero .OR. XTOL .LT. zero .OR. stepmin .LT. zero &
      .OR. stepmax .LT. stepmin ) return
!
!  Compute the initial gradient in the search direction and check that s is a descent direction.
!
  dginit = zero
  do J = 1, N
    dginit = dginit + G(J)*S(J)
  enddo
  if (dginit .GE. zero) then
    !write(ioout,'(''The search direction is not a descent direction'')')
    info = 6
    return
  endif
!
!  Initialize local variables.
!
  BRACKT = .false.
  STAGE1 = .true.
  FINIT = F
  dgtest = ftol*dginit
  width = stepmax - stepmin
  width1 = width/P5
  do J = 1, N
    WA(J) = X(J)
  enddo
!
!  THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
!  FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
!  THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
!  FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
!  THE INTERVAL OF UNCERTAINTY.
!  THE VARIABLES stp, F, DG CONTAIN THE VALUES OF THE STEP,
!  FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
!
  STX = zero
  FX = FINIT
  DGX = dginit
  STY = zero
  FY = FINIT
  DGY = dginit
!
!  Start of iteration.
!
30 continue
!
!  Set the minimum and maximum steps to correspond to the present interval of uncertainty.
!
  if (BRACKT) then
    STMIN = MIN(STX,STY)
    STMAX = MAX(STX,STY)
  else
    STMIN = STX
    STMAX = stp + XTRAPF*(stp - STX)
  endif
!
!  Force the step to be within the bounds stepmax and stepmin.
!
  stp = MAX(stp,stepmin)
  stp = MIN(stp,stepmax)
!
!  If an unusual termination is to occur then let stp be the lowest point obtained so far.
!
  IF ((BRACKT .AND. (stp .LE. STMIN .OR. stp .GE. STMAX)) &
        .OR. INFOC .EQ. 0 .OR. (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX)) stp = STX
!
!  Evaluate the function and gradient at stp and compute the directional derivative.
!  We return to main program to obtain F and G.
!
  do J = 1, N
    X(J) = WA(J) + stp*S(J)
  enddo
!
  INFO = 0
  iflag = 1_i4
  if (ldefect) then
    call deffun(iflag,N,X,F,G)
  else
    call funct(iflag,N,X,F,G)
  endif
  DG = zero
  do J = 1, N
    DG = DG + G(J)*S(J)
  enddo
  FTEST1 = FINIT + stp*dgtest
!
!  Test for convergence.
!
  IF ((BRACKT .AND. (stp .LE. STMIN .OR. stp .GE. STMAX)) .OR. INFOC .EQ. 0) INFO = 6
  IF (stp .EQ. stepmax .AND. F .LE. FTEST1 .AND. DG .LE. dgtest) INFO = 5
  IF (stp .EQ. stepmin .AND. (F .GT. FTEST1 .OR. DG .GE. dgtest)) INFO = 4
  IF (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX) INFO = 2
  IF (F .LE. FTEST1 .AND. ABS(DG) .LE. lmgtol*(-dginit)) INFO = 1
!
!  Check for termination.
!
  if (INFO .NE. 0) return
!
!  In the first stage we seek a step for which the modified function has a nonpositive value and nonnegative derivative.
!
  if (STAGE1 .AND. F .LE. FTEST1 .AND. DG .GE. MIN(ftol,lmgtol)*dginit) STAGE1 = .false.
!
!  A modified function is used to predict the step only if
!  we have not obtained a step for which the modified
!  function has a nonpositive function value and nonnegative
!  derivative, and if a lower function value has been
!  obtained but the decrease is not sufficient.
!
  if (STAGE1 .AND. F .LE. FX .AND. F .GT. FTEST1) then
!
!  Define the modified function and derivative values.
!
    FM = F - stp*dgtest
    FXM = FX - STX*dgtest
    FYM = FY - STY*dgtest
    DGM = DG - dgtest
    DGXM = DGX - dgtest
    DGYM = DGY - dgtest
!
!  Call cstep to update the interval of uncertainty and to compute the new step.
!
    call MCSTEP(STX,FXM,DGXM,STY,FYM,DGYM,stp,FM,DGM,BRACKT,STMIN,STMAX,INFOC)
!
!  Reset the function and gradient values for f.
!
    FX = FXM + STX*dgtest
    FY = FYM + STY*dgtest
    DGX = DGXM + dgtest
    DGY = DGYM + dgtest
  else
!
!  Call mcstep to update the interval of uncertainty and to compute the new step.
!
    call mcstep(STX,FX,DGX,STY,FY,DGY,stp,F,DG,BRACKT,STMIN,STMAX,INFOC)
  endif
!
!  Force a sufficient decrease in the size of the interval of uncertainty.
!
  if (BRACKT) then
    IF (ABS(STY-STX) .GE. P66*width1) stp = STX + P5*(STY - STX)
    width1 = width
    width = ABS(STY-STX)
  endif
!
!  End of iteration.
!
  goto 30
!
!  Last line of subroutine mcsrch.
!
  end

  subroutine mcstep(STX,FX,DX,STY,FY,DY,stp,FP,DMP,BRACKT,stpMIN,stpMAX,INFO)
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4) :: INFO
  real(dp)    :: STX,FX,DX,STY,FY,DY,stp,FP,DMP,stpMIN,stpMAX
  logical     :: BRACKT,bound
!
!  The purpose of mcstep is to compute a safeguarded step for
!  a linesearch and to update an interval of uncertainty for
!  a minimizer of the function.
!
!  The parameter stx contains the step with the least function
!  value. The parameter stp contains the current step. It is
!  assumed that the derivative at stx is negative in the
!  direction of the step. If brackt is set true then a
!  minimizer has been bracketed in an interval of uncertainty
!  with endpoints stx and sty.
!
!  THE SUBROUTINE STATEMENT IS
!
!  SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,stp,FP,DMP,BRACKT,
!                      stpMIN,stpMAX,INFO)
!
!   WHERE
!
!     STX, FX, AND DX ARE VARIABLES WHICH SPECIFY THE STEP,
!       THE FUNCTION, AND THE DERIVATIVE AT THE BEST STEP OBTAINED
!       SO FAR. THE DERIVATIVE MUST BE NEGATIVE IN THE DIRECTION
!       OF THE STEP, THAT IS, DX AND stp-STX MUST HAVE OPPOSITE
!       SIGNS. ON OUTPUT THESE PARAMETERS ARE UPDATED APPROPRIATELY.
!
!     STY, FY, AND DY ARE VARIABLES WHICH SPECIFY THE STEP,
!       THE FUNCTION, AND THE DERIVATIVE AT THE OTHER ENDPOINT OF
!       THE INTERVAL OF UNCERTAINTY. ON OUTPUT THESE PARAMETERS ARE
!       UPDATED APPROPRIATELY.
!
!     stp, FP, AND DMP ARE VARIABLES WHICH SPECIFY THE STEP,
!       THE FUNCTION, AND THE DERIVATIVE AT THE CURRENT STEP.
!       IF BRACKT IS SET.true.THEN ON INPUT stp MUST BE
!       BETWEEN STX AND STY. ON OUTPUT stp IS SET TO THE NEW STEP.
!
!     BRACKT IS A LOGICAL VARIABLE WHICH SPECIFIES IF A MINIMIZER
!       HAS BEEN BRACKETED. IF THE MINIMIZER HAS NOT BEEN BRACKETED
!       THEN ON INPUT BRACKT MUST BE SET.false. IF THE MINIMIZER
!       IS BRACKETED THEN ON OUTPUT BRACKT IS SET.true.
!
!     stpMIN AND stpMAX ARE INPUT VARIABLES WHICH SPECIFY LOWER
!       AND UPPER boundS FOR THE STEP.
!
!     INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!       IF INFO = 1,2,3,4,5, THEN THE STEP HAS BEEN COMPUTED
!       ACCORDING TO ONE OF THE FIVE CASES BELOW. OTHERWISE
!       INFO = 0, AND THIS INDICATES IMPROPER INPUT PARAMETERS.
!
!   SUBPROGRAMS CALLED
!
!     FORTRAN-SUPPLIED ... ABS,MAX,MIN,SQRT
!
!   ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!   JORGE J. MORE', DAVID J. THUENTE
!
  real(dp) :: GAMMA,P,Q,R,S,SGND,stpC,stpF,stpQ,THETA
!
  INFO = 0
!
!  Check the input parameters for errors.
!
  if ((BRACKT .AND. (stp .LE. MIN(STX,STY) .OR. stp .GE. MAX(STX,STY))) .OR. &
      DX*(stp-STX) .GE. 0.0 .OR. stpMAX .LT. stpMIN) return
!
!  Determine if the derivatives have opposite sign.
!
  SGND = DMP*(DX/ABS(DX))
!
!  First case. A higher function value.
!  The minimum is bracketed. If the cubic step is closer
!  to stx than the quadratic step, the cubic step is taken,
!  else the average of the cubic and quadratic steps is taken.
!
  if (FP .GT. FX) then
    INFO = 1
    bound = .true.
    THETA = 3*(FX - FP)/(stp - STX) + DX + DMP
    S = MAX(ABS(THETA),ABS(DX),ABS(DMP))
    GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DMP/S))
    IF (stp .LT. STX) GAMMA = -GAMMA
    P = (GAMMA - DX) + THETA
    Q = ((GAMMA - DX) + GAMMA) + DMP
    R = P/Q
    stpC = STX + R*(stp - STX)
    stpQ = STX + ((DX/((FX-FP)/(stp-STX)+DX))/2)*(stp - STX)
    if (ABS(stpC-STX) .LT. ABS(stpQ-STX)) then
      stpF = stpC
    else
      stpF = stpC + (stpQ - stpC)/2
    endif
    BRACKT = .true.
!
!  Second case. A lower function value and derivatives of
!  opposite sign. The minimum is bracketed. If the cubic
!  step is closer to stx than the quadratic (secant) step,
!  the cubic step is taken, else the quadratic step is taken.
!
  elseif (SGND .LT. 0.0) then
    INFO = 2
    bound = .false.
    THETA = 3*(FX - FP)/(stp - STX) + DX + DMP
    S = MAX(ABS(THETA),ABS(DX),ABS(DMP))
    GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DMP/S))
    IF (stp .GT. STX) GAMMA = -GAMMA
    P = (GAMMA - DMP) + THETA
    Q = ((GAMMA - DMP) + GAMMA) + DX
    R = P/Q
    stpC = stp + R*(STX - stp)
    stpQ = stp + (DMP/(DMP-DX))*(STX - stp)
    if (ABS(stpC-stp) .GT. ABS(stpQ-stp)) then
      stpF = stpC
    else
      stpF = stpQ
    endif
    BRACKT = .true.
!
!  Third case. A lower function value, derivatives of the
!  same sign, and the magnitude of the derivative decreases.
!  The cubic step is only used if the cubic tends to infinity
!  in the direction of the step or if the minimum of the cubic
!  is beyond stp. Otherwise the cubic step is defined to be
!  either stpmin or stpmax. The quadratic (secant) step is also
!  computed and if the minimum is bracketed then the the step
!  closest to stx is taken, else the step farthest away is taken.
!
  elseif (ABS(DMP) .LT. ABS(DX)) then
    INFO = 3
    bound = .true.
    THETA = 3*(FX - FP)/(stp - STX) + DX + DMP
    S = MAX(ABS(THETA),ABS(DX),ABS(DMP))
!
!  The case gamma = 0 only arises if the cubic does not tend to infinity in the direction of the step.
!
    GAMMA = S*SQRT(MAX(0.0D0,(THETA/S)**2 - (DX/S)*(DMP/S)))
    IF (stp .GT. STX) GAMMA = -GAMMA
    P = (GAMMA - DMP) + THETA
    Q = (GAMMA + (DX - DMP)) + GAMMA
    R = P/Q
    if (R .LT. 0.0 .AND. GAMMA .NE. 0.0) then
      stpC = stp + R*(STX - stp)
    elseif (stp .GT. STX) then
      stpC = stpMAX
    else
      stpC = stpMIN
    endif
    stpQ = stp + (DMP/(DMP-DX))*(STX - stp)
    if (BRACKT) then
      if (ABS(stp-stpC) .LT. ABS(stp-stpQ)) then
        stpF = stpC
      else
        stpF = stpQ
      endif
    else
      if (ABS(stp-stpC) .GT. ABS(stp-stpQ)) then
        stpF = stpC
      else
        stpF = stpQ
      endif
    endif
!
!  Fourth case. A lower function value, derivatives of the
!  same sign, and the magnitude of the derivative does
!  not decrease. If the minimum is not bracketed, the step
!  is either stpmin or stpmax, else the cubic step is taken.
!
  else
    INFO = 4
    bound = .false.
    if (BRACKT) then
      THETA = 3*(FP - FY)/(STY - stp) + DY + DMP
      S = MAX(ABS(THETA),ABS(DY),ABS(DMP))
      GAMMA = S*SQRT((THETA/S)**2 - (DY/S)*(DMP/S))
      IF (stp .GT. STY) GAMMA = -GAMMA
      P = (GAMMA - DMP) + THETA
      Q = ((GAMMA - DMP) + GAMMA) + DY
      R = P/Q
      stpC = stp + R*(STY - stp)
      stpF = stpC
    elseif (stp .GT. STX) then
      stpF = stpMAX
    else
      stpF = stpMIN
    endif
  endif
!
!  Update the interval of uncertainty. this update does not depend on the new step or the case analysis above.
!
  if (FP .GT. FX) then
    STY = stp
    FY = FP
    DY = DMP
  else
    if (SGND .LT. 0.0) then
      STY = STX
      FY = FX
      DY = DX
    endif
    STX = stp
    FX = FP
    DX = DMP
  endif
!
!  Compute the new step and safeguard it.
!
  stpF = MIN(stpMAX,stpF)
  stpF = MAX(stpMIN,stpF)
  stp = stpF
  if (BRACKT .AND. bound) then
    IF (STY .GT. STX) then
      stp = MIN(STX+0.66*(STY-STX),stp)
    else
      stp = MAX(STX+0.66*(STY-STX),stp)
    endif
  endif
  return
!
!  Last line of subroutine mcstep.
!
  end
  subroutine MCSRCH3neb(N,X,F,G,S,stp,INFO,WA,niter,nrep,nrepptr,nvar,tangent,fcrep)
!
  use datatypes
  use iochannels
  use optimisation
!
  integer(i4), intent(out) :: info
  integer(i4), intent(in)  :: n
  integer(i4), intent(in)  :: niter
  integer(i4), intent(in)  :: nrep
  integer(i4), intent(in)  :: nrepptr(nrep)
  integer(i4), intent(in)  :: nvar
  real(dp)                 :: F,stp,fcrep(*)
  real(dp)                 :: X(N),G(N),S(N),WA(N),tangent(*)
!
!  SUBROUTINE MCSRCH
!                
!  A slight modification of the subroutine CSRCH of More' and Thuente.
!  The changes are to allow reverse communication, and do not affect
!  the performance of the routine. 
!
!  THE PURPOSE OF MCSRCH IS TO FIND A STEP WHICH SATISFIES
!  A SUFFICIENT DECREASE CONDITION AND A CURVATURE CONDITION.
!
!  AT EACH STAGE THE SUBROUTINE UPDATES AN INTERVAL OF
!  UNCERTAINTY WITH ENDPOINTS STX AND STY. THE INTERVAL OF
!  UNCERTAINTY IS INITIALLY CHOSEN SO THAT IT CONTAINS A
!  MINIMIZER OF THE MODIFIED FUNCTION
!
!    F(X+stp*S) - F(X) - ftol*stp*(GRADF(X)'S).
!
!  IF A STEP IS OBTAINED FOR WHICH THE MODIFIED FUNCTION
!  HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE DERIVATIVE,
!  THEN THE INTERVAL OF UNCERTAINTY IS CHOSEN SO THAT IT
!  CONTAINS A MINIMIZER OF F(X+stp*S).
!
!  THE ALGORITHM IS DESIGNED TO FIND A STEP WHICH SATISFIES
!  THE SUFFICIENT DECREASE CONDITION
!
!    F(X+stp*S) .LE. F(X) + ftol*stp*(GRADF(X)'S),
!
!  AND THE CURVATURE CONDITION
!
!    ABS(GRADF(X+stp*S)'S)) .LE. GTOL*ABS(GRADF(X)'S).
!
!  IF ftol IS LESS THAN GTOL AND IF, FOR EXAMPLE, THE FUNCTION
!  IS boundED BELOW, THEN THERE IS ALWAYS A STEP WHICH SATISFIES
!  BOTH CONDITIONS. IF NO STEP CAN BE FOUND WHICH SATISFIES BOTH
!  CONDITIONS, THEN THE ALGORITHM USUALLY STOPS WHEN ROUNDING
!  ERRORS PREVENT FURTHER PROGRESS. IN THIS CASE stp ONLY
!  SATISFIES THE SUFFICIENT DECREASE CONDITION.
!
!  THE SUBROUTINE STATEMENT IS
!
!    SUBROUTINE MCSRCH(N,X,F,G,S,stp,ftol,XTOL, INFO, WA)
!
!  WHERE
!
!    N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
!      OF VARIABLES.
!
!    X IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!      BASE POINT FOR THE LINE SEARCH. ON OUTPUT IT CONTAINS
!      X + stp*S.
!
!    F IS A VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F
!      AT X. ON OUTPUT IT CONTAINS THE VALUE OF F AT X + stp*S.
!
!    G IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!      GRADIENT OF F AT X. ON OUTPUT IT CONTAINS THE GRADIENT
!      OF F AT X + stp*S.
!
!    S IS AN INPUT ARRAY OF LENGTH N WHICH SPECIFIES THE
!      SEARCH DIRECTION.
!
!    stp IS A NONNEGATIVE VARIABLE. ON INPUT stp CONTAINS AN
!      INITIAL ESTIMATE OF A SATISFACTORY STEP. ON OUTPUT
!      stp CONTAINS THE FINAL ESTIMATE.
!
!    ftol AND GTOL ARE NONNEGATIVE INPUT VARIABLES. (In this reverse
!      communication implementation GTOL is defined in a COMMON
!      statement.) TERMINATION OCCURS WHEN THE SUFFICIENT DECREASE
!      CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
!      SATISFIED.
!
!    XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS
!      WHEN THE RELATIVE width OF THE INTERVAL OF UNCERTAINTY
!      IS AT MOST XTOL.
!
!    stpmin and stpmax ARE NONNEGATIVE INPUT VARIABLES WHICH
!      SPECIFY LOWER AND UPPER boundS FOR THE STEP. (In this reverse
!      communication implementatin they are defined in a COMMON
!      statement).
!
!    INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!
!      INFO = 0  IMPROPER INPUT PARAMETERS.
!
!      INFO =-1  A return IS MADE TO COMPUTE THE FUNCTION AND GRADIENT.
!
!      INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
!                DIRECTIONAL DERIVATIVE CONDITION HOLD.
!
!      INFO = 2  RELATIVE width OF THE INTERVAL OF UNCERTAINTY
!                IS AT MOST XTOL.
!
!      INFO = 4  THE STEP IS AT THE LOWER bound stpMIN.
!
!      INFO = 5  THE STEP IS AT THE UPPER bound stpMAX.
!
!      INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
!                THERE MAY NOT BE A STEP WHICH SATISFIES THE
!                SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
!                TOLERANCES MAY BE TOO SMALL.
!
!      WA IS A WORK ARRAY OF LENGTH N.
!
!  Subprograms called
!
!    MCSTEP
!
!    FORTRAN-SUPPLIED...ABS,MAX,MIN
!
!    ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!    JORGE J. MORE', DAVID J. THUENTE
!
  integer(i4) :: INFOC,J,iflag
  logical     :: BRACKT,STAGE1
  real(dp)    :: DG,DGM,dginit,dgtest,DGX,DGXM,DGY,DGYM, &
                 FINIT,FTEST1,FM,FX,FXM,FY,FYM,P5,P66,STX, &
                 STY,STMIN,STMAX,width,width1,XTRAPF,zero
  DATA P5,P66,XTRAPF,zero /0.5D0,0.66D0,4.0D0,0.0D0/
  INFOC = 1
!
!  Check the input parameters for errors.
!
  if (N .LE. 0 .OR. stp .LE. zero .OR. ftol .LT. zero .OR. &
      lmgtol .LT. zero .OR. XTOL .LT. zero .OR. stepmin .LT. zero &
      .OR. stepmax .LT. stepmin ) return
!
!  Compute the initial gradient in the search direction and check that s is a descent direction.
!
  dginit = zero
  do J = 1,N
    dginit = dginit + G(J)*S(J)
  enddo
  if (dginit .GE. zero) then
    !write(ioout,'(''The search direction is not a descent direction'')')
    info = 6
    return
  endif
!
!  Initialize local variables.
!
  BRACKT = .false.
  STAGE1 = .true.
  FINIT = F
  dgtest = ftol*dginit
  width = stepmax - stepmin
  width1 = width/P5
  do J = 1,N
    WA(J) = X(J)
  enddo
!
!  THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
!  FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
!  THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
!  FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
!  THE INTERVAL OF UNCERTAINTY.
!  THE VARIABLES stp, F, DG CONTAIN THE VALUES OF THE STEP,
!  FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
!
  STX = zero
  FX = FINIT
  DGX = dginit
  STY = zero
  FY = FINIT
  DGY = dginit
!
!  Start of iteration.
!
30 continue
!
!  Set the minimum and maximum steps to correspond to the present interval of uncertainty.
!
  if (BRACKT) then
    STMIN = MIN(STX,STY)
    STMAX = MAX(STX,STY)
  else
    STMIN = STX
    STMAX = stp + XTRAPF*(stp - STX)
  endif
!
!  Force the step to be within the bounds stepmax and stepmin.
!
  stp = MAX(stp,stepmin)
  stp = MIN(stp,stepmax)
!
!  If an unusual termination is to occur then let stp be the lowest point obtained so far.
!
  IF ((BRACKT .AND. (stp .LE. STMIN .OR. stp .GE. STMAX)) &
        .OR. INFOC .EQ. 0 .OR. (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX)) stp = STX
!
!  Evaluate the function and gradient at stp and compute the directional derivative.
!  We return to main program to obtain F and G.
!
  do J = 1, N
    X(J) = WA(J) + stp*S(J)
  enddo
!
  INFO = 0
  iflag = 1_i4
  call functneb(iflag,niter,nrep,nrepptr,nvar,X,fcrep,G,tangent,F)
  DG = zero
  do J = 1, N
    DG = DG + G(J)*S(J)
  enddo
  FTEST1 = FINIT + stp*dgtest
!
!  Test for convergence.
!
  IF ((BRACKT .AND. (stp .LE. STMIN .OR. stp .GE. STMAX)) .OR. INFOC .EQ. 0) INFO = 6
  IF (stp .EQ. stepmax .AND. F .LE. FTEST1 .AND. DG .LE. dgtest) INFO = 5
  IF (stp .EQ. stepmin .AND. (F .GT. FTEST1 .OR. DG .GE. dgtest)) INFO = 4
  IF (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX) INFO = 2
  IF (F .LE. FTEST1 .AND. ABS(DG) .LE. lmgtol*(-dginit)) INFO = 1
!
!  Check for termination.
!
  if (INFO .NE. 0) return
!
!  In the first stage we seek a step for which the modified function has a nonpositive value and nonnegative derivative.
!
  if (STAGE1 .AND. F .LE. FTEST1 .AND. DG .GE. MIN(ftol,lmgtol)*dginit) STAGE1 = .false.
!
!  A modified function is used to predict the step only if
!  we have not obtained a step for which the modified
!  function has a nonpositive function value and nonnegative
!  derivative, and if a lower function value has been
!  obtained but the decrease is not sufficient.
!
  if (STAGE1 .AND. F .LE. FX .AND. F .GT. FTEST1) then
!
!  Define the modified function and derivative values.
!
    FM = F - stp*dgtest
    FXM = FX - STX*dgtest
    FYM = FY - STY*dgtest
    DGM = DG - dgtest
    DGXM = DGX - dgtest
    DGYM = DGY - dgtest
!
!  Call cstep to update the interval of uncertainty and to compute the new step.
!
    call mcstep(STX,FXM,DGXM,STY,FYM,DGYM,stp,FM,DGM,BRACKT,STMIN,STMAX,INFOC)
!
!  Reset the function and gradient values for f.
!
    FX = FXM + STX*dgtest
    FY = FYM + STY*dgtest
    DGX = DGXM + dgtest
    DGY = DGYM + dgtest
  else
!
!  Call mcstep to update the interval of uncertainty and to compute the new step.
!
    call mcstep(STX,FX,DGX,STY,FY,DGY,stp,F,DG,BRACKT,STMIN,STMAX,INFOC)
  endif
!
!  Force a sufficient decrease in the size of the interval of uncertainty.
!
  if (BRACKT) then
    IF (ABS(STY-STX) .GE. P66*width1) stp = STX + P5*(STY - STX)
    width1 = width
    width = ABS(STY-STX)
  endif
!
!  End of iteration.
!
  goto 30
!
!  Last line of subroutine mcsrch.
!
  end
  subroutine MCSRCH3sync(N,X,F,G,S,stp,INFO,WA,niter,nrep,nrepptr,nvar,tangent,fcrep,nlower,nhigher)
!
  use datatypes
  use iochannels
  use optimisation
!
  integer(i4), intent(out) :: info
  integer(i4), intent(in)  :: n
  integer(i4), intent(in)  :: nlower
  integer(i4), intent(in)  :: nhigher
  integer(i4), intent(in)  :: niter
  integer(i4), intent(in)  :: nrep
  integer(i4), intent(in)  :: nrepptr(nrep)
  integer(i4), intent(in)  :: nvar
  real(dp)                 :: F,stp,fcrep(*)
  real(dp)                 :: X(N,*),G(N,*),S(N),WA(N),tangent(*)
!
!  SUBROUTINE MCSRCH
!                
!  A slight modification of the subroutine CSRCH of More' and Thuente.
!  The changes are to allow reverse communication, and do not affect
!  the performance of the routine. 
!
!  THE PURPOSE OF MCSRCH IS TO FIND A STEP WHICH SATISFIES
!  A SUFFICIENT DECREASE CONDITION AND A CURVATURE CONDITION.
!
!  AT EACH STAGE THE SUBROUTINE UPDATES AN INTERVAL OF
!  UNCERTAINTY WITH ENDPOINTS STX AND STY. THE INTERVAL OF
!  UNCERTAINTY IS INITIALLY CHOSEN SO THAT IT CONTAINS A
!  MINIMIZER OF THE MODIFIED FUNCTION
!
!    F(X+stp*S) - F(X) - ftol*stp*(GRADF(X)'S).
!
!  IF A STEP IS OBTAINED FOR WHICH THE MODIFIED FUNCTION
!  HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE DERIVATIVE,
!  THEN THE INTERVAL OF UNCERTAINTY IS CHOSEN SO THAT IT
!  CONTAINS A MINIMIZER OF F(X+stp*S).
!
!  THE ALGORITHM IS DESIGNED TO FIND A STEP WHICH SATISFIES
!  THE SUFFICIENT DECREASE CONDITION
!
!    F(X+stp*S) .LE. F(X) + ftol*stp*(GRADF(X)'S),
!
!  AND THE CURVATURE CONDITION
!
!    ABS(GRADF(X+stp*S)'S)) .LE. GTOL*ABS(GRADF(X)'S).
!
!  IF ftol IS LESS THAN GTOL AND IF, FOR EXAMPLE, THE FUNCTION
!  IS boundED BELOW, THEN THERE IS ALWAYS A STEP WHICH SATISFIES
!  BOTH CONDITIONS. IF NO STEP CAN BE FOUND WHICH SATISFIES BOTH
!  CONDITIONS, THEN THE ALGORITHM USUALLY STOPS WHEN ROUNDING
!  ERRORS PREVENT FURTHER PROGRESS. IN THIS CASE stp ONLY
!  SATISFIES THE SUFFICIENT DECREASE CONDITION.
!
!  THE SUBROUTINE STATEMENT IS
!
!    SUBROUTINE MCSRCH(N,X,F,G,S,stp,ftol,XTOL, INFO, WA)
!
!  WHERE
!
!    N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
!      OF VARIABLES.
!
!    X IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!      BASE POINT FOR THE LINE SEARCH. ON OUTPUT IT CONTAINS
!      X + stp*S.
!
!    F IS A VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F
!      AT X. ON OUTPUT IT CONTAINS THE VALUE OF F AT X + stp*S.
!
!    G IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!      GRADIENT OF F AT X. ON OUTPUT IT CONTAINS THE GRADIENT
!      OF F AT X + stp*S.
!
!    S IS AN INPUT ARRAY OF LENGTH N WHICH SPECIFIES THE
!      SEARCH DIRECTION.
!
!    stp IS A NONNEGATIVE VARIABLE. ON INPUT stp CONTAINS AN
!      INITIAL ESTIMATE OF A SATISFACTORY STEP. ON OUTPUT
!      stp CONTAINS THE FINAL ESTIMATE.
!
!    ftol AND GTOL ARE NONNEGATIVE INPUT VARIABLES. (In this reverse
!      communication implementation GTOL is defined in a COMMON
!      statement.) TERMINATION OCCURS WHEN THE SUFFICIENT DECREASE
!      CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
!      SATISFIED.
!
!    XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS
!      WHEN THE RELATIVE width OF THE INTERVAL OF UNCERTAINTY
!      IS AT MOST XTOL.
!
!    stpmin and stpmax ARE NONNEGATIVE INPUT VARIABLES WHICH
!      SPECIFY LOWER AND UPPER boundS FOR THE STEP. (In this reverse
!      communication implementatin they are defined in a COMMON
!      statement).
!
!    INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!
!      INFO = 0  IMPROPER INPUT PARAMETERS.
!
!      INFO =-1  A return IS MADE TO COMPUTE THE FUNCTION AND GRADIENT.
!
!      INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
!                DIRECTIONAL DERIVATIVE CONDITION HOLD.
!
!      INFO = 2  RELATIVE width OF THE INTERVAL OF UNCERTAINTY
!                IS AT MOST XTOL.
!
!      INFO = 4  THE STEP IS AT THE LOWER bound stpMIN.
!
!      INFO = 5  THE STEP IS AT THE UPPER bound stpMAX.
!
!      INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
!                THERE MAY NOT BE A STEP WHICH SATISFIES THE
!                SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
!                TOLERANCES MAY BE TOO SMALL.
!
!      WA IS A WORK ARRAY OF LENGTH N.
!
!  Subprograms called
!
!    MCSTEP
!
!    FORTRAN-SUPPLIED...ABS,MAX,MIN
!
!    ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!    JORGE J. MORE', DAVID J. THUENTE
!
  integer(i4) :: INFOC,J,iflag
  logical     :: BRACKT,STAGE1
  real(dp)    :: DG,DGM,dginit,dgtest,DGX,DGXM,DGY,DGYM, &
                 FINIT,FTEST1,FM,FX,FXM,FY,FYM,P5,P66,STX, &
                 STY,STMIN,STMAX,width,width1,XTRAPF,zero
  DATA P5,P66,XTRAPF,zero /0.5D0,0.66D0,4.0D0,0.0D0/
  INFOC = 1
!
!  Check the input parameters for errors.
!
  if (N .LE. 0 .OR. stp .LE. zero .OR. ftol .LT. zero .OR. &
      lmgtol .LT. zero .OR. XTOL .LT. zero .OR. stepmin .LT. zero &
      .OR. stepmax .LT. stepmin ) return
!
!  Compute the initial gradient in the search direction and check that s is a descent direction.
!
  dginit = zero
  do j = 1,N
    dginit = dginit + G(j,nlower)*S(j)
  enddo
  if (dginit.ge.zero) then
    !write(ioout,'(''The search direction is not a descent direction'')')
    info = 6
    return
  endif
!
!  Initialize local variables.
!
  BRACKT = .false.
  STAGE1 = .true.
  FINIT = F
  dgtest = ftol*dginit
  width = stepmax - stepmin
  width1 = width/P5
  do j = 1,N
    WA(j) = X(j,nlower)
  enddo
!
!  THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
!  FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
!  THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
!  FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
!  THE INTERVAL OF UNCERTAINTY.
!  THE VARIABLES stp, F, DG CONTAIN THE VALUES OF THE STEP,
!  FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
!
  STX = zero
  FX = FINIT
  DGX = dginit
  STY = zero
  FY = FINIT
  DGY = dginit
!
!  Start of iteration.
!
30 continue
!
!  Set the minimum and maximum steps to correspond to the present interval of uncertainty.
!
  if (BRACKT) then
    STMIN = MIN(STX,STY)
    STMAX = MAX(STX,STY)
  else
    STMIN = STX
    STMAX = stp + XTRAPF*(stp - STX)
  endif
!
!  Force the step to be within the bounds stepmax and stepmin.
!
  stp = MAX(stp,stepmin)
  stp = MIN(stp,stepmax)
!
!  If an unusual termination is to occur then let stp be the lowest point obtained so far.
!
  IF ((BRACKT .AND. (stp .LE. STMIN .OR. stp .GE. STMAX)) &
        .OR. INFOC .EQ. 0 .OR. (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX)) stp = STX
!
!  Evaluate the function and gradient at stp and compute the directional derivative.
!  We return to main program to obtain F and G.
!
  do j = 1,N
    X(j,nlower) = WA(j) + stp*S(j)
  enddo
!
  INFO = 0
  iflag = 1_i4
  call functsync(iflag,niter,nrep,nrepptr,nvar,X,fcrep,G,tangent,F,nlower,nhigher)
  DG = zero
  do j = 1,N
    DG = DG + G(j,nlower)*S(j)
  enddo
  FTEST1 = FINIT + stp*dgtest
!
!  Test for convergence.
!
  if ((BRACKT .and. (stp.le.STMIN.or.stp.ge.STMAX)) .or. INFOC.eq.0) INFO = 6
  if (stp.eq.stepmax .and. F.le.FTEST1 .and. DG.le.dgtest) INFO = 5
  if (stp.eq.stepmin .and. (F.gt.FTEST1 .or. DG.ge.dgtest)) INFO = 4
  if (BRACKT .and. STMAX-STMIN.le.XTOL*STMAX) INFO = 2
  if (F.le.FTEST1 .and. abs(DG).le.lmgtol*(-dginit)) INFO = 1
!
!  Check for termination.
!
  if (INFO.ne.0) return
!
!  In the first stage we seek a step for which the modified function has a nonpositive value and nonnegative derivative.
!
  if (STAGE1 .and. F.le.FTEST1 .and. DG .ge. min(ftol,lmgtol)*dginit) STAGE1 = .false.
!
!  A modified function is used to predict the step only if
!  we have not obtained a step for which the modified
!  function has a nonpositive function value and nonnegative
!  derivative, and if a lower function value has been
!  obtained but the decrease is not sufficient.
!
  if (STAGE1 .and. F .le. FX .and. F .gt. FTEST1) then
!
!  Define the modified function and derivative values.
!
    FM = F - stp*dgtest
    FXM = FX - STX*dgtest
    FYM = FY - STY*dgtest
    DGM = DG - dgtest
    DGXM = DGX - dgtest
    DGYM = DGY - dgtest
!
!  Call cstep to update the interval of uncertainty and to compute the new step.
!
    call mcstep(STX,FXM,DGXM,STY,FYM,DGYM,stp,FM,DGM,BRACKT,STMIN,STMAX,INFOC)
!
!  Reset the function and gradient values for f.
!
    FX = FXM + STX*dgtest
    FY = FYM + STY*dgtest
    DGX = DGXM + dgtest
    DGY = DGYM + dgtest
  else
!
!  Call mcstep to update the interval of uncertainty and to compute the new step.
!
    call mcstep(STX,FX,DGX,STY,FY,DGY,stp,F,DG,BRACKT,STMIN,STMAX,INFOC)
  endif
!
!  Force a sufficient decrease in the size of the interval of uncertainty.
!
  if (BRACKT) then
    if (abs(STY-STX).ge.P66*width1) stp = STX + P5*(STY - STX)
    width1 = width
    width = abs(STY-STX)
  endif
!
!  End of iteration.
!
  goto 30
!
!  Last line of subroutine mcsrch.
!
  end
