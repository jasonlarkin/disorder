module m_conjgr

  use datatypes, only: dp, i4

  implicit none

  public :: conjgr
      
  private

CONTAINS

  subroutine conjgr(N,X,G,DXMAX,GTOL,CNTROL,H)

!DIRECTS A CONJUGATE-GRADIENT MINIMIZATION OF A FUNCTION WHICH
! IS EVALUATED BY THE CALLING PROGRAM.
!  N     : INPUT SPACE DIMENSIONALITY
!  X     : INPUT POSITION AT WHICH GRADIENT HAS BEEN EVALUATED AND
!          OUTPUT NEW POSITION AT WHICH GRADIENT MUST BE EVALUATED NEXT
!  G     : INPUT GRADIENT (WITH A MINUS SIGN, OR ACTIVATE A LINE BELOW)
!  DXMAX : INPUT MAXIMUM ALLOWED DISPLACEMENT IN EACH COORDINATE
!  GTOL  : INPUT MAXIMUM FINAL VALUE OF EACH GRADIENT COMPONENT
!  CNTROL: CONTROL ARRAY. FIRST ELEMENT MUST BE MADE ZERO BEFORE
!          FIRST CALL. IF IT IS ZERO ON OUTPUT, MINIMIZATION IS
!          CONVERGED. OTHERWISE, CALCULATE GRADIENT AT THE NEW
!          POSITION AND CALL AGAIN THIS ROUTINE. DO NOT MODIFY ANY
!          ARGUMENT OTHER THAN G BETWEEN CALLS.
!  H     : AUXILIARY ARRAY WHICH MUST NOT BE MODIFIED BETWEEN CALLS
!  IOPT  : PARAMETER BELOW WHICH DETERMINES METHOD USED AND AUXILIARY
!          STORAGE REQUIRED: IOP=1 => FLETCHER-REEVES. IOPT=2 =>
!          POLAK-RIBIERE. DETAILS IN SECT. 10.6 OF 'NUMERICAL RECIPES'
! WRITTEN BY J.SOLER. JAN/91. BASED ON ROUTINES IN 'NUMERICAL RECIPES'

! Local parameters
  integer(i4),  parameter     :: iopt = 2

! Passed variables
  integer(i4),  intent(in)    :: n
  real(dp),     intent(in)    :: dxmax, gtol
  real(dp),     intent(in)    :: G(N)
  real(dp),     intent(inout) :: X(N),H(N,IOPT),CNTROL(0:19)

! Local variables
  real(dp)                    :: gmax, gg, gamma
  integer(i4)                 :: i, j

  real(dp)                    :: ddot
  external  ddot

! If gradient is smaller than tolerence, return
  GMAX=ABS(G(1))
  do J=1,N
    GMAX=MAX(GMAX,ABS(G(J)))
  enddo
  if (GMAX.LE.GTOL) then
    CNTROL(0)=0
    GOTO 60
  endif

! First-call initializations
  if (NINT(CNTROL(0)).EQ.0) then
    do I=1,IOPT
      do J=1,N
        H(J,I)=G(J)
      enddo
    enddo
    CNTROL(0)=1
    CNTROL(1)=1
    CNTROL(2)=ddot(n,G,1,G,1)
    CNTROL(10)=0
    CNTROL(18)=DXMAX
  endif

! Line minimization is always called
40 call linmin(N,X,H,G,DXMAX,CNTROL(10))

! If line minimization is finished, find new line direction
  if (NINT(CNTROL(10)).EQ.0) then
    GG=ddot(n,G,1,G,1)
    if (IOPT.EQ.2) GG=GG-ddot(n,G,1,H(1,2),1)
    GAMMA=GG/CNTROL(2)
    do J=1,N
      H(J,1)=G(J)+GAMMA*H(J,1)
      IF (IOPT.EQ.2) H(J,2)=G(J)
    enddo
    CNTROL(1)=CNTROL(1)+1
    CNTROL(2)=ddot(n,G,1,G,1)
!*  WRITE(6,'(A,I4,F15.6)') ' CONJGR: NEW LINE DIRECTION. N,DX=',N,CNTROL(18)
    goto 40
  endif

60 continue
  end subroutine conjgr
!------------------------------------------------------------
  subroutine linmin(n,XVEC,HVEC,GVEC,DXMAX,CNTROL)

  integer(i4), intent(in) :: n
  real(dp)                ::  XVEC(N),HVEC(N),GVEC(N),CNTROL(0:9)
  real(dp),    intent(in) :: dxmax

  real(dp), parameter :: FACTOR = 1.6_dp

  integer(i4)  :: i, icntrl
  real(dp)     :: x1, x2, y1, y2, x0, hmod, hmax, dx, x, y

  real(dp)     :: ddot
  external  ddot

! TRANSLATE CONTROL PARAMETERS
  ICNTRL=NINT(CNTROL(0))
  X1=CNTROL(1)
  X2=CNTROL(2)
  Y1=CNTROL(3)
  Y2=CNTROL(4)
  X0=CNTROL(5)
  HMOD=CNTROL(6)
  HMAX=CNTROL(7)
  DX=CNTROL(8)
  X=X0
  Y=ddot(n,GVEC,1,HVEC,1)
! WRITE(6,'(A,I4,2F12.6)') ' LINMIN: ICNTRL,X,Y=',ICNTRL,X,Y

  IF (ICNTRL.EQ.0) THEN
!   INITIALIZE X1,Y1 ON FIRST CALL
    X1=0.D0
    Y1=Y
!   PREPARE SECOND POINT
    ICNTRL=1
    X0=0.D0
    IF (DX.EQ.0.D0) DX=DXMAX
    HMOD=SQRT(ddot(n,HVEC,1,HVEC,1))
    HMAX=0.D0
    DO I=1,N
      HMAX=MAX(HMAX,ABS(HVEC(I)))
    enddo
    X=MIN(DX/HMOD,DXMAX/HMAX)
    GOTO 20
  ELSEIF (ICNTRL.EQ.1) THEN
!   INITIALIZE X2,Y2 ON SECOND CALL
    X2=X
    Y2=Y
    ICNTRL=2
  ELSEIF (ICNTRL.EQ.2) THEN
!   SHIFT INTERVAL USING NEW POINT
    X1=X2
    Y1=Y2
    X2=X
    Y2=Y
  ELSEIF (ICNTRL.EQ.3) THEN
!   IF ROOT WAS FOUND IN LAST CALL, ALL IS DONE NOW
    ICNTRL=0
    GOTO 20
  ENDIF

  IF (Y2.GT.0.D0) THEN
!   ROOT NOT BRACKETED YET. TRY NEW RIGHT BRACKET
    X=X2+MIN(FACTOR*(X2-X1),DXMAX/HMAX)
  ELSE
!   INTERPOLATE FOR ROOT AND RETURN TO CALCULATE LAST GRADIENT
    X=(X1*Y2-X2*Y1)/(Y2-Y1)
    ICNTRL=3
  ENDIF

! STORE CONTROL PARAMETERS AND SET NEW POINT
 20 CNTROL(0)=ICNTRL
    CNTROL(1)=X1
    CNTROL(2)=X2
    CNTROL(3)=Y1
    CNTROL(4)=Y2
    CNTROL(5)=X
    CNTROL(6)=HMOD
    CNTROL(7)=HMAX
    CNTROL(8)=ABS(X)*HMOD
    DO I=1,N
      XVEC(I)=XVEC(I)+HVEC(I)*(X-X0)
    enddo
  END subroutine linmin

end module m_conjgr
