  subroutine outfitcon
!
!  Apply constraints to fitted parameters
!
!   9/05 Powers added for fitting constraints
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
!  Julian Gale, NRI, Curtin University, September 2005
!
  use fitting
  use iochannels
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: nff
  integer(i4) :: nfv
  integer(i4) :: nfv1
  integer(i4) :: nfv2
  integer(i4) :: nl
  integer(i4) :: nm
  integer(i4) :: nn
  real(dp)    :: sf
  real(dp)    :: sv
  real(dp)    :: rtmp
  real(dp)    :: tmp
!**********************
!  Constraint output  *
!**********************
  nl = 0
  nm = 0
  nn = 0
  do i = 1,nfcon
    if (nfcotyp(i).eq.1) then
      if (fconpower(i).eq.1.0_dp) then
        nl = nl + 1
      else
        nn = nn + 1
      endif
    else
      nm = nm + 1
    endif
  enddo
  write(ioout,'(''  Constraints on fitted variables : '',/)')
!
!  Linear constraints
!
  if (nl.gt.0) then
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Linear constraints : '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Constraint no.      Unconstrained     Constrained    Coefficient    Offset'')')
    write(ioout,'(''                         Variable         Variable'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nfcon
      if (nfcotyp(i).eq.1.and.fconpower(i).eq.1.0_dp) then
        nfv = nfcvar(i)
        nff = nfcfix(i)
        sv = scale(nfv)
        sf = scale(nff)
        write(ioout,'(6x,i4,16x,i4,14x,i4,8x,f10.5,2x,f11.4)') i,nfv,nff,sf*fconco(i)/sv,sf*fconadd(i)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Non-linear constraints
!
  if (nn.gt.0) then
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Non-linear constraints : '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Constraint  Unconstrained   Constrained   Coefficient    Offset     Power     '')')
    write(ioout,'(''      no.        Variable      Variable  '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nfcon
      if (nfcotyp(i).eq.1.and.fconpower(i).ne.1.0_dp) then
        nfv = nfcvar(i)
        nff = nfcfix(i)
        sv = scale(nfv)
        sf = scale(nff)
        write(ioout,'(2x,i6,8x,i6,8x,i6,8x,f10.5,2x,f11.4,2x,f8.4)') i,nfv,nff,fconco(i)*(sf/sv**fconpower(i)), &
          sf*fconadd(i),fconpower(i)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Geometric mean constraints
!
  if (nm.gt.0) then
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Geometric mean constraints : '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Constraint no.    Unconstrained     Unconstrained     Constrained     Coeff'')')
    write(ioout,'(''                     Variable  1       Variable  2       Variable'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nfcon
      if (nfcotyp(i).eq.2) then
        nfv1 = nfcvar(i)
        nfv2 = nint(fconadd(i))
        nff = nfcfix(i)
        if (nfv1.eq.nfv2) then
          tmp = scale(nfv1)
        else
          tmp = scale(nfv1)*scale(nfv2)
        endif
        rtmp = scale(nff)/sqrt(abs(tmp))
        write(ioout,'(6x,i4,14x,i4,14x,i4,14x,i4,6x,f9.4)') i,nfv1,nfv2,nff,rtmp*fconco(i)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
  return
  end
