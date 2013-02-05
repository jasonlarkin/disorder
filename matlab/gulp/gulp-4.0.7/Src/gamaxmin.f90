  subroutine gamaxmin(xc,xmax,xmin,imode)
!
!  Create maximum and minimum limits for gafit or gaopt
!  imode = indicates whether call is from gafit or gaopt
!          1 => gafit
!          2 => gaopt
!
!  10/98 Codes for fitting variables simplified
!  11/06 Error in addressing of xmaxcfg and xmincfg fixed
!   1/09 Use of nfvar for two-body potentials modified
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
!  Copyright Curtin University 2009
!  
!  Julian Gale, NRI, Curtin University, January 2009
!
  use current
  use fitting
  use genetic,   only : xmaxcfg, xmincfg
  use parallel
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)        :: imode
  real(dp)           :: xc(*)
  real(dp)           :: xmax(*)
  real(dp)           :: xmin(*)
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: ind
  integer(i4)        :: nf
  integer(i4)        :: np
  integer(i4)        :: nt
  integer(i4)        :: nv
!
  if (nbsm.gt.0) then
    call outerror('breathing shells not allowed with GA fitting',0_i4)
    call stopnow('gamaxmin')
  endif
  if (imode.eq.1) then
    do i = 1,nfit
      nt = nftyp(i)
      nf = nfpot(i)
      nv = nfvar(i)
      if (nt.eq.2.and.nv.le.5) then
        np = nptype(nf)
        ind = (np-1)*4 + nf
        if (ind.eq.2.or.ind.eq.22.or.ind.eq.38) then
          if (xmin(i).eq.0.0_dp) then
            xmin(i) = 0.5_dp*xc(i)
          else
            xmin(i) = xmin(i)/scale(i)
          endif
        else
          if (xmin(i).ne.0.0_dp) then
            xmin(i) = xmin(i)/scale(i)
          endif
        endif
        if (xmax(i).eq.0.0_dp) then
          xmax(i) = 2.0_dp*xc(i)
        else
          xmax(i) = xmax(i)/scale(i)
        endif
      else
        if (xmax(i).eq.0.0_dp) then
          xmax(i) = 2.0_dp*xc(i)
        else
          xmax(i) = xmax(i)/scale(i)
        endif
        if (xmin(i).ne.0.0_dp) then
          xmin(i) = xmin(i)/scale(i)
        endif
      endif
    enddo
  else
    do i = 1,nvar
      if (iopt(i).gt.nstrains.and.iopt(i).le.nstrains+3*nasym) then
        ind = iopt(i) - (nstrains+1)
        ii = ind/3
        ind = ind - 3*ii + 1
        if (xmax(i).eq.0.0_dp) xmax(i) = xmaxcfg(ind,ncf)
        if (xmin(i).eq.0.0_dp) xmin(i) = xmincfg(ind,ncf)
      else
        if (xmax(i).eq.0.0_dp) xmax(i) = 1.5_dp
      endif
    enddo
  endif
!
  return
  end
