  subroutine reaxFF_bo(i,j,nspeci,nspecj,ni,nj,deltapi,deltapj,nneigh,BOp,BOp_pi,BOp_pipi, &
                       d1BOp,d1BOp_pi,d1BOp_pipi,BOij_s,BOij_pi,BOij_pipi, &
                       d1BOi_s,d1BOi_pi,d1BOi_pipi,d1BOj_s,d1BOj_pi,d1BOj_pipi,lgrad1)
!
!  Calculates the bond order terms and derivatives
!
!  On entry : 
!
!  lgrad1          = if .true. calculate the first derivatives
!
!  On exit :
!
!  BOij_s          = corrected sigma bond order
!  BOij_pi         = corrected pi bond order
!  BOij_pipi       = corrected pi-pi bond order
!  + corresponding derivatives as required
!
!  10/07 Created from reaxff
!   6/09 Option to use old f1 instead of f1*f1 form added
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use datatypes
  use current
  use iochannels
  use neighbours
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: i
  integer(i4), intent(in)                        :: j
  integer(i4), intent(in)                        :: nspeci
  integer(i4), intent(in)                        :: nspecj
  integer(i4), intent(in)                        :: ni
  integer(i4), intent(in)                        :: nj
  integer(i4), intent(in)                        :: nneigh(*)
  real(dp),    intent(in)                        :: deltapi
  real(dp),    intent(in)                        :: deltapj
  real(dp),    intent(in)                        :: BOp(maxneigh,*)
  real(dp),    intent(in)                        :: BOp_pi(maxneigh,*)
  real(dp),    intent(in)                        :: BOp_pipi(maxneigh,*)
  real(dp),    intent(in)                        :: d1BOp(maxneigh,*)
  real(dp),    intent(in)                        :: d1BOp_pi(maxneigh,*)
  real(dp),    intent(in)                        :: d1BOp_pipi(maxneigh,*)
  real(dp),    intent(out)                       :: BOij_s
  real(dp),    intent(out)                       :: BOij_pi
  real(dp),    intent(out)                       :: BOij_pipi
  real(dp),    intent(out)                       :: d1BOi_s(*)
  real(dp),    intent(out)                       :: d1BOi_pi(*)
  real(dp),    intent(out)                       :: d1BOi_pipi(*)
  real(dp),    intent(out)                       :: d1BOj_s(*)
  real(dp),    intent(out)                       :: d1BOj_pi(*)
  real(dp),    intent(out)                       :: d1BOj_pipi(*)
  logical,     intent(in)                        :: lgrad1
!
!  Local variables
!
  integer(i4)                                    :: ind
  integer(i4)                                    :: k
  integer(i4)                                    :: l
  integer(i4)                                    :: n
  integer(i4)                                    :: nn
  integer(i4)                                    :: nneighi1
  integer(i4)                                    :: nneighj1
  integer(i4)                                    :: nneighi2
  integer(i4)                                    :: nneighj2
  integer(i4)                                    :: nneighi22
  integer(i4)                                    :: nneighj22
  integer(i4)                                    :: status
  logical                                        :: lf1squared
  real(dp)                                       :: BOij
  real(dp)                                       :: BOpij
  real(dp)                                       :: BOpij_s
  real(dp)                                       :: BOpij_pi
  real(dp),    dimension(:),   allocatable, save :: df1dri
  real(dp),    dimension(:),   allocatable, save :: df1drj
  real(dp),    dimension(:),   allocatable, save :: df4dri
  real(dp),    dimension(:),   allocatable, save :: df5drj
  real(dp),    dimension(:),   allocatable, save :: d2f1dr2i
  real(dp),    dimension(:),   allocatable, save :: d2f1dr2ij
  real(dp),    dimension(:),   allocatable, save :: d2f1dr2j
  real(dp),    dimension(:),   allocatable, save :: d2f4dr2i
  real(dp),    dimension(:),   allocatable, save :: d2f5dr2j
  real(dp)                                       :: dBOpijdr
  real(dp)                                       :: dBOpikdr
  real(dp)                                       :: dBOpildr
  real(dp)                                       :: dBOpjkdr
  real(dp)                                       :: dBOpjldr
  real(dp)                                       :: df1ddpi
  real(dp)                                       :: df1ddpj
  real(dp)                                       :: df4ddpi
  real(dp)                                       :: df4ddpj
  real(dp)                                       :: df4dbo
  real(dp)                                       :: df5ddpj
  real(dp)                                       :: df5dbo
  real(dp)                                       :: d2f1ddpi2
  real(dp)                                       :: d2f1ddpiddpj
  real(dp)                                       :: d2f1ddpj2
  real(dp)                                       :: d2f4ddpi2
  real(dp)                                       :: d2f4ddpidbo
  real(dp)                                       :: d2f4dbo2
  real(dp)                                       :: d2f5ddpj2
  real(dp)                                       :: d2f5ddpjdbo
  real(dp)                                       :: d2f5dbo2
  real(dp)                                       :: f1
  real(dp)                                       :: f4
  real(dp)                                       :: f5
  real(dp)                                       :: tp
  real(dp)                                       :: dtpdr
  real(dp)                                       :: d2tpdr2
  real(dp)                                       :: d3tpdr3
!
!  Set flag for f1squared or not
!
!  Original version didn't have square, but new one does
!
  lf1squared = .true.
!
!  Set total number of distances for neighbours of i
!
  nneighi1   = nneigh(i)*(nneigh(i) + 1)/2 
  nneighi2   = nneigh(i) + nneighi1
  nneighi22  = nneighi2*(nneighi2 + 1)/2
!
!  Set total number of distances for neighbours of j
!
  nneighj1   = nneigh(j)*(nneigh(j) + 1)/2
  nneighj2   = nneigh(j) + nneighj1
  nneighj22  = nneighj2*(nneighj2 + 1)/2
!
!  Branch according to which corrections need to be applied to the bond order
!
  if (nspeci.gt.nspecj) then
    ind = nspeci*(nspeci - 1)/2 + nspecj
  else
    ind = nspecj*(nspecj - 1)/2 + nspeci
  endif
  if (lreaxFFbocorrect(1,ind).and.lreaxFFbocorrect(2,ind)) then
!***********************************
!  Both corrections to be applied  *
!***********************************
!
!  Allocate local memory
!
    if (lgrad1) then
      allocate(df1dri(nneigh(i)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df1dri')
      allocate(df1drj(nneigh(j)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df1drj')
      allocate(df4dri(nneigh(i)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df4dri')
      allocate(df5drj(nneigh(j)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df5drj')
    endif
!
!  Calculate Bij and Bji - loop over all other neighbours
!
    if (lgrad1) then
      d1BOi_s(1:nneighi2) = 0.0_dp
      d1BOi_pi(1:nneighi2) = 0.0_dp
      d1BOi_pipi(1:nneighi2) = 0.0_dp
      d1BOj_s(1:nneighj2) = 0.0_dp
      d1BOj_pi(1:nneighj2) = 0.0_dp
      d1BOj_pipi(1:nneighj2) = 0.0_dp
      df1dri(1:nneigh(i)) = 0.0_dp
      df1drj(1:nneigh(j)) = 0.0_dp
      df4dri(1:nneigh(i)) = 0.0_dp
      df5drj(1:nneigh(j)) = 0.0_dp
    endif
!
!  Set local variable for bond order prime of currrent bond
!
    BOpij_s  = BOp(ni,i)
    BOpij_pi = BOp_pi(ni,i) + BOp_pipi(ni,i)
    BOpij = BOpij_s + BOpij_pi
!
!  Compute functions of delta'
!
    call reaxFF_f1(nspeci,nspecj,deltapi,deltapj,f1,df1ddpi,df1ddpj,d2f1ddpi2,d2f1ddpiddpj,d2f1ddpj2,lgrad1,.false.)
    call reaxFF_f45(nspeci,nspecj,BOpij,deltapi,f4,df4ddpi,df4dbo,d2f4ddpi2,d2f4ddpidbo,d2f4dbo2,lgrad1,.false.)
    call reaxFF_f45(nspecj,nspeci,BOpij,deltapj,f5,df5ddpj,df5dbo,d2f5ddpj2,d2f5ddpjdbo,d2f5dbo2,lgrad1,.false.)
!
!  Calculate corrected bond orders for i-j
!
    BOij = BOpij*f1*f4*f5
    if (lf1squared) then
      BOij_pi   = BOp_pi(ni,i)*f1*f1*f4*f5
      BOij_pipi = BOp_pipi(ni,i)*f1*f1*f4*f5
    else
      BOij_pi   = BOp_pi(ni,i)*f1*f4*f5
      BOij_pipi = BOp_pipi(ni,i)*f1*f4*f5
    endif
    BOij_s = BOij - BOij_pi - BOij_pipi
!
!  Compute derivatives of combined bond order term
!
    if (lgrad1) then
      if (lf1squared) then
        BOpij_s = BOpij_s + (1.0_dp - f1)*BOpij_pi
      endif
!
!  First derivatives of f1 
!
      do k = 1,nneigh(i)
        dBOpijdr = (d1BOp(k,i) + d1BOp_pi(k,i) + d1BOp_pipi(k,i))
        df1dri(k) = df1dri(k) + df1ddpi*dBOpijdr
      enddo
      do k = 1,nneigh(j)
        dBOpijdr = (d1BOp(k,j) + d1BOp_pi(k,j) + d1BOp_pipi(k,j))
        df1drj(k) = df1drj(k) + df1ddpj*dBOpijdr
      enddo
!
!  First derivatives of f4 
!
      dBOpijdr = (d1BOp(ni,i) + d1BOp_pi(ni,i) + d1BOp_pipi(ni,i))
      df4dri(ni) = df4dri(ni) + df4dbo*dBOpijdr
      do k = 1,nneigh(i)
        dBOpijdr = (d1BOp(k,i) + d1BOp_pi(k,i) + d1BOp_pipi(k,i))
        df4dri(k) = df4dri(k) + df4ddpi*dBOpijdr
      enddo
!
!  First derivatives of f5 
!
      dBOpijdr = (d1BOp(nj,j) + d1BOp_pi(nj,j) + d1BOp_pipi(nj,j))
      df5drj(nj) = df5drj(nj) + df5dbo*dBOpijdr
      do k = 1,nneigh(j)
        dBOpijdr = (d1BOp(k,j) + d1BOp_pi(k,j) + d1BOp_pipi(k,j))
        df5drj(k) = df5drj(k) + df5ddpj*dBOpijdr
      enddo
!
!  Derivatives of BO' 
!
      dBOpijdr = (d1BOp(ni,i) + d1BOp_pi(ni,i) + d1BOp_pipi(ni,i))
      if (lf1squared) then
        d1BOi_s(ni)    = d1BOi_s(ni)    + 0.5_dp*(d1BOp(ni,i) + (1.0_dp - f1)*(d1BOp_pi(ni,i) + d1BOp_pipi(ni,i)))*f1*f4*f5
        d1BOi_pi(ni)   = d1BOi_pi(ni)   + 0.5_dp*d1BOp_pi(ni,i)*f1*f1*f4*f5
        d1BOi_pipi(ni) = d1BOi_pipi(ni) + 0.5_dp*d1BOp_pipi(ni,i)*f1*f1*f4*f5
        d1BOj_s(nj)    = d1BOj_s(nj)    + 0.5_dp*(d1BOp(nj,j) + (1.0_dp - f1)*(d1BOp_pi(nj,j) + d1BOp_pipi(nj,j)))*f1*f4*f5
        d1BOj_pi(nj)   = d1BOj_pi(nj)   + 0.5_dp*d1BOp_pi(nj,j)*f1*f1*f4*f5
        d1BOj_pipi(nj) = d1BOj_pipi(nj) + 0.5_dp*d1BOp_pipi(nj,j)*f1*f1*f4*f5
!
!  Derivatives of f1, f4 & f5 - note that derivatives of BOp are the same as those of deltap with respect to the same bond
!
        do k = 1,nneigh(i)
          d1BOi_s(k)    = d1BOi_s(k)    + f4*f5*(BOpij_s - f1*BOpij_pi)*df1dri(k)
          d1BOi_pi(k)   = d1BOi_pi(k)   + 2.0_dp*f4*f5*f1*BOp_pi(ni,i)*df1dri(k)
          d1BOi_pipi(k) = d1BOi_pipi(k) + 2.0_dp*f4*f5*f1*BOp_pipi(ni,i)*df1dri(k)
        enddo
        do k = 1,nneigh(j)
          d1BOj_s(k)    = d1BOj_s(k)    + f4*f5*(BOpij_s - f1*BOpij_pi)*df1drj(k)
          d1BOj_pi(k)   = d1BOj_pi(k)   + 2.0_dp*f4*f5*f1*BOp_pi(ni,i)*df1drj(k)
          d1BOj_pipi(k) = d1BOj_pipi(k) + 2.0_dp*f4*f5*f1*BOp_pipi(ni,i)*df1drj(k)
        enddo
        do k = 1,nneigh(i)
          d1BOi_s(k)    = d1BOi_s(k)    + f1*f5*BOpij_s*df4dri(k)
          d1BOi_pi(k)   = d1BOi_pi(k)   + f1*f5*f1*BOp_pi(ni,i)*df4dri(k)
          d1BOi_pipi(k) = d1BOi_pipi(k) + f1*f5*f1*BOp_pipi(ni,i)*df4dri(k)
        enddo
        do k = 1,nneigh(j)
          d1BOj_s(k)    = d1BOj_s(k)    + f1*f4*BOpij_s*df5drj(k)
          d1BOj_pi(k)   = d1BOj_pi(k)   + f1*f4*f1*BOp_pi(ni,i)*df5drj(k)
          d1BOj_pipi(k) = d1BOj_pipi(k) + f1*f4*f1*BOp_pipi(ni,i)*df5drj(k)
        enddo
      else
        d1BOi_s(ni)    = d1BOi_s(ni)    + 0.5_dp*d1BOp(ni,i)*f1*f4*f5
        d1BOi_pi(ni)   = d1BOi_pi(ni)   + 0.5_dp*d1BOp_pi(ni,i)*f1*f4*f5
        d1BOi_pipi(ni) = d1BOi_pipi(ni) + 0.5_dp*d1BOp_pipi(ni,i)*f1*f4*f5
        d1BOj_s(nj)    = d1BOj_s(nj)    + 0.5_dp*d1BOp(nj,j)*f1*f4*f5
        d1BOj_pi(nj)   = d1BOj_pi(nj)   + 0.5_dp*d1BOp_pi(nj,j)*f1*f4*f5
        d1BOj_pipi(nj) = d1BOj_pipi(nj) + 0.5_dp*d1BOp_pipi(nj,j)*f1*f4*f5
!
!  Derivatives of f1, f4 & f5 - note that derivatives of BOp are the same as those of deltap with respect to the same bond
!
        do k = 1,nneigh(i)
          d1BOi_s(k)    = d1BOi_s(k)    + f4*f5*BOp(ni,i)*df1dri(k)
          d1BOi_pi(k)   = d1BOi_pi(k)   + f4*f5*BOp_pi(ni,i)*df1dri(k)
          d1BOi_pipi(k) = d1BOi_pipi(k) + f4*f5*BOp_pipi(ni,i)*df1dri(k)
        enddo
        do k = 1,nneigh(j)
          d1BOj_s(k)    = d1BOj_s(k)    + f4*f5*BOp(ni,i)*df1drj(k)
          d1BOj_pi(k)   = d1BOj_pi(k)   + f4*f5*BOp_pi(ni,i)*df1drj(k)
          d1BOj_pipi(k) = d1BOj_pipi(k) + f4*f5*BOp_pipi(ni,i)*df1drj(k)
        enddo
        do k = 1,nneigh(i)
          d1BOi_s(k)    = d1BOi_s(k)    + f1*f5*BOp(ni,i)*df4dri(k)
          d1BOi_pi(k)   = d1BOi_pi(k)   + f1*f5*BOp_pi(ni,i)*df4dri(k)
          d1BOi_pipi(k) = d1BOi_pipi(k) + f1*f5*BOp_pipi(ni,i)*df4dri(k)
        enddo
        do k = 1,nneigh(j)
          d1BOj_s(k)    = d1BOj_s(k)    + f1*f4*BOp(ni,i)*df5drj(k)
          d1BOj_pi(k)   = d1BOj_pi(k)   + f1*f4*BOp_pi(ni,i)*df5drj(k)
          d1BOj_pipi(k) = d1BOj_pipi(k) + f1*f4*BOp_pipi(ni,i)*df5drj(k)
        enddo
      endif
    endif
!
!  Free local memory
!
    if (lgrad1) then
      deallocate(df5drj,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df5drj')
      deallocate(df4dri,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df4dri')
      deallocate(df1drj,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df1drj')
      deallocate(df1dri,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df1dri')
    endif
  elseif (lreaxFFbocorrect(1,ind).and..not.lreaxFFbocorrect(2,ind)) then
!***************************************************
!  Only overcoordination correction to be applied  *
!***************************************************
!
!  Allocate local memory
!
    if (lgrad1) then
      allocate(df1dri(nneigh(i)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df1dri')
      allocate(df1drj(nneigh(j)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df1drj')
    endif
!
!  Calculate Bij and Bji - loop over all other neighbours
!
    if (lgrad1) then
      d1BOi_s(1:nneighi2) = 0.0_dp
      d1BOi_pi(1:nneighi2) = 0.0_dp
      d1BOi_pipi(1:nneighi2) = 0.0_dp
      d1BOj_s(1:nneighj2) = 0.0_dp
      d1BOj_pi(1:nneighj2) = 0.0_dp
      d1BOj_pipi(1:nneighj2) = 0.0_dp
      df1dri(1:nneigh(i)) = 0.0_dp
      df1drj(1:nneigh(j)) = 0.0_dp
    endif
!
!  Set local variable for bond order prime of currrent bond
!
    BOpij_s  = BOp(ni,i)
    BOpij_pi = BOp_pi(ni,i) + BOp_pipi(ni,i)
    BOpij = BOpij_s + BOpij_pi
!
!  Compute functions of delta'
!
    call reaxFF_f1(nspeci,nspecj,deltapi,deltapj,f1,df1ddpi,df1ddpj,d2f1ddpi2,d2f1ddpiddpj,d2f1ddpj2,lgrad1,.false.)
!
!  Calculate corrected bond orders for i-j
!
    BOij = BOpij*f1
    if (lf1squared) then
      BOij_pi   = BOp_pi(ni,i)*f1*f1
      BOij_pipi = BOp_pipi(ni,i)*f1*f1
    else
      BOij_pi   = BOp_pi(ni,i)*f1
      BOij_pipi = BOp_pipi(ni,i)*f1
    endif
    BOij_s = BOij - BOij_pi - BOij_pipi
!
!  Compute derivatives of combined bond order term
!
    if (lgrad1) then
      if (lf1squared) then
        BOpij_s = BOpij_s + (1.0_dp - f1)*BOpij_pi
      endif
!
!  First derivatives of f1 
!
      do k = 1,nneigh(i)
        dBOpijdr = (d1BOp(k,i) + d1BOp_pi(k,i) + d1BOp_pipi(k,i))
        df1dri(k) = df1dri(k) + df1ddpi*dBOpijdr
      enddo
      do k = 1,nneigh(j)
        dBOpijdr = (d1BOp(k,j) + d1BOp_pi(k,j) + d1BOp_pipi(k,j))
        df1drj(k) = df1drj(k) + df1ddpj*dBOpijdr
      enddo
!
!  Derivatives of BO' 
!
      if (lf1squared) then
        d1BOi_s(ni)    = d1BOi_s(ni)    + 0.5_dp*(d1BOp(ni,i) + (1.0_dp - f1)*(d1BOp_pi(ni,i) + d1BOp_pipi(ni,i)))*f1
        d1BOi_pi(ni)   = d1BOi_pi(ni)   + 0.5_dp*d1BOp_pi(ni,i)*f1*f1
        d1BOi_pipi(ni) = d1BOi_pipi(ni) + 0.5_dp*d1BOp_pipi(ni,i)*f1*f1
        d1BOj_s(nj)    = d1BOj_s(nj)    + 0.5_dp*(d1BOp(nj,j) + (1.0_dp - f1)*(d1BOp_pi(nj,j) + d1BOp_pipi(nj,j)))*f1
        d1BOj_pi(nj)   = d1BOj_pi(nj)   + 0.5_dp*d1BOp_pi(nj,j)*f1*f1
        d1BOj_pipi(nj) = d1BOj_pipi(nj) + 0.5_dp*d1BOp_pipi(nj,j)*f1*f1
!
!  Derivatives of f1 - note that derivatives of BOp are the same as those of deltap with respect to the same bond
!
        do k = 1,nneigh(i)
          d1BOi_s(k)    = d1BOi_s(k)    + (BOpij_s - f1*BOpij_pi)*df1dri(k)
          d1BOi_pi(k)   = d1BOi_pi(k)   + 2.0_dp*f1*BOp_pi(ni,i)*df1dri(k)
          d1BOi_pipi(k) = d1BOi_pipi(k) + 2.0_dp*f1*BOp_pipi(ni,i)*df1dri(k)
        enddo
        do k = 1,nneigh(j)
          d1BOj_s(k)    = d1BOj_s(k)    + (BOpij_s - f1*BOpij_pi)*df1drj(k)
          d1BOj_pi(k)   = d1BOj_pi(k)   + 2.0_dp*f1*BOp_pi(ni,i)*df1drj(k)
          d1BOj_pipi(k) = d1BOj_pipi(k) + 2.0_dp*f1*BOp_pipi(ni,i)*df1drj(k)
        enddo
      else
        d1BOi_s(ni)    = d1BOi_s(ni)    + 0.5_dp*d1BOp(ni,i)*f1
        d1BOi_pi(ni)   = d1BOi_pi(ni)   + 0.5_dp*d1BOp_pi(ni,i)*f1
        d1BOi_pipi(ni) = d1BOi_pipi(ni) + 0.5_dp*d1BOp_pipi(ni,i)*f1
        d1BOj_s(nj)    = d1BOj_s(nj)    + 0.5_dp*d1BOp(nj,j)*f1
        d1BOj_pi(nj)   = d1BOj_pi(nj)   + 0.5_dp*d1BOp_pi(nj,j)*f1
        d1BOj_pipi(nj) = d1BOj_pipi(nj) + 0.5_dp*d1BOp_pipi(nj,j)*f1
!
!  Derivatives of f1 - note that derivatives of BOp are the same as those of deltap with respect to the same bond
!
        do k = 1,nneigh(i)
          d1BOi_s(k)    = d1BOi_s(k)    + BOp(ni,i)*df1dri(k)
          d1BOi_pi(k)   = d1BOi_pi(k)   + BOp_pi(ni,i)*df1dri(k)
          d1BOi_pipi(k) = d1BOi_pipi(k) + BOp_pipi(ni,i)*df1dri(k)
        enddo
        do k = 1,nneigh(j)
          d1BOj_s(k)    = d1BOj_s(k)    + BOp(ni,i)*df1drj(k)
          d1BOj_pi(k)   = d1BOj_pi(k)   + BOp_pi(ni,i)*df1drj(k)
          d1BOj_pipi(k) = d1BOj_pipi(k) + BOp_pipi(ni,i)*df1drj(k)
        enddo
      endif
    endif
!
!  Free local memory
!
    if (lgrad1) then
      deallocate(df1drj,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df1drj')
      deallocate(df1dri,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df1dri')
    endif
  elseif (.not.lreaxFFbocorrect(1,ind).and.lreaxFFbocorrect(2,ind)) then
!**************************************
!  Only 1-3 correction to be applied  *
!**************************************
!
!  Allocate local memory
!
    if (lgrad1) then
      allocate(df4dri(nneigh(i)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df4dri')
      allocate(df5drj(nneigh(j)),stat=status)
      if (status/=0) call outofmemory('reaxFF','df5drj')
    endif
!
!  Calculate Bij and Bji - loop over all other neighbours
!
    if (lgrad1) then
      d1BOi_s(1:nneighi2) = 0.0_dp
      d1BOi_pi(1:nneighi2) = 0.0_dp
      d1BOi_pipi(1:nneighi2) = 0.0_dp
      d1BOj_s(1:nneighj2) = 0.0_dp
      d1BOj_pi(1:nneighj2) = 0.0_dp
      d1BOj_pipi(1:nneighj2) = 0.0_dp
      df4dri(1:nneigh(i)) = 0.0_dp
      df5drj(1:nneigh(j)) = 0.0_dp
    endif
!
!  Set local variable for bond order prime of currrent bond
!
    BOpij_s  = BOp(ni,i)
    BOpij_pi = BOp_pi(ni,i) + BOp_pipi(ni,i)
    BOpij = BOpij_s + BOpij_pi
!
!  Compute functions of delta'
!
    call reaxFF_f45(nspeci,nspecj,BOpij,deltapi,f4,df4ddpi,df4dbo,d2f4ddpi2,d2f4ddpidbo,d2f4dbo2,lgrad1,.false.)
    call reaxFF_f45(nspecj,nspeci,BOpij,deltapj,f5,df5ddpj,df5dbo,d2f5ddpj2,d2f5ddpjdbo,d2f5dbo2,lgrad1,.false.)
!
!  Calculate corrected bond orders for i-j
!
    BOij      = BOpij*f4*f5
    BOij_pi   = BOp_pi(ni,i)*f4*f5
    BOij_pipi = BOp_pipi(ni,i)*f4*f5
    BOij_s = BOij - BOij_pi - BOij_pipi
!
!  Compute derivatives of combined bond order term
!
    if (lgrad1) then
!
!  First derivatives of f4 
!
      dBOpijdr = (d1BOp(ni,i) + d1BOp_pi(ni,i) + d1BOp_pipi(ni,i))
      df4dri(ni) = df4dri(ni) + df4dbo*dBOpijdr
      do k = 1,nneigh(i)
        dBOpijdr = (d1BOp(k,i) + d1BOp_pi(k,i) + d1BOp_pipi(k,i))
        df4dri(k) = df4dri(k) + df4ddpi*dBOpijdr
      enddo
!
!  First derivatives of f5 
!
      dBOpijdr = (d1BOp(nj,j) + d1BOp_pi(nj,j) + d1BOp_pipi(nj,j))
      df5drj(nj) = df5drj(nj) + df5dbo*dBOpijdr
      do k = 1,nneigh(j)
        dBOpijdr = (d1BOp(k,j) + d1BOp_pi(k,j) + d1BOp_pipi(k,j))
        df5drj(k) = df5drj(k) + df5ddpj*dBOpijdr
      enddo
!
!  Derivatives of BO' 
!
      d1BOi_s(ni)    = d1BOi_s(ni)    + 0.5_dp*d1BOp(ni,i)*f4*f5
      d1BOi_pi(ni)   = d1BOi_pi(ni)   + 0.5_dp*d1BOp_pi(ni,i)*f4*f5
      d1BOi_pipi(ni) = d1BOi_pipi(ni) + 0.5_dp*d1BOp_pipi(ni,i)*f4*f5
      d1BOj_s(nj)    = d1BOj_s(nj)    + 0.5_dp*d1BOp(nj,j)*f4*f5
      d1BOj_pi(nj)   = d1BOj_pi(nj)   + 0.5_dp*d1BOp_pi(nj,j)*f4*f5
      d1BOj_pipi(nj) = d1BOj_pipi(nj) + 0.5_dp*d1BOp_pipi(nj,j)*f4*f5
!
!  Derivatives of f4 & f5 - note that derivatives of BOp are the same as those of deltap with respect to the same bond
!
      do k = 1,nneigh(i)
        d1BOi_s(k)    = d1BOi_s(k)    + f5*BOpij_s*df4dri(k)
        d1BOi_pi(k)   = d1BOi_pi(k)   + f5*BOp_pi(ni,i)*df4dri(k)
        d1BOi_pipi(k) = d1BOi_pipi(k) + f5*BOp_pipi(ni,i)*df4dri(k)
      enddo
      do k = 1,nneigh(j)
        d1BOj_s(k)    = d1BOj_s(k)    + f4*BOpij_s*df5drj(k)
        d1BOj_pi(k)   = d1BOj_pi(k)   + f4*BOp_pi(ni,i)*df5drj(k)
        d1BOj_pipi(k) = d1BOj_pipi(k) + f4*BOp_pipi(ni,i)*df5drj(k)
      enddo
    endif
!
!  Free local memory
!
    if (lgrad1) then
      deallocate(df5drj,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df5drj')
      deallocate(df4dri,stat=status)
      if (status/=0) call deallocate_error('reaxFF','df4dri')
    endif
  else
!*********************************
!  No corrections to be applied  *
!*********************************
!
!  Calculate Bij and Bji - loop over all other neighbours
!
    if (lgrad1) then
      d1BOi_s(1:nneighi2) = 0.0_dp
      d1BOi_pi(1:nneighi2) = 0.0_dp
      d1BOi_pipi(1:nneighi2) = 0.0_dp
      d1BOj_s(1:nneighj2) = 0.0_dp
      d1BOj_pi(1:nneighj2) = 0.0_dp
      d1BOj_pipi(1:nneighj2) = 0.0_dp
    endif
!
!  Set local variable for bond order prime of currrent bond
!
    BOpij_s  = BOp(ni,i)
    BOpij_pi = BOp_pi(ni,i) + BOp_pipi(ni,i)
    BOpij = BOpij_s + BOpij_pi
!
!  Calculate corrected bond orders for i-j
!
    BOij      = BOpij
    BOij_pi   = BOp_pi(ni,i)
    BOij_pipi = BOp_pipi(ni,i)
    BOij_s = BOij - BOij_pi - BOij_pipi
!
!  Compute derivatives of combined bond order term
!
    if (lgrad1) then
!
!  Derivatives of BO' 
!
      d1BOi_s(ni)    = d1BOi_s(ni)    + 0.5_dp*d1BOp(ni,i)
      d1BOi_pi(ni)   = d1BOi_pi(ni)   + 0.5_dp*d1BOp_pi(ni,i)
      d1BOi_pipi(ni) = d1BOi_pipi(ni) + 0.5_dp*d1BOp_pipi(ni,i)
      d1BOj_s(nj)    = d1BOj_s(nj)    + 0.5_dp*d1BOp(nj,j) 
      d1BOj_pi(nj)   = d1BOj_pi(nj)   + 0.5_dp*d1BOp_pi(nj,j)
      d1BOj_pipi(nj) = d1BOj_pipi(nj) + 0.5_dp*d1BOp_pipi(nj,j)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_bosum1(i,j,nspeci,nspecj,ni,nj,deltapi,deltapj,nneigh,BOp,BOp_pi,BOp_pipi, &
                           d1BOp,d1BOp_pi,d1BOp_pipi,BOij,d1BOi,d1BOj,lgrad1)
!
!  Calculates the bond order terms and first derivatives
!
!  On entry : 
!
!  lgrad1          = if .true. calculate the first derivatives
!
!  On exit :
!
!  ereaxFF         = the value of the energy contribution
!
!  10/07 Created from reaxff_bo as first derivative only version
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
!  Julian Gale, NRI, Curtin University, October 2007
!
  use datatypes
  use current
  use iochannels
  use neighbours
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: i
  integer(i4), intent(in)                        :: j
  integer(i4), intent(in)                        :: nspeci
  integer(i4), intent(in)                        :: nspecj
  integer(i4), intent(in)                        :: ni
  integer(i4), intent(in)                        :: nj
  integer(i4), intent(in)                        :: nneigh(*)
  real(dp),    intent(in)                        :: deltapi
  real(dp),    intent(in)                        :: deltapj
  real(dp),    intent(in)                        :: BOp(maxneigh,*)
  real(dp),    intent(in)                        :: BOp_pi(maxneigh,*)
  real(dp),    intent(in)                        :: BOp_pipi(maxneigh,*)
  real(dp),    intent(in)                        :: d1BOp(maxneigh,*)
  real(dp),    intent(in)                        :: d1BOp_pi(maxneigh,*)
  real(dp),    intent(in)                        :: d1BOp_pipi(maxneigh,*)
  real(dp),    intent(out)                       :: BOij
  real(dp),    intent(out)                       :: d1BOi(*)
  real(dp),    intent(out)                       :: d1BOj(*)
  logical,     intent(in)                        :: lgrad1
!
!  Local variables
!
  integer(i4)                                    :: ind
  integer(i4)                                    :: k
  integer(i4)                                    :: l
  integer(i4)                                    :: n
  integer(i4)                                    :: nn
  integer(i4)                                    :: nneighi1
  integer(i4)                                    :: nneighj1
  integer(i4)                                    :: nneighi2
  integer(i4)                                    :: nneighj2
  integer(i4)                                    :: status
  real(dp)                                       :: BOpij
  real(dp)                                       :: BOpij_s
  real(dp)                                       :: BOpij_pi
  real(dp),    dimension(:),   allocatable, save :: df1dri
  real(dp),    dimension(:),   allocatable, save :: df1drj
  real(dp),    dimension(:),   allocatable, save :: df4dri
  real(dp),    dimension(:),   allocatable, save :: df5drj
  real(dp)                                       :: dBOpijdr
  real(dp)                                       :: df1ddpi
  real(dp)                                       :: df1ddpj
  real(dp)                                       :: df4ddpi
  real(dp)                                       :: df4ddpj
  real(dp)                                       :: df4dbo
  real(dp)                                       :: df5ddpj
  real(dp)                                       :: df5dbo
  real(dp)                                       :: d2f1ddpi2
  real(dp)                                       :: d2f1ddpiddpj
  real(dp)                                       :: d2f1ddpj2
  real(dp)                                       :: d2f4ddpi2
  real(dp)                                       :: d2f4ddpidbo
  real(dp)                                       :: d2f4dbo2
  real(dp)                                       :: d2f5ddpj2
  real(dp)                                       :: d2f5ddpjdbo
  real(dp)                                       :: d2f5dbo2
  real(dp)                                       :: f1
  real(dp)                                       :: f4
  real(dp)                                       :: f5
!
!  Set total number of distances for neighbours of i
!
  nneighi1   = nneigh(i)*(nneigh(i) + 1)/2 
  nneighi2   = nneigh(i) + nneighi1
!
!  Set total number of distances for neighbours of j
!
  nneighj1   = nneigh(j)*(nneigh(j) + 1)/2
  nneighj2   = nneigh(j) + nneighj1
!
!  Allocate local memory
!
  if (lgrad1) then
    allocate(df1dri(nneigh(i)),stat=status)
    if (status/=0) call outofmemory('reaxFF','df1dri')
    allocate(df1drj(nneigh(j)),stat=status)
    if (status/=0) call outofmemory('reaxFF','df1drj')
    allocate(df4dri(nneigh(i)),stat=status)
    if (status/=0) call outofmemory('reaxFF','df4dri')
    allocate(df5drj(nneigh(j)),stat=status)
    if (status/=0) call outofmemory('reaxFF','df5drj')
  endif
!***************************************
!  Valid pairwise reaxFF contribution  *
!***************************************
!
!  Calculate Bij and Bji - loop over all other neighbours
!
  if (lgrad1) then
    d1BOi(1:nneighi2) = 0.0_dp
    d1BOj(1:nneighj2) = 0.0_dp
    df1dri(1:nneigh(i)) = 0.0_dp
    df1drj(1:nneigh(j)) = 0.0_dp
    df4dri(1:nneigh(i)) = 0.0_dp
    df5drj(1:nneigh(j)) = 0.0_dp
  endif
!
!  Set local variable for bond order prime of currrent bond
!
  BOpij_s  = BOp(ni,i)
  BOpij_pi = BOp_pi(ni,i) + BOp_pipi(ni,i)
  BOpij = BOpij_s + BOpij_pi
!
!  Compute functions of delta'
!
  call reaxFF_f1(nspeci,nspecj,deltapi,deltapj,f1,df1ddpi,df1ddpj,d2f1ddpi2,d2f1ddpiddpj,d2f1ddpj2,lgrad1,.false.)
  call reaxFF_f45(nspeci,nspecj,BOpij,deltapi,f4,df4ddpi,df4dbo,d2f4ddpi2,d2f4ddpidbo,d2f4dbo2,lgrad1,.false.)
  call reaxFF_f45(nspecj,nspeci,BOpij,deltapj,f5,df5ddpj,df5dbo,d2f5ddpj2,d2f5ddpjdbo,d2f5dbo2,lgrad1,.false.)
!
!  Calculate corrected bond orders for i-j
!
  BOij = BOpij*f1*f4*f5
!
!  Compute derivatives of combined bond order term
!
  if (lgrad1) then
!
!  First derivatives of f1 
!
    do k = 1,nneigh(i)
      dBOpijdr = (d1BOp(k,i) + d1BOp_pi(k,i) + d1BOp_pipi(k,i))
      df1dri(k) = df1dri(k) + df1ddpi*dBOpijdr
    enddo
    do k = 1,nneigh(j)
      dBOpijdr = (d1BOp(k,j) + d1BOp_pi(k,j) + d1BOp_pipi(k,j))
      df1drj(k) = df1drj(k) + df1ddpj*dBOpijdr
    enddo
!
!  First derivatives of f4 
!
    dBOpijdr = (d1BOp(ni,i) + d1BOp_pi(ni,i) + d1BOp_pipi(ni,i))
    df4dri(ni) = df4dri(ni) + df4dbo*dBOpijdr
    do k = 1,nneigh(i)
      dBOpijdr = (d1BOp(k,i) + d1BOp_pi(k,i) + d1BOp_pipi(k,i))
      df4dri(k) = df4dri(k) + df4ddpi*dBOpijdr
    enddo
!
!  First derivatives of f5 
!
    dBOpijdr = (d1BOp(nj,j) + d1BOp_pi(nj,j) + d1BOp_pipi(nj,j))
    df5drj(nj) = df5drj(nj) + df5dbo*dBOpijdr
    do k = 1,nneigh(j)
      dBOpijdr = (d1BOp(k,j) + d1BOp_pi(k,j) + d1BOp_pipi(k,j))
      df5drj(k) = df5drj(k) + df5ddpj*dBOpijdr
    enddo
!
!  Derivatives of BO' 
!
    dBOpijdr = (d1BOp(ni,i) + d1BOp_pi(ni,i) + d1BOp_pipi(ni,i))
    d1BOi(ni) = d1BOi(ni) + 0.5_dp*dBOpijdr*f1*f4*f5
    dBOpijdr = (d1BOp(nj,j) + d1BOp_pi(nj,j) + d1BOp_pipi(nj,j))
    d1BOj(nj) = d1BOj(nj) + 0.5_dp*dBOpijdr*f1*f4*f5
!
!  Derivatives of f1, f4 & f5 - note that derivatives of BOp are the same as those of deltap with respect to the same bond
!
    do k = 1,nneigh(i)
      d1BOi(k) = d1BOi(k) + BOpij*(f4*f5*df1dri(k) + f1*f5*df4dri(k))
    enddo
    do k = 1,nneigh(j)
      d1BOj(k) = d1BOj(k) + BOpij*(f4*f5*df1drj(k) + f1*f4*df5drj(k))
    enddo
  endif
!
!  Free local memory
!
  if (lgrad1) then
    deallocate(df5drj,stat=status)
    if (status/=0) call deallocate_error('reaxFF','df5drj')
    deallocate(df4dri,stat=status)
    if (status/=0) call deallocate_error('reaxFF','df4dri')
    deallocate(df1drj,stat=status)
    if (status/=0) call deallocate_error('reaxFF','df1drj')
    deallocate(df1dri,stat=status)
    if (status/=0) call deallocate_error('reaxFF','df1dri')
  endif
!
  return
  end
