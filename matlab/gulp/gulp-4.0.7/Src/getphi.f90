  subroutine getphi(r12,r14,r24,ir,jr,kr,cos1,cos3,sin1,sin3,cos11d,cos31d,sin11d,sin31d, &
                    cos12d,cos32d,sin12d,sin32d,cos13d,cos33d,sin13d,sin33d,cosphi, &
                    cosp1d,cosp2d,cosp3d,lgrad1,lgrad2,lgrad3)
!
!  Calculates phi for a torsional angle and it's derivatives
!
!  On input :
!
!  r12     = distance between atoms 1 & 2
!  r14     = distance between atoms 1 & 4
!  r24     = distance between atoms 2 & 4
!  ir      = no. of distance between atoms 1 & 2
!  jr      = no. of distance between atoms 1 & 4
!  kr      = no. of distance between atoms 2 & 4
!  cos1    = cosine of angle for 2-1-3
!  cos3    = cosine of angle for 3-1-4
!  sin1    = sine of angle 1
!  sin3    = sine of angle 3
!  cos11d  = first derivative of cos1
!  cos31d  = first derivative of cos3
!  sin11d  = first derivative of sin1
!  sin31d  = first derivative of sin3
!  cos12d  = second derivative of cos1
!  cos32d  = second derivative of cos3
!  sin12d  = second derivative of sin1
!  sin32d  = second derivative of sin3
!  cos13d  = third derivative of cos1
!  cos33d  = third derivative of cos3
!  sin13d  = third derivative of sin1
!  sin33d  = third derivative of sin3
!  lgrad1  = if .true. calculate the first derivatives
!  lgrad2  = if .true. calculate the second derivatives
!  lgrad3  = if .true. calculate the third derivatives
!
!  On return :
!
!  cosphi  = cosine of phi
!  cosp1d  = array of first derivative terms
!  cosp2d  = array of second derivative terms
!  cosp3d  = array of third derivative terms
!
!  10/05 Created from fourbody.f
!  12/07 Unused variables removed
!   2/11 integer type declared as i4
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
!  Julian Gale, NRI, Curtin University, February 2011
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(in)  :: ir
  integer(i4),  intent(in)  :: jr
  integer(i4),  intent(in)  :: kr
  logical,      intent(in)  :: lgrad1
  logical,      intent(in)  :: lgrad2
  logical,      intent(in)  :: lgrad3
  real(dp),     intent(in)  :: cos1
  real(dp),     intent(in)  :: cos3
  real(dp),     intent(in)  :: cos11d(6)
  real(dp),     intent(in)  :: cos31d(6)
  real(dp),     intent(in)  :: cos12d(21)
  real(dp),     intent(in)  :: cos32d(21)
  real(dp),     intent(in)  :: cos13d(56)
  real(dp),     intent(in)  :: cos33d(56)
  real(dp),     intent(out) :: cosphi
  real(dp),     intent(out) :: cosp1d(6)
  real(dp),     intent(out) :: cosp2d(21)
  real(dp),     intent(out) :: cosp3d(56)
  real(dp),     intent(in)  :: r12
  real(dp),     intent(in)  :: r14
  real(dp),     intent(in)  :: r24
  real(dp),     intent(in)  :: sin1
  real(dp),     intent(in)  :: sin3
  real(dp),     intent(in)  :: sin11d(6)
  real(dp),     intent(in)  :: sin31d(6)
  real(dp),     intent(in)  :: sin12d(21)
  real(dp),     intent(in)  :: sin32d(21)
  real(dp),     intent(in)  :: sin13d(56)
  real(dp),     intent(in)  :: sin33d(56)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: ij
  integer(i4) :: ik
  integer(i4) :: j
  integer(i4) :: jk
  integer(i4) :: k
  integer(i4) :: kb(6,6)
  real(dp)    :: b0
  real(dp)    :: b1i
  real(dp)    :: b1j
  real(dp)    :: b1k
  real(dp)    :: b2ij
  real(dp)    :: b2ik
  real(dp)    :: b2jk
  real(dp)    :: bot0
  real(dp)    :: r122
  real(dp)    :: r142
  real(dp)    :: r242
  real(dp)    :: rr12
  real(dp)    :: rr122
  real(dp)    :: rr124
  real(dp)    :: rr24
  real(dp)    :: rr242
  real(dp)    :: rr244
  real(dp)    :: rsin1
  real(dp)    :: rsin3
  real(dp)    :: t0
  real(dp)    :: t1i
  real(dp)    :: t1j
  real(dp)    :: t1k
  real(dp)    :: t2ij
  real(dp)    :: t2ik
  real(dp)    :: t2jk
  real(dp)    :: top0
  real(dp)    :: top1(6),bot1(6)
  real(dp)    :: top2(21),bot2(21)
  real(dp)    :: top3(56),bot3(56)
!
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17, &
          18,5,10,14,17,19,20,6,11,15,18,20,21/
!
!  Zero terms
!
  if (lgrad1) then
    do i = 1,6
      top1(i) = 0.0_dp
      bot1(i) = 0.0_dp
      cosp1d(i) = 0.0_dp
    enddo
    if (lgrad2) then
      do i = 1,21
        top2(i) = 0.0_dp
        bot2(i) = 0.0_dp
        cosp2d(i) = 0.0_dp
      enddo
      if (lgrad3) then
        do i = 1,56
          top3(i) = 0.0_dp
          bot3(i) = 0.0_dp
          cosp3d(i) = 0.0_dp
        enddo
      endif
    endif
  endif
!
!  Set up local constants
!
  r122 = r12*r12
  r142 = r14*r14
  r242 = r24*r24
  rr12 = 1.0_dp/r12
  rr24 = 1.0_dp/r24
  rr122 = rr12*rr12
  rr242 = rr24*rr24
  if (lgrad2) then
    rr124 = rr122*rr122
    rr244 = rr242*rr242
  endif
  rsin1 = 1.0_dp/sin1
  rsin3 = 1.0_dp/sin3
!$$$$$$$$$$$$$$
!  Phi terms  $
!$$$$$$$$$$$$$$
  top0 = r122 + r242 - r142 - 2.0_dp*r12*r24*cos1*cos3
  bot0 = rr12*rr24*rsin1*rsin3
  cosphi = 0.5_dp*top0*bot0
  if (abs(cosphi).gt.1.0_dp) cosphi = sign(1.0_dp,cosphi)
  if (lgrad1) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r14
!  4 = r23
!  5 = r24
!  6 = r34
!
    top1(ir) = 2.0_dp - 2.0_dp*r24*cos1*cos3*rr12
    top1(jr) = - 2.0_dp
    top1(kr) = 2.0_dp - 2.0_dp*r12*cos1*cos3*rr24
    do i = 1,6
      top1(i) = top1(i) - 2.0_dp*r12*r24*(cos11d(i)*cos3+cos1*cos31d(i))
    enddo
    bot1(ir) = r24*sin1*sin3*rr12
    bot1(kr) = r12*sin1*sin3*rr24
    do i = 1,6
      bot1(i) = bot1(i) + r12*r24*(sin11d(i)*sin3 + sin1*sin31d(i))
    enddo
!
!  Combine derivatives
!
    do i = 1,6
      cosp1d(i) = 0.5_dp*bot0*(top1(i) - bot0*top0*bot1(i))
    enddo
    if (lgrad2) then
!
!  Second
!
!  1 = 11  7 = 22 12 = 33 16 = 44 19 = 55 21 = 66
!  2 = 21  8 = 32 13 = 43 17 = 54 20 = 65
!  3 = 31  9 = 42 14 = 53 18 = 64
!  4 = 41 10 = 52 15 = 63
!  5 = 51 11 = 62
!  6 = 61 
!
!
!  Top / bottom part of cosphi derivatives 
!
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          top2(ii) = top2(ii) - 2.0_dp*r12*r24*(cos12d(ii)*cos3+cos1*cos32d(ii))
          top2(ii) = top2(ii) - 2.0_dp*r12*r24*(cos11d(i)*cos31d(j)+cos11d(j)*cos31d(i))
          if (i.eq.ir) then
            top2(ii) = top2(ii) - 2.0_dp*r24*rr12*(cos1*cos31d(j)+cos3*cos11d(j))
            if (j.eq.ir) then
              top2(ii) = top2(ii) + 2.0_dp*cos1*cos3*r24*rr122*rr12
            endif
          elseif (i.eq.kr) then
            top2(ii) = top2(ii) - 2.0_dp*r12*rr24*(cos1*cos31d(j)+cos3*cos11d(j))
            if (j.eq.kr) then
              top2(ii) = top2(ii) + 2.0_dp*cos1*cos3*r12*rr242*rr24
            elseif (j.eq.ir) then
              top2(ii) = top2(ii) - 2.0_dp*cos1*cos3*rr24*rr12
            endif
          endif
          if (j.eq.ir) then
            top2(ii) = top2(ii) - 2.0_dp*r24*rr12*(cos1*cos31d(i)+cos3*cos11d(i))
          elseif (j.eq.kr) then
            top2(ii) = top2(ii) - 2.0_dp*r12*rr24*(cos1*cos31d(i)+cos3*cos11d(i))
            if (i.eq.ir) then
              top2(ii) = top2(ii) - 2.0_dp*cos1*cos3*rr24*rr12
            endif
          endif
        enddo
      enddo
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          bot2(ii) = bot2(ii) + r12*r24*(sin12d(ii)*sin3+sin1*sin32d(ii))
          bot2(ii) = bot2(ii) + r12*r24*(sin11d(i)*sin31d(j)+sin11d(j)*sin31d(i))
          if (i.eq.ir) then
            bot2(ii) = bot2(ii) + r24*rr12*(sin1*sin31d(j)+sin3*sin11d(j))
            if (j.eq.ir) then
              bot2(ii) = bot2(ii) - sin1*sin3*r24*rr122*rr12
            endif
          elseif (i.eq.kr) then
            bot2(ii) = bot2(ii) + r12*rr24*(sin1*sin31d(j)+sin3*sin11d(j))
            if (j.eq.kr) then
              bot2(ii) = bot2(ii) - sin1*sin3*r12*rr242*rr24
            elseif (j.eq.ir) then
              bot2(ii) = bot2(ii) + sin1*sin3*rr24*rr12
            endif
          endif
          if (j.eq.ir) then
            bot2(ii) = bot2(ii) + r24*rr12*(sin1*sin31d(i)+sin3*sin11d(i))
          elseif (j.eq.kr) then
            bot2(ii) = bot2(ii) + r12*rr24*(sin1*sin31d(i)+sin3*sin11d(i))
            if (i.eq.ir) then
              bot2(ii) = bot2(ii) + sin1*sin3*rr24*rr12
            endif
          endif
        enddo
      enddo
!
!  Combine derivatives
!
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          cosp2d(ii) = 0.5_dp*bot0*bot0*(2.0_dp*top0*bot0*bot1(j)*bot1(i)-top1(j)*bot1(i)-top1(i)*bot1(j))
        enddo
      enddo
      do i = 1,21
        cosp2d(i) = cosp2d(i) + 0.5_dp*bot0*(top2(i)-bot0*top0*bot2(i))
      enddo
      if (lgrad3) then
!
!  Third 
!
!   1 = 111  22 = 222  37 = 333  47 = 444  53 = 555  56 = 666
!   2 = 211  23 = 322  38 = 433  48 = 544  54 = 655
!   3 = 311  24 = 422  39 = 533  49 = 644  55 = 665
!   4 = 411  25 = 522  40 = 633  50 = 554
!   5 = 511  26 = 622  41 = 443  51 = 654
!   6 = 611  27 = 332  42 = 543  52 = 664
!   7 = 221  28 = 432  43 = 643
!   8 = 321  29 = 532  44 = 553
!   9 = 421  30 = 632  45 = 653
!  10 = 521  31 = 442  46 = 663
!  11 = 621  32 = 542
!  12 = 331  33 = 642
!  13 = 431  34 = 552
!  14 = 531  35 = 652
!  15 = 631  36 = 662
!  16 = 441 
!  17 = 541 
!  18 = 641 
!  19 = 551 
!  20 = 651
!  21 = 661
!
!  Top part
!
        t0 = 2.0_dp*cos1*cos3
        ii = 0
        do i = 1,6
          t1i = cos1*cos31d(i) + cos3*cos11d(i)
          do j = i,6
            ij = kb(j,i)
            t1j = cos1*cos31d(j) + cos3*cos11d(j)
            t2ij = cos1*cos32d(ij) + cos3*cos12d(ij) + cos11d(i)*cos31d(j) + cos11d(j)*cos31d(i)
            do k = j,6
              ii = ii + 1
              ik = kb(k,i)
              jk = kb(k,j)
              t1k = cos1*cos31d(k) + cos3*cos11d(k)
              t2ik = cos1*cos32d(ik) + cos3*cos12d(ik) + cos11d(i)*cos31d(k) + cos11d(k)*cos31d(i)
              t2jk = cos1*cos32d(jk) + cos3*cos12d(jk) + cos11d(j)*cos31d(k) + cos11d(k)*cos31d(j)
              top3(ii) = - 2.0_dp*r12*r24*(cos1*cos33d(ii) + cos3*cos13d(ii) + cos31d(i)*cos12d(jk) +  &
                cos31d(j)*cos12d(ik) + cos31d(k)*cos12d(ij) + cos11d(i)*cos32d(jk) +  &
                cos11d(j)*cos32d(ik) + cos11d(k)*cos32d(ij))
              if (i.eq.ir) then
                top3(ii) = top3(ii) - 2.0_dp*r24*rr12*t2jk
                if (j.eq.ir) then
                  top3(ii) = top3(ii) + 2.0_dp*r24*rr122*rr12*t1k
                  if (k.eq.ir) then
                    top3(ii) = top3(ii) - 3.0_dp*r24*rr124*rr12*t0
                  elseif (k.eq.kr) then
                    top3(ii) = top3(ii) + rr24*rr122*rr12*t0
                  endif
                endif
                if (k.eq.ir) then
                  top3(ii) = top3(ii) + 2.0_dp*r24*rr122*rr12*t1j
                endif
                if (j.eq.kr) then
                  top3(ii) = top3(ii) - 2.0_dp*rr12*rr24*t1k
                  if (k.eq.kr) then
                    top3(ii) = top3(ii) + rr12*rr242*rr24*t0
                  endif
                endif
                if (k.eq.kr) then
                  top3(ii) = top3(ii) - 2.0_dp*rr12*rr24*t1j
                endif
              elseif (i.eq.kr) then
                top3(ii) = top3(ii) - 2.0_dp*r12*rr24*t2jk
                if (j.eq.kr) then
                  top3(ii) = top3(ii) + 2.0_dp*r12*rr242*rr24*t1k
                  if (k.eq.kr) then
                    top3(ii) = top3(ii) - 3.0_dp*r12*rr244*rr24*t0
                  endif
                endif
                if (k.eq.kr) then
                  top3(ii) = top3(ii) + 2.0_dp*r12*rr242*rr24*t1j
                endif
              endif
              if (j.eq.ir) then
                top3(ii) = top3(ii) - 2.0_dp*r24*rr12*t2ik
                if (k.eq.ir) then
                  top3(ii) = top3(ii) + 2.0_dp*r24*rr122*rr12*t1i
                endif
                if (k.eq.kr) then
                  top3(ii) = top3(ii) - 2.0_dp*rr12*rr24*t1i
                endif
              elseif (j.eq.kr) then
                top3(ii) = top3(ii) - 2.0_dp*r12*rr24*t2ik
                if (k.eq.kr) then
                  top3(ii) = top3(ii) + 2.0_dp*r12*rr242*rr24*t1i
                endif
              endif
              if (k.eq.ir) then
                top3(ii) = top3(ii) - 2.0_dp*r24*rr12*t2ij
              elseif (k.eq.kr) then
                top3(ii) = top3(ii) - 2.0_dp*r12*rr24*t2ij
              endif
            enddo
          enddo
        enddo
!
!  Bottom part
!
        b0 = sin1*sin3
        ii = 0
        do i = 1,6
          b1i = sin1*sin31d(i) + sin3*sin11d(i)
          do j = i,6
            ij = kb(j,i)
            b1j = sin1*sin31d(j) + sin3*sin11d(j)
            b2ij = sin1*sin32d(ij) + sin3*sin12d(ij) + sin11d(i)*sin31d(j) + sin11d(j)*sin31d(i)
            do k = j,6
              ii = ii + 1
              ik = kb(k,i)
              jk = kb(k,j)
              b1k = sin1*sin31d(k) + sin3*sin11d(k)
              b2ik = sin1*sin32d(ik) + sin3*sin12d(ik)+sin11d(i)*sin31d(k)+sin11d(k)*sin31d(i)
              b2jk = sin1*sin32d(jk) + sin3*sin12d(jk)+sin11d(j)*sin31d(k)+sin11d(k)*sin31d(j)
              bot3(ii) = r12*r24*(sin1*sin33d(ii)+sin3*sin13d(ii)+sin31d(i)*sin12d(jk)+sin31d(j)* &
                sin12d(ik)+sin31d(k)*sin12d(ij)+sin11d(i)*sin32d(jk)+sin11d(j)*sin32d(ik)+sin11d(k)* &
                sin32d(ij))
              if (i.eq.ir) then
                bot3(ii) = bot3(ii) + r24*rr12*b2jk
                if (j.eq.ir) then
                  bot3(ii) = bot3(ii) - r24*rr122*rr12*b1k
                  if (k.eq.ir) then
                    bot3(ii) = bot3(ii) + 3.0_dp*r24*rr124*rr12*b0
                  elseif (k.eq.kr) then
                    bot3(ii) = bot3(ii) - rr24*rr122*rr12*b0
                  endif
                endif
                if (k.eq.ir) then
                  bot3(ii) = bot3(ii) - r24*rr122*rr12*b1j
                endif
                if (j.eq.kr) then
                  bot3(ii) = bot3(ii) + rr12*rr24*b1k
                  if (k.eq.kr) then
                    bot3(ii) = bot3(ii) - rr12*rr242*rr24*b0
                  endif
                endif
                if (k.eq.kr) then
                  bot3(ii) = bot3(ii) + rr12*rr24*b1j
                endif
              elseif (i.eq.kr) then
                bot3(ii) = bot3(ii) + r12*rr24*b2jk
                if (j.eq.kr) then
                  bot3(ii) = bot3(ii) - r12*rr242*rr24*b1k
                  if (k.eq.kr) then
                    bot3(ii) = bot3(ii) + 3.0_dp*r12*rr244*rr24*b0
                  endif
                endif
                if (k.eq.kr) then
                  bot3(ii) = bot3(ii) - r12*rr242*rr24*b1j
                endif
              endif
              if (j.eq.ir) then
                bot3(ii) = bot3(ii) + r24*rr12*b2ik
                if (k.eq.ir) then
                  bot3(ii) = bot3(ii) - r24*rr122*rr12*b1i
                endif
                if (k.eq.kr) then
                  bot3(ii) = bot3(ii) + rr12*rr24*b1i
                endif
              elseif (j.eq.kr) then
                bot3(ii) = bot3(ii) + r12*rr24*b2ik
                if (k.eq.kr) then
                  bot3(ii) = bot3(ii) - r12*rr242*rr24*b1i
                endif
              endif
              if (k.eq.ir) then
                bot3(ii) = bot3(ii) + r24*rr12*b2ij
              elseif (k.eq.kr) then
                bot3(ii) = bot3(ii) + r12*rr24*b2ij
              endif
            enddo
          enddo
        enddo
!
!  Combine derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            do k = j,6
              ii = ii + 1
              cosp3d(ii) = bot0*bot0*bot0*(top1(i)*bot1(j)*bot1(k)+top1(j)*bot1(i)*bot1(k) +  &
                top1(k)*bot1(i)*bot1(j)+top0*(bot1(i)*bot2(kb(k,j))+bot1(j)*bot2(kb(k,i))+ &
                bot1(k)*bot2(kb(j,i))))
              cosp3d(ii) = cosp3d(ii) - 0.5_dp*bot0*bot0*(bot1(i)*top2(kb(k,j))+bot1(j)*top2(kb(k,i))+bot1(k)* &
                top2(kb(j,i))+top1(i)*bot2(kb(k,j))+top1(j)*bot2(kb(k,i))+top1(k)*bot2(kb(j,i)))
              cosp3d(ii) = cosp3d(ii) - 3.0_dp*top0*(bot0**4)*bot1(i)*bot1(j)*bot1(k)
            enddo
          enddo
        enddo
        do i = 1,56
          cosp3d(i) = cosp3d(i) + 0.5_dp*bot0*(top3(i)-bot0*top0*bot3(i))
        enddo
      endif
    endif
  endif
!
  return
  end
