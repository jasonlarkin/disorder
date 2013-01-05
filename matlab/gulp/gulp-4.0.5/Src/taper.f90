!*****************
!  Cosine taper  *
!*****************
  subroutine ctaper(r,rmin,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives 
!  using cosine interpolation.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!   5/02 Created
!  11/04 Pi accessed from module
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, October 2004
!
  use datatypes
  use constants, only : pi
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmin
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
!  Local variables
!
  real(dp)                            :: rd
  real(dp)                            :: cosr
  real(dp)                            :: sinr
!
  if (r .le. rmin) then
!*************
!  r < rmin  *
!*************
    f = 1.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  elseif (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
    rd = pi/(rmax - rmin)
    cosr = cos(rd*(r - rmin))
    f = 0.5_dp*(1.0_dp + cosr)
    if (lgrad1) then
      sinr = sin(rd*(r - rmin))
      dfdr = - 0.5_dp*rd*sinr
      if (lgrad2) then
        d2fdr2 = - 0.5_dp*rd*rd*cosr
        if (lgrad3) then
          d3fdr3 = 0.5_dp*rd*rd*rd*sinr
        endif
      endif
    endif
  endif
!
  return
  end
!*********************
!  Polynomial taper  *
!*********************
  subroutine p5taper(r,rmin,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives 
!  using a fifth order polynomial interpolation.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper
!  rmin            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!   5/02 Created
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, October 2004
!
  use datatypes
  use two
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmin
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
  if (r .le. rmin) then
!*************
!  r < rmin  *
!*************
    f = 1.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  elseif (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
    f = ((((pts5*r + pts4)*r + pts3)*r + pts2)*r + pts1)*r + pts0
    if (lgrad1) then
      dfdr = (((5.0_dp*pts5*r + 4.0_dp*pts4)*r + 3.0_dp*pts3)*r + 2.0_dp*pts2)*r + pts1
      if (lgrad2) then
        d2fdr2 = ((20.0_dp*pts5*r + 12.0_dp*pts4)*r + 6.0_dp*pts3)*r + 2.0_dp*pts2
        if (lgrad3) then
          d3fdr3 = (60.0_dp*pts5*r + 24.0_dp*pts4)*r + 6.0_dp*pts3
        endif
      endif
    endif
  endif
!
  return
  end
!
  subroutine p7taper(r,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives 
!  using a seventh order polynomial interpolation.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper = 0
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!   9/07 Created
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
!  Julian Gale, NRI, Curtin University, September 2007
!
  use datatypes
  use two
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
  if (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
    f = ((((((pts7_7*r + pts6_7)*r + pts5_7)*r + pts4_7)*r + pts3_7)*r + pts2_7)*r + pts1_7)*r + pts0_7
    if (lgrad1) then
      dfdr = (((((7.0_dp*pts7_7*r + 6.0_dp*pts6_7)*r + 5.0_dp*pts5_7)*r + 4.0_dp*pts4_7)*r + &
                 3.0_dp*pts3_7)*r + 2.0_dp*pts2_7)*r + pts1_7
      if (lgrad2) then
        d2fdr2 = ((((42.0_dp*pts7_7*r + 30.0_dp*pts6_7)*r + 20.0_dp*pts5_7)*r + 12.0_dp*pts4_7)*r + &
                    6.0_dp*pts3_7)*r + 2.0_dp*pts2_7
        if (lgrad3) then
          d3fdr3 = (((210.0_dp*pts7_7*r + 120.0_dp*pts6_7)*r + 60.0_dp*pts5_7)*r + 24.0_dp*pts4_7)*r + &
                     6.0_dp*pts3_7
        endif
      endif
    endif
  endif
!
  return
  end
!***************
!  Sine taper  *
!***************
  subroutine staper(r,rmin,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives using the
!  sine interpolation of Watanabe et al (Jpn. J. Appl. Phys., 38, L367 (1999).
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!  10/04 Created
!  11/04 Pi accessed from module
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, October 2004
!
  use datatypes
  use constants, only : pi
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmin
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
!  Local variables
!
  real(dp)                            :: arg
  real(dp)                            :: rd
  real(dp)                            :: rr
  real(dp)                            :: rrd
  real(dp)                            :: cosr
  real(dp)                            :: sinr
!
  if (r .le. rmin) then
!*************
!  r < rmin  *
!*************
    f = 1.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  elseif (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
    rd = 0.5_dp*(rmax - rmin)
    rrd = 1.0_dp/rd
    rr = rmin + rd
    arg = (r - rr + rd)*rrd
    sinr = sin(pi*arg)
    f = 1.0_dp - 0.5_dp*(arg - (sinr/pi))
    if (lgrad1) then
      cosr = cos(pi*arg)
      dfdr = 0.5_dp*rrd*(cosr - 1.0_dp)
      if (lgrad2) then
        d2fdr2 = - 0.5_dp*rrd*rrd*pi*sinr
        if (lgrad3) then
          d3fdr3 = - 0.5_dp*rrd*rrd*rrd*pi*pi*cosr
        endif
      endif
    endif
  endif
!
  return
  end
!*******************
!  Accelrys taper  *
!*******************
  subroutine ataper(lnorm,r,rmin,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives 
!  using Accelrys form of interpolation.
!
!  On entry : 
!
!  lnorm           = if .true. then apply normal taper
!                    if .false. then make the taper go to zero at the other end
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!   9/06 Created
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, September 2006
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmin
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
  logical,     intent(in)             :: lnorm
!
!  Local variables
!
  real(dp)                            :: rd
  real(dp)                            :: rterm
  real(dp)                            :: trm1
!
  if (lnorm) then
    if (r .le. rmin) then
!*************
!  r < rmin  *
!*************
      f = 1.0_dp
      if (lgrad1) then
        dfdr = 0.0_dp
        if (lgrad2) then
          d2fdr2 = 0.0_dp
          if (lgrad3) then
            d3fdr3 = 0.0_dp
          endif
        endif
      endif
    elseif (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
      f = 0.0_dp
      if (lgrad1) then
        dfdr = 0.0_dp
        if (lgrad2) then
          d2fdr2 = 0.0_dp
          if (lgrad3) then
            d3fdr3 = 0.0_dp
          endif
        endif
      endif
    else
!********************
!  rmin < r < rmax  *
!********************
      rterm = 1.0_dp/(rmax - rmin)**3
      rd = rmax - r
      trm1 = (rmax + 2.0_dp*r - 3.0_dp*rmin)
      f = rterm*rd*rd*trm1
      if (lgrad1) then
        dfdr = 2.0_dp*rd*rterm*(rd - trm1)
        if (lgrad2) then
          d2fdr2 = - 2.0_dp*rterm*(4.0_dp*rd - trm1)
          if (lgrad3) then
            d3fdr3 = 12.0_dp*rterm
          endif
        endif
      endif
    endif
  else
    if (r .le. rmin) then
!*************
!  r > rmin  *
!*************
      f = 0.0_dp
      if (lgrad1) then
        dfdr = 0.0_dp
        if (lgrad2) then
          d2fdr2 = 0.0_dp
          if (lgrad3) then
            d3fdr3 = 0.0_dp
          endif
        endif
      endif
    elseif (r .ge. rmax) then
!*************
!  r < rmax  *
!*************
      f = 1.0_dp
      if (lgrad1) then
        dfdr = 0.0_dp
        if (lgrad2) then
          d2fdr2 = 0.0_dp
          if (lgrad3) then
            d3fdr3 = 0.0_dp
          endif
        endif
      endif
    else
!********************
!  rmin < r < rmax  *
!********************
      rterm = 1.0_dp/(rmin - rmax)**3
      rd = rmin - r
      trm1 = (rmin + 2.0_dp*r - 3.0_dp*rmax)
      f = rterm*rd*rd*trm1
      if (lgrad1) then
        dfdr = 2.0_dp*rd*rterm*(rd - trm1)
        if (lgrad2) then
          d2fdr2 = - 2.0_dp*rterm*(4.0_dp*rd - trm1)
          if (lgrad3) then
            d3fdr3 = 12.0_dp*rterm
          endif
        endif
      endif
    endif
  endif
!
  return
  end
!**********************
!  Exponential taper  *
!**********************
  subroutine etaper(r,rmin,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives 
!  using an inverse exponential form.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper
!  rmin            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!   5/07 Created
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
!  Julian Gale, NRI, Curtin University, May 2007
!
  use datatypes
  use two
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmin
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
!  Local variables
!
  real(dp)                            :: exptrm
  real(dp)                            :: y
  real(dp)                            :: dydr
  real(dp)                            :: d2ydr2
  real(dp)                            :: d3ydr3
!
  if (r .le. rmin) then
!*************
!  r < rmin  *
!*************
    f = 1.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  elseif (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
    y = 1.0_dp/(rmax - r)
    exptrm = exp(-y)
    f = exptrm
    if (lgrad1) then
      dydr = y*y
      dfdr = - dydr*exptrm
      if (lgrad2) then
        d2ydr2 = 2.0_dp*dydr*y
        d2fdr2 = (dydr**2 - d2ydr2)*exptrm
        if (lgrad3) then
          d3ydr3 = 3.0_dp*d2ydr2*y
          d3fdr3 = - (dydr**3 - 3.0_dp*dydr*d2ydr2 + d3ydr3)*exptrm
        endif
      endif
    endif
  endif
!
  return
  end
!**************
!  MDF taper  *
!**************
  subroutine mdftaper(r,rmin,rmax,f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper function and derivatives 
!  using the MDF interpolation form (PRB, 43, 4653 (1991)).
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!  d3fwdr3         = third derivative of taper function
!
!  11/07 Created
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
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmin
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  real(dp),    intent(out)            :: d3fdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
!  Local variables
!
  real(dp)                            :: x
  real(dp)                            :: rrdiff
  real(dp)                            :: trm1
  real(dp)                            :: trm2
  real(dp)                            :: dtrm1dx
  real(dp)                            :: dtrm2dx
  real(dp)                            :: d2trm1dx2
  real(dp)                            :: d2trm2dx2
  real(dp)                            :: d3trm1dx3
!
  if (r .le. rmin) then
!*************
!  r < rmin  *
!*************
    f = 1.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  elseif (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
        if (lgrad3) then
          d3fdr3 = 0.0_dp
        endif
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
    rrdiff = 1.0_dp/(rmax - rmin)
    x = (r - rmin)*rrdiff
    trm1 = (1.0_dp - x)**3
    trm2 = (1.0_dp + 3.0_dp*x + 6.0_dp*x*x)
    f = trm1*trm2
    if (lgrad1) then
      dtrm1dx = - 3.0_dp*(1.0_dp - x)**2
      dtrm2dx = 3.0_dp + 12.0_dp*x
      dfdr = (dtrm1dx*trm2 + trm1*dtrm2dx)*rrdiff
      if (lgrad2) then
        d2trm1dx2 = 6.0_dp*(1.0_dp - x)
        d2trm2dx2 = 12.0_dp
        d2fdr2 = (d2trm1dx2*trm2 + 2.0_dp*dtrm1dx*dtrm2dx + trm1*d2trm2dx2)*rrdiff*rrdiff
        if (lgrad3) then
          d3trm1dx3 = - 6.0_dp
          d3fdr3 = (d3trm1dx3*trm2 + 3.0_dp*(d2trm1dx2*dtrm2dx + dtrm1dx*d2trm2dx2))*rrdiff*rrdiff**rrdiff
        endif
      endif
    endif
  endif
!
  return
  end
