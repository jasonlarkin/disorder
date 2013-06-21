  subroutine kindex
!
!  Routine sets up array of k vector indices which fixes the number of
!  k vectors used during any given call to funct to prevent steps in
!  the partial derivatives with respect to cell parameters.
!  Currently allows up to 10x10x10 k vectors
!
!  12/97 Selection of rspeed value according to calculation type added
!        as higher order derivatives are faster in real space
!   1/98 Correction added - if freezing is being used then the same
!        value of rspeed and same formula must be used for all types
!        of evaluation otherwise efreeze becomes variable
!   2/01 Modifications for general dimensionality added
!   2/01 Code modified so that 2*pi gets factored into vol4pi sooner
!        so that kvectors can be generated using subroutines
!   8/01 Handling of negative lattice vectors added
!  12/02 Extra margin of safety added for extreme angles in K vector loops
!   2/03 Definition of eta change to sqrt dependence on number of atoms
!   3/03 Default accuracy increased for 2-D case
!   9/03 Calculation of Ewald parameters moved to a separate routine
!   9/04 ncalctype removed as an argument
!  12/07 Unused variables removed
!   3/13 Error messages changed to handle higher dimensions
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use constants
  use control
  use current
  use general
  use iochannels
  use kspace
  use optimisation
  use parallel
  use shell
  use symmetry
  use times
  implicit none
!
!  Local variables
!
  integer(i4)              :: e
  integer(i4)              :: f
  integer(i4)              :: g
  integer(i4)              :: max1l
  integer(i4)              :: max1l1
  integer(i4)              :: max1u
  integer(i4)              :: max2l
  integer(i4)              :: max2l1
  integer(i4)              :: max2u
  integer(i4)              :: max3l
  integer(i4)              :: max3l1
  integer(i4)              :: max3u
  integer(i4)              :: nkaddx
  integer(i4)              :: nkaddy
  integer(i4)              :: nkaddz
  real(dp)                 :: ak
  real(dp)                 :: alphak
  real(dp)                 :: anglemax
  real(dp)                 :: anglemin
  real(dp)                 :: betak
  real(dp)                 :: bk
  real(dp)                 :: ck
  real(dp)                 :: cputime
  real(dp)                 :: gammak
  real(dp)                 :: kvx
  real(dp)                 :: kvy
  real(dp)                 :: kvz
  real(dp)                 :: perpk
  real(dp)                 :: ppk
  real(dp)                 :: projk
  real(dp)                 :: rk1x
  real(dp)                 :: rk1y
  real(dp)                 :: rk1z
  real(dp)                 :: rk2
  real(dp)                 :: rk2x
  real(dp)                 :: rk2y
  real(dp)                 :: rk2z
  real(dp)                 :: rk3x
  real(dp)                 :: rk3y
  real(dp)                 :: rk3z
  real(dp)                 :: rkk1
  real(dp)                 :: rkk2
  real(dp)                 :: rkk3
  real(dp)                 :: rrmx2
  real(dp)                 :: ruk2x
  real(dp)                 :: ruk3x
  real(dp)                 :: ruk2y
  real(dp)                 :: ruk3y
  real(dp)                 :: ruk2z
  real(dp)                 :: ruk3z
  real(dp)                 :: time1
  real(dp)                 :: time2
  real(dp)                 :: xk
  real(dp)                 :: xk2
  real(dp)                 :: xke
  real(dp)                 :: xkf
  real(dp)                 :: yk
  real(dp)                 :: yk2
  real(dp)                 :: yke
  real(dp)                 :: ykf
  real(dp)                 :: zk
  real(dp)                 :: zke
  real(dp)                 :: zkf
  real(dp)                 :: zero
!
  time1 = cputime()
  zero = 1.0d-12
  if (ndim.eq.3) then
    call kvector3D
  elseif (ndim.eq.2) then
    call kvector2D
  elseif (ndim.eq.1) then
    call kvector1D
  endif
!
  call setewald
!
  if (lra) then
    kvx = abs(kv(1,1))
    kvy = abs(kv(2,2))
    kvz = abs(kv(3,3))
    rkk1 = 1.0_dp/kvx
    if (ndim.eq.3) then
      rkk2 = 1.0_dp/kvy
      rkk3 = 1.0_dp/kvz
    elseif (ndim.eq.2) then
      rkk2 = 1.0_dp/kvy
      rkk3 = 0.0_dp
    elseif (ndim.eq.1) then
      rkk2 = 0.0_dp
      rkk3 = 0.0_dp
    endif
  else
    rk1x = kv(1,1)
    rk1y = kv(2,1)
    rk1z = kv(3,1)
    rk2x = kv(1,2)
    rk2y = kv(2,2)
    rk2z = kv(3,2)
    rk3x = kv(1,3)
    rk3y = kv(2,3)
    rk3z = kv(3,3)
    rkk1 = rk1x*rk1x + rk1y*rk1y + rk1z*rk1z
    rkk2 = rk2x*rk2x + rk2y*rk2y + rk2z*rk2z
    rkk3 = rk3x*rk3x + rk3y*rk3y + rk3z*rk3z
    rkk1 = sqrt(rkk1)
    rkk2 = sqrt(rkk2)
    rkk3 = sqrt(rkk3)
    rkk1 = 1.0_dp/rkk1
    if (ndim.eq.3) then
      rkk2 = 1.0_dp/rkk2
      rkk3 = 1.0_dp/rkk3
    elseif (ndim.eq.2) then
      rkk2 = 1.0_dp/rkk2
      rkk3 = 0.0_dp
    elseif (ndim.eq.1) then
      rkk2 = 0.0_dp
      rkk3 = 0.0_dp
    endif
    ruk2x = rkk2*rk2x
    ruk2y = rkk2*rk2y
    ruk2z = rkk2*rk2z
    ruk3x = rkk3*rk3x
    ruk3y = rkk3*rk3y
    ruk3z = rkk3*rk3z
  endif
!
!  Set amounts to add to looping indices
!
  if (ndim.eq.3) then
    anglemax = max(alpha,beta)
    anglemax = max(anglemax,gamma)
    anglemin = min(alpha,beta)
    anglemin = min(anglemin,gamma)
    if (anglemax.gt.150.0_dp.or.anglemin.lt.30.0_dp) then
      nkaddx = 8
      nkaddy = 8
      nkaddz = 1
    elseif (anglemax.gt.140.0_dp.or.anglemin.lt.40.0_dp) then
      nkaddx = 7
      nkaddy = 7
      nkaddz = 1
    elseif (anglemax.gt.130.0_dp.or.anglemin.lt.50.0_dp) then
      nkaddx = 6
      nkaddy = 6
      nkaddz = 1
    elseif (anglemax.gt.120.0_dp.or.anglemin.lt.60.0_dp) then
      nkaddx = 5
      nkaddy = 5
      nkaddz = 1
    elseif (anglemax.gt.110.0_dp.or.anglemin.lt.70.0_dp) then
      nkaddx = 4
      nkaddy = 4
      nkaddz = 1
    elseif (anglemax.gt.100.0_dp.or.anglemin.lt.80.0_dp) then
      nkaddx = 3
      nkaddy = 3
      nkaddz = 1
    else
      nkaddx = 2
      nkaddy = 2
      nkaddz = 1
    endif
  elseif (ndim.eq.2) then
    if (alpha.gt.150.0_dp.or.alpha.lt.30.0_dp) then
      nkaddx = 7
      nkaddy = 1
    elseif (alpha.gt.140.0_dp.or.alpha.lt.40.0_dp) then
      nkaddx = 6
      nkaddy = 1
    elseif (alpha.gt.130.0_dp.or.alpha.lt.50.0_dp) then
      nkaddx = 5
      nkaddy = 1
    elseif (alpha.gt.120.0_dp.or.alpha.lt.60.0_dp) then
      nkaddx = 4
      nkaddy = 1
    elseif (alpha.gt.110.0_dp.or.alpha.lt.70.0_dp) then
      nkaddx = 3
      nkaddy = 1
    else
      nkaddx = 2
      nkaddy = 1
    endif
    nkaddz = 0
  elseif (ndim.eq.1) then
    nkaddx = 1
    nkaddy = 0
    nkaddz = 0
  endif
!
!  Calculate and store k-vector indices as (e+40)*6400+(f+40)*80+g+40
!
  if (lra) then
    max1l = 0
    max1u = rradmax*rkk1 + 1
    max1l1 = 1
  else
    if (ndim.eq.3) then
      call uncell3D(kv,ak,bk,ck,alphak,betak,gammak)
      if (betak.eq.90.0.and.gammak.eq.90.0) then
        nkangle = 1
        max1l = 0
        max1u = rradmax*rkk1 + nkaddx
        max1l1 = 1
      elseif (alphak.eq.90.0.and.gammak.eq.90.0) then
        nkangle = 2
        max1u = rradmax*rkk1 + nkaddx
        max1l = max1u
        max1l1 = max1l + 1
      elseif (alphak.eq.90.0.and.betak.eq.90.0) then
        nkangle = 3
        max1u = rradmax*rkk1 + nkaddx
        max1l = max1u
        max1l1 = max1l + 1
      else
        nkangle = 1
        max1l = 0
        max1u = rradmax*rkk1 + nkaddx
        max1l1 = 1
      endif
    elseif (ndim.eq.2) then
      call uncell2D(kv,ak,bk,alphak)
      nkangle = 1
      max1l = 0
      max1u = rradmax*rkk1 + nkaddx
      max1l1 = max1l + 1
    elseif (ndim.eq.1) then
      nkangle = 1
      max1l = 0
      max1u = rradmax*rkk1 + nkaddx
      max1l1 = 1
    endif
  endif
  if (max1l.gt.40.or.max1u.gt.40) then
    if (ioproc) then
      write(ioout,'(/)')
      write(ioout,'(''  **** Too many reciprocal lattice vectors needed ****'')')
      write(ioout,'(''  **** Probably due to unphysical cell dimensions ****'')')
      if (ndim.eq.3) then
        call uncell3D(rv,a,b,c,alpha,beta,gamma)
        write(ioout,'(/,''  Current cell parameters : '',/)')
        write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
        write(ioout,'(''    b = '',f8.4,''    beta  = '',f8.4)') b,beta
        write(ioout,'(''    c = '',f8.4,''    gamma = '',f8.4)') c,gamma
      elseif (ndim.eq.2) then
        call uncell2D(rv,a,b,alpha)
        write(ioout,'(/,''  Current cell parameters : '',/)')
        write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
        write(ioout,'(''    b = '',f8.4)') b
      elseif (ndim.eq.1) then
        write(ioout,'(/,''  Current cell parameter : '',/)')
        write(ioout,'(''    a = '',f8.4)') rv(1,1)
      endif
    endif
    call stopnow('kindex')
  endif
!
!  Do loops!
!
  nkvec = 0
  rrmx2 = rradmax*rradmax
  if (lra) then
    xk = - max1l1*kvx
    do e = - max1l,max1u
      xk = xk + kvx
!
      perpk = rrmx2 - xk*xk
      if (perpk.ge.0.0_dp) then
        perpk = sqrt(perpk)
        max2u = perpk*rkk2 + nkaddy
        max2l = max2u
        max2l1 = max2l + 1
!
        if (max2l.gt.40.or.max2u.gt.40) then
          if (ioproc) then
            write(ioout,'(/)')
            write(ioout,'(''  **** Too many reciprocal lattice vectors needed ****'')')
            write(ioout,'(''  **** Probably due to unphysical cell dimensions ****'')')
            if (ndim.eq.3) then
              call uncell3D(rv,a,b,c,alpha,beta,gamma)
              write(ioout,'(/,''  Current cell parameters : '',/)')
              write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
              write(ioout,'(''    b = '',f8.4,''    beta  = '',f8.4)') b,beta
              write(ioout,'(''    c = '',f8.4,''    gamma = '',f8.4)') c,gamma
            elseif (ndim.eq.2) then
              call uncell2D(rv,a,b,alpha)
              write(ioout,'(/,''  Current cell parameters : '',/)')
              write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
              write(ioout,'(''    b = '',f8.4)') b 
            endif
          endif
          call stopnow('kindex')
        endif
!
        xk2 = xk*xk
        yk = - max2l1*kvy
        ppk = perpk
        do f = - max2l,max2u
          yk = yk + kvy
!
          perpk = ppk*ppk - yk*yk
          if (perpk.ge.0.0_dp) then
            perpk = sqrt(perpk)
            max3u = perpk*rkk3 + nkaddz
            max3l = max3u
            max3l1 = max3l + 1
!
            if (max3l.gt.40.or.max3u.gt.40) then
              if (ioproc) then
                write(ioout,'(/)')
                write(ioout,'(''  **** Too many reciprocal lattice vectors needed ****'')')
                write(ioout,'(''  **** Probably due to unphysical cell dimensions ****'')')
                call uncell3D(rv,a,b,c,alpha,beta,gamma)
                write(ioout,'(/,''  Current cell parameters : '',/)')
                write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)')a,alpha
                write(ioout,'(''    b = '',f8.4,''    beta  = '',f8.4)')b,beta
                write(ioout,'(''    c = '',f8.4,''    gamma = '',f8.4)')c,gamma
              endif
              call stopnow('kindex')
            endif
!
            yk2 = yk*yk
            zk = - max3l1*kvz
            do g = - max3l,max3u
              zk = zk + kvz
              rk2 = xk2+yk2 + zk*zk
!
!  Test for zero wavevector and exclude vectors outside the search
!  region, given by rradmx.
!
              if (rk2.gt.zero.and.rk2.le.rrmx2) then
                nkvec = nkvec + 1
                if (nkvec.gt.maxkvec) then
                  maxkvec = nkvec + 100
                  call changemaxkvec
                endif
                indk(nkvec) = 6400*(e+40) + 80*(f+40) + g + 40
              endif
!
!  End of e,f,g loops
!
            enddo
          endif
        enddo
      endif
    enddo
  else
    xke = - max1l1*rk1x
    yke = - max1l1*rk1y
    zke = - max1l1*rk1z
    do e = - max1l,max1u
      xke = xke + rk1x
      yke = yke + rk1y
      zke = zke + rk1z
!
      if (nkangle.eq.2) then
        perpk = rrmx2 - (xke*xke + yke*yke + zke*zke)
        if (perpk.ge.0.0_dp) then
          perpk = sqrt(perpk)
          max2u = perpk*rkk2 + nkaddy
        else
          max2u = 0
        endif
        max2l = 0
        max2l1 = 1
      else
        projk = xke*ruk2x + yke*ruk2y + zke*ruk2z
        max2u = (rradmax - projk)*rkk2 + nkaddy
        max2l = (rradmax + projk)*rkk2 + nkaddy
        max2l1 = max2l + 1
      endif
!
      if (max2l.gt.40.or.max2u.gt.40) then
        if (ioproc) then
          write(ioout,'(/)')
          write(ioout,'(''  **** Too many reciprocal lattice vectors needed ****'')')
          write(ioout,'(''  **** Probably due to unphysical cell dimensions ****'')')
          if (ndim.eq.3) then
            call uncell3D(rv,a,b,c,alpha,beta,gamma)
            write(ioout,'(/,''  Current cell parameters : '',/)')
            write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
            write(ioout,'(''    b = '',f8.4,''    beta  = '',f8.4)') b,beta
            write(ioout,'(''    c = '',f8.4,''    gamma = '',f8.4)') c,gamma
          elseif (ndim.eq.2) then
            call uncell2D(rv,a,b,alpha)
            write(ioout,'(/,''  Current cell parameters : '',/)')
            write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
            write(ioout,'(''    b = '',f8.4)') b 
          endif
        endif
        call stopnow('kindex')
      endif
!
      xkf = xke - max2l1*rk2x
      ykf = yke - max2l1*rk2y
      zkf = zke - max2l1*rk2z
      do f = - max2l,max2u
        xkf = xkf + rk2x
        ykf = ykf + rk2y
        zkf = zkf + rk2z
!
        projk = xkf*ruk3x + ykf*ruk3y + zkf*ruk3z
        perpk = xkf*xkf + ykf*ykf + zkf*zkf
        perpk = perpk - projk*projk
        perpk = rrmx2 - perpk
        if (perpk.ge.0.0_dp) then
          perpk = sqrt(perpk)
          if (nkangle.eq.3) then
            max3u = perpk*rkk3 + nkaddz
            max3l = 0
            max3l1 = 1
          else
            max3u = (perpk - projk)*rkk3 + nkaddz
            max3l = (perpk + projk)*rkk3 + nkaddz
            max3l1 = max3l + 1
          endif
!
          if (max3l.gt.40.or.max3u.gt.40) then
            if (ioproc) then
              write(ioout,'(/)')
              write(ioout,'(''  **** Too many reciprocal lattice vectors needed ****'')')
              write(ioout,'(''  **** Probably due to unphysical cell dimensions ****'')')
              call uncell3D(rv,a,b,c,alpha,beta,gamma)
              write(ioout,'(/,''  Current cell parameters : '',/)')
              write(ioout,'(''    a = '',f8.4,''    alpha = '',f8.4)') a,alpha
              write(ioout,'(''    b = '',f8.4,''    beta  = '',f8.4)') b,beta
              write(ioout,'(''    c = '',f8.4,''    gamma = '',f8.4)') c,gamma
            endif
            call stopnow('kindex')
          endif
!
          xk = xkf - max3l1*rk3x
          yk = ykf - max3l1*rk3y
          zk = zkf - max3l1*rk3z
          do g = - max3l,max3u
            xk = xk + rk3x
            yk = yk + rk3y
            zk = zk + rk3z
            rk2 = xk*xk + yk*yk + zk*zk
!
!  Test for zero wavevector and exclude vectors outside the search
!  region, given by rradmx.
!
            if (rk2.gt.zero.and.rk2.le.rrmx2) then
              nkvec = nkvec + 1
              if (nkvec.gt.maxkvec) then
                maxkvec = nkvec + 100
                call changemaxkvec
              endif
              indk(nkvec) = 6400*(e+40) + 80*(f+40) + g + 40
            endif
!
!  End of e,f,g loops
!
          enddo
        endif
      enddo
    enddo
  endif
!
!  Check that K vector arrays are at least numat long
!
  if (numat.gt.maxkvec) then
    maxkvec = numat
    call changemaxkvec
  endif
!
  time2 = cputime()
  tion = tion + time2 - time1
!
  return
  end
