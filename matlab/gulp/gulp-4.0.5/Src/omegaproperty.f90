  subroutine omegaproperty(modes,freq,fscale,oscstrength)
!
!  Subroutine calculates properties that depend on the applied 
!  frequency.
!
!   3/02 Created
!  11/04 Pi accessed from module
!   2/06 Modified to handle output for case where omegadir is not set
!   2/06 Option to input fractional omegadir added
!   3/06 Reflectivity directions signs adjusted to ensure that long wavelength
!        limit is positive
!   3/06 Complex arithmetic introduced for reflectivity
!   3/06 Damping factor added to frequency dependent expression
!   3/06 Option to output oscillator strengths to a file added
!
!  On input : 
!
!  modes       = number of modes
!  freq        = frequencies of modes
!  fscale      = factor that converts frequencies to wavenumbers
!                from (eV/(Angs**2.kg))**1/2
!  oscstrength = oscillator strengths of each mode
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
!  Julian Gale, NRI, Curtin University, March 2006
!
  use configurations
  use constants
  use current
  use files,         only : losc, oscfile
  use iochannels
  use properties
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: modes
  real(dp),    intent(in)  :: freq(modes)
  real(dp),    intent(in)  :: fscale
  real(dp),    intent(in)  :: oscstrength(3,3,modes)
!
!  Local variables
!
  complex(dpc)             :: cepsomega
  complex(dpc)             :: dlcomega(3,3)
  complex(dpc)             :: freqfactor
  integer(i4)              :: ia
  integer(i4)              :: ib
  integer(i4)              :: iom
  integer(i4)              :: iout
  integer(i4)              :: m
  logical                  :: lomegadir
  real(dp)                 :: convert
  real(dp)                 :: komega
  real(dp)                 :: nomega
  real(dp)                 :: ome
  real(dp)                 :: omegadirloc(6)
  real(dp)                 :: onorm1
  real(dp)                 :: onorm2
  real(dp)                 :: reflectivity
  real(dp)                 :: volume
!
!  Set up local constants
!
!  First check whether input and output omega directions have been output
!
  lomegadir = ((abs(omegadir(1,ncf))+abs(omegadir(2,ncf))+ &
                abs(omegadir(3,ncf))).gt.0.0_dp.and. &
               (abs(omegadir(4,ncf))+abs(omegadir(5,ncf))+ &
                abs(omegadir(6,ncf))).gt.0.0_dp)
  convert = 4.0_dp*pi*angstoev*(fscale**2)/volume(rv)
  if (lomegadir) then
!
!  If fractional form of omegadir has been used then convert to Cartesian space
!
    if (omegadirtype(ncf).eq.2) then
      if (ndim.eq.3) then
        omegadirloc(1) = omegadir(1,ncf)*r1x + omegadir(2,ncf)*r2x + omegadir(3,ncf)*r3x
        omegadirloc(2) = omegadir(1,ncf)*r1y + omegadir(2,ncf)*r2y + omegadir(3,ncf)*r3y
        omegadirloc(3) = omegadir(1,ncf)*r1z + omegadir(2,ncf)*r2z + omegadir(3,ncf)*r3z
        omegadirloc(4) = omegadir(4,ncf)*r1x + omegadir(5,ncf)*r2x + omegadir(6,ncf)*r3x
        omegadirloc(5) = omegadir(4,ncf)*r1y + omegadir(5,ncf)*r2y + omegadir(6,ncf)*r3y
        omegadirloc(6) = omegadir(4,ncf)*r1z + omegadir(5,ncf)*r2z + omegadir(6,ncf)*r3z
      elseif (ndim.eq.2) then
        omegadirloc(1) = omegadir(1,ncf)*r1x + omegadir(2,ncf)*r2x
        omegadirloc(2) = omegadir(1,ncf)*r1y + omegadir(2,ncf)*r2y
        omegadirloc(3) = omegadir(3,ncf)
        omegadirloc(4) = omegadir(4,ncf)*r1x + omegadir(5,ncf)*r2x
        omegadirloc(5) = omegadir(4,ncf)*r1y + omegadir(5,ncf)*r2y
        omegadirloc(6) = omegadir(6,ncf)
      elseif (ndim.eq.1) then
        omegadirloc(1) = omegadir(1,ncf)*r1x
        omegadirloc(2) = omegadir(2,ncf)
        omegadirloc(3) = omegadir(3,ncf)
        omegadirloc(4) = omegadir(4,ncf)*r1x
        omegadirloc(5) = omegadir(5,ncf)
        omegadirloc(6) = omegadir(6,ncf)
      else
        omegadirloc(1:6) = omegadir(1:6,ncf)
      endif
    else
      omegadirloc(1:6) = omegadir(1:6,ncf)
    endif
!
!  Normalise directions
!
    onorm1 = omegadirloc(1)**2 + omegadirloc(2)**2 + omegadirloc(3)**2
    onorm2 = omegadirloc(4)**2 + omegadirloc(5)**2 + omegadirloc(6)**2
    onorm1 = sqrt(onorm1)
    onorm2 = sqrt(onorm2)
    omegadirloc(1) = omegadirloc(1)/onorm1
    omegadirloc(2) = omegadirloc(2)/onorm1
    omegadirloc(3) = omegadirloc(3)/onorm1
    omegadirloc(4) = omegadirloc(4)/onorm2
    omegadirloc(5) = omegadirloc(5)/onorm2
    omegadirloc(6) = omegadirloc(6)/onorm2
!
!  If directions are opposite then change sign to obtain correct behaviour in the reflectivity
!
    onorm1 = omegadirloc(1)*omegadirloc(4) + omegadirloc(2)*omegadirloc(5) + omegadirloc(3)*omegadirloc(6)
    if (onorm1.lt.0.0_dp) then
      omegadirloc(4) = - omegadirloc(4)
      omegadirloc(5) = - omegadirloc(5)
      omegadirloc(6) = - omegadirloc(6)
    endif
  endif
!  
!  Write out oscillator strengths
!  
  if (losc) then
    iout = 36
    open(iout,file=oscfile,status='unknown',position='append',err=100)
    write (iout,'(''oscillator strengths and phonon frequencies'',i6)') modes
    do m = 1,modes
      write (iout,'(i5,2x,e18.10)') m, freq(m)
      do ia = 1,3
        do ib = 1,3
          write(iout,'(2(1x,i5),1x,e18.10)') ia,ib,convert*oscstrength(ib,ia,m)
        enddo
      enddo
    enddo
    write(iout,'(''high frequency dielectric constant tensor'')')
    write(iout,'(1x,3e20.10)') (diconh(1,ia),ia=1,3)
    write(iout,'(1x,3e20.10)') (diconh(2,ia),ia=1,3)
    write(iout,'(1x,3e20.10)') (diconh(3,ia),ia=1,3)
100    continue
    close(iout)
  endif
!
!  Output header for frequency dependent properties
!
  write(ioout,'(/,''  Frequency dependent properties : '',/)')
  write(ioout,'(''  Damping factor = '',f12.6,'' cm-1 '',/)') omegadamping(ncf)
  write(ioout,'(''-------------------------------------------------------------------------------'')')
  if (lomegadir) then
    write(ioout,'(''  Omega              Dielectric tensor                          Reflectivity'')')
  else
    write(ioout,'(''  Omega              Dielectric tensor                                      '')')
  endif
  write(ioout,'(''  (cm-1)     11       12       22       13       23       33'')')
  write(ioout,'(''-------------------------------------------------------------------------------'')')
!
!  Loop over omega range
!
!  ome = current value of omega
!
  ome = omega(ncf)
  do iom = 1,nomegastep(ncf)+1
!
!  Calculate frequency-dependent dielectric constant
!
    dlcomega(1:3,1:3) = cmplx(diconh(1:3,1:3),0.0_dp)
    do m = 1,modes
      freqfactor = cmplx(freq(m)**2 - ome**2,-omegadamping(ncf)*ome)
      freqfactor = 1.0_dp/freqfactor
      do ia = 1,3
        do ib = 1,3
          dlcomega(ib,ia) = dlcomega(ib,ia) + convert*freqfactor*oscstrength(ib,ia,m)
        enddo
      enddo
    enddo
!
!  Calculate reflectivity
!
    reflectivity = 0.0_dp
    if (lomegadir) then
      cepsomega = omegadirloc(1)*dlcomega(1,1)*omegadirloc(4) + &
                  omegadirloc(2)*dlcomega(2,1)*omegadirloc(4) + &
                  omegadirloc(3)*dlcomega(3,1)*omegadirloc(4) + &
                  omegadirloc(1)*dlcomega(1,2)*omegadirloc(5) + &
                  omegadirloc(2)*dlcomega(2,2)*omegadirloc(5) + &
                  omegadirloc(3)*dlcomega(3,2)*omegadirloc(5) + &
                  omegadirloc(1)*dlcomega(1,3)*omegadirloc(6) + &
                  omegadirloc(2)*dlcomega(2,3)*omegadirloc(6) + &
                  omegadirloc(3)*dlcomega(3,3)*omegadirloc(6)
      cepsomega = sqrt(cepsomega)
      nomega = dble(cepsomega)
      komega = aimag(cepsomega)
      reflectivity = ((1.0_dp - nomega)**2 + komega**2)/((1.0_dp + nomega)**2 + komega**2)
    endif
!
!  Output results
!
    if (lomegadir) then
      write(ioout,'(f8.2,6(1x,f8.4),1x,f10.4)') ome, &
        dble(dlcomega(1,1)),dble(dlcomega(2,1)),dble(dlcomega(2,2)), &
        dble(dlcomega(3,1)),dble(dlcomega(3,2)),dble(dlcomega(3,3)), &
        reflectivity
    else
      write(ioout,'(f8.2,6(1x,f8.4))') ome, &
        dble(dlcomega(1,1)),dble(dlcomega(2,1)),dble(dlcomega(2,2)), &
        dble(dlcomega(3,1)),dble(dlcomega(3,2)),dble(dlcomega(3,3))
    endif
!
!  Increment omega value for next point
!
    ome = ome + omegastep(ncf)
!
!  End loop over omega range
!
  enddo
!
!  Output footer
!
  write(ioout,'(''-------------------------------------------------------------------------------'',/)')
!
  return
  end
