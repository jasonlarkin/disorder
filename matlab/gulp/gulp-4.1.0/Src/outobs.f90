  subroutine outobs(ncfold)
!
!  Output observables for fitting
!
!   8/96 Bulk/shear modulus added 
!   3/98 Flag for phonons now set if Cv or entropy are present.
!   8/98 Refractive index added
!   8/01 Piezoelectric constant output modified
!   3/02 Born effective charges added
!   5/02 Frequency K point output/checks added
!   6/05 Intent added
!   6/05 Check on valid phonon mode number added
!   6/05 Monopole charges added 
!   7/06 Bug in phonon mode trapping fixed
!   5/07 Error in outputing of Born effective charge observables corrected
!   7/07 Bug corrected in checking of number of cores when this differs
!        between configurations
!   4/08 ReaxFF charges, bond lengths and bond angles added 
!   4/08 Reaction energies added
!   6/08 lfcprop not turned on with phonons when using finite differences
!   3/09 lkptdispersion added
!   1/10 Young's moduli and Poisson's ratios added as observables
!   3/10 lfcprop turned on for Young's moduli and Poisson's ratio
!   7/10 Coordination number added as an observable
!   9/10 S(Q,omega) added as an observable
!   6/12 Mode observable added
!   5/13 Volume observable added
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
!  Julian Gale, NRI, Curtin University, May 2013
!
  use configurations
  use control
  use current
  use fitting
  use ksample
  use iochannels
  use observables
  use parallel
  use shell,       only : ncore
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(inout) :: ncfold
!
!  Local variables
!
  character(len=5)           :: namecell(6)
  character(len=1)           :: namecrd(4)
  integer(i4)                :: i
  integer(i4)                :: idj
  integer(i4)                :: idk
  integer(i4)                :: ind
  integer(i4)                :: k
  integer(i4)                :: mvar
  integer(i4)                :: ncflast
  integer(i4)                :: ni
  integer(i4)                :: ninsert
  integer(i4)                :: nj
  integer(i4)                :: nk
  integer(i4)                :: nkncf
  integer(i4)                :: nptr
  integer(i4)                :: nt
!
  data namecell/'a    ','b    ','c    ','alpha','beta','gamma'/
  data namecrd/'x','y','z','r'/
!********************************************
!  Output table of observables and weights  *
!********************************************
  if (ioproc) then
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''   Observable no.  Type           Observable    Weight       Reference  Confign '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  ncflast = ncf
  do i = 1,nobs
    nt = nobtyp(i)
    ncf = nobcfg(i)
    if (ncf.gt.0) then
!
!  Update configration info if needed
!
      if (ncf.ne.ncflast) then
        call setup(.true.)
        ncflast = ncf
      endif
      if (nt.ge.3.and.nt.le.5) lfcprop(ncf) = .true.
      if (nt.eq.7.or.nt.eq.8) lfcprop(ncf) = .true.
      if (nt.eq.11.or.nt.eq.12) lfcprop(ncf) = .true.
      if (nt.eq.15.or.nt.eq.16) lfcprop(ncf) = .true.
      if (nt.eq.25.or.nt.eq.26) lfcprop(ncf) = .true.
      if (nt.eq.17) lfcprop(ncf) = .true.
      if (nt.eq.17) lfcscatter(ncf) = .true.
      if (nt.eq.18) lfcborn(ncf) = .true.
      if (nt.eq.9.or.nt.eq.13.or.nt.eq.14.or.nt.eq.29) then
        lfcphon(ncf) = .true.
        if (ndimen(ncf).eq.3.and..not.lnoanald2) then
!
!  Turn on calculation of Born charges and properties
!  in case they are needed for phonons
!
          lfcborn(ncf) = .true.
          lfcprop(ncf) = .true.
        endif
!
!  Check that mode number is within allowed range
!
        if (nt.eq.9.and.(nobptr(i).lt.1.or.nobptr(i).gt.3*ncore)) then
          call outerror('Phonon mode specified for fitting is outside allowed range',0_i4)
          call stopnow('outobs')
        endif
        if (nt.eq.29.and.(nobptr(i).lt.1.or.nobptr(i).gt.nobsmode)) then
          call outerror('Phonon mode specified for fitting is outside allowed range',0_i4)
          call stopnow('outobs')
        endif
!
!  If not 0-D check k-point is specified, otherwise use Gamma point
!
        if (ndimen(ncf).gt.0) then
          k = 0
          ninsert = 0
          nkncf = 0
          do while (k.lt.nkpt)
            k = k + 1
            if (nkptcfg(k).eq.ncf) nkncf = nkncf + 1
            if (nkptcfg(k).eq.ncf-1) ninsert = k + 1
          enddo
          if (nobptr2(i).gt.nkncf) then
            call outerror('observable K point pointer exceeds no. of points for structure',0_i4)
            call stopnow('outobs')
          endif
          if (nkncf.eq.0) then
            do k = nkpt,ninsert,-1
              xkpt(k+1) = xkpt(k)
              ykpt(k+1) = ykpt(k)
              zkpt(k+1) = zkpt(k)
              wkpt(k+1) = wkpt(k)
              nkptcfg(k+1) = nkptcfg(k)
              lkptdispersion(k+1) = lkptdispersion(k)
            enddo
            nkpt = nkpt + 1
            xkpt(ninsert) = 0.0_dp
            ykpt(ninsert) = 0.0_dp
            zkpt(ninsert) = 0.0_dp
            wkpt(ninsert) = 1.0_dp
            nkptcfg(ninsert) = ncf
            lkptdispersion(ninsert) = .false.
          endif
        endif
      endif
      if (ncf.ne.ncfold) then
        ncfold = ncf
        call setup(.true.)
      endif
    endif
    if (ioproc) then
      mvar = 3*nasym + nstrains
!
!  Energy 
!
      if (nt.eq.1) then
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,11x,i4)')i,nameobs(nt),fobs(i),weight(i),nobcfg(i)
!
!  Derivatives
!
      elseif (nt.eq.2) then
        nptr = nobptr(i)
        ind = iopt(nptr)
        if (ind.le.nstrains) then
!
!  Cell derivative
!
!  Correction for rhombohedral setting
          if ((ifhr(ncf).eq.1.or.ndim.eq.2).and.ind.eq.3.and.lrelax) ind = 4
!
          write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,2x,a5,4x,i4)') &
            i,nameobs(nt),fobs(i),weight(i),namecell(ind),nobcfg(i)
        elseif (ind.le.mvar) then
!
!  Internal derivative
!
          nj = (ind - (nstrains-2))/3
          idj = ind - (3*nj+(nstrains-3))
          write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,1x,a,6x,i4)') &
            i,nameobs(nt),fobs(i),weight(i),nj,namecrd(idj),nobcfg(i)
        else
!
!  Radial derivative
!
          nj = (ind - mvar)
          write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,1x,a,6x,i4)') &
            i,nameobs(nt),fobs(i),weight(i),nj,namecrd(4),nobcfg(i)
        endif
!
!  Elastic constants
!
      elseif (nt.eq.3) then
        ni = nobptr(i)/7
        nj = nobptr(i) - 7*ni
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,2i3,5x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nj,nobcfg(i)
!
!  Piezoelectric constants
!
      elseif (nt.eq.7.or.nt.eq.8) then
        ni = nobptr(i)/7
        nj = nobptr(i) - 7*ni
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,2x,a1,5x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,namecrd(nj),nobcfg(i)
!
!  Dielectric constants
!
      elseif (nt.eq.4.or.nt.eq.5) then
        ni = nobptr(i)/4
        nj = nobptr(i) - 4*ni
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,2i3,5x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nj,nobcfg(i)
!
!  Structural parameters
!
      elseif (nt.eq.6) then
        nptr = nobptr(i)
        ind = iopt(nptr)
        if (ind.le.nstrains) then
!
!  Cell strain
!
          write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,2x,a5,4x,i4)') &
            i,nameobs(nt),fobs(i),weight(i),namecell(ind),nobcfg(i)
        elseif (ind.le.mvar) then
!
!  Internal fractional coordinate
!
          nj = (ind-(nstrains-2))/3
          idj = ind - (3*nj+(nstrains-3))
          write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,1x,a,6x,i4)') &
            i,nameobs(nt),fobs(i),weight(i),nj,namecrd(idj),nobcfg(i)
        else
!
!  Radial derivative
!
          nj = (ind - mvar)
          write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,1x,a,6x,i4)') &
            i,nameobs(nt),fobs(i),weight(i),nj,namecrd(4),nobcfg(i)
        endif
!
!  Vibrational frequency
!
      elseif (nt.eq.9) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,1x,i4,3x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),nobptr2(i),ni,nobcfg(i)
!
!  Electrostatic potential
!
      elseif (nt.eq.10) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nobcfg(i)
!
!  Bulk/shear modulus
!
      elseif (nt.eq.11.or.nt.eq.12) then
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,13x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),nobcfg(i)
!
!  Heat Capacity (Cv)/entropy
!
      elseif (nt.eq.13.or.nt.eq.14) then
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,13x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),nobcfg(i)
!
!  Refractive indices
!
      elseif (nt.eq.15.or.nt.eq.16) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nobcfg(i)
!
!  S(Q,omega)
!
      elseif (nt.eq.17) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nobcfg(i)
!
!  Born effective charges
!
      elseif (nt.eq.18) then
        nptr = nobptr(i)
        ind = iopt(nptr)
        ni = (nobptr(i)-1)/9 + 1
        idj = nobptr(i) - 9*(ni-1)
        idk = (idj-1)/3 + 1
        idj = idj - 3*(idk-1)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,1x,2a1,5x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,namecrd(idj),namecrd(idk),nobcfg(i)
!
!  Monopole charges
!
      elseif (nt.eq.19) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nobcfg(i)
!
!  ReaxFF charges
!
      elseif (nt.eq.21) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nobcfg(i)
!
!  Bond length
!
      elseif (nt.eq.22) then
        ni = nobptr(i)
        nj = nobptr2(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,1x,i3,4x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nj,nobcfg(i)
!
!  Bond angle
!
      elseif (nt.eq.23) then
        ni = nobptr(i)
        nj = nobptr2(i)
        nk = nobptr3(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,1x,i3,1x,i3,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nj,nk,nobcfg(i)
!
!  Reaction energy 
!
      elseif (nt.eq.24) then
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,11x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),nobcfg(i)
!
!  Young's moduli
!
      elseif (nt.eq.25) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,4x,a1,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),namecrd(ni),nobcfg(i)
!
!  Poisson's ratios
!
      elseif (nt.eq.26) then
        if (nobptr(i).eq.1) then
          ni = 1
          nj = 2
        elseif (nobptr(i).eq.2) then
          ni = 1
          nj = 3
        else
          ni = 2
          nj = 3
        endif
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,4x,2a1,7x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),namecrd(ni),namecrd(nj),nobcfg(i)
!
!  Coordination number
!
      elseif (nt.eq.27) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,1x,i4,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),ni,nobcfg(i)
!
!  Vibrational mode
!
      elseif (nt.eq.29) then
        ni = nobptr(i)
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,2x,i3,1x,i4,3x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),nobptr2(i),ni,nobcfg(i)
!
!  Volume
!
      elseif (nt.eq.30) then
        write(ioout,'(7x,i4,8x,a13,1x,f12.6,2x,f12.4,5x,8x,i4)') &
          i,nameobs(nt),fobs(i),weight(i),nobcfg(i)
      endif
    endif
  enddo
  if (ioproc) then
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
  return
  end
