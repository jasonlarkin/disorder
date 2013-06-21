  subroutine fitfun(n,xc,fsumsq)
!
!  Supplies the sum of the squares of the differences
!
!  nfcf = configuration pointer; 0 = all configurations to be included
!                                n = only configuration n to be used
!
!   6/95 Use of symmetry in second derivative generation allowed for
!   6/95 Handling of lsymderv2 added so that relax fit optimisations
!        can use symmetrised second derivatives
!   8/95 Bulk/shear modulus added 
!   3/98 Heat capcity and entropy added as observables
!   8/98 Refractive indices added
!   3/02 Born effective charges added
!   5/02 K point pointer added for frequencies
!  11/04 Intent added
!   6/05 Monopole charge added as an observable
!   9/05 rv matrix transposed during conversion of Cartesian forces
!        to fractional in conventional fitting
!  11/06 Constraint handling corrected by only referencing gc when
!        lfound is true
!  12/07 Unused variables removed
!   4/08 ReaxFF charges, bond lengths and bond angles added
!   4/08 Reaction energy added
!   6/08 Structure/derivatives now handled before property calculation
!        to avoid corruption by finite difference procedure.
!   9/08 Position of cell reset moved until after properties for relax
!        fitting.
!  10/08 Structure reset for relaxed fitting moved to the end and fbond
!        fangle options now use rv rather than rvcfg
!   5/09 Calls to x0tostr routines moved from energy to calling routine
!  11/09 Modified to allow EVB use during fitting
!   1/10 Youngs moduli and Poisson ratios added
!   7/10 Coordination number added as an observable
!   9/10 S(Q,omega) added as an observable
!   9/11 ndimen added to arguments of nearestr
!   6/12 Vibrational mode added as an observable
!   6/12 Phonon call modified
!   7/12 finfo array added for mode observable
!  11/12 Unused argument removed
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
  use constants,        only : radtodeg
  use control
  use current
  use derivatives
  use fitting
  use frequencies
  use observables
  use potentialpoints
  use properties
  use reaxFFdata,       only : qreaxff
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(in)                  :: n
  real(dp),     intent(out)                 :: fsumsq
  real(dp),     intent(in)                  :: xc(*)
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: idj
  integer(i4)                               :: iflag
  integer(i4)                               :: ilower
  integer(i4)                               :: ind
  integer(i4)                               :: indf
  integer(i4)                               :: indv
  integer(i4)                               :: iupper
  integer(i4)                               :: iv
  integer(i4)                               :: j
  integer(i4)                               :: kk
  integer(i4)                               :: mvar
  integer(i4)                               :: nc
  integer(i4),                         save :: ncfold = 0
  integer(i4)                               :: neq
  integer(i4)                               :: ni
  integer(i4)                               :: nj
  integer(i4)                               :: nk
  integer(i4)                               :: nobsmodeptr0
  integer(i4)                               :: nobt
  integer(i4)                               :: nptr
  integer(i4)                               :: nr
  integer(i4)                               :: nv
  integer(i4)                               :: status
  logical                                   :: lfgrad
  logical                                   :: lfound
  logical,                             save :: lsymd2save
  logical                                   :: lusenumericd2
  real(dp)                                  :: aa
  real(dp)                                  :: alp
  real(dp)                                  :: bb
  real(dp)                                  :: bet
  real(dp)                                  :: cc
  real(dp)                                  :: costheta
  real(dp)                                  :: cputime
  real(dp)                                  :: deltim
  real(dp)                                  :: dummy
  real(dp)                                  :: fc
  real(dp)                                  :: gam
  real(dp), dimension(:), allocatable       :: gc
  real(dp)                                  :: gt
  real(dp)                                  :: rji
  real(dp)                                  :: rki
  real(dp)                                  :: rvt(3,3)
  real(dp)                                  :: theta
  real(dp)                                  :: tim1
  real(dp)                                  :: tim2
  real(dp)                                  :: time1
  real(dp)                                  :: time2
  real(dp)                                  :: volume
  real(dp), dimension(:), allocatable       :: xcfull
  real(dp)                                  :: xdv
  real(dp)                                  :: ydv
  real(dp)                                  :: zdv
  real(dp)                                  :: xji
  real(dp)                                  :: yji
  real(dp)                                  :: zji
  real(dp)                                  :: xki
  real(dp)                                  :: yki
  real(dp)                                  :: zki
!
  time1 = cputime()
!
!  Allocate local memory
!
  allocate(gc(maxvar),stat=status)
  if (status/=0) call outofmemory('fitfun','gc')
  allocate(xcfull(nfit),stat=status)
  if (status/=0) call outofmemory('fitfun','xcfull')
!
!  Set flag to indicate whether numerical second derivatives should be used for properties if called for
!
  lusenumericd2 = (lnoanald2.or.index(keyword,'nume').ne.0)
!
!  Expand parameters to full set
!
  do i = 1,nfitt
    xcfull(nfitptr(i)) = xc(i)
  enddo
!
!  Set parameters
!
  call fitcon(xcfull)
  call putpar(xcfull,ncfold,dummy,.false.)
!
!  Zero calculated properties array
!
  do i = 1,nobs
    fcalc(i) = 0.0_dp
    finfo(i) = 0.0_dp
  enddo
!**************************************
!  Start of loop over configurations  *
!**************************************
  deltim = 0.0_dp
  if (nfcf.eq.0) then
    ilower = 1
    iupper = ncfg
  else
    ilower = nfcf
    iupper = nfcf
  endif
  nobsmodeptr0 = 0
  do nc = ilower,iupper
!
!  Switch in correct coordinates
!
    ncf = nc
    if (ncfold.ne.ncf) then
      ncfold = ncf
      call setup(.true.)
      lsymd2save = lsymderv2
    else
      lsymderv2 = lsymd2save
    endif
    mvar = 3*nasym + nstrains
!****************************************
!  If in relax mode optimise structure  *
!****************************************
    if (lrelax) then
      call optim(.false.,.false.,0)
    else
      do i = 1,nasym
        x0(3*i+nstrains-2) = xcfg(nsft+i)
        x0(3*i+nstrains-1) = ycfg(nsft+i)
        x0(3*i+nstrains) = zcfg(nsft+i)
      enddo
      if (nbsm.gt.0) then
        do i = 1,nasym
          x0(mvar+i) = radcfg(nsft+i)
        enddo
      endif
    endif
!************************************************************
!  Convert linear structure array to main structure arrays  *
!************************************************************
    if (lx0centroid) then
      call x0tostrcentroid
    else
      call x0tostr
    endif
!
    tim1 = cputime()
    lfirst = .true.
    lfgrad = (lfcborn(nc).or.lfcprop(nc).or.lfcphon(nc))
    if (lusenumericd2.and.lfgrad) then
!
!  Analytical gradients but numerical second derivatives
!
      iflag = 2
      call functnf(iflag,nvar,xc,fc,gc,.false.)
    else
!
!  Analytical gradients and second derivatives
!
      lsymderv2 = .false.
      call energy(fc,.true.,lfgrad)
!
!  Complete strain derivatives
!
      if (lstr) call strfin(lfgrad)
    endif
!
!  Surface energy
!
    if (lseok) call surfaceenergy(fc)
!************************************************
!  Collect derivatives / structure observables  *
!************************************************
    if (lrelax) then
!******************
!  Relax fitting  *
!******************
      if (ncell.gt.0) then
        if (ncbl.gt.1.and.ifhr(ncf).eq.0) then
!
!  In relax mode the change in the full unit cell must be
!  followed to avoid problems with inconsistences between
!  strain flags and the angle flags
!
          do i = 1,3
            rvt(1,i) = rv(1,i)
            rvt(2,i) = rv(2,i)
            rvt(3,i) = rv(3,i)
          enddo
          call uncentre(rvt)
          call uncell3D(rvt,aa,bb,cc,alp,bet,gam)
        else
          aa = a
          bb = b
          cc = c
          alp = alpha
          bet = beta
          gam = gamma
        endif
      endif
      do i = 1,nvar
        ind = iopt(i)
        if (ind.gt.nstrains) then
          gc(i) = x0(ind)
        else
          if (ind.eq.1) then
            gc(i) = aa
          elseif (ind.eq.2) then
            gc(i) = bb
          elseif (ind.eq.3) then
!
!  For rhombohedral space groups, input in the rhombohedral setting the
!  third cell observable must be alpha instead of c for relax fitting.
!  This is also true for 2-D systems.
!
            if (ifhr(ncf).eq.1.or.ndimen(ncf).eq.2) then
              gc(i) = alp
            else
              gc(i) = cc
            endif
          elseif (ind.eq.4) then
            gc(i) = alp
          elseif (ind.eq.5) then
            gc(i) = bet
          else
            gc(i) = gam
          endif
        endif
      enddo
    else
!*******************
!  Static fitting  *
!*******************
!
!  If symmetry is being used correct internal first derivatives
!
      if (lsymopt.and.(.not.lsymderv.or.lfcprop(nc))) then
        do i = 1,nasym
          nr = nrel2(i)
          neq = neqv(i)
          xdrv(i) = neq*xdrv(nr)
          ydrv(i) = neq*ydrv(nr)
          zdrv(i) = neq*zdrv(nr)
          if (lbsmat(i+nsft)) raderv(i) = neq*raderv(nr)
        enddo
      endif
!
!  Generate full unit cell for derivatives
!
!  Transform cartesian derivatives to internal coords
!  Dot product with unit vectors, then scale the magnitude of
!  vector to change from per angstrom per frac
!
!
      if (ndimen(ncf).eq.3) then
        do i = 1,3
          rvt(1,i) = rv(1,i)
          rvt(2,i) = rv(2,i)
          rvt(3,i) = rv(3,i)
        enddo
        if (ncbl.gt.1.and.ifhr(ncf).eq.0) call uncentre(rvt)
        do kk = 1,nasym
          xdv = xdrv(kk)
          ydv = ydrv(kk)
          zdv = zdrv(kk)
          xdrv(kk) = xdv*rvt(1,1) + ydv*rvt(2,1) + zdv*rvt(3,1)
          ydrv(kk) = xdv*rvt(1,2) + ydv*rvt(2,2) + zdv*rvt(3,2)
          zdrv(kk) = xdv*rvt(1,3) + ydv*rvt(2,3) + zdv*rvt(3,3)
        enddo
      elseif (ndimen(ncf).eq.2) then
        do kk = 1,nasym
          xdv = xdrv(kk)
          ydv = ydrv(kk)
          xdrv(kk) = xdv*rv(1,1) + ydv*rv(2,1)
          ydrv(kk) = xdv*rv(1,2) + ydv*rv(2,2)
        enddo
      elseif (ndimen(ncf).eq.1) then
        do kk = 1,nasym
          xdv = xdrv(kk)
          xdrv(kk) = xdv*rv(1,1)
        enddo
      endif
!
!  Collect internal derivatives
!
      do i = ncell+1,nvar
        ind = iopt(i)
        if (ind.le.mvar) then
          nj = (ind-(nstrains-2))/3
          idj = ind-(3*nj+(nstrains-3))
          if (idj.eq.1) then
            gc(i) = xdrv(nj)
          elseif (idj.eq.2) then
            gc(i) = ydrv(nj)
          else
            gc(i) = zdrv(nj)
          endif
        else
          gc(i) = raderv(ind-mvar)
        endif
      enddo
!****************************
!  Constrained derivatives  *
!****************************
      if (ncon.gt.0) then
        do i = 1,ncon
          indf = ncfix(i)
          indv = ncvar(i)
          if (indf.gt.mvar) then
!
!  Radial derivatives
!
            nv = indf - mvar
            lfound = .false.
            j = mvar
            do while (.not.lfound.and.j.le.n)
              j = j + 1
              if (indv.eq.iopt(j)) lfound = .true.
            enddo
            gc(j) = gc(j) + raderv(nv)*conco(i)
          elseif (indf.gt.nstrains) then
!
!  Internal derivatives
!
            nv = (indf - (nstrains-2))/3
            iv = indf - (3*nv + (nstrains-3))
            lfound = .false.
            j = ncell
            do while (.not.lfound.and.j.le.n)
              j = j + 1
              if (indv.eq.iopt(j)) lfound = .true.
            enddo
            if (lfound) then
              if (iv.eq.1) then
                gc(j) = gc(j) + xdrv(nv)*conco(i)
              elseif (iv.eq.2) then
                gc(j) = gc(j) + ydrv(nv)*conco(i)
              else
                gc(j) = gc(j) + zdrv(nv)*conco(i)
              endif
            endif
          else
!
!  Strain derivatives
!
            strderv(indv) = strderv(indv) + strderv(indf)*conco(i)
          endif
        enddo
      endif
!
!  Collect cell derivatives
!
      if (ncell.gt.0) then
        do i = 1,ncell
          gc(i) = strderv(iopt(i))
        enddo
      endif
    endif
!***********************
!  Compute properties  *
!***********************
    if (lfcprop(nc)) then
      call property(.false.)
    endif
    if (lfcborn(nc)) then
      call borncharge(.false.)
    endif
    if (lfcphon(nc)) then
      if (ndimen(nc).gt.0) then
        call phonon(.false.,fc,nobsmodeptr0,nobsmodecfg(nc))
      else
        call deffreq(.false.,fc,2_i4,nobsmodeptr0,nobsmodecfg(nc))
      endif
    endif
    lsymderv2 = lsymd2save
    if (npotptcfg(nc).gt.0) call potential
    tim2 = cputime()
    deltim = deltim + tim2 - tim1
!**************************************
!  Place observables into fobs array  *
!**************************************
    do i = 1,nobs
      if (nobcfg(i).eq.nc.or.nobcfg(i).eq.0) then
        nobt = nobtyp(i)
!
!  Energy
!
        if (nobt.eq.1) then
          fcalc(i) = fc
!
!  Derivatives
!
        elseif (nobt.eq.2) then
          nptr = nobptr(i)
          gt = gc(nptr)
!
!  For relax mode - check that fractional coordinate is nearest
!  translationally equivalent position
!
!              if (lrelax.and.(nptr.gt.ncell)) then
!                gdif = fobs(i) - gt
!                if (abs(gdif).gt.0.5d0) then
!                  gt = gt + sign(1.0d0,gdif)
!                endif
!              endif
          fcalc(i) = fcalc(i) + gt
!
!  Elastic constants
!
        elseif (nobt.eq.3) then
          ni = nobptr(i)/7
          nj = nobptr(i) - 7*ni
          fcalc(i) = elcon(ni,nj)
!
!  High frequency dielectric constant
!
        elseif (nobt.eq.4) then
          ni = nobptr(i)/4
          nj = nobptr(i) - 4*ni
          fcalc(i) = diconh(ni,nj)
!
!  Static dielectric constant
!
        elseif (nobt.eq.5) then
          ni = nobptr(i)/4
          nj = nobptr(i) - 4*ni
          fcalc(i) = dicons(ni,nj)
!
!  Piezoelectric stress constant
!
        elseif (nobt.eq.7) then
          ni = nobptr(i)/7
          nj = nobptr(i) - 7*ni
          fcalc(i) = piezo(ni,nj)
!
!  Piezoelectric strain constant
!
        elseif (nobt.eq.8) then
          ni = nobptr(i)/7
          nj = nobptr(i) - 7*ni
          fcalc(i) = piezs(ni,nj)
!
!  Vibrational frequency
!
        elseif (nobt.eq.9) then
          ni = nobptr(i)
          fcalc(i) = freq(ni,nobptr2(i))
!
!  Electrostatic potential
!
        elseif (nobt.eq.10) then
          ni = nobptr(i)
          fcalc(i) = vpotpt(ni+npotpt0)
!
!  Bulk modulus
!
        elseif (nobt.eq.11) then
          fcalc(i) = bulkmod
!
!  Shear modulus
!
        elseif (nobt.eq.12) then
          fcalc(i) = shearmod
!
!  Heat capacity (Cv)
!
        elseif (nobt.eq.13) then
          fcalc(i) = cv
!
!  Entropy
!
        elseif (nobt.eq.14) then
          fcalc(i) = entropy
!
!  High frequency refractive index
!
        elseif (nobt.eq.15) then
          ni = nobptr(i)
          fcalc(i) = hfrefind(ni)
!
!  Static refractive index
!
        elseif (nobt.eq.16) then
          ni = nobptr(i)
          fcalc(i) = srefind(ni)
!
!  S(Q,omega)
!
        elseif (nobt.eq.17) then
          ni = nobptr(i)
          fcalc(i) = 0.0_dp
!
!  Born effective charges
!
        elseif (nobt.eq.18) then
          ni = (nobptr(i)-1)/9 + 1
          nj = nobptr(i) - 9*(ni-1)
          nk = (nj-1)/3 + 1
          nj = nj - 3*(nk-1)
          fcalc(i) = bornq(nj,nk,ni)
!
!  Monopole charges
!
        elseif (nobt.eq.19) then
          ni = nobptr(i)
          fcalc(i) = qf(ni)
!
!  ReaxFF charges
!
        elseif (nobt.eq.21) then
          ni = nobptr(i)
          fcalc(i) = qreaxFF(ni)
!
!  Bond length
!
        elseif (nobt.eq.22) then
          ni = nobptr(i)
          nj = nobptr2(i)
          xji = xclat(nj) - xclat(ni)
          yji = yclat(nj) - yclat(ni)
          zji = zclat(nj) - zclat(ni)
          call nearestr(ndimen(ncf),xji,yji,zji,rv(1,1),rji)
          fcalc(i) = rji
!
!  Bond angle
!
        elseif (nobt.eq.23) then
          ni = nobptr(i)
          nj = nobptr2(i)
          nk = nobptr3(i)
          xji = xclat(nj) - xclat(ni)
          yji = yclat(nj) - yclat(ni)
          zji = zclat(nj) - zclat(ni)
          call nearestr(ndimen(ncf),xji,yji,zji,rv(1,1),rji)
          xki = xclat(nk) - xclat(ni)
          yki = yclat(nk) - yclat(ni)
          zki = zclat(nk) - zclat(ni)
          call nearestr(ndimen(ncf),xki,yki,zki,rv(1,1),rki)
!
          costheta = (xji*xki + yji*yki + zji*zki)/(rji*rki + 1.0d-12)
          if (abs(costheta).gt.1.0_dp) costheta = sign(1.0_dp,costheta)
          theta = acos(costheta)*radtodeg
!
          fcalc(i) =  theta
!
!  Reaction energy
!
        elseif (nobt.eq.24) then
          fcalc(i) = fcalc(i) + freaction(ncf,i)*fc
!
!  Young's modulus
!
        elseif (nobt.eq.25) then
          ni = nobptr(i)
          fcalc(i) = ym(ni)
!
!  Poisson's ratio
!
        elseif (nobt.eq.26) then
          ni = nobptr(i)
          fcalc(i) = poissonratio(ni)
!
!  Coordination number
!
        elseif (nobt.eq.27) then
          ni = nobptr(i)
          call getcoordno(ni,fparameter(i),fcalc(i))
!
!  Vibrational mode
!
        elseif (nobt.eq.29) then
          ni = nobptr(i)
          fcalc(i) = fobsmodefreq(ni)
          finfo(i) = fobsmodeover(ni)
!
!  Volume
!
        elseif (nobt.eq.30) then
          fcalc(i) = volume(rv)
!
!  Structure
!
        elseif (nobt.eq.6) then
          nptr = nobptr(i)
          fcalc(i) = fcalc(i) + gc(nptr)
        endif
      endif
    enddo
!
    if (lrelax) then
!
!  Reset structure
!
      do i = 1,nasym
        xcfg(nsft+i) = xstore(i)
        ycfg(nsft+i) = ystore(i)
        zcfg(nsft+i) = zstore(i)
      enddo
      if (nbsm.gt.0) then
        do i = 1,nasym
          radcfg(nsft+i) = rstore(i)
        enddo
      endif
      if (ncell.gt.0) then
!
!  Reset cell
!
        do i = 1,3
          rv(1,i) = rvcfg(1,i,ncf)
          rv(2,i) = rvcfg(2,i,ncf)
          rv(3,i) = rvcfg(3,i,ncf)
        enddo
      endif
    endif
    nobsmodeptr0 = nobsmodeptr0 + nobsmodecfg(nc)
  enddo
!************************************
!  End of loop over configurations  *
!************************************
!
!  Evaluate sum of squares of differences
!
  fsumsq = 0.0_dp
  do i = 1,nobs
    fres(i) = weight(i)*(fcalc(i) - fobs(i))**2
  enddo
  do i = 1,nobs
    fsumsq = fsumsq + fres(i)
  enddo
!
!  Deallocate local memory
!
  deallocate(xcfull,stat=status)
  if (status/=0) call deallocate_error('fitfun','xcfull')
  deallocate(gc,stat=status)
  if (status/=0) call deallocate_error('fitfun','gc')
!
  time2 = cputime()
  tfitf = tfitf + time2 - time1 - deltim
!
  return
  end
