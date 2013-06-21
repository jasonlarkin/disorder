  subroutine mdprop(fc,nsteps,nmds,targettemperature,lprod)
!
!  Evaluate MD related properties during run.
!
!   2/97 Modifications from JRH added
!   3/97 NVT ensemble added
!   7/97 Constant pressure modifications added
!   8/99 Volume average added as output property
!  10/02 Constraint force average added
!   4/04 Summing of stresses added
!   7/05 lfirststep argument added to mdke call
!  10/06 Format changed for time
!   7/07 suminertia added
!  12/07 Temperature check added
!   3/08 Volume check added
!   3/08 Checks of T and P moved to earlier in the routine so that they
!        are performed every time. 
!   4/08 Modified for NPH ensemble
!   7/08 Cell averages corrected in NPH ensemble
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   2/09 Flag now passed in to indicate whether we are in the production phase or not
!   5/09 Printing of extra quantities added for stochastic baro/thermostat as debug option
!   6/09 Debugging output for siteenergy added
!   5/11 Pressure output moved outside condition on ensemble
!  10/11 Modified to prevent use of vol without being defined
!   5/12 Atomic stresses added
!   6/12 Target temperature output for case when temperature will change during run
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, June 2012
!
  use control
  use current
  use derivatives
  use energies,    only : siteenergy, epv
  use gulp_cml,    only : lcml
  use gulp_cml_md, only : gulp_cml_md_startstep, gulp_cml_md_endstep, &
                          gulp_cml_md_ensambledata, gulp_cml_md_celldata
  use iochannels
  use mdlogic,     only : ladiabatic
  use moldyn
  use m_pr,        only : pr_target_press, pr_cons, pr_ekin, pr_ekinbaro
  use parallel
  use shell
  use velocities
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: nmds
  integer(i4), intent(in) :: nsteps             ! number of steps so far
  logical,     intent(in) :: lprod              ! true for production, false for equil
  real(dp),    intent(in) :: fc                 ! energy at current step
  real(dp),    intent(in) :: targettemperature  ! target temperature
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: n
  logical                 :: lintsh
  real(dp)                :: aave
  real(dp)                :: alpave
  real(dp)                :: ara
  real(dp)                :: area
  real(dp)                :: aversfac
  real(dp)                :: bave
  real(dp)                :: betave
  real(dp)                :: cave
  real(dp),          save :: cfactor = 6.241460893d-3
  real(dp)                :: consave
  real(dp)                :: cstemp
  real(dp)                :: ctmp(6)
  real(dp)                :: econs
  real(dp)                :: econs1
  real(dp)                :: econs2
  real(dp)                :: econs3
  real(dp)                :: ekinave
  real(dp)                :: eneave
  real(dp)                :: etot
  real(dp)                :: etotave
  real(dp)                :: gamave
  real(dp)                :: lambdaRave
  real(dp)                :: lambdaVave
  real(dp)                :: pres
  real(dp)                :: presave
  real(dp)                :: rnsteps
  real(dp)                :: sum
  real(dp)                :: sumc3
  real(dp)                :: tempave
  real(dp)                :: tempratio
  real(dp)                :: trace
  real(dp)                :: velrsq
  real(dp)                :: virave
  real(dp)                :: vol
  real(dp)                :: volave
  real(dp)                :: volratio
  real(dp)                :: volume
  real(dp)                :: vsqave
  real(dp)                :: wold
!
  lintsh = (nshell.ne.0.and..not.ladiabatic)
!
!  Calculate Kinetic Energy and related factors
!
  call mdke(velrsq,.false.)
!
!  If NVT accumulate conserved quantity integral
!
  etot = ekin + fc
  if (nensemble(ncf).eq.2.or.nensemble(ncf).eq.3) then
    aversfac = 0.5_dp*(sfac + sfac0)
    sumsfac = sumsfac + aversfac
  endif
  temperature = velsq*rvfct
!
!  Check if temperature has exceed the maximum allowed
!  - add a small amount to target temperature to ensure there is no numerical issues
!
  tempratio = temperature/(targettemperature+1.0d-6)
  if (tempratio.gt.rmdmaxtemp) then
    call outerror('temperature has exceeded maximum allowed set by mdmaxtemp',0_i4)
    call gulpfinish
  endif
!
!  Check if volume has exceed the maximum allowed
!
  if (ndim.eq.3) then
    vol = volume(rv)
    volratio = vol/rmdvol0
    if (volratio.gt.rmdmaxvol) then
      call outerror('volume has exceeded maximum allowed set by mdmaxvolume',0_i4)
      call gulpfinish
    endif
  endif
!
!  If not sample call return
!
  if (mod(nsteps,nmds).ne.0) return
!
!  Accumulate running averages
!
  naverpt = naverpt + 1
  rnsteps = 1.0_dp/dble(naverpt)
!
  sumvsq = sumvsq + velsq
  sumener = sumener + fc
  sumvir = sumvir + virial
  if (ndim.eq.3) then
    ctmp(1:6) = - strderv(1:6)
    call mdkestrain(ctmp)
    do n = 1,nstrains
      sumstress(n) = sumstress(n) + ctmp(n)
    enddo
  endif
  if (lmdconstrain(ncf)) then
    sumlambdaR = sumlambdaR + lambdaR
    sumlambdaV = sumlambdaV + lambdaV
  endif
  if (latomicstress) then
    call mdkeatomicstress
    wold = dble(naverpt-1)*rnsteps
    do i = 1,numat
      do n = 1,nstrains
        sumatomicstress(n,i) = wold*sumatomicstress(n,i) + rnsteps*atomicstress(n,i)
      enddo
    enddo
  endif
!
!  Instantaneous properties
!
  sumtem = sumtem + temperature
  if (lintsh) then
    cstemp = velrsq*rvfct*dble(nmoving)/dble(nshell)
    sumcst = sumcst + cstemp
  endif
!
!  If NVT or NPT accumulate conserved quantity
!
  if ((nensemble(ncf).eq.2.or.nensemble(ncf).eq.3).and.nmdintegrator.ne.4) then
!
!  Thermostat terms
!
    econs1 = smdfctt*sfac*sfac/qtemp(ncf)
    econs2 = 2.0_dp*smdfctt*sumsfac
    econs = etot + econs1 + econs2
    sumcons = sumcons + econs
  endif
!
!  Averaged properties
!
  vsqave = sumvsq*rnsteps
  eneave = sumener*rnsteps
  virave = sumvir*rnsteps
  ekinave = 0.5_dp*vsqave*refct
  etotave = ekinave + eneave
  tempave = sumtem*rnsteps
  if (lmdconstrain(ncf)) then
    lambdaRave = sumlambdaR*rnsteps
    lambdaVave = sumlambdaV*rnsteps
  endif
!
  if (ndim.eq.3) then
    pres = pfct*(2.0_dp*ekin - virial)/vol
    presave = pfct*(2.0_dp*ekinave - virave)/vol
    if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
      if (nmdintegrator.ne.4) then
!
!  Barostat contribution to conserved quantity
!
        sumc3 = 2.0_dp*(c3(1)**2 + c3(2)**2 + c3(3)**2)
        sumc3 = sumc3 + 4.0_dp*(c3(4)**2 + c3(5)**2 + c3(6)**2)
        econs3 = press*vol*cfactor + sumc3/psfctt
        econs = econs + econs3
        sumcons = sumcons + econs3
      endif
!
!  Sample cell parameters
!
      call uncell3D(rv,a,b,c,alpha,beta,gamma)
      sumacell = sumacell + a
      sumbcell = sumbcell + b
      sumccell = sumccell + c
      sumalpcell = sumalpcell + alpha
      sumbetcell = sumbetcell + beta
      sumgamcell = sumgamcell + gamma
      sumvol = sumvol + vol
      aave = sumacell*rnsteps
      bave = sumbcell*rnsteps
      cave = sumccell*rnsteps
      alpave = sumalpcell*rnsteps
      betave = sumbetcell*rnsteps
      gamave = sumgamcell*rnsteps
      volave = sumvol*rnsteps
    endif
  elseif (ndim.eq.2) then
    ara = area(rv)
    if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
      if (nmdintegrator.ne.4) then
!
!  Barostat contribution to conserved quantity
!
        sumc3 = 2.0_dp*(c3(1)**2+c3(2)**2)
        sumc3 = sumc3 + 4.0_dp*(c3(3)**2)
        econs3 = press*ara*cfactor + sumc3/psfctt
        econs = econs + econs3
        sumcons = sumcons + econs3
      endif
!
!  Sample cell parameters
!
      call uncell2D(rv,a,b,alpha)
      sumacell = sumacell + a
      sumbcell = sumbcell + b
      sumalpcell = sumalpcell + alpha
      sumvol = sumvol + ara
      aave = sumacell*rnsteps
      bave = sumbcell*rnsteps
      alpave = sumalpcell*rnsteps
      volave = sumvol*rnsteps
    endif
  elseif (ndim.eq.1) then
    if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
      call uncell1D(rv,a)
      if (nmdintegrator.ne.4) then
!
!  Barostat contribution to conserved quantity
!
        sumc3 = 2.0_dp*(c3(1)**2)
        econs3 = press*a*cfactor + sumc3/psfctt
        econs = econs + econs3
        sumcons = sumcons + econs3
      endif
!
!  Sample cell parameters
!
      sumacell = sumacell + a
      sumvol = sumvol + a
      aave = sumacell*rnsteps
      volave = sumvol*rnsteps
    endif
  elseif (ndim.eq.0) then
    call gettraceinertia(trace)
    suminertia = suminertia + trace
  endif
  if (nmdintegrator.eq.4) then
!
!  Conserved quantity for new algorithm
!
    if (ndim.eq.3) then
      econs = fc + pr_ekin + pr_ekinbaro + pr_cons + pr_target_press*vol*cfactor
    else
      econs = fc + pr_ekin + pr_ekinbaro + pr_cons 
    endif
    sumcons = sumcons + econs
  endif
!*****************************
!  Write out current status  *
!*****************************
  if (ioproc) then
    write(ioout,'(''  ** Time : '',f18.5,'' ps :'')') nsteps*tstep(ncf)
    if (ntemperaturestep.gt.0) then
      write(ioout,'(''     Properties:                   Instantaneous       Averaged        Target'')')
    else
      write(ioout,'(''     Properties:                   Instantaneous       Averaged'')')
    endif
    write(ioout,'(7x,''Kinetic energy    (eV) = '',f14.6,4x,f14.6)') ekin,ekinave
    if (ldebug) then
      write(ioout,'(7x,''Kinetic energy PR (eV) = '',f14.6,4x,f14.6)') pr_ekin
      write(ioout,'(7x,''Kinetic barost PR (eV) = '',f14.6,4x,f14.6)') pr_ekinbaro
      write(ioout,'(7x,''Conserved corr PR (eV) = '',f14.6,4x,f14.6)') pr_cons
      if (ndim.eq.3) then
        write(ioout,'(7x,''PV correction  PR (eV) = '',f14.6,4x,f14.6)') pr_target_press*vol*cfactor
      endif
    endif
    write(ioout,'(7x,''Potential energy  (eV) = '',f14.6,4x,f14.6)') fc,eneave
    write(ioout,'(7x,''Total energy      (eV) = '',f14.6,4x,f14.6)') etot,etotave
    if ((lcml).and.(lintsh)) call gulp_cml_md_startstep(step=nsteps, &
                 cstemp=cstemp, acstemp=sumcst*rnsteps, time=nsteps*tstep(ncf), &
                 ke=ekin, pe=fc, te=etot, ake=ekinave, ape=eneave, ate=etotave, &
                 steptype=lprod, temp=temperature, atemp=tempave)
    if ((lcml).and..not.(lintsh)) call gulp_cml_md_startstep(step=nsteps, &
                 time=nsteps*tstep(ncf), &
                 ke=ekin, pe=fc, te=etot, ake=ekinave, ape=eneave, ate=etotave, &
                 steptype=lprod, temp=temperature, atemp=tempave)
    if (nensemble(ncf).ge.2.and.index(keyword,'cons').ne.0) then
      if (nmdintegrator.eq.4) then
        consave = sumcons*rnsteps
        write(ioout,'(7x,''Conserved quantity(eV) = '',f14.6,4x,f14.6)') econs,consave
      else
        consave = sumcons*rnsteps
        write(ioout,'(7x,''Conserved quantity(eV) = '',f14.6,4x,f14.6)') econs,consave
        write(ioout,'(7x,''Thermostat KE     (eV) = '',f14.6)') econs1
        write(ioout,'(7x,''Thermostat Integrl(eV) = '',f14.6)') econs2
        if (nensemble(ncf).eq.3) then
          write(ioout,'(7x,''Barostat term     (eV) = '',f14.6)') econs3
          if (lcml) call gulp_cml_md_ensambledata(cq=econs, themKE=econs1, &
                       thermInt=econs2, barostat=econs3, acq=consave)
        else
          if (lcml) call gulp_cml_md_ensambledata(cq=econs, themKE=econs1, &
                       thermInt=econs2, acq=consave)
        endif
      endif
    endif
    if (ntemperaturestep.gt.0) then
      write(ioout,'(7x,''Temperature       (K)  = '',f14.6,4x,f14.6,2x,f12.4)') temperature,tempave,targettemperature
    else
      write(ioout,'(7x,''Temperature       (K)  = '',f14.6,4x,f14.6)') temperature,tempave
    endif
    if (lintsh) then
      write(ioout,'(7x,''C/S temperature   (K)  = '',f14.6,4x,f14.6)') cstemp,sumcst*rnsteps
    endif
    if (ndim.eq.3) then
      write(ioout,'(7x,''Pressure         (GPa) = '',f14.6,4x,f14.6)') pres,presave
      if (nensemble(ncf).ge.3) then
        write(ioout,'(7x,''Cell parameter : a (A) = '',f14.6,4x,f14.6)') a,aave
        write(ioout,'(7x,''Cell parameter : b (A) = '',f14.6,4x,f14.6)') b,bave
        write(ioout,'(7x,''Cell parameter : c (A) = '',f14.6,4x,f14.6)') c,cave
        write(ioout,'(7x,''Cell angle : alpha (o) = '',f14.6,4x,f14.6)') alpha,alpave
        write(ioout,'(7x,''Cell angle : beta  (o) = '',f14.6,4x,f14.6)') beta,betave
        write(ioout,'(7x,''Cell angle : gamma (o) = '',f14.6,4x,f14.6)') gamma,gamave
        write(ioout,'(7x,''Cell volume :   (A**3) = '',f14.6,4x,f14.6)') vol,volave
        if (lcml) call gulp_cml_md_celldata(a=a, ava=aave, b=b, avb=bave, c=c, avc=cave, &
                          alpha=alpha, avalpha=alpave, beta=beta, avbeta=betave, &
                          gamma=gamma, avgamma=gamave, pressure=pres, avpressure=presave)
      endif
    elseif (ndim.eq.2) then
!      write(ioout,'(7x,''Pressure         (GPa) = '',f14.6,4x,f14.6)') pres,presave
      if (nensemble(ncf).ge.3) then
        write(ioout,'(7x,''Cell parameter : a (A) = '',f14.6,4x,f14.6)') a,aave
        write(ioout,'(7x,''Cell parameter : b (A) = '',f14.6,4x,f14.6)') b,bave
        write(ioout,'(7x,''Cell angle : alpha (o) = '',f14.6,4x,f14.6)') alpha,alpave
        write(ioout,'(7x,''Cell area   :   (A**2) = '',f14.6,4x,f14.6)') ara,volave
        if (lcml) call gulp_cml_md_celldata(a=a, ava=aave, b=b, avb=bave, c=c, avc=cave, &
                                            alpha=alpha, avalpha=alpave)
      endif
    elseif (ndim.eq.1) then
      if (nensemble(ncf).ge.3) then
!        write(ioout,'(7x,''Pressure         (GPa) = '',f14.6,4x,f14.6)') pres,presave
        write(ioout,'(7x,''Cell parameter : a (A) = '',f14.6,4x,f14.6)') a,aave
        if (lcml) call gulp_cml_md_celldata(a=a, ava=aave)
      endif
    endif
    if (lmdconstrain(ncf)) then
      write(ioout,'(7x,''Constraint R (eV/Angs) = '',f14.6,4x,f14.6)') 4.0_dp*lambdaR/stpsqh,4.0_dp*lambdaRave/stpsqh
      write(ioout,'(7x,''Constraint V (eV/Angs) = '',f14.6,4x,f14.6)') 4.0_dp*lambdaV/stpsqh,4.0_dp*lambdaVave/stpsqh
      if (lcml) call gulp_cml_md_endstep(constR=4.0_dp*lambdaR/stpsqh,&
                                         avconstR=4.0_dp*lambdaRave/stpsqh, &
                                         constV=4.0_dp*lambdaV/stpsqh, &
                                         avconstV=4.0_dp*lambdaVave/stpsqh)
    endif
    if (ldebug) then
      write(ioout,'(/,''  Site energies: '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom No.                Atom energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      sum = 0.0_dp
      do i = 1,numat
        write(ioout,'(i10,4x,f32.8)') i,siteenergy(i)
        sum = sum + siteenergy(i)
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Sum of site energies  = '',f20.8,'' eV'')') sum
      write(ioout,'(''  Total internal energy = '',f20.8,'' eV'')') fc-epv
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
    call gflush(ioout)
  endif
!
  return
  end
