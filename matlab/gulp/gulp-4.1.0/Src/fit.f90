  subroutine fit
!
!  Subroutine for fitting of interatomic potentials
!
!  nfit  = no. of variables specified for fitting in input
!  nfitt = no. of genuine variables after correction for constraints
!
!   6/95 Modified for additive constraints
!   5/98 Scale factors for certain variables reset after fit
!   7/98 Scale factors set to 1 after re-scaling loop otherwise
!        this causes an error for multiple references of the
!        same variable.
!  10/98 Codes for fitting variables simplified
!  10/02 Gnorm locally saved to avoid corruption from relax fitting opt
!  11/03 Bond order data added
!   4/05 Memory allocation cleaned up
!   6/05 Modifications for electronegativity fitting added
!   9/05 Powers added to fitting constraints
!   7/06 Sixbody potentials added
!   8/06 QEq radius added as fitted parameter
!   8/06 Bug in boundary potential correct fixed nt test set to 2
!        instead of 1
!  11/06 Call to ftow replaced with itow
!   2/07 EAM function parameters 4 & 5 added
!   3/07 Gotos replaced with f90 syntax
!   4/07 UFF parameters added
!   5/07 UFF torsion added
!   5/07 Bond charge increment added
!  12/07 Unused variables removed
!   3/08 SM electronegativity variables added as they were missing
!   3/08 ReaxFF parameters added
!   4/08 Handling of reaxFFval3 modified to allow for multiple angle pots
!   4/08 Initialisation of manybody parameters 6-9 was missing - now added
!   4/08 Post-fitting loop over observables modified to avoid problems
!        with configuration number = 0 for reaction energies
!   4/08 Array wvec separated from gradient array, gc so this can now be
!        passed to outfit for output.
!   4/08 Option for simplex fitting added
!   5/08 New UFF generators for OOP added
!   5/08 UFF chi added
!   5/08 Setting of default number of fitting cycles removed
!   5/08 Array to save original parameters added
!   6/08 ReaxFF Q shell structure arrays added
!   7/08 pval6 added to reaxFFval3 array
!   8/08 Format of final sum of squares extended
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 nfvar3 added for MEAM
!  12/08 Addressing of MEAM coefficient array modified to allow for change
!        from density to functional addressing
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 Use of nfvar for two-body potentials modified
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   4/09 Modified to allow for increased size of twopot
!   6/09 Module name changed from three to m_three
!  10/09 EVB parameters added
!   1/10 One-body potential added
!  10/10 EDIP potentials added
!  10/10 EDIP linear threebody modifications added
!   9/11 Error in referencing of threepoly fixed
!  11/12 Unused argument removed from call to putpar
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use bondcharge
  use bondorderdata
  use control
  use configurations
  use current
  use dump
  use eam
  use EDIPdata
  use element, only : maxele, chi, rmu, qeqchi, qeqmu, qeqrad, smmu, smchi, smzeta, smZnuc
  use element, only : reaxFFchi, reaxFFmu, reaxFFgamma, reaxFFshell, reaxFFqfix
  use fitting
  use four
  use general
  use iochannels
  use m_simplex
  use one
  use parallel
  use polarise
  use potchange
  use observables
  use optimisation
  use reaxFFdata
  use shifts
  use six
  use species
  use symmetry
  use m_three
  use two
  use uffdata
  implicit none
!
!  Local variables
!
  character(len=4)                             :: connum
  integer(i4)                                  :: i
  integer(i4)                                  :: idj
  integer(i4)                                  :: ifail
  integer(i4)                                  :: ind
  integer(i4)                                  :: io
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: maxfeval
  integer(i4)                                  :: mvar
  integer(i4)                                  :: nc
  integer(i4),                            save :: ncfold = 0
  integer(i4)                                  :: nio
  integer(i4)                                  :: nio2
  integer(i4)                                  :: nj
  integer(i4)                                  :: nf2
  integer(i4)                                  :: nf3
  integer(i4)                                  :: nfcal
  integer(i4)                                  :: np
  integer(i4)                                  :: nptr
  integer(i4)                                  :: nsht
  integer(i4)                                  :: nt
  integer(i4)                                  :: ntp
  integer(i4)                                  :: ntyps
  integer(i4)                                  :: nv
  integer(i4)                                  :: nvs
  integer(i4)                                  :: nv1
  integer(i4)                                  :: nv2
  integer(i4)                                  :: nv3
  integer(i4)                                  :: status
  logical                                      :: lfound
  logical                                      :: lfreqoutsave
  logical                                      :: lsimplex
  real(dp),    dimension(:), allocatable       :: fhess
  real(dp),    dimension(:), allocatable       :: wvec
  real(dp),    dimension(:), allocatable       :: gc
  real(dp),    dimension(:), allocatable       :: xc
  real(dp),    dimension(:), allocatable       :: xcoriginal
  real(dp),    dimension(:), allocatable       :: xctmp
  real(dp)                                     :: cputime
  real(dp)                                     :: dum
  real(dp)                                     :: fsumsq
  real(dp)                                     :: gnormfit
  real(dp)                                     :: rtmp
  real(dp)                                     :: time1
  real(dp)                                     :: tmp
!
  lfreeze = .false.
!
!  Set algorithm flag
!
  lsimplex = (index(keyword,'simp').eq.1.or.index(keyword,' simp').ne.0)
!
!  Save lfreqout and set to false
!
  lfreqoutsave = lfreqout
  lfreqout = .false.
!
!  If simultaneous optimisation of shells and fitting add variables
!  to fitting parameters
!
  if (index(keyword,'simu').gt.0.and..not.lrelax) then
    do nc = 1,ncfg
      ncf = nc
      if (ncf.ne.ncfold) then
        call setup(.true.)
        ncfold = ncf
      endif
      lfcborn(nc) = .false.
      lfcprop(nc) = .false.
      lfcphon(nc) = .false.
      mvar = 3*nasym + nstrains
      do i = ncell+1,nvar
        io = iopt(i)
        if (io.le.mvar) then
!
!  Shell coordinates
!
          nio = (io-(nstrains-2))/3
          if (lsymopt) then
            if (iatn(nio).gt.maxele) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit) = 1
              nfpot(nfit) = 1
              nfvar(nfit) = io
              nfcfg(nfit) = nc
            endif
          else
            if (nat(nio).gt.maxele) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit) = 1
              nfpot(nfit) = 1
              nfvar(nfit) = io
              nfcfg(nfit) = nc
            endif
          endif
        else
!
!  Breathing species radius
!
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 1
          nfpot(nfit) = 2
          nfvar(nfit) = io - mvar
          nfcfg(nfit) = nc
        endif
      enddo
    enddo
  endif
!
!  Trap error in number of variables
!
  if (nfit.eq.0) then
    call outerror('No parameters have been specified for fitting',0_i4)
    call stopnow('fit')
  endif
!
!  Check number of observables against the number of variables
!
  if (nfit-nfcon.gt.nobs) then
    if (ioproc) then
      write(ioout,'(/)')
      write(ioout,'(''  **** No. of variables exceeds no. of observables ****'')')
      write(ioout,'(''  **** Number of variables   = '',i4,16x,''****'')') nfit-nfcon
      write(ioout,'(''  **** Number of observables = '',i4,16x,''****'')') nobs
      write(ioout,'(/)')
    endif
    call stopnow('fit')
  endif
!*****************************
!  Allocate fitting hessian  *
!*****************************
  allocate(gc(nfit),stat=status)
  if (status/=0) call outofmemory('fit','gc')
  allocate(xc(nfit),stat=status)
  if (status/=0) call outofmemory('fit','xc')
  allocate(xcoriginal(nfit),stat=status)
  if (status/=0) call outofmemory('fit','xcoriginal')
  allocate(xctmp(nfit),stat=status)
  if (status/=0) call outofmemory('fit','xctmp')
  allocate(wvec(nfit),stat=status)
  if (status/=0) call outofmemory('fit','wvec')
  allocate(fres(nobs),stat=status)
  if (status/=0) call outofmemory('fit','fres')
  allocate(fhess(nfit*(nfit+1)/2),stat=status)
  if (status/=0) call outofmemory('fit','fhess')
!******************
!  Initialise xc  *
!******************
  nsht = 0
  do i = 1,nfit
    nt  = nftyp(i)
    np  = nfpot(i)
    nf2 = nfpot2(i)
    nf3 = nfpot3(i)
    nv  = nfvar(i)
    nvs = nfvar2(i)
    nv3 = nfvar3(i)
    ntp = nfatyp(i)
    if (nt.eq.1) then
!----------------------
!  General variables  -
!----------------------
      if (np.eq.1) then
!
!  Shell coordinate variables
!
        ncf = nfcfg(i)
        if (ncf.ne.ncfold) then
          call setup(.true.)
          ncfold = ncf
        endif
        nio = nv
        nio2 = (nio - (nstrains-2))/3
        nio = nio - nio2*3 - (nstrains-3)
        if (nio.eq.1) then
          scale(i) = xcfg(nsft+nio2)
        elseif (nio.eq.2) then
          scale(i) = ycfg(nsft+nio2)
        else
          scale(i) = zcfg(nsft+nio2)
        endif
        if (scale(i).eq.0.0_dp) scale(i) = 0.01_dp
      elseif (np.eq.2) then
!
!  Breathing species radius
!
        ncf = nfcfg(i)
        if (ncf.ne.ncfold) then
          call setup(.true.)
          ncfold = ncf
        endif
        scale(i) = radcfg(nsft+nv)
      elseif (np.eq.3) then
!
!  Shift
!
        nsht = nsht + 1
        scale(i) = shift(nsht)
      elseif (np.eq.4) then
!
!  Charges
!
        do k = 1,nspec
          ntyps = ntypspec(k)
          if (nv.eq.natspec(k).and.(ntp.eq.ntyps.or.ntp.eq.0)) then
            scale(i) = qlspec(k)
            exit
          endif
        enddo
      elseif (np.eq.5) then
!
!  Polarisability 
!
        do k = 1,nasym
          if (nv.eq.iatn(k)) then
            scale(i) = dpolar(k)
            exit
          endif
        enddo
      elseif (np.eq.6) then
!
!  Core - shell split
!
        do k = 1,nspec
          ntyps = ntypspec(k)
          if (nv.eq.natspec(k).and.(ntp.eq.ntyps.or.ntp.eq.0)) then
            scale(i) = qlspec(k)
            exit
          endif
        enddo
      elseif (np.eq.7) then
!
!  Epsilon value
!
        scale(i) = epsilon(nv)
      elseif (np.eq.8) then
!
!  Sigma value
!
        scale(i) = sigma(nv)
      elseif (np.eq.9) then
!
!  Atom A
!
        scale(i) = atoma(nv)
      elseif (np.eq.10) then
!
!  Atom B 
!
        scale(i) = atomb(nv)
      elseif (np.eq.11) then
!
!  EEM chi
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = chi(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.12) then
!
!  EEM mu
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = rmu(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.13) then
!
!  QEq chi
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = qeqchi(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.14) then
!
!  QEq mu
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = qeqmu(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.15) then
!
!  SM chi
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = smchi(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.16) then
!
!  SM mu
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = smmu(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.17) then
!
!  SM zeta
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = smzeta(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.18) then
!
!  SM Znuc
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = smZnuc(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.19) then
!
!  QEq radius
!
        if (nv.ge.1.and.nv.le.maxele) then
          scale(i) = qeqrad(nv)
        else
          scale(i) = 1.0_dp
        endif
      elseif (np.eq.20) then
!
!  UFF radius
!
        scale(i) = UFFr(nv)
      elseif (np.eq.21) then
!
!  UFF theta
!
        scale(i) = UFFtheta(nv)
      elseif (np.eq.22) then
!
!  UFF distance
!
        scale(i) = UFFx(nv)
      elseif (np.eq.23) then
!
!  UFF energy
!
        scale(i) = UFFd(nv)
      elseif (np.eq.24) then
!
!  UFF zeta
!
        scale(i) = UFFzeta(nv)
      elseif (np.eq.25) then
!
!  UFF effective charge
!
        scale(i) = UFFZeff(nv)
      elseif (np.eq.26) then
!
!  UFF torsion
!
        scale(i) = UFFtor(nv)
      elseif (np.eq.27) then
!
!  Bond charge increment
!
        scale(i) = bondQincrement(nv)
      elseif (np.eq.30) then
!
!  UFF Koop
!
        scale(i) = UFFKoop(nv)
      elseif (np.eq.31) then
!
!  UFF theta oop
!
        scale(i) = UFFthetaoop(nv)
      elseif (np.eq.32) then
!
!  UFF chi
!
        scale(i) = UFFchi(nv)
      elseif (np.eq.38) then
!
!  One-body energy
!
        scale(i) = onepot(nv)
      endif
    elseif (nt.eq.2) then
!-----------------------
!  Two-body variables  -
!-----------------------
      if (nv.eq.1) then
        scale(i) = twopot(1,np)
      elseif (nv.eq.2) then
        scale(i) = twopot(2,np)
      elseif (nv.eq.3) then
        scale(i) = twopot(3,np)
      elseif (nv.eq.4) then
        scale(i) = twopot(4,np)
      elseif (nv.eq.5) then
        scale(i) = twopot(5,np)
      elseif (nv.eq.6) then
        scale(i) = twopot(6,np)
      elseif (nv.eq.7) then
        scale(i) = twopot(7,np)
      elseif (nv.ge.10) then
        scale(i) = tpot(nv-9,np)
      endif
    elseif (nt.eq.3) then
!-------------------------
!  Three-body variables  -
!-------------------------
      if (nv.eq.1) then
        scale(i) = thbk(np)
      elseif (nv.eq.2) then
        scale(i) = theta(np)
      elseif (nv.eq.3) then
        scale(i) = thrho1(np)
      elseif (nv.eq.4) then
        scale(i) = thrho2(np)
      elseif (nv.eq.5) then
        scale(i) = thrho3(np)
      else
        scale(i) = threepoly(nv-5,np)
      endif
    elseif (nt.eq.4) then
!------------------------
!  Four-body variables  -
!------------------------
      if (nv.eq.1) then
        scale(i) = fork(np)
      else
        scale(i) = forpoly(nv-1,np)
      endif
    elseif (nt.eq.5) then
!------------------------
!  Many-body variables  -
!------------------------
      if (nv.eq.1) then
!
!  EAM density parameter 1
!
        scale(i) = denpar(1,nv3,nvs,np)
      elseif (nv.eq.2) then
!
!  EAM density parameter 2
!
        scale(i) = denpar(2,nv3,nvs,np)
      elseif (nv.eq.3) then
!
!  EAM density parameter 3
!
        scale(i) = denpar(3,nv3,nvs,np)
      elseif (nv.eq.4) then
!
!  EAM functional parameter 1
!
        scale(i) = eamfnpar(1,np)
      elseif (nv.eq.5) then
!
!  EAM functional parameter 2
!
        scale(i) = eamfnpar(2,np)
      elseif (nv.eq.6) then
!
!  EAM functional parameter 3
!
        scale(i) = eamfnpar(3,np)
      elseif (nv.eq.7) then
!
!  EAM alloy scale parameter 
!
        scale(i) = eamalloy(1,np)
      elseif (nv.eq.8) then
!
!  EAM alloy additive parameter 
!
        scale(i) = eamalloy(2,np)
      elseif (nv.eq.9) then
!
!  EAM functional parameter 4
!
        scale(i) = eamfnpar(4,np)
      elseif (nv.eq.10) then
!
!  EAM functional parameter 5
!
        scale(i) = eamfnpar(5,np)
      elseif (nv.eq.11) then
!
!  EAM functional parameter 6
!
        scale(i) = eamfnpar(6,np)
      elseif (nv.eq.12) then
!
!  EAM functional parameter 7
!
        scale(i) = eamfnpar(7,np)
      elseif (nv.eq.13) then
!
!  EAM functional parameter 8
!
        scale(i) = eamfnpar(8,np)
      elseif (nv.eq.14) then
!
!  EAM functional parameter 9
!
        scale(i) = eamfnpar(9,np)
      elseif (nv.eq.15) then
!
!  EAM density parameter 4
!
        scale(i) = denpar(4,nv3,nvs,np)
      elseif (nv.eq.16) then
!
!  EAM density parameter 5
!
        scale(i) = denpar(5,nv3,nvs,np)
      elseif (nv.eq.17) then
!
!  EAM density parameter 6
!
        scale(i) = denpar(6,nv3,nvs,np)
      elseif (nv.eq.18) then
!
!  EAM density parameter 7
!
        scale(i) = denpar(7,nv3,nvs,np)
      elseif (nv.eq.19) then
!
!  MEAM coefficient
!
        scale(i) = eamfnmeamcoeff(nvs,np)
      endif
    elseif (nt.eq.6) then
!-------------------------
!  Bond-order variables  -
!-------------------------
      if (nv.eq.1) then
!
!  Twobody A
!
        scale(i) = BOacoeff(np)
      elseif (nv.eq.2) then
!
!  Twobody B
!
        scale(i) = BObcoeff(np)
      elseif (nv.eq.3) then
!
!  Twobody zeta A
!
        scale(i) = BOzacoeff(np)
      elseif (nv.eq.4) then
!
!  Twobody zeta B
!
        scale(i) = BOzbcoeff(np)
      elseif (nv.eq.5) then
!
!  Repulsive BO alpha
!
        scale(i) = BOecoeffR(np)
      elseif (nv.eq.6) then
!
!  Repulsive BO n
!
        scale(i) = BOncoeffR(np)
      elseif (nv.eq.7) then
!
!  Repulsive BO lambda
!
        scale(i) = BOlcoeffR(np)
      elseif (nv.eq.8) then
!
!  Repulsive BO theta c
!
        scale(i) = BOccoeffR(np)
      elseif (nv.eq.9) then
!
!  Repulsive BO theta d
!
        scale(i) = BOdcoeffR(np)
      elseif (nv.eq.10) then
!
!  Repulsive BO theta h
!
        scale(i) = BOhcoeffR(np)
      elseif (nv.eq.11) then
!
!  Attractive BO alpha
!
        scale(i) = BOecoeffA(np)
      elseif (nv.eq.12) then
!
!  Attractive BO n
!
        scale(i) = BOncoeffA(np)
      elseif (nv.eq.13) then
!
!  Attractive BO lambda
!
        scale(i) = BOlcoeffA(np)
      elseif (nv.eq.14) then
!
!  Attractive BO theta c
!
        scale(i) = BOccoeffA(np)
      elseif (nv.eq.15) then
!
!  Attractive BO theta d
!
        scale(i) = BOdcoeffA(np)
      elseif (nv.eq.16) then
!
!  Attractive BO theta h
!
        scale(i) = BOhcoeffA(np)
      elseif (nv.eq.17) then
!
!  Repulsive BO chi
!
        scale(i) = BOchiR(np)
      elseif (nv.eq.18) then
!
!  Attractive BO chi
!
        scale(i) = BOchiA(np)
      endif
    elseif (nt.eq.7) then
!-----------------------
!  Six-body variables  -
!-----------------------
      if (nv.eq.1) then
        scale(i) = sixk(np)
      endif
    elseif (nt.eq.8) then
!---------------------
!  ReaxFF variables  -
!---------------------
      if (nv.eq.1) then
!
!  ReaxFF1 radii - sigma, pi & pi-pi
!
        if (nvs.eq.1) then
          scale(i) = reaxFFr(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFr(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFr(3,np)
        endif
      elseif (nv.eq.2) then
!    
!  ReaxFF1 valence - normal, boc, lp & angle
!
        if (nvs.eq.1) then
          scale(i) = reaxFFval(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFval(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFval(3,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFval(4,np)
        endif
      elseif (nv.eq.3) then
!    
!  ReaxFF1 over - p_boc3, p_boc4, p_boc5, p_ovun2
!
        if (nvs.eq.1) then
          scale(i) = reaxFFpboc(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFpboc(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFpboc(3,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFoc1(np)
        endif
      elseif (nv.eq.4) then
!    
!  ReaxFF1 under - p_ovun5
!
        if (nvs.eq.1) then
          scale(i) = reaxFFuc1(np)
        endif
      elseif (nv.eq.5) then
!    
!  ReaxFF1 lone pair - n_lp_opt & p_lp2
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlp(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFlp(2,np)
        endif
      elseif (nv.eq.6) then
!    
!  ReaxFF1 angle - p_val3 & p_val5
!
        if (nvs.eq.1) then
          scale(i) = reaxFFval1(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFval1(2,np)
        endif
      elseif (nv.eq.7) then
!    
!  ReaxFF1 morse - alpha, Dij, rvdw and gamma_w
!
        if (nvs.eq.1) then
          scale(i) = reaxFFalpha(np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFeps(np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFrvdw(np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFgammaw(np)
        endif
      elseif (nv.eq.8) then
!    
!  ReaxFF1 charge parameters - chi, mu, gamma, qshell
!
        if (nvs.eq.1) then
          scale(i) = reaxFFchi(np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFmu(np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFgamma(np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFshell(1,np)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFshell(2,np)
        elseif (nvs.eq.6) then
          scale(i) = reaxFFshell(3,np)
        elseif (nvs.eq.7) then
          scale(i) = reaxFFqfix(np)
        endif
      elseif (nv.eq.9) then
!     
!  ReaxFF0 bond parameters - p_boc1 p_boc2
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(1)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFlam(2)
        endif
      elseif (nv.eq.10) then
!
!  ReaxFF0 over parameters - p_ovun3 p_ovun4 p_ovun6 p_ovun7 p_ovun8
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(6)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFlam(31)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFlam(7)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFlam(8)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFlam(9)
        endif
      elseif (nv.eq.11) then
!
!  ReaxFF0 valence parameters - p_val6 p_val8 p_val9 p_val10
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(15)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFlam(16)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFlam(17)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFlam(18)
        endif
      elseif (nv.eq.12) then
!
!  ReaxFF0 penalty parameters - p_pen2 p_pen3 p_pen4
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(20)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFlam(21)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFlam(22)
        endif
      elseif (nv.eq.13) then
!         
!  ReaxFF0 torsion parameters - p_tor2 p_tor3 p_tor4 p_cot2
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(24)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFlam(25)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFlam(26)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFlam(27)
        endif
      elseif (nv.eq.14) then
!         
!  ReaxFF0 VDW parameter - p_vdw1
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(28)
        endif
      elseif (nv.eq.15) then
!         
!  ReaxFF0 lone pair parameter - p_lp1
!
        if (nvs.eq.1) then
          scale(i) = reaxFFlam(29)
        endif
      elseif (nv.eq.16) then
!
!  ReaxFF2 bond parameters - De_sigma, De_pi, De_pipi, p_be1 & p_be2
!
        if (nvs.eq.1) then
          scale(i) = reaxFFDe(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFDe(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFDe(3,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFpbe(1,np)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFpbe(2,np)
        endif
      elseif (nv.eq.17) then
!
!  ReaxFF2 over parameters - p_ovun1
!
        if (nvs.eq.1) then
          scale(i) = reaxFFoc2(np)
        endif
      elseif (nv.eq.18) then
!
!  ReaxFF2 bo parameters - p_bo1, p_bo2, p_bo3, p_bo4, p_bo5 & p_bo6
!
        if (nvs.eq.1) then
          scale(i) = reaxFFpbo(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFpbo(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFpbo(3,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFpbo(4,np)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFpbo(5,np)
        elseif (nvs.eq.6) then
          scale(i) = reaxFFpbo(6,np)
        endif
      elseif (nv.eq.19) then
!     
!  ReaxFF2 morse parameters - De, alpha, r0, r_sigma, r_pi & r_pipi
!
        if (nvs.eq.1) then
          scale(i) = reaxFFmorse(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFmorse(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFmorse(3,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFmorse(4,np)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFmorse(5,np)
        elseif (nvs.eq.6) then
          scale(i) = reaxFFmorse(6,np)
        endif
      elseif (nv.eq.20) then
!           
!  ReaxFF2 pen parameters - p_pen1, p_pen2 & p_pen3
!
        if (nvs.eq.1) then
          scale(i) = reaxFFpen2(1,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFpen2(2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFpen2(3,np)
        endif
      elseif (nv.eq.21) then
!         
!  ReaxFF3 ang parameters - theta_00, p_val1, p_val2, p_val4, p_val7 & p_val6
!
        if (nvs.eq.1) then
          scale(i) = reaxFFval3(1,nf3,nf2,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFval3(2,nf3,nf2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFval3(3,nf3,nf2,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFval3(4,nf3,nf2,np)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFval3(5,nf3,nf2,np)
        elseif (nvs.eq.6) then
          scale(i) = reaxFFval3(6,nf3,nf2,np)
        endif
      elseif (nv.eq.22) then
!
!  ReaxFF3 pen parameter - p_pen1
!
        if (nvs.eq.1) then
          scale(i) = reaxFFpen3(nf2,np)
        endif
      elseif (nv.eq.23) then
!
!  ReaxFF3 conj parameters - p_coa1, p_coa2, p_coa3 & p_coa4
!
        if (nvs.eq.1) then
          scale(i) = reaxFFconj3(1,nf2,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFconj3(2,nf2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFconj3(3,nf2,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFconj3(4,nf2,np)
        endif
      elseif (nv.eq.24) then
!
!  ReaxFF3 hbond parameters - r0_hb, p_hb1, p_hb2 & p_hb3
!
        if (nvs.eq.1) then
          scale(i) = reaxFFhb3(1,nf3,nf2,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFhb3(2,nf3,nf2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFhb3(3,nf3,nf2,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFhb3(4,nf3,nf2,np)
        endif
      elseif (nv.eq.25) then
!
!  ReaxFF4 torsion parameters - V1, V2, V3, p_tor1 & p_cot1
!
        if (nvs.eq.1) then
          scale(i) = reaxFFtor4(1,nf2,np)
        elseif (nvs.eq.2) then
          scale(i) = reaxFFtor4(2,nf2,np)
        elseif (nvs.eq.3) then
          scale(i) = reaxFFtor4(3,nf2,np)
        elseif (nvs.eq.4) then
          scale(i) = reaxFFtor4(4,nf2,np)
        elseif (nvs.eq.5) then
          scale(i) = reaxFFtor4(5,nf2,np)
        endif
      endif
    elseif (nt.eq.9) then
!-------------------
!  EDIP variables  -
!-------------------
      if (nv.eq.1) then
!
!  EDIP coordination number
!
        if (nvs.eq.1) then
          scale(i) = EDIPalpha(np)
        elseif (nvs.eq.2) then
          scale(i) = EDIPZdih(np)
        elseif (nvs.eq.3) then
          scale(i) = EDIPZrep(np)
        elseif (nvs.eq.4) then
          scale(i) = EDIPc0(np)
        endif
      elseif (nv.eq.2) then
!   
!  EDIP twobody energy
!
        if (nvs.eq.1) then
          scale(i) = EDIP2epsilon(np)
        elseif (nvs.eq.2) then
          scale(i) = EDIP2B(np)
        elseif (nvs.eq.3) then
          scale(i) = EDIP2beta(np)
        elseif (nvs.eq.4) then
          scale(i) = EDIP2sigma(np)
        elseif (nvs.eq.5) then
          scale(i) = EDIP2a(np)
        elseif (nvs.eq.6) then
          scale(i) = EDIP2aprime(np)
        endif
      elseif (nv.eq.3) then
!     
!  EDIP threebody energy
! 
        if (nvs.eq.1) then
          scale(i) = EDIP3lambda0(nf2,np)
        elseif (nvs.eq.2) then
          scale(i) = EDIP3lambdap(nf2,np)
        elseif (nvs.eq.3) then
          scale(i) = EDIP3Z0(nf2,np)
        elseif (nvs.eq.4) then
          scale(i) = EDIP3gamma0(nf2,np)
        elseif (nvs.eq.5) then
          scale(i) = EDIP3gammap(nf2,np)
        elseif (nvs.eq.6) then
          scale(i) = EDIP3q(nf2,np)
        elseif (nvs.eq.7) then
          scale(i) = EDIP3kq2(nf2,np)
        endif
      endif
    endif
  enddo
  do i = 1,nfit
    xc(i) = 1.0_dp
  enddo
!
!  Check that none of the scaling parameters have been set to zero
!  before fitting - if they have give warning and increment value.
!
  do i = 1,nfit
    nt = nftyp(i)
    np = nfpot(i)
    if (scale(i).eq.0.and.(nt.ne.1.or.np.gt.2)) then
      nwarn = nwarn + 1
      call outwarning('Fitted parameter initially set to zero',0_i4)
      scale(i) = 0.1_dp
    endif
  enddo
  if (nfcon.gt.0) then
!
!  Check that constraint pointers are all less than nfit
!
    do i = 1,nfcon
      if (nfcfix(i).gt.nfit.or.nfcvar(i).gt.nfit) then
        call itow(connum,i,4)
        call outerror('Constraint number '//connum//' involves a non-existent variable',0_i4)
        call stopnow('fit')
      endif
    enddo
!
!  Scale constraint additive parameters
!
    do i = 1,nfcon
      if (nfcotyp(i).eq.1) then
        fconadd(i) = fconadd(i)/scale(nfcfix(i))
        fconco(i) = fconco(i)*((scale(nfcvar(i)))**fconpower(i))/scale(nfcfix(i))
      else
        nv1 = nfcvar(i)
        nv2 = nint(fconadd(i))
        if (nv1.eq.nv2) then
          tmp = scale(nv1)
        else
          tmp = scale(nv1)*scale(nv2)
        endif
        fconco(i) = fconco(i)*sqrt(abs(tmp))/scale(nfcfix(i))
      endif
    enddo
!
!  Fitted parameter constraints
!
    call fitcon(xc)
  endif
!
!  Set pointers to potentials that have boundary corrections
!
  nchng = 0
  do i = 1,nfit
    nt = nftyp(i)
    np = nfpot(i)
    nv = nfvar(i)
    if (nt.eq.2) then
      if (nv.lt.10.and.leshift(np).and.nptype(np).ne.10) then
        lfound = .false.
        j = 0
        do while (j.le.nchng.and..not.lfound)
          j = j + 1
          if (nv.eq.npchng(j)) lfound = .true.
        enddo
        if (.not.lfound) then
          nchng = nchng + 1
          npchng(nchng) = nv
        endif
      endif
    endif
  enddo
!*********************************************
!  Contract fitting variables to unique set  *
!*********************************************
  if (nfcon.ne.0) then
    nfitt = 0
    do i = 1,nfit
      lfound = .false.
      do j = 1,nfcon
        if (i.eq.nfcfix(j)) lfound = .true.
      enddo
      if (.not.lfound) then
        nfitt = nfitt + 1
        nfitptr(nfitt) = i
        xctmp(nfitt) = xc(i)
      endif
    enddo
  else
    nfitt = nfit
    do i = 1,nfit
      nfitptr(i) = i
      xctmp(i) = xc(i)
    enddo
  endif
!
!  Save original values of parameters
!
  do i = 1,nfit
    xcoriginal(i) = xc(i)*scale(i)
  enddo
!
!  Print out observables and variables
!
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''  Number of variables   = '',i4)') nfit
    write(ioout,'(''  Number of observables = '',i4,/)') nobs
  endif
!***********************************
!  Output observables and weights  *
!***********************************
  call outobs(ncfold)
  if (ioproc) then
!*********************
!  Output variables  *
!*********************
    call outvar(xc,xcoriginal,1_i4)
    if (nfcon.gt.0) call outfitcon
!
    if (lga) write(ioout,'(''  Genetic Algorithm Fitting:'',/)')
    if (lsymopt) then
      write(ioout,'(''  Symmetry constraints used for fitting'')')
    elseif (lsym) then
      write(ioout,'(''  Symmetry not used for fitting'')')
    endif
  endif
  if (lga) then
    call gafit(xctmp,fsumsq,ifail)
  else
    if (ioproc) then
      if (index(keyword,'simu').ne.0) then
        write(ioout,'(''  Simultaneous optimisation will be performed during fitting'')')
      endif
      if (index(keyword,'unit').ne.0) then
        write(ioout,'(''  Unit matrix used as initial Hessian'')')
      endif
      if (lsimplex) then
        write(ioout,'(''  Simplex algorithm to be used in fitting'',/)')
      else
        write(ioout,'(''  First derivatives of residuals to be used in fitting'',/)')
      endif
      write(ioout,'(''  Maximum no. of cycles   = '',i10)')maxfcal
      write(ioout,'(''  Maximum step size       = '',f10.4)')fstepmx
      write(ioout,'(''  Tolerance on parameters = '',f10.7)')fxtol
      write(ioout,'(''  Tolerance on function   = '',f10.7)')fftol
      if (.not.lsimplex) then
        write(ioout,'(''  Tolerance on gradient   = '',f10.7)')fgtol
      endif
      write(ioout,'(''  Differencing interval   = '',f10.7,/)')delta
      if (ncycd.ne.1000) then
        if (ncycd.eq.1) then
          write(ioout,'(''  Dumpfile to be written after every cycle'',/)')
        else
          write(ioout,'(''  Dumpfile to be written after every '',i4,'' cycles'',/)')ncycd
        endif
      endif
      write(ioout,'(''  Start of fitting :'',/)')
      call gflush(ioout)
    endif
    if (lsimplex) then
!
!  Simplex
!
!  NB : maxfeval is a dummy variable which is the maximum number of function evaluations
!       as opposed to maxfcal which is the number of cycles. Furthermore, the size of fhess(1)
!       is the initial step size for simplex. For relax fitting this should not be too large.
!
      fhess(1) = -0.001_dp
      maxfeval = 1000*maxfcal
      call subplx(nfitt,fxtol,maxfeval,maxfcal,ncycd,idump,0_i4,fhess,xctmp,fsumsq,nfcal,ifail)
!
!  Translate simplex error codes to match BFGS ones
!
      if (ifail.eq.-2) then
        ifail = 1
      elseif (ifail.eq.-1) then
        ifail = 2
      elseif (ifail.eq.1.or.ifail.eq.2) then
        ifail = 3
      endif
    else
!
!  BFGS algorithm
!
      ifail = - 2
      call fitbfgs(xctmp,fsumsq,gc,fxtol,fftol,fgtol,maxfcal,fhess,ifail)
    endif
  endif
  if (ioproc) then
!
!  Has the calculation been successful?
!
    write(ioout,'(/)')
    if (ifail.eq.1) then
      write(ioout,'(''  **** Parameter outside specified range ****'')')
    elseif (ifail.eq.2) then
      write(ioout,'(''  **** Maximum number of function calls has been reached ****'')')
      write(ioout,'(''  **** or maximum time limit has been reached            ****'',/)')
    elseif (ifail.eq.0) then
      write(ioout,'(''  **** Fit completed successfully ****'',/)')
    elseif (ifail.eq.3) then
      write(ioout,'(''  **** No lower sum of squares could be found ****'',/)')
    elseif (ifail.eq.4) then
      write(ioout,'(''  **** Singularity or Jacobean failed to converge ****'')')
    elseif (ifail.eq.-1) then
      write(ioout,'(''  **** Time limit is about to be reached - stop fitting ****'')')
    else
      write(ioout,'(''  **** Unexpected termination of fitting ****'',/)')
    endif
  endif
!
!  Free hessian memory
!
  deallocate(fhess,stat=status)
  if (status/=0) call deallocate_error('calls','fhess')
!
!  Evaluate gnorm before gradient vector is overwritten
!
  gnormfit = 0.0_dp
  if (.not.lsimplex) then
    do i = 1,nfitt
      gnormfit = gnormfit + gc(i)*gc(i)
    enddo
    gnormfit = sqrt(gnormfit)
  endif
!****************************
!  Return fitted variables  *
!****************************
  ncfold = 0
  do i = 1,nfitt
    xc(nfitptr(i)) = xctmp(i)
  enddo
  call fitcon(xc)
  call putpar(xc,ncfold,wvec,.true.)
  if (lastq.gt.0) then
    ncf = 1
    call setup(.true.)
  endif
!*******************
!  Output results  *
!*******************
  nfcf = 0
  call fitfun(nfitt,xctmp,fsumsq)
  if (ioproc) then
    write(ioout,'(/,''  Final sum of squares = '',f20.6)') fsumsq
    if (.not.lga.and..not.lsimplex) write(ioout,'(/,''  Final gradient norm  = '',f20.6)') gnormfit
!
!  Output parameters and residuals
!
    call outfit(wvec,xcoriginal,gc,.not.lsimplex.and..not.lga)
  endif
!
!  Output fitted polarisability data
!
  call outpolar
!
!  Output fitted potentials
!
  call outpot
!
!  Return structural parameters to original values at the end
!  of a relax fitting run
!
  if (lrelax) then
    ncfold = 0
    do i = 1,nobs
      nt = nobtyp(i)
      if (nt.eq.6) then
        ncf = nobcfg(i)
        nptr = nobptr(i)
        if (ncf.ne.ncfold) then
          ncfold = ncf
          call setup(.true.)
        endif
        ind = iopt(nptr)
!
!  Structural parameters
!
        if (ind.gt.nstrains) then
!
!  Internal fractional coordinate
!
          nj = (ind-(nstrains-2))/3
          idj = ind - (3*nj+(nstrains-3))
          if (iatn(nj).le.maxele) then
            dum = fobs(i)
            if (idj.eq.1) then
              xcfg(nsft+nj) = dum
            elseif (idj.eq.2) then
              ycfg(nsft+nj) = dum
            elseif (idj.eq.3) then
              zcfg(nsft+nj) = dum
            endif
!
!  Take care of constraints by calling setup
!
            call setup(.true.)
          endif
        endif
      endif
    enddo
  endif
!
!  Rescale constraint additive parameters
!
  do i = 1,nfcon
    if (nfcotyp(i).eq.1) then
      fconadd(i) = fconadd(i)*scale(nfcfix(i))
      fconco(i) = fconco(i)*(scale(nfcfix(i))/scale(nfcvar(i)))**fconpower(i)
    else
      nv1 = nfcvar(i)
      nv2 = nint(fconadd(i))
      if (nv1.eq.nv2) then
        tmp = scale(nv1)
      else
        tmp = scale(nv1)*scale(nv2)
      endif
      rtmp = scale(nfcfix(i))/sqrt(abs(tmp))
      fconco(i) = fconco(i)*rtmp
    endif
  enddo
  do i = 1,nfcon
    if (nfcotyp(i).eq.1) then
!
!  Set scale factors to zero so that restart file output of
!  fconco and fconadd are correct
!
      scale(nfcfix(i)) = 1.0_dp
      scale(nfcvar(i)) = 1.0_dp
    endif
  enddo
!
!  Turn off relax mode at end of fitting
!
  lrelax = .false.
!
!  Reset lfreqout 
!
  lfreqout = lfreqoutsave
!
!  Free local memory
!
  deallocate(fres,stat=status)
  if (status/=0) call deallocate_error('calls','fres')
  deallocate(wvec,stat=status)
  if (status/=0) call deallocate_error('calls','wvec')
  deallocate(xctmp,stat=status)
  if (status/=0) call deallocate_error('calls','xctmp')
  deallocate(xcoriginal,stat=status)
  if (status/=0) call deallocate_error('calls','xcoriginal')
  deallocate(xc,stat=status)
  if (status/=0) call deallocate_error('calls','xc')
  deallocate(gc,stat=status)
  if (status/=0) call deallocate_error('calls','gc')
!
!  Timing
!
  time1 = cputime()
  time1 = time1 - time0
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(/,''  Total time to end of fitting = '',f12.4,'' seconds'')') time1
  endif
!
  return
  end
