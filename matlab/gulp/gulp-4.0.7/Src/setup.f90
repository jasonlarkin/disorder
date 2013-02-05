  subroutine setup(lall)
!
!  Set up each configuration
!
!   4/98 lra forced to be consistent with the space group
!   5/00 call to setup polarisability data added
!  11/00 arrays for structure prediction added (cn*/ox*)
!   4/02 Pointer to cores added
!  10/02 Storing of initial coordinates for external forces moved here
!  11/02 lewald flag turned on if EEM type method is being used
!  11/02 leinstein flag set to indicate presence of Einstein model
!   1/03 definition of lsymopt know includes space group
!   1/03 lewald flag turned off if lwolf is true
!   4/03 Check on sum of charges for individual regions
!   5/03 lspatialok initialised to .false. as default for config
!   6/03 Constraint handling corrected
!   4/04 lstr set to true if pressure file is to be written during MD
!   9/04 lewald set to true if there are any bond order charge potentials
!   9/04 Requirement for ndim > 0 for lewald to be set true removed
!   7/05 Streitz and Mintmire modifications added
!   7/05 Initialisation of EAM pointer call added
!   7/05 Copy atom to species number pointer to local array for configuration
!  12/05 Symmetry adapted derivative algorithms turned off for operator input
!  11/06 lfirst argument added to equpos call
!   5/07 Partial occupancy array initialisation added
!   5/07 Call to setspatial(bo) modified
!   6/07 Computation of region 1 species counters added
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 Forcing of lewald = .true. when lreaxFF is true removed
!   1/08 lreaxFFqreal removed
!   3/08 Array containing number of atoms of each species added
!   4/08 Turning off of lewald for lwolf = .true. case added back
!   4/08 Call to spatial decomposition version of setmol added
!  10/08 COSMIC setup added
!   6/09 Range of constrain index limited by mvar on upper bound for coordinates
!   7/09 cutoffmax(bo) removed as this is now passed via general module
!   5/10 Spatial decomposition turned off for Monte Carlo calculations
!   8/10 Spatial decomposition enabled for ReaxFF
!  10/10 Spatial decomposition enabled for EDIP model
!  11/10 Anisotropic pressure added
!   3/11 lstr is now true if lstressout is true
!   5/11 lstr now set to be true if this MD and the cell is 3-D so that pressure is computed
!        correctly.
!   7/11 Spatial option added for partial occupancy pointer routines
!   8/11 Check on compatibility of cell optimisation and electric field added
!   5/12 Atomic stresses causes symmetry to be turned off for derivatives
!   9/12 Pacha added
!  12/12 Time-dependent field added
!  12/12 Modified to allow for multiple time-dependent fields
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
!  Julian Gale, NRI, Curtin University, December 2012
!
  use bondorderdata, only : nboQ, nbopot
  use control
  use configurations
  use current
  use datatypes
  use element,       only : lqeq, maxele, lSandM, lpacha
  use field,         only : lfieldcfg, ntdfieldcfg
  use files,         only : lpre
  use iochannels
  use mdlogic,       only : lmd
  use molecule
  use parallel
  use partial
  use polarise
  use reallocate
  use reaxFFdata,    only : nreaxFFspec
  use shell
  use spatial,       only : lspatialok
  use spatialbo,     only : lspatialBOok
  use species
  use sutton,        only : lsuttonc
  use symmetry
  implicit none
!
!  Passed variables
!
  logical, intent(in)                          :: lall
!
!  Local variables
!
  character(len=1)                             :: cs
  character(len=5)                             :: lab
  integer(i4), dimension(:), allocatable       :: ncount
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: inat
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: mvar
  integer(i4)                                  :: nf
  integer(i4)                                  :: nf2
  integer(i4)                                  :: nr
  integer(i4)                                  :: nspg
  integer(i4)                                  :: nv
  integer(i4)                                  :: nv2
  integer(i4)                                  :: nvj
  integer(i4)                                  :: nvj2
  integer(i4)                                  :: nvk
  integer(i4)                                  :: nvk2
  integer(i4)                                  :: status
  logical                                      :: lfound
  logical                                      :: lp1
  logical                                      :: lqok
  real(dp)                                     :: asum
  real(dp)                                     :: diff
  real(dp)                                     :: qregion(2)
  real(dp)                                     :: qtrm
  real(dp)                                     :: sum
  real(dp)                                     :: vcrd
  real(dp)                                     :: vcrdj
  real(dp)                                     :: vcrdk
!
!  Initialise flags
!
  lspatialok = .false.
  lspatialBOok = .false.
  lstr = .false.
  lp1 = (hmssg(1,ncf).eq.'P'.and.hmssg(3,ncf).eq.'1'.and.hmssg(4,ncf).eq.' '.and.hmssg(5,ncf).eq.' ')
  lsymopt = (lsymset(ncf).and.(nspcg(ncf).gt.1.or..not.lp1.or.ngocfg(ncf).gt.1)) 
  lqok = (index(keyword,'qok').ne.0)
!
!  We can only use symmetry based derivative algorithms if the space group was input so that the
!  orientation of the cell is correctly handled.
!
  if (lsymopt.and.(lsymdok.and.nspcg(ncf).gt.1)) then
    lsymderv = .true.
    lsymderv2 = .true.
  else
    lsymderv = .false.
    lsymderv2 = .false.
  endif
!
!  Symmetry adapted second derivative algorithm cannot be
!  used with variable charges
!
  if (index(keyword,'nod2').ne.0.or.leem) lsymderv2 = .false.
!
!  Free energy cannot use symmetry adapted derivatives yet
!
  if (lfree) then
    lsymderv = .false.
    lsymderv2 = .false.
  endif
!
!  Atomic stresses cannot use symmetry either
!
  if (latomicstress) then
    lsymderv = .false.
    lsymderv2 = .false.
  endif
!
!  Find first atom shift
!
  nsft = 0
  if (ncf.gt.1) then
    do i = 1,ncf-1
      nsft = nsft + nascfg(i)
    enddo
  endif
!
!  Set dimensionality
!
  ndim = ndimen(ncf)
!
!  Set number of strains according to the dimensionality and pointer
!
  if (ndim.eq.3) then
    nstrains = 6
    nstrptr(1) = 1
    nstrptr(2) = 2
    nstrptr(3) = 3
    nstrptr(4) = 4
    nstrptr(5) = 5
    nstrptr(6) = 6
  elseif (ndim.eq.2) then
    nstrains = 3
    nstrptr(1) = 1
    nstrptr(2) = 2
    nstrptr(3) = 6
  elseif (ndim.eq.1) then
    nstrains = 1
    nstrptr(1) = 1
  else
    nstrains = 0
  endif
!
!  Set total charge
!
  totalcharge = totalchargecfg(ncf)
!
!  Set pressure and temperature
!
  press = presscfg(ncf)
  lanisotropicpress = lanisotropicpresscfg(ncf)
  if (lanisotropicpresscfg(ncf)) then
    anisotropicpress(1:6) = anisotropicpresscfg(1:6,ncf)
  else
    anisotropicpress(1:6) = 0.0_dp
  endif
  temperature = tempcfg(ncf)
  temperaturestep = tempstp(ncf)
  ntemperaturestep = ntempstp(ncf)
  ntemperaturestepstart = ntempstpstart(ncf)
!
!  Set stress
!
  stress(1:6) = stresscfg(1:6,ncf)
!
!  Set mode restrictions for free energy
!
  maxmode = maxmodecfg(ncf)
  minmode = minmodecfg(ncf)
  nummode = nummodecfg(ncf)
!
!  Transfer stored configuration data into working arrays
!
  nasym = nascfg(ncf)
  nbsmat = 0
  do i = 1,nasym
    iatn(i) = natcfg(nsft+i)
    natype(i) = ntypcfg(nsft+i)
    nspecptr(i) = nspecptrcfg(nsft+i)
    xafrac(i) = xcfg(nsft+i)
    yafrac(i) = ycfg(nsft+i)
    zafrac(i) = zcfg(nsft+i)
    qa(i) = qlcfg(nsft+i)
    occua(i) = occucfg(nsft+i)
    rada(i) = radcfg(nsft+i)
    if (lbsmat(nsft+i)) nbsmat = nbsmat + 1
    oxa(i) = oxcfg(nsft+i)
    cna(i) = cncfg(nsft+i)
  enddo
!
!  Set up constraint pointers
!
  if (ncf.eq.ncfg) then
    if (ncontot.gt.0) ncon = ncontot + 1 - n1con(ncf)
  else
    ncon = n1con(ncf+1) - n1con(ncf)
  endif
  if (ncon.gt.maxcon) then
    maxcon = ncon
    call changemaxcon
  endif
  ncfst = n1con(ncf) - 1
!
!  Apply constraints
!
  mvar = 3*nasym + nstrains
  if (ncon.gt.0) then
    do j = 1,ncon
      ncfix(j) = ncfixcfg(ncfst+j)
      ncvar(j) = ncvarcfg(ncfst+j)
      conco(j) = concocfg(ncfst+j)
      conadd(j) = conaddcfg(ncfst+j)
    enddo
    do j = 1,ncon
      nf = ncfix(j)
      if (nf.gt.nstrains.and.nf.le.mvar) then
        nf = nf - nstrains
        nf2 = (nf - 1)/3 + 1
        nf = nf - 3*(nf2 - 1)
        if (nf.eq.1) then
          xafrac(nf2) = 0.0_dp
        elseif (nf.eq.2) then
          yafrac(nf2) = 0.0_dp
        else
          zafrac(nf2) = 0.0_dp
        endif
      endif
    enddo
    do j = 1,ncon
      nv = ncvar(j)
      if (nv.gt.nstrains.and.nf.le.mvar) then
        nv = nv - nstrains
        nv2 = (nv - 1)/3 + 1
        nv = nv - 3*(nv2 - 1)
        if (nv.eq.1) then
          vcrd = xafrac(nv2)
        elseif (nv.eq.2) then
          vcrd = yafrac(nv2)
        else
          vcrd = zafrac(nv2)
        endif
        nf = ncfix(j) - nstrains
        nf2 = (nf - 1)/3 + 1
        nf = nf - 3*(nf2 - 1)
        if (nf.eq.1) then
          xafrac(nf2) = vcrd*conco(j) + conadd(j) + xafrac(nf2)
        elseif (nf.eq.2) then
          yafrac(nf2) = vcrd*conco(j) + conadd(j) + yafrac(nf2)
        else
          zafrac(nf2) = vcrd*conco(j) + conadd(j) + zafrac(nf2)
        endif
      endif
    enddo
!
!  Handle additive constraints for fractional coordinates
!  - take nearest pair of images
!
    if (ndim.eq.3) then
      allocate(ncount(mvar),stat=status)
      if (status/=0) call outofmemory('setup','ncount')
      do i = 1,mvar
        ncount(i) = 0
      enddo
      do i = 1,ncon
        ii = ncfix(i)
        ncount(ii) = ncount(ii) + 1
      enddo
      do i = nstrains+1,mvar
        if (ncount(i).ge.2) then
          lfound = .false.
          j = 0
          do while (.not.lfound.and.j.lt.ncon-1)
            j = j + 1
            if (ncfix(j).eq.i) then
              k = j
              do while (.not.lfound.and.k.lt.ncon) 
                k = k + 1
                lfound = (ncfix(k).eq.i)
              enddo
            endif
          enddo
          if (lfound) then
            nvj = ncvar(j)
            nvj2 = (nvj - (nstrains - 2))/3
            nvj = nvj - nvj2*3 - (nstrains - 3)
            if (nvj.eq.1) then
              vcrdj = xafrac(nvj2)
            elseif (nvj.eq.2) then
              vcrdj = yafrac(nvj2)
            else
              vcrdj = zafrac(nvj2)
            endif
            nvk = ncvar(k)
            nvk2 = (nvk - (nstrains - 2))/3
            nvk = nvk - nvk2*3 - (nstrains - 3)
            if (nvk.eq.1) then
              vcrdk = xafrac(nvk2)
            elseif (nvk.eq.2) then
              vcrdk = yafrac(nvk2)
            else
              vcrdk = zafrac(nvk2)
            endif
            diff = abs(vcrdk - vcrdj)
            if (diff.gt.0.5_dp) then
              nf = ncfix(j)
              nf2 = (nf - (nstrains - 2))/3
              nf = nf - nf2*3 - (nstrains - 3)
              if (nf.eq.1) then
                xafrac(nf2) = xafrac(nf2) + 0.5_dp
                xafrac(nf2) = mod(xafrac(nf2),1.0_dp)
              elseif (nf.eq.2) then
                yafrac(nf2) = yafrac(nf2) + 0.5_dp
                yafrac(nf2) = mod(yafrac(nf2),1.0_dp)
              else
                zafrac(nf2) = zafrac(nf2) + 0.5_dp
                zafrac(nf2) = mod(zafrac(nf2),1.0_dp)
              endif
            endif
          endif
        endif
      enddo
      deallocate(ncount,stat=status)
      if (status/=0) call deallocate_error('setup','ncount')
    endif
  endif
!********************
!  Symmetry set up  *
!********************
  if (lsymopt) then
    call symmet
    call equpos(lall,.true.)
  else
!***********************
!  No symmetry set up  *
!***********************
    ncbl = 1
    nccs = 1
    numat = nasym
    do i = 1,nasym
      qf(i) = qa(i)
      occuf(i) = occua(i)
      radf(i) = rada(i)
      nat(i) = iatn(i)
      nftype(i) = natype(i)
      neqv(i) = 1
      nrelat(i) = i
      nrel2(i) = i
      xfrac(i) = xafrac(i)
      yfrac(i) = yafrac(i)
      zfrac(i) = zafrac(i)
      cnf(i) = cna(i)
      oxf(i) = oxa(i)
    enddo
  endif
!********************************
!  Count atoms of each species  *
!********************************
  numofspec(1:nspec) = 0
  do i = 1,nasym
    ii = nspecptr(i)
    numofspec(ii) = numofspec(ii) + neqv(i)
  enddo
!*********************
!  Set up unit cell  *
!*********************
  if (ndim.eq.3) then
    do i = 1,3
      rv(1,i) = rvcfg(1,i,ncf)
      rv(2,i) = rvcfg(2,i,ncf)
      rv(3,i) = rvcfg(3,i,ncf)
    enddo
    call uncell3D(rv,a,b,c,alpha,beta,gamma)
    if (a.gt.1.0d-12) then
      recipa = 1.0_dp/a
    else
      recipa = 0.0_dp
    endif
    if (b.gt.1.0d-12) then
      recipb = 1.0_dp/b
    else
      recipb = 0.0_dp
    endif
    if (c.gt.1.0d-12) then
      recipc = 1.0_dp/c
    else
      recipc = 0.0_dp
    endif
    r1x = rv(1,1)
    r1y = rv(2,1)
    r1z = rv(3,1)
    r2x = rv(1,2)
    r2y = rv(2,2)
    r2z = rv(3,2)
    r3x = rv(1,3)
    r3y = rv(2,3)
    r3z = rv(3,3)
    sum = abs(r2x) + abs(r1y) + abs(r3x) + abs(r1z) + abs(r3y) + abs(r2z)
    lra = (sum.lt.1.0d-6)
!
!  Make sure lra is consistent with the space group
!
    nspg = nspcg(ncf)
    if (nspg.le.15.or.(nspg.ge.143.and.nspg.le.194)) then
      lra = .false.
    endif
  elseif (ndim.eq.2) then
    do i = 1,2
      rv(1,i) = rvcfg(1,i,ncf)
      rv(2,i) = rvcfg(2,i,ncf)
    enddo
    call uncell2D(rv,a,b,alpha)
    if (a.gt.1.0d-12) then
      recipa = 1.0_dp/a
    else
      recipa = 0.0_dp
    endif
    if (b.gt.1.0d-12) then
      recipb = 1.0_dp/b
    else
      recipb = 0.0_dp
    endif
    r1x = rv(1,1)
    r1y = rv(2,1)
    r1z = 0.0_dp
    r2x = rv(1,2)
    r2y = rv(2,2)
    r2z = 0.0_dp
    r3x = 0.0_dp
    r3y = 0.0_dp
    r3z = 0.0_dp
    lra = (abs(alpha-90.0_dp).lt.1.0d-6)
  elseif (ndim.eq.1) then
    rv(1,1) = rvcfg(1,1,ncf)
    call uncell1D(rv,a)
    if (a.gt.1.0d-12) then
      recipa = 1.0_dp/a
    else
      recipa = 0.0_dp
    endif
    r1x = rv(1,1)
    r1y = 0.0_dp
    r1z = 0.0_dp
    r2x = 0.0_dp
    r2y = 0.0_dp
    r2z = 0.0_dp
    r3x = 0.0_dp
    r3y = 0.0_dp
    r3z = 0.0_dp
    lra = .false.
  elseif (ndim.eq.0) then
    r1x = 0.0_dp
    r1y = 0.0_dp
    r1z = 0.0_dp
    r2x = 0.0_dp
    r2y = 0.0_dp
    r2z = 0.0_dp
    r3x = 0.0_dp
    r3y = 0.0_dp
    r3z = 0.0_dp
  endif
!
!  Setup cell vectors for neighbouring cells
!
  call rlist
!***********************************
!  Generate cartesian coordinates  *
!***********************************
  if (ndim.eq.3) then
    do i = 1,numat
      xclat(i) = xfrac(i)*r1x + yfrac(i)*r2x + zfrac(i)*r3x
      yclat(i) = xfrac(i)*r1y + yfrac(i)*r2y + zfrac(i)*r3y
      zclat(i) = xfrac(i)*r1z + yfrac(i)*r2z + zfrac(i)*r3z
    enddo
  elseif (ndim.eq.2) then
    do i = 1,numat
      xclat(i) = xfrac(i)*r1x + yfrac(i)*r2x
      yclat(i) = xfrac(i)*r1y + yfrac(i)*r2y
      zclat(i) = zfrac(i)
    enddo
  elseif (ndim.eq.1) then
    do i = 1,numat
      xclat(i) = xfrac(i)*r1x
      yclat(i) = yfrac(i)
      zclat(i) = zfrac(i)
    enddo
  else
    do i = 1,numat
      xclat(i) = xfrac(i)
      yclat(i) = yfrac(i)
      zclat(i) = zfrac(i)
    enddo
  endif
  if (lsymopt) then
    do i = 1,nasym
      nr = nrel2(i)
      xalat(i) = xclat(nr)
      yalat(i) = yclat(nr)
      zalat(i) = zclat(nr)
    enddo
  else
    do i = 1,nasym
      xalat(i) = xclat(i)
      yalat(i) = yclat(i)
      zalat(i) = zclat(i)
    enddo
  endif
!
!  Save initial Cartesian coordinates in case external forces are applied
!
  do i = 1,nasym
    xinitial(i) = xalat(i)
    yinitial(i) = yalat(i)
    zinitial(i) = zalat(i)
  enddo
!
!  COSMO parameters
!
  if (lcosmo) call setcosmo
!
!  Check charge and Ewald summation flag
!
  sum = 0.0_dp
  asum = 0.0_dp
  qregion(1:2) = 0.0_dp
  do i = 1,numat
    qtrm = qf(i)*occuf(i)
    sum = sum + qtrm
    asum = asum + abs(qtrm)
    if (nregionno(nsft+nrelat(i)).eq.1) then
      qregion(1) = qregion(1) + qtrm
    else
      qregion(2) = qregion(2) + qtrm
    endif
  enddo
  if (.not.lqok.and.abs(sum).gt.1d-4.and.lall.and.ndim.ne.0) then
    if (ioproc) then
      write(ioout,'(/,''  Configuration number = '',i3,/)')ncf
      write(ioout,'(''  **** Unit cell is not charge neutral    ****'')')
      write(ioout,'(''  **** Sum of charges = '',f15.10,''   ****'')')sum
      write(ioout,'(''  **** Check that a special position atom ****'')')
      write(ioout,'(''  **** coordinate has not been varied     ****'')')
      write(ioout,'(/)')
      write(ioout,'(''  Current coordinates : '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''     No.  Atomic         x            y            z          Charge  Occupancy'')')
      if (ndim.eq.3) then
        write(ioout,'(''          Number       (frac)       (frac)       (frac)         (e)  '')')
      elseif (ndim.eq.2) then
        write(ioout,'(''          Number       (frac)       (frac)       (Angs)         (e)  '')')
      elseif (ndim.eq.1) then
        write(ioout,'(''          Number       (frac)       (Angs)       (Angs)         (e)  '')')
      endif
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,numat
        inat = nat(i)
        itype = nftype(i)
        cs = 'c'
        if (inat.gt.maxele) cs = 's'
        call label(inat,itype,lab)
        if (ndim.eq.3) then
          write(ioout,'(3x,i4,2x,a5,1x,a1,1x,3(4x,f9.6),2x,f12.6,2x,f6.4)') &
            i,lab,cs,xfrac(i),yfrac(i),zfrac(i),qf(i),occuf(i)
        elseif (ndim.eq.2) then
          write(ioout,'(3x,i4,2x,a5,1x,a1,1x,2(4x,f9.6),4x,f9.4,2x,f12.6,2x,f6.4)') &
            i,lab,cs,xfrac(i),yfrac(i),zclat(i),qf(i),occuf(i)
        elseif (ndim.eq.1) then
          write(ioout,'(3x,i4,2x,a5,1x,a1,1x,4x,f9.6,2(4x,f9.4),2x,f12.6,2x,f6.4)') &
            i,lab,cs,xfrac(i),yfrac(i),zclat(i),qf(i),occuf(i)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(/)')
    endif
    call stopnow('setup')
  endif
!
!  Set flag as to whether Ewald sum is needed
!
  lewald = ((asum.ne.0.0_dp.or.(lc6.and.ndim.eq.3)).and.index(keyword,'noel').eq.0)
  if (leem.or.lqeq.or.lSandM.or.lpacha.or.(nboQ.gt.0)) lewald = .true.
  if (lwolf) lewald = .false.
!
!  Set Ewald parameters for system if required
!
  if (lewald.and.ndim.gt.1) call setewald
!
!  Check on region charge sum
!
  if (.not.lqok.and.abs(qregion(1)).gt.1d-4.and.lall.and.ndim.ne.0) then
    call outerror('region 1 is not charge neutral',0_i4)
    call stopnow('setup')
  elseif (.not.lqok.and.abs(qregion(2)).gt.1d-4.and.lall.and.ndim.ne.0) then
    call outerror('region 2 is not charge neutral',0_i4)
    call stopnow('setup')
  endif
!
!  Set up spatial decomposition if needed
!
  lspatialok = (lspatial.and..not.lmc)
  lspatialBOok = (lspatial.and.(lbrenner.or.lEDIP.or.(nboQ+nbopot).gt.0.or.nreaxFFspec.gt.0).and..not.lmc)
  if (lspatialok) then
    call setcutoffmax
    call setspatial(.true.)
  endif
  if (lspatialBOok) then
    call setcutoffmaxbo
    call setspatialbo(.true.)
  endif
!
!  Calculate parallel division of work :
!   
!  Spatial - divide cells over processors
!  Non-spatial - divide atoms over processors
!
  call setatomnodes(numat,nprocs,procid,lspatialok)
  call setatomnodesbo(numat,nprocs,procid,lspatialBOok)
!
!  Set flag for Einstein model
!
  leinstein = .false.
  do i = 1,nasym
    if (leinsteinat(nsft+i)) leinstein = .true.
  enddo
!
!  Set up one centre C terms
!
  if (lall.and.lc6one) then
    do i = 1,nasym
      do j = 1,nspec
        if (iatn(i).eq.natspec(j).and.(natype(i).eq.ntypspec(j).or.ntypspec(j).eq.0)) then
          c6a(i) = c6spec(j)
        endif
      enddo
    enddo
    do i = 1,numat
      do j = 1,nspec
        if (nat(i).eq.natspec(j).and.(nftype(i).eq.ntypspec(j).or.ntypspec(j).eq.0)) then
          c6f(i) = c6spec(j)
        endif
      enddo
    enddo
  endif
!
!  Set up shell pointer array for properties section
!
  ncore    = 0
  ncorer1  = 0
  nshell   = 0
  nshellr1 = 0
  do i = 1,numat
    if (nat(i).gt.maxele) then
      nshell = nshell + 1
      nshptr(nshell) = i
      if (nregionno(nsft+nrelat(i)).eq.1) nshellr1 = nshellr1 + 1
    else
      ncore = ncore + 1
      ncoptr(ncore) = i
      if (nregionno(nsft+nrelat(i)).eq.1) ncorer1 = ncorer1 + 1
    endif
  enddo
!
!  The values of ncorer1 and nshellr1 need to contain the number of species in region 1 only
!  for the 2-D case. For other dimensionalities, set equal to the total number of species
!
  if (ndim.ne.2) then
    ncorer1 = ncore
    nshellr1 = nshell
  endif
!
!  EAM set up if needed
!
  if (lsuttonc) call seteam
!
!  Polarisability set up if needed
!
  if (lpolar) call setpolar
!
!  Core-shell pair check
!
  if (lall) call cscheck
! 
!  Set breathing shell pointer
! 
  call setbsmptr(nbs,nbss,nbsptr,(ndim.eq.2))
! 
!  Set partial occupancy pointer
!
  if (lspatialok) then
    call setoccshptrs(nsfoc,nbsfoc,iocshptr,ibocshptr,(ndim.eq.2))
  else
    call setoccshptr(nsfoc,nbsfoc,iocshptr,ibocshptr,(ndim.eq.2))
  endif
  if (lspatialok) then
    call setoccptrs(ncfoc,nsfoc,nbfoc,iocptr,ibocptr,(ndim.eq.2))
  else
    call setoccptr(ncfoc,nsfoc,nbfoc,iocptr,ibocptr,(ndim.eq.2))
  endif
  ncsfoc = ncfoc + nsfoc
  lpocc = (ncsfoc.ne.numat)
!
!  If molecule option is selected call molecule setup routine
!
  if (lall.and.index(keyword,'mol').ne.0) then
    if (lspatialok) then
      call setmols
    else
      call setmol
    endif
  endif
!
!  Set up variables for optimisation and fitting
!
  nvar = nvarcfg(ncf)
  nfst = n1var(ncf) - 1
  ncell = 0
  nbsm = 0
!
  do i = 1,nvar
    iopt(i) = ioptcfg(i+nfst)
    if (iopt(i).le.nstrains) then
      ncell = ncell + 1
    elseif (iopt(i).gt.3*nasym+nstrains) then
      nbsm = nbsm + 1
    endif
  enddo
!
!  Check that no cell variables are specified with Einstein model
!
  if (leinstein.and.ncell.gt.0) then
    call outerror('cell must be fixed with Einstein model',0_i4)
    call stopnow('setup')
  endif
!
!  Check that no cell variables are specified with electric field
!
  if ((lfieldcfg(ncf).or.(ntdfieldcfg(ncf)).gt.0).and.ncell.gt.0) then
    call outerror('cell must be fixed with an electric field',0_i4)
    call stopnow('setup')
  endif
!
!  If any cell variables are to be optimised or a property calculation performed
!  then lstr must be true. Alternatively, if the pressure file is to be generated
!  in an MD run. Also, if the stress tensor is to be output then we must do the
!  strain derivatives. Furthermore, if this is a 3-D system and MD then set lstr
!  to be true so that the pressure is computed correctly.
!
  lstr = (ncell.gt.0.or.lprop.or.(lmd.and.(lpre.or.ndim.eq.3)).or.lstressout)
!
!  Surface energy flag - if cell is being optimised, don't do surface energy
!
  lseok = ((ndim.eq.1.or.ndim.eq.2).and.sbulkecfg(ncf).ne.0.0_dp.and.ncell.eq.0)
  nzmol = nzmolcfg(ncf)
!
  return
  end
