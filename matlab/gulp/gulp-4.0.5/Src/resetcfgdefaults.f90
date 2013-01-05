  subroutine resetcfgdefaults(nconfig)
!
!  Resets values to their initial defaults for configurations
!
!   5/07 Created
!   5/07 nregiontype & QMMMmode added
!   9/10 lfcscatter added
!  11/10 Anisotropic pressure added
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, November 2010
!
  use configurations
  use current,        only : maxat
  use defects
  use distances,      only : ndistancereset
  use field
  use freeze
  use genetic,        only : xmaxcfg, xmincfg
  use ksample
  use moldyn
  use neb
  use potentialgrid
  use potentialpoints
  use projectdos
  use radial
  use reallocate
  use scan
  use shifts
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: nconfig
!
!  Local variables
!
  integer(i4)             :: j
!
!  Check that configuration input is an allowed one
!
  if (nconfig.lt.1.or.nconfig.gt.maxcfg) then
    call outerror('invalid configuration number passed to resetcfgdefaults',0_i4)
    call stopnow('resetcfgdefaults')
  endif
!
!  Initialise defaults for specified configuration
!
  anisotropicpresscfg(1:6,nconfig) = 0.0_dp
  bornk(1,nconfig) = 1.0_dp
  bornk(2,nconfig) = 1.0_dp
  bornk(3,nconfig) = 1.0_dp
  dhklcfg(nconfig) = 0.0_dp
  energycfg(nconfig) = 0.0_dp
  lanisotropicpresscfg(nconfig) = .false.
  ldeflin(nconfig) = .false.
  lfcborn(nconfig) = .false.
  lfcphon(nconfig) = .false.
  lfcprop(nconfig) = .false.
  lfcscatter(nconfig) = .false.
  lmdconstrain(nconfig) = .false.
  lnebvaryspring(nconfig) = .false.
  lomega(nconfig) = .false.
  lopfc(nconfig) = .false.
  lopfreg(1:3*maxregion,nconfig) = .false.
  lreldin(nconfig) = .false.
  lsymset(nconfig) = .false.
  lufree(nconfig) = .false.
  lvecin(nconfig) = .false.
  ifso(nconfig) = 0
  ifhr(nconfig) = 0
  iflags(nconfig) = 0
  iperm(nconfig) = 1
  iufree(nconfig) = 0
  ivso(1:3,nconfig) = 0
  maxmodecfg(nconfig) = 0
  minmodecfg(nconfig) = 1
  n1con(nconfig) = 1
  n1var(nconfig) = 0
  nascfg(nconfig) = 0
  nbornstep(nconfig) = 0
  ndcentyp(nconfig) = 0
  ndimen(nconfig) = 0
  ndistancereset(nconfig) = 1
  nebspring(nconfig) = 0.00005_dp
  nebspringmin(nconfig) = 0.00005_dp
  nebfinalcell(1:6,nconfig) = 0.0_dp
  nebfinalradius(1:maxat,nconfig) = 0.0_dp
  nebfinalxyz(1:3,1:maxat,nconfig) = 0.0_dp
  neiglow(nconfig) = 0
  neighigh(nconfig) = 0
  nensemble(nconfig) = 1
  ngocfg(nconfig) = 1
  nmdconstrainatom(1:2,nconfig) = 0
  nmdeq(nconfig) = 0
  nmdprod(nconfig) = 0
  nmdsamp(nconfig) = 0
  nmdvelmode(nconfig) = - 1
  nmdvelmodp(nconfig) = - 1
  nmdwrite(nconfig) = 0
  nnebreplica(nconfig) = 0
  nomegastep(nconfig) = 0
  norigkpt(nconfig) = 0
  npotptcfg(nconfig) = 0
  nprojcfg(nconfig) = 0
  nprojdef(nconfig) = 0
  nregions(nconfig) = 1
  nccscfg(nconfig) = 1
  nshcfg(nconfig) = 1
  nspcg(nconfig) = 1
  nsregion2(nconfig) = 0
  nsuper(nconfig) = 1
  ntempstp(nconfig) = 0
  ntempstpstart(nconfig) = 0
  ntran(nconfig) = 0
  nummodecfg(nconfig) = 0
  nzmolcfg(nconfig) = 1
  nxks(nconfig) = 0
  nyks(nconfig) = 0
  nzks(nconfig) = 0
  nvarcfg(nconfig) = 0
  QMMMmode(nconfig) = 0
  nmdconstraindist(nconfig) = 0.0_dp
  presscfg(nconfig) = 0.0_dp
  omega(nconfig) = 0.0_dp
  omegadamping(nconfig) = 5.0_dp
  omegadir(1:6,nconfig) = 0.0_dp
  omegadirtype(nconfig) = 1
  omegastep(nconfig) = 0.0_dp
  qpres(nconfig) = 0.1_dp
  qtemp(nconfig) = 0.1_dp
  reg1(nconfig) = 0.0_dp
  reg2(nconfig) = 0.0_dp
  reg1last(nconfig) = 0.0_dp
  reg2a1(nconfig) = 0.0_dp
  rufree(nconfig) = 0.0_dp
  rvcfg(1:3,1:3,nconfig) = 0.0_dp
  sbulkecfg(nconfig) = 0.0_dp
  shift(nconfig) = 0.0_dp
  shscalecfg(nconfig) = 1.0_dp
  stresscfg(1:6,nconfig) = 0.0_dp
  tempcfg(nconfig) = 0.0_dp
  tempstp(nconfig) = 0.0_dp
  tmdeq(nconfig) = 0.0_dp
  tmdforcestart(nconfig) = 0.0_dp
  tmdforcestop(nconfig) = 0.0_dp
  tmdprod(nconfig) = 0.0_dp
  tmdsamp(nconfig) = 0.0_dp
  tmdscale(nconfig) = 0.0_dp
  tmdscint(nconfig) = 0.0_dp
  tmdwrite(nconfig) = 0.0_dp
  totalchargecfg(nconfig) = 0.0_dp
  tstep(nconfig) = 0.0_dp
  xdcent(nconfig) = 0.0_dp
  ydcent(nconfig) = 0.0_dp
  zdcent(nconfig) = 0.0_dp
  xtran(nconfig) = 0.0_dp
  ytran(nconfig) = 0.0_dp
  ztran(nconfig) = 0.0_dp
  xufree(1:3,nconfig) = 0.0_dp
  nxpg(nconfig) = 0
  nypg(nconfig) = 0
  nzpg(nconfig) = 0
  xminpg(nconfig) = 0.0_dp
  yminpg(nconfig) = 0.0_dp
  zminpg(nconfig) = 0.0_dp
  xmaxpg(nconfig) = 1.0_dp
  ymaxpg(nconfig) = 1.0_dp
  zmaxpg(nconfig) = 1.0_dp
  xmaxcfg(1:3,nconfig) = 1.0_dp
  xmincfg(1:3,nconfig) = 0.0_dp
  hmssg(1,nconfig) = '('
  hmssg(2,nconfig) = 'u'
  hmssg(3,nconfig) = 'n'
  hmssg(4,nconfig) = 'k'
  hmssg(5,nconfig) = 'n'
  hmssg(6,nconfig) = 'o'
  hmssg(7,nconfig) = 'w'
  hmssg(8,nconfig) = 'n'
  hmssg(9,nconfig) = ')'
  do j = 10,16
    hmssg(j,nconfig) = ' '
  enddo
  names(nconfig) = ' '
  do j = 1,maxregion
    nregiontype(j,nconfig) = 0
    if (j.eq.2) then
      lregionrigid(j,nconfig) = .true. 
    else
      lregionrigid(j,nconfig) = .false.
    endif
  enddo
!
  ropcfg(1:3,1:3,1,nconfig) = 0.0_dp
  ropcfg(1,1,1,nconfig) = 1.0_dp
  ropcfg(2,2,1,nconfig) = 1.0_dp
  ropcfg(3,3,1,nconfig) = 1.0_dp
  vitcfg(1:3,1,nconfig) = 0.0_dp
!
  lfieldcfg(nconfig) = .false.
  fieldcfg(nconfig) = 0.0_dp
  fielddirectioncfg(1,nconfig) = 0.0_dp
  fielddirectioncfg(2,nconfig) = 0.0_dp
  fielddirectioncfg(3,nconfig) = 1.0_dp
!
  lradialcfg(nconfig) = .false.
  radialKcfg(nconfig) = 0.0_dp
  radialXYZcfg(1:3,nconfig) = 0.0_dp
!
  return
  end
