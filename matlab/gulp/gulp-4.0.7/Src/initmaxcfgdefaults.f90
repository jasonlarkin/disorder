  subroutine initmaxcfgdefaults(icfg)
!
!  Reinitialises defaults associated with configuration icfg
!
!   9/10 Created from changemaxcfg
!  11/10 Anisotropic pressure added
!   9/11 Metadynamics internal code replaced with Plumed
!   6/12 nobsmodecfg added
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
  use configurations
  use cosmo
  use current,        only : maxat
  use defects
  use distances,      only : ndistancereset
  use field
  use freeze
  use genetic,        only : xmaxcfg, xmincfg
  use ksample
  use moldyn
  use m_pr
  use neb
  use observables,    only : freaction, maxobs, nobsmodecfg
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
  integer(i4), intent(in) :: icfg
!
!  Local variables
!
  integer(i4)             :: j
!
!  Initialise defaults for new part of array
!
  if (icfg.le.maxcfg.and.icfg.ge.1) then
    anisotropicpresscfg(1:6,icfg) = 0.0_dp
    bornk(1,icfg) = 1.0_dp
    bornk(2,icfg) = 1.0_dp
    bornk(3,icfg) = 1.0_dp
    cosmoepsilon(icfg) = 1.0_dp
    cosmodrsolv(icfg) = 0.1_dp
    cosmorsolv(icfg) = 1.0_dp
    dhklcfg(icfg) = 0.0_dp
    energycfg(icfg) = 0.0_dp
    freaction(icfg,1:maxobs) = 0.0_dp
    lanisotropicpresscfg(icfg) = .false.
    lcosmoeigin(icfg) = .false.
    ldeflin(icfg) = .false.
    lfcborn(icfg) = .false.
    lfcphon(icfg) = .false.
    lfcprop(icfg) = .false.
    lfcscatter(icfg) = .false.
    lmdconstrain(icfg) = .false.
    lnebvaryspring(icfg) = .false.
    lomega(icfg) = .false.
    lopfc(icfg) = .false.
    lopfreg(1:3*maxregion,icfg) = .false.
    lreldin(icfg) = .false.
    lsymset(icfg) = .false.
    lufree(icfg) = .false.
    lvecin(icfg) = .false.
    ifso(icfg) = 0
    ifhr(icfg) = 0
    iflags(icfg) = 0
    iperm(icfg) = 1
    iufree(icfg) = 0
    ivso(1:3,icfg) = 0
    maxmodecfg(icfg) = 0
    minmodecfg(icfg) = 1
    n1con(icfg) = 1
    n1var(icfg) = 0
    nascfg(icfg) = 0
    nbornstep(icfg) = 0
    ndcentyp(icfg) = 0
    ndimen(icfg) = 0
    ndistancereset(icfg) = 1
    nebspring(icfg) = 0.00005_dp
    nebspringmin(icfg) = 0.00005_dp
    nebfinalcell(1:6,icfg) = 0.0_dp
    nebfinalradius(1:maxat,icfg) = 0.0_dp
    nebfinalxyz(1:3,1:maxat,icfg) = 0.0_dp
    neiglow(icfg) = 0
    neighigh(icfg) = 0
    nensemble(icfg) = 1
    ngocfg(icfg) = 1
    nmdconstrainatom(1:2,icfg) = 0
    nmdeq(icfg) = 0
    nmdprod(icfg) = 0
    nmdsamp(icfg) = 0
    nmdvelmode(icfg) = - 1
    nmdvelmodp(icfg) = - 1
    nmdwrite(icfg) = 0
    nnebreplica(icfg) = 0
    nomegastep(icfg) = 0
    nobsmodecfg(icfg) = 0
    norigkpt(icfg) = 0
    npotptcfg(icfg) = 0
    nprojcfg(icfg) = 0
    nprojdef(icfg) = 0
    nregions(icfg) = 1
    nsasexcludemax(icfg) = - 1
    nsasexcludemin(icfg) = - 1
    nccscfg(icfg) = 1
    nshcfg(icfg) = 1
    nspcg(icfg) = 1
    nsregion2(icfg) = 0
    nsuper(icfg) = 1
    ntempstp(icfg) = 0
    ntempstpstart(icfg) = 0
    ntran(icfg) = 0
    nummodecfg(icfg) = 0
    nzmolcfg(icfg) = 1
    nxks(icfg) = 0
    nyks(icfg) = 0
    nzks(icfg) = 0
    nvarcfg(icfg) = 0
    QMMMmode(icfg) = 0
    nmdconstraindist(icfg) = 0.0_dp
    presscfg(icfg) = 0.0_dp
    pr_conscfg(icfg) = 0.0_dp
    omega(icfg) = 0.0_dp
    omegadamping(icfg) = 5.0_dp
    omegadir(1:6,icfg) = 0.0_dp
    omegadirtype(icfg) = 1
    omegastep(icfg) = 0.0_dp
    qpres(icfg) = 0.1_dp
    qtemp(icfg) = 0.1_dp
    reg1(icfg) = 0.0_dp
    reg2(icfg) = 0.0_dp
    reg1last(icfg) = 0.0_dp
    reg2a1(icfg) = 0.0_dp
    rufree(icfg) = 0.0_dp
    rvcfg(1:3,1:3,icfg) = 0.0_dp
    sbulkecfg(icfg) = 0.0_dp
    shift(icfg) = 0.0_dp
    shscalecfg(icfg) = 1.0_dp
    stresscfg(1:6,icfg) = 0.0_dp
    taubcfg(icfg) = 1.0_dp
    tautcfg(icfg) = 1.0_dp
    tempcfg(icfg) = 0.0_dp
    tempstp(icfg) = 0.0_dp
    tmdeq(icfg) = 0.0_dp
    tmdforcestart(icfg) = 0.0_dp
    tmdforcestop(icfg) = 0.0_dp
    tmdprod(icfg) = 0.0_dp
    tmdsamp(icfg) = 0.0_dp
    tmdscale(icfg) = 0.0_dp
    tmdscint(icfg) = 0.0_dp
    tmdwrite(icfg) = 0.0_dp
    totalchargecfg(icfg) = 0.0_dp
    tstep(icfg) = 0.0_dp
    xdcent(icfg) = 0.0_dp
    ydcent(icfg) = 0.0_dp
    zdcent(icfg) = 0.0_dp
    xtran(icfg) = 0.0_dp
    ytran(icfg) = 0.0_dp
    ztran(icfg) = 0.0_dp
    xufree(1:3,icfg) = 0.0_dp
    nxpg(icfg) = 0
    nypg(icfg) = 0
    nzpg(icfg) = 0
    xminpg(icfg) = 0.0_dp
    yminpg(icfg) = 0.0_dp
    zminpg(icfg) = 0.0_dp
    xmaxpg(icfg) = 1.0_dp
    ymaxpg(icfg) = 1.0_dp
    zmaxpg(icfg) = 1.0_dp
    xmaxcfg(1:3,icfg) = 1.0_dp
    xmincfg(1:3,icfg) = 0.0_dp
    hmssg(1,icfg) = '('
    hmssg(2,icfg) = 'u'
    hmssg(3,icfg) = 'n'
    hmssg(4,icfg) = 'k'
    hmssg(5,icfg) = 'n'
    hmssg(6,icfg) = 'o'
    hmssg(7,icfg) = 'w'
    hmssg(8,icfg) = 'n'
    hmssg(9,icfg) = ')'
    do j = 10,16
      hmssg(j,icfg) = ' '
    enddo
    names(icfg) = ' '
    do j = 1,maxregion
      nregiontype(j,icfg) = 0
      if (j.eq.2) then
        lregionrigid(j,icfg) = .true. 
      else
        lregionrigid(j,icfg) = .false.
      endif
    enddo
!
    ropcfg(1:3,1:3,1,icfg) = 0.0_dp
    ropcfg(1,1,1,icfg) = 1.0_dp
    ropcfg(2,2,1,icfg) = 1.0_dp
    ropcfg(3,3,1,icfg) = 1.0_dp
    vitcfg(1:3,1,icfg) = 0.0_dp
!
    lfieldcfg(icfg) = .false.
    ntdfieldcfg(icfg) = 0
    fieldcfg(icfg) = 0.0_dp
    fielddirectioncfg(1,icfg) = 0.0_dp
    fielddirectioncfg(2,icfg) = 0.0_dp
    fielddirectioncfg(3,icfg) = 1.0_dp
    td_fieldcfg(1,1:maxtdfield,icfg) = 0.0_dp
    td_fieldcfg(2,1:maxtdfield,icfg) = 0.0_dp
    td_fieldcfg(3,1:maxtdfield,icfg) = 0.0_dp
    td_fielddirectioncfg(1,1:maxtdfield,icfg) = 0.0_dp
    td_fielddirectioncfg(2,1:maxtdfield,icfg) = 0.0_dp
    td_fielddirectioncfg(3,1:maxtdfield,icfg) = 1.0_dp
    tmdfieldstart(icfg) = 0.0_dp
    tmdfieldstop(icfg) = 0.0_dp
!
    lradialcfg(icfg) = .false.
    radialKcfg(icfg) = 0.0_dp
    radialXYZcfg(1:3,icfg) = 0.0_dp
  endif
!
  return
  end
