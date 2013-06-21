  subroutine copycfg(ncfin,ncfout,action)
!
!  Copies data from one configuration to another
!
!   1/05 nsasexclude options added
!   3/08 New MD integrator added
!   8/08 pr_conscfg added
!  10/08 COSMO variables added
!  12/12 Time-dependent field added
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
  use defects
  use field
  use freeze
  use ksample
  use moldyn
  use m_pr,             only : taubcfg, tautcfg, pr_conscfg
  use potentialgrid
  use potentialpoints
  use projectdos
  use reallocate
  use scan
  use shifts
  use symmetry
!
  implicit none
!
!  Input parameters
!
!  ncfin  = configuration to copy from
!  ncfout = configuration to copy to
!  action = items to copy : 'all' => copy all configuration info
!                           'str' => structural info only
!
  integer(i4),       intent(in)       :: ncfin
  integer(i4),       intent(in)       :: ncfout
  character(len=20), intent(in)       :: action
!
!  Internal variables
!
  integer(i4)                         :: i
  integer(i4)                         :: iin
  integer(i4)                         :: iout
  integer(i4)                         :: j
!
!  Check if ncfin = ncfout - if so then nothing to do
!
  if (ncfin.eq.ncfout) return
!
!  Check whether we need to add a new configuration to accomodate ncfout
!
  if (ncfout.gt.maxcfg) then
    maxcfg = ncfout
    call changemaxcfg
  endif
!
!  Check maximum atom dimension
!
  if (nasum+nascfg(ncfin).gt.maxatot) then
    maxatot = nasum + nascfg(ncfin)
    call changemaxatot
  endif
!
!  Check whether ncfin exists
!
  if (ncfin.gt.maxcfg) then
    call outerror('Trying to copy invalid configuration',0_i4)
    call stopnow('copycfg')
  endif
!
!  Ensure that action string is lower case
!
  call stolc(action,20_i4)
!
!  Copy data between configuration array locations
!
  if (index(action,'all').eq.1.or.index(action,'str').eq.1) then
!*******************
!  Structure copy  *
!*******************
    dhklcfg(ncfout) = dhklcfg(ncfin)
    hmssg(1:16,ncfout) = hmssg(1:16,ncfin)
    iflags(ncfout) = iflags(ncfin)
    lsymset(ncfout) = lsymset(ncfin)
    lufree(ncfout) = lufree(ncfin)
    lvecin(ncfout) = lvecin(ncfin)
    ifso(ncfout) = ifso(ncfin)
    ifhr(ncfout) = ifhr(ncfin)
    iperm(ncfout) = iperm(ncfin)
    iufree(ncfout) = iufree(ncfin)
    ivso(1:3,ncfout) = ivso(1:3,ncfin)
    n1con(ncfout) = n1con(ncfin)
    n1var(ncfout) = n1var(ncfin)
    nascfg(ncfout) = nascfg(ncfin)
    ndimen(ncfout) = ndimen(ncfin)
    nregions(ncfout) = nregions(ncfin)
    nspcg(ncfout) = nspcg(ncfin)
    nsregion2(ncfout) = nsregion2(ncfin)
    nsuper(ncfout) = nsuper(ncfin)
    nzmolcfg(ncfout) = nzmolcfg(ncfin)
    nvarcfg(ncfout) = nvarcfg(ncfin)
    rufree(ncfout) = rufree(ncfin)
    rvcfg(1:3,1:3,ncfout) = rvcfg(1:3,1:3,ncfin)
    totalchargecfg(ncfout) = totalchargecfg(ncfin)
    xufree(1:3,ncfout) = xufree(1:3,ncfin)
!
    ngocfg(ncfout) = ngocfg(ncfin)
    if (ngocfg(ncfin).gt.1) then
      do i = 1,ngocfg(ncfin)
        do j = 1,3
          ropcfg(1:3,j,i,ncfout) = ropcfg(1:3,j,i,ncfin)
        enddo
        vitcfg(1:3,i,ncfout) = vitcfg(1:3,i,ncfin)
      enddo
    endif
!
    iin = 6*(ncfin - 1)
    iout = 6*(ncfout - 1)
    do i = 1,6
      lopfc(iout+i) = lopfc(iin+i)
    enddo
!
    iin = 0
    do i = 1,ncfin - 1
      iin = iin + nascfg(i)
    enddo
    iout = 0
    do i = 1,ncfout - 1
      iout = iout + nascfg(i)
    enddo
!
    do i = 1,nascfg(ncfin)
      lbsmat(iout+i) = lbsmat(iin+i)
      lfix(iout+i) = lfix(iin+i)
      lqmatom(iout+i) = lqmatom(iin+i)
      lsliceatom(iout+i) = lsliceatom(iin+i)
      ltranat(iout+i) = ltranat(iin+i)
      natcfg(iout+i) = natcfg(iin+i)
      nregionno(iout+i) = nregionno(iin+i)
      ntypcfg(iout+i) = ntypcfg(iin+i)
      cncfg(iout+i) = cncfg(iin+i)
      occucfg(iout+i) = occucfg(iin+i)
      oxcfg(iout+i) = oxcfg(iin+i)
      qlcfg(iout+i) = qlcfg(iin+i)
      radcfg(iout+i) = radcfg(iin+i) 
      xcfg(iout+i) = xcfg(iin+i)
      ycfg(iout+i) = ycfg(iin+i)
      zcfg(iout+i) = zcfg(iin+i)
    enddo
    do i = 1,3*nascfg(ncfin)
      lopfi(3*iout+i) = lopfi(3*iin+i)
    enddo
!
!  Increment total number of atoms
!
    nasum = nasum + nascfg(ncfout)
  endif
  if (index(action,'all').eq.1.or.index(action,'con').eq.1) then
!********************
!  Conditions copy  *
!********************
    ntempstp(ncfout) = ntempstp(ncfin)
    ntempstpstart(ncfout) = ntempstpstart(ncfin)
    presscfg(ncfout) = presscfg(ncfin)
    stresscfg(1:6,ncfout) = stresscfg(1:6,ncfin)
    tempcfg(ncfout) = tempcfg(ncfin)
    tempstp(ncfout) = tempstp(ncfin)
  endif
  if (index(action,'all').eq.1.or.index(action,'sol').eq.1) then
!***********************
!  Solvent/COSMO copy  *
!***********************
    cosmoepsilon(ncfout) = cosmoepsilon(ncfin)
    cosmodrsolv(ncfout) = cosmodrsolv(ncfin)
    cosmorsolv(ncfout) = cosmorsolv(ncfin)
    lcosmoeigin(ncfout) = lcosmoeigin(ncfin)
    nsasexcludemax(ncfout) = nsasexcludemax(ncfin)
    nsasexcludemin(ncfout) = nsasexcludemin(ncfin)
  endif
  if (index(action,'all').eq.1.or.index(action,'md').eq.1) then
!****************************
!  Molecular dynamics copy  *
!****************************
    lmdconstrain(ncfout) = lmdconstrain(ncfin)
    nensemble(ncfout) = nensemble(ncfin)
    nmdconstrainatom(1:2,ncfout) = nmdconstrainatom(1:2,ncfin)
    nmdconstraindist(ncfout) = nmdconstraindist(ncfin)
    nmdeq(ncfout) = nmdeq(ncfin)
    nmdprod(ncfout) = nmdprod(ncfin)
    nmdsamp(ncfout) = nmdsamp(ncfin)
    nmdvelmode(ncfout) = nmdvelmode(ncfin)
    nmdvelmodp(ncfout) = nmdvelmodp(ncfin)
    nmdwrite(ncfout) = nmdwrite(ncfin)
    pr_conscfg(ncfout) = pr_conscfg(ncfin)
    taubcfg(ncfout) = taubcfg(ncfin)
    tautcfg(ncfout) = tautcfg(ncfin)
    tmdeq(ncfout) = tmdeq(ncfin)
    tmdforcestart(ncfout) = tmdforcestart(ncfin)
    tmdforcestop(ncfout) = tmdforcestop(ncfin)
    tmdfieldstart(ncfout) = tmdfieldstart(ncfin)
    tmdfieldstop(ncfout) = tmdfieldstop(ncfin)
    tmdprod(ncfout) = tmdprod(ncfin)
    tmdsamp(ncfout) = tmdsamp(ncfin)  
    tmdscale(ncfout) = tmdscale(ncfin)  
    tmdscint(ncfout) = tmdscint(ncfin)
    tmdwrite(ncfout) = tmdwrite(ncfin)
    tstep(ncfout) = tstep(ncfin)
  endif
!
  return
  end
