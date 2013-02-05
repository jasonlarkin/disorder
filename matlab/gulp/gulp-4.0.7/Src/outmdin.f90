  subroutine outmdin
!
!  Output MD info prior to start of run.
!
!   2/97 Modifications added (JRH) for GC and shell model MD
!   3/97 NVT ensemble added
!  10/02 Output of constraint added
!   1/05 Output of vector reset frequency added
!  10/06 Format of output statements changed
!  12/06 Annealing information output
!   7/07 Modified to handle metadynamics
!   3/08 Modified for new MD integrator
!   8/08 Distance added as a metadynamics bias
!   9/08 Coordinates added as metadynamics variables
!   9/11 Metadynamics internal code replaced with Plumed
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
!  Julian Gale, NRI, Curtin University, December 2012.
!
  use control
  use current
  use distances, only : lStoreVectors, ndistancereset
  use general
  use iochannels
  use moldyn
  use m_pr,      only : taubcfg, tautcfg
  use optimisation
  implicit none
!
!  Local variables
!
!
!  Output banner
!
  write(ioout,'(/,''********************************************************************************'')')
  write(ioout,'(''*  Molecular Dynamics                                                          *'')')
  write(ioout,'(''********************************************************************************'',/)')
  if (nensemble(ncf).eq.1) then
    write(ioout,'(''  Microcanonical ensemble (NVE) to be used'',/)')
  elseif (nensemble(ncf).eq.2) then
    write(ioout,'(''  Canonical ensemble (NVT) to be used'',/)')
    write(ioout,'(''  Friction for temperature bath = '',f18.6,/)') qtemp(ncf)
  elseif (nensemble(ncf).eq.3) then
    write(ioout,'(''  Isothermal/baric ensemble (NPT) to be used'',/)')
    if (nmdintegrator.eq.4) then
      write(ioout,'(''  Relaxation time for thermostat= '',f18.6,'' ps'')') tautcfg(ncf)
      write(ioout,'(''  Relaxation time for barostat  = '',f18.6,'' ps'',/)') taubcfg(ncf)
    else
      write(ioout,'(''  Friction for temperature bath = '',f18.6)') qtemp(ncf)
      write(ioout,'(''  Friction for pressure    bath = '',f18.6,/)') qpres(ncf)
    endif
  elseif (nensemble(ncf).eq.4) then
    write(ioout,'(''  Isothermal/isoenthalpic ensemble (NPH) to be used'',/)')
    if (nmdintegrator.eq.4) then
      write(ioout,'(''  Relaxation time for barostat  = '',f18.6,'' ps'',/)') taubcfg(ncf)
    else
      write(ioout,'(''  Friction for pressure    bath = '',f18.6,/)') qpres(ncf)
    endif
  endif
  write(ioout,'(''  No. of mobile ions        = '',i18,/)') nmoving
  write(ioout,'(''  No. of degrees of freedom = '',i18,/)') ndof
  if ((nmdvelmode(ncf)+nmdvelmodp(ncf)).gt.-2) then
    write(ioout,'(''  Limit momentum correction : '')')
    if (nmdvelmode(ncf).gt.-1) then
      write(ioout,'(''    for equilibration       = '',i18)') nmdvelmode(ncf)
    endif
    if (nmdvelmodp(ncf).gt.-1) then
      write(ioout,'(''    for production          = '',i18)') nmdvelmodp(ncf)
    endif
    write(ioout,'(/)')
  endif
  write(ioout,'(''  Time step                 = '',f18.6,'' ps'')') tstep(ncf)
  write(ioout,'(''  Equilibration time        = '',f18.6,'' ps'')') tmdeq(ncf)
  write(ioout,'(''  Production time           = '',f18.6,'' ps'')') tmdprod(ncf)
  write(ioout,'(''  Scaling time              = '',f18.6,'' ps'')') tmdscale(ncf)
  write(ioout,'(''  Scaling frequency         = '',f18.6,'' ps'')') tmdscint(ncf)
  if (nmdsamp(ncf).gt.0) then
    write(ioout,'(''  Sampling frequency        = '',f18.6,'' ps'')') nmdsamp(ncf)*tstep(ncf)
  else
    write(ioout,'(''  Sampling frequency        = '',f18.6,'' ps'')') tmdsamp(ncf)
  endif
  if (nmdwrite(ncf).gt.0) then
    write(ioout,'(''  Write frequency           = '',f18.6,'' ps'')') nmdwrite(ncf)*tstep(ncf)
  else
    write(ioout,'(''  Write frequency           = '',f18.6,'' ps'')') tmdwrite(ncf)
  endif
  if (ntemperaturestep.gt.0) then
    write(ioout,'(''  Start temperature change  = '',f18.6,'' ps'')') ntemperaturestepstart*tstep(ncf)
    write(ioout,'(''  End   temperature change  = '',f18.6,'' ps'')') (ntemperaturestepstart+ntemperaturestep)*tstep(ncf)
    write(ioout,'(''  Size  temperature change  = '',f18.6,'' K/ps'')') temperaturestep/tstep(ncf)
  endif
  write(ioout,'(''  TD-Force start time       = '',f18.6,'' ps'')') tmdforcestart(ncf)
  if (tmdforcestop(ncf).gt.0.0_dp) then
    write(ioout,'(''  TD-Force stop  time       = '',f18.6,'' ps'')') tmdforcestop(ncf)
  endif
  write(ioout,'(''  TD-Field start time       = '',f18.6,'' ps'')') tmdfieldstart(ncf)
  if (tmdfieldstop(ncf).gt.0.0_dp) then
    write(ioout,'(''  TD-Field stop  time       = '',f18.6,'' ps'')') tmdfieldstop(ncf)
  endif
  write(ioout,'(/)') 
  if (timesofar.gt.1.0d-6) then
    write(ioout,'(''  Restarting after '',f18.6,'' ps '',/)') timesofar
  endif
  if (lmdconstrain(ncf)) then
    write(ioout,'(''  Constraint applied between atoms '',i6,'' and '',i6,'' : Distance = '',f7.4,'' Angs'')') &
      nmdconstrainatom(1,ncf),nmdconstrainatom(2,ncf),nmdconstraindist(ncf)
  endif
  if (lStoreVectors) then
    write(ioout,'(''  Vector reset frequency    = '',i18)') ndistancereset(ncf)
  endif
!
  return
  end
