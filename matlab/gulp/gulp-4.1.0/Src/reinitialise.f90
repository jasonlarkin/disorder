  subroutine reinitialise
!
!  Initialises all arrays back to their defaults
!
!   9/10 Created
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
!  Julian Gale, NRI, Curtin University, September 2010
!
  use datatypes
  use bondcharge,        only : maxbondQ
  use bondorderdata,     only : maxnboA, maxnboR, maxnboQ, maxnboQ0, maxnbopot
  use chargecoupled,     only : maxCCspec
  use configurations,    only : maxatot, maxregion, maxcfg
  use control,           only : lcosmo
  use cosmo,             only : maxnppa, maxnpts
  use current,           only : maxat, maxbond
  use defects,           only : maxdef, maxr1at
  use eam,               only : maxeamden, maxeamfnspec, maxeamspec
  use fitting,           only : maxfit
  use four,              only : maxfor
  use general,           only : maxtitle
  use library,           only : maxlib
  use m_three,           only : maxthb
  use molecule,          only : maxmol, maxconnect
  use montecarlo,        only : maxgcmcmol, maxmcswapspec
  use neb,               only : maxnebreplicatot
  use observables,       only : maxfgrad, maxfstress, maxobs
  use one,               only : maxone
  use plane,             only : maxplanepot
  use reaxFFdata,        only : maxreaxFFspec, maxreaxFFval3
  use spatial,           only : maxspcell
  use spatialbo,         only : maxspcellbo
  use species,           only : maxspec
  use six,               only : maxsix
  use two,               only : maxpot
  implicit none
!
!  Local variables
!
  integer(i4)  :: i
!
  do i = 1,maxat
    call initmaxatdefaults(i)
  enddo
  do i = 1,maxatot
    call initmaxatotdefaults(i)
  enddo
  do i = 1,maxbond
    call initmaxbonddefaults(i)
  enddo
  do i = 1,maxbondQ
    call initmaxbondqdefaults(i)
  enddo
  do i = 1,maxcfg
    call initmaxcfgdefaults(i)
  enddo
  do i = 1,maxCCspec
    call initmaxccspecdefaults(i)
  enddo
  do i = 1,maxconnect
    call initmaxconnectdefaults(i)
  enddo
  do i = 1,maxdef
    call initmaxdefdefaults(i)
  enddo
  do i = 1,maxeamden
    call initmaxeamdendefaults(i)
  enddo
  do i = 1,maxeamfnspec
    call initmaxeamfnspecdefaults(i)
  enddo
  do i = 1,maxeamspec
    call initmaxeamspecdefaults(i)
  enddo
  do i = 1,maxfgrad
    call initmaxfgraddefaults(i)
  enddo
  do i = 1,maxfit
    call initmaxfitdefaults(i)
  enddo
  do i = 1,maxfor
    call initmaxfordefaults(i)
  enddo
  do i = 1,maxfstress
    call initmaxfstressdefaults(i)
  enddo
  do i = 1,maxgcmcmol
    call initmaxgcmcmoldefaults(i)
  enddo
  do i = 1,maxlib
    call initmaxlibdefaults(i)
  enddo
  do i = 1,maxmcswapspec
    call initmaxmcswapspecdefaults(i)
  enddo
  do i = 1,maxmol
    call initmaxmoldefaults(i)
  enddo
  do i = 1,maxnboA
    call initmaxnboadefaults(i)
  enddo
  do i = 1,maxnbopot
    call initmaxnbopotdefaults(i)
  enddo
  do i = 1,maxnboQ0
    call initmaxnboq0defaults(i)
  enddo
  do i = 1,maxnboQ
    call initmaxnboqdefaults(i)
  enddo
  do i = 1,maxnboR
    call initmaxnbordefaults(i)
  enddo
  do i = 1,maxnebreplicatot
    call initmaxnebreplicatotdefaults(i)
  enddo
  if (lcosmo) then
    do i = 1,maxnppa
      call initmaxnppadefaults(i)
    enddo
  endif
  do i = 1,maxnpts
    call initmaxnptsdefaults(i)
  enddo
  do i = 1,maxobs
    call initmaxobsdefaults(i)
  enddo
  do i = 1,maxone
    call init1bodydefaults(i)
  enddo
  do i = 1,maxplanepot
    call initmaxplanepotdefaults(i)
  enddo
  do i = 1,maxpot
    call init2bodydefaults(i)
  enddo
  do i = 1,maxr1at
    call initmaxr1atdefaults(i)
  enddo
  do i = 1,maxreaxffspec
    call initmaxreaxffspecdefaults(i)
  enddo
  do i = 1,maxreaxffval3
    call initmaxreaxffval3defaults(i)
  enddo
  do i = 1,maxregion
    call initmaxregiondefaults(i)
  enddo
  do i = 1,maxsix
    call initmaxsixdefaults(i)
  enddo
  do i = 1,maxspcellbo
    call initmaxspcellbodefaults(i)
  enddo
  do i = 1,maxspcell
    call initmaxspcelldefaults(i)
  enddo
  do i = 1,maxspec
    call initmaxspecdefaults(i)
  enddo
  do i = 1,maxthb
    call initmaxthbdefaults(i)
  enddo
  do i = 1,maxtitle
    call initmaxtitledefaults(i)
  enddo
!
  return
  end
