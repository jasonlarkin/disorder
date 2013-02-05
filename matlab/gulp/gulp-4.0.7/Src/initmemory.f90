  subroutine initmemory
!
!  Initialises all module arrays to their default minimum size
!
!   5/06 Call to changemaxeamden added
!   7/06 Sixbody memory initiated
!   9/06 Call to changemaxiltor added
!  11/06 NEB modifications added
!   5/07 Call to changemaxbondq added
!   7/07 Initialisation of metadynamics arrays added
!   7/07 Initialisation of reaxFF arrays added
!   7/07 Initialisation of plane potential arrays added
!   4/08 Call to changemaxreaxffval3 added
!   1/09 swap move added to Monte Carlo
!   2/09 changemaxdis now initialised once here
!   2/09 Argument removed from changemaxdis call
!   6/09 EVB arrays added
!   9/10 Neutron scattering modifications added
!   9/10 Call to changemaxnpwtloc/changemaxnppa added
!   9/10 Call to changemaxedipspec added
!   8/11 Call to changemaxreaxFFfixQspec added
!   9/11 Metadynamics internal code replaced with Plumed
!   1/12 Call to changemaxobsmode added
!  12/12 Call to changemaxtdfield added
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
  use control,   only : lcosmo
  use parallel
  implicit none
!
  call changemaxallnearseg
  call changemaxat
  call changemaxatot
  call changemaxbond
  call changemaxbondq
  call changemaxbondvec
  call changemaxccspec
  call changemaxcfg
  call changemaxcon
  call changemaxconnect
  call changemaxcontot
  call changemaxdef
  call changemaxdis
  call changemaxedipspec
  call changemaxndistancetotal
  call changemaxnebreplicatot
  call changemaxeamden
  call changemaxeamspec
  call changemaxeamfnspec
  call changemaxextrapol
  call changemaxfgrad
  call changemaxfstress
  call changemaxfit
  call changemaxfor
  call changemaxgcmcmol
  call changemaxiltor
  call changemaxfkpt
  call changemaxkpt
  call changemaxskpt
  call changemaxkvec
  call changemaxlib
  call changemaxlist3
  call changemaxlist4
  call changemaxmany
  call changemaxmcswapspec
  call changemaxmol
  call changemaxnbopot
  call changemaxnboa
  call changemaxnbor
  call changemaxnboq
  call changemaxnboq0
  call changemaxnobo
  call changemaxnpts
  call changemaxnpts2
  call changemaxnptsh
  call changemaxnptstot
  call changemaxnqstepfit
  call changemaxnwstepfit
  call changemaxnset
  call changemaxobs
  call changemaxobsmode
  call changemaxone
  call changemaxpot
  call changemaxpotsites
  call changemaxppt
  call changemaxproj
  call changemaxproji
  call changemaxpts
  call changemaxr1at
  call changemaxr2at
  call changemaxregion
  call changemaxsasparticles
  call changemaxsasparticlespart
  call changemaxsix
  call changemaxspec
  call changemaxtdfield
  call changemaxthb
  call changemaxtitle
  call changemaxtrialatom
  call changemaxuffspec
  call changemaxvacint
  call changemaxvar
  call changemaxword
  call changemaxqatoms
  call changemaxreaxFFspec
  call changemaxreaxFFfixQspec
  call changemaxreaxFFval3
  call changemaxplanepot
  call changemaxreaxffspec
!
!  Neutron scattering
!
  call changemaxhold
  call changemaxqvector
!
!  COSMO
!
  if (lcosmo) then
    call changemaxnppa
    call changemaxnpwtloc
  endif
!
  return
  end
