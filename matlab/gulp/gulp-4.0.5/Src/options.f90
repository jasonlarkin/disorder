  subroutine gulp_options
!
!  Main subroutine from which options are called
!
!   4/01 Created from main routine gulp 
!  11/02 Scan point number passed to optim
!   7/05 Call to eem added after optimisation to output final charges
!  11/06 Modifications for NEB added
!   1/07 Gasteiger charges added
!   5/07 Bond increment charges added
!   7/07 Metadynamics added
!   8/08 Metadynamics removed as option and introduced as a bias
!        to a standard MD run.
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   6/09 Renamed to gulp_options to avoid conflict in chemshell
!   4/10 Flag added to defect call to signal whether the defect
!        calculation is OK. 
!   4/10 QTPie charge transfer option added
!   9/11 lgrad1 argument added to eem call
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, September 2011
!
  use configurations
  use control
  use current
  use defects
  use element,         only : lgasteiger
  use general
  use genetic
  use gulp_cml,        only : lcml, gulp_cml_StartModule, gulp_cml_EndModule, gulp_cml_structure, &
                              gulp_cml_startstep, gulp_cml_endstep
  use iochannels
  use mdlogic
  use optimisation
  use parallel
  use potentialgrid
  use scan

  implicit none
!
!  Local variables
!
  character(len=80)                         :: confignum
  integer(i4)                               :: i
  integer(i4)                               :: ic
  integer(i4)                               :: ii
  integer(i4)                               :: mc
  integer(i4)                               :: mcfg
  integer(i4)                               :: n
  integer(i4)                               :: ntat
  integer(i4)                               :: ntr
  logical                                   :: ldefectOK
  logical                                   :: lselect
  logical                                   :: lnofp
  real(dp)                                  :: rn
  real(dp)                                  :: xtr
  real(dp)                                  :: ytr
  real(dp)                                  :: ztr
!***********************
!  Configuration loop  *
!***********************
  do ic = 1,ncfg
    ncf = ic
!
!  Put nreg1 to zero so that dumpdur knows not to dump
!  explicit region 1 until defect calc has started.
!
    nreg1 = 0
    if (ioproc) then
      write(ioout,'(/,''********************************************************************************'')')
      if (names(ncf)(1:1).ne.' ') then
        write(ioout,'(''*  Output for configuration '',i3,'' : '',a45,''*'')')ncf,names(ncf)(1:45)
      else
        write(ioout,'(''*  Output for configuration '',i3,48x,''*'')')ncf
      endif
      write(ioout,'(''********************************************************************************'',/)')
    endif
!
! New CML
! Start a module for this configuration
!
    if (lcml) then
      write(confignum,'(i6)') ncf
      Call gulp_cml_StartModule(title='Gulp configuration')
    endif

!*************************
!  Structure prediction  *
!*************************
    mcfg = 1
    if (lpredict.and.ndimen(ic).eq.3) then
      call setup(.true.)
      call predict(mcfg)
    endif
!**************************************************************
!  Loop over the number of configurations found from predict  *
!**************************************************************
    do mc = 1,mcfg
      if (lpredict.and.ndimen(ic).eq.3) then
        ncf = ic
        call setup(.false.)
        call gabcfg(mc,mcfg)
      endif
      if (ntran(ncf).gt.0) then
!*********
!  Scan  *
!*********
        call setup(.true.)
        ntr = ntran(ncf)
        lnofp = (index(keyword,'nofi').ne.0)
!
!  Set the vector steps
!
        rn = 1.0_dp/dble(ntr)
        xtr = xtran(ncf)*rn
        ytr = ytran(ncf)*rn
        ztr = ztran(ncf)*rn
!
!  Count number of atoms being translated
!
        ntat = 0
        do i = 1,nasym
          if (ltranat(i+nsft)) then
            ntat = ntat + 1
          endif
        enddo
!
!  Check that there some atoms to be translated
!
        if (ntat.eq.0) then
          call outerror('no atoms specified for translation',0_i4)
          call stopnow('options')
        endif
        if (ioproc) then
          write(ioout,'(/,''  Repeat calculation for '',i3,'' points'')')ntr+1
          write(ioout,'(/,''  Translation vector :'',/)')
          write(ioout,'(''  x = '',f12.6,''  y = '',f12.6,''  z = '',f12.6,/)')xtran(ncf),ytran(ncf),ztran(ncf)
        endif
!*********************
!  Loop over points  *
!*********************
        do n = 0,ntr
!
! New CML
! Start a step for the point of the scan
! and add an Initialisation module
!
          if (lcml) then
             Call gulp_cml_startstep(cycle=n, type="gulp:energyscan")
             Call gulp_cml_StartModule(title='Initialisation', dictRef='eminerals:init')
             Call gulp_cml_structure(ncf)
             Call gulp_cml_endModule
          endif
          if (.not.lnofp.or.n.ne.0) then
            if (ioproc) then
              write(ioout,'(/,''################################################################################'')')
              write(ioout,'(''#  Output for point '',i5,54x,''#'')')n
              write(ioout,'(''################################################################################'',/)')
            endif
            lselect = .false.
            if (lrelax.and..not.lopt) then
              if (ioproc) then
                write(ioout,'(/,''  Properties for fitted structure :'',/)')
              endif
            endif
            if (lqbond) then
              lselect = .true.
              call setup(.true.)
              call bondq(.true.)
            endif
            if (lgasteiger) then
              lselect = .true.
              call setup(.true.)
              call gasteiger(.true.)
            endif
            if (leem) then
              lselect = .true.
              call setup(.true.)
              if (lqtpie) then
                call qtpie(.true.)
              else
                call eem(.true.,.false.,.false.)
              endif
            endif
            if (.not.lnoenergy) then
              lselect = .true.
              call setup(.true.)
              call optim(.true.,.true.,n)
              if (leem) then
                call setup(.true.)
                if (lqtpie) then
                  call qtpie(.true.)
                else
                  call eem(.true.,.false.,.false.)
                endif
              endif
            endif
            if (lpot.or.lefg) then
              lselect = .true.
              call setup(.true.)
              call latpot
            endif
            if (lneb) then 
              lselect = .true.
              call setup(.true.)
              if (index(keyword,'sync').ne.0) then
                call runsync
              else
                call runneb
              endif
            endif
            if (ldefect.and.ndimen(ic).eq.3) then
!$$$$$$$$$$$$$$$$$$$$$$$
!  Defect calculation  $
!$$$$$$$$$$$$$$$$$$$$$$$
              lselect = .true.
              call setup(.true.)
              if (.not.lnoenergy) call setdr2mat
!
!  Setup defect regions
!
              call defect(ldefectOK)
              if (ldefectOK) then
!
!  Function call
!
                if (.not.lnoenergy) then
                  call defopt(.true.)
                endif
!
!  Defect site potentials
!
                if (lpot) call defpot
!
!  Move region 2a ions into region with final displacements
!
                if (reg2a1(ncf).gt.0.0_dp) call move2a1
              endif
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  End of defect calculation  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            endif
            if (.not.lselect) then
              if (ioproc) then
                write(ioout,'(''  **** No options selected for configuration '',i3,''  ****'')')ncf
              endif
            endif
          endif
!
!  Shift coordinates for next point - if vector has
!  no component in a particular cartesian direction
!  then preserve value from last optimisation step.
!
          if (n.ne.ntr) then
            ii = 0
            do i = 1,nasym
              if (ltranat(i+nsft)) then
                ii = ii + 1
                xcfg(i+nsft) = xcfg(i+nsft) + xtr
                ycfg(i+nsft) = ycfg(i+nsft) + ytr
                zcfg(i+nsft) = zcfg(i+nsft) + ztr
              endif
            enddo
          endif
! 
! New CML
! Having done that step close it
!
          if (lcml) call gulp_cml_endstep
        enddo
      else
!***********************************
!  Single pass over configuration  *
!***********************************
! 
! New CML 
! For this single pass write an Initialisation module
!
        if (lcml) then
          write(confignum,'(i6)') ncf
          Call gulp_cml_StartModule(title='Initialisation', dictRef='eminerals:init')
          Call gulp_cml_structure(ncf)
          Call gulp_cml_endModule
        endif
        lselect = .false.
        if (lrelax.and..not.lopt) then
          if (ioproc) then
            write(ioout,'(/,''  Properties for fitted structure :'',/)')
          endif
        endif
        if (lqbond) then
          lselect = .true.
          call setup(.true.)
          call bondq(.true.)
        endif
        if (lgasteiger) then
          lselect = .true.
          call setup(.true.)
          call gasteiger(.true.)
        endif
        if (leem) then
          lselect = .true.
          call setup(.true.)
          if (lqtpie) then
            call qtpie(.true.)
          else
            call eem(.true.,.false.,.false.)
          endif
        endif
        if (.not.lnoenergy) then
          lselect = .true.
          call setup(.true.)
          call optim(.true.,.false.,0)
          if (leem) then
            call setup(.true.)
            if (lqtpie) then
              call qtpie(.true.)
            else
              call eem(.true.,.false.,.false.)
            endif
          endif
        endif
        if (lpot.or.lefg) then
          lselect = .true.
          call setup(.true.)
          call latpot
        endif
        if (nxpg(ic).ne.0) then
          lselect = .true.
          call setup(.true.)
          call potgrid
        endif
        if (lneb) then
          lselect = .true.
          call setup(.true.)
          if (index(keyword,'sync').ne.0) then
            call runsync
          else
            call runneb
          endif
        endif
        if (lmc) then
          lselect = .true.
          call runmc
        endif
        if (lmd) then
          lselect = .true.
          call runmd
        endif
        if (ldefect.and.ndimen(ic).eq.3) then
!$$$$$$$$$$$$$$$$$$$$$$$
!  Defect calculation  $
!$$$$$$$$$$$$$$$$$$$$$$$
          lselect = .true.
          call setup(.true.)
          if (.not.lnoenergy) call setdr2mat
!
!  Setup defect regions
!
          call defect(ldefectOK) 
          if (ldefectOK) then
!
!  Function call
!
            if (.not.lnoenergy) then
              call defopt(.true.)
            endif
!
!  Defect site potentials
!
            if (lpot) call defpot
!
!  Move region 2a ions into region with final displacements
!
            if (reg2a1(ncf).gt.0.0_dp) call move2a1
          endif
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  End of defect calculation  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        endif
        if (.not.lselect) then
          if (ioproc) then
            write(ioout,'(''  **** No options selected for configuration '',i3,'' ****'')') ncf
          endif
        endif
      endif
    enddo
    if (lpredict) call outfile
!
! New CML
! Close the module for the configuration.
! (Finalisation etc written by the called 
! subs I'm afraid - I can not find a way a
! around this).
    if (lcml) then
      Call gulp_cml_EndModule
    endif
  enddo
!
  return
  end
