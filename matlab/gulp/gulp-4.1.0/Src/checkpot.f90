  subroutine checkpot
!
!  Performs basic checking and sorting of potentials.
!
!   7/03 Created from output
!  10/04 nat2REBOspecies used to check atomic numbers for Brenner potential
!  10/04 Use of splines for Brenner now forced if non-C/H present
!   4/05 Mods for cosh-spring potential added
!  11/06 Call to ftow replaced with itow
!  11/06 Calls to outwarning used to replace local warning messages
!   6/07 Check on compatibility of atom types with Brenner removed
!   6/09 Module name changed from three to m_three
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use brennerdata
  use configurations
  use constants
  use control
  use current
  use eam
  use element, only : maxele
  use four
  use general, only : nwarn
  use iochannels
  use parallel
  use shell
  use shifts
  use species
  use m_three
  use two
  implicit none
!
!  Local variables
!
  character(len=3)                             :: numstring
  character(len=4)                             :: potnum
  character(len=5)                             :: lab1
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: ni
  integer(i4)                                  :: np
  integer(i4)                                  :: npt1
  integer(i4)                                  :: npt2
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntmp
  integer(i4)                                  :: ntype1
  integer(i4)                                  :: ntype2
  integer(i4)                                  :: ntypi
  logical                                      :: lwarn
  logical                                      :: lfound
  logical,                                save :: lfrst = .true.
!*********************
!  Check potentials  *
!*********************
  if (npote.gt.0.or.lbrenner.or.lreaxFF.or.lEDIP) then
    if (lfrst) then
!
!  Check for incorrect assignment of potentials to cores instead of shells
!
      do i = 1,npote
        nat1 = nspec1(i)
        nat2 = nspec2(i)
        np = nptype(i)
        if (np.ne.5.and.np.ne.8.and.np.ne.14.and.np.ne.17.and.np.ne.31.and.np.ne.33) then
          lwarn = .false.
          if (nat1.le.maxele) then
            ntype1 = nptyp1(i)
            do j = 1,nspec
              if (natspec(j).eq.nat1+maxele.and.(ntypspec(j).eq.ntype1.or.ntype1.eq.0)) lwarn = .true.
            enddo
            if (lwarn) then
              nwarn = nwarn + 1
              if (ioproc) then
                write(numstring,'(i3)') i
                call outwarning('potential '//numstring//' is acting on the core of an ion with a shell',0_i4)
              endif
            endif
          endif
          if (nat2.le.maxele.and..not.lwarn) then
            ntype2 = nptyp2(i)
            do j = 1,nspec
              if (natspec(j).eq.nat2+maxele.and.(ntypspec(j).eq.ntype2.or.ntype2.eq.0)) lwarn=.true.
            enddo
            if (lwarn) then
              nwarn = nwarn + 1
              if (ioproc) then
                write(numstring,'(i3)') i
                call outwarning('potential '//numstring//' is acting on the core of an ion with a shell',0_i4)
              endif
            endif
          endif
        endif
      enddo
!
!  Check that minimum cutoff is not greater than maximum cutoff
!
      do i = 1,npote
        if (rpot2(i).gt.rpot(i)) then
          call itow(potnum,i,4)
          call outerror('Minimum cutoff is greater than maximum for potl = '//potnum,0_i4)
          call stopnow('checkpot')
        endif
      enddo
!
!  Check that all core-shell pairs have spring constant
!
      do i = 1,nspec
        if (natspec(i).gt.maxele) then
          ni = natspec(i)
          nti = ntypspec(i)
          lfound = .false.
          do j = 1,npote
            np = nptype(j)
            if (np.eq.5.or.np.eq.8.or.np.eq.33) then
              nat1 = nspec1(j)
              nat2 = nspec2(j)
              npt1 = nptyp1(j)
              npt2 = nptyp2(j)
              if (abs(nat1-nat2).eq.maxele) then
                if (nat1.gt.nat2) then
                  ntmp = nat1
                  nat1 = nat2
                  nat2 = ntmp
                  ntmp = npt1
                  npt1 = npt2
                  npt2 = ntmp
                endif
                if (nat2.eq.ni.and.(npt2.eq.0.or.npt2.eq.nti)) lfound = .true.
              endif
            endif
          enddo
          if (.not.lfound) then
            nwarn = nwarn + 1
            if (ioproc) then
              call label(ni,nti,lab1)
              call outwarning('core-shell pair with no spring constant for species '//lab1,0_i4)
            endif
          endif
        endif
      enddo
!
!  Check that all breathing species have radial potential
!
      do i = 1,nasum
        if (lbsmat(i)) then
          lfound = .false.
          nati = natcfg(i)
          ntypi = ntypcfg(i)
          do j = 1,npote
            np = nptype(j)
            if (np.eq.14.or.np.eq.17.or.np.ne.31) then
              nat1 = nspec1(j)
              npt1 = nptyp1(j)
              if (nat1.eq.nati.and.(npt1.eq.ntypi.or.npt1.eq.0)) lfound = .true.
            endif
          enddo
          if (.not.lfound) then
            nwarn = nwarn + 1
            if (ioproc) then
              call label(nati,ntypi,lab1)
              write(ioout,'(/)')
              if (natcfg(i).gt.maxele) then
                call outwarning('breathing species with no BSM potential for '//lab1//' shel',0_i4)
              else
                call outwarning('breathing species with no BSM potential for '//lab1//' core',0_i4)
              endif
            endif
          endif
        endif
      enddo
!
!  Check that all combination rule potentials have species sigma
!  and epsilon values in place.
!
      do i = 1,npote
        if (lcombine(i)) then
          nat1 = nspec1(i)
          nat2 = nspec2(i)
          npt1 = nptyp1(i)
          npt2 = nptyp2(i)
          if (nptype(i).eq.2) then
            lfound = .false.
            j = 0
            do while (.not.lfound.and.j.lt.natab)
              j = j + 1
              if (nat1.eq.nattab(j).and.(npt1.eq.ntypab(j).or.ntypab(j).eq.0)) lfound = .true.
            enddo
            if (.not.lfound) then
              if (ioproc) then
                call label(nat1,npt1,lab1)
                if (nat1.gt.maxele) then
                  call outerror('no atom A/B values found for '//lab1//' shel',0_i4)
                else
                  call outerror('no atom A/B values found for '//lab1//' core',0_i4)
                endif
              endif
              call stopnow('checkpot')
            endif
            lfound = .false.
            j = 0
            do while (.not.lfound.and.j.lt.natab)
              j = j + 1
              if (nat2.eq.nattab(j).and.(npt2.eq.ntypab(j).or.ntypab(j).eq.0)) lfound = .true.
            enddo
            if (.not.lfound) then
              if (ioproc) then
                call label(nat2,npt2,lab1)
                if (nat2.gt.maxele) then
                  call outerror('no atom A/B values found for '//lab1//' shel',0_i4)
                else
                  call outerror('no atom A/B values found for '//lab1//' core',0_i4)
                endif
              endif
              call stopnow('checkpot')
            endif
          else
            lfound = .false.
            j = 0
            do while (.not.lfound.and.j.lt.nseps) 
              j = j + 1
              if (nat1.eq.natse(j).and.(npt1.eq.ntypse(j).or.ntypse(j).eq.0)) lfound = .true.
            enddo
            if (.not.lfound) then
              if (ioproc) then
                call label(nat1,npt1,lab1)
                if (nat1.gt.maxele) then
                  call outerror('no epsilon/sigmas found for '//lab1//' shel',0_i4)
                else
                  call outerror('no epsilon/sigmas found for '//lab1//' core',0_i4)
                endif
              endif
              call stopnow('checkpot')
            endif
            lfound = .false.
            j = 0
            do while (.not.lfound.and.j.lt.nseps) 
              j = j + 1
              if (nat2.eq.natse(j).and.(npt2.eq.ntypse(j).or.ntypse(j).eq.0)) lfound = .true.
            enddo
            if (.not.lfound) then
              if (ioproc) then
                call label(nat2,npt2,lab1)
                if (nat2.gt.maxele) then
                  call outerror('no epsilon/sigmas found for '//lab1//' shel',0_i4)
                else
                  call outerror('no epsilon/sigmas found for '//lab1//' core',0_i4)
                endif
              endif
              call stopnow('checkpot')
            endif
          endif
        endif
      enddo
      lfrst = .false.
    endif
!
!  Check that atom types are compatable with Brenner potential if used and if so setup Brenner parameters
!
    if (lbrenner) then
      if (nbrennertype.eq.3) then
        do i = 1,nasym
!
!  If atoms other than C & H are present then force use of spline since pretabulated data is not present
!
          if (nat2REBOspecies(iatn(i)).gt.2) then
            if (.not.lbrennersplinef.or..not.lbrennersplineh) then
              nwarn = nwarn + 1
              call outwarning('splines must be used for REBO with non-C/H atoms',0_i4)
            endif
            lbrennersplinef = .true.
            lbrennersplineh = .true.
          endif
        enddo
        call setbrenner3
      elseif (nbrennertype.eq.1) then
        do i = 1,nasym
          lbrennersplinef = .true.
          lbrennersplineh = .true.
        enddo
        call setbrenner1
      endif
    endif
!
!  Set up ReaxFF
!
    if (lreaxFF) call setreaxFF
!
!  Set up EDIP
!
    if (lEDIP) call setEDIP
  endif
!
  return
  end
