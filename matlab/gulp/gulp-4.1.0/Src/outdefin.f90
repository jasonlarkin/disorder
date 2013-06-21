  subroutine outdefin(nldef,ndefst,rdcmax)
!
!  Output defect information as input
!
!   4/04 Dimensions of itmp increased to allow for breathing shells
!   1/13 Format of output modified with extra blank line
!   3/13 ndptr replaced with ndintptr
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use control
  use current
  use defects
  use element, only : maxele
  use general, only : nwarn
  use iochannels
  use region2a
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: ndefst
  integer(i4)                                  :: nldef
  real(dp)                                     :: rdcmax
!
!  Local variables
!
  character(len=1)                             :: crd(3)
  character(len=2)                             :: cstype
  character(len=6)                             :: dchar
  character(len=5)                             :: lab
  character(len=4)                             :: lab2
  character(len=1)                             :: ocha(3)
  character(len=5)                             :: symb
  character(len=5)                             :: symb2
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: inat
  integer(i4)                                  :: ind
  integer(i4)                                  :: iptr
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4)                                  :: ityp
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: ncrf
  integer(i4)                                  :: ncrv
  integer(i4)                                  :: ncvi
  integer(i4)                                  :: ndt
  integer(i4)                                  :: nex
  integer(i4)                                  :: nfv
  integer(i4)                                  :: nfv1
  integer(i4)                                  :: nimp
  integer(i4)                                  :: ninst
  integer(i4)                                  :: nr2
  integer(i4)                                  :: nvac
  integer(i4)                                  :: nvv
  integer(i4)                                  :: nvv1
  integer(i4)                                  :: status
  logical                                      :: lbre
!
  data crd/'x','y','z'/
!
!  Number of defects
!
  write(ioout,'(''  Total number of defects = '',i6,/)') nldef
  write(ioout,'(''  Total charge on defect  = '',f6.2,/)') qdef
!
!  Defect centre
!
  if (ndcentyp(ncf).eq.1) then
    iptr = nint(xdcent(ncf))
    write(ioout,'(''  Defect centre is at atom number '',i4,/)') iptr
  elseif (ndcentyp(ncf).eq.2) then
    iptr = nint(xdcent(ncf))
    inat = nat(nrel2(iptr))
    itype = nftype(nrel2(iptr))
    call label(inat,itype,symb)
    write(ioout,'(''  Defect centre is at atom '',a5,/)') symb
  elseif (ndcentyp(ncf).eq.3) then
    write(ioout,'(''  Defect centre is at '',3(f8.4,1x),'' Frac'',/)') xdcent(ncf),ydcent(ncf),zdcent(ncf)
  elseif (ndcentyp(ncf).eq.4) then
    write(ioout,'(''  Defect centre is at '',3(f8.4,1x),'' Angs'',/)') xdcent(ncf),ydcent(ncf),zdcent(ncf)
  elseif (ndcentyp(ncf).eq.5) then
    write(ioout,'(''  Defect centre is at the centroid of molecule no. = '',i3,/)') nint(xdcent(ncf))
  endif
!
!  Region sizes
!
  write(ioout,'(''  Region 1 radius = '',f8.4,6x,''Number of ions = '',i6,/)') reg1(ncf),nreg1
  if (ndasym.lt.nreg1.and.ldsym) then
    write(ioout,'(''  Number of symmetry inequivalent region 1 ions  ='',i7,/)') ndasym
  endif
  if (reg2(ncf).le.reg1(ncf)) then
    nr2 = 0
  else
    nr2 = nreg2
  endif
  write(ioout,'(''  Region 2 radius = '',f8.4,6x,''Number of ions = '',i6,/)') reg2(ncf),nr2
  if (ndasym2a.lt.nreg2.and.ldsym) then
    write(ioout,'(''  Number of symmetry inequivalent region 2a ions = '',i6,/)') ndasym2a
  endif
  if (mode2a.eq.1) then
    write(ioout,'(''  Region 2a mode = 1 : screened electrostatics due to region 1 '',/)')
  elseif (mode2a.eq.2) then
    write(ioout,'(''  Region 2a mode = 2 : screened electrostatics due to region 1 '')')
    write(ioout,'(''                       neglect correction to region 1 forces from 2a'',/)')
  elseif (mode2a.eq.3) then
    write(ioout,'(''  Region 2a mode = 3 : screened electrostatics due to region 1 '')')
    write(ioout,'(''                       region 2a displacements based on defects only'',/)')
  elseif (mode2a.eq.4) then
    write(ioout,'(''  Region 2a mode = 4 : screened electrostatics due to region 1 '')')
    write(ioout,'(''                       neglect correction to region 1 forces from 2a'')')
    write(ioout,'(''                       region 2a displacements based on defects only'',/)')
  elseif (mode2a.eq.5) then
    write(ioout,'(''  Region 2a mode = 5 : screened electrostatics due to defects only '')')
    write(ioout,'(''                       neglect correction to region 1 forces from 2a'',/)')
  endif
  if (index(keyword,'exac').eq.0) then
    write(ioout,'(''  Region 2a ions will only interact with defects in region 1'',/)')
  else
    write(ioout,'(''  Region 2a ions will interact with all region 1 ions'',/)')
  endif
  if (reg1last(ncf).gt.0.0d0) then
    write(ioout,'(''  Region 1 radius from previous run = '',f8.4,/)')reg1last(ncf)
!
!  Zero region 1 last to stop output to restart file
!
    reg1last(ncf) = 0.0_dp
  endif
  if (mode2a.ge.3) then
    if ((reg2(ncf)-rdcmax).lt.(rpmax-1.0d-8).and.abs(qdef).gt.0.0d0) then
      nwarn = nwarn + 1
      write(ioout,'(''  **** Warning - radius of region 1 + 2a is too small for region 2b energy ****'')')
      write(ioout,'(''  **** to be valid. Ideally r2 should be greater than '',f6.2,'' Angstroms     ****'',/)')  &
        rpmax + rdcmax
    endif
  else
    if (reg2(ncf).lt.rpmax.and.abs(qdef).gt.0.0d0) then
      nwarn = nwarn + 1
      write(ioout,'(''  **** Warning - radius of region 1 + 2a is too small for region 2b energy ****'')')
      write(ioout,'(''  **** to be valid. Ideally r2-r1 should be greater than '',f6.2,'' Angstroms  ****'',/)') &
        rpmax
    endif
  endif
!
!  Move region 2a ions to region 1 info and checks
!
  if (reg2a1(ncf).gt.0.0d0) then
    if (reg2a1(ncf).lt.reg1(ncf)) then
      call outerror('Radius for moving region 2a ions to region 1 is too small',0_i4)
      call stopnow('outdefin')
    endif
    if (reg2a1(ncf).ge.reg2(ncf)) then
      write(ioout,'(''  At end move all region 2a ions into region 1'',/)')
    else
      write(ioout,'(''  Radius for region 2a to 1 move at end = '',f8.4,/)')reg2a1(ncf)
    endif
  endif
!
!  Explicit region 1 specified in input
!
  nex = 0
  do i = 1,nldef
    if (ndeftyp(ndefst+i).eq.0) then
      nex = nex + 1
    endif
  enddo
  if (nex.gt.0) then
    allocate(itmp(4*nreg1),stat=status)
    if (status/=0) call outofmemory('outdefin','itmp')
    write(ioout,'(/,''  Explicit region 1 specified :'',/)')
    write(ioout,'(''------------------------------------------------------------------------------------'')')
    write(ioout,'(''   No.   Atomic       x            y             z         Charge   Occupancy'')')
    write(ioout,'(''         Label      (Angs)       (Angs)        (Angs)        (e)    '')')
    write(ioout,'(''------------------------------------------------------------------------------------'')')
    if (.not.ldsym.or.ndasym.eq.nreg1) then
      do i = 1,4*nreg1
        itmp(i) = 0
      enddo
      do i = 1,nvar
        itmp(idopt(i)) = 1
      enddo
    endif
    do i = 1,nreg1
      inat = natdefe(i)
      itype = ntypdefe(i)
      call label(inat,itype,lab)
      if (ldefbsmat(i)) then
        cstype = 'bc'
        if (inat.gt.maxele) cstype = 'bs'
      else
        cstype = 'c '
        if (inat.gt.maxele) cstype = 's '
      endif
      if (.not.ldsym.or.ndasym.eq.nreg1) then
        ind = 3*i - 2
        if (itmp(ind).eq.1) then
          ocha(1) = '*'
        else
          ocha(1) = ' '
        endif
        if (itmp(ind+1).eq.1) then
          ocha(2) = '*'
        else
          ocha(2) = ' '
        endif
        if (itmp(ind+2).eq.1) then
          ocha(3) = '*'
        else
          ocha(3) = ' '
        endif
      else
        ocha(1) = ' '
        ocha(2) = ' '
        ocha(3) = ' '
      endif
      write(ioout,'(2x,i4,2x,a5,1x,a2,1x,3(f10.4,1x,a1,1x),f10.5,3x,f6.4)') &
        i,lab,cstype,xdefe(i),ocha(1),ydefe(i),ocha(2),zdefe(i),ocha(3),qdefe(i),occdefe(i)
    enddo
    write(ioout,'(''------------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('outdefin','itmp')
  endif
!
!  List of vacancies
!
  nvac = 0
  do i = 1,nldef
    if (ndeftyp(ndefst+i).lt.10.and.ndeftyp(ndefst+i).gt.0) then
      nvac = nvac + 1
    endif
  enddo
  if (nvac.gt.0) then
    write(ioout,'(''  Vacancies:'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Specification type     Symbol/Number         x          y          z'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nldef
      if (ndeftyp(ndefst+i).lt.10) then
        ndt = ndeftyp(ndefst+i)
        if (ndt.eq.1) then
          write(ioout,'(4x,''Atom'',27x,i4)')nint(xdef(ndefst+i))
        elseif (ndt.eq.2) then
          inat = ndefnat(ndefst+i)
          itype = ndeftp(ndefst+i)
          if (inat.gt.2*maxele) then
            call label(inat-2_i4*maxele,itype,symb)
            write(ioout,'(4x,''Atom'',20x,a5)')symb
          else
            if (inat.gt.maxele) then
              lab2 = 'shel'
            else
              lab2 = 'core'
            endif
            call label(inat,itype,symb)
            write(ioout,'(4x,''Atom'',20x,a5,1x,a4)') symb,lab2
          endif
        elseif (ndt.eq.3) then
          write(ioout,'(4x,''Fractional'',28x,3f11.6)') xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
        elseif (ndt.eq.4) then
          write(ioout,'(4x,''Cartesian '',28x,3f11.6)') xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
        elseif (ndt.eq.5) then
          write(ioout,'(4x,''Molecule'',23x,i4)') nint(xdef(ndefst+i))
        endif
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  List of interstitials
!
  ninst = 0
  do i = 1,nldef
    if (ndeftyp(ndefst+i).ge.20) then
      ninst = ninst + 1
    endif
  enddo
  if (ninst.gt.0) then
    write(ioout,'(''  Interstitials:'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Specification type     Symbol/Number         x          y          z'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nldef
      ndt = ndeftyp(i+ndefst)
      if (ndt.ge.20) then
        if (ndt.eq.21) then
          inat = ndefnat(ndefst+i)
          itype = ndeftp(ndefst+i)
          lbre = .false.
          if (inat.gt.3*maxele) then
            inat = inat - 3*maxele
            lbre = .true.
          endif
          if (inat.gt.2*maxele) then
            call label(inat-2_i4*maxele,itype,symb)
            write(ioout,'(4x,''Atom/Fractional'',9x,a5,9x,3f11.6)') symb,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          else
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'bshe'
              else
                lab2 = 'bcor'
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'shel'
              else
                lab2 = 'core'
              endif
            endif
            call label(inat,itype,symb)
            write(ioout,'(4x,''Atom/Fractional'',9x,a5,1x,a4,5x,3f11.6)') &
              symb,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          endif
        elseif (ndt.eq.22) then
          inat = ndefnat(ndefst+i)
          itype = ndeftp(ndefst+i)
          lbre = .false.
          if (inat.gt.3*maxele) then
            inat = inat - 3*maxele
            lbre = .true.
          endif
          if (inat.gt.2*maxele) then
            call label(inat-2_i4*maxele,itype,symb)
            write(ioout,'(4x,''Atom/Cartesian '',9x,a5,9x,3f11.6)')  &
              symb,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          else
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'bshe'
              else
                lab2 = 'bcor'
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'shel'
              else
                lab2 = 'core'
              endif
            endif
            call label(inat,itype,symb)
            write(ioout,'(4x,''Atom/Cartesian '',9x,a5,1x,a4,5x,3f11.6)') &
              symb,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          endif
        elseif (ndt.eq.23) then
          inat = ndefnat(ndefst+i)
          itype = ndeftp(ndefst+i)
          lbre = .false.
          if (inat.gt.3*maxele) then
            inat = inat - 3*maxele
            lbre = .true.
          endif
          if (inat.gt.2*maxele) then
            call label(inat-2_i4*maxele,itype,symb)
            inat = nint(xdef(ndefst+i))
            ityp = nint(ydef(ndefst+i))
            call label(inat,ityp,symb2)
            write(ioout,'(4x,''Atom/Bond-Symbol'',8x,a5,9x,a5)') symb,symb2
          else
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'bshe'
              else
                lab2 = 'bcor'
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'shel'
              else
                lab2 = 'core'
              endif
            endif
            call label(inat,itype,symb)
            inat = nint(xdef(ndefst+i))
            ityp = nint(ydef(ndefst+i))
            call label(inat,ityp,symb2)
            write(ioout,'(4x,''Atom/Bond-Symbol'',8x,a5,1x,a4,5x,a5)') symb,lab2,symb2
          endif
        elseif (ndt.eq.24) then
          inat = ndefnat(ndefst+i)
          itype = ndeftp(ndefst+i)
          lbre = .false.
          if (inat.gt.3*maxele) then
            inat = inat - 3*maxele
            lbre = .true.
          endif
          if (inat.gt.2*maxele) then
            call label(inat-2_i4*maxele,itype,symb)
            write(ioout,'(4x,''Atom/Bond-Frac  '',8x,a5,9x,3f11.6)') &
              symb,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          else
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'bshe'
              else
                lab2 = 'bcor'
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'shel'
              else
                lab2 = 'core'
              endif
            endif
            call label(inat,itype,symb)
            write(ioout,'(4x,''Atom/Bond-Frac  '',8x,a5,1x,a4,5x,3f11.6)') &
              symb,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          endif
        elseif (ndt.eq.25) then
          inat = ndefnat(ndefst+i)
          itype = ndeftp(ndefst+i)
          lbre = .false.
          if (inat.gt.3*maxele) then
            inat = inat - 3*maxele
            lbre = .true.
          endif
          if (inat.gt.2*maxele) then
            call label(inat-2_i4*maxele,itype,symb)
            write(ioout,'(4x,''Atom/Bond-Cart  '',8x,a5,9x,3f11.6)') &
              symb,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          else
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'bshe'
              else
                lab2 = 'bcor'
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'shel'
              else
                lab2 = 'core'
              endif
            endif
            call label(inat,itype,symb)
            write(ioout,'(4x,''Atom/Bond-Cart  '',8x,a5,1x,a4,5x,3f11.6)') &
              symb,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          endif
        endif
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  List of impurities
!
  nimp = 0
  do i = 1,nldef
    if (ndeftyp(ndefst+i).ge.10.and.ndeftyp(ndefst+i).lt.20) then
      nimp = nimp + 1
    endif
  enddo
  if (nimp.gt.0) then
    write(ioout,'(''  Impurities:'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Specification type     Symbol/Number         x          y          z'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nldef
      ndt = ndeftyp(ndefst+i)
      if (ndt.ge.10.and.ndt.lt.20) then
        inat = ndefnat(ndefst+i)
        itype = ndeftp(ndefst+i)
        lbre = .false.
        if (inat.gt.3*maxele) then
          inat = inat - 3*maxele
          lbre = .true.
        endif
        if (inat.gt.2*maxele) then
          call label(inat-2_i4*maxele,itype,symb)
          if (ndt.eq.11) then
            write(ioout,'(4x,''Symbol/Number'',11x,a5,1x,i4)') symb,nint(xdef(ndefst+i))
          elseif (ndt.eq.12) then
            iptr = nrel2(nint(xdef(ndefst+i)))
            inat = nat(iptr)
            itype = nftype(iptr)
            call label(inat,itype,symb2)
            write(ioout,'(4x,''Symbol/Symbol'',11x,a5,2x,a5)') symb,symb2
          elseif (ndt.eq.13) then
            write(ioout,'(4x,''Frac/Symbol'',13x,a5,9x,3f11.6)') symb,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          elseif (ndt.eq.14) then
            write(ioout,'(4x,''Cart/Symbol'',13x,a5,9x,3f11.6)') symb,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          endif
        else
          if (lbre) then
            if (inat.gt.maxele) then
              lab2 = 'bshe'
            else
              lab2 = 'bcor'
            endif
          else
            if (inat.gt.maxele) then
              lab2 = 'shel'
            else
              lab2 = 'core'
            endif
          endif
          call label(inat,itype,symb)
          if (ndt.eq.11) then
            write(ioout,'(4x,''Symbol/Number'',11x,a5,1x,a4,2x,i4)') symb,lab2,nint(xdef(ndefst+i))
          elseif (ndt.eq.12) then
            iptr = nint(xdef(ndefst+i))
            inat = nat(iptr)
            itype = nftype(iptr)
            call label(inat,itype,symb2)
            write(ioout,'(4x,''Symbol/Symbol'',11x,a5,1x,a4,2x,a5)') symb,lab2,symb2
          elseif (ndt.eq.13) then
            write(ioout,'(4x,''Frac/Symbol'',13x,a5,1x,a4,5x,3f11.6)')  &
              symb,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          elseif (ndt.eq.14) then
            write(ioout,'(4x,''Cart/Symbol'',13x,a5,1x,a4,5x,3f11.6)') &
              symb,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
          endif
        endif
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Full region 1 coordinates, if requested
!
  if (index(keyword,'regi').ne.0.and.nex.eq.0) then
    allocate(itmp(4*nreg1),stat=status)
    if (status/=0) call outofmemory('outdefin','itmp')
    write(ioout,'(/,''  Region 1 (Absolute coordinates) :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''   No.   Atomic       x            y             z         Charge   Occupancy'')')
    write(ioout,'(''         Label      (Angs)       (Angs)        (Angs)        (e)    '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if (nreg1.eq.0) then
      write(ioout,'(''  There are no ions in region 1 !'')')
    else
      if (.not.ldsym.or.ndasym.eq.nreg1) then
        do i = 1,4*nreg1
          itmp(i) = 0
        enddo
        do i = 1,nvar
          itmp(idopt(i)) = 1
        enddo
      endif
      do i = 1,nreg1
        inat = natdefe(i)
        itype = ntypdefe(i)
        call label(inat,itype,lab)
        if (ldefbsmat(i)) then
          cstype = 'bc'
          if (inat.gt.maxele) cstype = 'bs'
        else
          cstype = 'c '
          if (inat.gt.maxele) cstype = 's '
        endif
        if (.not.ldsym.or.ndasym.eq.nreg1) then
          ind = 3*i - 2
          if (itmp(ind).eq.1) then
            ocha(1) = '*'
          else
            ocha(1) = ' '
          endif
          if (itmp(ind+1).eq.1) then
            ocha(2) = '*'
          else
            ocha(2) = ' '
          endif
          if (itmp(ind+2).eq.1) then
            ocha(3) = '*'
          else
            ocha(3) = ' '
          endif
        else
          ocha(1) = ' '
          ocha(2) = ' '
          ocha(3) = ' '
        endif
        write(ioout,'(2x,i4,2x,a5,1x,a2,1x,3(f10.4,1x,a1,1x),f10.5,1x,f10.5)') &
          i,lab,cstype,xdefe(i),ocha(1),ydefe(i),ocha(2),zdefe(i),ocha(3),qdefe(i),occdefe(i)
      enddo
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
    if ((abs(xdc)+abs(ydc)+abs(zdc)).gt.1.0d-4.and.nreg1.gt.0.and.(.not.ldsym.or.ndasym.eq.nreg1)) then
      write(ioout,'(/,''  Region 1 (Relative to defect centre) :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''   No.   Atomic       x            y             z         Charge   Occupancy'')')
      write(ioout,'(''         Label      (Angs)       (Angs)        (Angs)        (e)    '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nreg1
        inat = natdefe(i)
        itype = ntypdefe(i)
        call label(inat,itype,lab)
        if (ldefbsmat(i)) then
          cstype = 'bc'
          if (inat.gt.maxele) cstype = 'bs'
        else
          cstype = 'c '
          if (inat.gt.maxele) cstype = 's '
        endif
        if (.not.ldsym.or.ndasym.eq.nreg1) then
          ind = 3*i - 2
          if (itmp(ind).eq.1) then
            ocha(1) = '*'
          else
            ocha(1) = ' '
          endif
          if (itmp(ind+1).eq.1) then
            ocha(2) = '*'
          else
            ocha(2) = ' '
          endif
          if (itmp(ind+2).eq.1) then
            ocha(3) = '*'
          else
            ocha(3) = ' '
          endif
        else
          ocha(1) = ' '
          ocha(2) = ' '
          ocha(3) = ' '
        endif
        write(ioout,'(2x,i4,2x,a5,1x,a2,1x,3(f10.4,1x,a1,1x),f10.5,1x,f10.5)') &
          i,lab,cstype,(xdefe(i)-xdc),ocha(1),(ydefe(i)-ydc),ocha(2),(zdefe(i)-zdc),ocha(3),qdefe(i),occdefe(i)
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('outdefin','itmp')
  endif
  if (ndasym.lt.nreg1.and.ldsym) then
    allocate(itmp(4*ndasym),stat=status)
    if (status/=0) call outofmemory('outdefin','itmp')
    write(ioout,'(/,''  Symmetry reduced region 1 (Relative to defect centre) :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''   No.   Atomic       x            y             z       Radius  Multiplicity   '')')
    write(ioout,'(''         Label      (Angs)       (Angs)        (Angs)    (Angs)     '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,4*ndasym
      itmp(i) = 0
    enddo
    do i = 1,nvar
      itmp(idopt(i)) = 1
    enddo
    do ii = 1,ndasym
      i = ndsptr(ii)
      inat = natdefe(i)
      itype = ntypdefe(i)
      call label(inat,itype,lab)
      if (ldefbsmat(i)) then
        cstype = 'bc'
        if (inat.gt.maxele) cstype = 'bs'
      else
        cstype = 'c '
        if (inat.gt.maxele) cstype = 's '
      endif
      ind = 3*ii - 2
      if (itmp(ind).eq.1) then
        ocha(1) = '*'
      else
        ocha(1) = ' '
      endif
      if (itmp(ind+1).eq.1) then
        ocha(2) = '*'
      else
        ocha(2) = ' '
      endif
      if (itmp(ind+2).eq.1) then
        ocha(3) = '*'
      else
        ocha(3) = ' '
      endif
!
!  Find whether species is a defect or not
!
      dchar = ' '
      do j = 1,ninte
        if (ndintptr(j).eq.i) dchar = 'Defect'
      enddo
      write(ioout,'(2x,i4,2x,a5,1x,a2,1x,3(f10.4,1x,a1,1x),f7.4,2x,i4,4x,a6)') &
        ii,lab,cstype,(xdefe(i)-xdc),ocha(1),(ydefe(i)-ydc),ocha(2),(zdefe(i)-zdc),ocha(3),radefe(i), &
        ndeqv(ii),dchar
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('outdefin','itmp')
  endif
!
!  Constraint output
!
  if (ndcon.gt.0) then
    write(ioout,'(/)')
    if (ndasym.lt.nreg1.and.ldsym) then
      write(ioout,'(''  Constraints on symmetry reduced coordinates :'',/)')
    else
      write(ioout,'(''  Constraints on coordinates :'',/)')
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Constraint no.      Unconstrained     Constrained    Coefficient    '')')
    write(ioout,'(''                         Variable         Variable'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    ncfst = 0
    do i = 1,ndcon
      ncvi = ncdvar(i+ncfst)
      nfv = ncdfix(i+ncfst) - 1
      nvv = ncvi - 1
      nfv1 = (nfv/3) + 1
      nvv1 = (nvv/3) + 1
      ncrf = nfv - 3*(nfv1-1) + 1
      ncrv = nvv - 3*(nvv1-1) + 1
      write(ioout,'(8x,i4,14x,i4,1x,a1,10x,i4,1x,a1,6x,f10.5,5x,f7.4)')i,nvv1,crd(ncrv),nfv1,crd(ncrf),dconco(i+ncfst)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Full region 2 coordinates, if requested
!
  if (index(keyword,'regi2').ne.0) then
    write(ioout,'(/,''  Region 2 :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''   No.   Atomic         x              y               z           Charge   '')')
    write(ioout,'(''         Label        (Angs)         (Angs)          (Angs)          (e)    '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if (nreg2.eq.0) then
      write(ioout,'(''  No region 2 ions'')')
    endif
    do i = 1,nreg2
      inat = nr2a(i)
      itype = ntr2a(i)
      call label(inat,itype,lab)
      if (ldbr2a(i)) then
        cstype = 'bc'
        if (inat.gt.maxele) cstype = 'bs'
      else
        cstype = 'c '
        if (inat.gt.maxele) cstype = 's '
      endif
      write(ioout,'(2x,i4,2x,a5,1x,a2,1x,f12.6,3(2x,1x,f12.6))') &
        i,lab,cstype,xr2a(i),yr2a(i),zr2a(i),qr2a(i)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
  endif
!
  return
  end
