  subroutine outstructure
!
!  Output structure
!
!   1/01 created from optout
!   5/02 output of Cartesian coordinates added
!  10/03 Rhombohedral coordinates now output in consistent setting
!   6/09 Modified to include charge print out for variable charge case
!  12/10 Hiding of shells option added
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
!  Julian Gale, NRI, Curtin University, December 2010
!
  use configurations
  use control
  use current
  use element
  use iochannels
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  character(len=2) :: cstype
  character(len=5) :: lab
  integer(i4)      :: i
  integer(i4)      :: inat
  integer(i4)      :: itype
  integer(i4)      :: nri
  logical          :: lrhombo
!
!  Output final geometry and derivatives
!
  if (ioproc) then
!
!  Check cell setting
!
    lrhombo = (ifhr(ncf).eq.1.and.(.not.lhex))
!
    if (lsymopt) then
      write(ioout,'(/,''  Final asymmetric unit coordinates :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      if (leem) then
        write(ioout,'(''   No.  Atomic        x           y           z         Radius      Charge'')')
      else
        write(ioout,'(''   No.  Atomic        x           y           z         Radius'')')
      endif
      if (ndim.eq.3) then
        write(ioout,'(''        Label       (Frac)      (Frac)      (Frac)       (Angs) '')')
      elseif (ndim.eq.2) then
        write(ioout,'(''        Label       (Frac)      (Frac)      (Angs)       (Angs) '')')
      elseif (ndim.eq.1) then
        write(ioout,'(''        Label       (Frac)      (Angs)      (Angs)       (Angs) '')')
      else
        write(ioout,'(''        Label       (Angs)      (Angs)      (Angs)       (Angs) '')')
      endif
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nasym
        inat = iatn(i)
        itype = natype(i)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (lbsmat(i+nsft)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          if (lrhombo) then
            nri = nrel2(i)
            if (leem) then
              write(ioout,'(2x,i4,2x,a5,1x,a2,5f12.6)') &
                i,lab,cstype,xfrac(nri),yfrac(nri),zfrac(nri),radcfg(i+nsft),qlcfg(i+nsft)
            else
              write(ioout,'(2x,i4,2x,a5,1x,a2,4f12.6)') &
                i,lab,cstype,xfrac(nri),yfrac(nri),zfrac(nri),radcfg(i+nsft)
            endif
          else
            if (leem) then
              write(ioout,'(2x,i4,2x,a5,1x,a2,5f12.6)') &
                i,lab,cstype,xcfg(nsft+i),ycfg(nsft+i),zcfg(nsft+i),radcfg(i+nsft),qlcfg(i+nsft)
            else
              write(ioout,'(2x,i4,2x,a5,1x,a2,4f12.6)') &
                i,lab,cstype,xcfg(nsft+i),ycfg(nsft+i),zcfg(nsft+i),radcfg(i+nsft)
            endif
          endif
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    else
      if (ndim.eq.3) then
        write(ioout,'(/,''  Final fractional coordinates of atoms :'',/)')
      elseif (ndim.eq.2) then
        write(ioout,'(/,''  Final fractional/Cartesian coordinates of atoms :'',/)')
      elseif (ndim.eq.1) then
        write(ioout,'(/,''  Final fractional/Cartesian coordinates of atoms :'',/)')
      else
        write(ioout,'(/,''  Final cartesian coordinates of atoms :'',/)')
      endif
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      if (leem) then
        write(ioout,'(''   No.  Atomic        x           y           z         Radius      Charge'')')
      else
        write(ioout,'(''   No.  Atomic        x           y          z          Radius'')')
      endif
      if (ndim.eq.3) then
        write(ioout,'(''        Label       (Frac)      (Frac)     (Frac)       (Angs) '')')
      elseif (ndim.eq.2) then
        write(ioout,'(''        Label       (Frac)      (Frac)     (Angs)       (Angs) '')')
      elseif (ndim.eq.1) then
        write(ioout,'(''        Label       (Frac)      (Angs)     (Angs)       (Angs) '')')
      else
        write(ioout,'(''        Label       (Angs)      (Angs)     (Angs)       (Angs) '')')
      endif
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,numat
        inat = nat(i)
        itype = nftype(i)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (lbsmat(nrelat(i)+nsft)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          if (leem) then
            write(ioout,'(1x,i5,2x,a5,1x,a2,5f12.6)')i,lab,cstype,xfrac(i),yfrac(i),zfrac(i),radf(i),qf(i)
          else
            write(ioout,'(1x,i5,2x,a5,1x,a2,4f12.6)')i,lab,cstype,xfrac(i),yfrac(i),zfrac(i),radf(i)
          endif
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
    if (ndimen(ncf).gt.0.and.index(keyword,'cart').ne.0) then
      write(ioout,'(''  Final Cartesian coordinates :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''   No.  Atomic        x           y           z          Charge   Occupancy'')')
      write(ioout,'(''        Label       (Angs)      (Angs)      (Angs)        (e)       (Frac)  '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,numat
        inat = nat(i)
        itype = nftype(i)
!
!  Hide shells?
!
        if (inat.le.maxele.or..not.lhideshells) then
          call label(inat,itype,lab)
          if (lbsmat(nrelat(i)+nsft)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          write(ioout,'(1x,i5,2x,a5,1x,a2,5f12.6)')i,lab,cstype,xclat(i),yclat(i),zclat(i),qf(i),occuf(i)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(/)')
    endif
  endif
!
  return
  end
