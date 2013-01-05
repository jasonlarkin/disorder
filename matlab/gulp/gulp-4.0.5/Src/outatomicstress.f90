  subroutine outatomicstress
!
!  Output atomic stresses
!
!   5/12 Created from optout
!   5/12 Symmetry reduction added for symmetry output
!   5/12 Sum of atomic stresses added as a cross check
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
!  Copyright Curtin University, 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations,   only : lbsmat
  use constants,        only : evtoj
  use control
  use current
  use derivatives
  use element
  use iochannels
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  character(len=2)                             :: cstype
  character(len=5)                             :: lab
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: inat
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: kl
  real(dp)                                     :: rvol
  real(dp)                                     :: sumas(6)
  real(dp)                                     :: vol
  real(dp)                                     :: volume
!
  if (ioproc) then
!
!  Compute sum of atomic stresses as a cross check
!
    sumas(1:nstrains) = 0.0_dp
    do i = 1,numat
      do kl = 1,nstrains
        sumas(kl) = sumas(kl) + atomicstress(kl,i)
      enddo
    enddo
!
    if (ndim.eq.3) then
!
!  Output header for stresses - 3 D only
!
      write(ioout,'(/,''  Atomic stresses (GPa): '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      if (ndim.eq.3) then
        write(ioout,'(''   No.  Atomic          xx            yy            zz         '')')
        write(ioout,'(''        Label          /yz           /xz           /xy         '')')
      elseif (ndim.eq.2) then
        write(ioout,'(''   No.  Atomic          xx            yy            xy         '')')
        write(ioout,'(''        Label                                                  '')')
      elseif (ndim.eq.1) then
        write(ioout,'(''   No.  Atomic          xx                                     '')')
        write(ioout,'(''        Label                                                  '')')
      endif
      write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Convert strain derivatives to stresses and adjust units to GPa
!
      vol = volume(rv)
      rvol = 10.0_dp*evtoj*1.0d20/vol
!
      do i = 1,nasym
        inat = iatn(i)
        itype = natype(i)
        ii = nrel2(i)
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
          write(ioout,'(i7,1x,a5,1x,a2,3f14.6)') i,lab,cstype,(rvol*atomicstress(j,ii),j=1,3)
          write(ioout,'(16x,3f14.6)') (rvol*atomicstress(j,ii),j=4,6)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''   Sum '',9x,3f14.6)') (rvol*sumas(j),j=1,3)
      write(ioout,'(16x,3f14.6)') (rvol*sumas(j),j=4,6)
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
!
!  Output header for strain derivatives
!
    write(ioout,'(/,''  Atomic strain derivatives (eV): '',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if (ndim.eq.3) then
      write(ioout,'(''   No.  Atomic          xx            yy            zz         '')')
      write(ioout,'(''        Label          /yz           /xz           /xy         '')')
    elseif (ndim.eq.2) then
      write(ioout,'(''   No.  Atomic          xx            yy            xy         '')')
      write(ioout,'(''        Label                                                  '')')
    elseif (ndim.eq.1) then
      write(ioout,'(''   No.  Atomic          xx                                     '')')
      write(ioout,'(''        Label                                                  '')')
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
!
    do i = 1,nasym
      inat = iatn(i)
      itype = natype(i)
      ii = nrel2(i)
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
        if (ndim.eq.3) then
          write(ioout,'(i7,1x,a5,1x,a2,3f14.6)') i,lab,cstype,(atomicstress(j,ii),j=1,3)
          write(ioout,'(16x,3f14.6)') (atomicstress(j,ii),j=4,6)
        elseif (ndim.eq.2) then
          write(ioout,'(i7,1x,a5,1x,a2,3f14.6)') i,lab,cstype,(atomicstress(j,ii),j=1,3)
        elseif (ndim.eq.1) then
          write(ioout,'(i7,1x,a5,1x,a2,f14.6)') i,lab,cstype,atomicstress(1,ii)
        endif
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if (ndim.eq.3) then
      write(ioout,'(''   Sum '',9x,3f14.6)') (sumas(j),j=1,3)
      write(ioout,'(16x,3f14.6)') (sumas(j),j=4,6)
    elseif (ndim.eq.2) then
      write(ioout,'(''   Sum '',9x,3f14.6)') (sumas(j),j=1,3)
    elseif (ndim.eq.1) then
      write(ioout,'(''   Sum '',9x,f14.6)') sumas(1)
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
  return
  end
