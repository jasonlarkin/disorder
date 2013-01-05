  subroutine outvarg(gc)
!
!  Output variable gradients from fitting
!
!   4/08 Created from outvar
!   6/09 Module name changed from three to m_three
!  12/09 Corrected for constraint case
!  10/10 EDIP potentials added
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
!  Julian Gale, NRI, Curtin University, October 2010
!
  use constants
  use current, only : nstrains
  use eam
  use element, only : maxele
  use fitting
  use four
  use iochannels
  use m_three
  use six
  use two
  implicit none
!
!  Passed variables
!
  real(dp)         :: gc(*)
!
!  Local variables
!
  character(len=1) :: cstyp1
  character(len=1) :: namecrd(3)
  character(len=5) :: lab1
  integer(i4)      :: i
  integer(i4)      :: ii
  integer(i4)      :: nat1
  integer(i4)      :: nccf
  integer(i4)      :: nf
  integer(i4)      :: nf2
  integer(i4)      :: nf3
  integer(i4)      :: nio
  integer(i4)      :: nio2
  integer(i4)      :: np
  integer(i4)      :: nt
  integer(i4)      :: ntp
  integer(i4)      :: ntyp1
  integer(i4)      :: ntyps
  integer(i4)      :: nv
  integer(i4)      :: nv2
  real(dp)         :: w1i
!
  data namecrd/'x','y','z'/
!***********************
!  Table of variables  *
!***********************
  write(ioout,'(/,''  Final values of numerical parameter gradients :'',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''     Parameter No.       Parameter Gradient      Parameter Type  Species'')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  do ii = 1,nfitt
    i = nfitptr(ii)
    nt  = nftyp(i)
    nf  = nfpot(i)
    nf2 = nfpot2(i)
    nf3 = nfpot3(i)
    nv  = nfvar(i)
    nv2 = nfvar2(i)
    if (nt.eq.1) then
!----------------------
!  General parameters -
!----------------------
      if (nf.eq.1) then
!
!  Shell coordinate
!
        nccf = nfcfg(i)
        nio = nv
        nio2 = (nio - (nstrains-2))/3
        nio = nio - (nstrains-3) - 3*nio2
        w1i = gc(ii)
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i2,1x,i3,1x,a)') i,w1i,fitlabel(1,nf,1),nccf,nio2,namecrd(nio)
      elseif (nf.eq.2) then
!
!  Breathing shell radius
!
        nccf = nfcfg(i)
        w1i = gc(ii)
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i2,1x,i3,1x)') i,w1i,fitlabel(1,nf,1),nccf,nv
      elseif (nf.eq.3) then
!
!  Shift
!
        w1i = gc(ii)
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(1,nf,1),nv
      elseif (nf.ge.7.and.nf.le.10) then
!
!  Epsilon/sigma/A/B
!
        w1i = gc(ii)
        if (nf.le.8) then
          nat1 = natse(nv)
          ntyp1 = ntypse(nv)
        else
          nat1 = nattab(nv)
          ntyp1 = ntypab(nv)
        endif
        call label(nat1,ntyp1,lab1)
        if (nat1.gt.maxele) then
          cstyp1 = 's'
        else
          cstyp1 = 'c'
        endif
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,a5,1x,a1)') i,w1i,fitlabel(1,nf,1),lab1,cstyp1
      else
!
!  Other species variables
!
        w1i = gc(ii)
        ntyps = nfatyp(i)
        call label(nv,ntyps,lab1)
        if (nv.gt.maxele) then
          cstyp1 = 's'
        else
          cstyp1 = 'c'
        endif
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,a5,1x,a1)') i,w1i,fitlabel(1,nf,1),lab1,cstyp1
      endif
    elseif (nt.eq.2) then
!-----------------------
!  Two-body potentials -
!-----------------------
      np = nptype(nf)
      w1i = gc(ii)
      write(ioout,'(10x,i4,10x,f16.6,10x,a15)') i,w1i,fitlabel(nv,np,2)
    elseif (nt.eq.3) then
!-------------------------
!  Three-body potentials -
!-------------------------
      w1i = gc(ii)
      write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv,nthrty(nf),3),nf
    elseif (nt.eq.4) then
!------------------------
!  Four-body potentials -
!------------------------
      w1i = gc(ii)
      write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv,nforty(nf),4),nf
    elseif (nt.eq.5) then
!------------------------
!  Many-body potentials -
!------------------------
      w1i = gc(ii)
      nat1 = neamnat(nf)
      ntp = neamtyp(nf)
      call label(nat1,ntp,lab1)
      if (nat1.gt.maxele) then
        cstyp1 = 's'
      else
        cstyp1 = 'c'
      endif
      write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,a5,1x,a1)') i,w1i,fitlabel(nv,1,5),lab1,cstyp1
    elseif (nt.eq.6) then
!-------------------------
!  Bond-order potentials -
!-------------------------
      w1i = gc(ii)
      write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv,1,6),nf
    elseif (nt.eq.7) then
!-----------------------
!  Six-body potentials -
!-----------------------
      w1i = gc(ii)
      write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv,nsixty(nf),7),nf
    elseif (nt.eq.8) then
!---------------------
!  ReaxFF potentials -
!---------------------
      w1i = gc(ii)
      if (nv.le.8) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv2,nv,8),nf
      elseif (nv.le.15) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv2,nv,8)
      elseif (nv.le.20) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv2,nv,8),nf
      elseif (nv.le.23) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4,1x,i4)') i,w1i,fitlabel(nv2,nv,8),nf,nf2
      elseif (nv.eq.24) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,3i4)') i,w1i,fitlabel(nv2,nv,8),nf,nf2,nf3
      elseif (nv.eq.25) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4,1x,i4)') i,w1i,fitlabel(nv2,nv,8),nf,nf2
      endif
    elseif (nt.eq.9) then
!-------------------
!  EDIP potentials -
!-------------------
      w1i = gc(ii)
      if (nv.le.2) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,i4)') i,w1i,fitlabel(nv2,nv,9),nf
      elseif (nv.eq.3) then
        write(ioout,'(10x,i4,10x,f16.6,10x,a15,2x,2i4)') i,w1i,fitlabel(nv2,nv,9),nf,nf2
      endif
    endif
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'',/)')
!
  return
  end
