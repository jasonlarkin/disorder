  subroutine outgafit
!
!  Output final configurations from genetic algorithm fitting
!
!   7/97 EAM parameters added
!   6/98 Three-body polynomial coefficients added
!  10/98 Codes for fitting variables simplified
!  11/03 ndennat/ndentyp replaced
!  11/04 Modifications for six-body potentials added
!   4/07 UFF parameters added
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
!  Copyright Curtin University 2007
!  
!  Julian Gale, NRI, Curtin University, April 2007
!
  use eam
  use element, only : maxele
  use fitting
  use gaconf
  use genetic
  use iochannels
  use two
  use uffdata
  implicit none
!
!  Local variables
!
  character(len=1)   :: cstyp1
  character(len=5)   :: lab1
  integer(i4)        :: i
  integer(i4)        :: j
  integer(i4)        :: jbase
  integer(i4)        :: jmax
  integer(i4)        :: nat1
  integer(i4)        :: nf
  integer(i4)        :: ng
  integer(i4)        :: nga
  integer(i4)        :: ngroup
  integer(i4)        :: nt
  integer(i4)        :: ntp
  integer(i4)        :: ntyps
  integer(i4)        :: nv
  real(dp)           :: w1(4)
!***************************
!  Decide type of results  *
!***************************
  write(ioout,'(/)')
  if (ngabest.eq.0) then
    write(ioout,'(''  Final configurations from genetic algorithm fitting :'',/)')
    nga = ngacfg
  else
    write(ioout,'(''  Best configurations from genetic algorithm fitting :'',/)')
    nga = ngabest
  endif
!
!  How many lots of 4?
!
  ngroup = ((nga - 1)/4) + 1
!***********************
!  Table of variables  *
!***********************
  do ng = 1,ngroup
    if (ng.eq.ngroup) then
      jmax = nga - 4*(ngroup-1)
    else
      jmax = 4
    endif
    jbase = 4*(ng-1)
    if (ngabest.gt.0) jbase = jbase + 2*ngacfg
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(25x,4(5x,i3,5x))')(4*(ng-1)+j,j=1,jmax)
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Sum of squares =  '',5x,4(1x,f12.5))') (fconf(jbase+j),j=1,jmax)
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nfit
      nt = nftyp(i)
      nf = nfpot(i)
      nv = nfvar(i)
      if (nt.eq.1) then
!----------------------
!  General parameters -
!----------------------
        if (nf.eq.3) then
!
!  Shift
!
          do j = 1,jmax
            w1(j) = xconf(i,jbase+j)*scale(i)
          enddo
          write(ioout,'(i3,2x,a15,1x,i4,2x,4(1x,f12.5))') i,fitlabel(1,nf,1),nv,(w1(j),j=1,jmax)
        elseif (nf.ge.7.and.nf.le.10) then
!
!  Epsilon/sigma/A/B
!
          do j = 1,jmax
            w1(j) = xconf(i,jbase+j)*scale(i)
          enddo
          if (nf.le.8) then
            nat1 = natse(nv)
            ntyps = ntypse(nv)
          else
            nat1 = nattab(nv)
            ntyps = ntypab(nv)
          endif
          call label(nat1,ntyps,lab1)
          if (nat1.gt.maxele) then
            cstyp1 = 's'
          else
            cstyp1 = 'c'
          endif
          write(ioout,'(i3,2x,a15,a5,1x,a1,4(1x,f12.6))') i,fitlabel(1,nf,1),lab1,cstyp1,(w1(j),j=1,jmax)
        elseif (nf.ge.20.and.nf.le.25) then
!
!  UFF parameters
!
          do j = 1,jmax
            w1(j) = xconf(i,jbase+j)*scale(i)
          enddo
          nat1 = natUFFspec(nv)
          ntyps = ntypUFFspec(nv)
          call label(nat1,ntyps,lab1)
          if (nat1.gt.maxele) then
            cstyp1 = 's'
          else
            cstyp1 = 'c'
          endif
          write(ioout,'(i3,2x,a15,a5,1x,a1,4(1x,f12.6))') i,fitlabel(1,nf,1),lab1,cstyp1,(w1(j),j=1,jmax)
        else
!
!  Species variables
!
          do j = 1,jmax
            w1(j) = xconf(i,jbase+j)*scale(i)
          enddo
          ntyps = nfatyp(i)
          call label(nv,ntyps,lab1)
          if (nv.gt.maxele) then
            cstyp1 = 's'
          else
            cstyp1 = 'c'
          endif
          write(ioout,'(i3,2x,a15,a5,1x,a1,4(1x,f12.6))') i,fitlabel(1,nf,1),lab1,cstyp1,(w1(j),j=1,jmax)
        endif
      elseif (nt.eq.2) then
!-----------------------
!  Two-body parameters -
!-----------------------
        do j = 1,jmax
          w1(j) = xconf(i,jbase+j)*scale(i)
        enddo
        write(ioout,'(i3,2x,a15,7x,4(1x,f12.5))') i,fitlabel(nv,nf,2),(w1(j),j=1,jmax)
      elseif (nt.eq.3) then
!-------------------------
!  Three-body parameters -
!-------------------------
        do j = 1,jmax
          w1(j) = xconf(i,jbase+j)*scale(i)
        enddo
        write(ioout,'(i3,2x,a15,1x,i4,2x,4(1x,f12.5))') i,fitlabel(nv,nf,3),nt,(w1(j),j=1,jmax)
      elseif (nt.eq.4) then
!------------------------
!  Four-body parameters -
!------------------------
        do j = 1,jmax
          w1(j) = xconf(i,jbase+j)*scale(i)
        enddo
        write(ioout,'(i3,2x,a15,1x,i4,2x,4(1x,f12.5))') i,fitlabel(nv,nf,4),nt,(w1(j),j=1,jmax)
      elseif (nt.eq.5) then
!------------------------
!  Many-body parameters -
!------------------------
        do j = 1,jmax
          w1(j) = xconf(i,jbase+j)*scale(i)
        enddo
        nat1 = neamnat(nv)
        ntp = neamtyp(nv)
        call label(nat1,ntp,lab1)
        if (nat1.gt.maxele) then
          cstyp1 = 's'
        else
          cstyp1 = 'c'
        endif
        write(ioout,'(i3,2x,a15,a5,1x,a1,4(1x,f12.6))') i,fitlabel(1,nf,5),lab1,cstyp1,(w1(j),j=1,jmax)
      elseif (nt.eq.7) then
!-----------------------
!  Six-body parameters -
!-----------------------
        do j = 1,jmax
          w1(j) = xconf(i,jbase+j)*scale(i)
        enddo
        write(ioout,'(i3,2x,a15,1x,i4,2x,4(1x,f12.5))') i,fitlabel(nv,nf,7),nt,(w1(j),j=1,jmax)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  enddo
!
  return
  end
