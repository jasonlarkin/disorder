  subroutine symout
!
!  Print out symmetry information
!
!     hmssg  = Hermann-Mauguin Symbol of Space Group
!     nspcg  = International Tables Number for Space Group
!     iflags = Integer Flag for Group Symbol
!     ifhr   = Integer Flag for Hexagonal or Rhombohedral Cell
!     ifso   = Integer Flag for Shift of the Origin
!     ivso   = Integer Value for Shift of the Origin
!     ishorg = Integer Shift of the Origin
!     nccs   = Numeric Code for Crystal System
!     ncpg   = Numeric Code for Point Group
!
  use current
  use iochannels
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: nspg
!
  if (ngocfg(ncf).gt.1) then
!
!  Output as explicitly input operators
!
    if (ifhr(ncf).eq.1) then
      write(ioout,'(''  Cell type = '',''rhombohedral'',/)') 
    else
      write(ioout,'(''  Cell type = '',a12,/)') texc(nccscfg(ncf))
    endif
    write(ioout,'(''  Number of symmetry operators = '',i2,/)') ngocfg(ncf)
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Operator : Rotation matrix                            : Translation vector    '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,ngocfg(ncf)
      write(ioout,'(4x,i2,6x,3(f12.6),8x,f12.6)') i,ropcfg(1,1,i,ncf),ropcfg(2,1,i,ncf), &
        ropcfg(3,1,i,ncf),vitcfg(1,i,ncf)
      write(ioout,'(12x,3(f12.6),8x,f12.6)') ropcfg(1,2,i,ncf),ropcfg(2,2,i,ncf), &
        ropcfg(3,2,i,ncf),vitcfg(2,i,ncf)
      write(ioout,'(12x,3(f12.6),8x,f12.6)') ropcfg(1,3,i,ncf),ropcfg(2,3,i,ncf), &
        ropcfg(3,3,i,ncf),vitcfg(3,i,ncf)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  else
!
!  Output space group data
!
    nspg = nspcg(ncf)
    write(ioout,'(''  Symmetry :''//''  Crystal family                   :  '',a12)') texc(nccs)
    write(ioout,'(''  Crystal class  (Groth - 1921)    :  '',a37)') cl(ncpg)
    if (ncs.eq.0) write(ioout,'(/''  Space group (centrosymmetric)    :  '',16a1)') (hmssg(i,ncf),i=1,16)
    if (ncs.eq.1) write(ioout,'(/''  Space group (noncentrosymmetric) :  '',16a1)') (hmssg(i,ncf),i=1,16)
    if (lalter) write(ioout,'(/''  Non-standard setting of group    :  '',a16)') gronam(nspg)
    write(ioout,'(/''  Patterson group                  :  '',a9)') patter(ipatgrp(nspg))
    if (ifso(ncf).eq.1.and.nspg.ne.0) then
      ii = ishorg(1,nspg) + ishorg(2,nspg) + ishorg(3,nspg)
      if (ii.ne.0) then
        ivso(1,ncf) = mod((ishorg(1,nspg)+240),24)
        ivso(2,ncf) = mod((ishorg(2,nspg)+240),24)
        ivso(3,ncf) = mod((ishorg(3,nspg)+240),24)
        write(ioout,'(/''  Shift of the origin              :'',3(2x,a5))') (trax(ivso(i,ncf)+1),i=1,3)
      endif
    elseif (ifso(ncf).eq.2) then
      ivso(1,ncf) = mod((ivso(1,ncf)+240),24)
      ivso(2,ncf) = mod((ivso(2,ncf)+240),24)
      ivso(3,ncf) = mod((ivso(3,ncf)+240),24)
      write(ioout,'(/''  Shift of the origin              :'',3(2x,a5))') (trax(ivso(i,ncf)+1),i=1,3)
    endif
  endif
!
  return
  end
