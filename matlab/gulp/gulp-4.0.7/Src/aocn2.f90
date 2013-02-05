  subroutine aocn2(n)
!
!  Return average observed coordination number
!  subroutine requires atomic number and oxidition state
!
!  Scott Woodley, Royal Institution of GB, June 1997
!
  use control
  use current, only : cna, nat, oxa
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)        :: n
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: o
  logical            :: lprint
!
  lprint = (index(keyword,'debug').ne.0)
!  Atomic number
  i = nat(n)
!  Oxidation number
  o = int(dabs(oxa(n)))
!  Zoom to periodic row
  if (i.gt.54) goto 6
  if (i.gt.36) goto 5
  if (i.gt.18) goto 4
  if (i.gt.10) goto 3
!  Lewis aocn 2
  if (i.eq.1) then
    if (o.eq.1) then
      cna(n) = 1.21_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.3) then
    if (o.eq.1) then
      cna(n) = 5.30_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.4) then
    if (o.eq.2) then
      cna(n) = 3.99_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.5) then
    if (o.eq.3) then
      cna(n) = 3.46_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.6) then
    if (o.eq.4) then
      cna(n) = 2.96_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.7) then
    if (o.eq.5) then
      cna(n) = 3.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.8) then
    if (o.eq.2) then
      cna(n) = 4.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.9) then
    if (o.eq.1) then
      cna(n) = 4.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
3     if (i.eq.11) then
    if (o.eq.1) then
      cna(n) = 6.70_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.12) then
    if (o.eq.2) then
      cna(n) = 5.98_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.13) then
    if (o.eq.3) then
      cna(n) = 5.27_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.14) then
    if (o.eq.4) then
      cna(n) = 4.02_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.15) then
    if (o.eq.5) then
      cna(n) = 4.01_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.16) then
    if (o.eq.4)then
      cna(n) = 3.40_dp
    elseif(o.eq.6)then
      cna(n) = 4.00_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.17) then
    if (o.eq.1)then
      cna(n) = 6.00_dp
    elseif(o.eq.7)then
      cna(n) = 4.00_dp
    else
      goto 999
    endif
    goto 1000
  endif
4     if (i.eq.19) then
    if (o.eq.1) then
      cna(n) = 9.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.20) then
    if (o.eq.2) then
      cna(n) = 7.31_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.21) then
    if (o.eq.3) then
      cna(n) = 6.18_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.22) then
    if (o.eq.4) then
      cna(n) = 5.96_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.23) then
    if (o.eq.2)then
      cna(n) = 4.00_dp
    elseif(o.eq.3)then
      cna(n) = 6.00_dp
    elseif(o.eq.4)then
      cna(n) = 5.60_dp
    elseif(o.eq.5)then
      cna(n) = 4.62_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.24) then
    if (o.eq.2)then
      cna(n) = 7.30_dp
    elseif(o.eq.3)then
      cna(n) = 6.00_dp
    elseif(o.eq.6)then
      cna(n) = 4.00_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.25) then
    if (o.eq.2)then
      cna(n) = 5.98_dp
    elseif(o.eq.3)then
      cna(n) = 5.78_dp
    elseif(o.eq.4)then
      cna(n) = 6.00_dp
    elseif(o.eq.6)then
      cna(n) = 6.00_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.26) then
    if (o.eq.2)then
      cna(n) = 5.89_dp
    elseif(o.eq.3)then
      cna(n) = 5.69_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.27) then
    if (o.eq.2)then
      cna(n) = 5.70_dp
    elseif(o.eq.3)then
      cna(n) = 5.91_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.28) then
    if (o.eq.2) then
      cna(n) = 5.90_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.29) then
    if (o.eq.1)then
      cna(n) = 2.20_dp
    elseif(o.eq.2)then
      cna(n) = 5.10_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.30) then
    if (o.eq.2) then
      cna(n) = 4.98_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.31) then
    if (o.eq.3) then
      cna(n) = 4.62_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.32) then
    if (o.eq.4) then
      cna(n) = 4.51_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.33) then
    if (o.eq.3)then
      cna(n) = 3.07_dp
    elseif(o.eq.5)then
      cna(n) = 4.41_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.34) then
    if (o.eq.4)then
      cna(n) = 3.30_dp
    elseif(o.eq.6)then
      cna(n) = 4.00_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.35) then
    if (o.eq.1) then
      cna(n) = 6.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
5     if (i.eq.37) then
    if (o.eq.1) then
      cna(n) = 9.80_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.38) then
    if (o.eq.2) then
      cna(n) = 8.57_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.39) then
    if (o.eq.3) then
      cna(n) = 7.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.40) then
    if (o.eq.4) then
      cna(n) = 6.72_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.41) then
    if (o.eq.5) then
      cna(n) = 6.07_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.42) then
    if (o.eq.6) then
      cna(n) = 4.88_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.44) then
    if (o.eq.5) then
      cna(n) = 6.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.45) then
    if (o.eq.3) then
      cna(n) = 6.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.46) then
    if (o.eq.2) then
      cna(n) = 4.40_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.47) then
    if (o.eq.1) then
      cna(n) = 5.10_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.48) then
    if (o.eq.2) then
      cna(n) = 6.14_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.49) then
    if (o.eq.1)then
      cna(n) = 6.70_dp
    elseif(o.eq.3)then
      cna(n) = 5.98_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.50) then
    if (o.eq.2)then
      cna(n) = 4.40_dp
    elseif(o.eq.4)then
      cna(n) = 5.86_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.51) then
    if (o.eq.3)then
      cna(n) = 4.80_dp
    elseif(o.eq.5)then
      cna(n) = 6.05_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.52) then
    if (o.eq.4)then
      cna(n) = 4.10_dp
    elseif(o.eq.6)then
      cna(n) = 6.00_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.53) then
    if (o.eq.1) then
      cna(n) = 6.00_dp
    elseif(o.eq.5)then
      cna(n) = 3.80_dp
    elseif(o.eq.7)then
      cna(n) = 5.60_dp
    else
      goto 999
    endif
    goto 1000
  endif
6     if (i.eq.55) then
    if (o.eq.1) then
      cna(n) = 10.4_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.56) then
    if (o.eq.2) then
      cna(n) = 10.24_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.57) then
    if (o.eq.3) then
      cna(n) = 8.50_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.73) then
    if (o.eq.5) then
      cna(n) = 6.08_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.74) then
    if (o.eq.6) then
      cna(n) = 5.60_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.75) then
    if (o.eq.5)then
      cna(n) = 6.00_dp
    elseif(o.eq.7)then
      cna(n) = 4.60_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.78) then
    if (o.eq.4) then
      cna(n) = 6.00_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.80) then
    if (o.eq.2) then
      cna(n) = 5.50_dp
      goto 1000
    else
      goto 999
    endif
  endif
  if (i.eq.81) then
    if (o.eq.1)then
      cna(n) = 7.40_dp
    elseif(o.eq.3)then
      cna(n) = 6.10_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.82) then
    if (o.eq.2) then
      cna(n) = 6.90_dp
    elseif(o.eq.4)then
      cna(n) = 5.73_dp
    else
      goto 999
    endif
    goto 1000
  endif
  if (i.eq.83) then
    if (o.eq.3) then
      cna(n) = 6.20_dp
      goto 1000
    else
      goto 999
    endif
  endif
999   continue
  call outerror('Unknown average observed coord. no. - please specify',0_i4)
  call stopnow('aocn2')
1000  continue
  if (ioproc.and.lprint) then
    write(ioout,'(''  atomic number ='',i3,''  oxidation = '',i3,''  coordination = '',f6.3)') &
      i,o,cna(n)
  endif
  return
  end
