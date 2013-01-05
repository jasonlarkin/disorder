  subroutine symmet
!***********************************************************************
!
!     Adapted from routine in CRYSTAL88 : QCPE
!
!     Generation of the space group for a three-dimensional lattice
!     Calculation of the special positions
!
!              Listing of principal variables
!              ------------------------------
!     gronam = Space group names
!     hmssg  = Hermann-Mauguin Symbol of Space Group
!     nspcg  = International Tables Number for Space Group
!     iflags = Integer flag for Group Symbol
!     ifhr   = Integer flag for Hexagonal or Rhombohedral Cell
!     ifso   = Integer flag for Shift of the Origin
!     ivso   = Integer value for Shift of the Origin
!     ishorg = Integer shift of the origin
!     ngo    = Number of Group Operators
!     nccs   = Numeric Code for Crystal System
!     ncbl   = Numeric Code for Bravais Lattice
!     ncpg   = Numeric Code for Point Group
!     r      = Rotation matrices
!     it     = Integer Translations (multiplied by 24)
!     tmat   = Translation matrices
!     ige    = Rotation parts needed for generation
!     ket    = Translation parts needed for generation
!     vit    = Translations (it divided by 24)
!
!******************************************************************
  use control
  use current
  use iochannels
  use numbers
  use parallel
  use symmetry
  use times
  implicit none
!
!  Local variables
!
  character(len=16)             :: gronam2(17)
  character(len=16)             :: ognam(92)
  character(len=5)              :: tradd(24)
  character(len=1)              :: hbr(8),hs(15),hmssg2(16)
  integer(i4)                   :: i
  integer(i4)                   :: id
  integer(i4)                   :: iflags2
  integer(i4)                   :: ifso2
  integer(i4)                   :: ige(3,3,36)
  integer(i4)                   :: ii
  integer(i4)                   :: il
  integer(i4)                   :: isc(3,3)
  integer(i4)                   :: iss(59)
  integer(i4)                   :: in(3,4)
  integer(i4)                   :: indgro2(17)
  integer(i4)                   :: iognam(92)
  integer(i4)                   :: iogperm(92)
  integer(i4),             save :: iq(3,4,5)
  integer(i4)                   :: iqdd(60)
  integer(i4)                   :: is(3)
  integer(i4)                   :: it1
  integer(i4)                   :: it2
  integer(i4)                   :: it3
  integer(i4)                   :: j
  integer(i4)                   :: j1
  integer(i4)                   :: j2
  integer(i4)                   :: j3
  integer(i4)                   :: j4
  integer(i4)                   :: jj
  integer(i4)                   :: jl
  integer(i4)                   :: js(3)
  integer(i4)                   :: k
  integer(i4)                   :: k1
  integer(i4)                   :: k2
  integer(i4)                   :: k3
  integer(i4)                   :: k4
  integer(i4)                   :: ket(3,34)
  integer(i4)                   :: kk
  integer(i4)                   :: kl
  integer(i4)                   :: l
  integer(i4)                   :: lf(216)
  integer(i4)                   :: lgn(3,45)
  integer(i4)                   :: lgt(48)
  integer(i4)                   :: lsh(2,78)
  integer(i4)                   :: lt
  integer(i4)                   :: m
  integer(i4)                   :: mcc
  integer(i4)                   :: mst1(3,6)
  integer(i4)                   :: mst2(3,6)
  integer(i4)                   :: mv
  integer(i4)                   :: n
  integer(i4),             save :: ncfold = 0
  integer(i4)                   :: nd
  integer(i4)                   :: ne
  integer(i4)                   :: net(3)
  integer(i4)                   :: ngt(48,48)
  integer(i4)                   :: nrt
  integer(i4)                   :: nspcg2
  real(dp)                      :: cputime
  real(dp)                      :: pm(3,3)
  real(dp)                      :: r24
  real(dp)                      :: rm(3,3)
  real(dp)                      :: time1
  real(dp)                      :: time2
  real(dp)                      :: wdat(126)
!
  equivalence (j2,in(2,2)),(j3,in(2,3)),(k2,in(3,2)),(k3,in(3,3)), &
              (j1,in(2,1)),(k1,in(3,1)),(j4,in(2,4)),(k4,in(3,4))
!
  data iqdd/4*0,2*12,9*0,12,0,12,9*0,2*12,11*0,3*12,0,3*12,4*0,3*12,6*0/
  data hbr/'P','A','B','C','F','I','R',' '/
  data hs/'M','A','B','C','N','D','-','0','1','2','3','4','5','6','/'/
  data tradd/'    0',' 1/24',' 1/12','  1/8','  1/6',' 5/24','  1/4', &
    ' 7/24','  1/3','  3/8',' 5/12','11/24','  1/2','13/24',' 7/12', &
    ' 5/8','  2/3','17/24','  3/4','19/24','  5/6','  7/8','11/12', &
    '23/24'/
  data wdat/1.,1.,.5,.5,.0,-.5,2.,3*.0,3*.5,1.,2*.0,-.5,.0,2*.5,1., &
    3*.0,-.5,2*.5,-1., 1.,.5,1.,.5,.0,-.5,1., .0,.5,2*.0,2*.5,1., &
    2*.0,.5,.0,2*.5,-1.,.0,-.5,2*.0,2*.5,-2.,1.,.5,.5,1.,.0,-.5,1., &
    2*1.,0.,1.,-1.,0.,1.,2*0.,1.,-1.,2*1.,-1., 2*0.,1.,0.,2*1.,0., &
    2*0.,4*1.,0.,2*1.,0.,1.,-1.,0.,1., 0.,-1.,1.,0.,2*1.,-1., &
    2*0.,1.,0.,2*1.,1.,0.,2*1.,0.,2*1.,1., 2*1.,0.,1.,-1.,0.,1./
  data mst1/3*1,0,2,2,3,0,3,4,4,0,5,6,7,9,10,11/
  data mst2/3*1,2,0,0,3,3,0,0,4,4,7,5,8,11,0,12/
  data lsh/2,26,6,11,5,15,7,26,435,28,603,26,453,28,645,15,747,26, &
    771,15,2287,30,2383,32,2299,31,1622,11,899,9,1319,10,1209,10,1749, &
    11,954,9,1601,11,898,9,1221,10,1770,11,955,9,1307,10,2401,13,2547, &
    13,2421,13,5105,16,7303,15,7205,15,29187,12,29687,12,31662,12,13721, &
    21,13202,11,13210,21,13691,22,17219,21,17221,15,17223,23,17227, &
    13,17229,15,17231,24,17721,24,14137,21,14147,11,14164,21,14170, &
    21,14638,3,14689,25,14692,26,15183,15,15160,21,15185,13,16229,15, &
    16228,11,16230,12,16729,15,15708,34,17122,3,17149,3,17102,21,17129, &
    21,17120,11,17148,11,17155,3,17132,3,17135,21,17112,11,17153,11, &
    17131,11,17603,3,17659,29,17662,15,5204,14,5104,14,5205,16/
  data lgn/1,0,0,2,0,0,2,0,0,2,0,0,2,0,0,2,0,0,2,0,0,3,4,0,3,4,0, &
    3,4,0,3,4,0,3,0,0,3,0,0,5,7,0,5,3,0,5,3,0,3,5,0,2,0,0,4,2,0,4,2,0, &
    4,2,0,4,2,0,5,0,0,5,0,0,7,10,0,7,4,0,7,4,0,7,10,0,2,5,0,5,14,0, &
    5,14,0,2,0,0,4,2,0,4,2,0,4,2,0,7,8,6,5,0,0,6,3,0,10,11,13,10,8,0, &
    10,8,0,14,19,22,8,5,0,26,37,5,14,5,0/
  data lf/1,1,-2,1,-3,1,-4,1,2,1,3,1,4,1,2,-3,-4,1,3,-2,-4,1,4,-2,-3, &
    1,4,2,3,1,4,-15,-14,1,4,15,14,1,4,15,14,-2,-3,-13,-16,1,4,2,3,-13, &
    -16,-14,-15,1,4,13,16,-2,-3,-14,-15,1,4,2,3,13,16,14,15,1,3,5,1,3, &
    5,-9,-11,-7,1,3,5,-10,-8,-12,1,3,5,9,11,7,1,3,5,10,8,12,1,3,5,-4, &
    -2,-6,1,4,3,5,2,6,1,4,3,5,2,6,-9,-11,-7,-10,-8,-12,1,3,5,10,8,12,-9, &
    -11,-7,-4,-2,-6,1,3,5,9,11,7,-10,-8,-12,-4,-2,-6,1,4,3,5,2,6,9,11, &
    7,10,8,12,1,4,2,3,5,9,7,12,8,10,6,11,1,4,2,3,5,9,7,12,8,10,6,11, &
    -13,-16,-14,-15,-17,-21,-19,-24,-20,-22,-18,-23,1,4,2,3,5,9,7,12,8, &
    10,6,11,13,16,14,15,17,21,19,24,20,22,18,23/
  data ige/1,3*0,1,3*0,1,1,3*0,-1,3*0,2*-1,3*0,1,3*0,2*-1,3*0, &
    -1,3*0,1,0,1,3*0,1,1,3*0,1,3*0,2*-1,3*0,-1,3*0,1,-1,3*0,-1,3*0,-1, &
    1,4*0,1,1,3*0,1,3*0,1,-1,3*0,-1,3*0,-1,1,3*0,-1,3*0,2*-1,3*0, &
    1,2*0,-1,0,-1,4*0,-1,0,-1,0,1,4*0,1,0,1,0,-1,4*0,1,0,1,0,1,4*0, &
    2*-1,4*0,-1,0,-1,0,-1,4*0,1,0,1,0,1,4*0,-1,0,1,0,1,4*0,1,0,-1, &
    3*0,-1,0,-1,0,-1,4*0,-1,0,1,0,1,4*0,1,0,-1,0,1,4*0,1,0,1,0,-1,0,0, &
    1,3*0,1,3*0,1,1,1,0,-1,4*0,1,0,1,0,2*-1,3*0,1,-1,3*0,-1,3*0,1,2*-1, &
    0,1,4*0,1,0,-1,0,1,1,3*0,1,0,1,0,1,4*0,-1,1,1,0,0,-1,3*0,-1,1,0, &
    0,2*-1,3*0,-1,0,-1,0,-1,4*0,3*-1,0,0,1,3*0,2*-1,0,0,1,1,3*0,-1/
  data iss/111,711,171,117,211,121,112,277,727,772,222,-389,411,477, &
    -373,-328,422,311,371,317,321,312,-589,611,677,-528,-573,622,231,-363, &
    432,-89,218,128,119,777,-289,418,484,-229,-283,684,618,737,731, &
    1,5,6,7,11,18,13,17,21,22,28,24,31,29/
  data ket/3*0,12,3*0,12,3*0,12,0,3*12,0,3*12,0,3*12,0,3*6,0,3*6, &
    0,3*6,18,2*6,0,0,4,0,0,6,0,0,8,0,0,12,0,0,16,0,0,18,0,0,20,18,6, &
    0,18,12,6,18,6,3,18,6,9,0,18,0,0,6,0,0,6,3,6,0,0,0,12,6,9,9,0,9, &
    0,9,0,9,9,6,18,18,0,18,9/
  data (gronam2(i),i=1,17)/'P M -3          ', &
    'P N -3          ', 'F M -3          ', 'F D -3          ', &
    'I M -3          ', 'P A -3          ', 'I A -3          ', &
    'P M -3 M        ', 'P N -3 N        ', 'P M -3 N        ', &
    'P N -3 M        ', 'F M -3 M        ', 'F M -3 C        ', &
    'F D -3 M        ', 'F D -3 C        ', 'I M -3 M        ', &
    'I A -3 D        '/
  data (ognam(i),i=1,92)/'P M A A ','P B M B ','P N C B ', &
    'P C N A ','P M M B ','P B M M ','P C M M ','P M C M ', &
    'P M A M ','P N N B ','P B N N ','P C N N ','P N C N ', &
    'P N A N ','P N M B ','P B M N ','P C N M ','P N C M ', &
    'P M A N ','P C C B ','P B A A ','P C A A ','P B C B ', &
    'P B A B ','P M C B ','P C M A ','P N A A ','P B N B ', &
    'P C A M ','P M C A ','P M A B ','P B M A ','P C M B ', &
    'P M N N ','P N M N ','P N M M ','P M N M ','P C A N ', &
    'P N C A ','P N A B ','P B N A ','P C N B ','P C A B ', &
    'P M N B ','P B N M ','P C M N ','P M C N ','P N A M ', &
    'C C M M ','A M M A ','A M A M ','B B M M ','B M M B ', &
    'C C M B ','A B M A ','A C A M ','B B C M ','B M A B ', &
    'A M M M ','B M M M ','A M A A ','B B M B ','C M M B ', &
    'A B M M ','A C M M ','B M C M ','B M A M ','C C C B ', &
    'A B A A ','A C A A ','B B C B ','B B A B ', &
    'P 4/M 2/M 2/M ','P 4/M 2/C 2/C ','P 4/N 2/B 2/M ','P 4/N 2/N 2/C ', &
    'P 4/M 21/B 2/M','P 4/M 21/N 2/C','P 4/N 21/M 2/M','P 4/N 21/C 2/C', &
    'P 42/M 2/M 2/C','P 42/M 2/C 2/M','P 42/N 2/B 2/C','P 42/N 2/N 2/M', &
    'P 42/M 21/B 2/C','P 42/M 21/N 2/M','P 42/N 21/M 2/C','P 42/N 21/C 2/M', &
    'I 4/M 2/M 2/M ','I 4/M 2/C 2/M ','I 41/A 2/M 2/D','I 41/A 2/C 2/D'/
  data (indgro2(i),i=1,17)/200,201,202,203,204,205,206,221,222,223,224,225,226,227,228,229,230/
  data iognam/2*49,2*50,5*51,5*52,5*53,5*54,2*55,2*56,5*57,2*58, &
    2*59,5*60,1*61,5*62,5*63,5*64,2*65,2*66,5*67,5*68,123,124,125,126,127,128,129,130,131,132, &
    133,134,135,136,137,138,139,140,141,142/
  data iogperm/3,5,3,5,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,3,5,3,5,2,3,4,5,6,3,5,3,5,2,3,4, &
               5,6,2,2,3,4,5,6,2,3,4,5,6,2,3,4,5,6,3,5,3,5,2,3,4,5,6,2,3,4,5,6,20*1/
!
!  Copy centring matrices into w/w1 arrays
!
  ii = 0
  do i = 1,3
    do j = 1,3
      do k = 1,7
        ii = ii+1
        w(k,j,i) = wdat(ii)
      enddo
    enddo
  enddo
  do i = 1,3
    do j = 1,3
      do k = 1,7
        ii = ii + 1
        w1(k,j,i) = wdat(ii)
      enddo
    enddo
  enddo
!
!  If operators were specified manually, then copy into current configuration workspace and return
!
  if (ngocfg(ncf).gt.1) then
    ncbl = 1
    ngo = ngocfg(ncf)
    nccs = nccscfg(ncf)
    do i = 1,ngo
      do j = 1,3
        rop(1,j,i) = ropcfg(1,j,i,ncf)
        rop(2,j,i) = ropcfg(2,j,i,ncf)
        rop(3,j,i) = ropcfg(3,j,i,ncf)
      enddo
      vit(1,i) = vitcfg(1,i,ncf)
      vit(2,i) = vitcfg(2,i,ncf)
      vit(3,i) = vitcfg(3,i,ncf)
    enddo
    return
  endif
!
  time1 = cputime()
!
!  Set local variables
!
  iflags2 = iflags(ncf)
  ifso2 = ifso(ncf)
  nspcg2 = nspcg(ncf)
  if (iperm(ncf).gt.1) then
    do j = 1,16
      hmssg2(j) = gronam(nspcg2)(j:j)
    enddo
  else
    do j = 1,16
      hmssg2(j) = hmssg(j,ncf)
    enddo
  endif
  if (ncf.ne.ncfold) lalter = .false.
  ncfold = ncf
!
  ii = 0
  do i = 1,5
    do j = 1,4
      do k = 1,3
        ii = ii + 1
        iq(k,j,i) = iqdd(ii)
      enddo
    enddo
  enddo
  do i = 1,24
    trax(i) = tradd(i)
  enddo
  n = 1
  k = 1
  nccs = 2
  id = 0
  il = 0
  lt = 0
  nrt = 0
  mcc = 0
  ncs = 0
  ngo = 0
  do i = 1,3
    js(i) = 0
    in(i,1) = 0
    in(i,2) = 0
    in(i,3) = 0
    in(i,4) = 0
    is(i) = 1
    net(i) = 1
    do j = 1,48
      it(i,j) = 0
    enddo
  enddo
  do j = 1,48
    lgt(j) = 0
  enddo
!
!  Division by 3 of W matrix for Rhombohedral cell
!
  do i = 1,3
    do j = 1,3
      w(7,j,i) = third*w(7,j,i)
    enddo
  enddo
!
!  Determine Bravais lattice and crystal family
!
  if (iflags2.eq.0) goto 577
  goto 270   
!
!  Space group number input case
!
577   if (nspcg2.eq.0) nspcg2 = 1
  if (nspcg2.gt.232.or.nspcg2.le.0) then
    call outerror('space group number outside range',0_i4)
    call stopnow('symmet')
  endif
  do i = 1,16
    hmssg2(i) = gronam(nspcg2)(i:i)
  enddo
!
!  Determine space group number
!
270   if (nspcg2.le.1) then
    do i = 1,232
      do j = 1,16
        if (hmssg2(j).ne.gronam(i)(j:j)) goto 345
      enddo
      nspcg2 = i
      nspcg(ncf) = i
      goto 347
345   continue
    enddo
!
!  Check alternative symbols
!
    do i = 1,17
      do j = 1,16
        if (hmssg2(j).ne.gronam2(i)(j:j)) goto 346
      enddo
      nspcg2 = indgro2(i)
      nspcg(ncf) = nspcg2
      do j = 1,16
        hmssg2(j) = gronam(nspcg2)(j:j)
      enddo
      goto 347
346     continue
    enddo
!
!  Check alternative orthorhombic symbols
!
    do i = 1,92
      do j = 1,16
        if (hmssg2(j).ne.ognam(i)(j:j)) goto 348
      enddo
      nspcg2 = iognam(i)
      iperm(ncf) = iogperm(i)
      nspcg(ncf) = nspcg2
      if (i.le.48.or.i.gt.72) then
        do j = 1,16
          hmssg2(j) = gronam(nspcg2)(j:j)
        enddo
      else
        iperm(ncf) = 1
      endif
      lalter = .true.
      goto 347
348   continue
    enddo
    call settings(hmssg2,gronam,hbr,nspcg2)
    if (nspcg2.gt.0) then
      lalter = .true.
      nspcg(ncf) = nspcg2
      goto 347
    endif
    if (ioproc) then
      write(ioout,'(/,''  **** Invalid space group symbol supplied : '',16a1,'' ****'',/)')(hmssg2(i),i = 1,16)
    endif
    call stopnow('symmet')
347 continue
  endif
!
!  Start processing symbol
!
  kk = 0
275   kk = kk + 1
  if (kk.gt.14) then
    if (ioproc) then
      write(ioout,'('' **** Too many blanks before space group symbol ****'')')
    endif
    call stopnow('symmet')
  endif
  if (hmssg2(kk).eq.hbr(8)) goto 275
  do i = kk,16
    hmssg2(i-kk+1) = hmssg2(i)
  enddo
  do i = 18-kk,16
    hmssg2(i) = hbr(8)
  enddo
!
!  Remove excess blanks from symbol
!
  do i = 3,15
    if (hmssg2(i-1).eq.hbr(8).and.hmssg2(i).eq.hbr(8).and.hmssg2(i+1).ne.hbr(8)) then
      do j = i,15
        hmssg2(j) = hmssg2(j+1)
      enddo
    endif
  enddo
  do i = 1,7
    if (hmssg2(1).eq.hbr(i)) ncbl = i
  enddo
  if (ncbl.ne.7.and.ifhr(ncf).ne.0) ifhr(ncf) = 0
!
!  If B centred then the W1 matrix must be corrected
!
  if (ncbl.eq.3) then
    w1(3,1,1) = 1.0_dp
    w1(3,2,1) = 0.0_dp
    w1(3,3,1) = 1.0_dp
    w1(3,1,2) = 0.0_dp
    w1(3,2,2) = 1.0_dp
    w1(3,3,2) = 0.0_dp
    w1(3,1,3) = -1.0_dp
    w1(3,2,3) = 0.0_dp
    w1(3,3,3) = 1.0_dp
  endif
!
!  Standardisation of the symbol
!
  do i = 3,16
    if (hmssg2(i).eq.hbr(8)) then
      k = 0
      n = n + 1
    else
      if (hmssg2(i).eq.hs(11).or.hmssg2(i).eq.hs(14)) nccs = 5
      do j = 1,15
        if (hmssg2(i).eq.hs(j))in(n,k) = j-8
      enddo
    endif
    k = k+1
  enddo
  if (in(1,1).eq.4.or.in(1,1).eq.-1.and.in(1,2).eq.4) nccs = 4
  if (j1.eq.3) nccs = 6
  if (j1.eq.0.and.in(1,1).eq.1.or.in(1,1).eq.-1.and.in(1,2).eq.1) nccs = 1
  if (nccs.ne.2.or.j1.ne.0) then
    if (nccs.eq.2.and.in(1,1).ne.1.and.j1.ne.1.and.k1.ne.1) nccs = 3
  else
    in(2,1) = in(1,1)
    in(1,1) = 0
    in(2,2) = in(1,2)
    in(1,2) = 0
    in(2,3) = in(1,3)
    in(1,3) = 0
    in(2,4) = in(1,4)
    in(1,4) = 0
    in(1,1) = 1
    k1 = 1
  endif
!
!  Determination of code no. ncpg for the point group
!
  do i = 1,3
    if (in(i,1).ne.0)is(i) = in(i,1)
    if (is(i).eq.-1)is(i) = -in(i,2)
    if (in(i,1).lt.-1)is(i) = 7
    if (in(i,1).le.-2)then
      in(i,4) = in(i,1)
      in(i,1) = 1
    endif
    if (in(i,2).eq.7)then
      in(i,4) = in(i,3)
      in(i,3) = 7
      in(i,2) = 0
    endif
  enddo
  if (k1.eq.2.and.(j1.eq.2.or.j1.eq.3)) nrt = 1
  m = 100*is(1)+10*is(2)+is(3)+in(1,3)+j3+k3
  do i = 1,45
    if (m.eq.iss(i)) ncpg = i
  enddo
!
!  Trap case if ncpg is zero
!
  if (ncpg.eq.0) then
    call outerror('Problem with space group symbol - ncpg is zero',0_i4)
    call stopnow('symmet')
  endif
!
!  Fix code no. ncs for centrosymmetry
!
  if (ncpg.le.31) then
    ncs = 1
    nd = ncpg
  else
    nd = iss(ncpg+14)
  endif
!
!  Rotation parts
!
  do i = 1,216
    if (lf(i).eq.1) id = id + 1
    if (id-nd.gt.0) then
      goto 420
    elseif (id-nd.eq.0) then
      m = abs(lf(i))
      if (nccs.eq.5) m = m + 24
      ngo = ngo + 1
      do k = 1,3
        do l = 1,3
          rop(l,k,ngo) = ige(l,k,m)*sign(1_i4,lf(i))
        enddo
      enddo
    endif
  enddo
!
!  Multiplication table
!
420 continue
  do i = 1,ngo
    do j = 1,3
      do k = 1,3
        if (ncs.eq.0) rop(k,j,ngo+i) = - rop(k,j,i)
      enddo
    enddo
    do 480 j = 1,ngo
      do k = 1,3
        do l = 1,3
          isc(l,k) = 0
        enddo
      enddo
      do k = 1,3
        do l = 1,3
          do m = 1,3
            isc(k,l) = isc(k,l) + rop(k,m,i)*rop(m,l,j)
          enddo
        enddo
      enddo
      do 470 k = 1,ngo
        do l = 1,3
          do m = 1,3
            if (isc(m,l).ne.rop(m,l,k)) goto 470
          enddo
        enddo
        ngt(i,j) = k
        if (ncs.eq.0) then
          ngt(ngo+i,j) = k + ngo
          ngt(i,ngo+j) = k + ngo
          ngt(ngo+i,ngo+j) = k
        endif
        goto 480
470   continue
480 continue
  enddo
  if (ncs.eq.0) ngo = ngo + ngo
!
!  Select translation parts for generators
!
  if (ncpg.eq.30.or.ncpg.eq.31) il = 1
  if (nccs.eq.3) mcc = 400*ncbl + in(1,4)*49 + j4*7 + k4 + 399
  if (nccs.ge.4) mcc = 1000*nd + 100*ncbl + 25*((k4+7)/2) + 5*in(1,4) + j4+42+4*in(1,2)+j2*2
  if (nccs.eq.5) mcc = 3000+100*ncpg + in(1,2) + j1 + k1
  if (ncpg.eq.11) mcc = in(1,2)*4 + j2*2 + k2
  do i = 1,3
    l = in(i,4) + 8
    if (l.lt.7) then
      il = il+1
      net(il) = mst1(i,l)
      if (nccs.gt.3)net(il) = mst2(i,l)
    endif
  enddo
  if (ncpg.eq.30.and.k4.eq.-2) net(il) = 13
  if (il.lt.2) then
    do i = 1,3
      if (in(i,1).eq.2) then
        il = il+1
        net(il) = 1 + i*in(i,2)
        if (nccs.gt.3.and.in(i,1).ne.-1) net(il) = 1 + in(i,2)
        if (nccs.eq.6) net(il) = 1 + 5*in(i,2)
      endif
    enddo
    if (in(1,1).ge.3.and.in(1,1).le.6.and.in(1,2).ne.0) then
      il = il + 1
      net(il) = 13+(10*in(1,2))/in(1,1)-(2*in(1,2))/in(1,1)
    endif
  endif
  n = net(3)
  if (ncpg.eq.31) n = 1 + 7*(in(1,2)/2)+in(1,2)/3+in(1,2)*(10*in(1,2)*in(1,2)-42*in(1,2)+43+mod(ncbl,5_i4))
  do i = 1,3
    id = lgn(i,ncpg)
    if (id.eq.0) goto 530
    m = net(i)
    do j = 1,3
      it(j,id) = ket(j,m) - nrt*(i/2)*(2/i)*(1-2*(nccs/6))*ket(j,n)
      if (nrt.eq.0.and.nccs.lt.4.and.ncbl.ne.5) js(j) = ket(j,m)/2+js(j)
    enddo
  enddo
!
!  Complete translation parts
!
530   do i = 1,3
    if (lgn(i,ncpg).ne.0) then
      lt = lt + 1
      lgt(lt) = lgn(i,ncpg)
    endif
  enddo
550 if (lt.eq.ngo) goto 588
  n = 0
  do i = 1,lt
    do j = 1,lt
      il = lgt(i)
      jl = lgt(j)
      kl = ngt(il,jl)
      do m = 1,ngo
        if (lgt(m).eq.kl) goto 590
      enddo
      do k = 1,3
        it(k,kl) = it(k,kl) + rop(k,1,il)*it(1,jl) + rop(k,2,il)*it(2,jl) + rop(k,3,il)*it(3,jl)
        it(k,kl) = it(k,kl) + it(k,il) + 48
        it(k,kl) = mod(it(k,kl),24_i4)
      enddo
      n = n + 1
      lgt(lt+n) = kl
590   continue
    enddo
  enddo
  lt = lt + n
  goto 550
!****************************
!  Selection of the origin  *
!****************************
588 if (abs(ishorg(1,nspcg2))+abs(ishorg(2,nspcg2))+abs(ishorg(3,nspcg2)).gt.0) then
    if (ifso2.eq.1) goto 620
  else
    if (ifso2.eq.0) goto 620
  endif
  if (ifso2.gt.1) then
    js(1) = js(1) + ivso(1,ncf)
    js(2) = js(2) + ivso(2,ncf)
    js(3) = js(3) + ivso(3,ncf)
  elseif (nspcg2.gt.0) then
    js(1) = js(1) + ishorg(1,nspcg2)
    js(2) = js(2) + ishorg(2,nspcg2)
    js(3) = js(3) + ishorg(3,nspcg2)
  endif
620 n = 0
  do i = 1,78
    if (mcc.eq.lsh(1,i)) n = lsh(2,i)
  enddo
  if (n.ne.0) then
    js(1) = js(1) + ket(1,n)
    js(2) = js(2) + ket(2,n)
    js(3) = js(3) + ket(3,n)
  endif
  do i = 1,ngo
    do j = 1,3
      it(j,i) = it(j,i) + rop(j,1,i)*js(1) + rop(j,2,i)*js(2) + rop(j,3,i)*js(3) - js(j) + 48
      it(j,i) = mod(it(j,i),24_i4)
    enddo
    if (ncbl.ne.1.and.ncbl.ne.7) then
      nd = 72
      do l = 1,4
        ne = 0
        do k = 1,3
          is(k) = it(k,i) + iq(k,l,ncbl-1)
          ne = ne + mod(is(k),24_i4)
        enddo
        if (nd.gt.ne) then
          nd = ne
          it(1,i) = mod(is(1),24_i4)
          it(2,i) = mod(is(2),24_i4)
          it(3,i) = mod(is(3),24_i4)
        endif
      enddo
    endif
  enddo
  if (iperm(ncf).le.1) then
    do j = 1,16
      hmssg(j,ncf) = hmssg2(j)
    enddo
  else
!
!  Permutate operators to correct setting
!
    do ii = 1,3
      pm(1,ii) = 0.0_dp
      pm(2,ii) = 0.0_dp
      pm(3,ii) = 0.0_dp
    enddo
    if (iperm(ncf).eq.2) then
      pm(1,2) = 1.0_dp
      pm(2,1) = 1.0_dp
      pm(3,3) = 1.0_dp
    elseif (iperm(ncf).eq.3) then
      pm(1,3) = 1.0_dp
      pm(2,1) = 1.0_dp
      pm(3,2) = 1.0_dp
    elseif (iperm(ncf).eq.4) then
      pm(1,3) = 1.0_dp
      pm(2,2) = 1.0_dp
      pm(3,1) = 1.0_dp
    elseif (iperm(ncf).eq.5) then
      pm(1,2) = 1.0_dp
      pm(2,3) = 1.0_dp
      pm(3,1) = 1.0_dp
    elseif (iperm(ncf).eq.6) then
      pm(1,1) = 1.0_dp
      pm(3,2) = 1.0_dp
      pm(2,3) = 1.0_dp
    endif
!
!  Permute shifts
!
    do i = 1,ngo
      it1 = it(1,i)
      it2 = it(2,i)
      it3 = it(3,i)
      it(1,i) = nint(it1*pm(1,1)+it2*pm(1,2)+it3*pm(1,3))
      it(2,i) = nint(it1*pm(2,1)+it2*pm(2,2)+it3*pm(2,3))
      it(3,i) = nint(it1*pm(3,1)+it2*pm(3,2)+it3*pm(3,3))
    enddo
!
!  Permute rotations
!
    do i = 1,ngo
      do ii = 1,3
        do jj = 1,3
          rm(jj,ii) = 0.0_dp
          do kk = 1,3
            rm(jj,ii) = rm(jj,ii) + pm(jj,kk)*rop(kk,ii,i)
          enddo
        enddo
      enddo
      do ii = 1,3
        do jj = 1,3
          rop(jj,ii,i) = 0.0_dp
          do kk = 1,3
            rop(jj,ii,i) = rop(jj,ii,i) + rm(jj,kk)*pm(ii,kk)
          enddo
        enddo
      enddo
    enddo
  endif
!
!  Divide it by 24
!
  r24 = 1.0_dp/24.0_dp
  do mv = 1,ngo
    vit(1,mv) = it(1,mv)*r24
    vit(2,mv) = it(2,mv)*r24
    vit(3,mv) = it(3,mv)*r24
  enddo
!
!  Debugging print statements!
!
  if (index(keyword,'opera').ne.0.and.ioproc) then
    write(ioout,'(/,''  Symmetry operators :'')')
    do i = 1,ngo
      write(ioout,'(/,''  Operator no.  =  '',i2,/)') i
      write(ioout,'(3f5.1,f6.2)') rop(1,1,i),rop(2,1,i),rop(3,1,i),vit(1,i)
      write(ioout,'(3f5.1,f6.2)') rop(1,2,i),rop(2,2,i),rop(3,2,i),vit(2,i)
      write(ioout,'(3f5.1,f6.2)') rop(1,3,i),rop(2,3,i),rop(3,3,i),vit(3,i)
    enddo
  endif
  time2 = cputime()
  tsym = tsym + time2 - time1
!
  return
  end
