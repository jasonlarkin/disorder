  subroutine outlammps(iout,itmp)
!
!  Subroutine for outputing input in Lammps format
!
!  NB: Currently designed to work with a single configuration
!
!  10/12 Created
!  11/12 Species numbers ordered so that they are smallest
!        first for pair potentials
!  11/12 Modified to handle charmm style and overlay/hybrid
!        potentials
!   5/13 Names of potentials changed to match Paolo's alterations to Lammps
!   5/13 rpot2 replaced with tapermin in pairwise potentials
!   5/13 Coulomb interaction now given even when potentials are present
!   5/13 Handling of 1-4 scaling factors added in special_bonds output
!   5/13 Conversion of Lennard-Jones A B form to epsilon/sigma removed
!   5/13 Output of improper dihedrals added
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
!  Julian Gale, NRI, Curtin University, May 2013
!
  use current
  use element,        only : atmass
  use files
  use four,           only : nfor, nforty, forpoly, npfor, fork
  use molecule,       only : natmol
  use numbers,        only : sixth
  use potentialnames, only : maxpottype
  use species,        only : nspec, natspec
  use m_three,        only : nthb, thbk, theta, nthrty
  use two
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in) :: iout
  integer(i4),      intent(in) :: itmp
!
!  Local variables
!
  character(len=80)                    :: lammpsinp
  character(len=80)                    :: line
  character(len=20)                    :: sym1
  character(len=20)                    :: sym2
  logical                              :: leof
  logical                              :: lfirst
  logical                              :: lmultipot
  logical                              :: lok1
  logical                              :: lok2
  logical                              :: ltype01
  logical                              :: ltype02
  integer(i4)                          :: i
  integer(i4)                          :: iline
  integer(i4)                          :: ind
  integer(i4)                          :: isp1
  integer(i4)                          :: isp2
  integer(i4)                          :: n
  integer(i4)                          :: ni
  integer(i4)                          :: nangtot
  integer(i4)                          :: nbontot
  integer(i4)                          :: nbptype
  integer(i4),       allocatable, save :: nbptypeptr(:)
  integer(i4)                          :: nfound
  integer(i4),       allocatable, save :: npotokptr(:)
  integer(i4),       allocatable, save :: npotspec1(:)
  integer(i4),       allocatable, save :: npotspec2(:)
  integer(i4),       allocatable, save :: npotspecpair(:)
  integer(i4)                          :: npottype
  integer(i4),       allocatable, save :: npottypes(:)
  integer(i4)                          :: nphitot
  integer(i4)                          :: npoteok
  integer(i4)                          :: nspec21
  real(dp)                             :: eps
  real(dp)                             :: phi
  real(dp)                             :: sig
  real(dp)                             :: scale14_coul
  real(dp)                             :: scale14_two
!
!  If name has been given then open file
!
  if (lammpsfile(1:1).ne.' ') then
    open(iout,file=lammpsfile,status='unknown')
    open(itmp,file=lammpsfile//"tmp",status='unknown')
  endif
!
!  Allocate local memory
!
  allocate(nbptypeptr(npote))
!***********
!  Header  *
!***********
  write(iout,'(''  LAMMPS description'')')
  write(iout,'('' '')')
!********************
!  Cell parameters  *
!********************
  write(itmp,'(7x,''0'',2x,f10.6,5x,''xlo xhi'')') rv(1,1)
  write(itmp,'(7x,''0'',2x,f10.6,5x,''ylo yhi'')') rv(2,2)
  write(itmp,'(7x,''0'',2x,f10.6,5x,''zlo zhi'')') rv(3,3)
  write(itmp,'(f11.7,2(4x,f11.7),5x,''xy xz yz'')') rv(1,2),rv(1,3),rv(2,3)
  write(itmp,'('' '')')
!**********************
!  Masses of species  *
!**********************
  write(itmp,'(2x,''Masses'')')
  write(itmp,'('' '')')
  do i = 1,nspec
    ni = natspec(i)
    if (atmass(ni).ge.100.0_dp) then
      write(itmp,'(i8,3x,f9.5)') i,atmass(ni)
    elseif (atmass(ni).ge.10.0_dp) then
      write(itmp,'(i8,3x,f9.6)') i,atmass(ni)
    elseif (atmass(ni).ge.1.0_dp) then
      write(itmp,'(i8,3x,f9.7)') i,atmass(ni)
    endif
  enddo
  write(itmp,'('' '')')
!**********
!  Atoms  *
!**********
  write(itmp,'(3x,''Atoms'',/)')
  do i = 1,numat
    ni = nrelat(i)
    write(itmp,'(3i8,4f12.6)') i,natmol(i),nspecptr(ni),qf(i),xclat(i),yclat(i),zclat(i)
  enddo
  write(itmp,'('' '')')
!**********
!  Bonds  *
!**********
  write(itmp,'(3x,''Bonds'',/)')
  call bondpot(itmp,nbontot,nbptype,nbptypeptr)
  write(itmp,'('' '')')
!***********
!  Angles  *
!***********
  write(itmp,'(3x,''Angles'',/)')
  call angle(itmp,1_i4,nangtot)
  write(itmp,'('' '')')
!**************
!  Dihedrals  *
!**************
  write(itmp,'(3x,''Dihedrals'',/)')
  call torsion(itmp,1_i4,nphitot)
  write(itmp,'('' '')')
!******************
!  Total numbers  *
!******************
  write(iout,'(i8,3x,''atoms'')') numat
  write(iout,'(i8,3x,''bonds'')') nbontot
  write(iout,'(i8,3x,''angles'')') nangtot
  write(iout,'(i8,3x,''dihedrals'')') nphitot
  write(iout,'('' '')')
!*****************
!  Type numbers  *
!*****************
  write(iout,'(i8,3x,''atom types'')') nspec
  write(iout,'(i8,3x,''bond types'')') nbptype
  write(iout,'(i8,3x,''angle types'')') nthb
  write(iout,'(i8,3x,''dihedral types'')') nfor
  write(iout,'('' '')')
!
!  Rewind itmp
!
  rewind(itmp)
!
!  Now read and append itmp on iout
!
  leof = .false.
  do while (.not.leof)
    read(itmp,'(a)',err=100,end=100) line
    write(iout,'(a)') trim(line)
  enddo
100 continue
!
!  Close files
!
  close(iout)
  close(itmp,status='delete')
!**************************************
!  Generate matching lammps.inp file  *
!**************************************
  lammpsinp = lammpsfile
  if (lammpsinp(1:1).ne.' ') then
    ind = index(lammpsinp,'.lmp')
    if (ind.gt.0) then
      lammpsinp(ind:ind+3) = '.inp'
    else 
      ind = index(lammpsinp,' ')
      lammpsinp(ind:ind+3) = '.inp'
    endif
    open(itmp,file=lammpsinp,status='unknown')
  endif
!
!  Banner for bonded interactions
!
  write(itmp,'(''################################################################################'')')
  write(itmp,'(''#### Covalent bond parameters'')')
  write(itmp,'(''################################################################################'')')
!
!  Two-body - harmonic
!
  lfirst = .true.
  do n = 1,nbptype
    i = nbptypeptr(n)
    if (nptype(i).eq.5.or.nptype(i).eq.6) then
      if (lfirst) then
        lfirst = .false.
        write(itmp,'(/,''bond_style harmonic'')')
      endif
      write(itmp,'(''bond_coeff'',i4,f14.6,1x,f14.7)') n,0.5_dp*twopot(1,i),twopot(2,i)
    endif
  enddo
!
!  Two-body - Morse
!
  lfirst = .true.
  do n = 1,nbptype
    i = nbptypeptr(n)
    if (nptype(i).eq.3.or.nptype(i).eq.4) then
      if (lfirst) then
        lfirst = .false.
        write(itmp,'(/,''bond_style morse'')')
      endif
      write(itmp,'(''bond_coeff'',i4,f14.6,1x,f14.7,1x,f14.7)') n,(twopot(ind,i),ind=1,3)
    endif
  enddo
!
  write(itmp,'('' '')')
!
!  Banner for angles
!
  write(itmp,'(''################################################################################'')')
  write(itmp,'(''#### Covalent angle parameters'')')
  write(itmp,'(''################################################################################'')')
!
!  Three-body
!
  lfirst = .true.
  do i = 1,nthb
    if (nthrty(i).eq.1) then
      if (lfirst) then
        lfirst = .false.
        write(itmp,'(/,''angle_style harmonic'')')
      endif
      write(itmp,'(''angle_coeff'',i4,f14.7,1x,f14.5)') i,0.5_dp*thbk(i),theta(i)
    endif
  enddo
!
  write(itmp,'('' '')')
!
!  Banner for angles
!
  write(itmp,'(''################################################################################'')')
  write(itmp,'(''#### Covalent torsion parameters'')')
  write(itmp,'(''################################################################################'')')
!
!  Four-body
!
  lfirst = .true.
  do i = 1,nfor
    if (nforty(i).eq.1) then
      if (lfirst) then
        lfirst = .false.
        write(itmp,'(/''dihedral_style charmm'')')
      endif
      if (npfor(i).lt.0) then
        phi = forpoly(1,i) + 180.0_dp
      else
        phi = forpoly(1,i)
      endif
      write(itmp,'(''dihedral_coeff'',i4,f18.8,i4,i6,"   0.0")') i,fork(i),abs(npfor(i)),int(phi)
    endif
  enddo
!
  write(itmp,'('' '')')
!
!  Banner for out of plane/improper potentials
!
  write(itmp,'(''################################################################################'')')
  write(itmp,'(''#### Covalent improper dihedral parameters'')')
  write(itmp,'(''################################################################################'')')
!
!  Out of plane potentials
!
  lfirst = .true.
  do i = 1,nfor
    if (nforty(i).eq.3) then
      if (lfirst) then
        lfirst = .false.
        write(itmp,'(/''improper_style distance'')')
      endif
      write(itmp,'(''improper_coeff'',i4,f18.8," 0.0000")') i,fork(i)
    endif
  enddo
!
  write(itmp,'('' '')')
!
!  Banner for pair potentials
!
  write(itmp,'(''################################################################################'')')
  write(itmp,'(''#### Pair potentials'')')
  write(itmp,'(''################################################################################'')')
!
!  Find out whether all potentials are of the same type
!
  lmultipot = .false.
!
  scale14_coul = 1.0_dp
  scale14_two  = 1.0_dp
!
  nspec21 = nspec*(nspec+1)/2
  allocate(npotspecpair(nspec21))
  allocate(npotokptr(npote))
  allocate(npotspec1(npote))
  allocate(npotspec2(npote))
  allocate(npottypes(maxpottype))
!
  if (npote.gt.0) then
    npottypes(1:maxpottype) = 0
!
!  Count number of potentials per species pair
!
    npotspecpair(1:nspec21) = 0
    npoteok = 0
    do n = 1,npote
!
!  Exclude intramolecular bonding potentials
!
      if (nptype(n).lt.3.or.nptype(n).gt.6) then
        sym1 = ' '
        sym2 = ' '
        sym1(1:5) = symbol2(1,n)
        sym2(1:5) = symbol2(2,n)
        call okspec(lok1,sym1,isp1,.true.,ltype01)
        call okspec(lok2,sym2,isp2,.true.,ltype02)
        if (lok1.and.lok2) then
          npoteok = npoteok + 1
          if (isp1.lt.isp2) then
            npotspec1(npoteok) = isp1
            npotspec2(npoteok) = isp2
          else
            npotspec1(npoteok) = isp2
            npotspec2(npoteok) = isp1
          endif
          npotokptr(npoteok) = n
          if (n.le.maxpottype) then
            npottypes(nptype(n)) = npottypes(nptype(n)) + 1
          endif
        endif
        if (isp1.gt.isp2) then
          ind = isp1*(isp1-1)/2 + isp2
        else
          ind = isp2*(isp2-1)/2 + isp1
        endif
        npotspecpair(ind) = npotspecpair(ind) + 1
      endif
    enddo
!
!  Find out how many different types of potential we have
!
    npottype = 0
    do n = 1,maxpottype
      if (npottypes(n).gt.0) then
        npottype = npottype + 1
      endif
    enddo
  endif
  if (npottype.eq.0) then
!
!  No potential just Coulomb term
!
    write(itmp,'(/,''pair_style coul/long '',f6.3)') cutp
  elseif (npottype.eq.1) then
!
!  Just one potential type
!
    if ((cutp-tapermin).gt.1.0d-12) then
!
!  Tapered form of potential - assume MDF type for now
!
      if (npottypes(2).gt.0) then
        write(itmp,'(/,''pair_style overlay coul/long '',f6.3,'' lennard/gulp '',2(f6.3,2x))') cutp,tapermin,cutp
      elseif (npottypes(12).gt.0.or.npottypes(13).gt.0) then
        write(itmp,'(/,''pair_style overlay coul/long '',f6.3,'' lj/gulp '',2(f6.3,2x))') cutp,tapermin,cutp
      elseif (npottypes(1).gt.0) then
        write(itmp,'(/,''pair_style overlay coul/long '',f6.3,'' buck/gulp '',2(f6.3,2x))') cutp,tapermin,cutp
      endif
    else
      if (npottypes(2).gt.0.or.npottypes(12).gt.0.or.npottypes(13).gt.0) then
        write(itmp,'(/,''pair_style overlay coul/long '',f6.3,'' lj '',f6.3)') cutp,cutp
      elseif (npottypes(1).gt.0) then
        write(itmp,'(/,''pair_style overlay coul/long '',f6.3,'' buck '',f6.3)') cutp,cutp
      endif
    endif
  elseif (npottype.gt.1) then
    iline = 36
    line = 'pair_style hybrid/overlay coul/long '
    write(line(iline+1:iline+6),'(f6.3)') cutp
    iline = iline + 6
    do n = 1,maxpottype
      if (npottypes(n).gt.0) then
        if (n.eq.1) then
!
!  Buckingham
!
          if ((cutp-tapermin).gt.1.0d-12) then
            write(line(iline+1:iline+18),'('' buck/gulp '')')
            iline = iline + 18
            write(line(iline+1:iline+13),'(f6.3,1x,f6.3)') tapermin,cutp
            iline = iline + 13
          else
            write(line(iline+1:iline+6),'('' buck '')')
            iline = iline + 6
            write(line(iline+1:iline+6),'(f6.3)') cutp
            iline = iline + 6
          endif
        elseif (n.eq.2) then
!
!  Lennard-Jones : A-B
!
          if ((cutp-tapermin).gt.1.0d-12) then
            write(line(iline+1:iline+14),'('' lennard/gulp '')')
            iline = iline + 14
            write(line(iline+1:iline+13),'(f6.3,1x,f6.3)') tapermin,cutp
            iline = iline + 13
          else
            write(line(iline+1:iline+6),'('' lennard '')')
            iline = iline + 6
            write(line(iline+1:iline+6),'(f6.3)') cutp
            iline = iline + 6
          endif
        elseif (n.eq.12.or.n.eq.13) then
!
!  Lennard-Jones
!
          if ((cutp-tapermin).gt.1.0d-12) then
            write(line(iline+1:iline+16),'('' lj/gulp '')')
            iline = iline + 16
            write(line(iline+1:iline+13),'(f6.3,1x,f6.3)') tapermin,cutp
            iline = iline + 13
          else
            write(line(iline+1:iline+4),'('' lj '')')
            iline = iline + 4
            write(line(iline+1:iline+6),'(f6.3)') cutp
            iline = iline + 6
          endif
        endif
      endif     
    enddo
    write(itmp,'(/,a80)') line
!
!  Write Coulomb term
!
    write(itmp,'(''pair_coeff   *   * coul/long '')') 
  else
    write(itmp,'(''pair_coeff   *   * coul/long '')') 
  endif
  if (npoteok.gt.0) then
!
!  Loop over species pairs
!
    ind = 0
    do isp1 = 1,nspec
      do isp2 = 1,isp1
        ind = ind + 1
        if (npotspecpair(ind).gt.0) then
!
!  Find potentials for this pair
!
          n = 0
          nfound = 0
          do while (n.lt.npoteok.and.nfound.lt.npotspecpair(ind))
            n = n + 1
            if (npotspec1(n).eq.isp2.and.npotspec2(n).eq.isp1) then
              nfound = nfound + 1
!
!  Scale 1-4 handling
!
              if (nptype(npotokptr(n)).eq.9) then
                scale14_coul = scale14(npotokptr(n))
              else
                scale14_two  = scale14(npotokptr(n))
              endif
!
              if (nptype(npotokptr(n)).eq.12) then
!
!  Lennard-Jones epsilon-sigma form
!
                eps = twopot(1,n)
                sig = twopot(2,n)
                if (npottype.eq.1) then
                  write(itmp,'(''pair_coeff '',i3,1x,i3,1x,f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                    isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                else
                  if ((cutp-tapermin).gt.1.0d-12) then
                    write(itmp,'(''pair_coeff '',i3,1x,i3,'' lj/gulp '',f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                  else
                    write(itmp,'(''pair_coeff '',i3,1x,i3,'' lj '',f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                  endif
                endif
              elseif (nptype(n).eq.13) then
!
!  Lennard-Jones epsilon-sigma form
!
                eps = twopot(1,n)
                sig = twopot(2,n)/(2.0_dp**sixth)
                if (npottype.eq.1) then
                  write(itmp,'(''pair_coeff '',i3,1x,i3,1x,f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                    isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                else
                  if ((cutp-tapermin).gt.1.0d-12) then
                    write(itmp,'(''pair_coeff '',i3,1x,i3,'' lj/gulp '',f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                  else
                    write(itmp,'(''pair_coeff '',i3,1x,i3,'' lj '',f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                  endif
                endif
              elseif (nptype(n).eq.2) then
!
!  Lennard-Jones A/B form - convert to epsilon/sigma - only works if B > 0 & m = 12 / n = 6
!
                if (twopot(2,n).gt.1.0d-12.and.tpot(1,n).eq.12.0_dp.and.tpot(2,n).eq.6.0_dp) then
                  eps = twopot(1,n)
                  sig = twopot(2,n)
                  if (npottype.eq.1) then
                    write(itmp,'(''pair_coeff '',i3,1x,i3,1x,f12.4,1x,f12.6,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                  else
                    if ((cutp-tapermin).gt.1.0d-12) then
                      write(itmp,'(''pair_coeff '',i3,1x,i3,'' lennard/gulp '',f12.4,1x,f12.6,1x,f8.4,1x,f8.4)') &
                        isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                    else
                      write(itmp,'(''pair_coeff '',i3,1x,i3,'' lennard '',f12.4,1x,f12.6,1x,f8.4,1x,f8.4)') &
                        isp1,isp2,eps,sig,tapermin,rpot(npotokptr(n))
                    endif
                  endif
                endif
              elseif (nptype(n).eq.1) then
!
!  Buckingham potential
!
                if (npottype.eq.1) then
                  write(itmp,'(''pair_coeff '',i3,1x,i3,1x,f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                    isp1,isp2,twopot(1,n),twopot(2,n),twopot(3,n),tapermin,rpot(npotokptr(n))
                else
                  if ((cutp-tapermin).gt.1.0d-12) then
                    write(itmp,'(''pair_coeff '',i3,1x,i3,'' buck/gulp '',f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,twopot(1,n),twopot(2,n),twopot(3,n),tapermin,rpot(npotokptr(n))
                  else
                    write(itmp,'(''pair_coeff '',i3,1x,i3,'' buck '',f12.10,1x,f13.10,1x,f8.4,1x,f8.4)') &
                      isp1,isp2,twopot(1,n),twopot(2,n),twopot(3,n),tapermin,rpot(npotokptr(n))
                  endif
                endif
              endif
            endif
          enddo
        endif
      enddo
    enddo
  endif
!
!  Output molecular mechanics handling - note that meaning of numbers in Lammps is the opposite to GULP
!
  write(itmp,'(/,''special_bonds lj 0.0 0.0 '',f8.5,'' coul 0.0 0.0 '',f8.5,/)') 1.0_dp-scale14_two,1.0_dp-scale14_coul
!
!  Deallocate local memory
!
  deallocate(npottypes)
  deallocate(npotspec2)
  deallocate(npotspec1)
  deallocate(npotokptr)
  deallocate(npotspecpair)
  deallocate(nbptypeptr)
!
  return
  end
