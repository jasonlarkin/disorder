  subroutine fouroopmds(eoop,esregion12,esregion2,eattach,lgrad1,xderv,yderv,zderv,rstrd)
!
!  Subroutine for four-body energy from out of plane potentials using a spatial decomposition
!
!  Strategy - sift by potential first, then cutoffs
!
!   5/03 Created from fouroopmd
!  10/03 Modifications made for changes in algorithm
!   6/04 Sign of virial corrected
!  10/05 Modified to handle inversion form of out of plane potential
!   6/06 Inversion squared potential added
!   1/07 Wildcard handling in lmatch calls corrected
!   2/07 Bonding types added
!   5/07 QM/MM schemes added
!   6/07 Structure of arrays for storing spatial distribution changed to 1-D
!  10/07 Angle-angle cross potential added
!   4/08 Modified for variable domain size
!   4/08 xvec1cell replaced by xvec2cell etc for spatial algorithm
!   4/08 ind1toijk replaced by ind2toijk
!   4/08 Minimum cutoff added for out of plane potentials
!   5/08 UFFoop potential added
!   5/08 only3 check added as an option
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Option to output energy terms added
!   6/09 Site energy and virials added
!  11/11 Region-region energy contributions stored
!  11/11 Out of plane site energy divided on a per bond basis
!   2/12 Site energy corrected as the centre atom was not weighted properly.
!   4/12 Explicit virial calculation removed as no longer needed
!   4/12 xvir, yvir and zvir removed
!   5/12 Atomic stresses added
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : lsliceatom, nregions, nregionno, nregiontype, QMMMmode
  use control,        only : lseok, latomicstress
  use current
  use derivatives,    only : atomicstress
  use energies,       only : siteenergy, eregion2region
  use four
  use iochannels,     only : ioout
  use mdlogic
  use molecule
  use optimisation
  use parallel
  use spatial
  use symmetry
  implicit none
!
!  Passed variables
!
  real(dp),   intent(inout)                    :: eattach
  real(dp),   intent(inout)                    :: eoop
  real(dp),   intent(inout)                    :: esregion12
  real(dp),   intent(inout)                    :: esregion2
  real(dp),   intent(inout)                    :: rstrd(*)
  real(dp),   intent(inout)                    :: xderv(*)
  real(dp),   intent(inout)                    :: yderv(*)
  real(dp),   intent(inout)                    :: zderv(*)
  logical,    intent(in)                       :: lgrad1
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ic
  integer(i4)                                  :: icx
  integer(i4)                                  :: icy
  integer(i4)                                  :: icz
  integer(i4)                                  :: ikcx
  integer(i4)                                  :: ikcy
  integer(i4)                                  :: ikcz
  integer(i4)                                  :: ilcx
  integer(i4)                                  :: ilcy
  integer(i4)                                  :: ilcz
  integer(i4)                                  :: ii
  integer(i4)                                  :: ijcx
  integer(i4)                                  :: ijcy
  integer(i4)                                  :: ijcz
  integer(i4)                                  :: isgn
  integer(i4)                                  :: imx
  integer(i4)                                  :: imy
  integer(i4)                                  :: imz
  integer(i4)                                  :: ind
  integer(i4)                                  :: ind2
  integer(i4)                                  :: indn
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4)                                  :: ix
  integer(i4)                                  :: ixyz
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixk
  integer(i4)                                  :: iyk
  integer(i4)                                  :: izk
  integer(i4)                                  :: ixl
  integer(i4)                                  :: iyl
  integer(i4)                                  :: izl
  integer(i4)                                  :: j
  integer(i4)                                  :: jc
  integer(i4)                                  :: jcx
  integer(i4)                                  :: jcy
  integer(i4)                                  :: jcz
  integer(i4)                                  :: jj
  integer(i4)                                  :: jmx
  integer(i4)                                  :: jmy
  integer(i4)                                  :: jmz
  integer(i4)                                  :: jndn
  integer(i4)                                  :: k
  integer(i4)                                  :: kc
  integer(i4)                                  :: kcx
  integer(i4)                                  :: kcy
  integer(i4)                                  :: kcz
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kmx
  integer(i4)                                  :: kmy
  integer(i4)                                  :: kmz
  integer(i4)                                  :: kndn
  integer(i4)                                  :: l
  integer(i4)                                  :: lc
  integer(i4)                                  :: lcx
  integer(i4)                                  :: lcy
  integer(i4)                                  :: lcz
  integer(i4)                                  :: ll
  integer(i4)                                  :: maxx
  integer(i4)                                  :: maxxy
  integer(i4)                                  :: n
  integer(i4)                                  :: n1i
  integer(i4)                                  :: n1j
  integer(i4)                                  :: n1k
  integer(i4)                                  :: n1l
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: natk
  integer(i4)                                  :: natl
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeil
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: nbtypeil2
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nregiontypl
  integer(i4)                                  :: nsplower(3)
  integer(i4)                                  :: nspupper(3)
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  logical                                      :: l2bonds
  logical                                      :: lattach
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: linter_only
  logical                                      :: lintra_only
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lopk
  logical                                      :: lopl
  logical                                      :: lreg12
  logical                                      :: lreg2qtet
  logical                                      :: lsamemol
  logical                                      :: lsg1
  logical                                      :: lslicei
  logical                                      :: lslicej
  logical                                      :: lslicek
  logical                                      :: lslicel
  logical                                      :: ltsym23
  logical                                      :: ltsym24
  logical                                      :: ltsym34
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(1)
  real(dp)                                     :: eterm
  real(dp)                                     :: eterm3rd
  real(dp)                                     :: fpoly(5)
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ofct 
  real(dp)                                     :: phi0 
  real(dp)                                     :: r21
  real(dp)                                     :: r212
  real(dp)                                     :: r31
  real(dp)                                     :: r312
  real(dp)                                     :: r32
  real(dp)                                     :: r322
  real(dp)                                     :: r41
  real(dp)                                     :: r412
  real(dp)                                     :: r42
  real(dp)                                     :: r422
  real(dp)                                     :: r43
  real(dp)                                     :: r432
  real(dp)                                     :: rkfor
  real(dp)                                     :: rkfor4
  real(dp)                                     :: rko
  real(dp)                                     :: rn
  real(dp)                                     :: rprod(6,6)
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr1min
  real(dp)                                     :: tr2min
  real(dp)                                     :: tr3min
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x32
  real(dp)                                     :: y32
  real(dp)                                     :: z32
  real(dp)                                     :: x42
  real(dp)                                     :: y42
  real(dp)                                     :: z42
  real(dp)                                     :: x43
  real(dp)                                     :: y43
  real(dp)                                     :: z43
  real(dp)                                     :: xc3
  real(dp)                                     :: yc3
  real(dp)                                     :: zc3
  real(dp)                                     :: xc4
  real(dp)                                     :: yc4
  real(dp)                                     :: zc4
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
!
  lsg1 = (lstr.and.lgrad1)
!
!  Openning banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  OOP  : Atom No. 1  Atom No. 2  Atom No. 3  Atom No. 4  OutOfPlane energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  Set variables for cell distribution
!
  maxxy = nspcell(1)*nspcell(2)
  maxx  = nspcell(1)
!*************************
!  Loop over potentials  *
!*************************
!
!  Convolute outer two loops for parallelisation
!
  pots: do n = 1,nfor
    nfortype = nforty(n)
    if (.not.loutofplane(n)) cycle pots
    nt1 = nfspec1(n)
    nt2 = nfspec2(n)
    nt3 = nfspec3(n)
    nt4 = nfspec4(n)
    ntyp1 = nfptyp1(n)
    ntyp2 = nfptyp2(n)
    ntyp3 = nfptyp3(n)
    ntyp4 = nfptyp4(n)
    tr1 = for1(n)**2
    tr2 = for2(n)**2
    tr3 = for3(n)**2
    tr1min = for1min(n)**2
    tr2min = for2min(n)**2
    tr3min = for3min(n)**2
    ltsym23 = (lmatch(nt2,ntyp2,nt3,ntyp3,.false.).or.lmatch(nt3,ntyp3,nt2,ntyp2,.false.))
    ltsym24 = (lmatch(nt2,ntyp2,nt4,ntyp4,.false.).or.lmatch(nt4,ntyp4,nt2,ntyp2,.false.))
    ltsym34 = (lmatch(nt3,ntyp3,nt4,ntyp4,.false.).or.lmatch(nt4,ntyp4,nt3,ntyp3,.false.))
    lbtyp = (mmfexc(n).eq.1)
    rkfor = fork(n)
    rkfor4 = forpoly(1,n)
    lintra_only = (lfintra(n).and..not.lfinter(n))
    linter_only = (lfinter(n).and..not.lfintra(n))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Loop over all local spatial cells except buffer regions
!  
    do ixyz = 1,ncellpernode
      ind = ncellnodeptr(ixyz)
      ind2 = ind - 1      
      iz = ind2/maxxy
      ind2 = ind2 - maxxy*iz
      iy = ind2/maxx
      ix = ind2 - maxx*iy + 1  
      iy = iy + 1
      iz = iz + 1
      if (.not.lbuffercell(ixyz)) then
!
!  Set cell search bounds
!
        nspupper(1) = min(ix+ncellsearch(1),nspcell(1))
        nspupper(2) = min(iy+ncellsearch(2),nspcell(2))
        nspupper(3) = min(iz+ncellsearch(3),nspcell(3))
        nsplower(1) = max(ix-ncellsearch(1),1)
        nsplower(2) = max(iy-ncellsearch(2),1)
        nsplower(3) = max(iz-ncellsearch(3),1)
!
!  Get number of atoms in this cell
!
        ni = nspcellat(ind)
        n1i = nspcellat1ptr(ind)
!
!  Outer loop over atoms within this cell
!
        iloop: do ii = 1,ni
          i = nspcellatptr(n1i+ii)
          nati = nat(i)
          ntypi = nftype(i)
!
!  Check i is allowed for n
!
          if (.not.lmatch(nati,ntypi,nt1,ntyp1,.true.)) cycle iloop
!
!  Set properties of atom i 
!
          ic = nspcellatptrcell(n1i+ii)
          call ind2toijk(ic,icx,icy,icz)
          oci = occuf(i)  
          nregioni = nregionno(nsft+nrelat(i))
          nregiontypi = nregiontype(nregioni,ncf)
          lopi = (.not.lfreeze.or.lopf(nrelat(i)))
          lslicei = lsliceatom(nsft + nrelat(i))
!
!  QM/MM handling : i is a QM atom and potential is of bonded type => exclude
!
          if (QMMMmode(ncf).gt.0) then
            if (nregiontypi.eq.1.and.lbtyp) cycle iloop
          endif
!
!  Only 3 bonds check
!
          if (lbtyp.and.lonly3oop(n).and.nbonds(i).ne.3) cycle iloop
!
!  Set coordinates of atom i       
!
          xi = xinbox(i) + xvec2cell(ic)
          yi = yinbox(i) + yvec2cell(ic)
          zi = zinbox(i) + zvec2cell(ic)
!
!  Molecule handling
!
          if (lmol.and.lneedmol) then
            nmi = natmol(i)
            if (ndim.gt.0) then
              indm = nmolind(i)
              call mindtoijk(indm,ixi,iyi,izi)
            endif
          endif
!
!  Loop over neighbouring cells
!
          do imz = nsplower(3),nspupper(3)
            do imy = nsplower(2),nspupper(2)
              do imx = nsplower(1),nspupper(1)
                indn = (imz-1)*maxxy + (imy-1)*maxx + imx
!
!  Loop over atoms within neighbouring cells
! 
                nj = nspcellat(indn)    
                n1j = nspcellat1ptr(indn)
                jloop: do jj = 1,nj
                  j = nspcellatptr(n1j+jj)
!
!  Set species type parameters for atom j
!
                  natj = nat(j)
                  ntypj = nftype(j)
!
!  Check j is allowed for n
!
                  if (.not.lmatch(natj,ntypj,nt2,ntyp2,.true.)) cycle jloop
!
                  if (lmol.and.lneedmol) then
!
!  Molecule handling
!
                    nmj = natmol(j)
                    if (ndim.gt.0) then
                      indmj = nmolind(j)
                      call mindtoijk(indmj,ixj,iyj,izj)
                      ixj = ixj - ixi
                      iyj = iyj - iyi
                      izj = izj - izi
                    endif
                    lmolok = (nmi.eq.nmj.and.nmi.gt.0)
                  else
                    lmolok = .false.
                  endif
!
!  Check for intra and but not in same molecule
!
                  if (lintra_only.and..not.lmolok) cycle jloop
                  if (lbtyp.and..not.lmolok) cycle jloop
!  
!  Set properties for j
!
                  jc = nspcellatptrcell(n1j+jj)
                  call ind2toijk(jc,jcx,jcy,jcz)
!
!  Calculate vector from atom 1 to atom 2
!
                  x21 = xvec2cell(jc) + xinbox(j) - xi
                  y21 = yvec2cell(jc) + yinbox(j) - yi
                  z21 = zvec2cell(jc) + zinbox(j) - zi
!
!  Check r21 is OK
!
                  r212 = x21*x21 + y21*y21 + z21*z21
                  if (r212.lt.1d-12) cycle jloop
!
!  Molecule checking
!
                  lbonded = .false.
                  if (lmolok) then
                    if (ndim.eq.0) then
                      if (linter_only) cycle jloop
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
                        if (.not.lbonded) cycle jloop
                      endif
                    else
                      ijcx = jcx - icx
                      ijcy = jcy - icy
                      ijcz = jcz - icz
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ijcx,ijcy,ijcz)
                        if (.not.lbonded) cycle jloop
                        lsamemol = (lbonded.or.l2bonds)
                      else
                        lsamemol = .false.
                      endif
                      if (.not.lsamemol) then
                        call samemol(lsamemol,nmi,ijcx,ijcy,ijcz,ixj,iyj,izj)
                      endif
                      if (lintra_only.and..not.lsamemol) cycle jloop
                      if (linter_only.and.lsamemol) cycle jloop
                    endif
                  endif
!
!  Distance checking
!
                  if ((r212.gt.tr1.or.r212.lt.tr1min).and.(.not.lbtyp.or..not.lbonded)) cycle jloop
                  r21 = sqrt(r212)
!  
!  Set remaining properties for j
!
                  nregionj = nregionno(nsft+nrelat(j))
                  nregiontypj = nregiontype(nregionj,ncf)
                  ocj = occuf(j)
                  lopj = (lopf(nrelat(j)).or..not.lfreeze)
                  lslicej = lsliceatom(nsft + nrelat(j))
!
!  Loop over neighbouring cells for k
!
                  do jmz = nsplower(3),nspupper(3)
                    do jmy = nsplower(2),nspupper(2)
                      do jmx = nsplower(1),nspupper(1)
                        jndn = (jmz-1)*maxxy + (jmy-1)*maxx + jmx
!
!  Loop over atoms within neighbouring cells
!
                        nk = nspcellat(jndn)
                        n1k = nspcellat1ptr(jndn)
                        kloop: do kk = 1,nk
                          k = nspcellatptr(n1k+kk)
!
!  Check whether we need to do this atom
!
                          if (ltsym23.and.k.ge.j) cycle kloop
!
!  Prevent atoms i and k being the same atom
!
                          if (k.eq.i) cycle kloop
                          natk = nat(k)
                          ntypk = nftype(k)
!
!  Check k is allowed for n
!
                          if (.not.lmatch(natk,ntypk,nt3,ntyp3,.true.)) cycle kloop
!
                          if (lmol.and.lneedmol) then
!
!  Molecule handling
!
                            nmk = natmol(k)
                            if (ndim.gt.0) then
                              indmk = nmolind(k)
                              call mindtoijk(indmk,ixk,iyk,izk)
                              ixk = ixk - ixi
                              iyk = iyk - iyi
                              izk = izk - izi
                            endif
                            lmolok = (nmi.eq.nmk.and.nmi.gt.0)
                          else
                            lmolok = .false.
                          endif
!
!  Check for intra and but not in same molecule
!
                          if (lintra_only.and..not.lmolok) cycle kloop
                          if (lbtyp.and..not.lmolok) cycle kloop
!
!  Set properties of atom k
!
                          kc = nspcellatptrcell(n1k+kk)
                          call ind2toijk(kc,kcx,kcy,kcz)
!
!  Calculate vectors
!
                          xc3 = xvec2cell(kc) + xinbox(k)
                          yc3 = yvec2cell(kc) + yinbox(k)
                          zc3 = zvec2cell(kc) + zinbox(k)
                          x31 = xc3 - xi     
                          y31 = yc3 - yi
                          z31 = zc3 - zi
!
!  Check r31 is OK
!
                          r312 = x31*x31 + y31*y31 + z31*z31
                          if (r312.lt.1d-12) cycle kloop
!
!  Molecule checking
!
                          lbonded = .false.
                          if (lmolok) then
                            if (ndim.eq.0) then
                              if (linter_only) cycle kloop
                              if (lbtyp) then
                                call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,0_i4,0_i4,0_i4)
                                if (.not.lbonded) cycle kloop
                              endif
                            else
                              ikcx = kcx - icx
                              ikcy = kcy - icy
                              ikcz = kcz - icz
                              if (lbtyp) then
                                call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,ikcx,ikcy,ikcz)
                                if (.not.lbonded) cycle kloop
                                lsamemol = (lbonded.or.l2bonds)
                              else
                                lsamemol = .false.
                              endif
                              if (.not.lsamemol) then
                                call samemol(lsamemol,nmi,ikcx,ikcy,ikcz,ixk,iyk,izk)
                              endif
                              if (lintra_only.and..not.lsamemol) cycle kloop
                              if (linter_only.and.lsamemol) cycle kloop
                            endif
                          endif
!
!  Distance checking
!
                          if ((r312.gt.tr2.or.r312.lt.tr2min).and.(.not.lbtyp.or..not.lbonded)) cycle kloop
                          r31 = sqrt(r312)
!
!  Set remaining properties of atom k
!
                          nregionk = nregionno(nsft+nrelat(k))
                          nregiontypk = nregiontype(nregionk,ncf)
                          ock = occuf(k)
                          lopk = (lopf(nrelat(k)).or..not.lfreeze)
                          lslicek = lsliceatom(nsft + nrelat(k))
!
!  Loop over neighbouring cells for l
!
                          do kmz = nsplower(3),nspupper(3)
                            do kmy = nsplower(2),nspupper(2)
                              do kmx = nsplower(1),nspupper(1)
                                kndn = (kmz-1)*maxxy + (kmy-1)*maxx + kmx
!
!  Loop over atoms within neighbouring cells
!
                                nl = nspcellat(kndn)
                                n1l = nspcellat1ptr(kndn)
                                lloop: do ll = 1,nl
                                  l = nspcellatptr(n1l+ll)
!
!  Check whether we need to do this atom
!
                                  if (ltsym34) then
                                    if (l.ge.k) cycle lloop
                                  elseif (ltsym24) then
                                    if (l.ge.j) cycle lloop
                                  endif
!
!  Prevent atoms i and l being the same atom
!
                                  if (l.eq.i) cycle lloop
!
!  Check l is allowed for n
!
                                  natl = nat(l)
                                  ntypl = nftype(l)
                                  if (.not.lmatch(natl,ntypl,nt4,ntyp4,.true.)) cycle lloop
!
!  If lfreeze=.true. and no atoms have any variables
!  then skip this four body term
!
                                  lopl = (lopf(nrelat(l)).or..not.lfreeze)
                                  if (.not.lopi.and..not.lopj.and..not.lopk.and..not.lopl) cycle lloop
!
                                  if (lmol.and.lneedmol) then
!
!  Molecule handling
!
                                    nml = natmol(l)
                                    if (ndim.gt.0) then
                                      indml = nmolind(l)
                                      call mindtoijk(indml,ixl,iyl,izl)
                                      ixl = ixl - ixi
                                      iyl = iyl - iyi
                                      izl = izl - izi
                                    endif
                                    lmolok = (nmi.eq.nml.and.nmi.gt.0)
                                  else
                                    lmolok = .false.
                                  endif
!
!  Check for intra and but not in same molecule
!
                                  if (lintra_only.and..not.lmolok) cycle lloop
                                  if (lbtyp.and..not.lmolok) cycle lloop
!
!  Set remaining properties of l
!
                                  lc = nspcellatptrcell(n1l+ll)
                                  call ind2toijk(lc,lcx,lcy,lcz)
                                  nregionl = nregionno(nsft+nrelat(l))
                                  nregiontypl = nregiontype(nregionl,ncf)
                                  ocl = occuf(l) 
!
!  Calculate vectors
!
                                  xc4 = xinbox(l) + xvec2cell(lc)
                                  yc4 = yinbox(l) + yvec2cell(lc)     
                                  zc4 = zinbox(l) + zvec2cell(lc)
                                  x41 = xc4 - xi
                                  y41 = yc4 - yi
                                  z41 = zc4 - zi
!
!  Check r41 is OK
!
                                  r412 = x41*x41 + y41*y41 + z41*z41
                                  if (r412.lt.1d-12) cycle lloop
!
!  Molecule checking
!
                                  lbonded = .false.
                                  if (lmolok) then
                                    if (ndim.eq.0) then
                                      if (linter_only) cycle lloop
                                      if (lbtyp) then
                                        call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,i,l,0_i4,0_i4,0_i4)
                                        if (.not.lbonded) cycle lloop
                                      endif
                                    else
                                      ilcx = lcx - icx
                                      ilcy = lcy - icy
                                      ilcz = lcz - icz
                                      if (lbtyp) then
                                        call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,i,l,ilcx,ilcy,ilcz)
                                        if (.not.lbonded) cycle lloop
                                        lsamemol = (lbonded.or.l2bonds)
                                      else
                                        lsamemol = .false.
                                      endif
                                      if (.not.lsamemol) then
                                        call samemol(lsamemol,nmi,ilcx,ilcy,ilcz,ixl,iyl,izl)
                                      endif
                                      if (lintra_only.and..not.lsamemol) cycle lloop
                                      if (linter_only.and.lsamemol) cycle lloop
                                    endif
                                  endif
!
!  Distance checking
!
                                  if ((r412.gt.tr3.or.r412.lt.tr3min).and.(.not.lbtyp.or..not.lbonded)) cycle lloop
!
!  Calculate other vectors needed
!
                                  x32 = x31 - x21
                                  y32 = y31 - y21
                                  z32 = z31 - z21
                                  r322 = x32*x32 + y32*y32 + z32*z32
                                  r32 = sqrt(r322)
                                  x43 = x41 - x31
                                  y43 = y41 - y31
                                  z43 = z41 - z31
!*********************************
!  Valid four-body term located  *
!*********************************
!
!  Calculate remaining distances
!
                                  r41 = sqrt(r412)
                                  x42 = x43 + x32
                                  y42 = y43 + y32
                                  z42 = z43 + z32
                                  r422 = x42*x42 + y42*y42 + z42*z42
                                  r432 = x43*x43 + y43*y43 + z43*z43
                                  r42 = sqrt(r422)
                                  r43 = sqrt(r432)
!
!  Set region 2 quartet flag
!
                                  lreg12    = .false.  
                                  lreg2qtet = .false.  
                                  if (lseok.and.nregions(ncf).gt.1) then
                                    lreg2qtet = (nregioni.gt.1.and.nregionj.gt.1.and.nregionk.gt.1.and.nregionl.gt.1)
                                    if (.not.lreg2qtet) lreg12 = (nregioni.gt.1.or.nregionj.gt.1.or. &
                                                                  nregionk.gt.1.or.nregionl.gt.1)
                                  endif
                                  lslicel = lsliceatom(nsft + nrelat(l))
                                  lattach = .true.
                                  if (lslicei.and.lslicej.and.lslicek.and.lslicel) lattach = .false.
                                  if (.not.lslicei.and..not.lslicej.and..not.lslicek.and..not.lslicel) lattach = .false.
!
!  QM/MM handling : i, j, k & l are all QM atoms => exclude
!
                                  if (QMMMmode(ncf).gt.0) then
                                    if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1) cycle lloop
                                  endif
!
                                  ofct = oci*ocj*ock*ocl
                                  rko = rkfor*ofct
                                  if (nfortype.eq.14.or.nfortype.eq.16) then
                                    fpoly(1) = forpoly(1,n)*ofct
                                    fpoly(2) = forpoly(2,n)*ofct
                                    fpoly(3) = forpoly(3,n) 
                                    fpoly(4) = forpoly(4,n)
                                    fpoly(5) = forpoly(5,n)
                                  elseif (nfortype.eq.15) then
                                    fpoly(1) = forpoly(1,n)
                                    fpoly(2) = forpoly(2,n)
                                    fpoly(3) = forpoly(3,n)
                                  else
                                    fpoly(1) = rkfor4*ofct
                                    fpoly(2) = forpoly(2,n)
                                  endif
                                  call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko,rn,phi0,isgn,fpoly, &
                                    lgrad1,.false.,.false.)
                                  if (lreg2qtet) then
                                    esregion2 = esregion2 + eterm
                                  elseif (lreg12) then
                                    esregion12 = esregion12 + eterm
                                  else
                                    eoop = eoop + eterm
                                  endif
                                  if (lattach) eattach = eattach + eterm
!
                                  eterm3rd = eterm/3.0_dp
                                  eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm3rd
                                  eregion2region(nregionk,nregioni) = eregion2region(nregionk,nregioni) + eterm3rd
                                  eregion2region(nregionl,nregioni) = eregion2region(nregionl,nregioni) + eterm3rd
!
                                  siteenergy(i) = siteenergy(i) + 1.5_dp*eterm3rd
                                  siteenergy(j) = siteenergy(j) + 0.5_dp*eterm3rd
                                  siteenergy(k) = siteenergy(k) + 0.5_dp*eterm3rd
                                  siteenergy(l) = siteenergy(l) + 0.5_dp*eterm3rd
!
!  Output energy contribution
!
                                  if (lPrintFour) then
                                    write(ioout,'(4x,4i12,1x,f22.10)') i,j,k,l,eterm
                                  endif
!*****************************
!  Out of plane derivatives  *
!*****************************
!
!  Set up strain products
!
                                  if (lsg1) then
                                    call fourstrterms(ndim,rprod,x21,y21,z21,x31,y31,z31, &
                                      x41,y41,z41,x32,y32,z32,x42,y42,z42,x43,y43,z43)
                                  endif
!***********************
!  Strain derivatives  *
!***********************
                                  if (lsg1) then
!
!  First strain derivatives
!
                                    rstrdloc(1:nstrains) = 0.0_dp
                                    do kl = 1,nstrains
                                      rstrdloc(kl) = rstrdloc(kl) + e1d(1)*rprod(kl,1)
                                      rstrdloc(kl) = rstrdloc(kl) + e1d(2)*rprod(kl,2)
                                      rstrdloc(kl) = rstrdloc(kl) + e1d(3)*rprod(kl,3)
                                      rstrdloc(kl) = rstrdloc(kl) + e1d(4)*rprod(kl,4)
                                      rstrdloc(kl) = rstrdloc(kl) + e1d(5)*rprod(kl,5)
                                      rstrdloc(kl) = rstrdloc(kl) + e1d(6)*rprod(kl,6)
                                      rstrd(kl) = rstrd(kl) + rstrdloc(kl)
                                    enddo
                                    if (latomicstress) then
                                      do kl = 1,nstrains
                                        atomicstress(kl,i) = atomicstress(kl,i) + 0.25_dp*rstrdloc(kl)
                                        atomicstress(kl,j) = atomicstress(kl,j) + 0.25_dp*rstrdloc(kl)
                                        atomicstress(kl,k) = atomicstress(kl,k) + 0.25_dp*rstrdloc(kl)
                                        atomicstress(kl,l) = atomicstress(kl,l) + 0.25_dp*rstrdloc(kl)
                                      enddo
                                    endif
                                  endif
!*************************
!  Internal derivatives  *
!*************************
                                  if (lgrad1) then
                                    xderv(i) = xderv(i) - x21*e1d(1) - x31*e1d(2) - x41*e1d(3)
                                    yderv(i) = yderv(i) - y21*e1d(1) - y31*e1d(2) - y41*e1d(3)
                                    zderv(i) = zderv(i) - z21*e1d(1) - z31*e1d(2) - z41*e1d(3)
                                    xderv(j) = xderv(j) - x32*e1d(4) + x21*e1d(1) - x42*e1d(5)
                                    yderv(j) = yderv(j) - y32*e1d(4) + y21*e1d(1) - y42*e1d(5)
                                    zderv(j) = zderv(j) - z32*e1d(4) + z21*e1d(1) - z42*e1d(5)
                                    xderv(k) = xderv(k) + x32*e1d(4) - x43*e1d(6) + x31*e1d(2)
                                    yderv(k) = yderv(k) + y32*e1d(4) - y43*e1d(6) + y31*e1d(2)
                                    zderv(k) = zderv(k) + z32*e1d(4) - z43*e1d(6) + z31*e1d(2)
                                    xderv(l) = xderv(l) + x43*e1d(6) + x42*e1d(5) + x41*e1d(3)
                                    yderv(l) = yderv(l) + y43*e1d(6) + y42*e1d(5) + y41*e1d(3)
                                    zderv(l) = zderv(l) + z43*e1d(6) + z42*e1d(5) + z41*e1d(3)
                                  endif
!
!  End of loop over atom l
!
                                enddo lloop
!
!  End of loops over neighbouring cells for l
!
                              enddo
                            enddo
                          enddo
!
!  End of loop over atom k
!
                        enddo kloop
!
!  End of loops over neighbouring cells for k
!
                      enddo
                    enddo
                  enddo
!
!  End of loop over atom j
!
                enddo jloop
!
!  End of loops over neighbouring cells for j
!
              enddo
            enddo
          enddo
!
!  End of loop over atom i
!
        enddo iloop
!
!  End if for non-buffer cell
!
      endif
!
!  End of loop over cells
!
    enddo
!
!  End of loop out of plane potentials
!
  enddo pots
!
!  Closing banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  All tidying up of derivatives is handled by four so we can just return here
!
  return
  end
