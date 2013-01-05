  subroutine getanyneighbour(maxneigh,rcutmax2,nneigh,neighno,rneigh, &
                             xneigh,yneigh,zneigh,latomdone,lmaxneighok)
!
!  Finds any neighbours within a specified cutoff
!
!  On entry : 
!
!  maxneigh      = size of neighbour arrays
!  rcutmax2      = maximum cutoff distance for all atoms squared
!
!  On exit :
!
!  nneigh        = number of neighbours for each atom
!  neighno       = pointer to atom numbers of neighbours for each atom
!  rneigh        = distances of neighbours for each atom
!  xneigh        = x component of distances of neighbours for each atom
!  yneigh        = y component of distances of neighbours for each atom
!  zneigh        = z component of distances of neighbours for each atom
!  latomdone     = logical indicating whether atom was done locally
!  lmaxneighok   = if .true. then dimension maxneigh was sufficient
!
!   6/09 Created from getBOneighbour
!   7/11 Bug in initialisation of neighbour numbers fixed
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, July 2011
!
  use datatypes
  use current,        only : numat, iimax, iimid
  use current,        only : xclat, yclat, zclat
  use current,        only : xvec1cell, yvec1cell, zvec1cell
  use current,        only : xvec2cell, yvec2cell, zvec2cell
  use spatial,        only : lbuffercell
  use spatial,        only : lspatialok
  use spatial,        only : ncellsearch
  use spatial,        only : ncellpernode
  use spatial,        only : ncellnodeptr
  use spatial,        only : nspcell
  use spatial,        only : nspcellat
  use spatial,        only : nspcellatptr
  use spatial,        only : nspcellat1ptr
  use spatial,        only : nspcellatptrcell
  use spatial,        only : xinbox
  use spatial,        only : yinbox
  use spatial,        only : zinbox
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: maxneigh
  integer(i4), intent(out)                       :: nneigh(numat)
  integer(i4), intent(out)                       :: neighno(maxneigh,numat)
  real(dp),    intent(in)                        :: rcutmax2
  real(dp),    intent(out)                       :: rneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: xneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: yneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: zneigh(maxneigh,numat)
  logical,     intent(out)                       :: latomdone(numat)
  logical,     intent(out)                       :: lmaxneighok
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ic
  integer(i4)                                    :: ii
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: ix
  integer(i4)                                    :: ixyz
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: maxx
  integer(i4)                                    :: n1
  integer(i4)                                    :: n1j
  integer(i4)                                    :: ni
  integer(i4)                                    :: nj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  real(dp)                                       :: rij
  real(dp)                                       :: r2
  real(dp)                                       :: xi
  real(dp)                                       :: yi     
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
!
!  Initialise lmaxneighok and nneigh
!
  lmaxneighok = .true.
  nneigh(1:numat) = 0
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  if (lspatialok) then
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!  
!  Loop over all spatial cells looking for non-buffer cells
!     
    do ixyz = 1,ncellpernode
      if (.not.lbuffercell(ixyz)) then
        ind1 = ncellnodeptr(ixyz)
        ind2 = ind1 - 1
        iz = ind2/maxxy
        ind2 = ind2 - maxxy*iz
        iy = ind2/maxx
        ix = ind2 - maxx*iy + 1
        iy = iy + 1
        iz = iz + 1
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
        ni = nspcellat(ind1)
        n1 = nspcellat1ptr(ind1)
!  
!  Loop over atoms in the cell finding neighbours
!     
        do ii = 1,ni
          i = nspcellatptr(n1+ii)
          ic = nspcellatptrcell(n1+ii)
          latomdone(i) = .true.
!       
!  Set coordinates
!
          xi = xinbox(i) + xvec2cell(ic)
          yi = yinbox(i) + yvec2cell(ic)
          zi = zinbox(i) + zvec2cell(ic)
!
!  Loop over neighbouring cells
!
          do imz = nsplower(3),nspupper(3)
            do imy = nsplower(2),nspupper(2)
              do imx = nsplower(1),nspupper(1)
                ind2 = (imz-1)*maxxy + (imy-1)*maxx + imx
!                     
!  Loop over atoms within neighbouring cells  
!                         
                nj = nspcellat(ind2)
                n1j = nspcellat1ptr(ind2)
                do jj = 1,nj
                  j = nspcellatptr(n1j+jj)
!                     
!  Exclude self term    
!                         
                  if (i.ne.j.or.ind1.ne.ind2) then
                    jc = nspcellatptrcell(n1j+jj)
!                             
!  Set centre cell coordinate differences
!  
                    xji = xvec2cell(jc) + xinbox(j) - xi
                    yji = yvec2cell(jc) + yinbox(j) - yi
                    zji = zvec2cell(jc) + zinbox(j) - zi
!  
                    r2 = xji*xji + yji*yji + zji*zji
                    if (r2 .lt. rcutmax2) then
                      if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                        lmaxneighok = .false.
                        nneigh(i) = nneigh(i) + 1
                      else        
                        rij = sqrt(r2)
                        nneigh(i) = nneigh(i) + 1
                        neighno(nneigh(i),i) = j
                        rneigh(nneigh(i),i) = rij
                        xneigh(nneigh(i),i) = xji
                        yneigh(nneigh(i),i) = yji
                        zneigh(nneigh(i),i) = zji
                      endif
                    endif
                  endif
!                             
                enddo
              enddo
            enddo
          enddo             
!                                   
        enddo                 
!                                 
      endif
    enddo                       
  else
    do i = 1,numat
      latomdone(i) = .true.
!
!  Loop over atoms
!
      do j = 1,numat
!
!  Set centre cell coordinate differences
!
        xji0 = xclat(j) - xclat(i)
        yji0 = yclat(j) - yclat(i)
        zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
        do ii = 1,iimax
!
!  Exclude self term
!
          if (i.ne.j.or.ii.ne.iimid) then
            xji = xji0 + xvec1cell(ii)
            yji = yji0 + yvec1cell(ii)
            zji = zji0 + zvec1cell(ii)
            r2 = xji*xji + yji*yji + zji*zji
            if (r2 .lt. rcutmax2) then
              if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                lmaxneighok = .false.
                nneigh(i) = nneigh(i) + 1
              else
                rij = sqrt(r2)
                nneigh(i) = nneigh(i) + 1
                neighno(nneigh(i),i) = j
                rneigh(nneigh(i),i) = rij
                xneigh(nneigh(i),i) = xji
                yneigh(nneigh(i),i) = yji
                zneigh(nneigh(i),i) = zji
              endif
            endif
          endif
        enddo
      enddo
    enddo
  endif
  return
  end
  subroutine getBOneighbour(maxneigh,rBOcutmax,nBOpotptr,nneigh,neighno,rneigh, &
                            xneigh,yneigh,zneigh,latomdone,lmaxneighok)
!
!  Finds neighbours for a bond order potential
!
!  On entry : 
!
!  maxneigh      = size of neighbour arrays
!  rBOcutmax     = array of maximum potential cutoffs for each atom
!
!  On exit :
!
!  nBOpotptr     = pointer to potential number for each neighbour
!  nneigh        = number of neighbours for each atom
!  neighno       = pointer to atom numbers of neighbours for each atom
!  rneigh        = distances of neighbours for each atom
!  xneigh        = x component of distances of neighbours for each atom
!  yneigh        = y component of distances of neighbours for each atom
!  zneigh        = z component of distances of neighbours for each atom
!  latomdone     = logical indicating whether atom was done locally
!  lmaxneighok   = if .true. then dimension maxneigh was sufficient
!
!  10/04 Created from bondorder.f
!   6/07 Structure of arrays for storing distribution changed to 1-D
!  12/07 lmaxneighok initialised on entry
!  12/07 lmaxneighok initialisation corrected
!   4/08 Modified for variable domain size in spatial case
!   4/08 xvec1cell replaced by xvec2cell etc for spatial algorithm
!   7/11 Bug in initialisation of neighbour numbers fixed
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, July 2011
!
  use datatypes
  use bondorderdata
  use current,        only : numat, nat, nftype, iimax, iimid
  use current,        only : xclat, yclat, zclat
  use current,        only : xvec1cell, yvec1cell, zvec1cell
  use current,        only : xvec2cell, yvec2cell, zvec2cell
  use spatialbo,      only : lbuffercell => lbuffercellBO
  use spatialbo,      only : lspatialok => lspatialBOok
  use spatialbo,      only : ncellsearch => ncellsearchbo
  use spatialbo,      only : ncellpernode => ncellpernodebo
  use spatialbo,      only : ncellnodeptr => ncellnodeptrbo
  use spatialbo,      only : nspcell => nspcellbo
  use spatialbo,      only : nspcellat => nspcellatbo
  use spatialbo,      only : nspcellatptr => nspcellatptrbo
  use spatialbo,      only : nspcellat1ptr => nspcellat1ptrbo
  use spatialbo,      only : nspcellatptrcell => nspcellatptrcellbo
  use spatialbo,      only : xinbox => xinboxbo
  use spatialbo,      only : yinbox => yinboxbo
  use spatialbo,      only : zinbox => zinboxbo
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: maxneigh
  integer(i4), intent(out)                       :: nneigh(numat)
  integer(i4), intent(out)                       :: nBOpotptr(maxneigh,numat)
  integer(i4), intent(out)                       :: neighno(maxneigh,numat)
  real(dp),    intent(in)                        :: rBOcutmax(numat)
  real(dp),    intent(out)                       :: rneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: xneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: yneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: zneigh(maxneigh,numat)
  logical,     intent(out)                       :: latomdone(numat)
  logical,     intent(out)                       :: lmaxneighok
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ic
  integer(i4)                                    :: ii
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: ix
  integer(i4)                                    :: ixyz
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: m
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: maxx
  integer(i4)                                    :: n1
  integer(i4)                                    :: n1j
  integer(i4)                                    :: nati
  integer(i4)                                    :: natj
  integer(i4)                                    :: ni
  integer(i4)                                    :: nj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  integer(i4)                                    :: ntypi
  integer(i4)                                    :: ntypj
  logical                                        :: lok
  real(dp)                                       :: bR22
  real(dp)                                       :: rij
  real(dp)                                       :: r2
  real(dp)                                       :: xi
  real(dp)                                       :: yi     
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
!
!  Initialise lmaxneighok and nneigh
!
  lmaxneighok = .true.
  nneigh(1:numat) = 0
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  if (lspatialok) then
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!  
!  Loop over all spatial cells looking for non-buffer cells
!     
    do ixyz = 1,ncellpernode
      if (.not.lbuffercell(ixyz)) then
        ind1 = ncellnodeptr(ixyz)
        ind2 = ind1 - 1
        iz = ind2/maxxy
        ind2 = ind2 - maxxy*iz
        iy = ind2/maxx
        ix = ind2 - maxx*iy + 1
        iy = iy + 1
        iz = iz + 1
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
        ni = nspcellat(ind1)
        n1 = nspcellat1ptr(ind1)
!  
!  Loop over atoms in the cell finding neighbours
!     
        do ii = 1,ni
          i = nspcellatptr(n1+ii)
          ic = nspcellatptrcell(n1+ii)
          nati = nat(i)
          ntypi = nftype(i)
          latomdone(i) = .true.
!       
!  Set coordinates
!
          xi = xinbox(i) + xvec2cell(ic)
          yi = yinbox(i) + yvec2cell(ic)
          zi = zinbox(i) + zvec2cell(ic)
!                             
!  Compute square of cut-off for distance checking
!  
          bR22 = rBOcutmax(i)**2
!
!  Loop over neighbouring cells
!
          do imz = nsplower(3),nspupper(3)
            do imy = nsplower(2),nspupper(2)
              do imx = nsplower(1),nspupper(1)
                ind2 = (imz-1)*maxxy + (imy-1)*maxx + imx
!                       
!  Loop over atoms within neighbouring cells  
!                         
                nj = nspcellat(ind2)
                n1j = nspcellat1ptr(ind2)
                do jj = 1,nj
                  j = nspcellatptr(n1j+jj)
!                     
!  Exclude self term    
!                         
                  if (i.ne.j.or.ind1.ne.ind2) then
                    jc = nspcellatptrcell(n1j+jj)
!                             
!  Set centre cell coordinate differences
!  
                    xji = xvec2cell(jc) + xinbox(j) - xi
                    yji = yvec2cell(jc) + yinbox(j) - yi
                    zji = zvec2cell(jc) + zinbox(j) - zi
!  
                    r2 = xji*xji + yji*yji + zji*zji
                    if (r2 .lt. bR22) then
!
!  Check whether j is within two body cut-off
!
                      natj = nat(j)
                      ntypj = nftype(j)
                      m = 0
                      lok = .false.
                      do while (m.lt.nbopot.and..not.lok)
                        m = m + 1
                        if (nati.eq.nBOspec1(m).and.(ntypi.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0).and. &
                            natj.eq.nBOspec2(m).and.(ntypj.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0)) then
                          lok = (r2.lt.rBOmax(m)**2)
                        elseif (nati.eq.nBOspec2(m).and.(ntypi.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0).and. &
                            natj.eq.nBOspec1(m).and.(ntypj.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0)) then
                          lok = (r2.lt.rBOmax(m)**2)
                        endif
                      enddo
                      if (lok) then
                        if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                          lmaxneighok = .false.
                          nneigh(i) = nneigh(i) + 1
                        else        
                          rij = sqrt(r2)
                          nneigh(i) = nneigh(i) + 1
                          neighno(nneigh(i),i) = j
                          nBOpotptr(nneigh(i),i) = m
                          rneigh(nneigh(i),i) = rij
                          xneigh(nneigh(i),i) = xji
                          yneigh(nneigh(i),i) = yji
                          zneigh(nneigh(i),i) = zji
                        endif
                      endif
                    endif
                  endif
!
                enddo
              enddo
            enddo
          enddo             
!
        enddo                 
!
      endif
    enddo                       
  else
    do i = 1,numat
      nati = nat(i)
      ntypi = nftype(i)
      latomdone(i) = .true.
!     
!  Compute square of cut-off for distance checking   
!     
      bR22 = rBOcutmax(i)**2
!
!  Loop over atoms
!
      do j = 1,numat
        natj = nat(j)
        ntypj = nftype(j)
!
!  Set centre cell coordinate differences
!
        xji0 = xclat(j) - xclat(i)
        yji0 = yclat(j) - yclat(i)
        zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
        do ii = 1,iimax
!
!  Exclude self term
!
          if (i.ne.j.or.ii.ne.iimid) then
            xji = xji0 + xvec1cell(ii)
            yji = yji0 + yvec1cell(ii)
            zji = zji0 + zvec1cell(ii)
            r2 = xji*xji + yji*yji + zji*zji
            if (r2 .lt. bR22) then
              m = 0
              lok = .false.   
              do while (m.lt.nbopot.and..not.lok)
                m = m + 1   
                if (nati.eq.nBOspec1(m).and.(ntypi.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0).and. &
                    natj.eq.nBOspec2(m).and.(ntypj.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0)) then
                  lok = (r2.lt.rBOmax(m)**2)
                elseif (nati.eq.nBOspec2(m).and.(ntypi.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0).and. &
                    natj.eq.nBOspec1(m).and.(ntypj.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0)) then
                  lok = (r2.lt.rBOmax(m)**2)
                endif
              enddo
              if (lok) then
                if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                  lmaxneighok = .false.
                  nneigh(i) = nneigh(i) + 1
                else
                  rij = sqrt(r2)
                  nneigh(i) = nneigh(i) + 1
                  neighno(nneigh(i),i) = j
                  nBOpotptr(nneigh(i),i) = m
                  rneigh(nneigh(i),i) = rij
                  xneigh(nneigh(i),i) = xji
                  yneigh(nneigh(i),i) = yji
                  zneigh(nneigh(i),i) = zji
                endif
              endif
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
  return
  end
!
  subroutine getREBOneighbour(maxneigh,bR22,nREBObond,nneigh,neighno,rneigh, &
                              xneigh,yneigh,zneigh,latomdone,lmaxneighok, &
                              nREBOatom,nREBOatomptr,nREBOatomRptr)
!
!  Finds neighbours for a REBO potential
!
!  On entry : 
!
!  maxneigh      = size of neighbour arrays
!  bR2           = array of potential cutoffs for each bond type
!  nREBOatom     = number of REBO atoms
!  nREBOatomptr  = pointer to REBO atoms
!  nREBOatomRptr = pointer from REBO atoms to full set of atoms
!
!  On exit :
!
!  nREBObond     = pointer to potential number for each neighbour
!  nneigh        = number of neighbours for each atom
!  neighno       = pointer to atom numbers of neighbours for each atom
!  rneigh        = distances of neighbours for each atom
!  xneigh        = x component of distances of neighbours for each atom
!  yneigh        = y component of distances of neighbours for each atom
!  zneigh        = z component of distances of neighbours for each atom
!  latomdone     = logical indicating whether atom was done locally
!  lmaxneighok   = if .true. then dimension maxneigh was sufficient
!
!  10/04 Created from brenner.f
!  11/04 Error in use statements equivalencing to BO terms fixed
!   6/07 nREBOatom arrays added to handle atoms that have no REBO species
!   6/07 Structure of arrays for storing distribution changed to 1-D
!  12/07 lmaxneighok initialised on entry
!  12/07 Unused variables removed
!  12/07 lmaxneighok initialisation corrected
!   4/08 Modified for variable domain size in spatial case
!   4/08 xvec1cell replaced by xvec2cell etc for spatial algorithm
!  11/09 Parallelised and index assignment moved out of loop for speed
!  11/09 Standard algorithm made triangular
!  11/09 Separate algorithm introduced for standard parallel case that doesn't use
!        triangular trick since this will fail in global summation.
!   3/10 Adapted to follow getReaxFFneighbour and spatial case parallelised
!   6/10 In parallel maxneigh is now globalised to set overall value of lmaxneighok
!  11/10 Algorithm corrected to avoid duplicate images in serial non-spatial case.
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
!  Julian Gale, NRI, Curtin University, November 2010
!
  use datatypes
  use brennerdata,    only : nat2REBOspecies
  use current,        only : numat, nat, iimax, iimid
  use current,        only : xclat, yclat, zclat
  use current,        only : xvec1cell, yvec1cell, zvec1cell
  use current,        only : xvec2cell, yvec2cell, zvec2cell
  use parallel,       only : procid, nprocs
  use spatial,        only : lspatialok
  use spatial,        only : natomcell
  use spatial,        only : ncellsearch
  use spatial,        only : nspcell
  use spatial,        only : nspcellat
  use spatial,        only : nspcellatptr
  use spatial,        only : nspcellat1ptr
  use spatial,        only : nspcellatptrcell
  use spatial,        only : xinbox
  use spatial,        only : yinbox
  use spatial,        only : zinbox
  use times,          only : tsum
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: maxneigh
  integer(i4), intent(in)                        :: nREBOatom
  integer(i4), intent(in)                        :: nREBOatomptr(numat)
  integer(i4), intent(in)                        :: nREBOatomRptr(numat)
  integer(i4), intent(out)                       :: nneigh(nREBOatom)
  integer(i4), intent(out)                       :: nREBObond(maxneigh,nREBOatom)
  integer(i4), intent(out)                       :: neighno(maxneigh,nREBOatom)
  real(dp),    intent(in)                        :: bR22(*)
  real(dp),    intent(out)                       :: rneigh(maxneigh,nREBOatom)
  real(dp),    intent(out)                       :: xneigh(maxneigh,nREBOatom)
  real(dp),    intent(out)                       :: yneigh(maxneigh,nREBOatom)
  real(dp),    intent(out)                       :: zneigh(maxneigh,nREBOatom)
  logical,     intent(out)                       :: latomdone(nREBOatom)
  logical,     intent(out)                       :: lmaxneighok
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: iiimax
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: maxx
  integer(i4)                                    :: n1j
  integer(i4)                                    :: nati
  integer(i4)                                    :: natj
  integer(i4)                                    :: nj
  integer(i4)                                    :: nri
  integer(i4)                                    :: nrj
  integer(i4)                                    :: nREBObo
  integer(i4)                                    :: nREBOsi
  integer(i4)                                    :: nREBOsj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  integer(i4), dimension(:),   allocatable, save :: ntmp1
  integer(i4), dimension(:,:), allocatable, save :: ntmp2
  logical                                        :: lmaxneighok2
  real(dp)                                       :: cputime
  real(dp)                                       :: rij
  real(dp)                                       :: r2
  real(dp)                                       :: tsuml
  real(dp)                                       :: xi
  real(dp)                                       :: yi     
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
  real(dp),    dimension(:,:), allocatable, save :: rtmp2
!
!  Initialise lmaxneighok and nneigh
!
  lmaxneighok = .true.
!
!  For parallel execution we need to ensure that all arrays are initialised ahead of summation
!
  if (nprocs.gt.1) then
    neighno(1:maxneigh,1:numat) = 0
    nREBObond(1:maxneigh,1:numat) = 0
    rneigh(1:maxneigh,1:numat) = 0.0_dp
    xneigh(1:maxneigh,1:numat) = 0.0_dp
    yneigh(1:maxneigh,1:numat) = 0.0_dp
    zneigh(1:maxneigh,1:numat) = 0.0_dp
  endif
  nneigh(1:numat) = 0
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  if (lspatialok) then
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!  
!  Loop over all spatial cells
!     
    iloops: do i = procid+1,numat,nprocs
!
!  Check whether this is a bond order atom
!
      nri = nREBOatomRptr(i)
!
!  If i is not a REBO species skip
!
      if (nri.eq.0) cycle iloops
!
      ind1 = natomcell(i)
      ind2 = ind1 - 1
      iz = ind2/maxxy
      ind2 = ind2 - maxxy*iz
      iy = ind2/maxx
      ix = ind2 - maxx*iy + 1
      iy = iy + 1
      iz = iz + 1
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
      nneigh(nri) = 0
      latomdone(nri) = .true.
      nati = nat(i)
      nREBOsi = nat2REBOspecies(nati)
!       
!  Set coordinates
!
      xi = xinbox(i)
      yi = yinbox(i)
      zi = zinbox(i)
!
!  Loop over neighbouring cells
!
      do imz = nsplower(3),nspupper(3)
        do imy = nsplower(2),nspupper(2)
          do imx = nsplower(1),nspupper(1)
            ind2 = (imz-1)*maxxy + (imy-1)*maxx + imx
!                         
!  Loop over atoms within neighbouring cells  
!                         
            nj = nspcellat(ind2)
            n1j = nspcellat1ptr(ind2)
            jloops: do jj = 1,nj
              j = nspcellatptr(n1j+jj)
              nrj = nREBOatomRptr(j)
!
!  If j is not a REBO species skip
!
              if (nrj.eq.0) cycle jloops
!                     
!  Exclude self term    
!                         
              if (i.ne.j.or.ind1.ne.ind2) then
                jc = nspcellatptrcell(n1j+jj)
                natj = nat(j)
                nREBOsj = nat2REBOspecies(natj)
!
!  Set bond type indicator
!
                if (nREBOsi.ge.nREBOsj) then
                  nREBObo = nREBOsi*(nREBOsi - 1)/2 + nREBOsj
                else
                  nREBObo = nREBOsj*(nREBOsj - 1)/2 + nREBOsi
                endif
!                             
!  Set centre cell coordinate differences
!  
                xji = xvec2cell(jc) + xinbox(j) - xi
                yji = yvec2cell(jc) + yinbox(j) - yi
                zji = zvec2cell(jc) + zinbox(j) - zi
!  
                r2 = xji*xji + yji*yji + zji*zji
                if (r2 .lt. bR22(nREBObo)) then
                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(nri) = nneigh(nri) + 1
                  else        
                    rij = sqrt(r2)
                    nneigh(nri) = nneigh(nri) + 1
                    neighno(nneigh(nri),nri) = j
                    nREBObond(nneigh(nri),nri) = nREBObo
                    rneigh(nneigh(nri),nri) = rij
                    xneigh(nneigh(nri),nri) = xji
                    yneigh(nneigh(nri),nri) = yji
                    zneigh(nneigh(nri),nri) = zji
                  endif
                endif
              endif
!                             
            enddo jloops
          enddo
        enddo
      enddo             
!                                 
    enddo iloops
  else
    if (nprocs.gt.1) then
!-----------------------------------------------------------------
!  Parallel - find all neighbours for each atom on the same node |
!-----------------------------------------------------------------
      do nri = procid+1,nREBOatom,nprocs
        i = nREBOatomptr(nri)
        nneigh(nri) = 0
        nati = nat(i)
        nREBOsi = nat2REBOspecies(nati)
        latomdone(nri) = .true.
!
!  Loop over atoms
!
        do nrj = 1,nREBOatom
          j = nREBOatomptr(nrj)
          natj = nat(j)
          nREBOsj = nat2REBOspecies(natj)
!                               
!  Set bond type indicator      
!                             
          if (nREBOsi.ge.nREBOsj) then
            nREBObo = nREBOsi*(nREBOsi - 1)/2 + nREBOsj
          else  
            nREBObo = nREBOsj*(nREBOsj - 1)/2 + nREBOsi
          endif 
!
!  Set centre cell coordinate differences
!
          xji0 = xclat(j) - xclat(i)
          yji0 = yclat(j) - yclat(i)
          zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
          do ii = 1,iimax
!
!  Exclude self term
!
            if (i.ne.j.or.ii.ne.iimid) then
              xji = xji0 + xvec1cell(ii)
              yji = yji0 + yvec1cell(ii)
              zji = zji0 + zvec1cell(ii)
              r2 = xji*xji + yji*yji + zji*zji
              if (r2 .lt. bR22(nREBObo)) then
                if (nneigh(nri).ge.maxneigh.or..not.lmaxneighok) then
                  lmaxneighok = .false.
                  nneigh(nri) = nneigh(nri) + 1
                else
                  rij = sqrt(r2)
                  nneigh(nri) = nneigh(nri) + 1
                  neighno(nneigh(nri),nri) = j
                  nREBObond(nneigh(nri),nri) = nREBObo
                  rneigh(nneigh(nri),nri) = rij
                  xneigh(nneigh(nri),nri) = xji
                  yneigh(nneigh(nri),nri) = yji
                  zneigh(nneigh(nri),nri) = zji
                endif
              endif
            endif
          enddo
        enddo
      enddo
    else
!-------------------------------------------------
!  Serial - loop over unique pairs of atoms only |
!-------------------------------------------------
      do nri = 1,nREBOatom
        i = nREBOatomptr(nri)
        nneigh(nri) = 0
        nati = nat(i)
        nREBOsi = nat2REBOspecies(nati)
        latomdone(nri) = .true.
!
!  Loop over atoms
!
        do nrj = 1,nri
          j = nREBOatomptr(nrj)
          natj = nat(j)
          nREBOsj = nat2REBOspecies(natj)
!                               
!  Set bond type indicator      
!                             
          if (nREBOsi.ge.nREBOsj) then
            nREBObo = nREBOsi*(nREBOsi - 1)/2 + nREBOsj
          else  
            nREBObo = nREBOsj*(nREBOsj - 1)/2 + nREBOsi
          endif 
!
!  Set centre cell coordinate differences
!
          xji0 = xclat(j) - xclat(i)
          yji0 = yclat(j) - yclat(i)
          zji0 = zclat(j) - zclat(i)
!
!  For self-term then need to limit vector search to half of total
!
          if (i.eq.j) then
            iiimax = iimid
          else
            iiimax = iimax
          endif
!
!  Loop over unit cells
!
          do ii = 1,iiimax
!
!  Exclude self term
!
            if (i.ne.j.or.ii.ne.iimid) then
              xji = xji0 + xvec1cell(ii)
              yji = yji0 + yvec1cell(ii)
              zji = zji0 + zvec1cell(ii)
              r2 = xji*xji + yji*yji + zji*zji
              if (r2 .lt. bR22(nREBObo)) then
                if (nneigh(nri).ge.maxneigh.or.nneigh(nrj).ge.maxneigh.or..not.lmaxneighok) then
                  lmaxneighok = .false.
                  nneigh(nri) = nneigh(nri) + 1
                  nneigh(nrj) = nneigh(nrj) + 1
                else
                  rij = sqrt(r2)
                  nneigh(nri) = nneigh(nri) + 1
                  neighno(nneigh(nri),nri) = j
                  nREBObond(nneigh(nri),nri) = nREBObo
                  rneigh(nneigh(nri),nri) = rij
                  xneigh(nneigh(nri),nri) = xji
                  yneigh(nneigh(nri),nri) = yji
                  zneigh(nneigh(nri),nri) = zji
!
                  nneigh(nrj) = nneigh(nrj) + 1
                  neighno(nneigh(nrj),nrj) = i
                  nREBObond(nneigh(nrj),nrj) = nREBObo
                  rneigh(nneigh(nrj),nrj) = rij
                  xneigh(nneigh(nrj),nrj) = - xji
                  yneigh(nneigh(nrj),nrj) = - yji
                  zneigh(nneigh(nrj),nrj) = - zji
                endif
              endif
            endif
          enddo
        enddo
      enddo
    endif
  endif
!
!  Globalisation of data in parallel
!
  if (nprocs.gt.1) then
    tsuml = cputime()
!
!  Globalise value of lmaxneighok
!
    call landall(lmaxneighok,lmaxneighok2,1,"lmaxneighok","getREBOneighbour")
    lmaxneighok = lmaxneighok2
!
    if (lmaxneighok) then
!
!  Sum all data
!
      allocate(ntmp1(nREBOatom))
      call isumall(nneigh,ntmp1,nREBOatom,"nneigh","getREBOneighbour")
      nneigh(1:nREBOatom) = ntmp1(1:nREBOatom)
      deallocate(ntmp1)
      allocate(ntmp2(maxneigh,nREBOatom))
      call isumall(neighno,ntmp2,nREBOatom*maxneigh,"neighno","getREBOneighbour")
      do i = 1,nREBOatom
        neighno(1:nneigh(i),i) = ntmp2(1:nneigh(i),i)
      enddo
      call isumall(nREBObond,ntmp2,nREBOatom*maxneigh,"neighno","getREBOneighbour")
      do i = 1,nREBOatom
        nREBObond(1:nneigh(i),i) = ntmp2(1:nneigh(i),i)
      enddo
      deallocate(ntmp2)
      allocate(rtmp2(maxneigh,nREBOatom))
      call sumall(rneigh,rtmp2,nREBOatom*maxneigh,"rneigh","getREBOneighbour")
      do i = 1,nREBOatom
        rneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(xneigh,rtmp2,nREBOatom*maxneigh,"xneigh","getREBOneighbour")
      do i = 1,nREBOatom
        xneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(yneigh,rtmp2,nREBOatom*maxneigh,"yneigh","getREBOneighbour")
      do i = 1,nREBOatom
        yneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(zneigh,rtmp2,nREBOatom*maxneigh,"zneigh","getREBOneighbour")
      do i = 1,nREBOatom
        zneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      deallocate(rtmp2)
!
!  Set latomdone to true for all atoms since this has been achieved by globalisation
!
      latomdone(1:numat) = .true.
    else
!
!  If maxneigh has been exceed only globalise nneigh
!
      allocate(ntmp1(nREBOatom))
      call isumall(nneigh,ntmp1,nREBOatom,"nneigh","get2sREBOFFneighbour")
      nneigh(1:nREBOatom) = ntmp1(1:nREBOatom)
      deallocate(ntmp1)
    endif
    tsum = tsum + cputime() - tsuml
  endif
!
  return
  end
!
  subroutine getReaxFFneighbour(maxneigh,nbosptr,nneigh,neighno,rneigh, &
                                xneigh,yneigh,zneigh,latomdone,lmaxneighok)
!
!  Finds neighbours for a reaxFF potential
!
!  On entry : 
!
!  maxneigh      = size of neighbour arrays
!  rBOcutmax     = array of maximum potential cutoffs for each atom
!  nbosptr       = pointer from atom number to reaxFF species
!
!  On exit :
!
!  nneigh        = number of neighbours for each atom
!  neighno       = pointer to atom numbers of neighbours for each atom
!  rneigh        = distances of neighbours for each atom
!  xneigh        = x component of distances of neighbours for each atom
!  yneigh        = y component of distances of neighbours for each atom
!  zneigh        = z component of distances of neighbours for each atom
!  latomdone     = logical indicating whether atom was done locally
!  lmaxneighok   = if .true. then dimension maxneigh was sufficient
!
!   7/07 Created from getBOneighbour
!   9/07 Modified to allow for more than 1 cell vector being search in each direction
!  12/07 lmaxneighok initialised on entry
!  12/07 lmaxneighok initialisation corrected
!   4/08 Modified for variable domain size in spatial case
!   4/08 Modified to include all distances < reaxFFrhtol if atom is hydrogen
!   4/08 Previous change reversed as hydrogen bonds are now calculated separately
!   4/08 Use of rBOcutmax array replaced by reaxFFrmax
!   4/08 Spatial decomposition version modified so that outer loop is over atoms
!        and not cells to avoid duplication of work.
!   6/08 Modified to exclude atoms that don't have a ReaxFF species
!  11/09 Parallelised and index assignment moved out of loop for speed
!  11/09 Standard algorithm made triangular
!  11/09 Separate algorithm introduced for standard parallel case that doesn't use 
!        triangular trick since this will fail in global summation.
!   3/10 Spatial case parallelised
!   6/10 In parallel maxneigh is now globalised to set overall value of lmaxneighok
!  11/10 Algorithm corrected to avoid duplicate images in serial non-spatial case.
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
!  Julian Gale, NRI, Curtin University, November 2010
!
  use datatypes
  use current,        only : numat
  use current,        only : xclat, yclat, zclat
  use current,        only : xvec2cell, yvec2cell, zvec2cell
  use parallel,       only : procid, nprocs
  use reaxFFdata,     only : reaxFFrmaxpair, reaxFFcutoff, reaxFFrmax
  use spatialbo,      only : lspatialok => lspatialBOok
  use spatialbo,      only : natomcell => natomcellbo
  use spatialbo,      only : ncellsearch => ncellsearchbo
  use spatialbo,      only : nspcell => nspcellbo
  use spatialbo,      only : nspcellat => nspcellatbo
  use spatialbo,      only : nspcellatptr => nspcellatptrbo
  use spatialbo,      only : nspcellat1ptr => nspcellat1ptrbo
  use spatialbo,      only : nspcellatptrcell => nspcellatptrcellbo
  use spatialbo,      only : xinbox => xinboxbo
  use spatialbo,      only : yinbox => yinboxbo
  use spatialbo,      only : zinbox => zinboxbo
  use times,          only : tsum
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: maxneigh
  integer(i4), intent(out)                       :: nneigh(numat)
  integer(i4), intent(out)                       :: neighno(maxneigh,numat)
  integer(i4), intent(in)                        :: nbosptr(numat)
  real(dp),    intent(out)                       :: rneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: xneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: yneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: zneigh(maxneigh,numat)
  logical,     intent(out)                       :: latomdone(numat)
  logical,     intent(out)                       :: lmaxneighok
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: imax
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: jmax
  integer(i4)                                    :: kmax
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: maxx
  integer(i4)                                    :: n1j
  integer(i4)                                    :: nj
  integer(i4)                                    :: nmiddle
  integer(i4)                                    :: nspeci
  integer(i4)                                    :: nspecj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  integer(i4)                                    :: nnvector
  integer(i4)                                    :: nvector
  integer(i4),                              save :: maxvector = 125
  integer(i4)                                    :: status
  integer(i4), dimension(:),   allocatable, save :: ntmp1
  integer(i4), dimension(:,:), allocatable, save :: ntmp2
  logical                                        :: lmaxneighok2
  real(dp)                                       :: cputime
  real(dp)                                       :: bR22
  real(dp)                                       :: bR22i
  real(dp)                                       :: bR22j
  real(dp)                                       :: rij
  real(dp)                                       :: r2
  real(dp)                                       :: tsuml
  real(dp)                                       :: xi
  real(dp)                                       :: yi     
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
  real(dp),    dimension(:),   allocatable, save :: xvec
  real(dp),    dimension(:),   allocatable, save :: yvec
  real(dp),    dimension(:),   allocatable, save :: zvec
  real(dp),    dimension(:,:), allocatable, save :: rtmp2
!
!  Initialise lmaxneighok and nneigh
!
  lmaxneighok = .true.
!
!  For parallel execution we need to ensure that all arrays are initialised ahead of summation
!  
  if (nprocs.gt.1) then
    neighno(1:maxneigh,1:numat) = 0
    rneigh(1:maxneigh,1:numat) = 0.0_dp
    xneigh(1:maxneigh,1:numat) = 0.0_dp
    yneigh(1:maxneigh,1:numat) = 0.0_dp
    zneigh(1:maxneigh,1:numat) = 0.0_dp
  endif
  nneigh(1:numat) = 0
  latomdone(1:numat) = .true.
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  if (lspatialok) then
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!  
!  Loop over all spatial cells
!     
    iloops: do i = procid+1,numat,nprocs
!
!  Check whether this is a bond order atom
!
      nspeci = nbosptr(i)
      if (nspeci.eq.0) cycle iloops
!
      ind1 = natomcell(i)
      ind2 = ind1 - 1
      iz = ind2/maxxy
      ind2 = ind2 - maxxy*iz
      iy = ind2/maxx
      ix = ind2 - maxx*iy + 1
      iy = iy + 1
      iz = iz + 1
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
      nneigh(i) = 0
      latomdone(i) = .true.
!       
!  Set coordinates
!
      xi = xinbox(i)
      yi = yinbox(i)
      zi = zinbox(i)
!
!  Compute square of cut-off for distance checking
!
      bR22 = reaxFFrmax(nbosptr(i))**2
!
!  Loop over neighbouring cells
!
      do imz = nsplower(3),nspupper(3)
        do imy = nsplower(2),nspupper(2)
          do imx = nsplower(1),nspupper(1)
            ind2 = (imz-1)*maxxy + (imy-1)*maxx + imx
!                         
!  Loop over atoms within neighbouring cells  
!                         
            nj = nspcellat(ind2)
            n1j = nspcellat1ptr(ind2)
            jloops: do jj = 1,nj
              j = nspcellatptr(n1j+jj)
!
!  Check whether this is a bond order atom
!
              nspecj = nbosptr(j)
              if (nspecj.eq.0) cycle jloops
!                     
!  Exclude self term    
!                         
              if (i.ne.j.or.ind1.ne.ind2) then
                jc = nspcellatptrcell(n1j+jj)
!                             
!  Set centre cell coordinate differences
!  
                xji = xvec2cell(jc) + xinbox(j) - xi
                yji = yvec2cell(jc) + yinbox(j) - yi
                zji = zvec2cell(jc) + zinbox(j) - zi
!  
                r2 = xji*xji + yji*yji + zji*zji
                if (r2 .lt. bR22) then
!
!  j is within overall two body cut-off - now check pairwise cutoff
!
                  if (nspeci.ge.nspecj) then
                    ind = nspeci*(nspeci-1)/2 + nspecj
                  else
                    ind = nspecj*(nspecj-1)/2 + nspeci
                  endif
                  if (r2.lt.reaxFFrmaxpair(ind)**2) then
                    if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                      lmaxneighok = .false.
                      nneigh(i) = nneigh(i) + 1
                    else        
                      rij = sqrt(r2)
                      nneigh(i) = nneigh(i) + 1
                      neighno(nneigh(i),i) = j
                      rneigh(nneigh(i),i) = rij
                      xneigh(nneigh(i),i) = xji
                      yneigh(nneigh(i),i) = yji
                      zneigh(nneigh(i),i) = zji
                    endif
                  endif
                endif
              endif
!                             
            enddo jloops
          enddo
        enddo
      enddo             
!                                 
    enddo iloops
  else
!
!  Set up memory for lattice vectors
!
100 continue
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('getReaxFFneighbour','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('getReaxFFneighbour','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('getReaxFFneighbour','zvec')
!
!  Find lattice vectors needed
!
    call rtlist(nvector,reaxFFcutoff,xvec,yvec,zvec,imax,jmax,kmax,nmiddle,maxvector)
    if (nvector.gt.maxvector) then
      maxvector = nvector
      deallocate(zvec,stat=status)
      if (status/=0) call deallocate_error('getReaxFFneighbour','zvec')
      deallocate(yvec,stat=status)
      if (status/=0) call deallocate_error('getReaxFFneighbour','yvec')
      deallocate(xvec,stat=status)
      if (status/=0) call deallocate_error('getReaxFFneighbour','xvec')
      goto 100
    endif
    if (nprocs.gt.1) then
!-----------------------------------------------------------------
!  Parallel - find all neighbours for each atom on the same node |
!-----------------------------------------------------------------
!
!  Loop over atoms
!
      piloop: do i = procid+1,numat,nprocs
!
!  Check whether this is a bond order atom
!
        nspeci = nbosptr(i)
        if (nspeci.eq.0) cycle piloop
!
!  Compute square of cut-off for distance checking
!
        bR22i = reaxFFrmax(nbosptr(i))**2
!
!  Loop over atoms
!
        pjloop: do j = 1,numat
!
!  Check whether this is a bond order atom
!
          nspecj = nbosptr(j)
          if (nspecj.eq.0) cycle pjloop
!
!  Find pair index
!
          if (nspeci.ge.nspecj) then
            ind = nspeci*(nspeci-1)/2 + nspecj
          else  
            ind = nspecj*(nspecj-1)/2 + nspeci
          endif
!
!  Set centre cell coordinate differences
!
          xji0 = xclat(j) - xclat(i)
          yji0 = yclat(j) - yclat(i)
          zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
          do ii = 1,nvector
!
!  Exclude self term
!
            if (i.ne.j.or.ii.ne.nmiddle) then
              xji = xji0 + xvec(ii)
              yji = yji0 + yvec(ii)
              zji = zji0 + zvec(ii)
              r2 = xji*xji + yji*yji + zji*zji
              if (r2.lt.bR22i) then
                if (r2.lt.reaxFFrmaxpair(ind)**2) then
                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(i) = nneigh(i) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(i) + 1
                    neighno(nneigh(i),i) = j
                    rneigh(nneigh(i),i) = rij
                    xneigh(nneigh(i),i) = xji
                    yneigh(nneigh(i),i) = yji
                    zneigh(nneigh(i),i) = zji
                  endif
                endif
              endif
            endif
          enddo
        enddo pjloop
      enddo piloop
    else
!-------------------------------------------------
!  Serial - loop over unique pairs of atoms only |
!-------------------------------------------------
!
!  Loop over atoms
!
      siloop: do i = 1,numat
!
!  Check whether this is a bond order atom
!
        nspeci = nbosptr(i)
        if (nspeci.eq.0) cycle siloop
!
!  Compute square of cut-off for distance checking
!
        bR22i = reaxFFrmax(nbosptr(i))**2
!
!  Loop over atoms
!
        sjloop: do j = 1,i
!
!  Check whether this is a bond order atom
!
          nspecj = nbosptr(j)
          if (nspecj.eq.0) cycle sjloop
!
!  Compute square of cut-off for distance checking
!
          bR22j = reaxFFrmax(nbosptr(j))**2
!
!  Find pair index
!
          if (nspeci.ge.nspecj) then
            ind = nspeci*(nspeci-1)/2 + nspecj
          else  
            ind = nspecj*(nspecj-1)/2 + nspeci
          endif
!
!  Set centre cell coordinate differences
!
          xji0 = xclat(j) - xclat(i)
          yji0 = yclat(j) - yclat(i)
          zji0 = zclat(j) - zclat(i)
!
!  For self-term then need to limit vector search to half of total
!
          if (i.eq.j) then
            nnvector = nmiddle
          else
            nnvector = nvector
          endif
!
!  Loop over unit cells
!
          do ii = 1,nnvector
!
!  Exclude self term
!
            if (i.ne.j.or.ii.ne.nmiddle) then
              xji = xji0 + xvec(ii)
              yji = yji0 + yvec(ii)
              zji = zji0 + zvec(ii)
              r2 = xji*xji + yji*yji + zji*zji
              if (r2.lt.reaxFFrmaxpair(ind)**2) then
                if (r2.lt.bR22i.and.r2.lt.bR22j) then
                  if (nneigh(i).ge.maxneigh.or.nneigh(j).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(i) = nneigh(i) + 1
                    nneigh(j) = nneigh(j) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(i) + 1
                    neighno(nneigh(i),i) = j
                    rneigh(nneigh(i),i) = rij
                    xneigh(nneigh(i),i) = xji
                    yneigh(nneigh(i),i) = yji
                    zneigh(nneigh(i),i) = zji
                    nneigh(j) = nneigh(j) + 1
                    neighno(nneigh(j),j) = i
                    rneigh(nneigh(j),j) = rij
                    xneigh(nneigh(j),j) = - xji
                    yneigh(nneigh(j),j) = - yji
                    zneigh(nneigh(j),j) = - zji
                  endif
                elseif (r2.lt.bR22i) then
                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(i) = nneigh(i) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(i) + 1
                    neighno(nneigh(i),i) = j
                    rneigh(nneigh(i),i) = rij
                    xneigh(nneigh(i),i) = xji
                    yneigh(nneigh(i),i) = yji
                    zneigh(nneigh(i),i) = zji
                  endif
                elseif (r2.lt.bR22j) then
                  if (nneigh(j).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(j) = nneigh(j) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(j) + 1
                    neighno(nneigh(j),j) = i
                    rneigh(nneigh(j),j) = rij
                    xneigh(nneigh(j),j) = - xji
                    yneigh(nneigh(j),j) = - yji
                    zneigh(nneigh(j),j) = - zji
                  endif
                endif
              endif
            endif
          enddo
        enddo sjloop
      enddo siloop
    endif
!
!  Free memory for lattice vectors
!
    deallocate(zvec,stat=status)
    if (status/=0) call deallocate_error('getReaxFFneighbour','zvec')
    deallocate(yvec,stat=status)
    if (status/=0) call deallocate_error('getReaxFFneighbour','yvec')
    deallocate(xvec,stat=status)
    if (status/=0) call deallocate_error('getReaxFFneighbour','xvec')
  endif
!
!  Globalisation of data in parallel
!
  if (nprocs.gt.1) then
    tsuml = cputime()
!
!  Globalise value of lmaxneighok
!
    call landall(lmaxneighok,lmaxneighok2,1,"lmaxneighok","getReaxFFneighbour")
    lmaxneighok = lmaxneighok2
!
    if (lmaxneighok) then
!
!  Sum all data
!
      allocate(ntmp1(numat))
      call isumall(nneigh,ntmp1,numat,"nneigh","getReaxFFneighbour")
      nneigh(1:numat) = ntmp1(1:numat)
      deallocate(ntmp1)
      allocate(ntmp2(maxneigh,numat))
      call isumall(neighno,ntmp2,numat*maxneigh,"neighno","getReaxFFneighbour")
      do i = 1,numat
        neighno(1:nneigh(i),i) = ntmp2(1:nneigh(i),i)
      enddo
      deallocate(ntmp2)
      allocate(rtmp2(maxneigh,numat))
      call sumall(rneigh,rtmp2,numat*maxneigh,"rneigh","getReaxFFneighbour")
      do i = 1,numat
        rneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(xneigh,rtmp2,numat*maxneigh,"xneigh","getReaxFFneighbour")
      do i = 1,numat
        xneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(yneigh,rtmp2,numat*maxneigh,"yneigh","getReaxFFneighbour")
      do i = 1,numat
        yneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(zneigh,rtmp2,numat*maxneigh,"zneigh","getReaxFFneighbour")
      do i = 1,numat
        zneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      deallocate(rtmp2)
    else
!
!  If maxneigh has been exceed only globalise nneigh
!
      allocate(ntmp1(numat))
      call isumall(nneigh,ntmp1,numat,"nneigh","getReaxFFneighbour")
      nneigh(1:numat) = ntmp1(1:numat)
      deallocate(ntmp1)
    endif
    tsum = tsum + cputime() - tsuml
  endif
!
  return
  end
!
  subroutine getEDIPneighbour(maxneigh,nbosptr,nneigh,neighno,rneigh, &
                              xneigh,yneigh,zneigh,latomdone,lmaxneighok)
!
!  Finds neighbours for a EDIP potential
!
!  On entry : 
!
!  maxneigh      = size of neighbour arrays
!  rBOcutmax     = array of maximum potential cutoffs for each atom
!  nbosptr       = pointer from atom number to EDIP species
!
!  On exit :
!
!  nneigh        = number of neighbours for each atom
!  neighno       = pointer to atom numbers of neighbours for each atom
!  rneigh        = distances of neighbours for each atom
!  xneigh        = x component of distances of neighbours for each atom
!  yneigh        = y component of distances of neighbours for each atom
!  zneigh        = z component of distances of neighbours for each atom
!  latomdone     = logical indicating whether atom was done locally
!  lmaxneighok   = if .true. then dimension maxneigh was sufficient
!
!   9/10 Created from getReaxFFneighbour
!  11/10 Algorithm corrected to avoid duplicate images in serial non-spatial case.
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
!  Julian Gale, NRI, Curtin University, November 2010
!
  use datatypes
  use current,        only : numat
  use current,        only : xclat, yclat, zclat
  use current,        only : xvec2cell, yvec2cell, zvec2cell
  use parallel,       only : procid, nprocs
  use EDIPdata,       only : EDIPrmaxpair, EDIPcutoff, EDIPrmax
  use spatialbo,      only : lspatialok => lspatialBOok
  use spatialbo,      only : natomcell => natomcellbo
  use spatialbo,      only : ncellsearch => ncellsearchbo
  use spatialbo,      only : nspcell => nspcellbo
  use spatialbo,      only : nspcellat => nspcellatbo
  use spatialbo,      only : nspcellatptr => nspcellatptrbo
  use spatialbo,      only : nspcellat1ptr => nspcellat1ptrbo
  use spatialbo,      only : nspcellatptrcell => nspcellatptrcellbo
  use spatialbo,      only : xinbox => xinboxbo
  use spatialbo,      only : yinbox => yinboxbo
  use spatialbo,      only : zinbox => zinboxbo
  use times,          only : tsum
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: maxneigh
  integer(i4), intent(out)                       :: nneigh(numat)
  integer(i4), intent(out)                       :: neighno(maxneigh,numat)
  integer(i4), intent(in)                        :: nbosptr(numat)
  real(dp),    intent(out)                       :: rneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: xneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: yneigh(maxneigh,numat)
  real(dp),    intent(out)                       :: zneigh(maxneigh,numat)
  logical,     intent(out)                       :: latomdone(numat)
  logical,     intent(out)                       :: lmaxneighok
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: imax
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: jmax
  integer(i4)                                    :: kmax
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: maxx
  integer(i4)                                    :: n1j
  integer(i4)                                    :: nj
  integer(i4)                                    :: nmiddle
  integer(i4)                                    :: nspeci
  integer(i4)                                    :: nspecj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  integer(i4)                                    :: nnvector
  integer(i4)                                    :: nvector
  integer(i4),                              save :: maxvector = 125
  integer(i4)                                    :: status
  integer(i4), dimension(:),   allocatable, save :: ntmp1
  integer(i4), dimension(:,:), allocatable, save :: ntmp2
  logical                                        :: lmaxneighok2
  real(dp)                                       :: cputime
  real(dp)                                       :: bR22
  real(dp)                                       :: bR22i
  real(dp)                                       :: bR22j
  real(dp)                                       :: rij
  real(dp)                                       :: r2
  real(dp)                                       :: tsuml
  real(dp)                                       :: xi
  real(dp)                                       :: yi     
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
  real(dp),    dimension(:),   allocatable, save :: xvec
  real(dp),    dimension(:),   allocatable, save :: yvec
  real(dp),    dimension(:),   allocatable, save :: zvec
  real(dp),    dimension(:,:), allocatable, save :: rtmp2
!
!  Initialise lmaxneighok and nneigh
!
  lmaxneighok = .true.
!
!  For parallel execution we need to ensure that all arrays are initialised ahead of summation
!  
  if (nprocs.gt.1) then
    neighno(1:maxneigh,1:numat) = 0
    rneigh(1:maxneigh,1:numat) = 0.0_dp
    xneigh(1:maxneigh,1:numat) = 0.0_dp
    yneigh(1:maxneigh,1:numat) = 0.0_dp
    zneigh(1:maxneigh,1:numat) = 0.0_dp
  endif
  nneigh(1:numat) = 0
  latomdone(1:numat) = .true.
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  if (lspatialok) then
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!  
!  Loop over all spatial cells
!     
    iloops: do i = procid+1,numat,nprocs
!
!  Check whether this is a bond order atom
!
      nspeci = nbosptr(i)
      if (nspeci.eq.0) cycle iloops
!
      ind1 = natomcell(i)
      ind2 = ind1 - 1
      iz = ind2/maxxy
      ind2 = ind2 - maxxy*iz
      iy = ind2/maxx
      ix = ind2 - maxx*iy + 1
      iy = iy + 1
      iz = iz + 1
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
      nneigh(i) = 0
      latomdone(i) = .true.
!       
!  Set coordinates
!
      xi = xinbox(i)
      yi = yinbox(i)
      zi = zinbox(i)
!
!  Compute square of cut-off for distance checking
!
      bR22 = EDIPrmax(nbosptr(i))**2
!
!  Loop over neighbouring cells
!
      do imz = nsplower(3),nspupper(3)
        do imy = nsplower(2),nspupper(2)
          do imx = nsplower(1),nspupper(1)
            ind2 = (imz-1)*maxxy + (imy-1)*maxx + imx
!                         
!  Loop over atoms within neighbouring cells  
!                         
            nj = nspcellat(ind2)
            n1j = nspcellat1ptr(ind2)
            jloops: do jj = 1,nj
              j = nspcellatptr(n1j+jj)
!
!  Check whether this is a bond order atom
!
              nspecj = nbosptr(j)
              if (nspecj.eq.0) cycle jloops
!                     
!  Exclude self term    
!                         
              if (i.ne.j.or.ind1.ne.ind2) then
                jc = nspcellatptrcell(n1j+jj)
!                             
!  Set centre cell coordinate differences
!  
                xji = xvec2cell(jc) + xinbox(j) - xi
                yji = yvec2cell(jc) + yinbox(j) - yi
                zji = zvec2cell(jc) + zinbox(j) - zi
!  
                r2 = xji*xji + yji*yji + zji*zji
                if (r2 .lt. bR22) then
!
!  j is within overall two body cut-off - now check pairwise cutoff
!
                  if (nspeci.ge.nspecj) then
                    ind = nspeci*(nspeci-1)/2 + nspecj
                  else
                    ind = nspecj*(nspecj-1)/2 + nspeci
                  endif
                  if (r2.lt.EDIPrmaxpair(ind)**2) then
                    if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                      lmaxneighok = .false.
                      nneigh(i) = nneigh(i) + 1
                    else        
                      rij = sqrt(r2)
                      nneigh(i) = nneigh(i) + 1
                      neighno(nneigh(i),i) = j
                      rneigh(nneigh(i),i) = rij
                      xneigh(nneigh(i),i) = xji
                      yneigh(nneigh(i),i) = yji
                      zneigh(nneigh(i),i) = zji
                    endif
                  endif
                endif
              endif
!                             
            enddo jloops
          enddo
        enddo
      enddo             
!                                 
    enddo iloops
  else
!
!  Set up memory for lattice vectors
!
100 continue
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('getEDIPneighbour','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('getEDIPneighbour','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('getEDIPneighbour','zvec')
!
!  Find lattice vectors needed
!
    call rtlist(nvector,EDIPcutoff,xvec,yvec,zvec,imax,jmax,kmax,nmiddle,maxvector)
    if (nvector.gt.maxvector) then
      maxvector = nvector
      deallocate(zvec,stat=status)
      if (status/=0) call deallocate_error('getEDIPneighbour','zvec')
      deallocate(yvec,stat=status)
      if (status/=0) call deallocate_error('getEDIPneighbour','yvec')
      deallocate(xvec,stat=status)
      if (status/=0) call deallocate_error('getEDIPneighbour','xvec')
      goto 100
    endif
    if (nprocs.gt.1) then
!-----------------------------------------------------------------
!  Parallel - find all neighbours for each atom on the same node |
!-----------------------------------------------------------------
!
!  Loop over atoms
!
      piloop: do i = procid+1,numat,nprocs
!
!  Check whether this is a bond order atom
!
        nspeci = nbosptr(i)
        if (nspeci.eq.0) cycle piloop
!
!  Compute square of cut-off for distance checking
!
        bR22i = EDIPrmax(nbosptr(i))**2
!
!  Loop over atoms
!
        pjloop: do j = 1,numat
!
!  Check whether this is a bond order atom
!
          nspecj = nbosptr(j)
          if (nspecj.eq.0) cycle pjloop
!
!  Find pair index
!
          if (nspeci.ge.nspecj) then
            ind = nspeci*(nspeci-1)/2 + nspecj
          else  
            ind = nspecj*(nspecj-1)/2 + nspeci
          endif
!
!  Set centre cell coordinate differences
!
          xji0 = xclat(j) - xclat(i)
          yji0 = yclat(j) - yclat(i)
          zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
          do ii = 1,nvector
!
!  Exclude self term
!
            if (i.ne.j.or.ii.ne.nmiddle) then
              xji = xji0 + xvec(ii)
              yji = yji0 + yvec(ii)
              zji = zji0 + zvec(ii)
              r2 = xji*xji + yji*yji + zji*zji
              if (r2.lt.bR22i) then
                if (r2.lt.EDIPrmaxpair(ind)**2) then
                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(i) = nneigh(i) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(i) + 1
                    neighno(nneigh(i),i) = j
                    rneigh(nneigh(i),i) = rij
                    xneigh(nneigh(i),i) = xji
                    yneigh(nneigh(i),i) = yji
                    zneigh(nneigh(i),i) = zji
                  endif
                endif
              endif
            endif
          enddo
        enddo pjloop
      enddo piloop
    else
!-------------------------------------------------
!  Serial - loop over unique pairs of atoms only |
!-------------------------------------------------
!
!  Loop over atoms
!
      siloop: do i = 1,numat
!
!  Check whether this is a bond order atom
!
        nspeci = nbosptr(i)
        if (nspeci.eq.0) cycle siloop
!
!  Compute square of cut-off for distance checking
!
        bR22i = EDIPrmax(nbosptr(i))**2
!
!  Loop over atoms
!
        sjloop: do j = 1,i
!
!  Check whether this is a bond order atom
!
          nspecj = nbosptr(j)
          if (nspecj.eq.0) cycle sjloop
!
!  Compute square of cut-off for distance checking
!
          bR22j = EDIPrmax(nbosptr(j))**2
!
!  Find pair index
!
          if (nspeci.ge.nspecj) then
            ind = nspeci*(nspeci-1)/2 + nspecj
          else  
            ind = nspecj*(nspecj-1)/2 + nspeci
          endif
!
!  Set centre cell coordinate differences
!
          xji0 = xclat(j) - xclat(i)
          yji0 = yclat(j) - yclat(i)
          zji0 = zclat(j) - zclat(i)
!
!  For self-term then need to limit vector search to half of total
!
          if (i.eq.j) then
            nnvector = nmiddle
          else
            nnvector = nvector
          endif
!
!  Loop over unit cells
!
          do ii = 1,nnvector
!
!  Exclude self term
!
            if (i.ne.j.or.ii.ne.nmiddle) then
              xji = xji0 + xvec(ii)
              yji = yji0 + yvec(ii)
              zji = zji0 + zvec(ii)
              r2 = xji*xji + yji*yji + zji*zji
              if (r2.lt.EDIPrmaxpair(ind)**2) then
                if (r2.lt.bR22i.and.r2.lt.bR22j) then
                  if (nneigh(i).ge.maxneigh.or.nneigh(j).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(i) = nneigh(i) + 1
                    nneigh(j) = nneigh(j) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(i) + 1
                    neighno(nneigh(i),i) = j
                    rneigh(nneigh(i),i) = rij
                    xneigh(nneigh(i),i) = xji
                    yneigh(nneigh(i),i) = yji
                    zneigh(nneigh(i),i) = zji
                    nneigh(j) = nneigh(j) + 1
                    neighno(nneigh(j),j) = i
                    rneigh(nneigh(j),j) = rij
                    xneigh(nneigh(j),j) = - xji
                    yneigh(nneigh(j),j) = - yji
                    zneigh(nneigh(j),j) = - zji
                  endif
                elseif (r2.lt.bR22i) then
                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(i) = nneigh(i) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(i) + 1
                    neighno(nneigh(i),i) = j
                    rneigh(nneigh(i),i) = rij
                    xneigh(nneigh(i),i) = xji
                    yneigh(nneigh(i),i) = yji
                    zneigh(nneigh(i),i) = zji
                  endif
                elseif (r2.lt.bR22j) then
                  if (nneigh(j).ge.maxneigh.or..not.lmaxneighok) then
                    lmaxneighok = .false.
                    nneigh(j) = nneigh(j) + 1
                  else
                    rij = sqrt(r2)
                    nneigh(i) = nneigh(j) + 1
                    neighno(nneigh(j),j) = i
                    rneigh(nneigh(j),j) = rij
                    xneigh(nneigh(j),j) = - xji
                    yneigh(nneigh(j),j) = - yji
                    zneigh(nneigh(j),j) = - zji
                  endif
                endif
              endif
            endif
          enddo
        enddo sjloop
      enddo siloop
    endif
!
!  Free memory for lattice vectors
!
    deallocate(zvec,stat=status)
    if (status/=0) call deallocate_error('getEDIPneighbour','zvec')
    deallocate(yvec,stat=status)
    if (status/=0) call deallocate_error('getEDIPneighbour','yvec')
    deallocate(xvec,stat=status)
    if (status/=0) call deallocate_error('getEDIPneighbour','xvec')
  endif
!
!  Globalisation of data in parallel
!
  if (nprocs.gt.1) then
    tsuml = cputime()
!
!  Globalise value of lmaxneighok
!
    call landall(lmaxneighok,lmaxneighok2,1,"lmaxneighok","getEDIPneighbour")
    lmaxneighok = lmaxneighok2
!
    if (lmaxneighok) then
!
!  Sum all data
!
      allocate(ntmp1(numat))
      call isumall(nneigh,ntmp1,numat,"nneigh","getEDIPneighbour")
      nneigh(1:numat) = ntmp1(1:numat)
      deallocate(ntmp1)
      allocate(ntmp2(maxneigh,numat))
      call isumall(neighno,ntmp2,numat*maxneigh,"neighno","getEDIPneighbour")
      do i = 1,numat
        neighno(1:nneigh(i),i) = ntmp2(1:nneigh(i),i)
      enddo
      deallocate(ntmp2)
      allocate(rtmp2(maxneigh,numat))
      call sumall(rneigh,rtmp2,numat*maxneigh,"rneigh","getEDIPneighbour")
      do i = 1,numat
        rneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(xneigh,rtmp2,numat*maxneigh,"xneigh","getEDIPneighbour")
      do i = 1,numat
        xneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(yneigh,rtmp2,numat*maxneigh,"yneigh","getEDIPneighbour")
      do i = 1,numat
        yneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      call sumall(zneigh,rtmp2,numat*maxneigh,"zneigh","getEDIPneighbour")
      do i = 1,numat
        zneigh(1:nneigh(i),i) = rtmp2(1:nneigh(i),i)
      enddo
      deallocate(rtmp2)
    else
!
!  If maxneigh has been exceed only globalise nneigh
!
      allocate(ntmp1(numat))
      call isumall(nneigh,ntmp1,numat,"nneigh","getEDIPneighbour")
      nneigh(1:numat) = ntmp1(1:numat)
      deallocate(ntmp1)
    endif
    tsum = tsum + cputime() - tsuml
  endif
!
  return
  end
