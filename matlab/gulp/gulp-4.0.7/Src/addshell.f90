  subroutine addshell
!
!  Subroutine to find whether any cores have a missing shell
!  and if so then add it automatically.
!
!  12/10 Created from cscheck
!   7/11 QM flag added
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
  use configurations
  use current
  use element,       only : maxele
  use iochannels
  use moldyn,        only : lfix
  use parallel
  use scan,          only : ltranat
  use shell
  use species
  implicit none
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: j
  integer(i4)                                  :: ni
  integer(i4)                                  :: ninsert
  integer(i4)                                  :: nj
  integer(i4)                                  :: nshellspec
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: nsnoc
  logical                                      :: lfound
  logical                                      :: lshellneeded
  real(dp)                                     :: cut2
  real(dp)                                     :: occ
  real(dp)                                     :: r
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  nsnoc = 0
  cut2 = cuts*cuts
!
!  Find first atom shift
!
  nsft = 0
  if (ncf.gt.1) then
    do i = 1,ncf-1
      nsft = nsft + nascfg(i)
    enddo
  endif
!
!  Set variables
!
  nasym = nascfg(ncf)
!
!  Loop over cores in asymmetric unit
!
  do i = 1,nasym
    ni = natcfg(nsft+i)
    ntypi = ntypcfg(nsft+i)
!
!  If this is a core that should have a shell according to the species definition
!
    lshellneeded = .false.
    nshellspec = 0
    do while (nshellspec.lt.nspec.and..not.lshellneeded) 
      nshellspec = nshellspec + 1
      lshellneeded = ((natspec(nshellspec)-ni).eq.maxele.and.ntypi.eq.ntypspec(nshellspec))
    enddo
    if (ni.le.maxele.and.lshellneeded) then
      xcd = xalat(i)
      ycd = yalat(i)
      zcd = zalat(i)
      occ = occua(i)
      lfound = .false.
!
!  Loop over shell sites
!
      j = 0
      do while (j.lt.numat.and..not.lfound)
        j = j + 1
        nj = nat(j)
        ntypj = nftype(j)
        if ((nj-ni).eq.maxele.and.ntypi.eq.ntypj) then
          xcdi = xclat(j) - xcd
          ycdi = yclat(j) - ycd
          zcdi = zclat(j) - zcd
!
!  Loop over unit cells
!
          ii = 0
          do while (ii.lt.iimax.and..not.lfound)
            ii = ii + 1
            xcrd = xcdi + xvec1cell(ii)
            ycrd = ycdi + yvec1cell(ii)
            zcrd = zcdi + zvec1cell(ii)
            r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
            if (r.le.cut2) then
              lfound = .true.
            endif
!
!  End of loop over cell vectors
!
          enddo
        endif
!
!  End loop over shells
!
      enddo
      if (.not.lfound) then
!
!  Increment number of shells added
!
        nsnoc = nsnoc + 1
!
!  Check that arrays can accommodate another particle
!
        nasym = nasym + 1
        if (nasym.gt.maxat) then
          maxat = nint(1.2_dp*dble(nasym))
          call changemaxat
        endif
        if (nasum+nasym.gt.maxatot) then
          maxatot = nint(1.2_dp*dble(nasym))+nasum
          call changemaxatot
        endif
!
!  Move information for later configurations to make way for new atom
!
        ninsert = nsft + nasym
        call insertatom(ninsert)       
!
!  Add properties of new shell to global arrays
!
        nasum = nasum + 1
        nascfg(ncf) = nasym
        natcfg(ninsert) = ni + maxele
        ntypcfg(ninsert) = ntypi
        nspecptrcfg(ninsert) = nshellspec
        nregionno(ninsert) = nregionno(nsft+i)
        xcfg(ninsert) = xcfg(nsft+i)
        ycfg(ninsert) = ycfg(nsft+i)
        zcfg(ninsert) = zcfg(nsft+i)
        qlcfg(ninsert) = qlspec(nshellspec)
        occucfg(ninsert) = occ
        oxcfg(ninsert) = oxcfg(nsft+i)
        cncfg(ninsert) = cncfg(nsft+i)
        radcfg(ninsert) = radspec(nshellspec)
        lbsmat(ninsert) = lbsmat(nsft+i)
        lfix(ninsert) = lfix(nsft+i)
        lqmatom(ninsert) = lqmatom(nsft+i)
        lsliceatom(ninsert) = lsliceatom(nsft+i)
        ltranat(ninsert) = ltranat(nsft+i)
        lopfi(3*(ninsert-1)+1) = lopfi(3*(i-1)+1)
        lopfi(3*(ninsert-1)+2) = lopfi(3*(i-1)+2)
        lopfi(3*(ninsert-1)+3) = lopfi(3*(i-1)+3)
      endif
    endif
!
!  End loop over cores in asymmetric unit that need shells
!
  enddo
!
  return
  end
