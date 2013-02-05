  subroutine setcosmo
!
!  Subroutine to initialise data required for COSMO solvation model.
!
!   5/03 Adjusted to handle shells
!  10/08 Converted to f90 format
!
  use cosmo
  use current
  use element
  implicit none
!
!  Local variables
!
  integer(i4), save :: ncflast = 0
  integer(i4)       :: i
  integer(i4)       :: iat
  integer(i4)       :: k
  integer(i4)       :: l
  integer(i4)       :: m
  integer(i4)       :: nati
  logical           :: lfirstcall
!
!  Ensure that atomic dimensions are set correctly for COSMO
!
  call changemaxatcosmo
!
!  Set flag for initialisation of spheres
!
  lfirstcall = (ncf.ne.ncflast)
  ncflast = ncf
!
!  Set radii parameters
!
  drsolv = cosmodrsolv(ncf)
  rsolv = cosmorsolv(ncf)
!
!  Set function of solvent dielectric constant
!
  cosmofneps = (cosmoepsilon(ncf) - 1.0_dp)/(cosmoepsilon(ncf) + 0.5_dp)
!
!  Set distance criteria
!
  cosmormax2 = cosmormax**2
  cosmormax2s = (cosmormax - cosmormaxs)**2
!
!  Check VDW radii
!
  do i = 1,nasym
    iat = iatn(i)
    if (iat.gt.maxele) iat = iat - maxele
    if (rvdw(iat).lt.0.1_dp.or.rvdw(iat).gt.10.0_dp) then
      call outerror('VDW radius for element '//atsym(iat)//' is not suited for a COSMO run',0_i4)
      call stopnow('setcosmo')
    endif
  enddo
!
!  Set combined atom/solvent radii
!
  do i = 1,numat
    nati = nat(i)
    if (nati.gt.maxele) nati = nati - maxele
    atsrad(i) = rvdw(nati) + rsolv - drsolv
  enddo
  if (lfirstcall) then
!
!  Check sphere size for segments
!
    if (ldodeca) then
      m = (nspa-2)/10
    else
      m = (nspa-2)/4
    endif
    do k = 0,10
      if ((m/3)*3 .ne. m) exit
      m = m/3
    enddo
    do l = 0,10
      if ((m/4)*4 .ne. m) exit
      m = m/4
    enddo
    if (ldodeca) then
      if (10*3**k*4**l+2 .ne. nspa) then
        call outerror('Invalid value of nspa : must be 10*3**k*4**l+2',0_i4)
        call stopnow('setcosmo')
      endif
    else
      if (4*3**k*4**l+2 .ne. nspa) then
        call outerror('Invalid value of nspa : must be 4*3**k*4**l+2',0_i4)
        call stopnow('setcosmo')
      endif
    endif
!
!  Check sphere size for segments - H atoms
!
    if (ldodeca) then
      m = (nspah-2)/10
    else
      m = (nspah-2)/4
    endif
    do k = 0,10
      if ((m/3)*3 .ne. m) exit
      m = m/3
    enddo
    do l = 0,10
      if ((m/4)*4 .ne. m) exit
      m = m/4
    enddo
    if (ldodeca) then
      if (10*3**k*4**l+2 .ne. nspah) then
        call outerror('Invalid value of nspah : must be 10*3**k*4**l+2',0_i4)
        call stopnow('setcosmo')
      endif
    else
      if (4*3**k*4**l+2 .ne. nspah) then
        call outerror('Invalid value of nspah : must be 4*3**k*4**l+2',0_i4)
        call stopnow('setcosmo')
      endif                                                                                                
    endif
!
!  Check sphere size for points
!
    if (ldodeca) then
      m = (nppa-2)/10
    else
      m = (nppa-2)/4
    endif
    do k = 0,10
      if ((m/3)*3 .ne. m) exit
      m = m/3
    enddo
    do l = 0,10
      if ((m/4)*4 .ne. m) exit
      m = m/4
    enddo
    if (ldodeca) then
      if (10*3**k*4**l+2 .ne. nppa) then
        call outerror('Invalid value of nppa : must be 10*3**k*4**l+2',0_i4)
        call stopnow('setcosmo')
      endif
    else
      if (4*3**k*4**l+2 .ne. nppa) then
        call outerror('Invalid value of nppa : must be 4*3**k*4**l+2',0_i4)
        call stopnow('setcosmo')
      endif
    endif
!
!  Find sizes of auxillary spheres
!
    nptsh = nspah
    npts = nspa
!
!  Allocate arrays
!
    if (npts.gt.maxnpts) then
      maxnpts = npts
      call changemaxnpts
    endif
    if (nptsh.gt.maxnptsh) then
      maxnptsh = nptsh
      call changemaxnptsh
    endif
    if (nppa.gt.maxnppa) then
      maxnppa = nppa
      call changemaxnppa
    endif
!
!  Generate unit spheres with different numbers of surface mesh points
!
!  sphere1h is sphere1, but for hydrogen
!
    if (ldodeca) then
      call createsphere_dodeca(nptsh,sphere1h)
      call createsphere_dodeca(npts,sphere1)
      call createsphere_dodeca(nppa,sphere2)
    else
      call createsphere_octa(nptsh,sphere1h)
      call createsphere_octa(npts,sphere1)
      call createsphere_octa(nppa,sphere2)
    endif
  endif
!
  return

  end subroutine setcosmo
