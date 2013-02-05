  subroutine reaxFFbo_sigma(nboij,nspeci,nspecj,rij,BO,dBOdr,lgrad1,ltaper,lnonzero)
!
!  Subroutine to calculate the bond order for reaxFF. This is the primed
!  version to be combined with other terms to yield the full expression.
!
!  On entry : 
!
!  nboij           = index for pair of reaxFF species
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  rij             = distance between i and j
!  lgrad1          = if .true. compute the first derivative 
!  ltaper          = if .true. apply taper to smooth cutoff
!
!  On exit :
!
!  BO              = bond order prime
!  dBOdr           = first derivative of BO w.r.t. r if lgrad1
!  lnonzero        = if .true. then a non-zero bond order was computed
!
!   7/07 Created
!   8/07 pi bond orders added
!   8/07 Separate routines created for sigma, pi and pi_pi
!  11/07 Bond orders shifted by cutoff tolerance
!  11/07 Radii of species used to check whether term is non-zero
!  11/07 Pairwise radii now checked for
!   3/08 Logical return flag added to indicate whether a value greater than zero was found
!   4/08 Factor of 1 + reaxFFtol scaling for sigma bond order added
!   4/08 Taper option added
!   1/09 lreaxFFpboOK added 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nboij
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: rij
  real(dp),    intent(out)            :: BO
  real(dp),    intent(out)            :: dBOdr
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: ltaper
  logical,     intent(out)            :: lnonzero
!
!  Local variables
!
  logical                             :: ltrm1
  real(dp)                            :: dTrm1
  real(dp)                            :: eTrm1
  real(dp)                            :: ft
  real(dp)                            :: dftdBO
  real(dp)                            :: d2ftdBO2
  real(dp)                            :: d3ftdBO3
  real(dp)                            :: r0ij1
  real(dp)                            :: rr1
  real(dp)                            :: rrp1
!
!  Calculate value and derivatives 
!
  BO = 0.0_dp
  if (lgrad1) then
    dBOdr = 0.0_dp
  endif
  lnonzero = .false.
!
!  If radii for either species is >= 0, then term is zero
!
  ltrm1 = (reaxFFr(1,nspeci).gt.0.0_dp.and.reaxFFr(1,nspecj).gt.0.0_dp)
  if (ltrm1.and.lreaxFFpboOK(nboij)) then
    if (reaxFFmorse(4,nboij).gt.0.0_dp) then
      r0ij1 = reaxFFmorse(4,nboij)
    else
      r0ij1 = 0.5_dp*(reaxFFr(1,nspeci) + reaxFFr(1,nspecj))
    endif
    rr1 = (rij/r0ij1)
    rrp1 = rr1**reaxFFpbo(2,nboij)
    eTrm1 = (1.0_dp + reaxFFtol)*exp(reaxFFpbo(1,nboij)*rrp1)
    if (eTrm1.gt.reaxFFtol) then
      lnonzero = .true.
      if (lTaper.and.eTrm1.lt.reaxFFtaperscale*reaxFFtol) then
        call ataper(.false.,eTrm1,reaxFFtol,reaxFFtaperscale*reaxFFtol,ft,dftdBO,d2ftdBO2,d3ftdBO3, &
                    lgrad1,.false.,.false.)
        BO = BO + ft*(eTrm1 - reaxFFtol)
        if (lgrad1) then
          dTrm1 = rrp1*reaxFFpbo(1,nboij)*reaxFFpbo(2,nboij)/rij
          dBOdr = dBOdr + (ft + dftdBO*(eTrm1 - reaxFFtol))*eTrm1*dTrm1
        endif
      else
        BO = BO + eTrm1 - reaxFFtol
        if (lgrad1) then
          dTrm1 = rrp1*reaxFFpbo(1,nboij)*reaxFFpbo(2,nboij)/rij
          dBOdr = dBOdr + eTrm1*dTrm1
        endif
      endif
    endif
  endif
!
  return
  end
!
  subroutine reaxFFbo_pi(nboij,nspeci,nspecj,rij,BO,dBOdr,lgrad1,ltaper)
!
!  Subroutine to calculate the bond order for reaxFF. This is the primed
!  version to be combined with other terms to yield the full expression.
!
!  On entry : 
!
!  nboij           = index for pair of reaxFF species
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  rij             = distance between i and j
!  lgrad1          = if .true. compute the first derivative 
!  ltaper          = if .true. apply taper to smooth cutoff
!
!  On exit :
!
!  BO              = bond order prime
!  dBOdr           = first derivative of BO w.r.t. r if lgrad1
!
!   7/07 Created
!   8/07 pi bond orders added
!   8/07 Separate routines created for sigma, pi and pi_pi
!  11/07 Bond orders shifted by cutoff tolerance
!  11/07 Radii of species used to check whether term is non-zero
!  11/07 Pairwise radii now checked for
!   3/08 Logical return flag added to indicate whether a value greater than zero was found
!   4/08 Screening of values removed since they should not be cut and shifted
!   4/08 Taper option added
!   5/08 Correction made to tapering by allowing for reaxFFtol offset during taper regime
!   1/09 lreaxFFpboOK added 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nboij
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: rij
  real(dp),    intent(out)            :: BO
  real(dp),    intent(out)            :: dBOdr
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: ltaper
!
!  Local variables
!
  logical                             :: ltrm1
  real(dp)                            :: dTrm1
  real(dp)                            :: eTrm1
  real(dp)                            :: ft
  real(dp)                            :: dftdBO
  real(dp)                            :: d2ftdBO2
  real(dp)                            :: d3ftdBO3
  real(dp)                            :: r0ij1
  real(dp)                            :: rr1
  real(dp)                            :: rrp1
!
!  Calculate value and derivatives 
!
  BO = 0.0_dp
  if (lgrad1) then
    dBOdr = 0.0_dp
  endif
!
!  If radii for either species is >= 0, then term is zero
!
  ltrm1 = (reaxFFr(2,nspeci).gt.0.0_dp.and.reaxFFr(2,nspecj).gt.0.0_dp)
  if (ltrm1.and.lreaxFFpboOK(nboij)) then
    if (reaxFFmorse(5,nboij).gt.0.0_dp) then
      r0ij1 = reaxFFmorse(5,nboij)
    else
      r0ij1 = 0.5_dp*(reaxFFr(2,nspeci) + reaxFFr(2,nspecj))
    endif
    rr1 = (rij/r0ij1)
    rrp1 = rr1**reaxFFpbo(4,nboij)
    eTrm1 = exp(reaxFFpbo(3,nboij)*rrp1)
    if (eTrm1.gt.reaxFFtol) then
      if (lTaper.and.eTrm1.lt.reaxFFtaperscale*reaxFFtol) then
        call ataper(.false.,eTrm1,reaxFFtol,reaxFFtaperscale*reaxFFtol,ft,dftdBO,d2ftdBO2,d3ftdBO3, &
                    lgrad1,.false.,.false.)
        BO = BO + ft*eTrm1 
        if (lgrad1) then
          dTrm1 = rrp1*reaxFFpbo(3,nboij)*reaxFFpbo(4,nboij)/rij
          dBOdr = dBOdr + (ft + dftdBO*eTrm1)*eTrm1*dTrm1
        endif
      else
        BO = BO + eTrm1
        if (lgrad1) then
          dTrm1 = rrp1*reaxFFpbo(3,nboij)*reaxFFpbo(4,nboij)/rij
          dBOdr = dBOdr + eTrm1*dTrm1
        endif
      endif
    endif
  endif
!
  return
  end
!
  subroutine reaxFFbo_pipi(nboij,nspeci,nspecj,rij,BO,dBOdr,lgrad1,ltaper)
!
!  Subroutine to calculate the bond order for reaxFF. This is the primed
!  version to be combined with other terms to yield the full expression.
!
!  On entry : 
!
!  nboij           = index for pair of reaxFF species
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  rij             = distance between i and j
!  lgrad1          = if .true. compute the first derivative 
!  ltaper          = if .true. apply taper to smooth cutoff
!
!  On exit :
!
!  BO              = bond order prime
!  dBOdr           = first derivative of BO w.r.t. r if lgrad1
!
!   7/07 Created
!   8/07 pi bond orders added
!   8/07 Separate routines created for sigma, pi and pi_pi
!  11/07 Bond orders shifted by cutoff tolerance
!  11/07 Radii of species used to check whether term is non-zero
!  11/07 Pairwise radii now checked for
!   3/08 Logical return flag added to indicate whether a value greater than zero was found
!   4/08 Screening of values removed since they should not be cut and shifted
!   4/08 Taper option added
!   1/09 lreaxFFpboOK added 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nboij
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: rij
  real(dp),    intent(out)            :: BO
  real(dp),    intent(out)            :: dBOdr
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: ltaper
!
!  Local variables
!
  logical                             :: ltrm1
  real(dp)                            :: dTrm1
  real(dp)                            :: eTrm1
  real(dp)                            :: ft
  real(dp)                            :: dftdBO
  real(dp)                            :: d2ftdBO2
  real(dp)                            :: d3ftdBO3
  real(dp)                            :: r0ij1
  real(dp)                            :: rr1
  real(dp)                            :: rrp1
!
!  Calculate value and derivatives 
!
  BO = 0.0_dp
  if (lgrad1) then
    dBOdr = 0.0_dp
  endif
!
!  If radii for either species is >= 0, then term is zero
!
  ltrm1 = (reaxFFr(3,nspeci).gt.0.0_dp.and.reaxFFr(3,nspecj).gt.0.0_dp)
  if (ltrm1.and.lreaxFFpboOK(nboij)) then
    if (reaxFFmorse(6,nboij).gt.0.0_dp) then
      r0ij1 = reaxFFmorse(6,nboij)
    else
      r0ij1 = 0.5_dp*(reaxFFr(3,nspeci) + reaxFFr(3,nspecj))
    endif
    rr1 = (rij/r0ij1)
    rrp1 = rr1**reaxFFpbo(6,nboij)
    eTrm1 = exp(reaxFFpbo(5,nboij)*rrp1)
    if (eTrm1.gt.reaxFFtol) then
      if (lTaper.and.eTrm1.lt.reaxFFtaperscale*reaxFFtol) then
        call ataper(.false.,eTrm1,reaxFFtol,reaxFFtaperscale*reaxFFtol,ft,dftdBO,d2ftdBO2,d3ftdBO3, &
                    lgrad1,.false.,.false.)
        BO = BO + ft*eTrm1  
        if (lgrad1) then
          dTrm1 = rrp1*reaxFFpbo(5,nboij)*reaxFFpbo(6,nboij)/rij
          dBOdr = dBOdr + (ft + dftdBO*eTrm1)*eTrm1*dTrm1
        endif
      else
        BO = BO + eTrm1
        if (lgrad1) then
          dTrm1 = rrp1*reaxFFpbo(5,nboij)*reaxFFpbo(6,nboij)/rij
          dBOdr = dBOdr + eTrm1*dTrm1
        endif
      endif
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f1(nspeci,nspecj,deltapi,deltapj,f1,df1ddpi,df1ddpj,d2f1ddpi2,d2f1ddpiddpj,d2f1ddpj2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f1 term for reaxFF. 
!
!  On entry : 
!
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  deltapi         = delta' for i
!  deltapj         = delta' for j
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f1              = function f1
!  df1ddpi         = first derivative of f1 w.r.t. deltapi if lgrad1
!  df1ddpj         = first derivative of f1 w.r.t. deltapi if lgrad1
!  d2f1ddpi2       = second derivative of f1 w.r.t. deltapi if lgrad2
!  d2f1ddpiddpj    = second derivative of f1 w.r.t. deltapi & deltapj if lgrad2
!  d2f1ddpj2       = second derivative of f1 w.r.t. deltapj if lgrad2
!
!   7/07 Created
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
!  Julian Gale, NRI, Curtin University, July 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: deltapi
  real(dp),    intent(in)             :: deltapj
  real(dp),    intent(out)            :: f1
  real(dp),    intent(out)            :: df1ddpi
  real(dp),    intent(out)            :: df1ddpj
  real(dp),    intent(out)            :: d2f1ddpi2
  real(dp),    intent(out)            :: d2f1ddpiddpj
  real(dp),    intent(out)            :: d2f1ddpj2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: df2ddpi
  real(dp)                            :: df2ddpj
  real(dp)                            :: df3ddpi
  real(dp)                            :: df3ddpj
  real(dp)                            :: d2f2ddpi2
  real(dp)                            :: d2f2ddpiddpj
  real(dp)                            :: d2f2ddpj2
  real(dp)                            :: d2f3ddpi2
  real(dp)                            :: d2f3ddpiddpj
  real(dp)                            :: d2f3ddpj2
  real(dp)                            :: f2
  real(dp)                            :: f3
  real(dp)                            :: Trm1
  real(dp)                            :: Trm1a
  real(dp)                            :: Trm1b
  real(dp)                            :: dTrm1ai
  real(dp)                            :: dTrm1aj
  real(dp)                            :: dTrm1bi
  real(dp)                            :: dTrm1bj
  real(dp)                            :: d2Trm1a
  real(dp)                            :: d2Trm1b
  real(dp)                            :: d2Trm1ai
  real(dp)                            :: d2Trm1aj
  real(dp)                            :: d2Trm1bi
  real(dp)                            :: d2Trm1bj
  real(dp)                            :: Trm2
  real(dp)                            :: Trm2a
  real(dp)                            :: Trm2b
  real(dp)                            :: dTrm2ai
  real(dp)                            :: dTrm2aj
  real(dp)                            :: dTrm2bi
  real(dp)                            :: dTrm2bj
  real(dp)                            :: d2Trm2a
  real(dp)                            :: d2Trm2b
  real(dp)                            :: d2Trm2ai
  real(dp)                            :: d2Trm2aj
  real(dp)                            :: d2Trm2bi
  real(dp)                            :: d2Trm2bj
!
!  Call f2 and f3 to get these functions
!
  call reaxFF_f2(deltapi,deltapj,f2,df2ddpi,df2ddpj,d2f2ddpi2,d2f2ddpiddpj,d2f2ddpj2,lgrad1,lgrad2)
  call reaxFF_f3(deltapi,deltapj,f3,df3ddpi,df3ddpj,d2f3ddpi2,d2f3ddpiddpj,d2f3ddpj2,lgrad1,lgrad2)
!
!  Calculate value
!
  Trm1a = (reaxFFval(1,nspeci) + f2)
  Trm1b = 1.0_dp/(reaxFFval(1,nspeci) + f2 + f3)
  Trm1 = Trm1a*Trm1b
  Trm2a = (reaxFFval(1,nspecj) + f2)
  Trm2b = 1.0_dp/(reaxFFval(1,nspecj) + f2 + f3)
  Trm2 = Trm2a*Trm2b
  f1 = 0.5_dp*(Trm1 + Trm2)
!
!  Calculate derivatives
!
  if (lgrad1) then
    dTrm1ai = df2ddpi
    dTrm1bi = - Trm1b*Trm1b*(df2ddpi + df3ddpi)
    dTrm2ai = df2ddpi
    dTrm2bi = - Trm2b*Trm2b*(df2ddpi + df3ddpi)
!
    df1ddpi = 0.5_dp*(dTrm1ai*Trm1b + Trm1a*dTrm1bi + dTrm2ai*Trm2b + Trm2a*dTrm2bi)
!
    dTrm1aj = df2ddpj
    dTrm1bj = - Trm1b*Trm1b*(df2ddpj + df3ddpj)
    dTrm2aj = df2ddpj
    dTrm2bj = - Trm2b*Trm2b*(df2ddpj + df3ddpj)
!
    df1ddpj = 0.5_dp*(dTrm1aj*Trm1b + Trm1a*dTrm1bj + dTrm2aj*Trm2b + Trm2a*dTrm2bj)
!
    if (lgrad2) then
      d2Trm1ai = d2f2ddpi2
      d2Trm1bi = Trm1b*Trm1b*(2.0_dp*Trm1b*(df2ddpi + df3ddpi)**2 - (d2f2ddpi2 + d2f3ddpi2))
      d2Trm2ai = d2f2ddpi2
      d2Trm2bi = Trm2b*Trm2b*(2.0_dp*Trm2b*(df2ddpi + df3ddpi)**2 - (d2f2ddpi2 + d2f3ddpi2))
!
      d2f1ddpi2 = 0.5_dp*(d2Trm1ai*Trm1b + 2.0_dp*dTrm1ai*dTrm1bi + Trm1a*d2Trm1bi + &
                          d2Trm2ai*Trm2b + 2.0_dp*dTrm2ai*dTrm2bi + Trm2a*d2Trm2bi)
!
      d2Trm1a = d2f2ddpiddpj
      d2Trm1b = Trm1b*Trm1b*(2.0_dp*Trm1b*(df2ddpi + df3ddpi)*(df2ddpj + df3ddpj) - (d2f2ddpiddpj + d2f3ddpiddpj))
      d2Trm2a = d2f2ddpiddpj
      d2Trm2b = Trm2b*Trm2b*(2.0_dp*Trm2b*(df2ddpi + df3ddpi)*(df2ddpj + df3ddpj) - (d2f2ddpiddpj + d2f3ddpiddpj))
!
      d2f1ddpiddpj = 0.5_dp*(d2Trm1a*Trm1b + dTrm1ai*dTrm1bj + dTrm1aj*dTrm1bi + Trm1a*d2Trm1b + &
                             d2Trm2a*Trm2b + dTrm2ai*dTrm2bj + dTrm2aj*dTrm2bi + Trm2a*d2Trm2b)
!
      d2Trm1aj = d2f2ddpj2
      d2Trm1bj = Trm1b*Trm1b*(2.0_dp*Trm1b*(df2ddpj + df3ddpj)**2 - (d2f2ddpj2 + d2f3ddpj2))
      d2Trm2aj = d2f2ddpj2
      d2Trm2bj = Trm2b*Trm2b*(2.0_dp*Trm2b*(df2ddpj + df3ddpj)**2 - (d2f2ddpj2 + d2f3ddpj2))
!
      d2f1ddpj2 = 0.5_dp*(d2Trm1aj*Trm1b + 2.0_dp*dTrm1aj*dTrm1bj + Trm1a*d2Trm1bj + &
                          d2Trm2aj*Trm2b + 2.0_dp*dTrm2aj*dTrm2bj + Trm2a*d2Trm2bj)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f2(deltapi,deltapj,f2,df2ddpi,df2ddpj,d2f2ddpi2,d2f2ddpiddpj,d2f2ddpj2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f2 term for reaxFF. 
!
!  On entry : 
!
!  deltapi         = delta' for i
!  deltapj         = delta' for j
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f2              = function f2
!  df2ddpi         = first derivative of f2 w.r.t. deltapi if lgrad1
!  df2ddpj         = first derivative of f2 w.r.t. deltapj if lgrad1
!  d2f2ddpi2       = second derivative of f2 w.r.t. deltapi if lgrad2
!  d2f2ddpiddpj    = second derivative of f2 w.r.t. deltapi/deltapj if lgrad2
!  d2f2ddpj2       = second derivative of f2 w.r.t. deltapj if lgrad2
!
!   7/07 Created
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
!  Julian Gale, NRI, Curtin University, July 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: deltapi
  real(dp),    intent(in)             :: deltapj
  real(dp),    intent(out)            :: f2
  real(dp),    intent(out)            :: df2ddpi
  real(dp),    intent(out)            :: df2ddpj
  real(dp),    intent(out)            :: d2f2ddpi2
  real(dp),    intent(out)            :: d2f2ddpiddpj
  real(dp),    intent(out)            :: d2f2ddpj2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: Trm1
  real(dp)                            :: Trm2
!
!  Calculate value
!
  Trm1 = exp(-reaxFFlam(1)*deltapi)
  Trm2 = exp(-reaxFFlam(1)*deltapj)
  f2 = Trm1 + Trm2
!
!  Calculate derivatives
!
  if (lgrad1) then
    df2ddpi = - Trm1*reaxFFlam(1)
    df2ddpj = - Trm2*reaxFFlam(1)
    if (lgrad2) then
      d2f2ddpi2 = Trm1*(reaxFFlam(1)**2)
      d2f2ddpiddpj = 0.0_dp
      d2f2ddpj2 = Trm2*(reaxFFlam(1)**2)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f3(deltapi,deltapj,f3,df3ddpi,df3ddpj,d2f3ddpi2,d2f3ddpiddpj,d2f3ddpj2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f3 term for reaxFF. 
!
!  On entry : 
!
!  deltapi         = delta' for i
!  deltapj         = delta' for j
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f3              = function f3
!  df3ddpi         = first derivative of f3 w.r.t. deltapi if lgrad1
!  df3ddpj         = first derivative of f3 w.r.t. deltapj if lgrad1
!  d2f3ddpi2       = second derivative of f3 w.r.t. deltapi if lgrad2
!  d2f3ddpiddpj    = second derivative of f3 w.r.t. deltapi/deltapj if lgrad2
!  d2f3ddpj2       = second derivative of f3 w.r.t. deltapj if lgrad2
!
!   7/07 Created
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
!  Julian Gale, NRI, Curtin University, July 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: deltapi
  real(dp),    intent(in)             :: deltapj
  real(dp),    intent(out)            :: f3
  real(dp),    intent(out)            :: df3ddpi
  real(dp),    intent(out)            :: df3ddpj
  real(dp),    intent(out)            :: d2f3ddpi2
  real(dp),    intent(out)            :: d2f3ddpiddpj
  real(dp),    intent(out)            :: d2f3ddpj2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: Trm1
  real(dp)                            :: Trm2
  real(dp)                            :: Trm3
  real(dp)                            :: rTrm3
!
!  Calculate value
!
  Trm1 = exp(-reaxFFlam(2)*deltapi)
  Trm2 = exp(-reaxFFlam(2)*deltapj)
  Trm3 = (Trm1 + Trm2)
  f3 = - log(0.5_dp*Trm3)/reaxFFlam(2)
!
!  Calculate derivatives
!
  if (lgrad1) then
    rTrm3 = 1.0_dp/Trm3
    df3ddpi = Trm1*rTrm3
    df3ddpj = Trm2*rTrm3
    if (lgrad2) then
      d2f3ddpi2 = - reaxFFlam(2)*rTrm3*Trm1*(1.0_dp - Trm1*rTrm3)
      d2f3ddpiddpj = reaxFFlam(2)*rTrm3*rTrm3*Trm1*Trm2
      d2f3ddpj2 = - reaxFFlam(2)*rTrm3*Trm2*(1.0_dp - Trm2*rTrm3)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f45(nspeci,nspecj,BOpij,deltapi,f4,df4ddpi,df4dbo,d2f4ddpi2,d2f4ddpidbo,d2f4dbo2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f4/f5 terms for reaxFF. Form of the
!  expression is the same for f4 and f5 - just differs in use of 
!  deltapi vs deltapj.
!
!  On entry : 
!
!  nspeci          = reaxFF species number of i
!  nspecj          = reaxFF species number of j
!  BOpij           = bond order primed for i-j
!  deltapi         = delta' for i
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f4              = function f4
!  df4ddpi         = first derivative of f4 w.r.t. deltapi if lgrad1
!  df4dbo          = first derivative of f4 w.r.t. bo' if lgrad1
!  d2f4ddpi2       = second derivative of f4 w.r.t. deltapi if lgrad2
!  d2f4ddpidbo     = second derivative of f4 w.r.t. deltapi/bo' if lgrad2
!  d2f4dbo2        = second derivative of f4 w.r.t. bo' if lgrad2
!
!   7/07 Created
!   8/07 Modified to handle new reaxFF form
!   8/07 Rule that parameters are geometric means added
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
!  Julian Gale, NRI, Curtin University, August 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: BOpij
  real(dp),    intent(in)             :: deltapi
  real(dp),    intent(out)            :: f4
  real(dp),    intent(out)            :: df4ddpi
  real(dp),    intent(out)            :: df4dbo
  real(dp),    intent(out)            :: d2f4ddpi2
  real(dp),    intent(out)            :: d2f4ddpidbo
  real(dp),    intent(out)            :: d2f4dbo2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: deltapi_boc
  real(dp)                            :: lam3
  real(dp)                            :: lam4
  real(dp)                            :: lam5
  real(dp)                            :: Trm1
  real(dp)                            :: Trm2
!
!  Assign local variables of lambda values for this species
!
  lam3 = sqrt(reaxFFpboc(1,nspeci)*reaxFFpboc(1,nspecj))
  lam4 = sqrt(reaxFFpboc(2,nspeci)*reaxFFpboc(2,nspecj))
  lam5 = sqrt(reaxFFpboc(3,nspeci)*reaxFFpboc(3,nspecj))
!
!  Deltapi has to be modified here due to the use of a different valence
!
  deltapi_boc = deltapi + reaxFFval(1,nspeci) - reaxFFval(2,nspeci)
!
!  Calculate value
!
  Trm1 = exp(-lam3*(lam4*BOpij**2-deltapi_boc)+lam5)
  f4 = 1.0_dp/(1.0_dp + Trm1)
!
!  Calculate derivatives
!
  if (lgrad1) then
    df4ddpi = - f4*f4*Trm1*lam3
    df4dbo  =   f4*f4*Trm1*2.0_dp*lam3*lam4*BOpij
    if (lgrad2) then
      d2f4ddpi2 = ((f4*lam3)**2)*Trm1*(2.0_dp*Trm1*f4 - 1.0_dp)
      Trm2 = 2.0_dp*lam3*lam3*lam4*BOpij
      d2f4ddpidbo = - f4*f4*Trm1*Trm2*(2.0_dp*f4*Trm1 - 1.0_dp)
      Trm2 = 4.0_dp*(lam3*lam4*BOpij)**2
      d2f4dbo2 = f4*f4*Trm1*(2.0_dp*f4*Trm1*Trm2 - Trm2 + 2.0_dp*lam3*lam4)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f7(nspeci,nspecj,nspeck,nval3,BOij,BOik,f7,df7dBOij,df7dBOik,d2f7dBOij2,d2f7dBOijdBOik,d2f7dBOik2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f7 term for reaxFF. Note that this subroutine
!  actually returns the product of f7(BOij) x f7(BOik) rather than the individual
!  terms. In GULP i is the pivot atom and j & k are the terminal atoms.
!
!  On entry : 
!
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  nspeck          = species index for k 
!  nval3           = valence potential number
!  BOij            = corrected bond order for i-j
!  BOik            = corrected bond order for i-k
!  lgrad1          = if .true. compute the first derivatives
!  lgrad2          = if .true. compute the second derivatives 
!
!  On exit :
!
!  f7              = function f7 for both bonds
!  df7dBOij        = first derivative of f7 w.r.t. BOij if lgrad1
!  df7dBOik        = first derivative of f7 w.r.t. BOik if lgrad1
!  d2f7dBOij2      = second derivative of f7 w.r.t. BOij if lgrad2
!  d2f7dBOijdBOik  = second derivative of f7 w.r.t. BOij & BOik if lgrad2
!  d2f7dBOik2      = second derivative of f7 w.r.t. BOik if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
!   4/08 Valence potential number added
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, April 2008
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  integer(i4), intent(in)             :: nspeck
  integer(i4), intent(in)             :: nval3
  real(dp),    intent(in)             :: BOij
  real(dp),    intent(in)             :: BOik
  real(dp),    intent(out)            :: f7
  real(dp),    intent(out)            :: df7dBOij
  real(dp),    intent(out)            :: df7dBOik
  real(dp),    intent(out)            :: d2f7dBOij2
  real(dp),    intent(out)            :: d2f7dBOijdBOik
  real(dp),    intent(out)            :: d2f7dBOik2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: ind
  real(dp)                            :: BOij4
  real(dp)                            :: BOik4
  real(dp)                            :: expij
  real(dp)                            :: expik
  real(dp)                            :: f7j
  real(dp)                            :: f7k
  real(dp)                            :: pval3
  real(dp)                            :: pval4
!
!  Compute index for terminal atoms
!
  if (nspecj.ge.nspeck) then
    ind = nspecj*(nspecj - 1)/2 + nspeck
  else
    ind = nspeck*(nspeck - 1)/2 + nspecj
  endif
!
!  Calculate values of components
!
  pval3 = reaxFFval1(1,nspeci)
  pval4 = reaxFFval3(4,nval3,ind,nspeci)
  BOij4 = BOij**pval4
  BOik4 = BOik**pval4
  expij = exp(-pval3*BOij4)
  expik = exp(-pval3*BOik4)
  f7j = 1.0_dp - expij
  f7k = 1.0_dp - expik
!
!  Compute total function
!
  f7 = f7j*f7k
!
!  Calculate derivatives
!
  if (lgrad1) then
    df7dBOij = f7k*pval3*expij*pval4*(BOij**(pval4-1.0_dp))
    df7dBOik = f7j*pval3*expik*pval4*(BOik**(pval4-1.0_dp))
    if (lgrad2) then
      d2f7dBOij2 = f7k*pval3*expij*pval4*(-pval3*pval4*(BOij**(2.0_dp*pval4-2.0_dp)) + (pval4-1.0_dp)*(BOij**(pval4-2.0_dp)))
      d2f7dBOijdBOik = pval3*pval3*expij*expik*pval4*pval4*(BOij**(pval4-1.0_dp))*(BOik**(pval4-1.0_dp))
      d2f7dBOik2 = f7j*pval3*expik*pval4*(-pval3*pval4*(BOik**(2.0_dp*pval4-2.0_dp)) + (pval4-1.0_dp)*(BOik**(pval4-2.0_dp)))
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f8(nspeci,nspecj,nspeck,nval3,deltai,f8,df8ddeltai,d2f8ddeltai2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f8 term for reaxFF. 
!  In GULP i is the pivot atom and j & k are the terminal atoms.
!
!  On entry : 
!
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  nspeck          = species index for k 
!  nval3           = valence potential number
!  deltai          = delta for i
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f8              = function f8 for both bonds
!  df8ddeltai      = first derivative of f8 w.r.t. deltai if lgrad1
!  d2f8ddeltai2    = second derivative of f8 w.r.t. deltai if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
!   4/08 Valence potential number added
!   7/08 pval6 added to reaxFFval3 array and used from here if the 
!        value is non-zero, otherwise it is taken from the old
!        location
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, July 2008
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  integer(i4), intent(in)             :: nspeck
  integer(i4), intent(in)             :: nval3
  real(dp),    intent(in)             :: deltai
  real(dp),    intent(out)            :: f8
  real(dp),    intent(out)            :: df8ddeltai
  real(dp),    intent(out)            :: d2f8ddeltai2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: ind
  real(dp)                            :: delta_angle
  real(dp)                            :: exp6
  real(dp)                            :: exp7
  real(dp)                            :: pval5
  real(dp)                            :: pval5m1
  real(dp)                            :: pval6
  real(dp)                            :: pval7
  real(dp)                            :: trm1
  real(dp)                            :: trm2
!
!  Compute index for terminal atoms
!
  if (nspecj.ge.nspeck) then
    ind = nspecj*(nspecj - 1)/2 + nspeck
  else
    ind = nspeck*(nspeck - 1)/2 + nspecj
  endif
!
!  Set parameters for triad
!
  pval5 = reaxFFval1(2,nspeci)
  if (abs(reaxFFval3(6,nval3,ind,nspeci)).gt.1.0d-6) then
    pval6 = reaxFFval3(6,nval3,ind,nspeci)
  else
    pval6 = reaxFFlam(15)
  endif
  pval7 = reaxFFval3(5,nval3,ind,nspeci)
!
!  Compute delta_angle
!
  delta_angle = deltai + reaxFFval(1,nspeci) - reaxFFval(4,nspeci)
!
!  Compute exponentials
!
  exp6 = exp(pval6*delta_angle)
  exp7 = exp(-pval7*delta_angle)
!
!  Compute remaining useful terms
!
  pval5m1 = pval5 - 1.0_dp
  trm1 = 1.0_dp/(1.0_dp + exp6 + exp7)
!
!  Compute function
!
  f8 = pval5 - pval5m1*(2.0_dp + exp6)*trm1
!
!  Calculate derivative
!
  if (lgrad1) then
    trm2 = pval6*exp6 - pval7*exp7
    df8ddeltai = - pval5m1*trm1*(pval6*exp6 - trm1*(2.0_dp + exp6)*trm2)
    if (lgrad2) then
      d2f8ddeltai2 = - pval5m1*trm1*(pval6*pval6*exp6 - trm1*(2.0_dp*pval6*exp6*trm2 + &
        (2.0_dp + exp6)*(pval6*pval6*exp6 + pval7*pval7*exp7) - trm1*(2.0_dp*(2.0_dp + exp6)*trm2)))
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_theta0(nspeci,nspecj,nspeck,nval3,sbo,theta0,dtheta0dsbo,d2theta0dsbo2,lgrad1,lgrad2)
!
!  Subroutine to calculate the theta0 term for reaxFF. 
!  In GULP i is the pivot atom and j & k are the terminal atoms.
!
!  On entry : 
!
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  nspeck          = species index for k 
!  nval3           = valence potential number
!  sbo             = SBO term for i
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  theta0          = function theta0 for both bonds
!  dtheta0dsbo     = first derivative of theta0 w.r.t. sbo if lgrad1
!  d2theta0dsbo2   = second derivative of theta0 w.r.t. sbo if lgrad2
!
!   8/07 Created
!  10/07 Second derivative added 
!   4/08 Valence potential number added
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, April 2008
!
  use constants,      only : degtorad
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  integer(i4), intent(in)             :: nspeck
  integer(i4), intent(in)             :: nval3
  real(dp),    intent(in)             :: sbo
  real(dp),    intent(out)            :: theta0
  real(dp),    intent(out)            :: dtheta0dsbo
  real(dp),    intent(out)            :: d2theta0dsbo2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: ind
  real(dp)                            :: dsbo2dsbo
  real(dp)                            :: d2sbo2dsbo2
  real(dp)                            :: exp10
  real(dp)                            :: theta00
  real(dp)                            :: pval9
  real(dp)                            :: pval10
  real(dp)                            :: sbo2
!
!  Compute index for terminal atoms
!
  if (nspecj.ge.nspeck) then
    ind = nspecj*(nspecj - 1)/2 + nspeck
  else
    ind = nspeck*(nspeck - 1)/2 + nspecj
  endif
!
!  Set parameters for triad
!
  pval9   = reaxFFlam(17)
  pval10  = reaxFFlam(18)
  theta00 = reaxFFval3(1,nval3,ind,nspeci)
!
!  Compute value of sbo2 from sbo
!
  if (sbo.le.0.0_dp) then
    sbo2 = 0.0_dp
    if (lgrad1) then
      dsbo2dsbo = 0.0_dp
      if (lgrad2) then
        d2sbo2dsbo2 = 0.0_dp
      endif
    endif
  elseif (sbo.le.1.0_dp) then
    sbo2 = sbo**pval9
    if (lgrad1) then
      dsbo2dsbo = pval9*sbo2/sbo
      if (lgrad2) then
        d2sbo2dsbo2 = pval9*(pval9 - 1.0_dp)*sbo2/sbo**2
      endif
    endif
  elseif (sbo.le.2.0_dp) then
    sbo2 = 2.0_dp - (2.0_dp - sbo)**pval9
    if (lgrad1) then
      dsbo2dsbo = pval9*(2.0_dp - sbo)**(pval9 - 1.0_dp)
      if (lgrad2) then
        d2sbo2dsbo2 = - pval9*(pval9 - 1.0_dp)*(2.0_dp - sbo)**(pval9 - 2.0_dp)
      endif
    endif
  else
    sbo2 = 2.0_dp
    if (lgrad1) then
      dsbo2dsbo = 0.0_dp
      if (lgrad2) then
        d2sbo2dsbo2 = 0.0_dp
      endif
    endif
  endif
!
!  Compute exponential
!
  exp10 = exp(-pval10*(2.0_dp - sbo2))
!
!  Compute function
!
  theta0 = 180.0_dp - theta00*(1.0_dp - exp10)
!
!  Convert units to radians
!
  theta0 = theta0*degtorad
!
!  Calculate derivative
!
  if (lgrad1) then
    dtheta0dsbo = degtorad*theta00*pval10*exp10*dsbo2dsbo
    if (lgrad2) then
      d2theta0dsbo2 = degtorad*theta00*pval10*exp10*(d2sbo2dsbo2 + pval10*dsbo2dsbo*dsbo2dsbo)
    endif
  endif
!
  return
  end
!
  subroutine reaxff_theta(r12,r13,r23,theta,the1d1,the1d2,the1d3,the2d,lgrad1,lgrad2)
!
!  Calculates the angle theta 2-1-3 and derivatives with respect to theta.
!
!   8/07 Created from threebody.f
!  10/07 Second derivatives added
!
!  r12      = distance between atoms 1 and 2
!  r13      = distance between atoms 1 and 3
!  r23      = distance between atoms 2 and 3
!  theta    = angle in radians
!  the1d1   = 1/r12 dtheta/dr12
!  the1d2   = 1/r13 dtheta/dr13
!  the1d3   = 1/r23 dtheta/dr23
!  the2d(6) = Second derivatives of theta with respect to distances
!  lgrad1   = if .true. calculate the first derivatives
!  lgrad2   = if .true. calculate the second derivatives
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
!  Julian Gale, NRI, Curtin University, August 2007
!
  use datatypes
  use constants, only : pi
  implicit none
!
!  Passed variables
!
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
  real(dp),    intent(in)  :: r12
  real(dp),    intent(in)  :: r13
  real(dp),    intent(in)  :: r23
  real(dp),    intent(out) :: theta
  real(dp),    intent(out) :: the1d1
  real(dp),    intent(out) :: the1d2
  real(dp),    intent(out) :: the1d3
  real(dp),    intent(out) :: the2d(6)
!
!  Local variables
!
  logical                  :: l180
  real(dp)                 :: cos1d1
  real(dp)                 :: cos1d2
  real(dp)                 :: cos1d3
  real(dp)                 :: cos2d(6)
  real(dp)                 :: costh
  real(dp)                 :: r122
  real(dp)                 :: r132
  real(dp)                 :: r232
  real(dp)                 :: rr12
  real(dp)                 :: rr122
  real(dp)                 :: rr124
  real(dp)                 :: rr13
  real(dp)                 :: rr132
  real(dp)                 :: rr134
  real(dp)                 :: rsinth
  real(dp)                 :: rsinth2
  real(dp)                 :: sinth2
  real(dp)                 :: trm1
!
!  Set up local constants
!
  r122 = r12*r12
  r132 = r13*r13
  r232 = r23*r23
  rr12 = 1.0_dp/r12
  rr13 = 1.0_dp/r13
!
!  Compute cosine of theta
!
  costh = 0.5_dp*(r122 + r132 - r232)*rr12*rr13
!
!  If theta= 0 or 180 then skip derivatives if there is no
!  distance dependence
!
  l180 = (abs(costh).gt.0.999999_dp)
!
!  Compute theta
!
  if (l180) then
    if (costh.gt.0.0_dp) then
      theta = 0.0_dp
    else
      theta = pi
    endif
  else
    theta = acos(costh)
  endif
  if (lgrad1) then
!
!  For theta = 180 case ensure derivatives are zeroed
!  ready for return
!
    if (l180) then
      the1d1 = 0.0_dp
      the1d2 = 0.0_dp
      the1d3 = 0.0_dp
      if (lgrad2) then
        the2d(1:6) = 0.0_dp
      endif
      return
    endif
    rr122 = rr12*rr12
    rr132 = rr13*rr13
!***************************************
!  Set up potential independent terms  *
!***************************************
!
!  Calculate inverse sin(theta) as this forms part of d(theta)/dr
!
    sinth2 = 1.0_dp - costh**2
    rsinth2 = 1.0_dp/sinth2
    rsinth = sqrt(rsinth2)
!****************************************
!  Calculate derivatives of cos(theta)  *
!****************************************
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r23
!
    cos1d1 = rr12*rr13 - costh*rr122
    cos1d2 = rr12*rr13 - costh*rr132
    cos1d3 = - rr12*rr13
!***********************************
!  Calculate derivatives of theta  *
!***********************************
!
!  First
!
    the1d1 = - cos1d1*rsinth
    the1d2 = - cos1d2*rsinth
    the1d3 = - cos1d3*rsinth
    if (lgrad2) then
      rr124 = rr122*rr122
      rr134 = rr132*rr132
!
!  Second
!
      cos2d(1) = - 2.0_dp*rr122*rr12*rr13 + 3.0_dp*costh*rr124
      cos2d(2) = costh*rr122*rr132 - rr12*rr13*(rr122+rr132)
      cos2d(3) = rr122*rr12*rr13
      cos2d(4) = - 2.0_dp*rr132*rr13*rr12 + 3.0_dp*costh*rr134
      cos2d(5) = rr132*rr12*rr13
      cos2d(6) = 0.0_dp
!
      trm1 = costh*rsinth2
      the2d(1) = trm1*cos1d1*the1d1 - rsinth*cos2d(1)
      the2d(2) = trm1*cos1d1*the1d2 - rsinth*cos2d(2)
      the2d(3) = trm1*cos1d1*the1d3 - rsinth*cos2d(3)
      the2d(4) = trm1*cos1d2*the1d2 - rsinth*cos2d(4)
      the2d(5) = trm1*cos1d2*the1d3 - rsinth*cos2d(5)
      the2d(6) = trm1*cos1d3*the1d3 - rsinth*cos2d(6)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f9(deltai,f9,df9ddeltai,d2f9ddeltai2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f9 term for reaxFF. 
!  In GULP i is the pivot atom and j & k are the terminal atoms.
!
!  On entry : 
!
!  deltai          = delta for i
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f9              = function f9 for both bonds
!  df9ddeltai      = first derivative of f9 w.r.t. deltai if lgrad1
!  d2f9ddeltai2    = second derivative of f9 w.r.t. deltai if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
!  11/12 Unused arguments removed
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: deltai
  real(dp),    intent(out)            :: f9
  real(dp),    intent(out)            :: df9ddeltai
  real(dp),    intent(out)            :: d2f9ddeltai2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: exp3
  real(dp)                            :: exp4
  real(dp)                            :: ppen3
  real(dp)                            :: ppen4
  real(dp)                            :: trm1
  real(dp)                            :: trm2
!
!  Set parameters for triad
!
  ppen3 = reaxFFlam(21)
  ppen4 = reaxFFlam(22)
!
!  Compute exponentials
!
  exp3 = exp(-ppen3*deltai)
  exp4 = exp(ppen4*deltai)
!
!  Compute remaining useful terms
!
  trm1 = 1.0_dp/(1.0_dp + exp3 + exp4)
!
!  Compute function
!
  f9 = (2.0_dp + exp3)*trm1
!
!  Calculate derivative
!
  if (lgrad1) then
    trm2 = (-ppen3*exp3+ppen4*exp4)
    df9ddeltai = - trm1*(ppen3*exp3 + trm1*(2.0_dp + exp3)*trm2)
    if (lgrad2) then
      d2f9ddeltai2 = trm1*(ppen3*ppen3*exp3 + trm1*(2.0_dp*ppen3*exp3*trm2 - (2.0_dp + exp3)* &
        (ppen3*ppen3*exp3 + ppen4*ppen4*exp4) + trm1*(2.0_dp*(2.0_dp + exp3)*trm2*trm2)))
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f10(BOij,BOjk,BOkl,f10,df10dBOij,df10dBOjk,df10dBOkl,d2f10dBOij2,d2f10dBOijdBOjk, &
                        d2f10dBOijdBOkl,d2f10dBOjk2,d2f10dBOjkdBOkl,d2f10dBOkl2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f10 term for reaxFF. 
!  Here j & k represent the central torsional atoms whose delta values are being passed in 
!
!  On entry : 
!
!  BOij            = bond order for i-j
!  BOjk            = bond order for j-k
!  BOkl            = bond order for k-l
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the first derivative 
!
!  On exit :
!
!  f10              = function f10 
!  df10dBOij        = first derivative of f10 w.r.t. BOij if lgrad1
!  df10dBOjk        = first derivative of f10 w.r.t. BOjk if lgrad1
!  df10dBOkl        = first derivative of f10 w.r.t. BOkl if lgrad1
!  d2f10dBOij2      = second derivative of f10 w.r.t. BOij if lgrad2
!  d2f10dBOijdBOjk  = second derivative of f10 w.r.t. BOij/BOjk if lgrad2
!  d2f10dBOijdBOkl  = second derivative of f10 w.r.t. BOij/BOkl if lgrad2
!  d2f10dBOjk2      = second derivative of f10 w.r.t. BOjk if lgrad2
!  d2f10dBOjkdBOkl  = second derivative of f10 w.r.t. BOjk/BOkl if lgrad2
!  d2f10dBOkl2      = second derivative of f10 w.r.t. BOkl if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
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
!  Julian Gale, NRI, Curtin University, October 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: BOij
  real(dp),    intent(in)             :: BOjk
  real(dp),    intent(in)             :: BOkl
  real(dp),    intent(out)            :: f10
  real(dp),    intent(out)            :: df10dBOij
  real(dp),    intent(out)            :: df10dBOjk
  real(dp),    intent(out)            :: df10dBOkl
  real(dp),    intent(out)            :: d2f10dBOij2
  real(dp),    intent(out)            :: d2f10dBOijdBOjk
  real(dp),    intent(out)            :: d2f10dBOijdBOkl
  real(dp),    intent(out)            :: d2f10dBOjk2
  real(dp),    intent(out)            :: d2f10dBOjkdBOkl
  real(dp),    intent(out)            :: d2f10dBOkl2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: expij
  real(dp)                            :: expjk
  real(dp)                            :: expkl
  real(dp)                            :: ptor2
!
!  Set parameters for quartet
!
  ptor2 = reaxFFlam(24)
!
!  Compute exponentials
!
  expij = exp(-ptor2*BOij)
  expjk = exp(-ptor2*BOjk)
  expkl = exp(-ptor2*BOkl)
!
!  Compute function
!
  f10 = (1.0_dp - expij)*(1.0_dp - expjk)*(1.0_dp - expkl)
!
!  Calculate derivative
!
  if (lgrad1) then
    df10dBOij = ptor2*expij*(1.0_dp - expjk)*(1.0_dp - expkl)
    df10dBOjk = ptor2*expjk*(1.0_dp - expij)*(1.0_dp - expkl)
    df10dBOkl = ptor2*expkl*(1.0_dp - expij)*(1.0_dp - expjk)
    if (lgrad2) then
      d2f10dBOij2 = - ptor2*ptor2*expij*(1.0_dp - expjk)*(1.0_dp - expkl)
      d2f10dBOjk2 = - ptor2*ptor2*expjk*(1.0_dp - expij)*(1.0_dp - expkl)
      d2f10dBOkl2 = - ptor2*ptor2*expkl*(1.0_dp - expij)*(1.0_dp - expjk)
      d2f10dBOijdBOjk = ptor2*ptor2*expij*expjk*(1.0_dp - expkl)
      d2f10dBOijdBOkl = ptor2*ptor2*expij*expkl*(1.0_dp - expjk)
      d2f10dBOjkdBOkl = ptor2*ptor2*expjk*expkl*(1.0_dp - expij)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f11(nspeci,nspecj,deltai,deltaj,f11,df11ddeltaij,d2f11ddeltaij2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f11 term for reaxFF. 
!  Here i & j represent the central torsional atoms whose delta values are being passed in 
!
!  On entry : 
!
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  deltai          = delta for i
!  deltaj          = delta for j
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f11              = function f11 
!  df11ddeltaij     = first derivative of f11 w.r.t. deltai or deltaj if lgrad1
!  d2f11ddeltaij2   = second derivative of f11 w.r.t. deltai or deltaj if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
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
!  Julian Gale, NRI, Curtin University, October 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: deltai
  real(dp),    intent(in)             :: deltaj
  real(dp),    intent(out)            :: f11
  real(dp),    intent(out)            :: df11ddeltaij
  real(dp),    intent(out)            :: d2f11ddeltaij2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: exp3
  real(dp)                            :: exp4
  real(dp)                            :: ptor3
  real(dp)                            :: ptor4
  real(dp)                            :: sumdelta
  real(dp)                            :: trm1
  real(dp)                            :: trm2
!
!  Set parameters for quartet
!
  ptor3 = reaxFFlam(25)
  ptor4 = reaxFFlam(26)
!
!  Compute sum of delta_angle values
!
  sumdelta = deltai + reaxFFval(1,nspeci) - reaxFFval(4,nspeci) + deltaj + reaxFFval(1,nspecj) - reaxFFval(4,nspecj)
!
!  Compute exponentials
!
  exp3 = exp(-ptor3*sumdelta)
  exp4 = exp(ptor4*sumdelta)
!
!  Compute remaining useful terms
!
  trm1 = 1.0_dp/(1.0_dp + exp3 + exp4)
!
!  Compute function
!
  f11 = (2.0_dp + exp3)*trm1
!
!  Calculate derivative
!
  if (lgrad1) then
    trm2 = - ptor3*exp3 + ptor4*exp4
    df11ddeltaij = - trm1*(ptor3*exp3 + trm1*(2.0_dp + exp3)*trm2)
    if (lgrad2) then
      d2f11ddeltaij2 = trm1*(ptor3*ptor3*exp3 + trm1*(2.0_dp*ptor3*exp3*trm2 - (2.0_dp + exp3)* &
        (ptor3*ptor3*exp3 + ptor4*ptor4*exp4) + trm1*(2.0_dp*(2.0_dp + exp3)*trm2*trm2)))
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f12(BOij,BOjk,BOkl,f12,df12dBOij,df12dBOjk,df12dBOkl,d2f12dBOij2,d2f12dBOijdBOjk, &
                        d2f12dBOijdBOkl,d2f12dBOjk2,d2f12dBOjkdBOkl,d2f12dBOkl2,lStrict,lgrad1,lgrad2)
!
!  Subroutine to calculate the f12 term for reaxFF. 
!  Here j & k represent the central torsional atoms whose delta values are being passed in 
!
!  On entry : 
!
!  BOij            = bond order for i-j
!  BOjk            = bond order for j-k
!  BOkl            = bond order for k-l
!  lStrict         = if .true. don't smooth function
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f12              = function f12 
!  df12dBOij        = first derivative of f12 w.r.t. BOij if lgrad1
!  df12dBOjk        = first derivative of f12 w.r.t. BOjk if lgrad1
!  df12dBOkl        = first derivative of f12 w.r.t. BOkl if lgrad1
!  d2f12dBOij2      = second derivative of f12 w.r.t. BOij if lgrad2
!  d2f12dBOijdBOjk  = second derivative of f12 w.r.t. BOij/BOjk if lgrad2
!  d2f12dBOijdBOkl  = second derivative of f12 w.r.t. BOij/BOkl if lgrad2
!  d2f12dBOjk2      = second derivative of f12 w.r.t. BOjk if lgrad2
!  d2f12dBOjkdBOkl  = second derivative of f12 w.r.t. BOjk/BOkl if lgrad2
!  d2f12dBOkl2      = second derivative of f12 w.r.t. BOkl if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
!   4/08 lStrict option added to control whether we use strictly the
!        ReaxFF formalism or whether we send terms to zero at the cutoff.
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: BOij
  real(dp),    intent(in)             :: BOjk
  real(dp),    intent(in)             :: BOkl
  real(dp),    intent(out)            :: f12
  real(dp),    intent(out)            :: df12dBOij
  real(dp),    intent(out)            :: df12dBOjk
  real(dp),    intent(out)            :: df12dBOkl
  real(dp),    intent(out)            :: d2f12dBOij2
  real(dp),    intent(out)            :: d2f12dBOijdBOjk
  real(dp),    intent(out)            :: d2f12dBOijdBOkl
  real(dp),    intent(out)            :: d2f12dBOjk2
  real(dp),    intent(out)            :: d2f12dBOjkdBOkl
  real(dp),    intent(out)            :: d2f12dBOkl2
  logical,     intent(in)             :: lStrict
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: expij
  real(dp)                            :: expjk
  real(dp)                            :: expkl
  real(dp)                            :: exptol
  real(dp)                            :: pcot2
!
!  Set parameters for quartet
!
  pcot2 = reaxFFlam(27)
!
!  Compute exponentials
!
  expij = exp(-pcot2*(BOij - 1.5_dp)**2)
  expjk = exp(-pcot2*(BOjk - 1.5_dp)**2)
  expkl = exp(-pcot2*(BOkl - 1.5_dp)**2)
!
!  Compute function
!
  if (lStrict) then
    f12 = expij*expjk*expkl
  else
    exptol = exp(-pcot2*(reaxFFatol - 1.5_dp)**2)
    f12 = (expij - exptol)*(expjk - exptol)*(expkl - exptol)
  endif
!
!  Calculate derivatives
!
  if (lgrad1) then
    if (lStrict) then
      df12dBOij = - 2.0_dp*pcot2*f12*(BOij - 1.5_dp)
      df12dBOjk = - 2.0_dp*pcot2*f12*(BOjk - 1.5_dp)
      df12dBOkl = - 2.0_dp*pcot2*f12*(BOkl - 1.5_dp)
    else
      df12dBOij = - 2.0_dp*pcot2*(BOij - 1.5_dp)*expij*(expjk - exptol)*(expkl - exptol)
      df12dBOjk = - 2.0_dp*pcot2*(BOjk - 1.5_dp)*expjk*(expij - exptol)*(expkl - exptol)
      df12dBOkl = - 2.0_dp*pcot2*(BOkl - 1.5_dp)*expkl*(expij - exptol)*(expjk - exptol)
    endif
    if (lgrad2) then
      d2f12dBOij2     = 4.0_dp*pcot2*pcot2*f12*(BOij - 1.5_dp)*(BOij - 1.5_dp)*expij*(expjk - exptol)*(expkl - exptol) &
                        - 2.0_dp*pcot2*expij*(expjk - exptol)*(expkl - exptol)
      d2f12dBOijdBOjk = 4.0_dp*pcot2*pcot2*f12*(BOij - 1.5_dp)*(BOjk - 1.5_dp)*expij*expjk*(expkl - exptol)
      d2f12dBOijdBOkl = 4.0_dp*pcot2*pcot2*f12*(BOij - 1.5_dp)*(BOkl - 1.5_dp)*expij*expkl*(expjk - exptol)
      d2f12dBOjk2     = 4.0_dp*pcot2*pcot2*f12*(BOjk - 1.5_dp)*(BOjk - 1.5_dp)*expjk*(expij - exptol)*(expkl - exptol) &
                        - 2.0_dp*pcot2*expjk*(expij - exptol)*(expkl - exptol)
      d2f12dBOjkdBOkl = 4.0_dp*pcot2*pcot2*f12*(BOjk - 1.5_dp)*(BOkl - 1.5_dp)*expjk*expkl*(expij - exptol)
      d2f12dBOkl2     = 4.0_dp*pcot2*pcot2*f12*(BOkl - 1.5_dp)*(BOkl - 1.5_dp)*expkl*(expij - exptol)*(expjk - exptol) &
                        - 2.0_dp*pcot2*expkl*(expij - exptol)*(expjk - exptol)
    endif
  endif
!
  return
  end
!
  subroutine reaxFF_f13(rij,gamma,f13,df13drij,d2f13drij2,lgrad1,lgrad2)
!
!  Subroutine to calculate the f13 term for reaxFF. 
!
!  On entry : 
!
!  rij             = distance for i-j
!  gamma           = gamma screening parameter
!  lgrad1          = if .true. compute the first derivative 
!  lgrad2          = if .true. compute the second derivative 
!
!  On exit :
!
!  f13              = function f13 
!  df13drij         = 1/rij x first derivative of f13 w.r.t. rij if lgrad1
!  d2f13drij2       = 1/rij x second derivative of f13 w.r.t. rij if lgrad2
!
!   8/07 Created
!  10/07 Second derivatives added
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
!  Julian Gale, NRI, Curtin University, October 2007
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: rij
  real(dp),    intent(in)             :: gamma
  real(dp),    intent(out)            :: f13
  real(dp),    intent(out)            :: df13drij
  real(dp),    intent(out)            :: d2f13drij2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  real(dp)                            :: trm1
  real(dp)                            :: trm2
  real(dp)                            :: pvdw1
!
!  Set parameters 
!
  pvdw1 = reaxFFlam(28)
!
!  Compute terms
!
  trm1 = rij**pvdw1
  trm2 = gamma**(-pvdw1)
!
!  Compute function
!
  f13 = (trm1 + trm2)**(1.0_dp/pvdw1)
!
!  Calculate derivative
!
  if (lgrad1) then
    df13drij = (f13/(trm1 + trm2))*rij**(pvdw1 - 2.0_dp)
    if (lgrad2) then
      d2f13drij2 = (f13/(trm1 + trm2))*(pvdw1 - 2.0_dp)*rij**(pvdw1 - 4.0_dp) + &
                   (f13/(trm1 + trm2)**2)*(1.0_dp - pvdw1)*rij**(2.0_dp*pvdw1 - 4.0_dp)
    endif
  endif
!
  return
  end
!
  subroutine reaxff_sintheta(r12,r13,r23,sintheta,sinthe1d1,sinthe1d2,sinthe1d3,sinthe2d,lgrad1,lgrad2)
!
!  Calculates the sine of the angle theta 2-1-3 and derivatives with respect to sin(theta).
!
!   8/07 Created from threebody.f
!
!  r12    = distance between atoms 1 and 2
!  r13    = distance between atoms 1 and 3
!  r23    = distance between atoms 2 and 3
!  sintheta  = sine of angle
!  sinthe1d1 = 1/r12 dsintheta/dr12
!  sinthe1d2 = 1/r13 dsintheta/dr13
!  sinthe1d3 = 1/r23 dsintheta/dr23
!  sinthe2d(6) = second derivatives of sine theta
!  lgrad1 = if .true. calculate the first derivatives
!  lgrad2 = if .true. calculate the second derivatives
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
!  Julian Gale, NRI, Curtin University, October 2007
!
  use datatypes
  implicit none
!
!  Passed variables
!
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
  real(dp),    intent(in)  :: r12
  real(dp),    intent(in)  :: r13
  real(dp),    intent(in)  :: r23
  real(dp),    intent(out) :: sintheta
  real(dp),    intent(out) :: sinthe1d1
  real(dp),    intent(out) :: sinthe1d2
  real(dp),    intent(out) :: sinthe1d3
  real(dp),    intent(out) :: sinthe2d(6)
!
!  Local variables
!
  logical                  :: l180
  real(dp)                 :: cos1d1
  real(dp)                 :: cos1d2
  real(dp)                 :: cos1d3
  real(dp)                 :: costh
  real(dp)                 :: cos2d(6)
  real(dp)                 :: r122
  real(dp)                 :: r132
  real(dp)                 :: r232
  real(dp)                 :: rr12
  real(dp)                 :: rr122
  real(dp)                 :: rr124
  real(dp)                 :: rr13
  real(dp)                 :: rr132
  real(dp)                 :: rr134
  real(dp)                 :: rsinth
  real(dp)                 :: rtan
!
!  Set up local constants
!
  r122 = r12*r12
  r132 = r13*r13
  r232 = r23*r23
  rr12 = 1.0_dp/r12
  rr13 = 1.0_dp/r13
!
!  Compute cosine of theta
!
  costh = 0.5_dp*(r122 + r132 - r232)*rr12*rr13
!
!  If theta= 0 or 180 then skip derivatives if there is no
!  distance dependence
!
  l180 = (abs(costh).gt.0.999999_dp)
  if (l180) then
    sintheta = 0.0_dp
  else
    sintheta = sqrt(1.0_dp - costh**2)
  endif
  if (lgrad1) then
    rr122 = rr12*rr12
    rr132 = rr13*rr13
!
!  For theta = 180 case ensure derivatives are zeroed
!  ready for return
!
    if (l180) then
      sinthe1d1 = 0.0_dp
      sinthe1d2 = 0.0_dp
      sinthe1d3 = 0.0_dp
      if (lgrad2) then
        sinthe2d(1:6) = 0.0_dp
      endif
      return
    endif
!***************************************
!  Set up potential independent terms  *
!***************************************
!
!  Calculate inverse sin(theta) as this forms part of d(theta)/dr
!
    rsinth = 1.0_dp/sintheta
!****************************************
!  Calculate derivatives of cos(theta)  *
!****************************************
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r23
!
    cos1d1 = rr12*rr13 - costh*rr122
    cos1d2 = rr12*rr13 - costh*rr132
    cos1d3 = - rr12*rr13
!***********************************
!  Calculate derivatives of theta  *
!***********************************
!
!  First
!
    rtan = costh*rsinth
    sinthe1d1 = - rtan*cos1d1
    sinthe1d2 = - rtan*cos1d2
    sinthe1d3 = - rtan*cos1d3
    if (lgrad2) then
      rr124 = rr122*rr122
      rr134 = rr132*rr132
!
      cos2d(1) = - 2.0_dp*rr122*rr12*rr13 + 3.0_dp*costh*rr124
      cos2d(2) = costh*rr122*rr132 - rr12*rr13*(rr122+rr132)
      cos2d(3) = rr122*rr12*rr13
      cos2d(4) = - 2.0_dp*rr132*rr13*rr12 + 3.0_dp*costh*rr134
      cos2d(5) = rr132*rr12*rr13
      cos2d(6) = 0.0_dp
!
      sinthe2d(1) = - rtan*cos2d(1) - rsinth*cos1d1*cos1d1*(1.0_dp + rtan*rtan)
      sinthe2d(2) = - rtan*cos2d(2) - rsinth*cos1d1*cos1d2*(1.0_dp + rtan*rtan)
      sinthe2d(3) = - rtan*cos2d(3) - rsinth*cos1d1*cos1d3*(1.0_dp + rtan*rtan)
      sinthe2d(4) = - rtan*cos2d(4) - rsinth*cos1d2*cos1d2*(1.0_dp + rtan*rtan)
      sinthe2d(5) = - rtan*cos2d(5) - rsinth*cos1d2*cos1d3*(1.0_dp + rtan*rtan)
      sinthe2d(6) = - rtan*cos2d(6) - rsinth*cos1d3*cos1d3*(1.0_dp + rtan*rtan)
    endif
  endif
!
  return
  end
!
  subroutine reaxff_cosphi(r12,r13,r14,r23,r24,r34,cosphi,cosp1d,cosp2d,lgrad1,lgrad2)
!
!  Calculates the torsional angle phi and it's derivatives
!
!   8/07 Created based on fourbody.f90
!  10/07 Second derivatives added
!
!  r12     = distance between atoms 1 and 2
!  r13     = distance between atoms 1 and 3
!  r14     = distance between atoms 1 and 4
!  r23     = distance between atoms 2 and 3
!  r24     = distance between atoms 2 and 4
!  r34     = distance between atoms 3 and 4
!  cosphi  = cosine of torsional angle
!  cosp1d  = array of first derivative terms
!  cosp2d  = array of second derivative terms
!  lgrad1  = if .true. calculate the first derivatives
!  lgrad2  = if .true. calculate the second derivatives
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
!  Julian Gale, NRI, Curtin University, October 2007
!
  use constants
  use control
  implicit none
!
!  Passed variables
!
  logical,  intent(in)   :: lgrad1
  logical,  intent(in)   :: lgrad2
  real(dp), intent(out)  :: cosp1d(6)
  real(dp), intent(out)  :: cosp2d(21)
  real(dp), intent(out)  :: cosphi
  real(dp), intent(in)   :: r12
  real(dp), intent(in)   :: r13
  real(dp), intent(in)   :: r14
  real(dp), intent(in)   :: r23
  real(dp), intent(in)   :: r24
  real(dp), intent(in)   :: r34
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: j
  real(dp)    :: bot0
  real(dp)    :: cos1
  real(dp)    :: cos2
  real(dp)    :: cos3
  real(dp)    :: cos11d(6)
  real(dp)    :: cos12d(21)
  real(dp)    :: cos31d(6)
  real(dp)    :: cos32d(21)
  real(dp)    :: r122
  real(dp)    :: r132
  real(dp)    :: r142
  real(dp)    :: r232
  real(dp)    :: r242
  real(dp)    :: r342
  real(dp)    :: rr12
  real(dp)    :: rr122
  real(dp)    :: rr124
  real(dp)    :: rr23
  real(dp)    :: rr232
  real(dp)    :: rr234
  real(dp)    :: rr24
  real(dp)    :: rr242
  real(dp)    :: rr244
  real(dp)    :: rsin1
  real(dp)    :: rsin3
  real(dp)    :: rtan1
  real(dp)    :: rtan3
  real(dp)    :: sin11d(6)
  real(dp)    :: sin12d(21)
  real(dp)    :: sin31d(6)
  real(dp)    :: sin32d(21)
  real(dp)    :: sin1
  real(dp)    :: sin3
  real(dp)    :: top0
  real(dp)    :: top1(6),bot1(6)
  real(dp)    :: top2(21),bot2(21)
!
!  Zero terms
!
  if (lgrad1) then
    cos11d(1:6) = 0.0_dp
    cos31d(1:6) = 0.0_dp
    cosp1d(1:6) = 0.0_dp
    sin11d(1:6) = 0.0_dp
    sin31d(1:6) = 0.0_dp
    top1(1:6) = 0.0_dp
    bot1(1:6) = 0.0_dp
    if (lgrad2) then
      cos12d(1:21) = 0.0_dp
      cos32d(1:21) = 0.0_dp
      cosp2d(1:21) = 0.0_dp
      sin12d(1:21) = 0.0_dp
      sin32d(1:21) = 0.0_dp
      top2(1:21) = 0.0_dp
      bot2(1:21) = 0.0_dp
    endif
  endif
!
!  Set up local constants
!
  r122 = r12*r12
  r132 = r13*r13
  r142 = r14*r14
  r232 = r23*r23
  r242 = r24*r24
  r342 = r34*r34
  rr12 = 1.0_dp/r12
  rr23 = 1.0_dp/r23
  rr24 = 1.0_dp/r24
  rr122 = rr12*rr12
  rr232 = rr23*rr23
  rr242 = rr24*rr24
!$$$$$$$$$$$$$$$$$
!  Cosine terms  $
!$$$$$$$$$$$$$$$$$
!
!  Cosine theta 1 = 1-2-3, 2 = 2-3-4 and 3 = 3-2-4
!
  cos1 = 0.5_dp*(r232+r122-r132)/(r12*r23)
  cos2 = 0.5_dp*(r232+r342-r242)/(r23*r34)
  cos3 = 0.5_dp*(r232+r242-r342)/(r23*r24)
!
!  Check for angles which are 0 or 180 degrees with potentials
!  which cannot cope with these. For those that can the four-
!  body contribution must go to zero when the angle approaches
!  this limit - hence we can just return having set all the
!  derivatives and energy to zero
!
  if (abs(cos1).ge.0.99999999_dp) then
    cosphi = 0.0_dp
    if (lgrad1) then
      cosp1d(1:6) = 0.0_dp
      if (lgrad2) then
        cosp2d(1:21) = 0.0_dp
      endif
    endif
    return
  endif
  if (abs(cos2).ge.0.99999999_dp) then
    cosphi = 0.0_dp
    if (lgrad1) then
      cosp1d(1:6) = 0.0_dp
      if (lgrad2) then
        cosp2d(1:21) = 0.0_dp
      endif
    endif
    return
  endif
  if (lgrad1) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r14
!  4 = r23
!  5 = r24
!  6 = r34
!
    cos11d(1) = rr12*rr23 - cos1*rr122
    cos11d(2) = - rr12*rr23
    cos11d(4) = rr12*rr23 - cos1*rr232
!
    cos31d(4) = rr23*rr24 - cos3*rr232
    cos31d(5) = rr23*rr24 - cos3*rr242
    cos31d(6) = - rr23*rr24
    if (lgrad2) then
      rr124 = rr122*rr122
      rr234 = rr232*rr232
      rr244 = rr242*rr242
!
      cos12d(1) = - 2.0_dp*rr122*rr12*rr23 + 3.0_dp*cos1*rr124
      cos12d(2) = rr122*rr12*rr23
      cos12d(4) = rr12*rr23*(cos1*rr12*rr23 - rr122 - rr232)
      cos12d(9) = rr232*rr23*rr12
      cos12d(16) = - 2.0_dp*rr232*rr23*rr12 + 3.0_dp*cos1*rr234
!
      cos32d(16) = - 2.0_dp*rr232*rr23*rr24 + 3.0_dp*cos3*rr234
      cos32d(17) = rr23*rr24*(cos3*rr23*rr24 - rr232 - rr242)
      cos32d(18) = rr232*rr23*rr24
      cos32d(19) = - 2.0_dp*rr242*rr24*rr23 + 3.0_dp*cos3*rr244
      cos32d(20) = rr242*rr24*rr23
    endif
  endif
!$$$$$$$$$$$$$$$
!  Sine terms  $
!$$$$$$$$$$$$$$$
  sin1 = sqrt(1.0_dp - cos1*cos1)
  sin3 = sqrt(1.0_dp - cos3*cos3)
  rsin1 = 1.0_dp/sin1
  rsin3 = 1.0_dp/sin3
  if (lgrad1) then
!
!  First derivatives
!
    rtan1 = cos1*rsin1
    rtan3 = cos3*rsin3
    do i = 1,6
      sin11d(i) = - rtan1*cos11d(i)
      sin31d(i) = - rtan3*cos31d(i)
    enddo
    if (lgrad2) then
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          sin12d(ii) = - rtan1*cos12d(ii) - rsin1*cos11d(i)*cos11d(j)*(1.0_dp + rtan1*rtan1)
          sin32d(ii) = - rtan3*cos32d(ii) - rsin3*cos31d(i)*cos31d(j)*(1.0_dp + rtan3*rtan3)
        enddo
      enddo
    endif
  endif
!$$$$$$$$$$$$$$
!  Phi terms  $
!$$$$$$$$$$$$$$
  top0 = r122 + r242 - r142 - 2.0_dp*r12*r24*cos1*cos3
  bot0 = rr12*rr24*rsin1*rsin3
  cosphi = 0.5_dp*top0*bot0
  if (abs(cosphi).gt.1.0_dp) cosphi = sign(1.0_dp,cosphi)
  if (lgrad1) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r14
!  4 = r23
!  5 = r24
!  6 = r34
!
    top1(1) = 2.0_dp - 2.0_dp*r24*cos1*cos3*rr12
    top1(3) = - 2.0_dp
    top1(5) = 2.0_dp - 2.0_dp*r12*cos1*cos3*rr24
    do i = 1,6
      top1(i) = top1(i) - 2.0_dp*r12*r24*(cos11d(i)*cos3+cos1*cos31d(i))
    enddo
    bot1(1) = r24*sin1*sin3*rr12
    bot1(5) = r12*sin1*sin3*rr24
    do i = 1,6
      bot1(i) = bot1(i) + r12*r24*(sin11d(i)*sin3 + sin1*sin31d(i))
    enddo
!
!  Combine derivatives
!
    do i = 1,6
      cosp1d(i) = 0.5_dp*bot0*(top1(i) - bot0*top0*bot1(i))
    enddo
    if (lgrad2) then
!  
!  Top / bottom part of cosphi derivatives
!     
      ii = 0 
      do i = 1,6
        do j = i,6
          ii = ii + 1
          top2(ii) = top2(ii) - 2.0_dp*r12*r24*(cos12d(ii)*cos3+cos1*cos32d(ii))
          top2(ii) = top2(ii) - 2.0_dp*r12*r24*(cos11d(i)*cos31d(j)+cos11d(j)*cos31d(i))
          if (i.eq.1) then
            top2(ii) = top2(ii) - 2.0_dp*r24*rr12*(cos1*cos31d(j)+cos3*cos11d(j))
            if (j.eq.1) then
              top2(ii) = top2(ii) + 2.0_dp*cos1*cos3*r24*rr122*rr12
            endif
          elseif (i.eq.5) then 
            top2(ii) = top2(ii) - 2.0_dp*r12*rr24*(cos1*cos31d(j)+cos3*cos11d(j))
            if (j.eq.5) then
              top2(ii) = top2(ii) + 2.0_dp*cos1*cos3*r12*rr242*rr24
            elseif (j.eq.1) then 
              top2(ii) = top2(ii) - 2.0_dp*cos1*cos3*rr24*rr12
            endif
          endif
          if (j.eq.1) then
            top2(ii) = top2(ii) - 2.0_dp*r24*rr12*(cos1*cos31d(i)+cos3*cos11d(i))
          elseif (j.eq.5) then 
            top2(ii) = top2(ii) - 2.0_dp*r12*rr24*(cos1*cos31d(i)+cos3*cos11d(i))
            if (i.eq.1) then
              top2(ii) = top2(ii) - 2.0_dp*cos1*cos3*rr24*rr12
            endif
          endif
        enddo
      enddo
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          bot2(ii) = bot2(ii) + r12*r24*(sin12d(ii)*sin3+sin1*sin32d(ii))
          bot2(ii) = bot2(ii) + r12*r24*(sin11d(i)*sin31d(j)+sin11d(j)*sin31d(i))
          if (i.eq.1) then
            bot2(ii) = bot2(ii) + r24*rr12*(sin1*sin31d(j)+sin3*sin11d(j))
            if (j.eq.1) then
              bot2(ii) = bot2(ii) - sin1*sin3*r24*rr122*rr12
            endif
          elseif (i.eq.5) then
            bot2(ii) = bot2(ii) + r12*rr24*(sin1*sin31d(j)+sin3*sin11d(j))
            if (j.eq.5) then
              bot2(ii) = bot2(ii) - sin1*sin3*r12*rr242*rr24
            elseif (j.eq.1) then
              bot2(ii) = bot2(ii) + sin1*sin3*rr24*rr12
            endif
          endif
          if (j.eq.1) then
            bot2(ii) = bot2(ii) + r24*rr12*(sin1*sin31d(i)+sin3*sin11d(i))
          elseif (j.eq.5) then 
            bot2(ii) = bot2(ii) + r12*rr24*(sin1*sin31d(i)+sin3*sin11d(i))
            if (i.eq.1) then
              bot2(ii) = bot2(ii) + sin1*sin3*rr24*rr12
            endif
          endif
        enddo
      enddo
!
!  Combine derivatives
!
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          cosp2d(ii) = 0.5_dp*bot0*bot0*(2.0_dp*top0*bot0*bot1(j)*bot1(i)-top1(j)*bot1(i)-top1(i)*bot1(j))
        enddo
      enddo
      do i = 1,21 
        cosp2d(i) = cosp2d(i) + 0.5_dp*bot0*(top2(i)-bot0*top0*bot2(i))
      enddo
    endif
  endif
!
  return
  end
!
  subroutine p7reaxFFvdwtaper(r,rmax,f,dfdr,d2fdr2,lgrad1,lgrad2)
!
!  Subroutine to calculate the taper function and derivatives 
!  using a seventh order polynomial interpolation for reaxFF.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper = 0
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!
!  11/07 Created
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
!  Julian Gale, NRI, Curtin University, November 2007
!
  use datatypes
  use reaxFFdata
  use two
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
  if (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
!
!  Use the fact that we know some coefficients are zero
!
    f = (((reaxFFtaperVDW(8)*r + reaxFFtaperVDW(7))*r + reaxFFtaperVDW(6))*r + reaxFFtaperVDW(5))*r*r*r*r + reaxFFtaperVDW(1)
    if (lgrad1) then
      dfdr = (((7.0_dp*reaxFFtaperVDW(8)*r + 6.0_dp*reaxFFtaperVDW(7))*r + 5.0_dp*reaxFFtaperVDW(6))*r + &
                4.0_dp*reaxFFtaperVDW(5))*r*r
      if (lgrad2) then
        d2fdr2 = (((42.0_dp*reaxFFtaperVDW(8)*r + 30.0_dp*reaxFFtaperVDW(7))*r + 20.0_dp*reaxFFtaperVDW(6))*r + &
                12.0_dp*reaxFFtaperVDW(5))*r*r
      endif
    endif
  endif
!
  return
  end
!
  subroutine p7reaxFFqtaper(r,rmax,f,dfdr,d2fdr2,lgrad1,lgrad2)
!
!  Subroutine to calculate the taper function and derivatives 
!  using a seventh order polynomial interpolation for reaxFF.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  rmin            = minimum distance of taper = 0
!  rmax            = maximum distance of taper
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!
!  On exit :
!
!  f               = the value of the taper function at r
!  dfdr            = first derivative of taper function
!  d2fwdr2         = second derivative of taper function
!
!  11/07 Created
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
!  Julian Gale, NRI, Curtin University, November 2007
!
  use datatypes
  use reaxFFdata
  use two
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: rmax
  real(dp),    intent(out)            :: f
  real(dp),    intent(out)            :: dfdr
  real(dp),    intent(out)            :: d2fdr2
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
  if (r .ge. rmax) then
!*************
!  r > rmax  *
!*************
    f = 0.0_dp
    if (lgrad1) then
      dfdr = 0.0_dp
      if (lgrad2) then
        d2fdr2 = 0.0_dp
      endif
    endif
  else
!********************
!  rmin < r < rmax  *
!********************
!
!  Use the fact that we know some coefficients are zero
!
    f = (((reaxFFtaperQ(8)*r + reaxFFtaperQ(7))*r + reaxFFtaperQ(6))*r + reaxFFtaperQ(5))*r*r*r*r + reaxFFtaperQ(1)
    if (lgrad1) then
      dfdr = (((7.0_dp*reaxFFtaperQ(8)*r + 6.0_dp*reaxFFtaperQ(7))*r + 5.0_dp*reaxFFtaperQ(6))*r + &
                4.0_dp*reaxFFtaperQ(5))*r*r
      if (lgrad2) then
        d2fdr2 = (((42.0_dp*reaxFFtaperQ(8)*r + 30.0_dp*reaxFFtaperQ(7))*r + 20.0_dp*reaxFFtaperQ(6))*r + &
                12.0_dp*reaxFFtaperQ(5))*r*r
      endif
    endif
  endif
!
  return
  end
!
  subroutine reaxFFbocut_sigma(nboij,nspeci,nspecj,BOtol,rcut)
!
!  Subroutine to calculate the cutoff distance beyond which the sigma
!  bond order is below the bond order tolerance.
!
!  On entry : 
!
!  nboij           = index for pair of reaxFF species
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  BOtol           = bond order tolerance
!
!  On exit :
!
!  rcut            = cutoff distance between i and j
!
!   4/08 Created
!   4/08 Corrected for factor of 1 + BOtol
!   1/09 lreaxFFpboOK added 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nboij
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: BOtol
  real(dp),    intent(out)            :: rcut
!
!  Local variables
!
  logical                             :: ltrm1
  real(dp)                            :: r0ij1
  real(dp)                            :: rrp1
!
!  If radii for either species is >= 0, then term is zero
!
  ltrm1 = (reaxFFr(1,nspeci).gt.0.0_dp.and.reaxFFr(1,nspecj).gt.0.0_dp)
  if (ltrm1.and.lreaxFFpboOK(nboij)) then
    if (reaxFFmorse(4,nboij).gt.0.0_dp) then
      r0ij1 = reaxFFmorse(4,nboij)
    else
      r0ij1 = 0.5_dp*(reaxFFr(1,nspeci) + reaxFFr(1,nspecj))
    endif
    rrp1 = log((BOtol/(1.0_dp + BOtol)))/reaxFFpbo(1,nboij)
    rcut = r0ij1*(rrp1**(1.0_dp/reaxFFpbo(2,nboij)))
  else
    rcut = 0.0_dp
  endif
!
  return
  end
!
  subroutine reaxFFbocut_pi(nboij,nspeci,nspecj,BOtol,rcut)
!
!  Subroutine to calculate the cutoff distance beyond which the pi
!  bond order is below the bond order tolerance.
!
!  On entry : 
!
!  nboij           = index for pair of reaxFF species
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  BOtol           = bond order tolerance
!
!  On exit :
!
!  rcut            = cutoff distance between i and j
!
!   4/08 Created
!   1/09 lreaxFFpboOK added 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nboij
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: BOtol
  real(dp),    intent(out)            :: rcut
!
!  Local variables
!
  logical                             :: ltrm1
  real(dp)                            :: r0ij1
  real(dp)                            :: rrp1
!
!  If radii for either species is >= 0, then term is zero
!
  ltrm1 = (reaxFFr(2,nspeci).gt.0.0_dp.and.reaxFFr(2,nspecj).gt.0.0_dp)
  if (ltrm1.and.lreaxFFpboOK(nboij)) then
    if (reaxFFmorse(5,nboij).gt.0.0_dp) then
      r0ij1 = reaxFFmorse(5,nboij)
    else
      r0ij1 = 0.5_dp*(reaxFFr(2,nspeci) + reaxFFr(2,nspecj))
    endif
    rrp1 = log(BOtol)/reaxFFpbo(3,nboij)
    rcut = r0ij1*(rrp1**(1.0_dp/reaxFFpbo(4,nboij)))
  else
    rcut = 0.0_dp
  endif
!
  return
  end
!
  subroutine reaxFFbocut_pipi(nboij,nspeci,nspecj,BOtol,rcut)
!
!  Subroutine to calculate the cutoff distance beyond which the pi-pi
!  bond order is below the bond order tolerance.
!
!  On entry : 
!
!  nboij           = index for pair of reaxFF species
!  nspeci          = species index for i 
!  nspecj          = species index for j 
!  BOtol           = bond order tolerance
!
!  On exit :
!
!  rcut            = cutoff distance between i and j
!
!   4/08 Created
!   1/09 lreaxFFpboOK added 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nboij
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  real(dp),    intent(in)             :: BOtol
  real(dp),    intent(out)            :: rcut
!
!  Local variables
!
  logical                             :: ltrm1
  real(dp)                            :: r0ij1
  real(dp)                            :: rrp1
!
!  If radii for either species is >= 0, then term is zero
!
  ltrm1 = (reaxFFr(3,nspeci).gt.0.0_dp.and.reaxFFr(3,nspecj).gt.0.0_dp)
  if (ltrm1.and.lreaxFFpboOK(nboij)) then
    if (reaxFFmorse(6,nboij).gt.0.0_dp) then
      r0ij1 = reaxFFmorse(6,nboij)
    else
      r0ij1 = 0.5_dp*(reaxFFr(3,nspeci) + reaxFFr(3,nspecj))
    endif
    rrp1 = log(BOtol)/reaxFFpbo(5,nboij)
    rcut = r0ij1*(rrp1**(1.0_dp/reaxFFpbo(6,nboij)))
  else
    rcut = 0.0_dp
  endif
!
  return
  end
