  subroutine getbonds(i,j)
!
!  Finds valid bonded atoms for i-j pair and adds to list
!
!  lbonded = if .true. then atoms are directly connected
!  l2bonds = if .true. then atoms are connected via 2 bonds
!  l3bonds = if .true. then atoms are connected via 3 bonds
!
!  11/04 Earlier if statements added to save work
!   2/07 Bond types added
!   3/07 Intent added
!   3/12 Error in 3 bond checking corrected
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
!  Copyright Curtin University, 2012
!
!  Julian Gale, NRI, Curtin University, March 2012
!
  use current
  use bondvectors
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
  integer(i4), intent(in) :: j
!
!  Local variables
!
  integer(i4)             :: indsum
  integer(i4)             :: icm
  integer(i4)             :: imm
  integer(i4)             :: jcm
  integer(i4)             :: jmm
  integer(i4)             :: kcm
  integer(i4)             :: kmm
  integer(i4)             :: n
  integer(i4)             :: nprevious
  logical                 :: lprevious
  logical                 :: loverwrite
!
!  Initialise number of bond vectors
!
  nbondvec = 0
!
!  Loop over bonds of i
!
  do icm = 1,nbonds(i)
    imm = nbonded(icm,i)
    if (imm.eq.j) then
!***************************
!  1 bond between i and j  *
!***************************
      lprevious = .false.
      do n = 1,nbondvec
        if (nbondvecind(n).eq.nbondind(icm,i)) then
          lprevious = .true.
          nprevious = n
        endif
      enddo
      if (lprevious) then
        nbondvecind(nprevious) = nbondind(icm,i)
        nbtypevec(nprevious)   = nbondedtype(1,icm,i)
        nbtype2vec(nprevious)  = nbondedtype(2,icm,i)
        lbondedvec(nprevious)  = .true.
        l2bondsvec(nprevious)  = .false.
        l3bondsvec(nprevious)  = .false.
      else
        nbondvec = nbondvec + 1
        if (nbondvec.ge.maxbondvec) then
          maxbondvec = nbondvec + 3
          call changemaxbondvec
        endif
        nbondvecind(nbondvec) = nbondind(icm,i)
        nbtypevec(nbondvec)   = nbondedtype(1,icm,i)
        nbtype2vec(nbondvec)  = nbondedtype(2,icm,i)
        lbondedvec(nbondvec)  = .true.
        l2bondsvec(nbondvec)  = .false.
        l3bondsvec(nbondvec)  = .false.
      endif
    endif
    do jcm = 1,nbonds(j)
      jmm = nbonded(jcm,j)
      if (imm.eq.jmm) then
        indsum = nbondind(icm,i) - nbondind(jcm,j) + 555
        lprevious = .false.
        loverwrite = .false.
        nprevious = 0
        do n = 1,nbondvec
          if (nbondvecind(n).eq.indsum) then
            if (lbondedvec(n).or.l2bondsvec(n)) then
              lprevious = .true.
            else
              loverwrite = .true.
              nprevious = n
            endif
          endif
        enddo
        if ((i.ne.j.or.indsum.ne.555).and..not.lprevious) then
!****************************
!  2 bonds between i and j  *
!****************************
          if (loverwrite) then
            nbondvecind(nprevious) = indsum
            nbtypevec(nprevious)   = 0
            nbtype2vec(nprevious)  = 0
            lbondedvec(nprevious)  = .false.
            l2bondsvec(nprevious)  = .true.
            l3bondsvec(nprevious)  = .false.
          else
            nbondvec = nbondvec + 1
            if (nbondvec.ge.maxbondvec) then
              maxbondvec = nbondvec + 3
              call changemaxbondvec
            endif
            nbondvecind(nbondvec) = indsum
            nbtypevec(nbondvec)   = 0
            nbtype2vec(nbondvec)  = 0
            lbondedvec(nbondvec)  = .false.
            l2bondsvec(nbondvec)  = .true.
            l3bondsvec(nbondvec)  = .false.
          endif
        endif
      endif
      do kcm = 1,nbonds(jmm)
        kmm = nbonded(kcm,jmm)
        if (imm.eq.kmm) then
          indsum = nbondind(icm,i) - nbondind(kcm,jmm) - nbondind(jcm,j) + 2*555
          lprevious = .false.
          do n = 1,nbondvec
            if (nbondvecind(n).eq.indsum) lprevious = .true.
          enddo
          if ((i.ne.jmm.or.indsum.ne.555).and..not.lprevious) then
!****************************
!  3 bonds between i and j  *
!****************************
            nbondvec = nbondvec + 1 
            if (nbondvec.ge.maxbondvec) then
              maxbondvec = nbondvec + 3
              call changemaxbondvec
            endif
            nbondvecind(nbondvec) = indsum
            nbtypevec(nbondvec)   = 0
            nbtype2vec(nbondvec)  = 0
            lbondedvec(nbondvec)  = .false.
            l2bondsvec(nbondvec)  = .false.
            l3bondsvec(nbondvec)  = .true.
          endif
        endif
      enddo
    enddo
  enddo
!
  return
  end
