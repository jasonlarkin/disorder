  subroutine gasort(ibest,nbest,fconf,ngacfg,dif,id)
!
!     Subroutine for obtaining nbest possible crystal structure
!     from fconf. Array label placed in ibest. 
!     Scott Woodley, R.I.G.B. , June/Sept 97 Sept 98 Aug 00
!
!     id=-1 then find the worse candigates
!     id=+1 then find the best  candigates
!
  use datatypes
  use iochannels
  use parallel
  implicit none
!
  real(dp)    :: fconf(*)
  integer(i4) :: ibest(*)
  integer(i4) :: id
  integer(i4) :: nbest
  integer(i4) :: ngacfg
  real(dp)    :: dif
!
  integer(i4) :: i
  integer(i4) :: itest
  integer(i4) :: j
  integer(i4) :: k
  integer(i4) :: next
  real(dp)    :: fbest
  real(dp)    :: fworst
!
  if (id.gt.0) then
    fbest = 1.0d20
    next = 1
    do i = 1,ngacfg
      if (fconf(i).lt.fbest) then
        fbest = fconf(i)
        next = i
      endif
    enddo
    ibest(1) = next
1   do j = 1,nbest-1
      fbest = 1.0d20
      next = 0
      do i = 1,ngacfg
        if (fconf(i).lt.fbest) then
          itest = 0
          if (dif.gt.0.0_dp) then
            do k = 1,j
              if (dabs(fconf(i)-ibest(k)).lt.dif) itest = 1
            enddo
          else
            do k = 1,j
              if (i.eq.ibest(k)) itest = 1
            enddo
          endif
          if (itest.eq.0)then
            fbest = fconf(i)
            next = i
          endif
        endif
      enddo
      if (next.eq.0) then
        write(ioout,*) 'required difference between candidates too large'
        if (dabs(dif).eq.0.0_dp) call stopnow('gasort')
        dif = 0.5_dp*dif
        write(ioout,*) 'Reducing unique parameter to',dif
        goto 1
      endif
      ibest(j+1) = next
    enddo
  else
    fworst = - 1.0d20
    next = 1
    do i = 1,ngacfg
      if (fconf(i).gt.fworst) then
        fworst = fconf(i)
        next = i
      endif
    enddo
    ibest(1) = next
    do j = 1,nbest-1
      fworst = -1.0d20
      next = 0
2     do i = 1,ngacfg
        if (fconf(i).gt.fworst) then
          itest = 0
          if (dif.gt.0.0_dp) then 
            do k = 1,j
              if (dabs(fconf(i)-ibest(k)).lt.dif) itest = 1
            enddo
          else
            do k = 1,j
              if (i.eq.ibest(k)) itest = 1
            enddo
          endif
          if (itest.eq.0)then
            fworst = fconf(i)
            next = i
          endif
        endif
      enddo
      if (next.eq.0) then
        write(ioout,*)'required difference between candidates too large'
        if (dabs(dif).eq.0.0_dp) call stopnow('gasort')
        dif = 0.5_dp*dif
        write(ioout,*)'Reducing unique parameter to',dif
        goto 2
      endif
      ibest(j+1) = next
    enddo
  endif

  return
  end
