  subroutine jcg(n,ndim,maxnz,jcoef,coef,rhs,u,iwksp,nw,wksp, &
                 iparm,rparm,ier)
!
!     itpackv 2d main routine  jcg  (jacobi conjugate gradient)
!     each of the main routines --
!           jcg, jsi, sor, ssorcg, ssorsi, rscg, rssi
!     can be used independently of the others
!
! ... function --
!
!          jcg drives the jacobi conjugate gradient algorithm.
!
! ... parameter list --
!
!          n      input integer.  dimension of the matrix.
!          ndim   row dimension of jcoef and coef arrays in calling
!                   routine
!          maxnz  maximum number of nonzeros per row
!          jcoef  integer array for sparse matrix representation.
!          coef   array for sparse matrix representation.
!                 jcoef and coef use the ellpack data structure.
!          rhs    input vector.  contains the right hand side
!                 of the matrix problem.
!          u      input/output vector.  on input, u contains the
!                 initial guess to the solution. on output, it contains
!                 the latest estimate to the solution.
!          iwksp  integer vector workspace of length 3*n
!          nw     input integer.  length of available wksp.  on output,
!                 iparm(8) is amount used.
!          wksp   vector used for working space.  jacobi conjugate
!                 gradient needs this to be in length at least
!                 4*n + 4*itmax.  here itmax = iparm(1) is the
!                 maximum allowable number of iterations.
!          iparm  integer vector of length 12.  allows user to specify
!                 some integer parameters which affect the method.
!          rparm  vector of length 12. allows user to specify some
!                 parameters which affect the method.
!          ier    output integer.  error flag.
!
! ... jcg module references --
!
!         from itpackv    chgcon, determ, dfault, echall,
!                         eigvns, eigvss, eqrt1s, iterm ,
!                         itjcg , parcon, permat, perror,
!                         pervec, pjac  , pmult , prbndx, pstop ,
!                         sbelm , scal  , sum3  , unscal,
!                         vout  , zbrent
!          system         abs, log10, amax0, max, mod, sqrt
!
! ... local itpackv references --
!
!          echall, itjcg , permat,
!          perror, pervec, pjac  , prbndx, sbelm , scal  ,
!          unscal
!
!     version -  itpackv 2d (january 1990) 
!
!     code written by - david kincaid, roger grimes, john respess
!                       center for numerical analysis
!                       university of texas
!                       austin, tx  78712
!                       (512) 471-1242
!
!     for additional details on the
!          (a) routine    see toms article 1982
!          (b) algorithm  see cna report 150
!
!     based on theory by - david young, david kincaid, lou hageman
!
!     reference the book - applied iterative methods
!                          l. hageman, d. young
!                          academic press, 1981
!
!     **************************************************
!     *               important note                   *
!     *                                                *
!     *      when installing itpackv routines on a     *
!     *  different computer, reset some of the values  *
!     *  in  subroutne dfault.   most important are    *
!     *                                                *
!     *   srelpr      machine relative precision       *
!     *   rparm(1)    stopping criterion               *
!     *                                                *
!     *   also change system-dependent routine         *
!     *   second used in timer                         *
!     *                                                *
!     **************************************************
!
! ... specifications for arguments
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
  integer(i4) :: n, ndim, maxnz, ier, nw
  integer(i4) :: jcoef(ndim,1),iwksp(1),iparm(12)
  real(dp)    :: coef(ndim,1),rhs(n),u(n),wksp(nw),rparm(12) 
!
  integer(i4) :: i, nb, nbo, n3, loop, ib1, ib2, ib3, ib4, ib5, idgts
  real(dp)    :: digit1, digit2
  real(dp)    :: temp, time2
  real(dp)    :: timer, time1, timi1, timi2, timj1, timj2, tol
!
  external perror
!
! ... variables in common block - itcom1
!
!     in     - iteration number
!     is     - iteration number when parameters last changed
!     isym   - symmetric/nonsymmetric case switch 
!     itmax  - maximum number of iterations allowed
!     level  - level of output control switch
!     nout   - output unit number
!
! ... variables in common block - itcom2
!
!     adapt  - fully adaptive procedure switch
!     betadt - switch for adaptive determination of beta
!     caseii - adaptive procedure case switch
!     halt   - stopping test switch
!     partad - partially adaptive procedure switch
!
! ... variables in common block - itcom3
!
!     bdelnm - two norm of b times delta-super-n
!     betab  - estimate for the spectral radius of lu matrix
!     cme    - estimate of largest eigenvalue
!     delnnm - inner product of pseudo-residual at iteration n
!     delsnm - inner product of pseudo-residual at iteration s
!     ff     - adaptive procedure damping factor
!     gamma  - acceleration parameter
!     omega  - overrelaxation parameter for sor and ssor
!     qa     - pseudo-residual ratio
!     qt     - virtual spectral radius
!     rho    - acceleration parameter
!     rrr    - adaptive parameter
!     sige   - parameter sigma-sub-e
!     sme    - estimate of smallest eigenvalue
!     specr  - spectral radius estimate for ssor
!     srelpr - machine relative precision
!     stptst - stopping parameter
!     udnm   - two norm of u
!     zeta   - stopping criterion
!
! ... initialize common blocks
!
  ier = 0
  level = iparm(2)
  nout = iparm(4)
  if (level.ge.1) write (nout,10)
10 format (///1x,'i t p a c k      j c g      ')
  if (iparm(1).le.0) go to 370
  if (iparm(11).eq.0) timj1 = timer(0.0_dp)
  call echall (n,ndim,maxnz,jcoef,coef,rhs,iparm,rparm,1_i4)
  temp = 500.0*srelpr
  if (zeta.ge.temp) go to 30
  if (level.ge.1) write (nout,20) zeta,srelpr,temp
20 format (/1x,'*** w a r n i n g ************'//1x, &
        '    in itpackv routine jcg'/1x, &
        '    rparm(1) =',e10.3,' (zeta)'/1x, &
        '    a value this small may hinder convergence '/1x, &
        '    since machine precision srelpr =',e10.3/1x, &
        '    zeta reset to ',e10.3) 
  zeta = temp
30 continue
  time1 = rparm(9)
  time2 = rparm(10)
  digit1 = rparm(11)
  digit2 = rparm(12)
!
! ... verify n
!
  if (n.gt.0) go to 50
  ier = 11
  if (level.ge.0) write (nout,40) n 
40 format (/1x,'*** f a t a l     e r r o r ************'//1x, &
          '    called from itpackv routine jcg '/1x, &
          '    invalid matrix dimension, n =',i8)
  go to 370
!
! ... scale linear system, u, and rhs by the square root of the
! ... diagonal elements.
!
50 continue
  call scal(n,ndim,maxnz,jcoef,coef,rhs,u,wksp,ier)
  if (ier.eq.0) go to 70
  if (level.ge.0) write (nout,60) ier
60 format (/1x,'*** f a t a l     e r r o r ************'//1x, &
     '    called from itpackv routine jcg '/1x, &
     '    error detected in routine  scal  '/1x, &
     '    which scales the system   '/1x, &
     '    ier = ',i5)
  go to 370
!
! ... remove rows and columns if requested
!
70 continue
  if (iparm(10).eq.0) go to 80
  tol = rparm(8)
  call sbelm(n,ndim,maxnz,jcoef,coef,rhs,wksp,tol)
!
! ... initialize wksp base addresses.
!
80 ib1 = 1
  ib2 = ib1+n
  ib3 = ib2+n
  ib4 = ib3+n
  ib5 = ib4+n
!
! ... permute to  red-black system if requested
!
  nb = iparm(9) 
  if (nb.ge.0) go to 110
  if (nb.le.-2) go to 170 
  n3 = n*3
  do 90 i = 1,n3
     iwksp(i) = 0
90 continue
  call prbndx(n,ndim,maxnz,jcoef,iwksp,iwksp(ib2),nb,level,nout,ier)
  if (ier.eq.0) go to 110 
  if (level.ge.0) write (nout,100) ier,nb
100 format (/1x,'*** f a t a l     e r r o r ************'//1x, &
     '    called from itpackv routine jcg  '/1x, &
     '    error detected in routine  prbndx'/1x, &
     '    which computes the red-black indexing'/1x, &
     '    ier = ',i5,' iparm(9) = ',i5,' (nb)')
  go to 350
110 if (nb.ge.0.and.nb.le.n) go to 130
  ier = 14
  if (level.ge.0) write (nout,120) ier,nb
120 format (/1x,'*** f a t a l     e r r o r ************'//1x, &
     '    called from itpackv routine jcg      '/1x, &
     '    error detected in red-black subsystem index'/1x, &
     '    ier = ',i5,' iparm(9) =',i5,' (nb)')
  go to 350
130 if (nb.ne.0.and.nb.ne.n) go to 150
  nbo = nb
  nb = n/2
  if (level.ge.2) write (nout,140) nbo,nb
140 format (/10x,' nb = ',i5,' implies matrix is diagonal'/10x,' nb reset to ',i5)
!
! ... permute matrix and rhs
!
150 if (level.ge.2) write (nout,160) nb
160 format (/10x,'order of black subsystem = ',i5,' (nb)')
  if (iparm(9).ge.0) go to 170
  call permat(n,ndim,maxnz,jcoef,coef,iwksp,wksp,iwksp(ib3))
  call pervec(n,iwksp,rhs,wksp)
  call pervec(n,iwksp,u,wksp)
!
! ... check for sufficient workspace.
!
170 iparm(8) = 4*n+4*itmax
  if (nw.ge.iparm(8)) go to 190
  ier = 12
  if (level.ge.0) write (nout,180) nw,iparm(8)
180 format (/1x,'*** f a t a l     e r r o r ************'//1x, &
     '    called from itpackv routine jcg '/1x, &
     '    not enough workspace at ',i10/1x, &
     '    set iparm(8) =',i10,' (nw)')
  go to 330
!
190 continue
  if (level.le.2) go to 220
  write (nout,200)
200 format (///1x,'in the following, rho and gamma are acceleration parameters')
  if (adapt) write (nout,210)
210 format (1x,'cme is the estimate of the largest eigenvalue of the jacobi matrix')
220 if (iparm(11).eq.0) timi1 = timer(0.0_dp)
!
! ... compute initial pseudo-residual
!
  do 230 i = 1,nw
     wksp(i) = 0.0
230 continue
  call dcopy(n,rhs,1_i4,wksp(ib2),1_i4)
  call pjac(n,ndim,maxnz,jcoef,coef,u,wksp(ib2))
  do 240 i = 1,n
     wksp(n+i) = wksp(n+i)-u(i)
240 continue
!
! ... iteration sequence
!
  itmax = itmax+1
  do 260 loop = 1,itmax
     in = loop-1
     if (mod(in,2_i4).eq.1) go to 250
!
! ... code for the even iterations.
!
!     u           = u(in)             wksp(ib2) = del(in)
!     wksp(ib1)   = u(in-1)           wksp(ib3) = del(in-1) 
!
     call itjcg(n,ndim,maxnz,jcoef,coef,u,wksp(ib1),wksp(ib2),wksp(ib3),wksp(ib4),wksp(ib5))
!
     if (halt) go to 290
     go to 260
!
! ... code for the odd iterations.
!
!     u           = u(in-1)           wksp(ib2) = del(in-1) 
!     wksp(ib1)   = u(in)             wksp(ib3) = del(in)
!
250    call itjcg(n,ndim,maxnz,jcoef,coef,wksp(ib1),u,wksp(ib3),wksp(ib2),wksp(ib4),wksp(ib5))
!
     if (halt) go to 290
260 continue
!
! ... itmax has been reached
!
  if (iparm(11).ne.0) go to 270
  timi2 = timer(0.0_dp)
  time1 = timi2-timi1
270 ier = 13
  if (level.ge.1) write (nout,280) itmax
280 format (/1x,'*** w a r n i n g ************'//1x, &
     '    in itpackv routine jcg'/1x, &
     '    failure to converge in',i5,' iterations')
  if (iparm(3).eq.0) rparm(1) = stptst
  go to 320
!
! ... method has converged
!
290 if (iparm(11).ne.0) go to 300
  timi2 = timer(0.0_dp)
  time1 = timi2-timi1
300 if (level.ge.1) write (nout,310) in
310 format (/1x,'jcg  has converged in ',i5,' iterations')
!
! ... put solution into u if not already there.
!
320 continue
  if (mod(in,2_i4).eq.1) call dcopy(n,wksp,1_i4,u,1_i4)
!
! ... un-permute matrix,rhs, and solution
!
330 if (iparm(9).ne.-1) go to 340
  call permat(n,ndim,maxnz,jcoef,coef,iwksp(ib2),wksp(ib4),iwksp(ib3))
  call pervec(n,iwksp(ib2),rhs,wksp(ib4))
  call pervec(n,iwksp(ib2),u,wksp(ib4))
  if (ier.eq.12) go to 350
!
! ... optional error analysis 
!
340 idgts = iparm(12)
  if (idgts.lt.0) go to 350
  if (iparm(2).le.0) idgts = 0
  call perror(n,ndim,maxnz,jcoef,coef,rhs,u,wksp,digit1,digit2,idgts)
!
! ... unscale the matrix, solution, and rhs vectors.
!
350 continue
  call unscal(n,ndim,maxnz,jcoef,coef,rhs,u,wksp)
!
! ... set return parameters in iparm and rparm
!
  iparm(8) = iparm(8)-4*(itmax-in)
  if (iparm(11).ne.0) go to 360
  timj2 = timer(0.0_dp)
  time2 = timj2-timj1
360 if (iparm(3).ne.0) go to 370
  iparm(1) = in 
  iparm(9) = nb 
  rparm(2) = cme
  rparm(3) = sme
  rparm(9) = time1
  rparm(10) = time2
  rparm(11) = digit1
  rparm(12) = digit2
!
370 continue
  if (level.ge.3) call echall(n,ndim,maxnz,jcoef,coef,rhs,iparm,rparm,2_i4)
  if (ier.eq.0.and.level.ge.1) write (nout,380)
380 format (/1x,'execution successful')
!
  return
  end 
!
  subroutine itjcg(n,ndim,maxnz,jcoef,coef,u,u1,d,d1,dtwd,tri)
!
! ... itjcg performs one iteration of the jacobi conjugate gradient
!     algorithm.  it is called by jcg.
!
! ... parameter list --
!
!          n      input integer.  dimension of the matrix.
!          ndim   row dimension of jcoef and coef arrays in calling
!                   routine
!          maxnz  maximum number of nonzeros per row
!          jcoef  integer sparse matrix representation
!          coef   sparse matrix representation
!          u      input vector.  contains the value of the
!                 solution vector at the end of in iterations.
!          u1     input/output vector.  on input, it contains
!                 the value of the solution at the end of the in-1
!                 iteration.  on output, it will contain the newest
!                 estimate for the solution vector.
!          d      input vector.  contains the pseudo-residual
!                 vector after in iterations.
!          d1     input/output vector.  on input, d1 contains
!                 the pseudo-residual vector after in-1 iterations.  on
!                 output, it will contain the newest pseudo-residual
!                 vector.
!          dtwd   array.  used in the computations of the
!                 acceleration parameter gamma and the new pseudo-
!                 residual.
!          tri    array.  stores the tridiagonal matrix associated
!                 with the eigenvalues of the conjugate gradient
!                 polynomial. 
!
! ... local itpackv references --
!
!          chgcon, iterm , parcon, pjac  , pstop ,
!          sum3
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz
  integer(i4) :: jcoef(ndim,1)
  real(dp)    :: u(n),u1(n),d(n),d1(n),dtwd(n),tri(2,1),coef(ndim,1)
!
! ... specifications for local variables
!
  logical     :: q1
  integer(i4) :: i
  real(dp)    :: con, c1, c2, c3, c4, rhoold, rhotmp, gamold
  real(dp)    :: ddot, dnrm, dtnrm
!
!     description of variables in common blocks in routine jcg
!
! ... compute new estimate for cme if adapt = .true.
!
  save
  if (adapt) call chgcon(itmax,tri,gamold,rhoold,1_i4)
!
! ... test for stopping
!
  delnnm = ddot(n,d,1_i4,d,1_i4)
  dnrm = delnnm 
  con = cme
  call pstop(n,u,dnrm,con,1_i4,q1)
  if (halt) go to 50
!
! ... compute rho and gamma - acceleration parameters
!
  do i = 1,n 
     dtwd(i) = 0.0
  enddo
  call pjac(n,ndim,maxnz,jcoef,coef,d,dtwd)
  dtnrm = ddot(n,d,1_i4,dtwd,1_i4)
  if (isym.eq.0) go to 20 
  rhotmp = ddot(n,dtwd,1_i4,d1,1_i4)
  call parcon(dtnrm,c1,c2,c3,c4,gamold,rhotmp,1_i4)
  rhoold = rhotmp
  go to 30
20 call parcon(dtnrm,c1,c2,c3,c4,gamold,rhoold,1_i4)
!
! ... compute u(in+1) and d(in+1)
!
30 do i = 1,n 
     u1(i) = c1*d(i)+c2*u(i)+c3*u1(i)
     d1(i) = c1*dtwd(i)+c4*d(i)+c3*d1(i)
   enddo
!
! ... output intermediate information
!
50 call iterm(n,coef,u,dtwd,1_i4)
!
  return
  end 
!
  subroutine echall(n,ndim,maxnz,jcoef,coef,rhs,iparm,rparm,icall)
!
! ... echall initializes the itpackv common blocks from the
! ... information contained in iparm and rparm. echall also prints the
! ... values of all the parameters in iparm and rparm.
!
! ... parameter list --
!
!          iparm
!           and
!          rparm  arrays of parameters specifying options and
!                    tolerances
!          icall  indicator of which parameters are being printed
!                    icall = 1,  initial parameters
!                    icall = 2,  final parameters 
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz, icall
  integer(i4) :: jcoef(ndim,1),iparm(12)
  real(dp)    :: coef(ndim,1),rhs(n),rparm(12)
!
  integer(i4) :: i, j
!
!
!     description of variables in common blocks in main routine
!
  if (icall.ne.1) go to 120
!
! ... initialize itpackv common
!
  zeta = rparm(1)
  cme = rparm(2)
  sme = rparm(3)
  ff = rparm(4) 
  omega = rparm(5)
  specr = rparm(6)
  betab = rparm(7)
  itmax = iparm(1)
  level = iparm(2)
  isym = iparm(5)
!
  adapt = .false.
  partad = .false.
  betadt = .false.
  if (iparm(6).eq.1.or.iparm(6).eq.3) adapt = .true.
  if (iparm(6).eq.1) betadt = .true.
  if (iparm(6).eq.2) partad = .true.
!
  caseii = .false.
  if (iparm(7).eq.2) caseii = .true.
  if (caseii) sme = -cme
  if (.not.caseii.and.sme.eq.0.0) sme = -1.0
  spr = sme
!
! ... set rest of common variables to zero
!
  in = 0
  is = 0
  halt = .false.
  bdelnm = 0.0
  delnnm = 0.0
  delsnm = 0.0
  gamma = 0.0
  qa = 0.0
  qt = 0.0
  rho = 0.0
  rrr = 0.0
  sige = 0.0
  stptst = 0.0
  udnm = 0.0
!
  if (level.le.4) go to 100
!
!     this section of echall causes printing of the linear system and 
!     the iterative parameters
!
  write (nout,10)
10 format (///5x,'the linear system is as follows')
  write (nout,20)
20 format (/2x,'jcoef array')
  do 30 i = 1,n 
     write (nout,40) (jcoef(i,j),j=1,maxnz)
30 continue
40 format (1x,8(1x,i8))
  write (nout,50)
50 format (/2x,'coef array')
  do 60 i = 1,n 
     write (nout,70) (coef(i,j),j=1,maxnz)
60 continue
70 format (1x,5(2x,g14.6)) 
  write (nout,80)
80 format (/2x,'rhs array')
  write (nout,90) (rhs(i),i=1,n)
90 format (1x,5g16.6)
100 if (level.le.2) return
  write (nout,110)
110 format (///5x,'initial iterative parameters'/)
  go to 140
120 write (nout,130)
130 format (///5x,'final iterative parameters'/)
140 write (nout,150) iparm(1),level,iparm(3),nout,isym,iparm(6)
150 format (10x,'iparm(1)  =',i15,4x,'(itmax)'/  &
          10x,'iparm(2)  =',i15,4x,'(level)'/  &
          10x,'iparm(3)  =',i15,4x,'(ireset)'/ &
          10x,'iparm(4)  =',i15,4x,'(nout)'/ &
          10x,'iparm(5)  =',i15,4x,'(isym)'/ &
          10x,'iparm(6)  =',i15,4x,'(iadapt)')
  write (nout,160) iparm(7),iparm(8),iparm(9),iparm(10),iparm(11),iparm(12)
160 format (10x,'iparm(7)  =',i15,4x,'(icase)'/  &
          10x,'iparm(8)  =',i15,4x,'(nwksp)'/  &
          10x,'iparm(9)  =',i15,4x,'(nb)'/ &
          10x,'iparm(10) =',i15,4x,'(iremove)'/ &
          10x,'iparm(11) =',i15,4x,'(itime)'/  &
          10x,'iparm(12) =',i15,4x,'(idgts)') 
  write (nout,170) zeta,cme,sme,ff,omega,specr
170 format (10x,'rparm(1)  =',e15.8,4x,'(zeta)'/ &
          10x,'rparm(2)  =',e15.8,4x,'(cme)'/  &
          10x,'rparm(3)  =',e15.8,4x,'(sme)'/  &
          10x,'rparm(4)  =',e15.8,4x,'(ff)'/ &
          10x,'rparm(5)  =',e15.8,4x,'(omega)'/ &
          10x,'rparm(6)  =',e15.8,4x,'(specr)')
  write (nout,180) betab,rparm(8),rparm(9),rparm(10),rparm(11),rparm(12)
180 format (10x,'rparm(7)  =',e15.8,4x,'(betab)'/ &
          10x,'rparm(8)  =',e15.8,4x,'(tol)'/  &
          10x,'rparm(9)  =',e15.8,4x,'(time1)'/ &
          10x,'rparm(10) =',e15.8,4x,'(time2)'/ &
          10x,'rparm(11) =',e15.8,4x,'(digit1)'/ &
          10x,'rparm(12) =',e15.8,4x,'(digit2)')
!
  return
  end 
  subroutine iterm(n,coef,u,wk,imthd)
!
!     iterm produces the iteration summary line at the end
!     of each iteration. if level .ge. 4, the latest approximation
!     to the solution will be printed.
!
! ... parameter list --
!
!          n      order of system or, for reduced system
!                    routines, order of black subsystem
!          coef   iteration matrix
!          u      solution estimate
!          wk     work array of length n
!          imthd  indicator of method
!                    imthd = 1,  jcg
!                    imthd = 2,  jsi
!                    imthd = 3,  sor
!                    imthd = 4,  ssorcg 
!                    imthd = 5,  ssorsi 
!                    imthd = 6,  rscg
!                    imthd = 7,  rssi
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, imthd
  real(dp)    :: coef(n,1),u(n),wk(n)
!
  integer(i4) :: i, ip
  real(dp)    :: qtff
!
! ... print various parameters after each iteration
!
  if (level.lt.2) return
  go to (10,100,140,170,50,10,100), imthd
10 if (in.gt.0) go to 30
!
! ... print header for jcg and rscg
!
  write (nout,20)
20 format (////5x,'intermediate output after each iteration'// &
     ' number of',3x,'convergence',5x,'cme ',10x,'rho',8x,'gamma'/ &
     ' iterations',5x,'test '//)
!
! ... print summary line
!
30 write (nout,40) in,stptst,cme,rho,gamma
40 format (3x,i5,3x,5e13.5)
  if (level.ge.4) go to 200
!
  return
!
50 if (in.gt.0) go to 70
!
! ... print header for ssor-si
!
  write (nout,60)
60 format (////5x,'intermediate output after each iteration'// &
     ' number of',3x,'convergence',3x,'parameter change test',8x,  &
     'rho',8x,'gamma'/' iterations',5x,'test ',6x,'lhs(qa)',4x, &
     'rhs(qt**ff)'//)
!
! ... print summary line
!
70 ip = in-is
  if (imthd.eq.7) ip = 2*ip
  if (ip.lt.3) go to 80
  qtff = qt**ff 
  write (nout,40) in,stptst,qa,qtff,rho,gamma 
  if (level.ge.4) go to 200
  return
!
80 write (nout,90) in,stptst,rho,gamma
90 format (3x,i5,3x,e13.5,26x,2e13.5)
  if (level.ge.4) go to 200
  return
!
100 if (in.gt.0) go to 120
!
! ... print header for j-si and rs-si
!
  write (nout,110)
110 format (////5x,'intermediate output after each iteration'// &
     ' number of',3x,'convergence',3x,'parameter change test',8x,  &
     'rho'/' iterations',5x,'test ',6x,'lhs(qa)',4x,'rhs(qt**ff)'//)
!
! ... print summary line
!
120 ip = in-is
  if (imthd.eq.7) ip = 2*ip
  if (ip.lt.3) go to 130
  qtff = qt**ff 
  write (nout,40) in,stptst,qa,qtff,rho
  if (level.ge.4) go to 200
  return
!
130 write (nout,90) in,stptst,rho
  if (level.ge.4) go to 200
  return
!
! ... print various parameters after each iteration for sor.
!
140 if (in.gt.0) go to 160
!
! ... print header for sor
!
  write (nout,150)
150 format (////5x,'intermediate output after each iteration'// &
     ' number of',3x,'convergence',5x,'cme ',8x,'omega',7x, &
     'spectral'/' iterations',5x,'test',34x,'radius'//) 
!
! ... print summary line for sor
!
160 continue
  write (nout,40) in,stptst,cme,omega,specr
  if (level.ge.4) go to 200
!
  return
!
! ... print various parameters after each iteration for ssor-cg.
!
170 if (in.gt.0) go to 190
!
! ... print header for ssor-cg
!
  write (nout,180)
180 format (////5x,'intermediate output after each iteration'// &
     ' number of',3x,'convergence',2x,' spectral',5x,'s-prime',9x, &
     'rho',8x,'gamma'/' iterations',5x,'test ',7x,'radius'//)
!
! ... print summary line for ssor-cg
!
190 continue
  write (nout,40) in,stptst,specr,spr,rho,gamma
  if (level.ge.4) go to 200
  return
!
200 if (imthd.gt.5) go to 220
  write (nout,210) in
210 format (/1x,2x,'estimate of solution at iteration ',i5)
  go to 240
220 write (nout,230) in
230 format (/1x,2x,'estimate of solution at black points at iteration ',i5)
240 do 250 i = 1,n
     wk(i) = u(i)*coef(i,1)
250 continue
  write (nout,260) (wk(i),i=1,n)
260 format (1x,5g16.7)
  write (nout,270)
270 format (//)
!
  return
  end 
  subroutine permat(n,ndim,maxnz,jcoef,coef,p,work,iwork)
!
! ... permat takes the sparse matrix representation
!     of the matrix stored in the arrays jcoef and coef and 
!     permutes both rows and columns, overwriting the previous
!     structure.
!
! ... parameter list --
!
!          n         order of system
!          ndim      row dimension of arrays jcoef and coef in
!                       the calling routine
!          maxnz     maximum number of nonzero entries per row
!          jcoef     integer array for data
!          coef      array for data structure coefficients
!          p         permutation vector 
!          work      workspace of length n
!          iwork     integer workspace of length n
!
! ... it is assumed that the i-th entry of the permutation vector
!     p indicates the row the i-th row gets mapped into.  (i.e.
!     if ( p(i) = j ) row i gets mapped into row j)
!
!     *** note ***  this routine is to be called after routine scal.
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz
  integer(i4) :: jcoef(ndim,1),p(1),iwork(1)
  real(dp)    :: coef(ndim,1),work(1)
!
  integer(i4) :: i, j
!
!
  if (n.le.0) return
  do 50 j = 1,maxnz
     call dcopy(n,coef(1,j),1_i4,work,1_i4)
     do 10 i = 1,n
        iwork(i) = jcoef(i,j)
10    continue
     do 20 i = 1,n
        coef(p(i),j) = work(i)
20    continue
     do 30 i = 1,n
        jcoef(p(i),j) = iwork(i)
30    continue
     do 40 i = 1,n
        jcoef(i,j) = p(jcoef(i,j))
40    continue
50 continue
  return
  end 
  subroutine perror(n,ndim,maxnz,jcoef,coef,rhs,u,work,digit1,digit2,idgts)
!
!     perror computes the residual, r = rhs - a*u.  the user
!     also has the option of printing the residual and/or the
!     unknown vector depending on idgts.
!
! ... parameter list --
!
!          n      dimension of matrix
!          ndim   row dimension of jcoef and coef in calling routine
!          maxnz  maximum number of nonzeros per row
!          jcoef  integer array of sparse matrix representation
!          coef   array of sparse matrix representation
!          rhs    right hand side of matrix problem
!          u      latest estimate of solution
!          work   workspace vector of length 2*n
!          digit1 output - measure of accuracy of stopping test
!          digit2 output - measure of accuracy of solution
!          idgts   parameter controlling level of output
!                    if idgts < 1 or idgts > 4, then no output.
!                            = 1, then number of digits is printed, pro-
!                                 vided level .ge. 1
!                            = 2, then solution vector is printed, pro-
!                                 vided level .ge. 1
!                            = 3, then residual vector is printed, pro-
!                                 vided level .ge. 1
!                            = 4, then both vectors are printed, pro- 
!                                 vided level .ge. 1
!
! ... local itpackv references --
!
!          pmult , vout
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz, idgts
  integer(i4) :: jcoef(ndim,1)
  real(dp)    :: digit1, digit2
  real(dp)    :: coef(ndim,1),rhs(1),u(1),work(1)
!
  integer(i4) :: i
  real(dp)    :: bnrm, rnrm, temp
  real(dp)    :: ddot
!
!
!     description of variables in common block in main routine
!
  digit1 = 0.0
  digit2 = 0.0
  if (n.le.0) go to 70
!
  digit1 = -log10(abs(srelpr))
  if (stptst.gt.0.0) digit1 = -log10(abs(stptst))
  do 10 i = 1,n 
     work(i) = rhs(i)/coef(i,1)
10 continue
  bnrm = ddot(n,work,1_i4,work,1_i4)
  if (bnrm.eq.0.0) go to 30
  call pmult(n,ndim,maxnz,jcoef,coef,u,work) 
  do 20 i = 1,n 
     work(i) = (rhs(i)-work(i))/coef(i,1)
20 continue
  rnrm = ddot(n,work,1_i4,work,1_i4)
  temp = rnrm/bnrm
  if (temp.eq.0.0) go to 30
  digit2 = -log10(abs(temp))/2.0
  go to 40
!
30 digit2 = -log10(abs(srelpr))
!
40 if ((idgts.lt.1).or.(level.le.0)) go to 70
  write (nout,50) digit1,digit2
50 format (/10x,'approx. no. of digits in stopping test =', &
                  f5.1,'  (digit1)'  &
          /10x,'approx. no. of digits in ratio test    =', &
                  f5.1,'  (digit2)')
!
  if (idgts.le.1.or.idgts.gt.4) go to 70
  if (idgts.ge.3) call vout(n,work,1_i4,nout)
  do 60 i = 1,n 
     work(i) = u(i)*coef(i,1)
60 continue
  if (idgts.ne.3) call vout(n,work,2_i4,nout)
!
70 continue
  return
  end 
  subroutine pjac(n,ndim,maxnz,jcoef,coef,u,rhs)
!
! ... pjac performs one jacobi iteration.
!
! ... parameter list --
!
!         n       dimension of matrix
!         ndim    row dimension of jcoef and coef arrays in calling
!                   routine
!         maxnz   maximum number of nonzeros per row
!         jcoef   integer data structure for coefficient columns
!         coef    data structure for array coefficients
!         u       estimate of solution of a matrix problem
!         rhs     on input -- contains the right hand side of the
!                             matrix problem
!                 on output -- contains b*u + rhs  where b = i - a
!                              and a has been scaled to have a unit
!                              diagonal 
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz
  integer(i4) :: jcoef(ndim,2)
  real(dp)    :: coef(ndim,2),u(1),rhs(1)
!
  integer(i4) :: maxm1
!
!
  maxm1 = maxnz - 1
  call ymasx2(ndim,n,maxm1,coef(1,2),jcoef(1,2),rhs,u) 
  return
  end 
  subroutine pmult(n,ndim,maxnz,jcoef,coef,b,c)
!
! ... pmult computes c = a*b, a matrix-vector product.  matrix
!     a is assumed to be stored in the coef, jcoef ellpack
!     data structure and all entries in the column array jcoef
!     are assumed to be between 1 and n, inclusive.
!     a is assumed to have a unit diagonal.
!
! ... parameter list --
!
!          n        dimension of matrix 
!          ndim     row dimension of coef and jcoef in calling routine
!          maxnz    maximum number of nonzeros per row
!          jcoef    integer array for coefficient columns
!          coef     array for coefficients
!          b        multiplying vector of length n
!          c        product vector of length n
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz
  integer(i4) :: jcoef(ndim,2)
  real(dp)    :: coef(ndim,2),b(1),c(1)
!
  integer(i4) :: maxm1
!
  call dcopy(n,b,1_i4,c,1_i4)
  maxm1 = maxnz-1
  call ypasx2(ndim,n,maxm1,coef(1,2),jcoef(1,2),c,b)
  return
  end 
  subroutine prbndx(n,ndim,maxnz,jcoef,p,ip,nblack,level,nout,ier)
!
!**************************************************************
!
!     prbndx computes the red-black permutation
!     vectors p ( and its inverse ip ) if possible.
!
!     the algorithm is to mark the first node as red (arbitrary).
!     all of its adjacent nodes are marked black and placed in
!     a stack.  the remainder of the code pulls the first node
!     off the top of the stack and tries to type its adjacent nodes.
!     the typing of the adjacent point is a five way case statement
!     which is well commented below (see do loop 100).
!
!     the array p is used both to keep track of the color of a node
!     (red node is positive, black is negative) but also the father
!     node that caused the color marking of that point.  since
!     complete information on the adjacency structure is hard to come 
!     by this forms a link to enable the color change of a partial
!     tree when a recoverable color conflict occurs.
!
!     the array ip is used as a stack to point to the set of nodes
!     left to be typed that are known to be adjacent to the current
!     father node.
!
!     *** note ***  this routine is to be called after routine scal.
!
!*********************************************************************
!
!     input parameters --
!
!        n      number of nodes.  (integer, scalar)
!
!        ndim   row dimension of jcoef in calling routine.
!
!        maxnz  maximum number of nonzeros per row
!
!        jcoef  array of column indices.  it is assumed
!               that for every row where only one element is
!               stored that element corresponds to the diagonal
!               entry.  the diagonal must be the first entry stored.
!                 (integer, arrays)
!
!        level  switch for printing
!
!        nout   output tape number
!
!     output parameters --
!
!        nblack number of black nodes.  number of red nodes is
!               n - nblack.  (integer, scalar)
!
!        p, ip  permutation and inverse permutation vectors.
!               (integer, arrays each of length n)
!
!        ier    error flag. (integer, scalar)
!
!               ier = 0, normal return.  indexing performed 
!                        successfully
!               ier = 201, red-black indexing not possible. 
!
!******************************************************************** 
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: ier, n, ndim, maxnz, level, nout, nblack
  integer(i4) :: jcoef(ndim,1),p(1),ip(1)
!
! ... specifications for local variables
!
  integer(i4) :: first,old,young,curtyp,type
  integer(i4) :: i, ibgn, j, jcol, k, l, last, nxttyp, nred, next
!
!-----------------------------------------------------------------------
!
  ier = 0
  if (n.le.0) return
  do 10 i = 1,n 
     p(i) = 0
     ip(i) = 0
10 continue
!
! ... handle the first set of points until some adjacent points
! ... are found
!
  first = 1
!
20 p(first) = first
  if (maxnz.gt.1) go to 40
!
! ... search for next entry that has not been marked
!
  if (first.eq.n) go to 120
  ibgn = first+1
  do 30 i = ibgn,n
     if (p(i).ne.0) go to 30
     first = i
     go to 20
30 continue
  go to 120
!
! ... first set of adjacent points found
!
40 next = 1
  last = 1
  ip(1) = first 
!
! ... loop over labeled points indicated in the stack stored in
! ... the array ip
!
50 k = ip(next)
  curtyp = p(k) 
  nxttyp = -curtyp
  do 100 j = 2,maxnz
     jcol = jcoef(k,j)
     if (jcol.eq.k) go to 100
     type = p(jcol)
!
!==================================================================
!
!     the following is a five way case statement dealing with the
!     labeling of the adjacent node.
!
! ... case i.  if the adjacent node has already been labeled with
!              label equal to nxttyp, then skip to the next adjacent
!              node.
!
     if (type.eq.nxttyp) go to 100
!
! ... case ii.  if the adjacent node has not been labeled yet label
!               it with nxttyp and enter it in the stack
!
     if (type.ne.0) go to 60
     last = last+1
     ip(last) = jcol
     p(jcol) = nxttyp
     go to 100
!
! ... case iii.  if the adjacent node has already been labeled with
!                opposite color and the same father seed, then there
!                is an irrecoverable color conflict.
!
60    if (type.eq.curtyp) go to 140
!
! ... case iv.  if the adjacent node has the right color and a different
!               father node, then change all nodes of the youngest fathe
!               node to point to the oldest father seed and retain the
!               same colors.
!
     if (type*nxttyp.lt.1) go to 80 
     old = min0(abs(type),abs(nxttyp))
     young = max0(abs(type),abs(nxttyp))
     do 70 l = young,n
        if (abs(p(l)).eq.young) p(l) = sign(old,p(l)) 
70    continue
     curtyp = p(k)
     nxttyp = -curtyp
     go to 100
!
! ... case v.  if the adjacent node has the wrong color and a different
!              father node, then change all nodes of the youngest father
!              node to point to the oldest father node along with
!              changing their colors.  since until this time the
!              youngest father node tree has been independent no other
!              color conflicts will arise from this change. 
!
80    old = min0(abs(type),abs(nxttyp))
     young = max0(abs(type),abs(nxttyp))
     do 90 l = young,n
        if (abs(p(l)).eq.young) p(l) = sign(old,-p(l))
90    continue
     curtyp = p(k)
     nxttyp = -curtyp
!
! ... end of case statement
!
!==================================================================
!
100 continue
!
! ... advance to next node in the stack 
!
  next = next+1 
  if (next.le.last) go to 50
!
! ... all nodes in the stack have been removed
!
! ... check for nodes not labeled.  if any are found
! ... start the labeling process again at the first
! ... node found that is not labeled.
!
  ibgn = first+1
  do 110 i = ibgn,n
     if (p(i).ne.0) go to 110
     first = i
     go to 20
110 continue
!
!===================================================================
!
! ... all nodes are now typed either red or black 
!
! ... generate permutation vectors
!
120 call whenige(n,p,0_i4,ip,nred)
  call whenilt(n,p,0_i4,ip(nred+1),nblack)
  do 130 i = 1,n
     p(ip(i)) = i
130 continue
!
! ... successful red-black ordering completed
!
  return
!
! ...... type conflict
!
140 ier = 201
  if (level.ge.0) write (nout,150)
150 format (//1x,'*** f a t a l     e r r o r ************'//1x, &
     '    in itpackv routine prbndx  '/1x, &
     '    red-black indexing not possible')
  return
  end 
  subroutine prsblk(nb,nr,ndim,maxnz,jcoef,coef,u,v)
!
! ... prsblk computes a black-rs sweep on a red vector into a black
! ... vector.
!
! ... parameter list --
!
!          nb       number of black points
!          nr       number of red points
!          ndim     row dimension of jcoef and coef arrays in calling 
!                    routine
!          maxnz    maximum number of nonzeros per row
!          jcoef    integer data structure for coefficient columns
!          coef     data structure for array coefficients
!          u        latest estimate of the solution
!          v        on input -- contains the right hand side
!                   on output -- v(nr+1,...n) contains (fr**t)*ur+cb
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: nb, nr, ndim, maxnz
  integer(i4) :: jcoef(ndim,2)
  real(dp)    :: coef(ndim,2),u(1),v(1)
!
  integer(i4) :: maxm1, nrp1
!
!
  if (nb.le.0.or.maxnz.le.1) return 
  nrp1 = nr+1
  maxm1 = maxnz-1
  call ymasx2(ndim,nb,maxm1,coef(nrp1,2),jcoef(nrp1,2),v(nrp1),u)
  return
  end 
  function pvtbv (n,ndim,maxnz,jcoef,coef,v)
!
! ... pvtbv computes  (v**t)*b*v  where  b = i - a.
!
! ... parameter list --
!
!        n      dimension of matrix
!        ndim   row dimension of jcoef and coef in calling routine
!        maxnz  maximum number of nonzeros per row
!        jcoef  integer data structure for coefficient columns
!        coef   data structure for equation coefficients
!        v      array of length n
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: maxnz, n, ndim
  integer(i4) :: jcoef(ndim,1)
  real(dp)    :: coef(ndim,1),v(1)
  real(dp)    :: pvtbv
!
  integer(i4) :: i, j
!
!
  pvtbv = 0.0
  if (maxnz.le.1) return
  do j = 2,maxnz
    do i = 1,n
      pvtbv = pvtbv-v(i)*coef(i,j)*v(jcoef(i,j))
    enddo
  enddo
  return
  end 
  subroutine sbelm(n,ndim,maxnz,jcoef,coef,rhs,work,tol)
!
! ... sbelm is designed to remove rows of the matrix for which
! ... all off-diagonal elements are very small (less than tol).
! ... this is to take care of matrices arising from finite
! ... element discretizations of partial differential equations
! ... with dirichlet boundary conditions.  any such rows and
! ... corresponding columns are then eliminated (set to the 
! ... identity after correcting the rhs).
! ... *** note ***  this routine is to be called after routine scal.
!
! ... parameter list --
!
!         n       dimension of matrix
!         ndim    row dimension of arrays jcoef and coef in the
!                    calling program
!         maxnz   maximum number of nonzero entries per row 
!         jcoef   integer array of matrix representation
!         coef    array of sparse matrix representation
!         rhs     right hand side of matrix problem
!         work    work array of length n
!         tol     tolerance factor
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: maxnz, n, ndim
  integer(i4) :: jcoef(ndim,1)
  real(dp)    :: tol
  real(dp)    :: coef(ndim,1),rhs(1),work(1)
!
  integer(i4) :: i, j, jcol
!
  if (n.le.0.or.maxnz.lt.2) return
!
! ... find maximum off-diagonal elements in absolute value. 
!
  do 10 i = 1,n 
     work(i) = 0.0
10 continue
  do 30 j = 2,maxnz
     do 20 i = 1,n
        work(i) = max(work(i),abs(coef(i,j)))
20    continue
30 continue
!
! ... eliminate desired rows and columns.
!
  do 60 j = 2,maxnz
     do 50 i = 1,n
        if (work(i).lt.tol) go to 40
        jcol = jcoef(i,j) 
        if (work(jcol).ge.tol) go to 50
        rhs(i) = rhs(i)-coef(i,j)*rhs(jcol)
40       coef(i,j) = 0.0
        jcoef(i,j) = i
50    continue
60 continue
  return
  end 
  subroutine scal (n,ndim,maxnz,jcoef,coef,rhs,u,work,ier)
!
! ... scal scales original matrix to a unit diagonal matrix.  rhs
! ... and u vectors are scaled accordingly.  the data
! ... structure is adjusted to have diagonal entries in
! ... column 1.  zero entries in jcoef array are changed to 
! ... positive integers between 1 and n.
!
! ... parameter list --
!
!         n       dimension of matrix
!         ndim    row dimension of arrays jcoef and coef in the
!                    calling program
!         maxnz   maximum number of nonzero entries per row 
!         jcoef   integer array of matrix representation
!         coef    array of sparse matrix representation
!         rhs     right hand side of matrix problem
!         u       latest estimate of solution
!         work    work array of length n
!         ier     error flag -- on return, nonzero values mean
!                    401 -- zero diagonal element 
!                    402 -- nonexistent diagonal element
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: maxnz, n, ndim, ier
  integer(i4) :: jcoef(ndim,1)
  real(dp)    :: coef(ndim,1),rhs(1),u(1),work(1)
!
  integer(i4) :: i, j, nsgncg
  real(dp)    :: save
!
!     description of variables in common block in main routine
!
! ... check for positive diagonal entries for each row.
! ... put diagonal entries in column 1.  replace zeros in
! ... row i of jcoef with i.
!
  ier = 0
  nsgncg = 0
  if (n.le.0) return
  do 110 i = 1,n
     if (jcoef(i,1).eq.i) go to 50
     if (maxnz.lt.2) go to 20
     do 10 j = 2,maxnz
        if (jcoef(i,j).eq.i) go to 40
10    continue
!
! ... fatal error -- no diagonal entry for row i. 
!
20    ier = 402
     if (level.ge.0) write (nout,30) i
30    format (//1x,'*** f a t a l     e r r o r ************'//1x,  &
        '    in itpackv routine scal    '/1x, &
        '    no diagonal entry in row',i10)
     return
!
! ... shift row i so that diagonal element is in column 1.
!
40    save = coef(i,j)
     coef(i,j) = coef(i,1)
     jcoef(i,j) = jcoef(i,1)
     coef(i,1) = save
     jcoef(i,1) = i
!
! ... check sign of diagonal entry.  if negative, change signs of
! ... all row coefficients and corresponding rhs element.
!
50    if (coef(i,1)) 60 , 90 , 110
60    do 70 j = 1,maxnz
        coef(i,j) = -coef(i,j)
70    continue
     rhs(i) = -rhs(i)
     nsgncg = nsgncg+1
     if (level.ge.5) write (nout,80) i
80    format (//1x,'*** n o t e ***'//1x, &
        '    in itpackv routine scal'/1x, &
        '    equation ',i10,' has been negated')
     go to 110
!
! ... fatal error -- zero diagonal element for row i.
!
90    ier = 401
     if (level.ge.0) write (nout,100) i
100    format (//1x,'*** f a t a l     e r r o r ************'//1x,  &
        '    in itpackv routine scal    '/1x, &
        '    diagonal entry in row ',i10,' is zero')
     return
110 continue
!
! ... change zero elements of jcoef array.
!
  if (maxnz.lt.2) go to 140
  do 130 j = 2,maxnz
     do 120 i = 1,n
        if (jcoef(i,j).le.0) jcoef(i,j) = i
120    continue
130 continue
!
! ... scale rhs and u arrays.  store reciprocal square roots
! ... of diagonal entries in column 1 of coef.
!
140 do 150 i = 1,n
     work(i) = sqrt(coef(i,1))
150 continue
  do 160 i = 1,n
     u(i) = u(i)*work(i)
160 continue
  do 170 i = 1,n
     work(i) = 1.0/work(i)
170 continue
  call dcopy(n,work,1_i4,coef,1_i4)
  do 180 i = 1,n
     rhs(i) = rhs(i)*work(i)
180 continue
!
! ... scale matrix. 
!
  if (maxnz.lt.2) return
  do 200 j = 2,maxnz
     do 190 i = 1,n
        coef(i,j) = coef(i,j)*work(i)*work(jcoef(i,j))
190    continue
200 continue
!
! ... adjust isym if the  0 .lt. nsgncg .lt. n
!
  if (nsgncg.gt.0.and.nsgncg.lt.n) isym = 1
!
  return
  end 
  subroutine unscal(n,ndim,maxnz,jcoef,coef,rhs,u,work)
!
! ... unscal reverses the scaling done in routine scal.
!
! ... parameter list --
!
!         n       dimension of matrix
!         ndim    row dimension of arrays jcoef and coef in the
!                    calling program
!         maxnz   maximum number of nonzero entries per row 
!         jcoef   integer array of matrix representation
!         coef    array of sparse matrix representation
!         rhs     right hand side of matrix problem
!         u       latest estimate of solution
!         work    work array of length n
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, ndim, maxnz
  integer(i4) :: jcoef(ndim,1)
  real(dp)    :: coef(ndim,1),rhs(1),u(1),work(1)
!
  integer(i4) :: i, j
!
! ... unscale u and rhs arrays.
!
  call dcopy(n,coef,1_i4,work,1_i4)
  do 10 i = 1,n 
     u(i) = u(i)*work(i)
10 continue
  do 20 i = 1,n 
     work(i) = 1.0/work(i)
20 continue
  do 30 i = 1,n 
     rhs(i) = rhs(i)*work(i)
30 continue
!
! ... unscale matrix.
!
  if (maxnz.lt.2) go to 80
  do 50 j = 2,maxnz
     do 40 i = 1,n
        coef(i,j) = coef(i,j)*work(i)*work(jcoef(i,j))
40    continue
50 continue
!
! ... put original zeros back in icoef array and restore unscaled
! ... diagonal entries to column one.
!
  do 70 j = 2,maxnz
     do 60 i = 1,n
        if (jcoef(i,j).eq.i) jcoef(i,j) = 0
60    continue
70 continue
80 do 90 i = 1,n 
     coef(i,1) = work(i)**2
90 continue
  return
  end 
  subroutine chgcon(ldt,tri,gamold,rhoold,ibmth)
!
! ... chgcon computes the new estimate for the largest eigenvalue
!     for conjugate gradient acceleration.
!
! ... parameter list --
!
!          ldt    leading dimension of tri
!          tri    tridiagonal matrix associated with the eigenvalues
!                    of the conjugate gradient polynomial
!          gamold
!            and
!          rhoold previous values of acceleration parameters
!          ibmth  indicator of basic method being accelerated by cg
!                      ibmth = 1,  jacobi
!                            = 2,  reduced system 
!                            = 3,  ssor 
!
! ... local itpackv references --
!
!          eigvns, eigvss
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: ldt, ibmth
  real(dp)    :: gamold, rhoold
  real(dp)    :: tri(ldt,4)
!
  integer(i4) :: ip, ier
  real(dp)    :: start, cmold, end
  real(dp)    :: eigvns, eigvss
!
!     description of variables in common blocks in main routine
!
  save
  go to (10,20,30), ibmth 
!
! ... jacobi conjugate gradient
!
10 start = cme
  ip = in
  go to 40
!
! ... reduced system cg
!
20 start = cme**2
  ip = in
  go to 40
!
! ... ssor cg
!
30 if (adapt) start = spr
  if (.not.adapt) start = specr
  ip = in-is
!
! ... define the matrix
!
40 if (ip.ge.2) go to 60
  if (ip.eq.1) go to 50
!
! ... ip = 0
!
  end = 0.0
  cmold = 0.0
  go to 110
!
! ... ip = 1
!
50 end = 1.0-1.0/gamma
  tri(1,1) = end
  tri(1,2) = 0.0
  go to 110
!
! ... ip > 1
!
60 if (abs(start-cmold).le.zeta*start) go to 120
  cmold = start 
!
! ... compute the largest eigenvalue
!
  tri(ip,1) = 1.0-1.0/gamma
  tri(ip,2) = (1.0-rho)/(rho*rhoold*gamma*gamold)
  if (isym.ne.0) go to 80 
  end = eigvss(ip,tri,start,zeta,itmax,ier)
  if (ier.eq.0) go to 100 
  if (level.ge.2) write (nout,70) ier
70 format (/10x,'difficulty in computation of maximum eigenvalue'/  &
           15x,'of iteration matrix'/ &
           10x,'routine zbrent returned ier =',i5)
  go to 100
80 continue
  end = eigvns(ldt,ip,tri,tri(1,3),tri(1,4),ier)
  if (ier.eq.0) go to 100 
  if (level.ge.2) write (nout,90) ier
90 format (/10x,'difficulty in computation of maximum eigenvalue'/  &
           15x,'of iteration matrix'/ &
           10x,'routine eqrt1s returned ier =',i5)
100 continue
  if (ier.ne.0) go to 130 
!
! ... set spectral radius for the various methods 
!
110 if (ibmth.eq.1) cme = end
  if (ibmth.eq.2) cme = sqrt(abs(end))
  if (ibmth.eq.3.and.adapt) spr = end
  if (ibmth.eq.3.and..not.adapt) specr = end
  return
!
! ... relative change in cme is less than zeta.  therefore stop
!     changing.
!
120 adapt = .false.
  partad = .false.
  return
!
! ... estimate for cme > one.  therefore need to stop adaptive
!     procedure and keep old value of cme.
!
130 adapt = .false.
  partad = .false.
  if (level.ge.2) write (nout,140) in,start
140 format (/10x,'estimate of maximum eigenvalue of jacobi   '/15x,  &
     'matrix (cme) not accurate'/10x, &
     'adaptive procedure turned off at iteration ',i5/10x, &
     'final estimate of maximum eigenvalue =',e15.7/)
!
  return
  end 
!
  function determ(ldt,n,tri,wk1,wk2,xlmda)
!
!     determ computes the determinant of a symmetric
!     tridiagonal matrix given by tri. det(tri - xlmda*i) = 0
!
! ... parameter list --
!
!          ldt    leading dimension of array tri
!          n      order of tridiagonal system
!          tri    symmetric tridiagonal matrix of order n
!          wk1,   workspace vectors of length n
!           wk2
!          xlmda  argument for characteristic equation
!
! ... specifications for arguments
!
  use datatypes
  implicit none
!
  integer(i4) :: n, ldt
  real(dp)    :: determ, xlmda
  real(dp)    :: tri(ldt,2),wk1(1),wk2(1)
!
  integer(i4) :: i, l
!
  do 10 i = 1,n 
     wk1(i) = tri(i,1)-xlmda
10 continue
  wk2(n) = wk1(n)
  wk2(n-1) = wk1(n-1)*wk2(n)+tri(n,2)
  if (n.eq.2) go to 30
!
! ... beginning of loop
!
  do 20 l = n-2,1,-1
     wk2(l) = wk1(l)*wk2(l+1)+tri(l+1,2)*wk2(l+2)
20 continue
!
!     wk2(1) = solrn (n,wk1(-1),-1,tri(0,2),-1,wk2,-1)
!
! ... determinant computed
!
30 determ = wk2(1)
!
  return
  end 
  subroutine dfault(iparm,rparm)
!
! ... dfault sets the default values of iparm and rparm.
!
! ... parameter list --
!
!          iparm
!           and
!          rparm  arrays specifying options and tolerances
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: iparm(12)
  real(dp)    :: rparm(12)
!
!     description of variables in common blocks in main routine
!
!     srelpr  - computer precision (approx.)
!     if installer of package does not know srelpr value,
!     an approximate value can be determined from a simple
!     fortran program such as 
!
!     srelpr = 1.0
!   2 srelpr = 0.5*srelpr
!     temp = srelpr + 1.0
!     if (temp .gt. 1.0)  go to 2
!     srelpr = 2.0*srelpr
!     write (6,3) srelpr
!   3 format (5x,e15.8)
!     stop
!     end 
!
!     some values are-
!
!     srelpr = 7.1e-15   for cray x-mp  (approx.) 2**-47
!          = 1.49e-8   for dec 10  (approx.) 2**-26
!          = 1.192e-7  for vax 11/780 (approx) 2**-23
!          = 4.768e-7  for ibm 370/158
!
!             *** should be changed for other machines ***
!
!     to facilitate convergence, rparm(1) should be set to
!          500.*srelpr or larger
!
  srelpr = 2.0**(-47)
!
  iparm(1) = 100
  iparm(2) = 0
  iparm(3) = 0
  iparm(4) = 6
  iparm(5) = 0
  iparm(6) = 1
  iparm(7) = 1
  iparm(8) = 0
  iparm(9) = -2 
  iparm(10) = 0 
  iparm(11) = 0 
  iparm(12) = 0 
!
  rparm(1) = 0.5e-5
  rparm(2) = 0.0
  rparm(3) = 0.0
  rparm(4) = .75
  rparm(5) = 1.0
  rparm(6) = 0.0
  rparm(7) = .25
  rparm(8) = 100.0*srelpr 
  rparm(9) = 0.0
  rparm(10) = 0.0
  rparm(11) = 0.0
  rparm(12) = 0.0
!
  return
  end 
  function eigvns(ldt,n,tri,d,e2,ier)
!
! ... eigvns computes the largest eigenvalue of a symmetric 
!     tridiagonal matrix for conjugate gradient acceleration.
!
! ... parameter list --
!
!          ldt    leading dimension of tri
!          n      order of tridiagonal system
!          tri    symmetric tridiagonal matrix of order n
!          d      array for eqrt1s (negative diagonal elements)
!          e2     array for eqrt1s (super diagonal elements)
!          ier    error flag -- on return, ier=0 indicates that
!                    the largest eigenvalue of tri was found.
!
! ... local itpackv references --
!
!          eqrt1s
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: ldt, n, ier
  real(dp)    :: tri(ldt,2),d(n),e2(n)
  real(dp)    :: eigvns
!
  integer(i4) :: i
!
  eigvns = 0.0
!
  d(1) = -tri(1,1)
  do i = 2,n 
     d(i) = -tri(i,1)
     e2(i) = abs(tri(i,2))
  enddo
!
  call eqrt1s(d,e2,n,1_i4,0_i4,ier)
  eigvns = -d(1)
!
  return
  end 
  function eigvss(n,tri,start,zeta,itmax,ier)
!
! ... eigvss computes the largest eigenvalue of a symmetric 
!     tridiagonal matrix for conjugate gradient acceleration.
!     modified imsl routine zbrent used.
!
! ... parameter list --
!
!          n      order of tridiagonal system
!          tri    symmetric tridiagonal matrix of order n
!          start  initial lower bound of interval containing root
!          zeta   stopping criteria
!          ier    error flag -- on return, ier = 0 indicates that
!                    the largest eigenvalue of tri was found.
!
! ... local itpackv references --
!
!          zbrent
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, itmax, ier
  real(dp)    :: tri(1), start, zeta
  real(dp)    :: eigvss
!
  integer(i4) :: itmp, maxfn, nsig
  real(dp)    :: a, b, eps
!
  eigvss = 0.0
  itmp = int(-log10(abs(zeta)))
  nsig = max0(itmp,4)
  maxfn = max0(itmax,50)
  eps = 0.0
  a = start
  b = 1.0
  call zbrent(n,tri,eps,nsig,a,b,maxfn,ier)
  eigvss = b
!
  return
  end 
  subroutine eqrt1s (d,e2,n,m,isw,ier)
!
!   modified imsl routine name   - eqrt1s
!
!-----------------------------------------------------------------------
!
!   computer            - cdc/single
!
!   latest revision     - june 1, 1980
!
!   purpose             - smallest or largest m eigenvalues of a
!                           symmetric tridiagonal matrix
!
!   usage               - call eqrt1s (d,e2,n,m,isw,ier)
!
!   arguments    d      - input vector of length n containing
!                           the diagonal elements of the matrix.  the 
!                           computed eigenvalues replace the first m
!                           components of the vector d in non-
!                           decreasing sequence, while the remaining
!                           components are lost.
!                e2     - input vector of length n containing
!                           the squares of the off-diagonal elements
!                           of the matrix.  input e2 is destroyed.
!                n      - input scalar containing the order of the
!                           matrix.
!                m      - input scalar containing the number of
!                           smallest eigenvalues desired (m is
!                           less than or equal to n).
!                isw    - input scalar meaning as follows - 
!                           isw=1 means that the matrix is known to be
!                             positive definite.
!                           isw=0 means that the matrix is not known
!                             to be positive definite.
!                ier    - error parameter. (output)
!                           warning error
!                             ier = 601 indicates that successive
!                               iterates to the k-th eigenvalue were not
!                               monotone increasing. the value k is
!                               stored in e2(1).
!                           terminal error
!                             ier = 602 indicates that isw=1 but matrix
!                               is not positive definite
!
!   precision/hardware  - single and double/h32
!                       - single/h36,h48,h60
!
!   notation            - information on special notation and
!                           conventions is available in the manual
!                           introduction or through imsl routine uhelp
!
!   remarks      as written, the routine computes the m smallest
!                eigenvalues. to compute the m largest eigenvalues,
!                reverse the sign of each element of d before and
!                after calling the routine. in this case, isw must
!                equal zero.
!
!   copyright           - 1980 by imsl, inc. all rights reserved.
!
!   warranty            - imsl warrants only that imsl testing has been
!                           applied to this code. no other warranty,
!                           expressed or implied, is applicable.
!
!-----------------------------------------------------------------------
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, m, ier, isw
  real(dp)    :: d(n),e2(n)
!
  integer(i4) :: i, ii, ismin, j, k, k1
  real(dp)    :: dlam, q, err, ep, f, delta, tot, qp, r, p, s
!
!                                  srelpr = machine precision
!                                  first executable statement
!
  ier = 0
  dlam = 0.0
  err = 0.0
  s = 0.0
!
!                                  look for small sub-diagonal entries
!                                  define initial shift from lower
!                                  gerschgorin bound.
!
  tot = d(1)
  q = 0.0
  j = 0
  do 30 i = 1,n 
     p = q
     if (i.eq.1) go to 10 
     if (p.gt.srelpr*(abs(d(i))+abs(d(i-1)))) go to 20
10    e2(i) = 0.0
!
!                                  count if e2(i) has underflowed
!
20    if (e2(i).eq.0.0) j = j+1
     q = 0.0
     if (i.ne.n) q = sqrt(abs(e2(i+1)))
     tot = min(d(i)-p-q,tot)
30 continue
  if (isw.eq.1.and.tot.lt.0.0) go to 50
  do 40 i = 1,n 
     d(i) = d(i)-tot
40 continue
  go to 60
50 tot = 0.0
60 do 190 k = 1,m
!
!                                  next qr transformation
!
70    tot = tot+s
     delta = d(n)-s
     i = n
     f = abs(srelpr*tot)
     if (dlam.lt.f) dlam = f
     if (delta.gt.dlam) go to 90
     if (delta.ge.(-dlam)) go to 160
     ier = 602
     if (level.ge.1) write (nout,80)
80    format (/1x,'*** w a r n i n g ************'/1x, &
        '    in itpackv routine eqrt1s'/1x, &
        '    parameter isw = 1 but matrix'/1x, &
        '    not positive definite')
     go to 200
!
!                                  replace small sub-diagonal squares 
!                                  by zero to reduce the incidence of 
!                                  underflows
!
90    if (k.eq.n) go to 110
     k1 = k+1
     do 100 j = k1,n
        if (e2(j).le.(srelpr*(d(j)+d(j-1)))**2) e2(j) = 0.0
100    continue
110    f = e2(n)/delta
     qp = delta+f
     p = 1.0
     if (k.eq.n) go to 140
     k1 = n-k
     do 130 ii = 1,k1
        i = n-ii
        q = d(i)-s-f
        r = q/qp
        p = p*r+1.0
        ep = f*r
        d(i+1) = qp+ep
        delta = q-ep
        if (delta.gt.dlam) go to 120
        if (delta.ge.(-dlam)) go to 160
        ier = 602
        if (level.ge.1) write (nout,80)
        go to 200
120       f = e2(i)/q
        qp = delta+f
        e2(i+1) = qp*ep
130    continue
140    d(k) = qp
     s = qp/p
     if (tot+s.gt.tot) go to 70
     ier = 601
     e2(1) = k
     if (level.ge.1) write (nout,150) k
150    format (/1x,'*** w a r n i n g ************'//1x, &
        '    in itpackv routine eqrt1s'/1x, &
        '    successive iterates to the',i10/1x, &
        '    eigenvalue were not monotone increasing')
!
!                                  set error -- irregular end
!                                  deflate minimum diagonal element
!
     s = 0.0
     i = ismin(n-k+1_i4,d(k),1_i4)
     delta = min(qp,d(i))
!
!                                  convergence
!
160    if (i.lt.n) e2(i+1) = e2(i)*f/qp
     if (i.eq.k) go to 180
     do 170 j = i-1,k,-1
        d(j+1) = d(j)-s
        e2(j+1) = e2(j)
170    continue
180    d(k) = tot 
     err = err+abs(delta) 
     e2(k) = err
190 continue
  if (ier.eq.0) go to 210 
200 continue
210 return
  end 
  subroutine parcon (dtnrm,c1,c2,c3,c4,gamold,rhotmp,ibmth)
!
! ... parcon computes acceleration parameters for conjugate gradient
!     acceleration methods.
!
! ... parameter list --
!
!          dtnrm  inner product of residuals
!          c1     output -- rho*gamma
!          c2     output -- rho
!          c3     output -- 1-rho
!          c4     output -- rho*(1-gamma)
!          gamold output -- value of gamma at preceding iteration
!          rhotmp last estimate for value of rho
!          ibmth  indicator of basic method being accelerated by cg
!                      ibmth = 1,   jacobi
!                            = 2,   reduced system
!                            = 3,   ssor
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
  integer(i4) :: ibmth
  real(dp)    :: dtnrm, c1, c2, c3, c4, gamold, rhotmp
!
  integer(i4) :: ip
  real(dp)    :: rhoold
!
!     description of variables in common blocks in main routine
!
  ip = in-is
!
! ... set rhoold and gamold
!
  rhoold = rho
  gamold = gamma
!
! ... compute gamma (in+1)
!
! ... for jacobi or reduced system cg
!
  if (ibmth.le.2) gamma = 1.0/(1.0-dtnrm/delnnm)
!
! ... for ssor cg
!
  if (ibmth.eq.3) gamma = delnnm/dtnrm
!
! ... compute rho (in+1)
!
  rho = 1.0
  if (ip.eq.0) go to 20
  if (isym.eq.0) go to 10 
  rho = 1.0/(1.0-gamma*rhotmp/delsnm)
  go to 20
10 rho = 1.0/(1.0-gamma*delnnm/(gamold*delsnm*rhoold))
!
! ... compute constants c1, c2, c3, and c4
!
20 delsnm = delnnm
  rhotmp = rhoold
  c1 = rho*gamma
  c2 = rho
  c3 = 1.0-rho
  c4 = rho*(1.0-gamma)
!
  return
  end 
  subroutine pervec (n,p,v,work)
!
! ... pervec permutes a vector as dictated by the permutation
! ... vector p.  if p(i) = j, then v(j) gets v(i).
!
! ... parameter list --
!
!          n       length of vectors p, v, and work
!          p       integer permutation vector
!          v       vector to be permuted
!          work    workspace vector of length n
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n
  integer(i4) :: p(1)
  real(dp)    :: v(1),work(1)
!
  integer(i4) :: i
!
  call dcopy(n,v,1_i4,work,1_i4)
  do i = 1,n 
     v(p(i)) = work(i)
  enddo
  return
  end 
  subroutine pstop(n,u,dnrm,ccon,iflag,q1)
!
!     pstop performs a test to see if the iterative
!     method has converged to a solution inside the error
!     tolerance, zeta.
!
! ... parameter list --
!
!          n      order of system
!          u      present solution estimate
!          dnrm   inner product of pseudo-residuals at preceding
!                    iteration
!          con    stopping test parameter (= ccon)
!          iflag  stopping test integer flag
!                    iflag = 0,  sor iteration zero
!                    iflag = 1,  non-rs method
!                    iflag = 2,  rs method
!          q1     stopping test logical flag
!
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, iflag
  real(dp)    :: ccon, dnrm
  real(dp)    :: u(n)
  logical     :: q1
!
  real(dp)    :: con, tl, tr, ddot
  real(dp)    :: uold
!
!     description of variables in common block in main routine
!
  con = ccon
  halt = .false.
!
!     special procedure for zeroth iteration
!
  if (in.ge.1) go to 10
  q1 = .false.
  udnm = 1.0
  stptst = 1000.0
  if (iflag.le.0) return
!
! ... test if udnm needs to be recomputed
!
10 continue
  if (q1) go to 20
  if ((in.gt.5).and.(mod(in,5_i4).ne.0)) go to 20
  uold = udnm
  udnm = ddot(n,u,1_i4,u,1_i4)
  if (udnm.eq.0.0) udnm = 1.0
  if ((in.gt.5).and.(abs(udnm-uold).le.udnm*zeta)) q1 = .true.
!
! ... compute stopping test
!
20 tr = sqrt(udnm)
  tl = 1.0
  if (con.eq.1.0) go to 40
  if (iflag.eq.2) go to 30
  tl = sqrt(dnrm)
  tr = tr*(1.0-con)
  go to 40
30 tl = sqrt(2.0*dnrm)
  tr = tr*(1.0-con*con)
40 stptst = tl/tr
  if (tl.ge.tr*zeta) return
  halt = .true. 
!
  return
  end 

  function timer(timdmy)
!
! ... timer is a routine to return the execution time in
!     seconds.
!
! ... parameter list --
!
!          timdmy   dummy argument
  use datatypes
  implicit none
!
  real(dp) :: timer
  real(dp) :: timdmy
!
  real(dp) :: cputime
!
  timer = cputime()
!
!     real tarray(2)
!     call etime (tarray)
!     timer = tarray(1)
!
  return
  end 
  subroutine ypasx2(ndim,n,m,a,ja,y,x)
!
! ... ypasx2 does the loop
!
!           do 20 j = 1,m
!              do 10 i = 1,n
!                 y(i) = y(i) + a(i,j)*x(ja(i,j)) 
!       10     continue
!       20  continue
!
! ... parameters -- 
!
!       ndim      row dimension of a and ja arrays
!       n         order of system
!       m         number of columns in a and ja arrays
!       a         real array of active size n by m
!       ja        integer array of active size n by m
!       y         accumulation vector
!       x         right-hand-side vector
!
! ... specifications for parameters
!
  use datatypes
  implicit none
  integer(i4) :: ndim, n, m
  integer(i4) :: ja(ndim,3)
  real(dp)    :: a(ndim,3),x(1),y(1)
!
  integer(i4) :: i, j, l, lp1
!
  if (n.le.0 .or. m.le.0) return
  l = mod(m,4_i4)
  if (l.eq.0) go to 80
!
! ... initial short computations
!
  go to (10,30,50), l
10 do 20 i = 1,n 
     y(i) = y(i)+a(i,1)*x(ja(i,1))
20 continue
  go to 70
30 do 40 i = 1,n 
     y(i) = y(i)+a(i,1)*x(ja(i,1))+a(i,2)*x(ja(i,2))
40 continue
  go to 70
50 do 60 i = 1,n 
     y(i) = y(i)+a(i,1)*x(ja(i,1))+a(i,2)*x(ja(i,2))+a(i,3)*x(ja(i,3))
60 continue
70 if (m.le.4) return
!
! ... loop unrolling to a level of 4.
!
80 lp1 = l+1
  do 100 j = lp1,m,4
     do 90 i = 1,n
        y(i) = y(i)+a(i,j)*x(ja(i,j))+a(i,j+1)*x(ja(i,j+1))+a(i,j+2) &
           *x(ja(i,j+2))+a(i,j+3)*x(ja(i,j+3))
90    continue
100 continue
  return
  end 
  subroutine ymasx2(ndim,n,m,a,ja,y,x)
!
! ... ymasx2 does the loop
!
!           do 20 j = 1,m
!              do 10 i = 1,n
!                 y(i) = y(i) - a(i,j)*x(ja(i,j)) 
!       10     continue
!       20  continue
!
! ... parameters -- 
!
!       ndim      row dimension of a and ja arrays
!       n         order of system
!       m         number of columns in a and ja arrays
!       a         real array of active size n by m
!       ja        integer array of active size n by m
!       y         accumulation vector
!       x         right-hand-side vector
!
  use datatypes
  implicit none
!
! ... specifications for parameters
!
  integer(i4) :: n, ndim, m
  integer(i4) :: ja(ndim,3)
  real(dp)    :: a(ndim,3),x(1),y(1)
!
  integer(i4) :: i, j, l, lp1
!
  if (n.le.0 .or. m.le.0) return
  l = mod(m,4_i4)
  if (l.eq.0) go to 80
!
! ... initial short computations
!
  go to (10,30,50), l
10 do 20 i = 1,n 
     y(i) = y(i)-a(i,1)*x(ja(i,1))
20 continue
  go to 70
30 do 40 i = 1,n 
     y(i) = y(i)-a(i,1)*x(ja(i,1))-a(i,2)*x(ja(i,2))
40 continue
  go to 70
50 do 60 i = 1,n 
     y(i) = y(i)-a(i,1)*x(ja(i,1))-a(i,2)*x(ja(i,2))-a(i,3)*x(ja(i,3))
60 continue
70 if (m.le.4) return
!
! ... loop unrolling to a level of 4.
!
80 lp1 = l+1
  do 100 j = lp1,m,4
     do 90 i = 1,n
        y(i) = y(i)-a(i,j)*x(ja(i,j))-a(i,j+1)*x(ja(i,j+1))-a(i,j+2) &
           *x(ja(i,j+2))-a(i,j+3)*x(ja(i,j+3))
90    continue
100 continue
  return
  end 
  subroutine vout(n,v,iswt,nout)
!
!     vout effects printing of residual and solution
!     vectors - called from perror
!
! ... parameter list --
!
!          v      vector of length n
!          iswt   labelling information 
!          nout   output device number
!
  use datatypes
  implicit none
!
! ... specifications for arguments
!
  integer(i4) :: n, nout, iswt
  real(dp)    :: v(n)
!
  integer(i4) :: i, j, jm1, k, kupper
!
!        if (n .le. 0) return 
!
  kupper = min0(n,4)
  if (iswt.eq.1) write (nout,10)
10 format (//5x,'residual vector')
  if (iswt.eq.2) write (nout,20)
20 format (//5x,'solution vector')
  write (nout,30) (i,i=1,kupper)
30 format (10x,4i15)
  write (nout,40)
40 format (10x,65('-')/)
!
  do 60 j = 1,n,4
     kupper = min0(j+3,n) 
     jm1 = j-1
     write (nout,50) jm1,(v(k),k=j,kupper)
50    format (4x,i5,'+  ',4e15.5)
60 continue
!
  return
  end 
  subroutine zbrent (n,tri,eps,nsig,a,b,maxfn,ier)
!
!   modified imsl routine name   - zbrent
!
!-----------------------------------------------------------------------
!
!   computer            - cdc/single
!
!   latest revision     - january 1, 1978
!
!   purpose             - zero of a function which changes sign in a
!                           given interval (brent algorithm)
!
!   usage               - call zbrent (f,eps,nsig,a,b,maxfn,ier)
!
!   arguments    tri    - a tridiagonal matrix of order n
!                eps    - first convergence criterion (input).  a root,
!                           b, is accepted if abs(f(b)) is less than or
!                           equal to eps.  eps may be set to zero.
!                nsig   - second convergence criterion (input).  a root,
!                           b, is accepted if the current approximation
!                           agrees with the true solution to nsig
!                           significant digits.
!                a,b    - on input, the user must supply two points, a
!                           and b, such that f(a) and f(b) are opposite
!                           in sign.
!                           on output, both a and b are altered.  b
!                           will contain the best approximation to the
!                           root of f. see remark 1.
!                maxfn  - on input, maxfn should contain an upper bound
!                           on the number of function evaluations
!                           required for convergence.  on output, maxfn
!                           will contain the actual number of function
!                           evaluations used.
!                ier    - error parameter. (output)
!                         terminal error
!                           ier = 501 indicates the algorithm failed to
!                             converge in maxfn evaluations.
!                           ier = 502 indicates f(a) and f(b) have the
!                             same sign.
!
!   precision/hardware  - single and double/h32
!                       - single/h36,h48,h60
!
!   notation            - information on special notation and
!                           conventions is available in the manual
!                           introduction or through imsl routine uhelp
!
!   remarks  1.  let f(x) be the characteristic function of the matrix
!                tri evaluated at x. function determ evaluates f(x).
!                on exit from zbrent, when ier=0, a and b satisfy the 
!                following,
!                f(a)*f(b) .le. 0,
!                abs(f(b)) .le. abs(f(a)), and
!                either abs(f(b)) .le. eps or
!                abs(a-b) .le. max(abs(b),0.1)*10.0**(-nsig).
!                the presence of 0.1 in this error criterion causes
!                leading zeroes to the right of the decimal point to be
!                counted as significant digits. scaling may be required
!                in order to accurately determine a zero of small
!                magnitude.
!            2.  zbrent is guaranteed to reach convergence within
!                k = (log((b-a)/d)+1.0)**2 function evaluations where
!                  d=min(over x in (a,b) of
!                    max(abs(x),0.1)*10.0**(-nsig)).
!                this is an upper bound on the number of evaluations. 
!                rarely does the actual number of evaluations used by 
!                zbrent exceed sqrt(k). d can be computed as follows, 
!                  p = min(abs(a),abs(b))
!                  p = max (0.1,p)
!                  if ((a-0.1)*(b-0.1).lt.0.0) p = 0.1
!                  d = p*10.0**(-nsig)
!
!   copyright           - 1977 by imsl, inc. all rights reserved.
!
!   warranty            - imsl warrants only that imsl testing has been
!                           applied to this code. no other warranty,
!                           expressed or implied, is applicable.
!
!-----------------------------------------------------------------------
  use datatypes
  use itcom1
  use itcom2
  use itcom3
  implicit none
!
!
! ... specifications for arguments
!
  integer(i4) :: n, nsig, maxfn, ier
  real(dp)    :: eps
  real(dp)    :: tri(1)
!
  integer(i4) :: ib3, ib4, ic
  real(dp)    :: a, b, c, d, e, fa, fb, fc, s, t, tol, temp, rone, r, rm, p
  real(dp)    :: determ, q
!
!     description of variables in common block in main routine
!
! ... local itpackv references --
!
!          determ
!
!                                  first executable statement
!
  ier = 0
  ib3 = 2*itmax+1
  ib4 = 3*itmax+1
  t = 10.0**(-nsig)
  ic = 2
  fa = determ(itmax,n,tri,tri(ib3),tri(ib4),a)
  fb = determ(itmax,n,tri,tri(ib3),tri(ib4),b)
  s = b
!
!                                  test for same sign
!
  if (fa*fb.gt.0.0) go to 110
10 c = a
  fc = fa
  d = b-c
  e = d
20 if (abs(fc).ge.abs(fb)) go to 30
  a = b
  b = c
  c = a
  fa = fb
  fb = fc
  fc = fa
30 continue
  tol = t*max(abs(b),0.1d0)
  rm = (c-b)*0.5
!
!                                  test for first convergence criteria
!
  if (abs(fb).le.eps) go to 80
!
!                                  test for second convergence criteria
!
  if (abs(c-b).le.tol) go to 80
!
!                                  check evaluation counter 
!
  if (ic.ge.maxfn) go to 90
!
!                                  is bisection forced
!
  if (abs(e).lt.tol) go to 60
  if (abs(fa).le.abs(fb)) go to 60
  s = fb/fa
  if (a.ne.c) go to 40
!
!                                  linear interpolation
!
  p = (c-b)*s
  q = 1.0-s
  go to 50
!
!                                  inverse quadratic interpolation
!
40 q = fa/fc
  r = fb/fc
  rone = r-1.0
  p = s*((c-b)*q*(q-r)-(b-a)*rone)
  q = (q-1.0)*rone*(s-1.0)
50 if (p.gt.0.0) q = -q
  if (p.lt.0.0) p = -p
  s = e
  e = d
!
!                                  if abs(p/q).ge.75*abs(c-b) then
!                                     force bisection
!
  if (p+p.ge.3.0*rm*q) go to 60
!
!                                  if abs(p/q).ge..5*abs(s) then force
!                                     bisection. s = the value of p/q 
!                                     on the step before the last one 
!
  if (p+p.ge.abs(s*q)) go to 60
  d = p/q
  go to 70
!
!                                  bisection
!
60 e = rm
  d = e
!
!                                  increment b
!
70 a = b
  fa = fb
  temp = d
  if (abs(temp).le.0.5*tol) temp = sign(0.5*tol,rm)
  b = b+temp
  s = b
  fb = determ(itmax,n,tri,tri(ib3),tri(ib4),s)
  ic = ic+1
  if (fb*fc.le.0.0) go to 20
  go to 10
!
!                                  convergence of b
!
80 a = c
  maxfn = ic
  go to 130
!
!                                  maxfn evaluations
!
90 ier = 501
  a = c
  maxfn = ic
  if (level.ge.1) write (nout,100) maxfn
100 format (/1x,'*** w a r n i n g ************'//1x, &
     '    in itpackv routine zbrent'/1x, &
     '    algorithm failed to converge'/1x, &
     '    in',i6,' iterations ')
  go to 130
!
!                                  terminal error - f(a) and f(b) have
!                                  the same sign
!
110 ier = 502
  maxfn = ic
  if (level.ge.1) write (nout,120)
120 format (/1x,'*** w a r n i n g ************'//1x, &
     '    in itpackv routine zbrent  '/1x, &
     '    f(a) and f(b) have same sign   ')
130 continue
  return
  end 
  function ismin(n,sx,incx)
!
!     find smallest index of minimum value of single precision sx.
!
  use datatypes
  implicit none
!
  integer(i4) :: ismin
  integer(i4) :: n, incx
  real(dp)    :: sx(1), smin, xval
!
  integer(i4) :: i, ii, ns
!
  ismin = 0
  if (n.le.0) return
  ismin = 1
  if (n.le.1) return
  if (incx.eq.1) go to 30 
!
!        code for increments not equal to 1.
!
  smin = sx(1)
  ns = n*incx
  ii = 1
  do 20 i = 1,ns,incx
     xval = sx(i)
     if (xval.ge.smin) go to 10
     ismin = ii 
     smin = xval
10    ii = ii+1
20 continue
  return
!
!        code for increments equal to 1.
!
30 smin = sx(1)
  do 40 i = 2,n 
     xval = sx(i)
     if (xval.ge.smin) go to 40
     ismin = i
     smin = xval
40 continue
  return
  end 
  subroutine whenige(n,p,itarg,ip,npt)
  use datatypes
  implicit none
!
  integer(i4) :: n, npt, itarg
  integer(i4) :: p(n), ip(n)
!
  integer(i4) :: i
!
  npt = 0
  do 10 i = 1,n 
     if (p(i) .lt. itarg) go to 10
     npt = npt + 1
     ip(npt) = i
10   continue
  return
  end 

  subroutine whenilt(n,p,itarg,ip,npt)
  use datatypes
  implicit none
!
  integer(i4) :: n, itarg, npt
  integer(i4) :: p(n), ip(n)
!
  integer(i4) :: i
!
  npt = 0
  do 10 i = 1,n 
     if (p(i) .ge. itarg) go to 10
     npt = npt + 1
     ip(npt) = i
10   continue
  return
  end 
