  subroutine createsphere_dodeca(nppa,spherevec)
!
!  Creates atom centred mesh of points on the surface 
!  of a sphere of unit radius. Dodecahedral version.
!
!  On input :
!
!  nppa      = total number of mesh points
!
!  On output :
!
!  spherevec = unit Cartesian vectors in direction of each
!              mesh point from centre of the sphere
!
!  Adapted from the COSMO routine by A. Klamt
!
  use datatypes
  use numbers, only : third
  implicit none
!
!  Passed arguments
!
  integer(i4) :: nppa
  real(dp)    :: spherevec(3,nppa)
!
!  Local variables
!
  integer(i4) :: fset(3,20)
  integer(i4) :: kset(2,30)
  integer(i4) :: i, ix, j, j1, j2, l
  integer(i4) :: k, kh, m, na, nb, nc, nd
  real(dp)    :: beta, dist, h, r
!
!  Data blocks
!
  data kset/ 1, 2, 1, 3, 1, 4, 1, 5, 1, 6, &
            12,11,12,10,12, 9,12, 8,12, 7, &
             2, 3, 3, 4, 4, 5, 5, 6, 6, 2, &
             7, 8, 8, 9, 9,10,10,11,11, 7, &
             2, 7, 7, 3, 3, 8, 8, 4, 4, 9, &
             9, 5, 5,10,10, 6, 6,11,11, 2/
  data fset/ 1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, &
            12,11,10,12,10, 9,12, 9, 8,12, 8, 7,12, 7,11, &
             2, 3, 7, 3, 4, 8, 4, 5, 9, 5, 6,10, 6, 2,11, &
             7, 8, 3, 8, 9, 4, 9,10, 5,10,11, 6,11, 7, 2/
!
!  Create basic dodecahedron
!
  nd = 1
  spherevec(1,nd) = -1.0_dp
  spherevec(2,nd) =  0.0_dp
  spherevec(3,nd) =  0.0_dp
  r = sqrt(0.8d0)
  h = sqrt(0.2d0)
  do i = -1,1,2
    do j = 1,5
      nd = nd + 1
      beta = 1.0_dp + j*1.25663706_dp + (i+1)*0.3141593_dp
      spherevec(2,nd) = r*cos(beta)
      spherevec(3,nd) = r*sin(beta)
      spherevec(1,nd) = i*h
    enddo
  enddo
  nd = 12
  spherevec(2,nd) = 0.0_dp
  spherevec(3,nd) = 0.0_dp
  spherevec(1,nd) = 1.0_dp
!
!  Fill in dodecahedron as required to create mesh with
!  NPPA points on the surface of a sphere.
!
  m = (nppa - 2)/10
  do k = 0,10
    if (mod(m,3_i4) .ne. 0) goto 10
    m = m/3
  enddo
10 do l = 0,10
    if (mod(m,4_i4) .ne. 0) goto 20
    m = m/4
  enddo
20 kh = k/2
  m = 2**l*3**kh
!
!  Create on each edge 2**l*3**kh-1 new points
!
  do i = 1,30
    na = kset(1,i)
    nb = kset(2,i)
    do j = 1,m-1
      nd = nd+1
      do ix = 1,3
        spherevec(ix,nd) = spherevec(ix,na)*(m-j) + spherevec(ix,nb)*j
      enddo
    enddo
  enddo
!
!  Create points within each triangle
!
  do i = 1,20
    na = fset(1,i)
    nb = fset(2,i)
    nc = fset(3,i)
    do j1 = 1,m-1
      do j2 = 1,m-j1-1
        nd = nd+1
        do ix = 1,3
          spherevec(ix,nd) = spherevec(ix,na)*(m-j1-j2) &
                           + spherevec(ix,nb)*j1 &
                           + spherevec(ix,nc)*j2
        enddo
      enddo
    enddo
  enddo
  if (k .ne. 2*kh) then
!
!  Create additional subgrids
!
    do i = 1,20
      na = fset(1,i)
      nb = fset(2,i)
      nc = fset(3,i)
      do j1 = 0,m-1
        do j2 = 0,m-j1-1
          nd = nd + 1
          do ix = 1,3
            spherevec(ix,nd) = spherevec(ix,na)*(m-j1-j2-2*third) &
                             + spherevec(ix,nb)*(j1+third) &
                             + spherevec(ix,nc)*(j2+third)
          enddo
        enddo
      enddo
    enddo
    do i = 1,20
      na = fset(1,i)
      nb = fset(2,i)
      nc = fset(3,i)
      do j1 = 0,m-2
        do j2 = 0,m-j1-2
          nd = nd+1
          do ix = 1,3
            spherevec(ix,nd) = spherevec(ix,na)*(m-j1-j2-4*third) &
                             + spherevec(ix,nb)*(j1+2*third) &
                             + spherevec(ix,nc)*(j2+2*third)
          enddo
        enddo
      enddo
    enddo
  endif
!
!  Normalize all vectors
!
  do i = 1,nppa
    dist = spherevec(1,i)**2 + spherevec(2,i)**2 + spherevec(3,i)**2
    dist = 1.0_dp/sqrt(dist)
    spherevec(1,i) = spherevec(1,i)*dist
    spherevec(2,i) = spherevec(2,i)*dist
    spherevec(3,i) = spherevec(3,i)*dist
  enddo
!
  return
  end
  subroutine createsphere_octa(nppa,spherevec)
!
!  Creates atom centred mesh of points on the surface 
!  of a sphere of unit radius. Octahedral version.
!
!  On input :
!
!  nppa      = total number of mesh points
!
!  On output :
!
!  spherevec = unit Cartesian vectors in direction of each
!              mesh point from centre of the sphere
!
  use datatypes
  use numbers, only : third
  implicit none
!
!  Passed arguments
!
  integer(i4) :: nppa
  real(dp)    :: spherevec(3,nppa)
!
!  Local variables
!
  integer(i4) :: fset(3,8)
  integer(i4) :: kset(2,12)
  integer(i4) :: i, ix, j, j1, j2, l
  integer(i4) :: k, kh, m, na, nb, nc, nd
  real(dp)    :: dist
!
!  Data blocks
!
  data kset/ 1, 2, 1, 3, 1, 4, 1, 5, 2, 3, &
             2, 4, 2, 6, 3, 5, 3, 6, 4, 5, &
             4, 6, 5, 6/
  data fset/ 1, 2, 3, 1, 2, 4, 1, 4, 5, 1, 3, 5, 2, 3, 6, &
             2, 4, 6, 3, 5, 6, 4, 5, 6/
!
!  Create basic octahedron
!
  nd = 1
  spherevec(1,nd) = -1.0d0
  spherevec(2,nd) =  0.0d0
  spherevec(3,nd) =  0.0d0
  nd = 2
  spherevec(1,nd) =  0.0d0
  spherevec(2,nd) = -1.0d0
  spherevec(3,nd) =  0.0d0
  nd = 3
  spherevec(1,nd) =  0.0d0
  spherevec(2,nd) =  0.0d0
  spherevec(3,nd) = -1.0d0
  nd = 4
  spherevec(1,nd) =  0.0d0
  spherevec(2,nd) =  0.0d0
  spherevec(3,nd) =  1.0d0
  nd = 5
  spherevec(1,nd) =  0.0d0
  spherevec(2,nd) =  1.0d0
  spherevec(3,nd) =  0.0d0
  nd = 6
  spherevec(2,nd) =  0.0d0
  spherevec(3,nd) =  0.0d0
  spherevec(1,nd) =  1.0d0
!
!  Fill in octahedron as required to create mesh with
!  NPPA points on the surface of a sphere.
!
  m = (nppa - 2)/4
  do k = 0,10
    if (mod(m,3_i4) .ne. 0) goto 10
    m = m/3
  enddo
10 do l = 0,10
    if (mod(m,4_i4) .ne. 0) goto 20
    m = m/4
  enddo
20 kh = k/2
  m = 2**l*3**kh
!
!  Create on each edge 2**l*3**kh-1 new points
!
  do i = 1,12
    na = kset(1,i)
    nb = kset(2,i)
    do j = 1,m-1
      nd = nd+1
      do ix = 1,3
        spherevec(ix,nd) = spherevec(ix,na)*(m-j) + spherevec(ix,nb)*j
      enddo
    enddo
  enddo
!
!  Create points within each triangle
!
  do i = 1,8
    na = fset(1,i)
    nb = fset(2,i)
    nc = fset(3,i)
    do j1 = 1,m-1
      do j2 = 1,m-j1-1
        nd = nd+1
        do ix = 1,3
          spherevec(ix,nd) = spherevec(ix,na)*(m-j1-j2) &
                           + spherevec(ix,nb)*j1 &
                           + spherevec(ix,nc)*j2
        enddo
      enddo
    enddo
  enddo
  if (k .ne. 2*kh) then
!
!  Create additional subgrids
!
    do i = 1,8
      na = fset(1,i)
      nb = fset(2,i)
      nc = fset(3,i)
      do j1 = 0,m-1
        do j2 = 0,m-j1-1
          nd = nd + 1
          do ix = 1,3
            spherevec(ix,nd) = spherevec(ix,na)*(m-j1-j2-2*third) &
                             + spherevec(ix,nb)*(j1+third) &
                             + spherevec(ix,nc)*(j2+third)
          enddo
        enddo
      enddo
    enddo
    do i = 1,8
      na = fset(1,i)
      nb = fset(2,i)
      nc = fset(3,i)
      do j1 = 0,m-2
        do j2 = 0,m-j1-2
          nd = nd+1
          do ix = 1,3
            spherevec(ix,nd) = spherevec(ix,na)*(m-j1-j2-4*third) &
                             + spherevec(ix,nb)*(j1+2*third) &
                             + spherevec(ix,nc)*(j2+2*third)
          enddo
        enddo
      enddo
    enddo
  endif
!
!  Normalize all vectors
!
  do i = 1,nppa
    dist = spherevec(1,i)**2 + spherevec(2,i)**2 + spherevec(3,i)**2
    dist = 1.0_dp/sqrt(dist)
    spherevec(1,i) = spherevec(1,i)*dist
    spherevec(2,i) = spherevec(2,i)*dist
    spherevec(3,i) = spherevec(3,i)*dist
  enddo
!
  return
  end
