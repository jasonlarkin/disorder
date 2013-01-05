      program mc2arc
C
C  Generates an archive file for visualising GULP .gmc files
C
C  species symbols have to be truncated to 4 characters due
C  to fixed format of arc files
C
C  Julian Gale, Curtin University, May 2007
C
      use datatypes
      implicit real(dp) (a-h,o-z), integer(i4) (i-n)
C
      integer(i4), dimension(:), allocatable :: nat
      integer(i4), dimension(:), allocatable :: ntype
      real(dp),    dimension(:), allocatable :: x
      real(dp),    dimension(:), allocatable :: y
      real(dp),    dimension(:), allocatable :: z
      real(dp)                               :: rv(3,3)
C
      character*1 blank
      character*2 asym
      character*4 wtype1,lab2
      character*5 lab
      character*60 filein,fileout
      character*80 line
      logical eof
      iin = 31
      iout = 32
      eof=.false.
      etot=0.0_dp
      blank=' '
      wtype1='CORE'
      q=0.0_dp
      evtokcal = 23.0604_dp
C
C  Write banner
C
      write(6,'(''********************************************'')')
      write(6,'(''* MC2arc - file convertor for .gmc -> .arc *'')')
      write(6,'(''********************************************'',/)')
C
C  Get file name
C
      write(6,'('' What is the file name? '',$)')
      read(5,'(a)') filein
C
C  Create output filename
C
C  Locate .gmc ending and replace with .arc or
C  if not present add to the end while remaining
C  within string length.
C
      ind = index(filein,'.gmc')
      if (ind.eq.0) ind = index(filein,' ')
      ind = min(ind,57)
      fileout = filein
      fileout(ind:ind+3) = '.arc'
C****************************
C  Initialisation of files  *
C****************************
      open(iin,file=filein,status='old',form='formatted',err=20)
      open(iout,file=fileout,status='unknown')
C
C  Write out header
C
      write(iout,'(''!BIOSYM archive 2'')')
      write(iout,'(''PBC=ON'')')
C*********************************************
C  Loop over file looking for relevant data  *
C*********************************************
      do while (.not.eof)
C#######################
C  Read line and skip  #
C#######################
        read(iin,'(a)',end=10) line(1:1)
C################
C  Read energy  #
C################
        read(iin,'(f32.8)',end=10) etot
C##############
C  Read cell  #
C##############
        read(iin,'(3f16.8)',end=10) (rv(j,1),j=1,3)
        read(iin,'(3f16.8)',end=10) (rv(j,2),j=1,3)
        read(iin,'(3f16.8)',end=10) (rv(j,3),j=1,3)
        call uncell(rv,a,b,c,alpha,beta,gamma)
C#########################
C  Read number of atoms  #
C#########################
        read(iin,'(i8)',end=10) numat
C
C  Allocate memory for atoms
C
        allocate(nat(numat))
        allocate(ntype(numat))
        allocate(x(numat))
        allocate(y(numat))
        allocate(z(numat))
C################
C  Coordinates  #
C################
        do i = 1,numat
          read(iin,'(i4,1x,i5,3(1x,f12.6))',end=10) 
     .      nat(i),ntype(i),x(i),y(i),z(i)
        enddo
C***********************
C  Configuration dump  *
C***********************
        write(iout,'(64a1,f16.6)')(blank,i=1,64),etot*evtokcal
        write(iout,'(''!DATE'')')
        write(iout,'(''PBC'',6f10.4)') a,b,c,alpha,beta,gamma
        do i=1,numat
          call label(nat(i),ntype(i),lab)
          lab2=lab(1:4)
          call label(nat(i),0,lab)
          asym=lab(1:2)
          write(iout,
     *      '(a4,1x,3f15.9,1x,a4,1x,i4,2(1x,a2),1x,f8.4,1x,i4)')
     *      lab2,x(i),y(i),z(i),wtype1,i,asym,asym,q,i
        enddo
        write(iout,'(''end'')')
        write(iout,'(''end'')')
C
C  Free memory for atoms
C
        deallocate(nat)
        deallocate(ntype)
        deallocate(x)
        deallocate(y)
        deallocate(z)
      enddo
   10 continue
C
C  Close files
C
      close(iin)
      close(iout)
C
C  Output output file name
C
      write(6,'('' Archive file written as '',a60)')fileout
      stop
C
C  Errors
C
   20 write(6,'('' ERROR - input file not found!'')')
      stop
      end
