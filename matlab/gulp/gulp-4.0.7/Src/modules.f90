!*********************
!  Modules for GULP  *
!*********************
!
!   9/01 nregionno added to keep track of region numbers
!  10/01 Extra arrays added for attachment energies
!  11/01 Attachment energy added to energies module
!   5/02 Energy shift scale for each configuration added
!   5/02 Minimum cell criteria added
!   5/02 Tapers adjusted
!   5/02 Brenner module added
!   6/02 Bi- and tri-cubic interpolation coefficient arrays added
!   6/02 Torsional spline interpolation coefficient array added
!   8/02 Density added
!   8/02 Output of files for DLV added
!   8/02 Modifications made for external force
!  10/02 Interpolation data arrays for Brenner potentials modified
!  10/02 MD constraint added
!  10/02 ReaxFF module added
!  11/02 Einstein model added
!  11/02 Parallel distribution pointers added
!   1/03 Wolf sum parameters added
!   1/03 Cell module added for spatial decomposition
!   2/03 Nudged elastic band module added
!   3/03 Extra arrays added for EEM/QEq
!   6/03 XML modifications made
!   6/03 Potsites data store added
!   6/03 LBFGS option added
!   7/03 Bond order potential data added
!  10/03 Point group symmetry module added
!  10/03 New variables added to spatial module
!  11/03 Module for maxmany dynamic memory added
!   4/04 Module bondvectors added
!   7/04 Bond order charge arrays added
!   9/04 Extra data structures for charge derivatives added
!  10/04 Element module moved to the start so that maxele can be used later
!  11/04 pi now evaluated rather than preset
!   6/05 Monopole charge added as observable
!   7/05 Arrays for charge coupled potentials added
!   2/06 Omegadirtype added
!   3/06 Array for multiple densities added in EAM
!   3/06 Optical damping factor added
!   5/06 Fitting of stresses added
!   5/06 Separate species indicator for EAM functionals added
!   5/06 Individual species masses allowed for
!   7/06 Sixbody module added
!   9/06 Dreiding force constant rules flag added for four body terms
!   9/06 Theta tapering added
!   9/06 Arrays added to store literal symbols
!  10/06 Neutron module and other neutron related things added : ers29
!  11/06 NEB data structures added
!  11/06 x/y/zfracimage arrays added
!   1/07 Gasteiger arrays added
!   2/07 nbondedtype added
!   2/07 Electric field module added
!   3/07 lPrintEAM option added
!   3/07 Radial force added
!   3/07 ldumpconnectivity added to dump module
!   3/07 Chemshell module added
!   3/07 nbotype/nbotype2 added to realvectors
!   4/07 UFF species data arrays added
!   5/07 UFF torsion array added
!   5/07 Region type added
!   7/07 Extra dimension added to n3botype
!   7/07 GCMC molecule flag added
!   7/07 Metadynamics module added
!   7/07 Arrays added to reaxFFdata module
!   7/07 Plane potential module added
!  11/07 lreaxFFqreal added
!  12/07 reaxFF 3-body conjugation parameters added
!  12/07 mdmaxtemp option added
!  12/07 Logical arrays added to control whether the overcoordination/1-3 
!        corrections are applied to bond orders in ReaxFF
!   1/08 Monte Carlo arrays modified to create a single pointer for atoms
!        involved in a trial operation
!   1/08 lreaxFFqreal removed
!   3/08 mdmaxvol option added
!   3/08 Bond order threshold added for ReaxFF
!   3/08 numofspec array added
!   4/08 Extra VDW & Coulomb parameter arrays added to ReaxFF to save computation
!   4/08 Ability to handle multiple reaxFFval3 potentials added
!   6/08 ReaxFF Q shell structure arrays added
!  10/08 COSMO module and variables added
!  10/08 MEAM modifications added
!  11/08 nfvar3 added to fitting module
!  11/08 Name of array for 1/bpot for Buckingham potential changed from rho to rhopot
!  11/08 Logical array to flag out of plane potentials added
!  11/08 Values in element module all made explicitly double precision
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!  12/08 synchro module added
!   1/09 swap move added to Monte Carlo
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   1/09 VBO_twobody potential added
!   1/09 Core-shell vector array added
!   1/09 lreaxFFpboOK added 
!   3/09 Value of MPI communicator added to parallel module
!   3/09 Finite difference flag added
!   4/09 New order 3 MEAM density components added by increasing maxmeamcomponent to 24 from 21
!   4/09 New array added, neamfnmeamtype, to indicate whether a MEAM function should use 21 or 24 components
!   4/09 New array added, neamfnmeamcombotype, to indicate whether a MEAM function should use the square
!        rooted sum of the density components or an exponential function.
!   4/09 Vectors module added with vector_pair type
!   6/09 Name of module three changed to m_three for benefit of chemshell
!   6/09 Module added for EVB method
!   6/09 New molecule indexing arrays added
!   7/09 cutoffmax/cutoffmaxbo moved to module
!   1/10 Arrays added to properties module for Young's moduli and Poisson ratios
!   1/10 One-body potential module added
!   3/10 Separate default weights for cell, coordinates and properties added
!   3/10 Flags introduced to indicate whether a potential was generated by a rule or not
!   5/10 Molecule time added
!   8/10 New quantities added to random number module
!   8/10 lfix1atom added
!   8/10 floatwords added
!   9/10 scatterdata module added
!   1/11 Alternative space group symbols moved to symmetry module
!   3/11 Lammps potential file added
!   4/11 Potential names moved to module
!   8/11 Array structure to keep track of fixed charges species in ReaxFF added for restarting
!   8/11 Plumed modifications added
!   1/12 => null() added to all pointers for benefit of NAG compiler
!   3/12 Output of DCD files added
!   9/12 Pacha parameters added from Marc Henry
!  10/12 Module added for variables associated with trapping sigint
!  10/12 Lammps file output added 
!  11/12 Modified Wolf sum of Fennell and Gezelter added
!  12/12 Q0 value arrays added for electronegativity methods
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
!  Julian Gale, NRI, Curtin University, December 2012
!

!
!  Elemental data (including EEM)
!
  module element
    use datatypes
    integer(i4), parameter :: maxele = 107
    integer(i4),      save :: nqeqitermax
    integer(i4),      save :: ngastitermax
    logical,          save :: lgasteiger
    logical,          save :: lpacha
    logical,          save :: lqeq
    logical,          save :: lSandM
    logical,          save :: lSandMnoZZ
    logical,          save :: leemparaltered(maxele)
    logical,          save :: lelementOK(maxele)
    character(len=2), save :: atsym(maxele)
    character(len=2), save :: atsymin(maxele)
    logical,          save :: lreaxFFqfix(maxele)
    logical,          save :: lreaxFFunder(maxele)
    real(dp),         save :: atmass(maxele)
    real(dp),         save :: atmassin(maxele)
    real(dp),         save :: bbar(maxele) !ers29 8/2/06
    real(dp),         save :: bbarin(maxele) !ers29 20/8/07
    real(dp),         save :: cc(maxele)
    real(dp),         save :: gasteigerA(maxele)
    real(dp),         save :: gasteigerB(maxele)
    real(dp),         save :: gasteigerC(maxele)
    real(dp),         save :: rcov(maxele)
    real(dp),         save :: rcovin(maxele)
    real(dp),         save :: rion(maxele)
    real(dp),         save :: rionin(maxele)
    real(dp),         save :: rr(maxele)
    real(dp),         save :: rvdw(maxele)
    real(dp),         save :: rvdwin(maxele)
    real(dp),         save :: chi(maxele)                          ! Electronegativity parameters for EEM model
    real(dp),         save :: rmu(maxele)                          ! Hardness parameters for EEM model
    real(dp),         save :: q0(maxele)                           ! Q0 for EEM model
    real(dp),         save :: chiold(18)
    real(dp),         save :: rmuold(18)
    real(dp),         save :: chi_pacha(maxele)                    ! Electronegativity parameters for Pacha model
    real(dp),         save :: rad_pacha(maxele)                    ! Atomic radii used in Pacha model to get hardness
    real(dp),         save :: mu_pacha(maxele)                     ! Hardness parameters for Pacha model
    real(dp),         save :: q0_pacha(maxele)                     ! Q0 for Pacha model
    real(dp),         save :: gasttol
    real(dp),         save :: qeqchi(maxele)                       ! Electronegativity parameters for QEq model
    real(dp),         save :: qeqmu(maxele)                        ! Hardness parameters for QEq model
    real(dp),         save :: qeqrad(maxele)                       ! Atomic radii used in QEq model
    real(dp),         save :: qeqq0(maxele)                        ! Q0 for QEq model
    real(dp),         save :: qeqlambda
    real(dp),         save :: qeqscfcrit
    real(dp),         save :: rqeq
    real(dp),         save :: reaxFFchi(maxele)
    real(dp),         save :: reaxFFgamma(maxele)
    real(dp),         save :: reaxFFmu(maxele)
    real(dp),         save :: reaxFFq0(maxele)
    real(dp),         save :: reaxFFqfix(maxele)
    real(dp),         save :: reaxFFshell(3,maxele)
    real(dp),         save :: siginc(maxele) !ers29 8/2/06
    real(dp),         save :: sigincin(maxele) !ers29 20/8/07
    real(dp),         save :: smchi(maxele)                        ! Electronegativity parameters for SM model
    real(dp),         save :: smmu(maxele)                         ! Hardness parameters for SM model
    real(dp),         save :: smq0(maxele)                         ! Q0 for SM model
    real(dp),         save :: smZnuc(maxele)
    real(dp),         save :: smzeta(maxele)
    integer(i4), private   :: i
    data atsym/'H ','He','Li','Be','B ','C ','N ','O ','F ', &
               'Ne','Na','Mg','Al','Si','P ','S ','Cl','Ar', &
               'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co', &
               'Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr', &
               'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh', &
               'Pd','Ag','Cd','In','Sn','Sb','Te','I ','Xe', &
               'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu', &
               'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf', &
               'Ta','W ','Re','Os','Ir','Pt','Au','Hg','Tl', &
               'Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th', &
               'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es', &
               'Fm','Md','No','Lr','Rf','Ha','D ','X '/
    data atmass/1.01_dp,4.00_dp,6.94_dp,9.01_dp,10.81_dp,12.01_dp,14.01_dp,16.00_dp, &
               19.00_dp,20.18_dp,22.99_dp,24.31_dp,26.98_dp,28.09_dp,30.97_dp, &
               32.07_dp,35.45_dp,39.95_dp,39.10_dp,40.08_dp,44.96_dp,47.88_dp, &
               50.94_dp,52.00_dp,54.94_dp,55.85_dp,58.93_dp,58.69_dp,63.55_dp, &
               65.39_dp,69.72_dp,72.61_dp,74.92_dp,78.96_dp,79.90_dp,83.80_dp, &
               85.47_dp,87.62_dp,88.91_dp,91.22_dp,92.91_dp,95.94_dp,98.00_dp, &
               101.07_dp,102.91_dp,106.42_dp,107.87_dp,112.41_dp,114.82_dp, &
               118.71_dp,121.75_dp,127.60_dp,126.91_dp,131.29_dp,132.91_dp, &
               137.33_dp,138.91_dp,140.12_dp,140.91_dp,144.24_dp,145.00_dp, &
               150.36_dp,151.97_dp,157.25_dp,158.93_dp,162.50_dp,164.93_dp, &
               167.26_dp,168.93_dp,173.04_dp,174.97_dp,178.49_dp,180.95_dp, &
               183.85_dp,186.21_dp,190.20_dp,192.22_dp,195.08_dp,196.97_dp, &
               200.59_dp,204.38_dp,207.20_dp,208.98_dp,209.00_dp,210.00_dp, &
               222.00_dp,223.00_dp,226.03_dp,227.03_dp,232.04_dp,231.04_dp, &
               238.03_dp,237.05_dp,244.00_dp,243.00_dp,247.00_dp,247.00_dp, &
               251.00_dp,252.00_dp,257.00_dp,258.00_dp,259.00_dp,260.00_dp, &
               261.00_dp,260.00_dp,2.01_dp,0.00_dp/
    data rcov/0.37_dp,0.00_dp,0.68_dp,0.35_dp,0.83_dp,0.77_dp,0.75_dp,0.73_dp,0.71_dp, &
              0.00_dp,0.97_dp,1.10_dp,1.35_dp,1.20_dp,1.05_dp,1.02_dp,0.99_dp,0.00_dp, &
              1.33_dp,0.99_dp,1.44_dp,1.47_dp,1.33_dp,1.35_dp,1.35_dp,1.34_dp,1.33_dp, &
              1.50_dp,1.52_dp,1.45_dp,1.22_dp,1.17_dp,1.21_dp,1.22_dp,1.21_dp,1.89_dp, &
              1.47_dp,1.12_dp,1.78_dp,1.56_dp,1.48_dp,1.47_dp,1.35_dp,1.40_dp,1.45_dp, &
              1.50_dp,1.59_dp,1.69_dp,1.63_dp,1.46_dp,1.46_dp,1.47_dp,1.40_dp,0.00_dp, &
              1.67_dp,1.34_dp,1.87_dp,1.83_dp,1.82_dp,1.81_dp,1.80_dp,1.80_dp,1.99_dp, &
              1.79_dp,1.76_dp,1.75_dp,1.74_dp,1.73_dp,1.72_dp,1.94_dp,1.72_dp,1.57_dp, &
              1.43_dp,1.37_dp,1.35_dp,1.37_dp,1.32_dp,1.50_dp,1.50_dp,1.70_dp,1.55_dp, &
              1.54_dp,1.54_dp,1.68_dp,0.00_dp,0.00_dp,0.00_dp,1.90_dp,1.88_dp,1.79_dp, &
              1.61_dp,1.58_dp,1.55_dp,1.53_dp,1.51_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp, &
              0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.37_dp,0.00_dp/
    data rion/-0.24_dp,0.000_dp,0.900_dp,0.590_dp,0.250_dp,0.300_dp,1.320_dp,1.260_dp, &
              1.190_dp,0.000_dp,1.160_dp,0.860_dp,0.675_dp,0.400_dp,0.310_dp,1.700_dp, &
              1.670_dp,0.000_dp,1.520_dp,1.140_dp,0.885_dp,0.745_dp,0.680_dp,0.755_dp, &
              0.670_dp,0.690_dp,0.790_dp,0.830_dp,0.870_dp,0.880_dp,0.760_dp,0.670_dp, &
              0.720_dp,1.840_dp,1.820_dp,0.000_dp,1.660_dp,1.320_dp,1.040_dp,0.860_dp, &
              0.780_dp,0.730_dp,0.700_dp,0.705_dp,0.690_dp,0.755_dp,1.290_dp,1.090_dp, &
              0.940_dp,0.830_dp,0.740_dp,2.070_dp,2.060_dp,0.000_dp,1.810_dp,1.490_dp, &
              1.172_dp,1.010_dp,1.130_dp,1.123_dp,1.110_dp,1.098_dp,1.087_dp,1.078_dp, &
              1.063_dp,1.052_dp,1.041_dp,1.030_dp,1.020_dp,1.008_dp,1.001_dp,0.850_dp, &
              0.780_dp,0.650_dp,0.670_dp,0.530_dp,0.710_dp,0.710_dp,0.710_dp,1.160_dp, &
              1.025_dp,1.330_dp,1.170_dp,0.810_dp,0.760_dp,0.000_dp,1.940_dp,1.620_dp, &
              1.260_dp,0.000_dp,0.920_dp,1.030_dp,0.850_dp,0.850_dp,1.350_dp,0.990_dp, &
              0.970_dp,0.961_dp,0.000_dp,0.000_dp,0.000_dp,1.240_dp,0.000_dp,0.000_dp, &
              0.000_dp,-0.24_dp,0.00_dp/
    data rvdw/1.08_dp,1.00_dp,1.80_dp,0.52_dp,1.70_dp,1.53_dp,1.48_dp,1.36_dp,1.30_dp, &
              0.00_dp,2.30_dp,1.64_dp,2.05_dp,2.10_dp,1.75_dp,1.70_dp,1.65_dp,0.00_dp, &
              2.80_dp,2.75_dp,2.15_dp,2.19_dp,1.99_dp,2.01_dp,2.01_dp,2.00_dp,1.99_dp, &
              1.81_dp,1.54_dp,2.16_dp,1.82_dp,1.75_dp,2.20_dp,2.00_dp,1.80_dp,2.82_dp, &
              2.19_dp,1.67_dp,2.66_dp,2.33_dp,2.21_dp,2.19_dp,2.01_dp,2.09_dp,2.16_dp, &
              2.24_dp,2.37_dp,2.52_dp,2.43_dp,2.18_dp,2.17_dp,2.20_dp,2.05_dp,0.00_dp, &
              2.49_dp,2.00_dp,2.79_dp,2.73_dp,2.72_dp,2.70_dp,2.69_dp,2.69_dp,2.97_dp, &
              2.67_dp,2.63_dp,2.61_dp,2.60_dp,2.58_dp,2.57_dp,2.90_dp,2.57_dp,2.34_dp, &
              2.13_dp,2.04_dp,2.01_dp,2.04_dp,1.97_dp,1.97_dp,1.85_dp,1.90_dp,2.31_dp, &
              2.30_dp,2.30_dp,2.51_dp,0.00_dp,0.00_dp,0.00_dp,2.84_dp,2.81_dp,2.67_dp, &
              2.40_dp,2.36_dp,2.31_dp,2.28_dp,2.25_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp, &
              0.00_dp,0.00_dp,1.24_dp,0.00_dp,0.00_dp,0.00_dp,1.17_dp,0.00_dp/
!
!  Neutron data (from Sears table) (ers29)
!
    data bbar/-0.3739_dp,0.326_dp,-0.19_dp,0.7790_dp,0.5300_dp,0.6646_dp,0.936_dp,0.5803_dp, &
               0.5654_dp,0.4566_dp,0.358_dp,0.5375_dp,0.3449_dp,0.41534_dp,0.513_dp,0.2847_dp, &
               0.9577_dp,0.1909_dp,0.367_dp,0.476_dp,1.229_dp,-0.3438_dp,-0.03824_dp,0.3635_dp, &
              -0.373_dp,0.954_dp,0.278_dp,1.03_dp,0.7718_dp,0.568_dp,0.7288_dp,0.8185_dp,0.658_dp, &
               0.797_dp,0.6795_dp,0.781_dp,0.709_dp,0.702_dp,0.775_dp,0.716_dp,0.7054_dp,0.6715_dp, &
               0.68_dp,0.721_dp,0.588_dp,0.591_dp,0.5922_dp,0.51_dp,0.4065_dp,0.6225_dp,0.557_dp, &
               0.58_dp,0.528_dp,0.492_dp,0.542_dp,0.507_dp,0.824_dp,0.484_dp,0.445_dp,0.769_dp, &
               1.26_dp,0.08_dp,0.722_dp,0.65_dp,0.738_dp,1.69_dp,0.801_dp,0.816_dp,0.707_dp,1.243_dp, &
               0.721_dp,0.777_dp,0.691_dp,0.486_dp,0.92_dp,1.07_dp,1.06_dp,0.96_dp,0.763_dp,1.2692_dp, &
               0.8776_dp,0.9405_dp,0.8532_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,1.052_dp,&
               0.91_dp,0.8417_dp,1.055_dp,0.0_dp,0.83_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,&
               0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.6671_dp,0.0_dp/
    data siginc/0.004496397_dp,80.26320618_dp,  0.916354021_dp, 0.00421109_dp,  1.710106494_dp, &
                0.000520054_dp, 0.50065297_dp,  0.000298697_dp, 0.00081833_dp,  0.008118318_dp, &
                1.674443677_dp, 0.07949699_dp,  0.008154692_dp, 0.003209138_dp, 0.004920812_dp, &
                0.007444265_dp, 5.274259458_dp, 0.225046143_dp, 0.267448108_dp, 0.042762012_dp, &
                4.519238603_dp, 2.864674597_dp, 5.081624226_dp, 1.829577176_dp, 0.401653423_dp, &
                0.383145042_dp, 4.828820613_dp, 5.168337415_dp, 0.544524168_dp, 0.076787247_dp, &
                0.155379285_dp, 0.181257396_dp, 0.059213913_dp, 0.317728288_dp, 0.097852218_dp, &
                0.015004014_dp, 0.483124253_dp, 0.057242296_dp, 0.15232365_dp,  0.017774706_dp, &
                0.002110202_dp, 0.043669552_dp, 0.489310228_dp, 0.067485333_dp, 0.255252758_dp, &
                0.090805505_dp, 0.58296327_dp,  2.431487003_dp, 0.543504645_dp, 0.022452847_dp, &
                0.001296083_dp, 0.092672925_dp, 0.306696935_dp,-0.001865936_dp, 0.208452703_dp, &
                0.149827_dp,    1.127735946_dp, 0.000252285_dp, 0.041544459_dp, 9.168738507_dp, &
                1.349630013_dp, 38.91957523_dp, 2.649352061_dp, 174.6907084_dp,-0.004198357_dp, &
                54.40918889_dp, 0.357404047_dp, 0.832606728_dp, 0.098712215_dp, 3.984341649_dp, &
                0.667485333_dp, 2.613317635_dp, 0.009796793_dp, 1.631873526_dp, 0.863823912_dp, &
                0.312762284_dp,-0.019574022_dp, 0.128832842_dp, 0.434248586_dp, 6.557227659_dp, &
                0.211610563_dp, 0.002539395_dp, 0.008307495_dp, 0.0_dp,         0.0_dp, &
                0.0_dp,         0.0_dp,         0.433629386_dp, 0.0_dp,         0.002747376_dp, &
                0.093788494_dp, 0.005243023_dp, 0.513315347_dp, 0.0_dp,         0.343027284_dp, &
                0.0_dp,         0.0_dp,         0.0_dp,         0.0_dp,         0.0_dp,&
                0.0_dp,         0.0_dp,         0.0_dp,         0.0_dp,         0.0_dp,&
                2.047683464_dp, 0.0_dp/
!
!  Cost function parameters for elements
!
    data   rr/0.38_dp,0.00_dp,1.00_dp,0.81_dp,0.79_dp,0.78_dp,0.72_dp,0.63_dp,0.58_dp, &
              0.00_dp,1.36_dp,1.21_dp,1.13_dp,1.02_dp,1.09_dp,1.03_dp,0.99_dp,0.00_dp, &
              1.73_dp,1.50_dp,1.34_dp,1.27_dp,1.21_dp,1.16_dp,1.17_dp,1.16_dp,1.09_dp, &
              1.04_dp,0.87_dp,1.07_dp,1.14_dp,1.21_dp,1.21_dp,1.18_dp,1.13_dp,0.00_dp, &
              1.84_dp,1.66_dp,1.52_dp,1.43_dp,1.40_dp,1.37_dp,0.00_dp,1.21_dp,1.18_dp, &
              1.11_dp,1.12_dp,1.28_dp,1.34_dp,1.37_dp,1.41_dp,1.40_dp,1.33_dp,0.00_dp, &
              2.05_dp,1.88_dp,1.71_dp,1.68_dp,1.66_dp,1.64_dp,0.00_dp,1.61_dp,1.62_dp, &
              1.58_dp,1.56_dp,1.54_dp,1.53_dp,1.51_dp,1.50_dp,1.49_dp,1.47_dp,1.42_dp, &
              1.39_dp,1.38_dp,1.37_dp,0.00_dp,1.37_dp,0.00_dp,0.00_dp,1.32_dp,1.62_dp, &
              1.53_dp,1.54_dp,1.54_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,1.70_dp, &
              0.00_dp,1.59_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp, &
              0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp/
    data   cc/0.89_dp,0.00_dp,0.97_dp,1.47_dp,1.60_dp,2.00_dp,2.61_dp,3.15_dp,3.98_dp, &
              0.00_dp,1.01_dp,1.23_dp,1.47_dp,1.58_dp,1.96_dp,2.35_dp,2.74_dp,0.00_dp, &
              0.91_dp,1.04_dp,1.20_dp,1.32_dp,1.45_dp,1.56_dp,1.60_dp,1.64_dp,1.70_dp, &
              1.75_dp,1.75_dp,1.66_dp,1.82_dp,1.51_dp,2.23_dp,2.51_dp,2.58_dp,0.00_dp, &
              0.89_dp,0.99_dp,1.11_dp,1.22_dp,1.23_dp,1.30_dp,0.00_dp,1.42_dp,1.54_dp, &
              1.35_dp,1.42_dp,1.46_dp,1.49_dp,1.72_dp,1.72_dp,2.72_dp,2.38_dp,0.00_dp, &
              0.86_dp,0.97_dp,1.08_dp,1.08_dp,1.07_dp,1.07_dp,0.00_dp,1.07_dp,1.01_dp, &
              1.11_dp,1.10_dp,1.10_dp,1.10_dp,1.11_dp,1.11_dp,1.06_dp,1.14_dp,1.23_dp, &
              1.33_dp,1.40_dp,1.46_dp,0.00_dp,1.55_dp,0.00_dp,0.00_dp,1.44_dp,1.44_dp, &
              1.55_dp,1.67_dp,1.67_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,1.11_dp, &
              0.00_dp,1.22_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp, &
              0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp,0.00_dp/
!
!  Mortier parameters
!
    data chiold/4.40877d0,3.17392d0,0.0d0,0.0d0,0.0d0,5.68045d0, &
             10.59916d0,8.5d0,32.42105d0,0.0d0,0.0d0,0.0d0, &
             -2.23952d0,1.33182d0,2.90541d0,0.0d0,0.0d0,0.0d0/
    data rmuold/13.77324d0,9.91710d0,0.0d0,0.0d0,0.0d0,9.05058d0, &
             13.18623d0,11.08287d0,90.00488d0,0.0d0,0.0d0,0.0d0, &
             7.67245d0,6.49259d0,6.29415d0,0.0d0,0.0d0,0.0d0/
!
!  QEq parameters
!
    data (qeqchi(i),i=1,36) /4.528d0,9.660d0,3.006d0,4.877d0, &
             5.110d0,5.343d0,6.899d0,8.741d0,10.874d0,11.04d0, &
             2.843d0,3.951d0,4.060d0,4.168d0,5.463d0,6.928d0, &
             8.564d0,9.465d0,2.421d0,3.231d0,3.395d0,3.470d0, &
             3.650d0,3.415d0,3.325d0,3.760d0,4.105d0,4.465d0, &
             3.729d0,5.106d0,3.641d0,4.051d0,5.188d0,6.428d0, &
             7.790d0,8.505d0/
    data (qeqchi(i),i=37,54)/2.331d0,3.024d0,3.830d0,3.400d0, &
             3.550d0,3.465d0,3.290d0,3.575d0,3.975d0,4.320d0, &
             4.436d0,5.034d0,3.506d0,3.987d0,4.899d0,5.816d0, &
             6.822d0,7.595d0/
    data (qeqchi(i),i=55,86)/2.183d0,2.814d0,2.8355d0,2.744d0, &
             2.858d0,2.8685d0,2.881d0,2.9115d0,2.8785d0,3.1665d0, &
             3.018d0,3.0555d0,3.127d0,3.1865d0,3.2514d0,3.2889d0, &
             2.9629d0,3.700d0,5.100d0,4.630d0,3.960d0,5.140d0, &
             5.000d0,4.790d0,4.894d0,6.270d0,3.200d0,3.900d0, &
             4.690d0,4.210d0,4.750d0,5.370/
    data (qeqchi(i),i=87,103)/2.00d0,2.843d0,2.835d0,3.175d0, &
             2.985d0,3.341d0,3.549d0,3.243d0,2.9895d0,2.8315d0, &
             3.1935d0,3.197d0,3.333d0,3.400d0,3.470d0,3.475d0, &
             3.500d0/
!        data (qeqmu(i),i=1,36)/6.49205d0,14.92d0,2.386d0,4.443d0,
    data (qeqmu(i),i=1,36)/6.9452d0,14.92d0,2.386d0,4.443d0, &
             4.750d0,5.063d0,5.880d0,6.682d0,7.474d0,10.55d0, &
             2.296d0,3.693d0,3.590d0,3.487d0,4.000d0,4.486d0, &
             4.946d0,6.355d0,1.920d0,2.880d0,3.080d0,3.380d0, &
             3.410d0,3.865d0,4.105d0,4.140d0,4.175d0,4.205d0, &
             2.501d0,4.285d0,3.160d0,3.438d0,3.809d0,4.131d0, &
             4.425d0,5.715d0/
    data (qeqmu(i),i=37,54) /1.846d0,2.440d0,2.810d0,3.550d0, &
             3.380d0,3.755d0,3.990d0,4.015d0,4.005d0,4.000d0, &
             3.134d0,3.957d0,2.896d0,3.124d0,3.342d0,3.526d0, &
             3.762d0,4.975d0/
    data (qeqmu(i),i=55,86) /1.711d0,2.396d0,2.7415d0,2.692d0, &
             2.564d0,2.6205d0,2.673d0,2.7195d0,2.7875d0,2.9745d0, &
             2.834d0,2.8715d0,2.891d0,2.9145d0,2.9329d0,2.965d0, &
             2.4629d0,3.400d0,2.850d0,3.310d0,3.920d0,3.630d0, &
             4.000d0,4.430d0,2.586d0,4.160d0,2.900d0,3.530d0, &
             3.740d0,4.210d0,4.750d0,5.370d0/
    data (qeqmu(i),i=87,103)/2.000d0,2.434d0,2.835d0,2.905d0, &
             2.905d0,2.853d0,2.717d0,2.819d0,3.0035d0,3.1895d0, &
             3.0355d0,3.101d0,3.089d0,3.10d0,3.110d0,3.175d0, &
             3.200d0/
    data (qeqrad(i),i=1,36) /0.371d0,1.300d0,1.557d0,1.240d0, &
             0.822d0,0.759d0,0.715d0,0.669d0,0.706d0,1.768d0, &
             2.085d0,1.500d0,1.201d0,1.176d0,1.102d0,1.047d0, &
             0.994d0,2.108d0,2.586d0,2.000d0,1.750d0,1.607d0, &
             1.470d0,1.402d0,1.533d0,1.393d0,1.406d0,1.398d0, &
             1.434d0,1.400d0,1.211d0,1.189d0,1.204d0,1.224d0, &
             1.141d0,2.270d0/
    data (qeqrad(i),i=37,54)/2.770d0,2.415d0,1.998d0,1.758d0, &
             1.603d0,1.530d0,1.500d0,1.500d0,1.509d0,1.544d0, &
             1.622d0,1.600d0,1.404d0,1.354d0,1.404d0,1.380d0, &
             1.333d0,2.459d0/
    data (qeqrad(i),i=55,86)/2.984d0,2.442d0,2.071d0,1.925d0, &
             2.007d0,2.007d0,2.000d0,1.978d0,2.227d0,1.968d0, &
             1.954d0,1.934d0,1.925d0,1.915d0,2.000d0,2.158d0, &
             1.896d0,1.759d0,1.605d0,1.538d0,1.600d0,1.700d0, &
             1.866d0,1.557d0,1.618d0,1.600d0,1.530d0,1.444d0, &
             1.514d0,1.480d0,1.470d0,2.200d0/
    data (qeqrad(i),i=87,103)/2.30d0,2.200d0,2.108d0,2.018d0, &
             1.800d0,1.713d0,1.800d0,1.840d0,1.942d0,1.900d0, &
             1.900d0,1.900d0,1.900d0,1.900d0,1.900d0,1.900d0, &
             1.900d0/
!
!  Pacha parameters - Marc Henry
!
    data (chi_pacha(i),i=1,36)/13.610d0,24.590d0, 5.392d0, 9.323d0, &
             12.130d0,15.050d0,18.130d0,21.360d0,24.800d0,28.310d0, &
              5.140d0, 7.646d0, 9.539d0,11.330d0,13.330d0,15.310d0, &
             16.970d0,19.170d0, 4.340d0, 6.113d0, 7.042d0, 8.170d0, &
              9.062d0, 9.768d0,10.340d0,10.638d0,10.857d0,11.128d0, &
             10.963d0, 9.394d0,10.390d0,11.800d0,13.080d0,14.340d0, &
             15.880d0,17.540d0/
    data (chi_pacha(i),i=37,54)/4.177d0, 5.695d0, 6.631d0, 7.808d0, &
              8.340d0, 8.707d0, 8.891d0, 8.939d0, 9.258d0, 9.401d0, &
             11.046d0, 8.994d0, 9.793d0,10.790d0,11.740d0,12.760d0, &
             13.950d0,15.270d0/
    data (chi_pacha(i),i=55,86)/3.898d0, 5.211d0, 5.830d0, 5.830d0, &
              5.830d0, 5.830d0, 5.830d0, 5.830d0, 5.830d0, 5.830d0, &
              5.830d0, 5.830d0, 5.830d0, 5.830d0, 5.830d0, 5.830d0, &
              6.454d0, 6.830d0, 7.932d0, 8.680d0, 9.456d0, 9.782d0, &
              9.959d0,10.159d0,11.332d0,10.438d0,10.580d0,10.970d0, &
             11.900d0,13.000d0,14.100d0,15.400d0/
    data (chi_pacha(i),i=87,102)/16*5.0d0/
    data (rad_pacha(i),i=1,36) /0.5300d0,0.2910d0,1.5860d0,1.0400d0, &
              0.7690d0,0.6200d0,0.5210d0,0.4500d0,0.3960d0,0.3540d0, &
              1.7130d0,1.2790d0,1.0440d0,0.9040d0,0.8030d0,0.7230d0, &
              0.6600d0,0.6070d0,2.1620d0,1.6900d0,1.5700d0,1.4770d0, &
              1.4010d0,1.4530d0,1.2780d0,1.2270d0,1.1810d0,1.1390d0, &
              1.1910d0,1.0650d0,0.9600d0,0.8860d0,0.8260d0,0.7750d0, &
              0.7300d0,0.6910d0/
    data (rad_pacha(i),i=37,54)/2.2870d0,1.8360d0,1.6930d0,1.5930d0, &
              1.5890d0,1.5200d0,1.3910d0,1.4100d0,1.3640d0,1.3250d0, &
              1.2860d0,1.1840d0,1.0930d0,1.0270d0,0.9690d0,0.9200d0, &
              0.8760d0,0.8370d0/
    data (rad_pacha(i),i=55,86)/2.5180d0,2.0600d0,1.0870d0,0.3660d0, &
              0.3510d0,0.3370d0,0.3250d0,0.3140d0,0.3055d0,0.2925d0, &
              0.2875d0,0.2790d0,0.2715d0,0.2640d0,0.2575d0,0.2515d0, &
              0.2435d0,0.8290d0,0.7840d0,0.7460d0,0.7230d0,0.6925d0, &
              0.6655d0,0.6470d0,0.6240d0,0.5980d0,0.5750d0,0.5540d0, &
              0.5340d0,0.5165d0,0.5000d0,0.4850d0/
    data (rad_pacha(i),i=87,102)/2.4470d0,2.0420d0,1.2740d0,1.1860d0, &
              0.5670d0,0.5360d0,0.5160d0,0.5040d0,0.4930d0,0.4735d0, &
              0.4595d0,0.4475d0,0.4355d0,0.4250d0,0.4150d0,0.4075d0/
  end module element
!
!  Bond charges
!
  module bondcharge
    use datatypes
    integer(i4),                               save :: maxbondQ = 1
    character(len=5), dimension(:,:), pointer, save :: symbolbondQ => null()
    integer(i4),                               save :: nbondQ
    integer(i4),      dimension(:),   pointer, save :: nbondQspec1 => null() 
    integer(i4),      dimension(:),   pointer, save :: nbondQspec2 => null()
    integer(i4),      dimension(:),   pointer, save :: nbondQtyp1 => null()
    integer(i4),      dimension(:),   pointer, save :: nbondQtyp2 => null()
    logical,                                   save :: lbondQ
    real(dp),         dimension(:),   pointer, save :: bondQincrement => null()
  end module bondcharge
!
!  Bond order potentials
!
  module bondorderdata
    use datatypes
    integer(i4),                         save :: maxnboA = 1
    integer(i4),                         save :: maxnboR = 1
    integer(i4),                         save :: maxnboQ = 1
    integer(i4),                         save :: maxnboQ0 = 1
    integer(i4),                         save :: maxnbopot = 1
    integer(i4),                         save :: nboA = 0
    integer(i4),                         save :: nboR = 0
    integer(i4),                         save :: nboQ = 0
    integer(i4),                         save :: nboQ0 = 0
    integer(i4),                         save :: nbopot = 0
    integer(i4),                         save :: nlibnboA = 0
    integer(i4),                         save :: nlibnboR = 0
    integer(i4),                         save :: nlibnboQ = 0
    integer(i4),                         save :: nlibnboQ0 = 0
    integer(i4),                         save :: nlibnbopot = 0
    integer(i4), dimension(:),  pointer, save :: nBOspec1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspec2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecA1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecA2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecR1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecR2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecQ0 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecQ1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOspecQ2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtaperQ => null()
    integer(i4), dimension(:),  pointer, save :: nBOtyp1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtyp2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypA1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypA2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypR1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypR2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypQ0 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypQ1 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypQ2 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypeA => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypeR => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypeQ => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypeQ0 => null()
    integer(i4), dimension(:),  pointer, save :: nBOtypeT => null()
    logical,     dimension(:),  pointer, save :: BOcombi => null()
    real(dp),    dimension(:),  pointer, save :: BOacoeff => null()
    real(dp),    dimension(:),  pointer, save :: BObcoeff => null()
    real(dp),    dimension(:),  pointer, save :: BOccoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOccoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOchiA => null()
    real(dp),    dimension(:),  pointer, save :: BOchiR => null()
    real(dp),    dimension(:),  pointer, save :: BOdcoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOdcoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOecoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOecoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOhcoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOhcoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOlcoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOlcoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOmcoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOmcoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOncoeffA => null()
    real(dp),    dimension(:),  pointer, save :: BOncoeffR => null()
    real(dp),    dimension(:),  pointer, save :: BOq0 => null()
    real(dp),    dimension(:),  pointer, save :: BOq0pot => null()
    real(dp),    dimension(:),  pointer, save :: BOq0ref => null()
    real(dp),    dimension(:),  pointer, save :: BOq0rho => null()
    real(dp),    dimension(:),  pointer, save :: BOzacoeff => null()
    real(dp),    dimension(:),  pointer, save :: BOzbcoeff => null()
    real(dp),    dimension(:),  pointer, save :: rBOmax => null()
    real(dp),    dimension(:),  pointer, save :: rBOmin => null()
    real(dp),    dimension(:),  pointer, save :: rBOmaxQ => null()
    real(dp),    dimension(:),  pointer, save :: rBOminQ => null()
  end module bondorderdata
!   
!  Bond list for pair of atoms
!   
  module bondvectors
    use datatypes
    integer(i4),                          save :: maxbondvec = 3
    integer(i4),                          save :: nbondvec
    integer(i4), dimension(:),   pointer, save :: nbondvecind => null()
    integer(i4), dimension(:),   pointer, save :: nbtypevec => null()
    integer(i4), dimension(:),   pointer, save :: nbtype2vec => null()
    logical,     dimension(:),   pointer, save :: lbondedvec => null()
    logical,     dimension(:),   pointer, save :: l2bondsvec => null()
    logical,     dimension(:),   pointer, save :: l3bondsvec => null()
  end module bondvectors
!
!  Brenner potential parameters
!
  module brennerdata
    use datatypes
    use element,    only : maxele
    integer(i4),         save :: nbrennertype = 3
    integer(i4),    parameter :: nREBOspecies = 4
    integer(i4),         save :: nREBOspecies2
    integer(i4),         save :: nat2REBOspecies(maxele)
    logical,             save :: lbrennersplinef
    logical,             save :: lbrennersplineh
    logical,             save :: lbP_CHdone(0:3,0:3,3)
    logical,             save :: lbPdone(0:3,0:3,nREBOspecies*(nREBOspecies+1)/2,nREBOspecies)
    logical,             save :: Fneeded(nREBOspecies*(nREBOspecies+1)/2)
    logical,             save :: Tneeded(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: balpha(nREBOspecies**3)
    real(dp),            save :: bdelta(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bexpco(nREBOspecies**3)
    real(dp),            save :: bF(0:7,0:3,0:3,1:9,nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bG(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bcos(8,2)
    real(dp),            save :: bGcos(8,nREBOspecies)
    real(dp),            save :: bGcosCspecial(8,2)
    real(dp),            save :: bLcos(8,4)
    real(dp),            save :: bP_CH(0:3,0:3,0:3,3)
    real(dp),            save :: bP_CHcoeff(4,4,0:3,0:3,3)
    real(dp),            save :: bP(0:3,0:3,0:3,nREBOspecies*(nREBOspecies+1)/2,nREBOspecies)
    real(dp),            save :: bPcoeff(4,4,0:3,0:3,nREBOspecies*(nREBOspecies+1)/2,nREBOspecies)
    real(dp),            save :: bR1(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bR2(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bR22(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bRe(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bTR1(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: bTR2(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: battB(3,nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: battBeta(3,nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: brepA(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: brepAlpha(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: brepQ(nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: cobicubic(16,0:9,0:9,nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: cotorcubic(64,0:3,0:3,0:2)
    real(dp),            save :: cotricubic(64,0:3,0:3,0:8,3)
    real(dp),            save :: pbicubic(16,nREBOspecies*(nREBOspecies+1)/2)
    real(dp),            save :: ptricubic(64,3)
  end module brennerdata
!
!  Cell multipole method data
!
  module cellmultipole
    use datatypes
    integer(i4), dimension(:), pointer, save :: nboxat => null()
    integer(i4),                        save :: icmm
    integer(i4),                        save :: nboxcmm
    integer(i4),                        save :: nboxx
    integer(i4),                        save :: nboxy
    integer(i4),                        save :: nboxz
    real(dp),                           save :: rbox
    real(dp),                           save :: xlength
    real(dp),                           save :: ylength
    real(dp),                           save :: zlength
    real(dp),                           save :: xboxo
    real(dp),                           save :: yboxo
    real(dp),                           save :: zboxo
  end module cellmultipole
!
!  Cell input flag
!
  module cellinputflag
    use datatypes
    logical,                            save :: lcelllasttime
  end module cellinputflag
!
!  Charge coupled potentials
!
  module chargecoupled
    use datatypes
    integer(i4),                         save :: maxCCspec = 10
    integer(i4),                         save :: nCCspec
    integer(i4), dimension(:),  pointer, save :: natCCspec => null()
    integer(i4), dimension(:),  pointer, save :: ntypCCspec => null()
    integer(i4), dimension(:),  pointer, save :: nCCparNb => null()
    real(dp),    dimension(:),  pointer, save :: CCbeta => null()
    real(dp),    dimension(:),  pointer, save :: CCeta => null()
    real(dp),    dimension(:),  pointer, save :: CClambda => null()
    real(dp),    dimension(:),  pointer, save :: CCmu => null()
    real(dp),    dimension(:),  pointer, save :: CCparA => null()
    real(dp),    dimension(:),  pointer, save :: CCparAE => null()
    real(dp),    dimension(:),  pointer, save :: CCparB => null()
    real(dp),    dimension(:),  pointer, save :: CCparC => null()
    real(dp),    dimension(:),  pointer, save :: CCparD => null()
    real(dp),    dimension(:),  pointer, save :: CCparDL => null()
    real(dp),    dimension(:),  pointer, save :: CCparDU => null()
    real(dp),    dimension(:),  pointer, save :: CCparH => null()
    real(dp),    dimension(:),  pointer, save :: CCparM => null()
    real(dp),    dimension(:),  pointer, save :: CCparN => null()
    real(dp),    dimension(:),  pointer, save :: CCparIE => null()
    real(dp),    dimension(:),  pointer, save :: CCparQL => null()
    real(dp),    dimension(:),  pointer, save :: CCparQU => null()
    real(dp),    dimension(:),  pointer, save :: CCvdwC => null()
    real(dp),    dimension(:),  pointer, save :: rCCmaxL => null()
    real(dp),    dimension(:),  pointer, save :: rCCmaxS => null()
    real(dp),    dimension(:),  pointer, save :: rCCminL => null()
    real(dp),    dimension(:),  pointer, save :: rCCminS => null()
  end module chargecoupled
!
!  Configurations
!
  module configurations
    use datatypes
    integer(i4),                              save :: maxatot = 0
    integer(i4),                              save :: maxcfg = 1
    integer(i4),                              save :: maxcontot = 1
    integer(i4),                              save :: maxregion = 1
    integer(i4),                              save :: maxvar = 1
    character(len=80), dimension(:), pointer, save :: names => null()
    integer(i4), dimension(:),     pointer, save :: ioptcfg => null()
    integer(i4), dimension(:),     pointer, save :: maxmodecfg => null()
    integer(i4), dimension(:),     pointer, save :: minmodecfg => null()
    integer(i4), dimension(:),     pointer, save :: n1con => null()
    integer(i4), dimension(:),     pointer, save :: n1var => null()
    integer(i4), dimension(:),     pointer, save :: ndimen => null()
    integer(i4), dimension(:),     pointer, save :: nascfg => null()
    integer(i4), dimension(:),     pointer, save :: natcfg => null()
    integer(i4), dimension(:),     pointer, save :: nbornstep => null()
    integer(i4), dimension(:),     pointer, save :: ncfixcfg => null()
    integer(i4), dimension(:),     pointer, save :: nconcfg => null()
    integer(i4), dimension(:),     pointer, save :: ncvarcfg => null()
    integer(i4), dimension(:),     pointer, save :: neiglow => null()
    integer(i4), dimension(:),     pointer, save :: neighigh => null()
    integer(i4), dimension(:),     pointer, save :: nomegastep => null()
    integer(i4), dimension(:),     pointer, save :: nregionno => null()
    integer(i4), dimension(:),     pointer, save :: nregions => null()
    integer(i4), dimension(:,:),   pointer, save :: nregiontype => null()
    integer(i4), dimension(:),     pointer, save :: nspecptrcfg => null()
    integer(i4), dimension(:),     pointer, save :: nsregion2 => null()
    integer(i4), dimension(:),     pointer, save :: nsuper => null()
    integer(i4), dimension(:),     pointer, save :: ntempstp => null()
    integer(i4), dimension(:),     pointer, save :: ntempstpstart => null()
    integer(i4), dimension(:),     pointer, save :: ntypcfg => null()
    integer(i4), dimension(:),     pointer, save :: nummodecfg => null()
    integer(i4), dimension(:),     pointer, save :: nvarcfg => null()
    integer(i4), dimension(:),     pointer, save :: nzmolcfg => null()
    integer(i4), dimension(:),     pointer, save :: omegadirtype => null()
    integer(i4), dimension(:),     pointer, save :: QMMMmode => null()
    logical,     dimension(:),     pointer, save :: lanisotropicpresscfg => null()
    logical,     dimension(:),     pointer, save :: lbsmat => null()
    logical,     dimension(:),     pointer, save :: leinsteinat => null()
    logical,     dimension(:),     pointer, save :: lfcborn => null()
    logical,     dimension(:),     pointer, save :: lfcphon => null()
    logical,     dimension(:),     pointer, save :: lfcprop => null()
    logical,     dimension(:),     pointer, save :: lfcscatter => null()
    logical,     dimension(:),     pointer, save :: lomega => null()
    logical,     dimension(:),     pointer, save :: lopfc => null()
    logical,     dimension(:),     pointer, save :: lopfi => null()
    logical,     dimension(:,:),   pointer, save :: lopfreg => null()
    logical,     dimension(:),     pointer, save :: lqmatom => null()
    logical,     dimension(:,:),   pointer, save :: lregionrigid => null()
    logical,     dimension(:),     pointer, save :: lsliceatom => null()
    logical,     dimension(:,:),   pointer, save :: ltdforcecfg => null()
    logical,     dimension(:),     pointer, save :: lvecin => null()
    real(dp),    dimension(:,:),   pointer, save :: anisotropicpresscfg => null()
    real(dp),    dimension(:,:),   pointer, save :: bornk => null()
    real(dp),    dimension(:),     pointer, save :: cncfg => null()
    real(dp),    dimension(:),     pointer, save :: conaddcfg => null()
    real(dp),    dimension(:),     pointer, save :: concocfg => null()
    real(dp),    dimension(:),     pointer, save :: dhklcfg => null()
    real(dp),    dimension(:),     pointer, save :: energycfg => null()
    real(dp),    dimension(:,:),   pointer, save :: forcecfg => null()
    real(dp),    dimension(:),     pointer, save :: keinsteinat => null()
    real(dp),    dimension(:),     pointer, save :: occucfg => null()
    real(dp),    dimension(:),     pointer, save :: omega => null()
    real(dp),    dimension(:),     pointer, save :: omegadamping => null()
    real(dp),    dimension(:,:),   pointer, save :: omegadir => null()
    real(dp),    dimension(:),     pointer, save :: omegastep => null()
    real(dp),    dimension(:),     pointer, save :: oxcfg => null()
    real(dp),    dimension(:),     pointer, save :: qlcfg => null()
    real(dp),    dimension(:),     pointer, save :: presscfg => null()
    real(dp),    dimension(:),     pointer, save :: radcfg => null()
    real(dp),    dimension(:,:,:), pointer, save :: rvcfg => null()                   ! Cell vectors for configurations
    real(dp),    dimension(:),     pointer, save :: sbulkecfg => null()
    real(dp),    dimension(:,:),   pointer, save :: stresscfg => null()
    real(dp),    dimension(:,:,:), pointer, save :: tdforcecfg => null()
    real(dp),    dimension(:),     pointer, save :: tempcfg => null()
    real(dp),    dimension(:),     pointer, save :: tempstp => null()
    real(dp),    dimension(:),     pointer, save :: totalchargecfg => null()
    real(dp),    dimension(:),     pointer, save :: xcfg => null()
    real(dp),    dimension(:),     pointer, save :: ycfg => null()
    real(dp),    dimension(:),     pointer, save :: zcfg => null()
    real(dp),    dimension(:),     pointer, save :: xeinsteinat => null()
    real(dp),    dimension(:),     pointer, save :: yeinsteinat => null()
    real(dp),    dimension(:),     pointer, save :: zeinsteinat => null()
    integer(i4),                            save :: nasum
    integer(i4),                            save :: ncfg
    integer(i4),                            save :: ncontot
    integer(i4),                            save :: nconin
  end module configurations
!
!  Fundamental constants and conversion factors
!
  module constants
    use datatypes
!***********************
!  Conversion factors  *
!***********************
!
!  Atomic units to angstroms
!
!  Old value for backwards compatability 
    real(dp), save :: autoangs = 0.529177_dp
!  Use this value for more accuracy
!    real(dp), save :: autoangs = 0.529177249_dp
!
!  Atomic units to eV
!
    real(dp), save :: autoev = 27.211654_dp
!
!  Inverse angstroms to eV
!
!  Old value for backwards compatability 
    real(dp), save :: angstoev = 14.3997584_dp
!  Use this value for more accuracy
!    real(dp), save :: angstoev = autoev*autoangs
!
!  Kcal -> eV
!
!  Old value for backward compatibility
!    real(dp), save :: kcaltoev = 4.3393453d-2
!  New value
    real(dp), save :: kcaltoev = 4.3364432032d-2
!
!  KJmol-1 -> eV
!
    real(dp), save :: kjmtoev = 1.0364348d-2
!
!  Radians to degrees
!
    real(dp), parameter :: radtodeg = 57.29577951_dp
!
!  Degrees to radians
!
    real(dp), parameter :: degtorad = 1/radtodeg
!
!  eV -> Joules
!
    real(dp), save :: evtoj = 1.6021917d-19
!
!  eV -> Kcal
!
    real(dp), save :: evtokcal = 23.0604_dp
!   
!  cm-1 -> rad/s, THz -> rad/s, meV -> rad/s
!   
    real(dp), save :: cmtorads,thztorad,mevtorad,mevtot !done in initial
!**************
!  Constants  *
!**************
!
!  Avogadros number
!
    real(dp), parameter :: avogadro = 6.022045d23
!
!  Boltzmanns constant (J/K)
!
    real(dp), parameter :: boltz = 1.38066244d-23
!
!  Pi - now derived using 4.0*atan(1.0)
!
!        real(dp), parameter :: pi = 3.14159265359_dp
!
!  Planck's constant (Js)
!
    real(dp), parameter :: planck = 6.62617636d-34
!
!  Speed of light (in cm/s)
!
    real(dp), parameter :: speedl = 2.99792458d10
!
!  Neutron Mass (in Kg) (ers29)
!
    real(dp), parameter :: neutronmass = 1.674927211d-27
!
!  Storage for derived constants
!
    real(dp),      save :: pi
    real(dp),      save :: sqrtpi
!
!  Hbar - Planck's constant / 2*pi in (Js)
!
    real(dp),      save :: hbar_js 
!
!  Hbar (in A^2 Kg /s) (ers29)
!
    real(dp),      save :: hbar_A2kgpers
  end module constants
!
!  Control
!
  module control
    use datatypes
    character(len=400),               save :: keyword
    logical,                          save :: lallowgt1            ! If true, sum of partial occupancies for a site can exceed one
    logical,                          save :: langle               ! If true, output angles for valid 3-body potentials
    logical,                          save :: lanneal
    logical,                          save :: latomicstress        ! If true then output atomic stresses at final geometry
    logical,                          save :: laver                ! If true, print average bond length
    logical,                          save :: lbond                ! If true, print out bonds for atoms
    logical,                          save :: lborn
    logical,                          save :: lbrenner             ! If true, use Brenner's REBO model
    logical,                          save :: lbroad
    logical,                          save :: lbulknoopt           ! If true, do not optimise the bulk structure
    logical,                          save :: lc6                  ! If true, use Ewald-style sum to compute dispersion sums
    logical,                          save :: lcello               ! Cell only optimisation if true
    logical,                          save :: lcomp                ! If true, print out comparison of initial and final structures
    logical,                          save :: lconj                ! If true, use conjugate gradients for optimisation
    logical,                          save :: lconp                ! Constant pressure optimisation if true
    logical,                          save :: lconv                ! Constant volume optimisation if true
    logical,                          save :: lcosmo               ! If true, include a COSMO continuum solvation model
    logical,                          save :: ldcharge
    logical,                          save :: ldebug               ! If true, turn on debug printing
    logical,                          save :: ldefect              ! If true, perform a Mott-Littleton defect calculation
    logical,                          save :: ldfp                 ! If true, use Davidon-Fletcher-Powell updating of Hessian
    logical,                          save :: lDoElectrostatics    ! If true, then compute electrostatics
    logical,                          save :: lDoQDeriv1
    logical,                          save :: lDoQDeriv2
    logical,                          save :: ldsym
    logical,                          save :: ld1sym               ! If true, use symmetry for first derivatives
    logical,                          save :: ld2sym               ! If true, use symmetry for second derivatives
    logical,                          save :: leckart              ! Remove translations and rotations from vibrations of molecules
    logical,                          save :: lEDIP                ! Indicates that an EDIP force field is to be used
    logical,                          save :: leprint
    logical,                          save :: leregion             ! If true, then print out region-region energies
    logical,                          save :: ldipole
    logical,                          save :: ldist                ! If true, compute distances out to cutd
    logical,                          save :: leem                 ! If true, determine charges by EEM
    logical,                          save :: lefg                 ! Compute electric field gradient tensor if true
    logical,                          save :: leigen               ! If true, print out the phonon eigenvectors
    logical,                          save :: lfbfgs
    logical,                          save :: lfirst
    logical,                          save :: lfit                 ! Perform a fitting run if true
    logical,                          save :: lflags               ! If true, control optimisation variables using flags
    logical,                          save :: lforcemin            ! If true, then minimise force norm rather than energy
    logical,                          save :: lfree                ! Perform a free energy calculation
    logical,                          save :: lfreq
    logical,                          save :: lfreqout
    logical,                          save :: lga
    logical,                          save :: lgrad                ! Compute gradients if true
    logical,                          save :: lhex
    logical,                          save :: lHideShells          ! If true then hide the shells in the output
    logical,                          save :: linten               ! Compute vibrational intensities if true
    logical,                          save :: lkfull
    logical,                          save :: llbfgs
    logical,                          save :: lmadelung            ! Use a Madelung correction to the electrostatic energy
    logical,                          save :: lmarvreg2
    logical,                          save :: lmc                  ! Perform Monte Carlo calculation if true
    logical,                          save :: lmeanke
    logical,                          save :: lminimage            ! Use minimum image convention if true
    logical,                          save :: lmodco               ! Mod fractional coordinates to keep in central cell
    logical,                          save :: lneb                 ! If true, perform nudged elastic band calculation
    logical,                          save :: lnoanald1            ! If true, analytic first derivatives are not available
    logical,                          save :: lnoanald2            ! If true, analytic second derivatives are not available
    logical,                          save :: lnoanald3            ! If true, analytic third derivatives are not available
    logical,                          save :: lnomottlittleton
    logical,                          save :: lnoenergy
    logical,                          save :: lnoflags             ! If true, don't attempt to read flags from input file
    logical,                          save :: lnoreal              ! If true, don't calculate real space electrostatics
    logical,                          save :: lnorecip             ! If true, don't calculate reciprocal space electrostatics
    logical,                          save :: lnumdiag             ! If true, use numerical on-diagonal Hessian matrix elements
    logical,                          save :: loldinten            ! Use old formula to compute vibrational intensities 
    logical,                          save :: lopprt
    logical,                          save :: lopt
    logical,                          save :: lphon                ! Compute phonons if true
    logical,                          save :: lposidef
    logical,                          save :: lpot
    logical,                          save :: lpredict
    logical,                          save :: lpreserveQ
    logical,                          save :: lprop                ! Compute properties if true
    logical,                          save :: lqbond
    logical,                          save :: lqtpie               ! If true, determine charges by QTPIE charge algorithm
    logical,                          save :: lquicksearch
    logical,                          save :: lreaxFF              ! Indicates that a reactive force field (ReaxFF) is to be used
    logical,                          save :: lreaxFFQ             ! Indicates that charges are to be used in ReaxFF
    logical,                          save :: lrelax               ! Use relax fitting algorithm if true
    logical,                          save :: lregionforce         ! Print out forces on regions
    logical,                          save :: lrest
    logical,                          save :: lrfo                 ! Use RFO method for optimisation if true
    logical,                          save :: lsave
    logical,                          save :: lscatter             ! Perform INS calculation on final configuration
    logical,                          save :: lseok
    logical,                          save :: lshello              ! Shell only optimisation if true
    logical,                          save :: lsiteenergy          ! Output site energies
    logical,                          save :: lspatial             ! Spatial (domain) decomposition algorithm if true
    logical,                          save :: lstaticfirst
    logical,                          save :: lstressout           ! If true then output stresses at final geometry
    logical,                          save :: lthermal             ! Compute thermal conductivity if true
    logical,                          save :: ltors                ! Print out torsions corresponding to valid interactions
    logical,                          save :: ltran
    logical,                          save :: lunit                ! Use a unit initial Hessian if true
    logical,                          save :: lwolf                ! If true use the potential consistent Wolf sum
    logical,                          save :: lwolffennell         ! If true use the force consistent Wolf sum
    logical,                          save :: lwolforiginal        ! If true then use the original form of the Wolf sum
    logical,                          save :: lx0centroid          ! Use rigid molecules and only move centroid if true
    logical,                          save :: lzsisa               ! Use ZSISA approximation for free energy minimisation
  end module control
!       
!  Cosmo solvation model                  
!       
  module cosmo                        
    use datatypes                     
    integer(i4),                            save :: maxallnearseg = 1
    integer(i4),                            save :: maxnearseg = 1
    integer(i4),                            save :: maxnppa = 1
    integer(i4),                            save :: maxnpts = 1
    integer(i4),                            save :: maxnpts2 = 1
    integer(i4),                            save :: maxnptsh = 1
    integer(i4),                            save :: maxnptstot = 1
    integer(i4),                            save :: maxnpwt = 4
    integer(i4),                            save :: maxnset = 1
    integer(i4),                            save :: maxsasparticles = 1
    integer(i4),                            save :: maxsasparticlespart = 1
    integer(i4), dimension(:),     pointer, save :: cosmoatomptr => null()
    integer(i4), dimension(:),     pointer, save :: nar => null()
    integer(i4), dimension(:),     pointer, save :: nallnearsegptr => null()
    integer(i4), dimension(:),     pointer, save :: nallnearsegrptr => null()
    integer(i4), dimension(:),     pointer, save :: nnearseg => null()
    integer(i4), dimension(:,:),   pointer, save :: nnearsegptr => null()
    integer(i4), dimension(:,:),   pointer, save :: nnearsegptrcell => null()
    integer(i4), dimension(:),     pointer, save :: npwt => null()
    integer(i4), dimension(:,:),   pointer, save :: npwtptr => null()
    integer(i4), dimension(:),     pointer, save :: nsasexcludemax => null()
    integer(i4), dimension(:),     pointer, save :: nsasexcludemin => null()
    integer(i4), dimension(:),     pointer, save :: nsasparticleptr => null()
    integer(i4), dimension(:),     pointer, save :: nsasparticlepartptr => null()
    integer(i4), dimension(:),     pointer, save :: nset => null()
    integer(i4), dimension(:),     pointer, save :: nsetf => null()
    logical,     dimension(:),     pointer, save :: lcosmoeigin => null()
    real(dp),    dimension(:),     pointer, save :: atsrad => null()
    real(dp),    dimension(:),     pointer, save :: cosmoA => null()
    real(dp),    dimension(:),     pointer, save :: cosmoBq => null()
    real(dp),    dimension(:,:,:), pointer, save :: cosmoeigen => null()
    real(dp),    dimension(:),     pointer, save :: cosmoepsilon => null()
    real(dp),    dimension(:),     pointer, save :: cosmodrsolv => null()
    real(dp),    dimension(:),     pointer, save :: cosmorsolv => null()
    real(dp),    dimension(:,:,:), pointer, save :: cosmotm => null()
    real(dp),    dimension(:),     pointer, save :: cosmopwt => null()
    real(dp),    dimension(:,:),   pointer, save :: cosmowt => null()
    real(dp),    dimension(:),     pointer, save :: qonsas => null()
    real(dp),    dimension(:),     pointer, save :: qsasparticles => null()
    real(dp),    dimension(:,:),   pointer, save :: sas => null()
    real(dp),    dimension(:),     pointer, save :: segweight => null()
    real(dp),    dimension(:,:),   pointer, save :: sphere1h => null()
    real(dp),    dimension(:,:),   pointer, save :: sphere1 => null()
    real(dp),    dimension(:,:),   pointer, save :: sphere2 => null()
    real(dp),    dimension(:,:),   pointer, save :: spxyz => null()
    real(dp),    dimension(:,:),   pointer, save :: spxyzouter => null()
    integer(i4),                            save :: isasatomoption = 1
    integer(i4),                            save :: nallnearseg = 0
    integer(i4),                            save :: nppa = 110
    integer(i4),                            save :: npts = 0
    integer(i4),                            save :: nptsh = 0
    integer(i4),                            save :: nsasparticles
    integer(i4),                            save :: nsasparticlespart
    integer(i4),                            save :: nspa = 110
    integer(i4),                            save :: nspah = 110
    logical,                                save :: ldodeca = .false.
    logical,                                save :: lcosmic
    logical,                                save :: lsegsmooth = .false.
    real(dp),                               save :: cosmofneps 
    real(dp),                               save :: cosmorange = 0.0_dp
    real(dp),                               save :: cosmormax = 10.0_dp
    real(dp),                               save :: cosmormaxs = 1.0_dp
    real(dp),                               save :: cosmormax2
    real(dp),                               save :: cosmormax2s
    real(dp),                               save :: deltaq
    real(dp),                               save :: drsolv
    real(dp),                               save :: qonsastot
    real(dp),                               save :: rsolv
    real(dp),                               save :: totsegweight
  end module cosmo
!  
!  Locally used pointer data for Cosmo solvation model
!     
  module cosmopwtloc
    use datatypes                           
    integer(i4),                            save :: maxnpwtloc = 1
    integer(i4), dimension(:,:),   pointer, save :: npwtloc => null()
    integer(i4), dimension(:,:,:), pointer, save :: npwtptrloc => null()
  end module cosmopwtloc
!
!  Cost function
!
  module costfunction
    use datatypes
    real(dp),                         save :: kacf = 1.0_dp
    real(dp),                         save :: kbcf = 1.0_dp
    real(dp),                         save :: kcccf = 0.1_dp
    real(dp),                         save :: kcacf = 0.2_dp
    real(dp),                         save :: kqccf = 1.0_dp
    real(dp),                         save :: kqacf = 1.0_dp
    real(dp),                         save :: kscf = 0.0_dp
  end module costfunction
!
!  Current configuration
!
  module current
    use datatypes
    integer(i4),                            save :: maxat = 0
    integer(i4),                            save :: maxbond = 12
    integer(i4),                            save :: maxcon = 1
    integer(i4),                            save :: maxdis = 1000
    integer(i4),                            save :: maxmdis = 1
    integer(i4), dimension(:),     pointer, save :: nbonds => null()
    integer(i4), dimension(:,:),   pointer, save :: nbonded => null()
    integer(i4), dimension(:,:,:), pointer, save :: nbondedtype => null()
    integer(i4), dimension(:,:),   pointer, save :: nbondind => null()
    integer(i4), dimension(:),     pointer, save :: icosx => null()
    integer(i4), dimension(:),     pointer, save :: icosy => null()
    integer(i4), dimension(:),     pointer, save :: icosz => null()
    integer(i4), dimension(:),     pointer, save :: iopt => null()
    integer(i4), dimension(:),     pointer, save :: iatn => null()
    integer(i4), dimension(:),     pointer, save :: nat => null()
    integer(i4), dimension(:),     pointer, save :: natype => null()
    integer(i4), dimension(:),     pointer, save :: nftype => null()
    integer(i4), dimension(:),     pointer, save :: ncfix => null()
    integer(i4), dimension(:),     pointer, save :: ncvar => null()
    integer(i4), dimension(:),     pointer, save :: neamfnspecptr => null()
    integer(i4), dimension(:),     pointer, save :: neamspecptr => null()
    integer(i4), dimension(:),     pointer, save :: neemptr => null()
    integer(i4), dimension(:),     pointer, save :: neqv => null()
    integer(i4), dimension(:),     pointer, save :: nrel2 => null()
    integer(i4), dimension(:),     pointer, save :: nrelat => null()
    integer(i4), dimension(:),     pointer, save :: nrotop => null()
    integer(i4), dimension(:),     pointer, save :: nspecptr => null()
    real(dp),    dimension(:,:,:), pointer, save :: bornq => null()
    real(dp),    dimension(:),     pointer, save :: c6a => null()
    real(dp),    dimension(:),     pointer, save :: c6f => null()
    real(dp),    dimension(:),     pointer, save :: cna => null()
    real(dp),    dimension(:),     pointer, save :: cnf => null()
    real(dp),    dimension(:),     pointer, save :: conadd => null()
    real(dp),    dimension(:),     pointer, save :: conco => null()
    real(dp),    dimension(:),     pointer, save :: mass => null()
    real(dp),    dimension(:),     pointer, save :: occua => null()
    real(dp),    dimension(:),     pointer, save :: occuf => null()
    real(dp),    dimension(:),     pointer, save :: oxa => null()
    real(dp),    dimension(:),     pointer, save :: oxf => null()
    real(dp),    dimension(:),     pointer, save :: qa => null()
    real(dp),    dimension(:),     pointer, save :: qf => null()
    real(dp),    dimension(:),     pointer, save :: rada => null()
    real(dp),    dimension(:),     pointer, save :: radf => null()
    real(dp),    dimension(:),     pointer, save :: rmass => null()
    real(dp),    dimension(:),     pointer, save :: xalat => null()
    real(dp),    dimension(:),     pointer, save :: yalat => null()
    real(dp),    dimension(:),     pointer, save :: zalat => null()
    real(dp),    dimension(:),     pointer, save :: xclat => null()
    real(dp),    dimension(:),     pointer, save :: yclat => null()
    real(dp),    dimension(:),     pointer, save :: zclat => null()
    real(dp),    dimension(:),     pointer, save :: xafrac => null()
    real(dp),    dimension(:),     pointer, save :: yafrac => null()
    real(dp),    dimension(:),     pointer, save :: zafrac => null()
    real(dp),    dimension(:),     pointer, save :: xfrac => null()
    real(dp),    dimension(:),     pointer, save :: yfrac => null()
    real(dp),    dimension(:),     pointer, save :: zfrac => null()
    real(dp),    dimension(:),     pointer, save :: xfracimage => null()
    real(dp),    dimension(:),     pointer, save :: yfracimage => null()
    real(dp),    dimension(:),     pointer, save :: zfracimage => null()
    real(dp),    dimension(:),     pointer, save :: xinitial => null()
    real(dp),    dimension(:),     pointer, save :: yinitial => null()
    real(dp),    dimension(:),     pointer, save :: zinitial => null()
    real(dp),    dimension(:),     pointer, save :: xstore => null()
    real(dp),    dimension(:),     pointer, save :: ystore => null()
    real(dp),    dimension(:),     pointer, save :: zstore => null()
    real(dp),    dimension(:),     pointer, save :: rstore => null()
    real(dp),    dimension(:),     pointer, save :: x0 => null()
    integer(i4),                            save :: icfhr
    integer(i4),                            save :: ictype
    integer(i4),                            save :: iimax
    integer(i4),                            save :: iimax2
    integer(i4),                            save :: iimid
    integer(i4),                            save :: iimid2
    integer(i4),                            save :: imaxl
    integer(i4),                            save :: jmaxl
    integer(i4),                            save :: kmaxl
    integer(i4),                            save :: imaxl2
    integer(i4),                            save :: jmaxl2
    integer(i4),                            save :: kmaxl2
    integer(i4),                            save :: maxmode
    integer(i4),                            save :: minmode
    integer(i4),                            save :: nasym
    integer(i4),                            save :: nbsmat
    integer(i4),                            save :: nbsm
    integer(i4),                            save :: ncbl
    integer(i4),                            save :: ncell
    integer(i4),                            save :: ncf
    integer(i4),                            save :: ncfmdrun
    integer(i4),                            save :: ncfst
    integer(i4),                            save :: ncon
    integer(i4),                            save :: ndim
    integer(i4),                            save :: neem
    integer(i4),                            save :: nfcf
    integer(i4),                            save :: nfst
    integer(i4),                            save :: nsft
    integer(i4),                            save :: nstrains
    integer(i4),                            save :: nstrptr(6)
    integer(i4),                            save :: ntemperaturestep
    integer(i4),                            save :: ntemperaturestepstart
    integer(i4),                            save :: numat
    integer(i4),                            save :: nummode
    integer(i4),                            save :: nvar
    integer(i4),                            save :: nzmol
    logical,                                save :: lanisotropicpress
    logical,                                save :: lc6one
    logical,                                save :: leinstein
    logical,                                save :: lewald
    real(dp),                               save :: anisotropicpress(6)
    real(dp),                               save :: density
    real(dp),                               save :: press
    real(dp),                               save :: temperature
    real(dp),                               save :: temperaturestep
    real(dp),                               save :: totalcharge
    real(dp),                               save :: kv(3,3)
    real(dp),                               save :: rv(3,3)
    real(dp),                               save :: a
    real(dp),                               save :: b
    real(dp),                               save :: c
    real(dp),                               save :: alpha
    real(dp),                               save :: beta
    real(dp),                               save :: gamma
    real(dp),                               save :: recipa
    real(dp),                               save :: recipb
    real(dp),                               save :: recipc
    real(dp),                               save :: refvolume
    real(dp),                               save :: r1x
    real(dp),                               save :: r1y
    real(dp),                               save :: r1z
    real(dp),                               save :: r2x
    real(dp),                               save :: r2y
    real(dp),                               save :: r2z
    real(dp),                               save :: r3x
    real(dp),                               save :: r3y
    real(dp),                               save :: r3z
    real(dp),                               save :: stress(6)
    real(dp),                               save :: xvec1cell(27)
    real(dp),                               save :: yvec1cell(27)
    real(dp),                               save :: zvec1cell(27)
    real(dp),                               save :: xvec2cell(125)
    real(dp),                               save :: yvec2cell(125)
    real(dp),                               save :: zvec2cell(125)
  end module current
!
!  Defects
!
  module defects
    use datatypes
    integer(i4),                          save :: maxdcon = 0
    integer(i4),                          save :: maxdef = 10
    integer(i4),                          save :: maxr1at = 1
    integer(i4),                          save :: maxvacint = 1
    integer(i4), dimension(:),   pointer, save :: nbondsdef => null()
    integer(i4), dimension(:,:), pointer, save :: nbondeddef => null()
    integer(i4), dimension(:),   pointer, save :: nreldef => null()
    integer(i4), dimension(:),   pointer, save :: idopt => null()
    integer(i4), dimension(:),   pointer, save :: inddeffix => null()
    integer(i4), dimension(:),   pointer, save :: inddfix => null()
    integer(i4), dimension(:),   pointer, save :: natdefe => null()
    integer(i4), dimension(:),   pointer, save :: natp => null()
    integer(i4), dimension(:),   pointer, save :: ntypdefe => null()
    integer(i4), dimension(:),   pointer, save :: ntypep => null()
    integer(i4), dimension(:),   pointer, save :: ncdfix => null()
    integer(i4), dimension(:),   pointer, save :: ncdvar => null()
    integer(i4), dimension(:),   pointer, save :: ndcentyp => null()
    integer(i4), dimension(:),   pointer, save :: ndefcfg => null()
    integer(i4), dimension(:),   pointer, save :: ndefind => null()
    integer(i4), dimension(:),   pointer, save :: ndefindp => null()
    integer(i4), dimension(:),   pointer, save :: ndefmol => null()
    integer(i4), dimension(:),   pointer, save :: ndefmolp => null()
    integer(i4), dimension(:),   pointer, save :: ndefnat => null()
    integer(i4), dimension(:),   pointer, save :: ndeftp => null()
    integer(i4), dimension(:),   pointer, save :: ndeftyp => null()
    integer(i4), dimension(:),   pointer, save :: ndeqv => null()
    integer(i4), dimension(:),   pointer, save :: ndptr => null()
    integer(i4), dimension(:),   pointer, save :: ndrel => null()
    integer(i4), dimension(:),   pointer, save :: ndrelop => null()
    integer(i4), dimension(:),   pointer, save :: ndsptr => null()
    integer(i4), dimension(:),   pointer, save :: npsite => null()
    integer(i4), dimension(:),   pointer, save :: nptrr1 => null()
    logical,     dimension(:),   pointer, save :: ldefbsmat => null()
    logical,     dimension(:),   pointer, save :: ldeffix => null()
    logical,     dimension(:),   pointer, save :: ldeflin => null()
    logical,     dimension(:),   pointer, save :: ldfix => null()
    logical,     dimension(:),   pointer, save :: ldqmatom => null()
    logical,     dimension(:),   pointer, save :: lreldin => null()
    logical,     dimension(:),   pointer, save :: lr1created => null()
    real(dp),    dimension(:),   pointer, save :: dconco => null()
    real(dp),    dimension(:,:), pointer, save :: dscrho => null()
    real(dp),    dimension(:),   pointer, save :: occdefe => null()
    real(dp),    dimension(:),   pointer, save :: occp => null()
    real(dp),    dimension(:),   pointer, save :: qdefe => null()
    real(dp),    dimension(:),   pointer, save :: qp => null()
    real(dp),    dimension(:),   pointer, save :: radefe => null()
    real(dp),    dimension(:),   pointer, save :: reg1 => null()
    real(dp),    dimension(:),   pointer, save :: reg1last => null()
    real(dp),    dimension(:),   pointer, save :: reg2 => null()
    real(dp),    dimension(:),   pointer, save :: reg2a1 => null()
    real(dp),    dimension(:),   pointer, save :: xdefe => null()
    real(dp),    dimension(:),   pointer, save :: ydefe => null()
    real(dp),    dimension(:),   pointer, save :: zdefe => null()
    real(dp),    dimension(:),   pointer, save :: xperf => null()
    real(dp),    dimension(:),   pointer, save :: yperf => null()
    real(dp),    dimension(:),   pointer, save :: zperf => null()
    real(dp),    dimension(:),   pointer, save :: xdef => null()
    real(dp),    dimension(:),   pointer, save :: ydef => null()
    real(dp),    dimension(:),   pointer, save :: zdef => null()
    real(dp),    dimension(:),   pointer, save :: xdcent => null()
    real(dp),    dimension(:),   pointer, save :: ydcent => null()
    real(dp),    dimension(:),   pointer, save :: zdcent => null()
    integer(i4),                          save :: mode2a
    integer(i4),                          save :: ncoreg1
    integer(i4),                          save :: ndasym
    integer(i4),                          save :: nshreg1
    integer(i4),                          save :: ndcon
    integer(i4),                          save :: ndef
    integer(i4),                          save :: nreg1
    integer(i4),                          save :: nreg1old
    integer(i4),                          save :: nreg2
    integer(i4),                          save :: npreg2
    integer(i4),                          save :: ntreg2
    integer(i4),                          save :: ninte = 0
    integer(i4),                          save :: nvaca = 0
    logical,                              save :: ldbsm
    logical,                              save :: ldcellr
    logical,                              save :: lmodeset
    logical,                              save :: lr2f
    real(dp),                             save :: dsymop(3,3,48)
    real(dp),                             save :: qdef
    real(dp),                             save :: r2dmax
    real(dp),                             save :: xdc
    real(dp),                             save :: ydc
    real(dp),                             save :: zdc
  end module defects
!
!  Derivatives
!
  module derivatives
    use datatypes
    integer(i4),                            save :: maxd1 = 0
    integer(i4),                            save :: maxd2 = 0
    integer(i4),                            save :: maxd2q = 0
    integer(i4),                            save :: maxd2qu = 0
    integer(i4),                            save :: maxd2u = 0
    integer(i4),                            save :: maxqatoms = 0
    integer(i4),                            save :: maxqatoms2 = 0
    integer(i4), dimension(:),     pointer, save :: nqatoms => null()
    integer(i4), dimension(:,:),   pointer, save :: nqatomptr => null()
    real(dp),    dimension(:,:),   pointer, save :: atomicstress => null()           ! Atomic stresses
    real(dp),    dimension(:,:),   pointer, save :: sumatomicstress => null()        ! Sum of atomic stresses during MD run
    real(dp),                               save :: cderv(6,6)                       ! Second derivatives - cell-parameter - cell-parameter
    real(dp),                               save :: cderv2(6,6)
    real(dp),    dimension(:,:),   pointer, save :: derv2 => null()                  ! Second derivatives - coordinate-coordinate
    real(dp),    dimension(:),     pointer, save :: derv2d=> null()                  ! Diagonal of second derivative array
    real(dp),    dimension(:,:),   pointer, save :: dervi=> null()
    real(dp),    dimension(:,:),   pointer, save :: derv3 => null()                  ! Second derivatives - strain-coordinate (radial forces/total)
    real(dp),    dimension(:,:),   pointer, save :: diagblock=> null()               ! On-diagonal block of second derivatives
    real(dp),    dimension(:,:),   pointer, save :: dqds=> null()                    ! First derivatives of charge w.r.t. strain
    real(dp),    dimension(:,:),   pointer, save :: d2qds2 => null()
    real(dp),    dimension(:,:),   pointer, save :: dqdxyz=> null()                  ! First derivatives of charge w.r.t. coordinates
    real(dp),    dimension(:,:),   pointer, save :: d2qdxyz2 => null()
    real(dp),    dimension(:,:,:), pointer, save :: d2qdxyzs=> null()
    real(dp),    dimension(:),     pointer, save :: raderv=> null()                  ! First derivatives - radii
    real(dp),    dimension(:),     pointer, save :: xdrv=> null()                    ! First derivatives - x coordinate (radial/total)
    real(dp),    dimension(:),     pointer, save :: ydrv=> null()                    ! First derivatives - y coordinate (radial/total)
    real(dp),    dimension(:),     pointer, save :: zdrv=> null()                    ! First derivatives - z coordinate (radial/total)
    real(dp),    dimension(:),     pointer, save :: xdrvnr=> null()                  ! First derivatives - x coordinate (non-radial component)
    real(dp),    dimension(:),     pointer, save :: ydrvnr=> null()                  ! First derivatives - y coordinate (non-radial component)
    real(dp),    dimension(:),     pointer, save :: zdrvnr=> null()                  ! First derivatives - z coordinate (non-radial component)
    real(dp),    dimension(:),     pointer, save :: xregdrv=> null()                 ! First derivatives - x coordinate (total for region)
    real(dp),    dimension(:),     pointer, save :: yregdrv=> null()                 ! First derivatives - y coordinate (total for region)
    real(dp),    dimension(:),     pointer, save :: zregdrv=> null()                 ! First derivatives - z coordinate (total for region)
    real(dp),                               save :: sderv2(6,6)                      ! Second derivatives - strain-strain (total)
    real(dp),                               save :: sdrv2(6,6)                       ! Second derivatives - strain-strain
    real(dp),                               save :: rstrd(6)                         ! First derivatives - strain (real space/radial)
    real(dp),                               save :: rstrdnr(6)                       ! First derivatives - strain (real space/non-radial)
    real(dp),                               save :: strderv(6)                       ! First derivatives - strain (reciprocal space/total)
    real(dp),                               save :: stresses(6)                      ! Used to store full strderv values before symmetry reduction
    real(dp),                               save :: virial                           ! Virial for MD
  end module derivatives
!
!  Interatomic distances
!
  module distances
    use datatypes
    logical,                              save :: lStoreVectors = .false.
    integer(i4),                          save :: maxndistancetotal = 1
    integer(i4),                          save :: ndistancetotal = 1
    integer(i4), dimension(:),   pointer, save :: icosxs => null()
    integer(i4), dimension(:),   pointer, save :: icosys => null()
    integer(i4), dimension(:),   pointer, save :: icoszs => null()
    integer(i4), dimension(:),   pointer, save :: ndistance => null()
    integer(i4), dimension(:,:), pointer, save :: ndistancecell => null()
    integer(i4), dimension(:,:), pointer, save :: ndistanceij => null()
    integer(i4), dimension(:),   pointer, save :: ndistanceind => null()
    integer(i4), dimension(:,:), pointer, save :: ndistancemolonly => null()
    integer(i4), dimension(:),   pointer, save :: ndistanceptr => null()
    integer(i4), dimension(:),   pointer, save :: ndistancereset => null()
    integer(i4), dimension(:),   pointer, save :: ndistbotype => null()
    integer(i4), dimension(:),   pointer, save :: ndistbotype2 => null()
    logical,     dimension(:),   pointer, save :: distl1bond => null()
    logical,     dimension(:),   pointer, save :: distl2bond => null()
    logical,     dimension(:),   pointer, save :: distl3bond => null()
    logical,     dimension(:),   pointer, save :: distlptrmol => null()
    logical,     dimension(:,:), pointer, save :: distlself => null()
    real(dp),                             save :: extracutoff
    real(dp),    dimension(:),   pointer, save :: distance => null()
    real(dp),    dimension(:),   pointer, save :: distance2 => null()
    real(dp),    dimension(:,:), pointer, save :: distancexyz => null()
  end module distances
!
!  Phonon dispersion
!
  module dispersion
    use datatypes
    integer(i4), dimension(:),   pointer, save :: ndde => null()
    integer(i4), dimension(:),   pointer, save :: ndds => null()
    integer(i4), dimension(:),   pointer, save :: ndispcfg => null()
    integer(i4), dimension(:),   pointer, save :: ndstart => null()
    integer(i4), dimension(:),   pointer, save :: ndend => null()
    real(dp),    dimension(:),   pointer, save :: xdisp => null()
    real(dp),    dimension(:),   pointer, save :: ydisp => null()
    real(dp),    dimension(:),   pointer, save :: zdisp => null()
    integer(i4),                          save :: ndispres
    integer(i4),                          save :: ndline
    integer(i4),                          save :: ndpoint
  end module dispersion
!
!  Dump file information
!
  module dump
    use datatypes
    character(len=80),                    save :: dfile
    character(len=80),                    save :: mdafil
    integer(i4),                          save :: idump
    integer(i4),                          save :: ncycd
    integer(i4),                          save :: ndumpstep
    logical,                              save :: ldumpcart
    logical,                              save :: ldumpconnectivity
    logical,                              save :: ldumpnooverwrite
    logical,                              save :: lmdarcunique                     ! If true then atom numbers will be append to symbols
  end module dump
!
!  Embedded atom model data
!
  module eam
    use datatypes
    integer(i4),                           parameter     :: maxmeamorder = 4       ! Number of MEAM orders : 4 => l = 0 -> 3
    integer(i4),                           parameter     :: maxmeamcomponent = 24  ! Used to dimension array for rho - 
                                                                                   ! do not change without much thought!
    character(len=5),  dimension(:),       pointer, save :: symboleamfnspec => null()
    character(len=5),  dimension(:,:),     pointer, save :: symboleamspec => null()
    character(len=80), dimension(:),       pointer, save :: eamfnfile(:) => null()
    character(len=80), dimension(:),       pointer, save :: eamdenfile(:) => null()
    integer(i4),                                    save :: maxeamden = 3
    integer(i4),                                    save :: maxeamspec = 1
    integer(i4),                                    save :: maxeamfnspec = 1
    integer(i4),                                    save :: maxneamfnnumeric = 1
    integer(i4),       dimension(:,:),     pointer, save :: ndenfn => null()
    integer(i4),       dimension(:),       pointer, save :: ndenfncomp => null()
    integer(i4),       dimension(:),       pointer, save :: neamfnnumeric => null()
    integer(i4),       dimension(:),       pointer, save :: neamnat => null()
    integer(i4),       dimension(:),       pointer, save :: neamtyp => null()
    integer(i4),       dimension(:),       pointer, save :: neamnat2 => null()
    integer(i4),       dimension(:),       pointer, save :: neamtyp2 => null()
    integer(i4),       dimension(:),       pointer, save :: neamfnmeamcombotype => null()    ! Indicates combining method for MEAM components
    integer(i4),       dimension(:),       pointer, save :: neamfnmeamtype => null()         ! Indicates whether to use the 21 or 24 MEAM component formulation
    integer(i4),       dimension(:),       pointer, save :: neamfnnat => null()
    integer(i4),       dimension(:),       pointer, save :: neamfntyp => null()
    integer(i4),       dimension(:,:),     pointer, save :: neammeamorder => null()          ! Specifies the MEAM order of a density component
    integer(i4),       dimension(:),       pointer, save :: neamfnmeamorder => null()        ! Specifies the MEAM order of a function species
    logical,           dimension(:),       pointer, save :: lmeamspec => null()              ! Indicates whether an EAM species needs MEAM or not
    real(dp),          dimension(:,:,:,:), pointer, save :: denpar => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric1 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric2 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric3 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric4 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric5 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric6 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric7 => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnnumeric8 => null()
    real(dp),          dimension(:),       pointer, save :: eamfnnumericdrho => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnpar => null()
    real(dp),          dimension(:,:),     pointer, save :: eamalloy => null()
    real(dp),          dimension(:,:),     pointer, save :: eamfnmeamcoeff => null()         ! Contains the coefficients for expanding the MEAM density
    real(dp),          dimension(:),       pointer, save :: eamtaperdrho => null()
    real(dp),          dimension(:),       pointer, save :: eamtaperrho => null()
    integer(i4),                                    save :: neamfn
    integer(i4),                                    save :: neamfnspec
    integer(i4),                                    save :: neampower
    integer(i4),                                    save :: neamspec
    logical,                                        save :: lPrintEAM  = .false.
    logical,                                        save :: lMEAM    = .false.
    logical,                                        save :: lMEAMfn  = .false.              ! Indicates whether function uses MEAM or not
    logical,                                        save :: lMEAMden = .false.              ! Indicates whether density uses MEAM or not
    logical,                                        save :: lMEAMscreen = .false.           ! Indicates whether screening is to be used for MEAM or not
    real(dp),                                       save :: meam_Cmin                       ! Minimum ellipse parameter for MEAM screening
    real(dp),                                       save :: meam_Cmax                       ! Maximum ellipse parameter for MEAM screening
!
    type, public :: screening_atoms
      integer(i4)                            :: sa_maxdim = 0
      integer(i4), dimension(:),     pointer :: sa_atom => null()                  ! Pointer to atom number
      integer(i4), dimension(:),     pointer :: sa_kxc => null()                   ! Pointer to position in second derivative array for this atom
      real(dp),    dimension(:),     pointer :: sa_rij => null()                   ! Distance of i-j vector for this triad
      real(dp),    dimension(:),     pointer :: sa_rik => null()                   ! Distance of i-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_rjk => null()                   ! Distance of j-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_xik => null()                   ! x component of i-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_yik => null()                   ! y component of i-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_zik => null()                   ! z component of i-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_xjk => null()                   ! x component of j-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_yjk => null()                   ! y component of j-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_zjk => null()                   ! z component of j-k vector for this triad
      real(dp),    dimension(:),     pointer :: sa_Sikj => null()                  ! Screening function contribution due to this triad
      real(dp),    dimension(:,:),   pointer :: sa_dSikjdr => null()               ! First derivative of the screening function for this triad
      real(dp),    dimension(:,:),   pointer :: sa_drhototij=> null()              ! First derivative of total rho for i-j resulting from screening
      real(dp),    dimension(:,:),   pointer :: sa_drhototik=> null()              ! First derivative of total rho for i-k resulting from screening
      real(dp),    dimension(:,:),   pointer :: sa_drhototjk=> null()              ! First derivative of total rho for j-k resulting from screening
      real(dp),    dimension(:,:),   pointer :: sa_drhotots => null()              ! First derivative of total rho with respect to strain 
    end type screening_atoms
  end module eam
!
!  EDIP data
!
  module EDIPdata
    use datatypes
    integer(i4),                                   save :: maxEDIPspec = 1                   ! Maximum number of EDIP species used in array allocation
    integer(i4),                                   save :: nEDIPspec = 0                     ! Number of EDIP species
    integer(i4),                                   save :: nlibEDIPspec = 0                  ! Number of EDIP species prior to library read
    character(len=5), dimension(:),       pointer, save :: symbolEDIPspec => null()          ! Character symbols for EDIP species
    integer(i4),      dimension(:),       pointer, save :: natEDIPspec => null()             ! Atomic numbers for EDIP species
    integer(i4),      dimension(:),       pointer, save :: ntypEDIPspec => null()            ! Type numbers for EDIP species
    real(dp),                                      save :: EDIPaccuracy1                     ! Parameter that controls the start of the exponential function taper
    real(dp),                                      save :: EDIPaccuracy2                     ! Parameter that controls the end of the exponential function taper
    real(dp),                                      save :: EDIPaccuracy1drmax                ! Decrease in rmax associated with EDIPaccuracy1 when scaled by sigma
    real(dp),                                      save :: EDIPaccuracy2drmax                ! Decrease in rmax associated with EDIPaccuracy2 when scaled by sigma
    real(dp),                                      save :: EDIPcutoff                        ! Maximum cutoff distance in EDIP
    real(dp),                                      save :: EDIPmaxZcutoff = 6.0_dp           ! Maximum Z value allowed for in cut-off calculation
    logical,          dimension(:),       pointer, save :: lEDIPpairOK => null()             ! Flag to indicate that parameters have been set for this pair
    logical,          dimension(:,:),     pointer, save :: lEDIPtriadOK => null()            ! Flag to indicate that parameters have been set for this triad
    real(dp),         dimension(:),       pointer, save :: EDIPrmax => null()                ! Maximum radius for computing bond order per species
    real(dp),         dimension(:),       pointer, save :: EDIPrmaxpair => null()            ! Maximum radius for computing bond order for a pair
    real(dp),         dimension(:),       pointer, save :: EDIPfhigh => null()               ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPflow => null()                ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPphigh => null()               ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPplow => null()                ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPalpha => null()               ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPZdih => null()                ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPZrep => null()                ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIPc0 => null()                  ! EDIP coordination number
    real(dp),         dimension(:),       pointer, save :: EDIP2epsilon => null()            ! EDIP twobody
    real(dp),         dimension(:),       pointer, save :: EDIP2a => null()                  ! EDIP twobody
    real(dp),         dimension(:),       pointer, save :: EDIP2aprime => null()             ! EDIP twobody
    real(dp),         dimension(:),       pointer, save :: EDIP2B => null()                  ! EDIP twobody
    real(dp),         dimension(:),       pointer, save :: EDIP2beta => null()               ! EDIP twobody
    real(dp),         dimension(:),       pointer, save :: EDIP2sigma => null()              ! EDIP twobody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3lambda0 => null()            ! EDIP threebody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3lambdap => null()            ! EDIP threebody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3gamma0 => null()             ! EDIP threebody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3gammap => null()             ! EDIP threebody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3q => null()                  ! EDIP threebody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3kq2 => null()                ! EDIP threebody
    real(dp),         dimension(:,:),     pointer, save :: EDIP3Z0 => null()                 ! EDIP threebody
  end module EDIPdata
!
!  Energy contributions
!
  module energies
    use datatypes
    real(dp),         dimension(:,:),     pointer, save :: eregion2region => null()! Energy of interaction between regions
! Bulk
    real(dp),                                      save :: eatom                   ! Energy from interatomic potentials
    real(dp),                                      save :: ebgd                    ! Energy from charge neutralising background
    real(dp),                                      save :: ebondorder              ! Energy from bond order potentials
    real(dp),                                      save :: eboQself
    real(dp),                                      save :: ebrenner                ! Energy from brenner REBO potentials
    real(dp),                                      save :: ec6                     ! Energy from C6 long-range sums
    real(dp),                                      save :: echargecoupled
    real(dp),                                      save :: echemsh
    real(dp),                                      save :: ecmm                    ! Energy from cell multipole method
    real(dp),                                      save :: ecosmo                  ! Energy from solvation model
    real(dp),                                      save :: edipole                 ! Energy from dipole correction
    real(dp),                                      save :: eedip                   ! Energy from EDIP force field
    real(dp),                                      save :: eeinstein               ! Energy from Einstein model potentials
    real(dp),                                      save :: eevb                    ! Energy from empirical valence bond coupling
    real(dp),                                      save :: efield                  ! Energy from interaction with electric field
    real(dp),                                      save :: efor                    ! Energy from four-body potentials
    real(dp),                                      save :: eforce                  ! Energy from integral of force x distance
    real(dp),                                      save :: emad                    ! Energy from Madelung correction
    real(dp),                                      save :: emany                   ! Energy from many-body potentials
    real(dp),                                      save :: eone                    ! Energy from one-body potentials
    real(dp),                                      save :: eoop                    ! Energy from out of plane potentials
    real(dp),                                      save :: eplane                  ! Energy from deviation out of a 2-D plane
    real(dp),                                      save :: epolar                  ! Energy from polarisation in a point ion model
    real(dp),                                      save :: epv                     ! Energy from enthalpy term (PV)
    real(dp),                                      save :: eqeq                    ! Energy from self terms in QEq model
    real(dp),                                      save :: eradial                 ! Energy from a radial potential
    real(dp),                                      save :: ereal                   ! Energy from real space electrostatics
    real(dp),                                      save :: ereaxFF                 ! Energy from ReaxFF force field
    real(dp),                                      save :: erecip                  ! Energy from reciprocal space electrostatics
    real(dp),                                      save :: eself                   ! Energy from self terms in charge equilibration
    real(dp),                                      save :: esix                    ! Energy from six-body potentials
    real(dp),                                      save :: ethb                    ! Energy from three-body potentials
    real(dp),                                      save :: evib                    ! Energy from vibrational contribution
    real(dp),                                      save :: ewolfself               ! Energy from self-term in Wolf sum
    real(dp),                                      save :: efreeze
    real(dp),                                      save :: fcsave
    real(dp),                                      save :: fcstore
! Surface
    real(dp),                                      save :: eattach
    real(dp),                                      save :: esregion12
    real(dp),                                      save :: esregion2
    real(dp),                                      save :: esurface
! Defect
    real(dp),                                      save :: e11
    real(dp),                                      save :: e11old
    real(dp),                                      save :: e12a
    real(dp),                                      save :: e12aold
    real(dp),                                      save :: e12ap
    real(dp),                                      save :: e12ad
    real(dp),                                      save :: e2a
    real(dp),                                      save :: e2b
    real(dp),                                      save :: e12b
    real(dp),                                      save :: e12bold
    real(dp),                                      save :: e12bo
    real(dp),                                      save :: e12boold
    real(dp),                                      save :: e12f
    real(dp),                                      save :: e12fold
    real(dp),                                      save :: e12m
    real(dp),                                      save :: e12mold
    real(dp),                                      save :: e12t
    real(dp),                                      save :: e12told
    real(dp),          dimension(:),      pointer, save :: siteenergy => null()   ! Site energies of atoms
  end module energies
!
!  Free energy workspace
!
  module feworkspace
    use datatypes
    integer(i4),                          save :: maxmany  = 1
    integer(i4),                          save :: maxmany2 = 1
    integer(i4), dimension(:),   pointer, save :: nptrfork => null()  
    integer(i4), dimension(:),   pointer, save :: nptrforl => null()
    integer(i4), dimension(:),   pointer, save :: nptrmanyk => null()
    real(dp),    dimension(:,:), pointer, save :: d33 => null()
    real(dp),    dimension(:,:), pointer, save :: d33r => null()
    real(dp),    dimension(:,:), pointer, save :: d33i => null()
    real(dp),    dimension(:,:), pointer, save :: d33s => null()
    real(dp),    dimension(:,:), pointer, save :: d33rs => null()
    real(dp),    dimension(:,:), pointer, save :: d33is => null()
    real(dp),    dimension(:,:), pointer, save :: d34 => null()
    real(dp),    dimension(:,:), pointer, save :: d34r => null()
    real(dp),    dimension(:,:), pointer, save :: d34i => null()
    real(dp),    dimension(:,:), pointer, save :: d34s => null()
    real(dp),    dimension(:,:), pointer, save :: d34rs => null()
    real(dp),    dimension(:,:), pointer, save :: d34is => null()
  end module feworkspace
!
!  Field data
!
  module field
    use datatypes
    integer(i4),                            save :: maxtdfield = 2                   ! maximum number of time-dependent fields per configuration
    logical,     dimension(:),     pointer, save :: lfieldcfg => null()              ! If true there is a field acting on this configuration
    integer(i4), dimension(:),     pointer, save :: ntdfieldcfg => null()            ! Number of time-dependent fields for configuration
    real(dp),    dimension(:),     pointer, save :: fieldcfg => null()               ! Magnitude of static field for configuration
    real(dp),    dimension(:,:),   pointer, save :: fielddirectioncfg => null()      ! Direction of static field for configuration
    real(dp),    dimension(:,:,:), pointer, save :: td_fieldcfg => null()            ! Magnitude of time-dependent field for configuration & evolution parameters
    real(dp),    dimension(:,:,:), pointer, save :: td_fielddirectioncfg => null()   ! Direction of time-dependent field for configuration
  end module field
!
!  File data
!
  module files
    use datatypes
    character(len=80), save :: arcfile
    character(len=80), save :: biofile
    character(len=80), save :: ciffile
    character(len=80), save :: cosmofile
    character(len=80), save :: cssrfile
    character(len=80), save :: dcdfile
    character(len=80), save :: debyefile !ers29
    character(len=80), save :: dlvfile
    character(len=80), save :: drvfile
    character(len=80), save :: eigfile
    character(len=80), save :: fdffile
    character(len=80), save :: frcfile
    character(len=80), save :: freqfile
    character(len=80), save :: hisfile
    character(len=80), save :: lammpsfile
    character(len=80), save :: lammpspotsfile
    character(len=80), save :: marvfile
    character(len=80), save :: marvtemp
    character(len=80), save :: mcfile
    character(len=80), save :: oscfile
    character(len=80), save :: pdffile !ers29
    character(len=80), save :: phonfile
    character(len=80), save :: prefile
    character(len=80), save :: qbofile
    character(len=80), save :: sasfile
    character(len=80), save :: thbfile
    character(len=80), save :: trjfile 
    character(len=80), save :: xrfile
    character(len=80), save :: xtlfile
    character(len=80), save :: xyzfile
    integer(i4),      save :: narcwrite = 1
    integer(i4),      save :: nfreqdecimals = 6
    integer(i4),      save :: nlammpspoints = 1000                   ! Number of points for Lammps table potential
    logical,          save :: lbio
    logical,          save :: ldcd
    logical,          save :: ldlv
    logical,          save :: leig
    logical,          save :: lfrq
    logical,          save :: lfrqbin
    logical,          save :: lmarv
    logical,          save :: lmarv2
    logical,          save :: lsas
    logical,          save :: lthb
    logical,          save :: lxtl
    logical,          save :: lphono
    logical,          save :: larc
    logical,          save :: lmovie
    logical,          save :: lmcout
    logical,          save :: losc
    logical,          save :: loutshell
    logical,          save :: lpre
    logical,          save :: lxr
    logical,          save :: lcif
    logical,          save :: lcosmofile
    logical,          save :: lcssr
    logical,          save :: ltrjascii
    logical,          save :: ltrjequil
    logical,          save :: llammps
    logical,          save :: llammpspots
    logical,          save :: ltrj
    logical,          save :: lxyz
    logical,          save :: lxyzmovie
    logical,          save :: lfdf
    logical,          save :: lhis
    logical,          save :: ldrv
    logical,          save :: lfrc
    logical,          save :: lqbo
    logical,          save :: lqboappend
    real(dp),         save :: lammps_r0                              ! Start of Lammps table potential range
    real(dp),         save :: lammps_rend                            ! End of Lammps table potential range
  end module files
!
!  Fitting data
!
  module fitting
    use datatypes
    integer(i4),                          save :: maxfit = 1
    integer(i4),                          save :: maxfcon = 0
    integer(i4), dimension(:),   pointer, save :: nfatyp => null()
    integer(i4), dimension(:),   pointer, save :: nfcfix => null()
    integer(i4), dimension(:),   pointer, save :: nfcotyp => null()
    integer(i4), dimension(:),   pointer, save :: nfcvar => null()
    integer(i4), dimension(:),   pointer, save :: nfcfg => null()
    integer(i4), dimension(:),   pointer, save :: nfpot=> null()
    integer(i4), dimension(:),   pointer, save :: nfpot2 => null()
    integer(i4), dimension(:),   pointer, save :: nfpot3 => null()
    integer(i4), dimension(:),   pointer, save :: nftyp=> null()
    integer(i4), dimension(:),   pointer, save :: nfvar=> null()
    integer(i4), dimension(:),   pointer, save :: nfvar2 => null()
    integer(i4), dimension(:),   pointer, save :: nfvar3 => null()
    integer(i4), dimension(:),   pointer, save :: nfitptr => null()
    real(dp),    dimension(:),   pointer, save :: fconadd => null()
    real(dp),    dimension(:),   pointer, save :: fconco => null()
    real(dp),    dimension(:),   pointer, save :: fconpower => null()
    real(dp),    dimension(:),   pointer, save :: fres => null()
    real(dp),    dimension(:),   pointer, save :: scale => null()
    character(len=15),                    save :: fitlabel(20,54,9)
    integer(i4),                          save :: lastq
    integer(i4),                          save :: lastt
    integer(i4),                          save :: maxfcal
    integer(i4),                          save :: ncycp
    integer(i4),                          save :: nfcon
    integer(i4),                          save :: nfit
    integer(i4),                          save :: nfitt
    integer(i4),                          save :: nfupdate
    real(dp),                             save :: delta
    real(dp),                             save :: fstepmx
    real(dp),                             save :: fftol
    real(dp),                             save :: fgmax
    real(dp),                             save :: fgtol
    real(dp),                             save :: fxtol
  end module fitting
!
!  Four-body potential data
!
  module four
    use datatypes
    character(len=5), dimension(:,:), pointer, save :: symbol4
    integer(i4),                               save :: maxlist4 = 1
    integer(i4),                               save :: maxfor = 10
    integer(i4),                               save :: maxiltor = 9
    integer(i4),      dimension(:),   pointer, save :: icell41 => null()
    integer(i4),      dimension(:),   pointer, save :: icell42 => null()
    integer(i4),      dimension(:),   pointer, save :: icell43 => null()
    integer(i4),      dimension(:),   pointer, save :: ilind => null()
    integer(i4),      dimension(:),   pointer, save :: ilnum => null()
    integer(i4),      dimension(:,:), pointer, save :: iltor => null()
    integer(i4),      dimension(:,:), pointer, save :: ilftor => null()
    integer(i4),      dimension(:,:), pointer, save :: ilxtor => null()
    integer(i4),      dimension(:),   pointer, save :: jkind => null()
    integer(i4),      dimension(:),   pointer, save :: mmfexc => null()
    integer(i4),      dimension(:,:), pointer, save :: n4botype => null()
    integer(i4),      dimension(:),   pointer, save :: neqiltor => null()
    integer(i4),      dimension(:),   pointer, save :: nforptr => null()
    integer(i4),      dimension(:),   pointer, save :: nforty => null()
    integer(i4),      dimension(:),   pointer, save :: nfptyp1 => null()
    integer(i4),      dimension(:),   pointer, save :: nfptyp2 => null()
    integer(i4),      dimension(:),   pointer, save :: nfptyp3 => null()
    integer(i4),      dimension(:),   pointer, save :: nfptyp4 => null()
    integer(i4),      dimension(:),   pointer, save :: nfspec1 => null()
    integer(i4),      dimension(:),   pointer, save :: nfspec2 => null()
    integer(i4),      dimension(:),   pointer, save :: nfspec3 => null()
    integer(i4),      dimension(:),   pointer, save :: nfspec4 => null()
    integer(i4),      dimension(:),   pointer, save :: npfor => null()
    integer(i4),      dimension(:),   pointer, save :: nfortor => null()
    logical,          dimension(:),   pointer, save :: lfdreiding => null()
    logical,          dimension(:),   pointer, save :: lfintra => null()
    logical,          dimension(:),   pointer, save :: lfinter => null()
    logical,          dimension(:),   pointer, save :: lgenerated4 => null()
    logical,          dimension(:),   pointer, save :: lonly3oop => null()
    logical,          dimension(:,:), pointer, save :: lopiltor => null()
    logical,          dimension(:),   pointer, save :: loutofplane => null()
    logical,                                   save :: lPrintFour = .false.
    logical,          dimension(:),   pointer, save :: liltorswitch => null()
    logical,          dimension(:),   pointer, save :: ljktorswitch => null()
    logical,          dimension(:,:), pointer, save :: lsurfiltor => null()
    real(dp),         dimension(:),   pointer, save :: fork => null()
    real(dp),         dimension(:),   pointer, save :: for1 => null()
    real(dp),         dimension(:),   pointer, save :: for2 => null()
    real(dp),         dimension(:),   pointer, save :: for3 => null()
    real(dp),         dimension(:),   pointer, save :: for4 => null()
    real(dp),         dimension(:),   pointer, save :: for1min => null()
    real(dp),         dimension(:),   pointer, save :: for2min => null()
    real(dp),         dimension(:),   pointer, save :: for3min => null()
    real(dp),         dimension(:),   pointer, save :: for4min => null()
    real(dp),         dimension(:,:), pointer, save :: forpoly => null()
    real(dp),         dimension(:),   pointer, save :: oiltor => null()
    real(dp),         dimension(:,:), pointer, save :: riltor => null()
    real(dp),         dimension(:,:), pointer, save :: xiltor => null()
    real(dp),         dimension(:,:), pointer, save :: yiltor => null()
    real(dp),         dimension(:,:), pointer, save :: ziltor => null()
    integer(i4),                               save :: nfor
    integer(i4),                               save :: nlist4md
  end module four
!
!  Frequency values for current structure
!
  module frequencies
    use datatypes
    integer(i4),                          save :: maxfkpt = 1        ! Maximum right hand dimension of freq
    integer(i4),                          save :: maxfqat = 0        ! Maximum left hand dimension of freq
    real(dp),    dimension(:,:), pointer, save :: freq               ! Array to store frequencies for each k point
  end module frequencies
!
!  Genetic algorithm configuration data
!
  module gaconf
    use datatypes
    integer(i4),                          save :: maxbcfg = 0
    integer(i4),                          save :: maxgcfg = 0
    integer(i4),                          save :: maxmvar = 0
    integer(i4),                          save :: mvar
    integer(i4), dimension(:),   pointer, save :: ithbest => null()
    real(dp),    dimension(:,:), pointer, save :: xconf => null()
    real(dp),    dimension(:,:), pointer, save :: xbest => null()
    real(dp),    dimension(:),   pointer, save :: fconf => null()
  end module gaconf
!
!  General information
!
  module general
    use datatypes
    integer(i4),                             save :: maxtitle = 20         ! Maximum number of title lines
    character(len=80), dimension(:),pointer, save :: titleword => null()   ! String array storing the title lines
    character(len=132),                      save :: elefile               ! Stores the name / location of the element info file
    character(len=132),                      save :: gulpdir
    character(len=132),                      save :: gulpdoc               ! Directory location of GULP documentation
    character(len=132),                      save :: helpfile              ! Stores the name / location of the help info file
    character(len=80),                       save :: site
    integer(i4),                             save :: nmaxcells
    integer(i4),                             save :: nadd                  ! Number of cells added as a safeguard during distance searching
    integer(i4),                             save :: nemorder
    integer(i4),                             save :: ntitle                ! Number of title lines read in
    integer(i4),                             save :: nwarn                 ! Number of warnings encountered during execution
    logical,                                 save :: lfinitediff           ! If true then use finite difference algorithms
    real(dp),                                save :: accuracy              ! Controls the precision of the Ewald summation
    real(dp),                                save :: bfactor
    real(dp),                                save :: cellmin 
    real(dp),                                save :: cutb
    real(dp),                                save :: cutw
    real(dp),                                save :: cutoffmax             ! Maximum cutoff of any potential 
    real(dp),                                save :: cutoffmaxbo           ! Maximum cutoff of any bond order potential 
    real(dp),                                save :: etaw
    real(dp),                                save :: findiff               ! Finite difference step size
    real(dp),                                save :: findiffc              ! Finite difference step size for property evaluation - Cartesian
    real(dp),                                save :: findiffs              ! Finite difference step size for property evaluation - strain
    real(dp),                                save :: phondiff              ! Finite difference step size for phonon evaluation
    real(dp),                                save :: lowerscale            ! Scale factor for eigenvectors during lower operation
    real(dp),                                save :: rkw
    real(dp),                                save :: rspeed0               ! Factor that controls the weighting between real / recip space in Ewald for energy
    real(dp),                                save :: rspeed1               ! Factor that controls the weighting between real / recip space in Ewald for 1st dervs
    real(dp),                                save :: rspeed2               ! Factor that controls the weighting between real / recip space in Ewald for 2nd dervs
    real(dp),                                save :: scalefactor
    real(dp),                                save :: selfwolf              ! Self term for the Wolf sum energy
    real(dp),                                save :: selfwolfenergy        ! Self term for the Wolf sum energy due to the force
    real(dp),                                save :: selfwolfforce         ! Self term for the Wolf sum force
    real(dp),                                save :: smallself             ! This value is used for distance^2 test for self terms
    real(dp),                                save :: targetrradmax
    real(dp),                                save :: tdel
    real(dp),                                save :: time0           
    real(dp),                                save :: timesofar             ! Time spent executing so far
    real(dp),                                save :: timmax                ! Maximum time allowed for execution
    real(dp),                                save :: version               ! GULP version number
  end module general
!
!  Genetic algorithm data
!
  module genetic
    use datatypes
    integer(i4), dimension(:),   pointer, save :: ndiscret => null()
    real(dp),    dimension(:),   pointer, save :: xmax => null()
    real(dp),    dimension(:,:), pointer, save :: xmaxcfg => null()
    real(dp),    dimension(:),   pointer, save :: xmin => null()
    real(dp),    dimension(:,:), pointer, save :: xmincfg => null()
    integer(i4),                          save :: iseed=-1
    integer(i4),                          save :: maxgacyc=300
    integer(i4),                          save :: mgacfg=10
    integer(i4),                          save :: nd
    integer(i4),                          save :: ndi
    integer(i4),                          save :: ndmax
    integer(i4),                          save :: ngabest
    integer(i4),                          save :: ngacfg
    integer(i4),                          save :: ngacjg
    integer(i4),                          save :: nspar
    real(dp),                             save :: prob(9)
    integer(i4),                          save :: ngabset=0
    logical,                              save :: l2pxo=.false.
    logical,                              save :: lgabest=.false.
    logical,                              save :: lgaconjg
    logical,                              save :: lgacost
    logical,                              save :: lgadef=.false.
    logical,                              save :: lgaexpw=.false.
    logical,                              save :: loxygen=.false.
    logical,                              save :: lstruc=.false.
    real(dp),                             save :: anftol
    real(dp),                             save :: anfac
    real(dp),                             save :: antemp
    real(dp),                             save :: antlimit
    real(dp),                             save :: udif=0.0_dp
  end module genetic
!
!  Input data
!
  module gulpinput
    use datatypes
    integer(i4),                              save :: maxlinelength = 80
    integer(i4),                              save :: maxword = 20
    character(len=80), dimension(:), pointer, save :: floatwords => null()
    character(len=80), dimension(:), pointer, save :: words => null()
    integer(i4),       dimension(:), pointer, save :: nlorder => null()
    real(dp),          dimension(:), pointer, save :: floats => null()
    integer(i4),                              save :: nfloat
    integer(i4),                              save :: nword
  end module gulpinput
!
!  Interupt module
!
  module interupt
    use datatypes
    logical,                              save :: controlC = .false.
    integer,                              save :: sigint = 2
  end module interupt
!
!  K point sampling data
!
  module ksample
    use datatypes
    integer(i4),                          save :: maxkpt = 1                    ! Maximum number of K points
    integer(i4), dimension(:),   pointer, save :: nkptcfg => null()             ! Pointer to configuration of K point
    integer(i4), dimension(:),   pointer, save :: norigkpt => null()            ! Number of K points input for each configuration
    integer(i4), dimension(:),   pointer, save :: nxks => null()                ! Shrinking factor for the Brillouin zone in direction a
    integer(i4), dimension(:),   pointer, save :: nyks => null()                ! Shrinking factor for the Brillouin zone in direction b
    integer(i4), dimension(:),   pointer, save :: nzks => null()                ! Shrinking factor for the Brillouin zone in direction c
    logical,     dimension(:),   pointer, save :: lkptdispersion => null()      ! If true this K point is part of a dispersion curve
    real(dp),    dimension(:),   pointer, save :: wkpt => null()                ! Weight for this K point
    real(dp),    dimension(:),   pointer, save :: xkpt => null()                ! X component of K vector
    real(dp),    dimension(:),   pointer, save :: ykpt => null()                ! Y component of K vector
    real(dp),    dimension(:),   pointer, save :: zkpt => null()                ! Z component of K vector
    integer(i4),                          save :: nkpt                          ! Number of K points
  end module ksample
!
!  K point sampling data - scattering calculation. Current configuration only.
!
  module ksample_scatter
    use datatypes
    integer(i4),                          save :: maxskpt = 1                   ! Maximum number of K points
    real(dp),    dimension(:),   pointer, save :: wskpt => null()               ! Weight for this K point
    real(dp),    dimension(:),   pointer, save :: xskpt => null()               ! X component of K vector
    real(dp),    dimension(:),   pointer, save :: yskpt => null()               ! Y component of K vector
    real(dp),    dimension(:),   pointer, save :: zskpt => null()               ! Z component of K vector
    integer(i4),                          save :: nskpt                         ! Number of K points
  end module ksample_scatter
!
!  Reciprocal space data
!
  module kspace
    use datatypes
    integer(i4),                          save :: maxkvec = 500
    integer(i4), dimension(:),   pointer, save :: indk => null()
    real(dp),    dimension(:),   pointer, save :: argc => null()
    real(dp),    dimension(:),   pointer, save :: csin => null()
    real(dp),    dimension(:),   pointer, save :: kmod => null()
    real(dp),    dimension(:),   pointer, save :: ktrm => null()
    real(dp),    dimension(:),   pointer, save :: ktrms => null()
    real(dp),    dimension(:),   pointer, save :: sine => null()
    real(dp),    dimension(:),   pointer, save :: xrk => null()
    real(dp),    dimension(:),   pointer, save :: yrk => null()
    real(dp),    dimension(:),   pointer, save :: zrk => null()
    real(dp),    dimension(:),   pointer, save :: xrk0 => null()
    real(dp),    dimension(:),   pointer, save :: yrk0 => null()
    real(dp),    dimension(:),   pointer, save :: zrk0 => null()
    integer(i4),                          save :: nkangle
    integer(i4),                          save :: nkvec
    real(dp),                             save :: accf
    real(dp),                             save :: eta
    real(dp),                             save :: eta4
    real(dp),                             save :: radmax
    real(dp),                             save :: rhseta
    real(dp),                             save :: rpieta
    real(dp),                             save :: rradmax
    real(dp),                             save :: rmx2
    real(dp),                             save :: seta
    real(dp),                             save :: tweatpi
    real(dp),                             save :: vol4pi
  end module kspace
!
!  Library file data
!
  module library
    use datatypes
    integer(i4),                              save :: maxlib = 4
    character(len=80), dimension(:), pointer, save :: libname => null()
    character(len=16), dimension(:), pointer, save :: libspec => null()
    integer(i4),                              save :: nlib
    integer(i4),                              save :: nlib1s
    integer(i4),                              save :: nlib2s
    integer(i4),                              save :: nlib3s
    integer(i4),                              save :: nlib4s
    integer(i4),                              save :: nlib6s
    integer(i4),                              save :: nlibatab
    integer(i4),                              save :: nlibbondQ
    integer(i4),                              save :: nlibsp
    integer(i4),                              save :: nlibseps
    integer(i4),                              save :: nlibUFFsp
    logical,                                  save :: llibdump
    logical,                                  save :: llibsymdump = .false.
  end module library
!
!  Maths library choices
!
  module maths
    use datatypes
    logical,                                  save :: leispack_eigensolve = .false.
    logical,                                  save :: ldivide_and_conquer = .true.
  end module maths
!
!  Monte Carlo data
!
  module montecarlo
    use datatypes
    integer(i4),                            save :: maxmcswapspec = 1
    integer(i4), dimension(:),     pointer, save :: ngcmcnat => null()
    integer(i4), dimension(:),     pointer, save :: ngcmctype => null()
    integer(i4), dimension(:),     pointer, save :: ngcmcmolat => null()
    integer(i4), dimension(:,:),   pointer, save :: ngcmcmolnat => null()
    integer(i4), dimension(:,:),   pointer, save :: ngcmcmoltype => null()
    integer(i4), dimension(:),     pointer, save :: nptrdestroyable => null()
    integer(i4), dimension(:),     pointer, save :: nptrmoveable => null()
    integer(i4), dimension(:),     pointer, save :: nptrrotateable => null()
    integer(i4), dimension(:),     pointer, save :: nptrstrainable => null()
    integer(i4), dimension(:),     pointer, save :: nptrswapable => null()
    integer(i4), dimension(:),     pointer, save :: nptrtrialatom => null()
    integer(i4), dimension(:),     pointer, save :: nmcswapnat => null()
    integer(i4), dimension(:),     pointer, save :: nmcswaptype => null()
    logical,     dimension(:),     pointer, save :: ltrialatom => null()
    real(dp),    dimension(:,:),   pointer, save :: xgcmcmol => null()
    real(dp),    dimension(:,:),   pointer, save :: ygcmcmol => null()
    real(dp),    dimension(:,:),   pointer, save :: zgcmcmol => null()
    real(dp),                               save :: chempot
    real(dp),                               save :: dmaxmc
    real(dp),                               save :: rmaxmc
    real(dp),                               save :: smaxmc
    real(dp),                               save :: pcreate
    real(dp),                               save :: pdestroy
    real(dp),                               save :: pmove
    real(dp),                               save :: pstrain
    real(dp),                               save :: protate 
    real(dp),                               save :: pswap
    real(dp),                               save :: targetmove
    real(dp),                               save :: targetrota
    real(dp),                               save :: targetstrain
    real(dp),                               save :: mcelowest
    real(dp),                               save :: mcemean
    real(dp),                               save :: mcnmean
    real(dp),                               save :: mcvolume
    logical,                                save :: lmclowestwrite
    logical,                                save :: lmcswapany = .true.
    integer(i4),                            save :: maxdestroyable
    integer(i4),                            save :: maxgcmcmol = 1
    integer(i4),                            save :: maxgcmcmolat = 10
    integer(i4),                            save :: maxmoveable
    integer(i4),                            save :: maxrotateable
    integer(i4),                            save :: maxswapable
    integer(i4),                            save :: maxtrialatom = 10
    integer(i4),                            save :: ndestroyable
    integer(i4),                            save :: ndestroyablemol
    integer(i4),                            save :: nmoveable
    integer(i4),                            save :: nptrrotationtype(3)
    integer(i4),                            save :: nrotationtype
    integer(i4),                            save :: nrotateable
    integer(i4),                            save :: nstrainable
    integer(i4),                            save :: nswapable
    integer(i4),                            save :: nmcswapspec
    integer(i4),                            save :: ngcmcmol
    integer(i4),                            save :: ngcmcspec
    integer(i4),                            save :: nmcaccepted
    integer(i4),                            save :: nmcoutfreq
    integer(i4),                            save :: nmcsample
    integer(i4),                            save :: nmcstep
    integer(i4),                            save :: nmctrial
    integer(i4),                            save :: ntargetfreq
    integer(i4),                            save :: ntargetfreqr
    integer(i4),                            save :: ntargetfreqs
    integer(i4),                            save :: ntrialatom
  end module montecarlo
!
!  Logicals connected with MD runs
!
!  lmd     = if .true. then this is an MD run
!  llist3  = if .true. then use list methods for 3-body terms
!  llist4  = if .true. then use list methods for 4-body terms
!  llist6  = if .true. then use list methods for 6-body terms
!
  module mdlogic
    use datatypes
    logical,                                save :: ladiabatic 
    logical,                                save :: llist3
    logical,                                save :: llist4
    logical,                                save :: llist6
    logical,                                save :: lmd
  end module mdlogic
!
!  Molecular dynamics data
!
  module moldyn
    use datatypes
    integer(i4), dimension(:),   pointer, save :: nensemble => null()
    integer(i4), dimension(:,:), pointer, save :: nmdconstrainatom => null()
    integer(i4), dimension(:),   pointer, save :: nmdeq => null()
    integer(i4), dimension(:),   pointer, save :: nmdprod => null()
    integer(i4), dimension(:),   pointer, save :: nmdsamp => null()
    integer(i4), dimension(:),   pointer, save :: nmdvelmode => null()
    integer(i4), dimension(:),   pointer, save :: nmdvelmodp => null()
    integer(i4), dimension(:),   pointer, save :: nmdwrite => null()
    logical,     dimension(:),   pointer, save :: lfix => null()
    logical,     dimension(:),   pointer, save :: lmdconstrain => null()
    real(dp),    dimension(:),   pointer, save :: nmdconstraindist => null()
    real(dp),    dimension(:),   pointer, save :: qpres => null()
    real(dp),    dimension(:),   pointer, save :: qtemp => null()
    real(dp),    dimension(:),   pointer, save :: tmdforcestart => null()
    real(dp),    dimension(:),   pointer, save :: tmdforcestop => null()
    real(dp),    dimension(:),   pointer, save :: tmdfieldstart => null()
    real(dp),    dimension(:),   pointer, save :: tmdfieldstop => null()
    real(dp),    dimension(:),   pointer, save :: tmdeq => null()
    real(dp),    dimension(:),   pointer, save :: tmdprod => null()
    real(dp),    dimension(:),   pointer, save :: tmdsamp => null()
    real(dp),    dimension(:),   pointer, save :: tmdscale => null()
    real(dp),    dimension(:),   pointer, save :: tmdscint => null()
    real(dp),    dimension(:),   pointer, save :: tmdwrite => null()
    real(dp),    dimension(:),   pointer, save :: tstep => null()
    real(dp),    dimension(:),   pointer, save :: xabsco => null()
    real(dp),    dimension(:),   pointer, save :: yabsco => null()
    real(dp),    dimension(:),   pointer, save :: zabsco => null()
    integer(i4),                          save :: naverpt
    integer(i4),                          save :: ndof
    integer(i4),                          save :: nmdintegrator
    integer(i4),                          save :: nmditer
    integer(i4),                          save :: nmoving
    logical,                              save :: ltethered
    real(dp),                             save :: cellmass
    real(dp),                             save :: cdrv(9)
    real(dp),                             save :: ekin
    real(dp),                             save :: lambdaR
    real(dp),                             save :: lambdaV
    real(dp),                             save :: pfct
    real(dp),                             save :: psfct
    real(dp),                             save :: psfctt
    real(dp),                             save :: refct
    real(dp),                             save :: rmdmaxtemp
    real(dp),                             save :: rmdmaxvol
    real(dp),                             save :: rmdvol0
    real(dp),                             save :: rnmoving
    real(dp),                             save :: rvfct
    real(dp),                             save :: sfac
    real(dp),                             save :: sfac0
    real(dp),                             save :: smdfct
    real(dp),                             save :: smdfctt
    real(dp),                             save :: sumsfac
    real(dp),                             save :: svel
    real(dp),                             save :: stpsqh
    real(dp),                             save :: sumacell
    real(dp),                             save :: sumbcell
    real(dp),                             save :: sumccell
    real(dp),                             save :: sumalpcell
    real(dp),                             save :: sumbetcell
    real(dp),                             save :: sumgamcell
    real(dp),                             save :: suminertia
    real(dp),                             save :: sumlambdaR
    real(dp),                             save :: sumlambdaV
    real(dp),                             save :: sumvol
    real(dp),                             save :: sumvsq
    real(dp),                             save :: sumener
    real(dp),                             save :: sumvir
    real(dp),                             save :: sumtem
    real(dp),                             save :: sumcst
    real(dp),                             save :: sumcons
    real(dp),                             save :: sumstress(6)
    real(dp),                             save :: velfct
    real(dp),                             save :: velfctt
    real(dp),                             save :: velmax
    real(dp),                             save :: velsq
    real(dp),                             save :: xcell(9)
  end module moldyn
!
!  Molecule related data
!
  module molecule
    use datatypes
    integer(i4),                          save :: maxconnect = 1
    integer(i4),                          save :: maxmol = 1
    integer(i4),                          save :: maxnobo = 1
    integer(i4), dimension(:),   pointer, save :: moldim => null()
    integer(i4), dimension(:),   pointer, save :: moldimi=> null()
    integer(i4), dimension(:),   pointer, save :: molgcmc => null()
    integer(i4), dimension(:),   pointer, save :: ixshift => null()
    integer(i4), dimension(:),   pointer, save :: iyshift => null()
    integer(i4), dimension(:),   pointer, save :: izshift => null()
    integer(i4), dimension(:),   pointer, save :: natmol=> null()             ! Molecule number for each atom : 0 if not in a molecule
    integer(i4), dimension(:),   pointer, save :: n1connect => null()
    integer(i4), dimension(:),   pointer, save :: n2connect => null()
    integer(i4), dimension(:),   pointer, save :: nconnectcfg => null()
    integer(i4), dimension(:),   pointer, save :: nconnectind => null()
    integer(i4), dimension(:,:), pointer, save :: nconnecttype => null()
    integer(i4), dimension(:),   pointer, save :: nmolatom => null()           ! Number of atoms in each molecule
    integer(i4), dimension(:),   pointer, save :: nmolind => null()            ! Cell index for each atom in its respective molecule
    integer(i4), dimension(:),   pointer, save :: nmollist => null()           ! List of atoms in each molecule as a linear array
    integer(i4), dimension(:),   pointer, save :: nmolptr => null()            ! Pointer to start of each molecule in nmollist - 1
    integer(i4), dimension(:),   pointer, save :: nobond => null()
    integer(i4), dimension(:),   pointer, save :: nobotyp => null()
    integer(i4),                          save :: nconnect
    integer(i4),                          save :: nmol               ! Number of molecules
    integer(i4),                          save :: nnobo
    logical,     dimension(:),   pointer, save :: lgcmcmol => null()
    logical,                              save :: lnoautobond
    logical,                              save :: lmol
    logical,                              save :: lmolq
    logical,                              save :: lmolfix
    logical,                              save :: lmolmec
    real(dp),                             save :: rtol
    real(dp),    dimension(:),   pointer, save :: xfsave => null()
    real(dp),    dimension(:),   pointer, save :: yfsave => null()
    real(dp),    dimension(:),   pointer, save :: zfsave => null()
  end module molecule
!
!  Nudged elastic band data
!
  module neb
    use datatypes
    integer(i4),                            save :: maxnebreplicatot = 1
    integer(i4),                            save :: nnebiter
    integer(i4),                            save :: nnebreplicatot 
    integer(i4), dimension(:),     pointer, save :: nnebreplica => null()
    integer(i4), dimension(:),     pointer, save :: nnebreplicano => null()
    integer(i4), dimension(:),     pointer, save :: nebreplicacfgptr => null()
    integer(i4),                            save :: nebtangent
    logical,                                save :: lnebdoublynudged
    logical,                                save :: lnebclimbingimage
    logical,     dimension(:),     pointer, save :: lnebvaryspring => null()
    real(dp),    dimension(:),     pointer, save :: nebspring => null()
    real(dp),    dimension(:),     pointer, save :: nebspringmin => null()
    real(dp),    dimension(:,:),   pointer, save :: nebfinalcell => null()
    real(dp),    dimension(:,:),   pointer, save :: nebfinalradius => null()
    real(dp),    dimension(:,:,:), pointer, save :: nebfinalxyz => null()
    real(dp),    dimension(:,:),   pointer, save :: nebreplicacell => null()
    real(dp),    dimension(:,:),   pointer, save :: nebreplicaradius => null()
    real(dp),    dimension(:,:,:), pointer, save :: nebreplicaxyz => null()
    real(dp),                               save :: nebmaxdisp
    real(dp),                               save :: nebrandom
    real(dp),                               save :: nebtol
  end module neb
!
!  Module for sharing maximum number of neighbours in bond order approaches
!
  module neighbours
    use datatypes
    integer(i4), save :: maxneigh = 12
  end module neighbours
!
!  Numbers
!
  module numbers
    use datatypes
    real(dp),                             save :: fct(30)                      ! Contains factorials up to 30!
    real(dp),                             save :: third                        ! Contains the value of the fraction 1/3
    real(dp),                             save :: sixth                        ! Contains the value of the fraction 1/6
  end module numbers
!
!  Observables data for fitting
!
  module observables
    use datatypes
    integer(i4),                            save :: maxfgrad = 10
    integer(i4),                            save :: maxfstress = 6
    integer(i4),                            save :: maxobs = 10
    integer(i4),                            save :: maxobsmode = 1
    integer(i4), dimension(:),     pointer, save :: nfgracfg => null()
    integer(i4), dimension(:),     pointer, save :: nfgrat => null()
    integer(i4), dimension(:),     pointer, save :: nfstrcfg => null()
    integer(i4), dimension(:),     pointer, save :: nfstrt => null()
    integer(i4), dimension(:),     pointer, save :: nobcfg => null()
    integer(i4), dimension(:),     pointer, save :: nobptr => null()
    integer(i4), dimension(:),     pointer, save :: nobptr2 => null()
    integer(i4), dimension(:),     pointer, save :: nobptr3 => null()
    integer(i4), dimension(:),     pointer, save :: nobtyp => null()
    integer(i4), dimension(:),     pointer, save :: nobsmodeat => null()   ! Number of atoms whose eigenvectors are input
    integer(i4), dimension(:),     pointer, save :: nobsmodecfg => null()  ! Number of mode observables per configuration
    real(dp),    dimension(:),     pointer, save :: fcalc => null()        ! Array for calculated value of observable
    real(dp),    dimension(:),     pointer, save :: fcalcoriginal => null()
    real(dp),    dimension(:),     pointer, save :: fgrad => null()
    real(dp),    dimension(:),     pointer, save :: fgradweight => null()
    real(dp),    dimension(:),     pointer, save :: finfo => null()        ! Array for information related to calculate value of observable
    real(dp),    dimension(:),     pointer, save :: fstress => null()
    real(dp),    dimension(:),     pointer, save :: fstressweight => null()
    real(dp),    dimension(:),     pointer, save :: fobs => null()
    real(dp),    dimension(:,:,:), pointer, save :: fobsmode => null()     ! Eigenvectors for mode observables
    real(dp),    dimension(:),     pointer, save :: fobsmodefreq => null() ! Frequencies for mode observables
    real(dp),    dimension(:),     pointer, save :: fobsmodeover => null() ! Overlaps for mode observables
    real(dp),    dimension(:),     pointer, save :: fparameter => null()
    real(dp),    dimension(:,:),   pointer, save :: freaction => null()
    real(dp),    dimension(:),     pointer, save :: weight => null()
    character(len=13),                      save :: nameobs(29)
    integer(i4),                            save :: nfgrad
    integer(i4),                            save :: nfstress
    integer(i4),                            save :: nobs
    integer(i4),                            save :: nobsmode             ! Number of observables that have eigenvectors for a mode
    real(dp),                               save :: delwht
    real(dp),                               save :: delwht_angle         ! Default fitting weight for angles
    real(dp),                               save :: delwht_bond          ! Default fitting weight for bonds
    real(dp),                               save :: delwht_cell_angle    ! Default fitting weight for cell angles
    real(dp),                               save :: delwht_cell_length   ! Default fitting weight for cell lengths
    real(dp),                               save :: delwht_coord         ! Default fitting weight for Cartesian coordinates
    real(dp),                               save :: delwht_dielectric    ! Default fitting weight for dielectric, refractive and piezos
    real(dp),                               save :: delwht_elastic       ! Default fitting weight for elastic constants
    real(dp),                               save :: delwht_energy        ! Default fitting weight for energies
    real(dp),                               save :: delwht_frac          ! Default fitting weight for fractional coordinates
    real(dp),                               save :: delwht_freq          ! Default fitting weight for frequencies
    real(dp),                               save :: delwht_grad          ! Default fitting weight for gradients
    real(dp),                               save :: delwht_modulus       ! Default fitting weight for moduli
    real(dp),                               save :: delwht_stress        ! Default fitting weight for stresses
    data nameobs/'Energy       ','Derivative   ','Elastic Const', &
                 'High Freq DiC','Static Di C  ','Structure    ', &
                 'Piezo strain ','Piezo stress ','Frequency    ', &
                 'Potential    ','Bulk modulus ','Shear modulus', &
                 'Heat Capacity','Entropy      ','High Freq R.I', &
                 'Static R. I. ','S(Q,omega)   ','Born charge  ', &
                 'Monopole Q   ','Stress       ','ReaxFF charge', &
                 'Bond length  ','Bond angle   ','Reactn energy', &
                 'Youngs moduli','Poisson ratio','Coordinatn no', &
                 'Abs dipole mt','Mode         '/
  end module observables
!
!  One-body potential data
!
  module one
    use datatypes
    integer(i4),                               save :: maxone = 10
    character(len=5), dimension(:),   pointer, save :: symbol1 => null()
    integer(i4),      dimension(:),   pointer, save :: nptyp11 => null()
    integer(i4),      dimension(:),   pointer, save :: nspec11 => null()
    real(dp),         dimension(:),   pointer, save :: onepot => null()
    integer(i4),                               save :: none
  end module one
!
!  Optimisation related data
!
  module optimisation
    use datatypes
    logical,     dimension(:),   pointer, save :: lopf => null()
    real(dp),    dimension(:),   pointer, save :: rmode => null()
    integer(i4),                          save :: lmbfgsorder          ! Order of LM-BFGS algorithm
    integer(i4),                          save :: maxcal
    integer(i4),                          save :: maxline
    integer(i4),                          save :: minchcrit
    integer(i4),                          save :: minchtyp
    integer(i4),                          save :: mintype
    integer(i4),                          save :: mode
    integer(i4),                          save :: morder
    integer(i4),                          save :: nlinmin
    integer(i4),                          save :: nupdate              ! Hessian updating frequency
    logical,                              save :: lfix1atom            ! If true then one atom should be fixed during optimisation
    logical,                              save :: lfreeze
    logical,                              save :: lminch
    logical,                              save :: loptcellpar
    logical,                              save :: loptsuccess          ! If true then optimisation was successful
    real(dp),                             save :: chcrit
    real(dp),                             save :: cosine
    real(dp),                             save :: delfc
    real(dp),                             save :: ftol                 ! Function (energy) tolerance for convergence
    real(dp),                             save :: gdcrit
    real(dp),                             save :: grmax
    real(dp),                             save :: gnorm
    real(dp),                             save :: gtol                 ! Gradient tolerance for convergence
    real(dp),                             save :: lmgtol
    real(dp),                             save :: stepmax              ! Maximum step size
    real(dp),                             save :: stepmin
    real(dp),                             save :: xtol                 ! Coordinate tolerance for convergence
  end module optimisation
!
!  Parallel execution related data
!
!  nprocs        = number of processors 
!  procid        = local processor number 
!  ioproc        = is the local node the I/O node?
!  MPI_comm_GULP = MPI communicator for current process
!
  module parallel
    use datatypes
    integer(i4),                          save :: natomsonnode
    integer(i4), dimension(:), pointer,   save :: atom2node => null()
    integer(i4), dimension(:), pointer,   save :: node2atom => null()
    integer(i4),                          save :: nprocs                      ! Number of processors for parallel execution
    integer(i4),                          save :: procid                      ! Number for local processor
    logical,                              save :: ioproc                      ! If true, this is the I/O processor
! NOTE: The following variable uses an explicit precision since it must match MPI
    integer*4,                            save :: MPI_comm_GULP               ! MPI communicator under which GULP is running
  end module parallel
!
!  Partial occupancy data
!
  module partial
    use datatypes
    integer(i4),                          save :: nbs
    integer(i4),                          save :: nbss
    integer(i4),                          save :: nbfoc
    integer(i4),                          save :: nbsfoc
    integer(i4),                          save :: ncfoc
    integer(i4),                          save :: ncsfoc
    integer(i4),                          save :: nsfoc
    integer(i4), dimension(:), pointer,   save :: nbsptr => null()
    integer(i4), dimension(:), pointer,   save :: ibocptr => null()
    integer(i4), dimension(:), pointer,   save :: ibocshptr => null()
    integer(i4), dimension(:), pointer,   save :: iocptr => null()
    integer(i4), dimension(:), pointer,   save :: iocshptr => null()
    logical,                              save :: lpocc
  end module partial
!
!  Phonon output data
!
  module phonout
    use datatypes
    integer(i4),                          save :: nkfin(10)
    integer(i4),                          save :: nbox
    integer(i4),                          save :: nkline
    integer(i4),                          save :: nlbox
    real(dp),                             save :: fbox
    real(dp),                             save :: flbox
  end module phonout
!
!  Plane potential data
!
  module plane
    use datatypes
    integer(i4),                               save :: maxplanepot = 0
    integer(i4),                               save :: nplanepot
    character(len=5), dimension(:),   pointer, save :: planepotsymbol => null()
    integer(i4),      dimension(:,:), pointer, save :: nplanepotpower => null()
    integer(i4),      dimension(:),   pointer, save :: nplanepottype => null()
    integer(i4),      dimension(:),   pointer, save :: natplanepot => null()
    integer(i4),      dimension(:),   pointer, save :: ntypplanepot => null()
    real(dp),         dimension(:,:), pointer, save :: planepot => null()
    real(dp),         dimension(:),   pointer, save :: planepotrmin => null()
    real(dp),         dimension(:),   pointer, save :: planepotrmax => null()
  end module plane
!
!  Plumed variables
!
  module plumed
    use datatypes
    character(len=80),                         save :: plumedfile        ! Name of plumed input file
    logical,                                   save :: lplumed           ! If true then call plumed
    logical,                                   save :: lplumed_available ! If true then plumed can be called
  end module plumed
!
!  Point group data
!
!      module pointgroups
!        use datatypes
!        character(len=4),                     save :: pgsymmetrylabel(47)
!        integer(i4),                          save :: nchartables(12,12,32)
!        integer(i4),                          save :: nchartablelabel(12,32)
!        integer(i4),                          save :: nchartableoperator(12,32)
!        integer(i4),                          save :: nchartableops(32)
!        data nchartableops/1,2,2,2,4,4,4,8,4,4,8,6,6,6,12
!        data pgsymmetrylabel/'A   ','Ag  ','Au  ','A!  ','A!! ','B   ','E   ','E1  ','E2  ',
!     &    'A1  ','A2  ','B1  ','B2  ','Bg  ','Bu  ','E!  ','E!! ','Eg  ','Eu  ','B3  ','A1g ',
!     &    'A2g ','A1u ','A2u ','B1g ','B2g ','B3g ','B1u ','B2u ','B3u ','A1! ','A2! ','A1!!',
!     &    'A2!!','E1g ','E2g ','E1u ','E2u ','T   ','Tg  ','Tu  ','T1  ','T2  ','T1g ','T2g ',
!     &    'T1u ','T2u '/
!      end module pointgroups
!
!  Polarisability data
!
  module polarise
    use datatypes
    integer(i4), dimension(:),   pointer, save :: natpolspec => null()
    integer(i4), dimension(:),   pointer, save :: ntyppolspec => null()
    real(dp),    dimension(:),   pointer, save :: dpolar => null()
    real(dp),    dimension(:),   pointer, save :: qpolar => null()
    real(dp),    dimension(:),   pointer, save :: dpolspec => null()
    real(dp),    dimension(:),   pointer, save :: qpolspec => null()
    integer(i4),                          save :: npolspec
    logical,                              save :: lpolar
    logical,                              save :: lqpolar
  end module polarise
!
!  Potential change pointer for boundary corrections
!
  module potchange
    use datatypes
    integer(i4), dimension(:),   pointer, save :: npchng
    integer(i4),                          save :: nchng
  end module potchange
!
!  Potential grid points
!
  module potentialgrid
    use datatypes
    integer(i4), dimension(:),   pointer, save :: nxpg => null()
    integer(i4), dimension(:),   pointer, save :: nypg => null()
    integer(i4), dimension(:),   pointer, save :: nzpg => null()
    real(dp),    dimension(:),   pointer, save :: xmaxpg => null()
    real(dp),    dimension(:),   pointer, save :: xminpg => null()
    real(dp),    dimension(:),   pointer, save :: ymaxpg => null()
    real(dp),    dimension(:),   pointer, save :: yminpg => null()
    real(dp),    dimension(:),   pointer, save :: zmaxpg => null()
    real(dp),    dimension(:),   pointer, save :: zminpg => null()
  end module potentialgrid
!
!  Potential interpolation
!
  module potentialinterpolation
    use datatypes
    logical,                              save :: lpotlinterpolate
    integer(i4),                          save :: maxptsinterpolate = 0
    integer(i4),                          save :: nptsinterpolate
    real(dp),    dimension(:),   pointer, save :: dRinterpolate => null()
    real(dp),    dimension(:),   pointer, save :: rdRinterpolate => null()
    real(dp),    dimension(:,:), pointer, save :: FofR => null()
    real(dp),    dimension(:,:), pointer, save :: dFofR => null()
  end module potentialinterpolation
!
!  Potential names
!
  module potentialnames
    use datatypes
    integer(i4),                          save :: maxpottype = 54
    character(len=13),                    save :: namepot(54)             ! Names of twobody potentials
    data namepot/'Buckingham   ','Lennard      ','Morse        ', &
                 'Morse-Coulomb','Harmonic     ','Harm-Coulomb ', &
                 'General      ','Spring (c-s) ','Coulomb      ', &
                 'Buck 4 range ','Spline       ','Lenn/es      ', &
                 'Lenn/es      ','BSM          ','Still-Weber 2', &
                 'Inverse Gauss','BSM exponentl','Damped dispn ', &
                 'Many-body    ','Rydberg      ','Lenn/ESFF    ', &
                 'Qtaper       ','Polynomial   ','Qerfc        ', &
                 'CovExp       ','Fermi-Dirac  ','L-J buffered ', &
                 'SqrdHarmonic ','SqrdHarm-Coul','Tsuneyuki    ', &
                 'BSM single ex','SW2JiangBrown','Cosh spring  ', &
                 'EAM pot shift','Poly harmonic','QoverR2      ', &
                 'Force const  ','SRGlue       ','Morse-etaper ', &
                 'Morse-etap-Cb','Mei-Davenport','Erf-erfc     ', &
                 'Rep-erfc     ','Erf-pot      ','Baskes       ', &
                 'VBO_twobody  ','Exp-powers   ','Grimme_C6    ', &
                 'CFM harmonic ','CFM Gaussian ','CFM power-law', &
                 'CFM Fermi    ','gCoulomb     ','Becke-Jhsn_C6'/
  end module potentialnames
!
!  Potential points
!
  module potentialpoints
    use datatypes
    integer(i4),                          save :: maxppt = 1
    integer(i4), dimension(:),   pointer, save :: npotptcfg => null()
    real(dp),    dimension(:),   pointer, save :: xpotpt => null()
    real(dp),    dimension(:),   pointer, save :: ypotpt => null()
    real(dp),    dimension(:),   pointer, save :: zpotpt => null()
    real(dp),    dimension(:),   pointer, save :: vpotpt => null()
    integer(i4),                          save :: npotpt
    integer(i4),                          save :: npotpt0
  end module potentialpoints
!
!  Potential sites
!
  module potentialsites
    use datatypes
    integer(i4),                          save :: maxpotsites = 1
    integer(i4), dimension(:),   pointer, save :: npotsitecfg => null()
    real(dp),    dimension(:),   pointer, save :: xpotsite => null()
    real(dp),    dimension(:),   pointer, save :: ypotsite => null()
    real(dp),    dimension(:),   pointer, save :: zpotsite => null()
    real(dp),    dimension(:),   pointer, save :: vpotsite => null()
    integer(i4),                          save :: npotsites
  end module potentialsites
!
!  Electrostatic potential for current structure
!
  module potentialxyz
    use datatypes
    real(dp),    dimension(:,:), pointer, save :: v2xyz => null()
    real(dp),    dimension(:),   pointer, save :: vx => null()
    real(dp),    dimension(:),   pointer, save :: vy => null()
    real(dp),    dimension(:),   pointer, save :: vz => null()
    real(dp),    dimension(:,:), pointer, save :: v2xyz12 => null()
    real(dp),    dimension(:),   pointer, save :: vx12 => null()
    real(dp),    dimension(:),   pointer, save :: vy12 => null()
    real(dp),    dimension(:),   pointer, save :: vz12 => null()
  end module potentialxyz
!
!  Projection of phonon density of states data
!
  module projectdos
    use datatypes
    integer(i4),                          save :: maxproj = 1
    integer(i4),                          save :: maxproji = 5
    integer(i4), dimension(:),   pointer, save :: nprojit => null()
    integer(i4), dimension(:),   pointer, save :: nprojcfg => null()
    integer(i4), dimension(:),   pointer, save :: nprojnat => null()
    integer(i4), dimension(:),   pointer, save :: nprojtyp => null()
    integer(i4), dimension(:),   pointer, save :: nprojdb => null()
    integer(i4), dimension(:),   pointer, save :: nprojdef => null()
    integer(i4), dimension(:),   pointer, save :: nprojptr => null()
  end module projectdos
!
!  Properties
!
  module properties
    use datatypes
    real(dp),                             save :: bulkmod
    real(dp),                             save :: shearmod
    real(dp),                             save :: cv
    real(dp),                             save :: entropy
    real(dp),                             save :: diconh(3,3)
    real(dp),                             save :: dicons(3,3)
    real(dp),                             save :: elcon(6,6)
    real(dp),                             save :: hfrefind(3)
    real(dp),                             save :: srefind(3)
    real(dp),                             save :: piezo(6,3)
    real(dp),                             save :: piezs(6,3)
    real(dp),                             save :: ym(3)
    real(dp),                             save :: poissonratio(3)
  end module properties
!
!  Coulomb matrix element data
!
  module qmedata
    use datatypes
    integer(i4), save :: maxloop(3)
    real(dp),    save :: rmax2
  end module qmedata
!
!  Radial force data
!
  module radial
    use datatypes
    logical,     dimension(:),   pointer, save :: lradialcfg => null()
    real(dp),    dimension(:),   pointer, save :: radialKcfg => null()
    real(dp),    dimension(:,:), pointer, save :: radialXYZcfg => null()
  end module radial
!
!  Random number counts - needed for MC/MD restart
!
  module randomnumbers
    use datatypes
    integer(i4),                                   save :: nrandomcalls = 0
    integer(i4),                                   save :: npr_randomcalls = 0
    integer(i4),                                   save :: npr_grandomcalls = 0
    integer(i4),                                   save :: npr_randomcalls_adv = 0
    integer(i4),                                   save :: npr_grandomcalls_adv = 0
    logical,                                       save :: lGaussianLast = .false.
  end module randomnumbers
!
!  ReaxFF data
!
  module reaxFFdata
    use datatypes
    integer(i4),                                   save :: maxreaxFFspec = 1
    integer(i4),                                   save :: maxreaxFFfixQspec = 1
    integer(i4),                                   save :: maxreaxFFval3 = 2
    integer(i4),                                   save :: nreaxFFspec = 0
    integer(i4),                                   save :: nreaxFFfixQspec = 0
    integer(i4),                                   save :: nlibreaxFFspec = 0
    integer(i4),                                   save :: nlibreaxFFfixQspec = 0
    integer(i4),                                   save :: nreaxFFqiter = 50   ! Number of iterations for charge solution
    character(len=5), dimension(:),       pointer, save :: symbolreaxFFspec => null()
    integer(i4),      dimension(:),       pointer, save :: nreaxFFfixQspecptr => null()
    integer(i4),      dimension(:),       pointer, save :: natreaxFFspec => null()
    integer(i4),      dimension(:),       pointer, save :: ntypreaxFFspec => null()
    integer(i4),      dimension(:,:),     pointer, save :: nreaxFFval3 => null()
    logical,          dimension(:,:),     pointer, save :: lreaxFFbocorrect => null()
    logical,          dimension(:),       pointer, save :: lreaxFFmorseinput => null()
    logical,          dimension(:),       pointer, save :: lreaxFFpboOK => null()        ! Flag to indicate whether bond order parameters have been set in reaxFFpbo
    logical,          dimension(:,:),     pointer, save :: lreaxFFtorsinput => null()
    real(dp),         dimension(:),       pointer, save :: qreaxFF => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFr => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFalpha => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFeps => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFrvdw => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFgammaw => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFpover => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFpunder => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFhincrement => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFlp => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFmorse => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFoc1 => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFoc2 => null()
    real(dp),         dimension(:),       pointer, save :: reaxFFuc1 => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFpboc => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFval => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFval1 => null()
    real(dp),         dimension(:,:,:,:), pointer, save :: reaxFFval3 => null()
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFconj3 => null()
    real(dp),         dimension(:,:,:,:), pointer, save :: reaxFFhb3 => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFpen2 => null()           ! Pairwise bond order penalty
    real(dp),         dimension(:,:),     pointer, save :: reaxFFpen3 => null()
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFtor4 => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFDe => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFpbe => null()
    real(dp),         dimension(:,:),     pointer, save :: reaxFFpbo => null()            ! Pairwise bond order parameters
    real(dp),         dimension(:),       pointer, save :: reaxFFDeVDW => null()          ! Pairwise De for VDW computed at run time
    real(dp),         dimension(:),       pointer, save :: reaxFFalphaVDW => null()       ! Pairwise alpha for VDW computed at run time
    real(dp),         dimension(:),       pointer, save :: reaxFFr0VDW => null()          ! Pairwise r0 for VDW computed at run time
    real(dp),         dimension(:),       pointer, save :: reaxFFgammaVDW => null()       ! Pairwise gamma for VDW computed at run time
    real(dp),         dimension(:),       pointer, save :: reaxFFgammaQ => null()         ! Pairwise gamma for Coulomb term  computed at run time
    real(dp),         dimension(:),       pointer, save :: reaxFFrmax => null()           ! Maximum radius for computing bond order per species
    real(dp),         dimension(:),       pointer, save :: reaxFFrmaxpair => null()       ! Maximum radius for computing bond order for a pair
    real(dp),                                      save :: reaxFFlam(31)                  ! Lambda parameters of the reaxFF method
    real(dp),                                      save :: reaxFFcutoff                   ! Cutoff for smoothed bond order
    real(dp),                                      save :: reaxFFcutoffVDW                ! Cutoff for VDW term
    real(dp),                                      save :: reaxFFcutoffQ                  ! Cutoff for Coulomb term
    real(dp),                                      save :: reaxFFqconverged               ! Convergence criterion for iterative charge solution - average
    real(dp),                                      save :: reaxFFqconverged1              ! Convergence criterion for iterative charge solution - individual
    real(dp),                                      save :: reaxFFqdamp                    ! Damping factor for iterative charge solution
    real(dp),                                      save :: reaxFFtaperQ(8)                ! Taper coefficients for Coulomb term
    real(dp),                                      save :: reaxFFtaperVDW(8)              ! Taper coefficients for VDW term
    real(dp),                                      save :: reaxFFtapermin                 ! Minimum distance for taper
    real(dp),                                      save :: reaxFFtaperscale               ! Scale factor for tapering upper bound relative to tolerance
    real(dp),                                      save :: reaxFFtol                      ! Tolerance for neglecting contributions in pairwise/bond orders
    real(dp),                                      save :: reaxFFatol                     ! Tolerance for neglecting contributions for many body terms
    real(dp),                                      save :: reaxFFatol2                    ! Tolerance for neglecting contributions in valence based on bond order product
    real(dp),                                      save :: reaxFFatol3                    ! Tolerance for neglecting contributions in torsion based on bond order product
    real(dp),                                      save :: reaxFFhtol                     ! Tolerance for neglecting contributions for hydrogen bond terms
    real(dp),                                      save :: reaxFFrhtol                    ! Distance tolerance for neglecting contributions for hydrogen bond terms
!
!  reaxFFlam contains species independent parameters for the energy expressions
!
!   1 = p_boc1
!   2 = p_boc2
!   3 = p_coa2
!   4 = p_coa3
!   5 = p_coa4
!   6 = p_ovun3
!   7 = p_ovun6
!   8 = p_ovun7
!   9 = p_ovun8
!  10 = 
!  11 = 
!  12 = 
!  13 = 
!  14 = 
!  15 = p_val6
!  16 = p_val8
!  17 = p_val9
!  18 = p_val10
!  19 = 
!  20 = p_pen2
!  21 = p_pen3
!  22 = p_pen4
!  23 =
!  24 = p_tor2
!  25 = p_tor3
!  26 = p_tor4
!  27 = p_cot2
!  28 = p_vdw1
!  29 = p_lp1
!  30 = exponent in exponential for lone pair energy (formally fixed at 75)
!  31 = p_ovun4
!
    data reaxFFlam/50.00_dp,4.3822_dp,5.02_dp,18.32_dp,8.32_dp,5.6937_dp,1.0053_dp, &
      7.6280_dp,14.5067_dp,12.38_dp,1.49_dp,1.28_dp,6.3_dp,2.72_dp,33.8667_dp,2.5067_dp, &
      1.1177_dp,1.9645_dp,36.0_dp,6.6623_dp,0.1809_dp,3.9954_dp,3.17_dp,4.8815_dp,10.0_dp, &
      2.3276_dp,1.7905_dp,1.5591_dp,25.6125_dp,75.0_dp,1.6356_dp/
    data EconjTolerance/0.00001_dp/
  end module reaxFFdata
!
!  Real space scratch arrays
!
  module realvectors
    use datatypes
    integer(i4), dimension(:,:), pointer, save :: cellindex => null()
    integer(i4), dimension(:),   pointer, save :: nbotype => null()
    integer(i4), dimension(:),   pointer, save :: nbotype2 => null()
    logical,     dimension(:),   pointer, save :: lbonded => null()         ! If true then atoms are bonded
    logical,     dimension(:),   pointer, save :: l2bonds => null()         ! If true then atoms are connected via bonds to a common central atom
    logical,     dimension(:),   pointer, save :: l3bonds => null()         ! If true then atoms are connected via three bonds
    logical,     dimension(:),   pointer, save :: lptrmol => null()
    real(dp),    dimension(:),   pointer, save :: deriv => null()           ! First derivative w.r.t. distance
    real(dp),    dimension(:),   pointer, save :: deriv2 => null()          ! Second derivative w.r.t. distance
    real(dp),    dimension(:),   pointer, save :: deriv3 => null()          ! Third derivative w.r.t. distance
    real(dp),    dimension(:),   pointer, save :: derive0 => null()
    real(dp),    dimension(:),   pointer, save :: derive => null()          ! First derivative w.r.t. distance of electrostatic contribution
    real(dp),    dimension(:),   pointer, save :: derive2 => null()         ! Second derivative w.r.t. distance of electrostatic contribution
    real(dp),    dimension(:),   pointer, save :: derive3 => null()         ! Thurd derivative w.r.t. distance of electrostatic contribution
    real(dp),    dimension(:),   pointer, save :: dist => null()
    real(dp),    dimension(:),   pointer, save :: dist2 => null()
    real(dp),    dimension(:),   pointer, save :: dist3 => null()
    real(dp),    dimension(:),   pointer, save :: d0i => null()
    real(dp),    dimension(:),   pointer, save :: d0j => null()
    real(dp),    dimension(:),   pointer, save :: d1i => null()
    real(dp),    dimension(:),   pointer, save :: d1j => null()
    real(dp),    dimension(:),   pointer, save :: d2i2 => null()
    real(dp),    dimension(:),   pointer, save :: d2ij => null()
    real(dp),    dimension(:),   pointer, save :: d2j2 => null()
    real(dp),    dimension(:),   pointer, save :: rderiv => null()          ! First derivative w.r.t. radius
    real(dp),    dimension(:,:), pointer, save :: rpd => null()             ! Contains strain products (two Cartesian coordinates)
    real(dp),    dimension(:),   pointer, save :: rtrm2 => null()
    real(dp),    dimension(:),   pointer, save :: rtrm3 => null()
    real(dp),    dimension(:),   pointer, save :: rtrm32 => null()
    real(dp),    dimension(:),   pointer, save :: xtmp => null()            ! Cartesian x coordinate of vector
    real(dp),    dimension(:),   pointer, save :: ytmp => null()            ! Cartesian y coordinate of vector
    real(dp),    dimension(:),   pointer, save :: ztmp => null()            ! Cartesian z coordinate of vector
    real(dp),    dimension(:),   pointer, save :: xtmp2 => null()
    real(dp),    dimension(:),   pointer, save :: ytmp2 => null()
    real(dp),    dimension(:),   pointer, save :: ztmp2 => null()
    real(dp),    dimension(:),   pointer, save :: xtmp3 => null()
    real(dp),    dimension(:),   pointer, save :: ytmp3 => null()
    real(dp),    dimension(:),   pointer, save :: ztmp3 => null()
  end module realvectors
!
!  Region 2a data
!
  module region2a
    use datatypes
    integer(i4),                          save :: maxr2at = 0
    integer(i4), dimension(:),   pointer, save :: nr2a => null()
    integer(i4), dimension(:),   pointer, save :: ntr2a => null()
    integer(i4), dimension(:),   pointer, save :: nmr2a => null()
    integer(i4), dimension(:),   pointer, save :: nmir2a => null()
    integer(i4), dimension(:),   pointer, save :: nps => null()
    integer(i4), dimension(:),   pointer, save :: ndsptr2a => null()
    integer(i4), dimension(:),   pointer, save :: ndeqv2a => null()
    integer(i4), dimension(:),   pointer, save :: ndrel2a => null()
    integer(i4), dimension(:),   pointer, save :: ndrelop2a => null()
    logical,     dimension(:),   pointer, save :: ldbr2a => null()
    real(dp),    dimension(:,:), pointer, save :: dscrhor2d => null()
    real(dp),    dimension(:,:), pointer, save :: dscrhor2p => null()
    real(dp),    dimension(:),   pointer, save :: xdis => null()
    real(dp),    dimension(:),   pointer, save :: ydis => null()
    real(dp),    dimension(:),   pointer, save :: zdis => null()
    real(dp),    dimension(:),   pointer, save :: xr2a => null()
    real(dp),    dimension(:),   pointer, save :: yr2a => null()
    real(dp),    dimension(:),   pointer, save :: zr2a => null()
    real(dp),    dimension(:),   pointer, save :: qr2a => null()
    real(dp),    dimension(:),   pointer, save :: or2a => null()
    real(dp),    dimension(:),   pointer, save :: rr2a => null()
    integer(i4),                          save :: ndasym2a
    integer(i4),                          save :: ndpasym2a
  end module region2a
!
!  Scan data
!
  module scan
    use datatypes
    integer(i4), dimension(:),   pointer, save :: ntran => null()
    logical,     dimension(:),   pointer, save :: ltranat => null()
    real(dp),    dimension(:),   pointer, save :: xtran => null()
    real(dp),    dimension(:),   pointer, save :: ytran => null()
    real(dp),    dimension(:),   pointer, save :: ztran => null()
  end module scan
!
!  Neutron scattering data
!
  module scatterdata
    use datatypes
    use element
    integer(i4), parameter :: maxisotopes=15
    integer(i4),                                save :: maxnq_step_fit = 1
    integer(i4),                                save :: maxnw_step_fit = 1
!  holding array sizes, to be passed to realloc when required
    integer(i4),                                save :: maxHold1 = 1
    integer(i4),                                save :: maxHold2 = 1
    integer(i4),                                save :: maxHold3 = 1
    integer(i4),                                save :: maxHold4 = 1
    integer(i4),                                save :: maxqvector = 1
!
    logical,     dimension(9),                  save :: lscatopt
    logical,     dimension(9),                  save :: lscatanopt
    logical,                                    save :: lscatoptflag
    logical,                                    save :: lscatanoptflag
    logical,                                    save :: lscatsurfaceweight
    logical,                                    save :: lscattercall         ! Indicates whether a phonon call is from scatter or not
    logical,                                    save :: lscat_xeig           ! Exclude output on eigen - scatter
!
    logical,                                    save :: out_eig_flag
!
    character(len=80),                          save :: q_filename                         
    character(len=80),                          save :: sofomega_filename
!
    integer(i4),                                save :: nq_step
    integer(i4),                                save :: nq_step_fit
    integer(i4),                                save :: nq_intstep
    integer(i4),                                save :: n_qvector
    integer(i4), dimension(:),         pointer, save :: n_qpoint
    integer(i4),                                save :: n_qpointcurr
    integer(i4),                                save :: n_qpointmax
    integer(i4),                                save :: n_freqs
    integer(i4),                                save :: q_runtype
    integer(i4),                                save :: q_caltype
    integer(i4),                                save :: nw_step
    integer(i4),                                save :: nw_step_fit
!  global 'zero' parameter for use with checking for zeros
    real(dp),    parameter :: globenull_chk = 1d-10
!
    real(dp),                                   save :: theta_initial
    real(dp),                                   save :: phi_initial
    real(dp),    dimension(:,:),       pointer, save :: q_ordinate => null()
    real(dp),                                   save :: q_qmax
    real(dp),                                   save :: q_qmin
    real(dp),                                   save :: q_wmax
    real(dp),                                   save :: q_wmin
    real(dp),    dimension(:,:,:,:),   pointer, save :: Hold_Q => null()
    real(dp),    dimension(:,:,:,:),   pointer, save :: Hold_smq => null()
    real(dp),    dimension(:,:,:,:),   pointer, save :: Hold_tau => null()
! need to change bcoh, binc into complex format for added accuracy -- 9/9/08
    real(dp),                                   save :: b_coh(maxisotopes,maxele)
    real(dp),                                   save :: b_inc(maxisotopes,maxele)
    real(dp),                                   save :: b_mass(maxisotopes,maxele)
    real(dp),    dimension(:,:),       pointer, save :: Qvector => null()
    real(dp),    dimension(:),         pointer, save :: scatlencoh => null()
    real(dp),    dimension(:),         pointer, save :: scatleninc => null()
    real(dp),    dimension(:,:),       pointer, save :: sofomega => null()
    real(dp),    dimension(:,:),       pointer, save :: sofomega_fit => null()
    real(dp),    dimension(:,:),       pointer, save :: tauvector => null()
  end module scatterdata  
!
!  Shell model related data
!
  module shell
    use datatypes
    integer(i4), dimension(:),   pointer, save :: ncsptr => null()
    integer(i4), dimension(:),   pointer, save :: ncoptr => null()
    integer(i4), dimension(:),   pointer, save :: nshptr => null()
    integer(i4), dimension(:),   pointer, save :: natratiomspec => null()
    integer(i4), dimension(:),   pointer, save :: ntypratiomspec => null()
    integer(i4),                          save :: moptit
    integer(i4),                          save :: ncore
    integer(i4),                          save :: ncorer1
    integer(i4),                          save :: nshell
    integer(i4),                          save :: nshellr1
    integer(i4),                          save :: ncuts
    integer(i4),                          save :: ncutss
    integer(i4),                          save :: ncus
    integer(i4),                          save :: nratiomspec
    real(dp),    dimension(:),   pointer, save :: ratiom => null()
    real(dp),    dimension(:),   pointer, save :: ratiomspec => null()
    real(dp),    dimension(:,:), pointer, save :: csvector => null()            ! Vector between core and shell
    real(dp),                             save :: cuts
    real(dp),                             save :: sgtol
  end module shell
!
!  Shell extrapolation for MD
!
  module shellextrapolation
    use datatypes
    logical,                              save :: lextrapolateshells
    integer(i4),                          save :: maxextrapol = 1
    real(dp),    dimension(:,:), pointer, save :: xshellsave => null()
    real(dp),    dimension(:,:), pointer, save :: yshellsave => null()
    real(dp),    dimension(:,:), pointer, save :: zshellsave => null()
  end module shellextrapolation
!
!  Shifts for energy
!
  module shifts
    use datatypes
    integer(i4), dimension(:),   pointer, save :: nshcfg => null()
    real(dp),    dimension(:),   pointer, save :: shift => null()
    real(dp),    dimension(:),   pointer, save :: shscalecfg => null()
    integer(i4),                          save :: nshift
  end module shifts
!
!  Six-body potential data
!
  module six
    use datatypes
    character(len=5), dimension(:,:), pointer, save :: symbol6 => null()
    integer(i4),                               save :: maxlist6 = 1
    integer(i4),                               save :: maxsix = 10
    integer(i4),      dimension(:),   pointer, save :: icell61 => null()
    integer(i4),      dimension(:),   pointer, save :: icell62 => null()
    integer(i4),      dimension(:),   pointer, save :: icell63 => null()
    integer(i4),      dimension(:),   pointer, save :: icell64 => null()
    integer(i4),      dimension(:),   pointer, save :: icell65 => null()
    integer(i4),      dimension(:),   pointer, save :: ijind => null()
    integer(i4),      dimension(:),   pointer, save :: klind => null()
    integer(i4),      dimension(:),   pointer, save :: mnind => null()
    integer(i4),      dimension(:),   pointer, save :: mmsexc => null()
    integer(i4),      dimension(:,:), pointer, save :: n6botype => null()
    integer(i4),      dimension(:),   pointer, save :: nsixptr => null()
    integer(i4),      dimension(:),   pointer, save :: nsixty => null()
    integer(i4),      dimension(:),   pointer, save :: nsptyp1 => null()
    integer(i4),      dimension(:),   pointer, save :: nsptyp2 => null()
    integer(i4),      dimension(:),   pointer, save :: nsptyp3 => null()
    integer(i4),      dimension(:),   pointer, save :: nsptyp4 => null()
    integer(i4),      dimension(:),   pointer, save :: nsptyp5 => null()
    integer(i4),      dimension(:),   pointer, save :: nsptyp6 => null()
    integer(i4),      dimension(:),   pointer, save :: nsspec1 => null()
    integer(i4),      dimension(:),   pointer, save :: nsspec2 => null()
    integer(i4),      dimension(:),   pointer, save :: nsspec3 => null()
    integer(i4),      dimension(:),   pointer, save :: nsspec4 => null()
    integer(i4),      dimension(:),   pointer, save :: nsspec5 => null()
    integer(i4),      dimension(:),   pointer, save :: nsspec6 => null()
    integer(i4),      dimension(:),   pointer, save :: npsix => null()
    logical,          dimension(:),   pointer, save :: lsintra => null()
    logical,          dimension(:),   pointer, save :: lsinter => null()
    real(dp),         dimension(:),   pointer, save :: sixk => null()
    real(dp),         dimension(:),   pointer, save :: six1 => null()
    real(dp),         dimension(:),   pointer, save :: six2 => null()
    real(dp),         dimension(:),   pointer, save :: six3 => null()
    real(dp),         dimension(:),   pointer, save :: six4 => null()
    real(dp),         dimension(:),   pointer, save :: six5 => null()
    integer(i4),                               save :: nsix
    integer(i4),                               save :: nlist6md
  end module six
!
!  Spatial decomposition data
!
  module spatial
    use datatypes
    integer(i4),                          save :: maxatompernode = 0
    integer(i4),                          save :: maxcellpernode = 0
    integer(i4),                          save :: maxspcell = 0
    integer(i4),                          save :: maxnspcellattot = 0
    integer(i4),                          save :: natompernode
    integer(i4),                          save :: nbufferx
    integer(i4),                          save :: nbuffery
    integer(i4),                          save :: nbufferz
    integer(i4),                          save :: ncellpernode
    integer(i4),                          save :: ncellsearch(3)
    integer(i4), dimension(:),   pointer, save :: natomcell => null()
    integer(i4), dimension(:),   pointer, save :: natomnodeptr => null()
    integer(i4), dimension(:),   pointer, save :: ncellnodeptr => null()
    integer(i4),                          save :: nspcell(3)
    integer(i4), dimension(:),   pointer, save :: nspcellat => null()
    integer(i4), dimension(:),   pointer, save :: nspcellat1ptr => null()
    integer(i4), dimension(:),   pointer, save :: nspcellatptr => null()
    integer(i4), dimension(:),   pointer, save :: nspcell2atptr => null()
    integer(i4), dimension(:),   pointer, save :: nspcellatptrcell => null()
    integer(i4),                          save :: nspcellattot
    integer(i4),                          save :: nspcelltot
    integer(i4),                          save :: nspmax(3)
    integer(i4),                          save :: nspmin(3)
    logical,     dimension(:),   pointer, save :: lbuffercell => null()
    logical,                              save :: lspatialok
    logical,                              save :: lrcspatial_anisotropic
    real(dp),                             save :: rcspatial           ! Domain decomposition target size (isotropic)
    real(dp),                             save :: rcspatialx          ! Domain decomposition target size in x direction
    real(dp),                             save :: rcspatialy          ! Domain decomposition target size in y direction
    real(dp),                             save :: rcspatialz          ! Domain decomposition target size in z direction
    real(dp),                             save :: rnearestx
    real(dp),                             save :: rnearesty
    real(dp),                             save :: rnearestz
    real(dp),                             save :: spcell(3)
    real(dp),                             save :: spmin(3)
    real(dp),    dimension(:),   pointer, save :: xinbox => null()
    real(dp),    dimension(:),   pointer, save :: yinbox => null()
    real(dp),    dimension(:),   pointer, save :: zinbox => null()
  end module spatial
!
!  Spatial decomposition data - bond order data
!
  module spatialbo
    use datatypes
    integer(i4),                          save :: maxatompernodebo = 0
    integer(i4),                          save :: maxcellpernodebo = 0
    integer(i4),                          save :: maxspcellbo = 0
    integer(i4),                          save :: maxnspcellattotbo = 0
    integer(i4),                          save :: natompernodebo
    integer(i4),                          save :: nbufferxbo
    integer(i4),                          save :: nbufferybo
    integer(i4),                          save :: nbufferzbo
    integer(i4),                          save :: ncellpernodebo
    integer(i4),                          save :: ncellsearchbo(3)
    integer(i4), dimension(:),   pointer, save :: natomcellbo => null()
    integer(i4), dimension(:),   pointer, save :: natomnodeptrbo => null()
    integer(i4), dimension(:),   pointer, save :: ncellnodeptrbo => null()
    integer(i4),                          save :: nspcellbo(3)
    integer(i4), dimension(:),   pointer, save :: nspcellatbo => null()
    integer(i4), dimension(:),   pointer, save :: nspcellat1ptrbo => null()
    integer(i4), dimension(:),   pointer, save :: nspcellatptrbo => null()
    integer(i4), dimension(:),   pointer, save :: nspcellatptrcellbo => null()
    integer(i4),                          save :: nspcellattotbo
    integer(i4),                          save :: nspcelltotbo
    integer(i4),                          save :: nspmaxbo(3)
    integer(i4),                          save :: nspminbo(3)
    logical,     dimension(:),   pointer, save :: lbuffercellbo => null()
    logical,                              save :: lspatialBOok
    logical,                              save :: lrcspatialBO_anisotropic
    real(dp),                             save :: rcspatialbo         ! Domain decomposition target size (isotropic)
    real(dp),                             save :: rcspatialbox        ! Domain decomposition target size in x direction
    real(dp),                             save :: rcspatialboy        ! Domain decomposition target size in y direction
    real(dp),                             save :: rcspatialboz        ! Domain decomposition target size in z direction
    real(dp),                             save :: rnearestxbo
    real(dp),                             save :: rnearestybo
    real(dp),                             save :: rnearestzbo
    real(dp),                             save :: spcellbo(3)
    real(dp),                             save :: spminbo(3)
    real(dp),    dimension(:),   pointer, save :: xinboxbo => null()
    real(dp),    dimension(:),   pointer, save :: yinboxbo => null()
    real(dp),    dimension(:),   pointer, save :: zinboxbo => null()
  end module spatialbo
!
!  Atomic species related data
!
  module species
    use datatypes
    integer(i4),                          save :: maxspec = 20
    integer(i4), dimension(:),   pointer, save :: natspec => null()          ! Atomic number of each species
    integer(i4), dimension(:),   pointer, save :: numofspec => null()        ! Number of atoms of a species type
    integer(i4), dimension(:),   pointer, save :: ntypspec => null()         ! Type number of each species
    logical,     dimension(:),   pointer, save :: lbrspec => null()          ! If true, this species has a breathing shell
    logical,     dimension(:),   pointer, save :: ldefshspec => null()
    logical,     dimension(:),   pointer, save :: linspec => null()
    logical,     dimension(:),   pointer, save :: lqinspec => null()         ! If true then charges were read in for these species
    logical,     dimension(:),   pointer, save :: lmassinspec => null()      ! If true then masses were read in for these species
    logical,     dimension(:),   pointer, save :: lnmrinspec => null()       ! If true then NMR parameters were read in for these species
    logical,     dimension(:),   pointer, save :: lmask => null()
    real(dp),    dimension(:),   pointer, save :: c6spec => null()           ! C6 coefficient of this species
    real(dp),    dimension(:),   pointer, save :: qlspec => null()           ! Charge of this species
    real(dp),    dimension(:),   pointer, save :: massspec => null()         ! Mass of this species
    real(dp),    dimension(:,:), pointer, save :: nmrspec => null()          ! NMR parameters of this species
    real(dp),    dimension(:),   pointer, save :: radspec => null()          ! Radius of this species
    integer(i4),                          save :: nspec                      ! Number of species
  end module species
!
!  Spline data
!
  module splinedata
    use datatypes
    integer(i4), dimension(:),   pointer, save :: nsplpt => null()
    integer(i4), dimension(:),   pointer, save :: nsplty => null()
    real(dp),    dimension(:,:), pointer, save :: d1f => null()
    real(dp),    dimension(:,:), pointer, save :: d2f => null()
    real(dp),    dimension(:,:), pointer, save :: splf => null()
    real(dp),    dimension(:,:), pointer, save :: splr => null()
    integer(i4),                          save :: maxpts = 50
  end module splinedata
!
!  Spline weights for REBO bicubic splines
!
  module splineweights
    use datatypes
    integer(i4), save :: bicubicweights(16,16)
    data bicubicweights/ &
      1,0,-3,2,4*0,-3,0,9,-6,2,0,-6,4,8*0,3,0,-9,6,-2,0,6,-4, &
      10*0,9,-6,2*0,-6,4,2*0,3,-2,6*0,-9,6,2*0,6,-4, &
      4*0,1,0,-3,2,-2,0,6,-4,1,0,-3,2,8*0,-1,0,3,-2,1,0,-3,2, &
      10*0,-3,2,2*0,3,-2,6*0,3,-2,2*0,-6,4,2*0,3,-2, &
      0,1,-2,1,5*0,-3,6,-3,0,2,-4,2,9*0,3,-6,3,0,-2,4,-2, &
      10*0,-3,3,2*0,2,-2,2*0,-1,1,6*0,3,-3,2*0,-2,2, &
      5*0,1,-2,1,0,-2,4,-2,0,1,-2,1,9*0,-1,2,-1,0,1,-2,1, &
      10*0,1,-1,2*0,-1,1,6*0,-1,1,2*0,2,-2,2*0,-1,1/
  end module splineweights
!
!  Sutton-Chen (EAM) data
!
  module sutton
    use datatypes
    real(dp),    dimension(:,:),   pointer, save :: scrho => null()
    real(dp),    dimension(:,:),   pointer, save :: scrho12 => null()
    logical,                                save :: lsuttonc
    real(dp),                               save :: scmaxsearch
  end module sutton
!
!  Symmetry data
!
  module symmetry
    use datatypes
    integer(i4),                               save :: maxsymop = 48
    character(len=1), dimension(:,:), pointer, save :: hmssg => null()
    integer(i4),      dimension(:),   pointer, save :: ifhr => null()
    integer(i4),      dimension(:),   pointer, save :: iflags => null()
    integer(i4),      dimension(:),   pointer, save :: ifso => null()
    integer(i4),      dimension(:),   pointer, save :: iperm => null()
    integer(i4),      dimension(:),   pointer, save :: nccscfg => null()
    integer(i4),      dimension(:),   pointer, save :: nspcg => null()
    integer(i4),      dimension(:,:), pointer, save :: ivso => null()
    logical,          dimension(:),   pointer, save :: lsymset => null()
    character(len=40),                         save :: cl(45)
    character(len=12),                         save :: texc(6)
    character(len=9),                          save :: patter(24)
    character(len=16),                         save :: gronam(232)
    character(len=16),                         save :: altgnam(18,74)
    character(len=5),                          save :: trax(24)
    integer(i4),                               save :: icentfct(7)
    integer(i4),                               save :: inverse(48)
    integer(i4),                               save :: ipatgrp(232)
    integer(i4),                               save :: iptab(48,48)
    integer(i4),                               save :: ishorg(3,232)
    integer(i4),                               save :: it(3,48)
    integer(i4),                               save :: naltgnam(74)
    integer(i4),                               save :: ncs
    integer(i4),                               save :: nccs
    integer(i4),                               save :: ncpg
    integer(i4),                               save :: ngo
    integer(i4),      dimension(:),   pointer, save :: ngocfg => null()
    logical,                                   save :: lalter
    logical,                                   save :: lra
    logical,                                   save :: lsym
    logical,                                   save :: lsymdok
    logical,                                   save :: lsymderv
    logical,                                   save :: lsymderv2
    logical,                                   save :: lsymoff
    logical,                                   save :: lsymopt
    logical,                                   save :: lstr
    real(dp),                                  save :: rop(3,3,48)
    real(dp),     dimension(:,:,:,:), pointer, save :: ropcfg => null()
    real(dp),                                  save :: w(7,3,3)
    real(dp),                                  save :: w1(7,3,3)
    real(dp),                                  save :: vit(3,48)
    real(dp),     dimension(:,:,:),   pointer, save :: vitcfg => null()
    integer(i4), private                            :: i
    data icentfct/1,2,2,2,4,2,3/
    data (gronam(i),i=1,59)/'P 1             ','P -1            ','P 2             ','P 21            ', &
      'C 2             ','P M             ','P C             ','C M             ','C C             ', &
      'P 2/M           ','P 21/M          ','C 2/M           ','P 2/C           ','P 21/C          ', &
      'C 2/C           ','P 2 2 2         ','P 2 2 21        ','P 21 21 2       ','P 21 21 21      ', &
      'C 2 2 21        ','C 2 2 2         ','F 2 2 2         ','I 2 2 2         ','I 21 21 21      ', &
      'P M M 2         ','P M C 21        ','P C C 2         ','P M A 2         ','P C A 21        ', &
      'P N C 2         ','P M N 21        ','P B A 2         ','P N A 21        ','P N N 2         ', &
      'C M M 2         ','C M C 21        ','C C C 2         ','A M M 2         ','A B M 2         ', &
      'A M A 2         ','A B A 2         ','F M M 2         ','F D D 2         ','I M M 2         ', &
      'I B A 2         ','I M A 2         ','P M M M         ','P N N N         ','P C C M         ', &
      'P B A N         ','P M M A         ','P N N A         ','P M N A         ','P C C A         ', &
      'P B A M         ','P C C N         ','P B C M         ','P N N M         ','P M M N         '/
    data (gronam(i),i=60,118) /'P B C N         ','P B C A         ','P N M A         ','C M C M         ', &
      'C M C A         ','C M M M         ','C C C M         ','C M M A         ','C C C A         ', &
      'F M M M         ','F D D D         ','I M M M         ','I B A M         ','I B C A         ', &
      'I M M A         ','P 4             ','P 41            ','P 42            ','P 43            ', &
      'I 4             ','I 41            ','P -4            ','I -4            ','P 4/M           ', &
      'P 42/M          ','P 4/N           ','P 42/N          ','I 4/M           ','I 41/A          ', &
      'P 4 2 2         ','P 4 21 2        ','P 41 2 2        ','P 41 21 2       ','P 42 2 2        ', &
      'P 42 21 2       ','P 43 2 2        ','P 43 21 2       ','I 4 2 2         ','I 41 2 2        ', &
      'P 4 M M         ','P 4 B M         ','P 42 C M        ','P 42 N M        ','P 4 C C         ', &
      'P 4 N C         ','P 42 M C        ','P 42 B C        ','I 4 M M         ','I 4 C M         ', &
      'I 41 M D        ','I 41 C D        ','P -4 2 M        ','P -4 2 C        ','P -4 21 M       ', &
      'P -4 21 C       ','P -4 M 2        ','P -4 C 2        ','P -4 B 2        ','P -4 N 2        '/
    data (gronam(i),i=119,177)/'I -4 M 2        ','I -4 C 2        ','I -4 2 M        ','I -4 2 D        ', &
      'P 4/M M M       ','P 4/M C C       ','P 4/N B M       ','P 4/N N C       ','P 4/M B M       ', &
      'P 4/M N C       ','P 4/N M M       ','P 4/N C C       ','P 42/M M C      ','P 42/M C M      ', &
      'P 42/N B C      ','P 42/N N M      ','P 42/M B C      ','P 42/M N M      ','P 42/N M C      ', &
      'P 42/N C M      ','I 4/M M M       ','I 4/M C M       ','I 41/A M D      ','I 41/A C D      ', &
      'P 3             ','P 31            ','P 32            ','R 3             ','P -3            ', &
      'R -3            ','P 3 1 2         ','P 3 2 1         ','P 31 1 2        ','P 31 2 1        ', &
      'P 32 1 2        ','P 32 2 1        ','R 3 2           ','P 3 M 1         ','P 3 1 M         ', &
      'P 3 C 1         ','P 3 1 C         ','R 3 M           ','R 3 C           ','P -3 1 M        ', &
      'P -3 1 C        ','P -3 M 1        ','P -3 C 1        ','R -3 M          ','R -3 C          ', &
      'P 6             ','P 61            ','P 65            ','P 62            ','P 64            ', &
      'P 63            ','P -6            ','P 6/M           ','P 63/M          ','P 6 2 2         '/
    data (gronam(i),i=178,232)/'P 61 2 2        ','P 65 2 2        ','P 62 2 2        ','P 64 2 2        ', &
      'P 63 2 2        ','P 6 M M         ','P 6 C C         ','P 63 C M        ','P 63 M C        ', &
      'P -6 M 2        ','P -6 C 2        ','P -6 2 M        ','P -6 2 C        ','P 6/M M M       ', &
      'P 6/M C C       ','P 63/M C M      ','P 63/M M C      ','P 2 3           ','F 2 3           ', &
      'I 2 3           ','P 21 3          ','I 21 3          ','P M 3           ','P N 3           ', &
      'F M 3           ','F D 3           ','I M 3           ','P A 3           ','I A 3           ', &
      'P 4 3 2         ','P 42 3 2        ','F 4 3 2         ','F 41 3 2        ','I 4 3 2         ', &
      'P 43 3 2        ','P 41 3 2        ','I 41 3 2        ','P -4 3 M        ','F -4 3 M        ', &
      'I -4 3 M        ','P -4 3 N        ','F -4 3 C        ','I -4 3 D        ','P M 3 M         ', &
      'P N 3 N         ','P M 3 N         ','P N 3 M         ','F M 3 M         ','F M 3 C         ', &
      'F D 3 M         ','F D 3 C         ','I M 3 M         ','I A 3 D         ','C 1             ', &
      'C -1            '/
    data ishorg/141*0,3*6,3*0,2*6,25*0,2*6,26*0,2*6,3*0,3*-3,42*0,-6,6,0,3*-6,4*0,-6,-3,108*0,2*-6,0, &
      3*-6,6*0,-6,6,0,-6,6,7*0,4*-6,6,-6,6*0,-6,6,2*-6,6,-6,7*0,6,-3,0,6,-3,174*0,3*-6,3*0,3*-3,54*0, &
      3*-6,3*0,3*-6,6*0,3*-3,3*-9,12*0/
!
    data  texc/'Triclinic   ','Monoclinic  ','Orthorhombic', &
               'Tetragonal  ','Hexagonal   ','Cubic       '/
    data (cl(i),i=1,45)/'Triclinic Pedial                       ', &
      'Monoclinic Domatic                      ','Monoclinic Domatic                      ', &
      'Monoclinic Domatic                      ','Monoclinic Sphenoidal                   ', &
      'Monoclinic Sphenoidal                   ','Monoclinic Sphenoidal                   ', &
      'Orthorhombic Pyramidal                  ','Orthorhombic Pyramidal                  ', &
      'Orthorhombic Pyramidal                  ','Orthorhombic Disphenoidal               ', &
      'Tetragonal Disphenoidal                 ','Tetragonal Pyramidal                    ', &
      'Ditetragonal Pyramidal                  ','Tetragonal Scalenohedral                ', &
      'Tetragonal Scalenohedral                ','Tetragonal Trapezohedral                ', &
      'Trigonal Pyramidal                      ','Ditrigonal Pyramidal                    ', &
      'Ditrigonal Pyramidal                    ','Trigonal Trapezohedral                  ', &
      'Trigonal Trapezohedral                  ','Trigonal Dipyramidal                    ', &
      'Hexagonal Pyramidal                     ','Dihexagonal Pyramidal                   ', &
      'Ditrigonal Dipyramidal                  ','Ditrigonal Dipyramidal                  ', &
      'Hexagonal Trapezohedral                 ','Cubic Tetrahedral-Pentagonaldodecahedral', &
      'Cubic Hexakistetrahedral                ','Cubic Pentagonalicositetrahedral        ', &
      'Triclinic Pinakoidal                    ','Monoclinic Prismatic                    ', &
      'Monoclinic Prismatic                    ','Monoclinic Prismatic                    ', &
      'Orthorhombic Bipyramidal                ','Trigonal Rhombohedral                   ', &
      'Tetragonal Dipyramidal                  ','Ditetragonal Dipyramidal                ', &
      'Ditrigonal Scalenohedral                ','Ditrigonal Scalenohedral                ', &
      'Dihexagonal Dipyramidal                 ','Hexagonal Dipyramidal                   ', &
      'Cubic Hexakisoctahedral                 ','Cubic Dyakisdodecahedral                '/
    data patter/'P -1     ','P 2/m    ','C 2/m    ','P m m m  ','C m m m  ','I m m m  ', &
                'F m m m  ','P 4/m    ','I 4/m    ','P 4/m m m','I 4/m m m','P -3     ', &
                'R -3     ','P -3 m 1 ','R -3 m   ','P -3 1 m ','P 6/m    ','P 6/m m m', &
                'P m -3   ','I m -3   ','F m -3   ','P m -3 m ','I m -3 m ','F m -3 m '/
    data ipatgrp/2*1,2*2,3,2*2,2*3,2*2,3,2*2,3,4*4,2*5,7,2*6,10*4,7*5,2*7,3*6,16*4,6*5,2*7, &
                 4*6,4*8,2*9,8,9,4*8,2*9,8*10,2*11,8*10,4*11,8*10,4*11,16*10,4*11,3*12,13, &
                 12,13,16,14,16,14,16,14,15,14,16,14,16,2*15,2*16,2*14,2*15,9*17,18*18,19, &
                 21,20,19,20,2*19,2*21,20,19,20,2*22,2*24,23,2*22,23,22,24,23,22,24,23,4*22, &
                 4*24,2*23,2*1/
  end module symmetry
!
!  Synchronous transit parameters
!
  module synchro
    use datatypes
    integer(i4),                                 save :: maxsynciter
    integer(i4),                                 save :: maxsyncstep
    logical,                                     save :: lfixtangent
    real(dp),                                    save :: synctol
  end module synchro
!
!  Terse output flags
!
  module terse
    use datatypes
    logical,                                     save :: ltersederivs
    logical,                                     save :: lterseincell
    logical,                                     save :: lterseincoords
    logical,                                     save :: lterseoutcell
    logical,                                     save :: lterseoutcoords
    logical,                                     save :: ltersepotentials
  end module terse
!
!  Three-body potential data
!
  module m_three
    use datatypes
    character(len=5), dimension(:,:),   pointer, save :: symbol3 => null()
    integer(i4),                                 save :: maxlist3 = 1
    integer(i4),                                 save :: maxthb = 10
    integer(i4),                                 save :: maxn3bondnono = 2
    integer(i4),      dimension(:),     pointer, save :: icell31 => null()
    integer(i4),      dimension(:),     pointer, save :: icell32 => null()
    integer(i4),      dimension(:),     pointer, save :: i3ind => null()
    integer(i4),      dimension(:),     pointer, save :: j3ind => null()
    integer(i4),      dimension(:),     pointer, save :: k3ind => null()
    integer(i4),      dimension(:),     pointer, save :: mmtexc => null()
    integer(i4),      dimension(:,:,:), pointer, save :: n3botype => null()
    integer(i4),      dimension(:,:,:), pointer, save :: n3bondno => null()
    integer(i4),      dimension(:,:),   pointer, save :: n3bondnono=> null()
    integer(i4),      dimension(:),     pointer, save :: nthbptr => null()
    integer(i4),      dimension(:),     pointer, save :: nthrty=> null()
    integer(i4),      dimension(:),     pointer, save :: ntptyp1 => null()
    integer(i4),      dimension(:),     pointer, save :: ntptyp2 => null()
    integer(i4),      dimension(:),     pointer, save :: ntptyp3 => null()
    integer(i4),      dimension(:),     pointer, save :: ntspec1 => null()
    integer(i4),      dimension(:),     pointer, save :: ntspec2 => null()
    integer(i4),      dimension(:),     pointer, save :: ntspec3 => null()
    logical,                                     save :: lPrintThree = .false.
    logical,          dimension(:),     pointer, save :: lgenerated3 => null()
    logical,          dimension(:),     pointer, save :: lthetataper => null()
    logical,          dimension(:),     pointer, save :: ltdreiding => null()
    logical,          dimension(:),     pointer, save :: ltintra => null()
    logical,          dimension(:),     pointer, save :: ltinter => null()
    real(dp),         dimension(:),     pointer, save :: thbk => null()
    real(dp),         dimension(:),     pointer, save :: theta => null()
    real(dp),         dimension(:),     pointer, save :: thetatapermax => null()
    real(dp),         dimension(:),     pointer, save :: thetatapermin => null()
    real(dp),         dimension(:),     pointer, save :: thr1min => null()
    real(dp),         dimension(:),     pointer, save :: thr2min => null()
    real(dp),         dimension(:),     pointer, save :: thr3min => null()
    real(dp),         dimension(:),     pointer, save :: thr1 => null()
    real(dp),         dimension(:),     pointer, save :: thr2 => null()
    real(dp),         dimension(:),     pointer, save :: thr3 => null()
    real(dp),         dimension(:),     pointer, save :: thrho1 => null()
    real(dp),         dimension(:),     pointer, save :: thrho2 => null()
    real(dp),         dimension(:),     pointer, save :: thrho3 => null()
    real(dp),         dimension(:,:),   pointer, save :: threepoly => null()
    integer(i4),                                 save :: nlist3md
    integer(i4),                                 save :: nthb
  end module m_three
!
!  Timing information
!
  module times
    use datatypes
    real(dp), save :: tatom = 0.0_dp
    real(dp), save :: tbondorder = 0.0_dp
    real(dp), save :: tbrenner = 0.0_dp
    real(dp), save :: tcosmo = 0.0_dp
    real(dp), save :: tcosmoderv = 0.0_dp
    real(dp), save :: tderv3 = 0.0_dp
    real(dp), save :: tdiag = 0.0_dp
    real(dp), save :: tdisk = 0.0_dp
    real(dp), save :: tedip = 0.0_dp
    real(dp), save :: teem = 0.0_dp
    real(dp), save :: tfederiv = 0.0_dp
    real(dp), save :: tfitf = 0.0_dp
    real(dp), save :: tfun = 0.0_dp
    real(dp), save :: tfour = 0.0_dp
    real(dp), save :: thes = 0.0_dp
    real(dp), save :: tion = 0.0_dp
    real(dp), save :: tmany = 0.0_dp
    real(dp), save :: tmati = 0.0_dp
    real(dp), save :: tmc   = 0.0_dp
    real(dp), save :: tmol  = 0.0_dp
    real(dp), save :: tphon = 0.0_dp
    real(dp), save :: tpolar = 0.0_dp
    real(dp), save :: tproj = 0.0_dp
    real(dp), save :: tprop = 0.0_dp
    real(dp), save :: treaxFF = 0.0_dp
    real(dp), save :: treg1 = 0.0_dp
    real(dp), save :: treg2a = 0.0_dp
    real(dp), save :: treg2b = 0.0_dp
    real(dp), save :: treg3 = 0.0_dp
    real(dp), save :: treg4 = 0.0_dp
    real(dp), save :: tregm = 0.0_dp
    real(dp), save :: tres = 0.0_dp
    real(dp), save :: trls = 0.0_dp
    real(dp), save :: tscatter = 0.0_dp
    real(dp), save :: tsearch = 0.0_dp
    real(dp), save :: tsix = 0.0_dp
    real(dp), save :: tspline = 0.0_dp
    real(dp), save :: tsum = 0.0_dp
    real(dp), save :: tsym = 0.0_dp
    real(dp), save :: tthree = 0.0_dp
    real(dp), save :: ttmat = 0.0_dp
  end module times
!
!  Optimisation transformation matrices
!
  module transform
    use datatypes
    integer(i4),                          save :: maxn3a = 1
    integer(i4),                          save :: maxn3f = 1
    real(dp),    dimension(:,:), pointer, save :: tmat => null()
    real(dp),                             save :: stmat(6,6)
  end module transform
!
!  Two-body potential data
!
  module two
    use datatypes
    character(len=5), dimension(:,:), pointer, save :: symbol2 => null()
    integer(i4),      dimension(:),   pointer, save :: mmexc => null()
    integer(i4),      dimension(:,:), pointer, save :: n2botype => null()
    integer(i4),      dimension(:),   pointer, save :: natse => null()
    integer(i4),      dimension(:),   pointer, save :: ncombipower => null()
    integer(i4),      dimension(:),   pointer, save :: ntypse => null()
    integer(i4),      dimension(:),   pointer, save :: nattab=> null()
    integer(i4),      dimension(:),   pointer, save :: ntypab=> null()
    integer(i4),      dimension(:),   pointer, save :: nptype => null()
    integer(i4),      dimension(:),   pointer, save :: nptyp1 => null()            ! Type number of species for atom 1
    integer(i4),      dimension(:),   pointer, save :: nptyp2 => null()            ! Type number of species for atom 2
    integer(i4),      dimension(:),   pointer, save :: nspec1 => null()            ! Atomic number of species for atom 1
    integer(i4),      dimension(:),   pointer, save :: nspec2 => null()            ! Atomic number of species for atom 2
    logical,          dimension(:),   pointer, save :: lcombine => null()
    logical,          dimension(:),   pointer, save :: lgenerated2 => null()       ! If true this potential was generated by rules
    logical,          dimension(:),   pointer, save :: lintra => null()            ! If true this potential acts intramolecularly
    logical,          dimension(:),   pointer, save :: linter => null()            ! If true this potential acts intermolecularly
    logical,          dimension(:),   pointer, save :: leshift => null()           ! If true this potential has an energy shift
    logical,          dimension(:),   pointer, save :: lgshift => null()           ! If true this potential has a gradient shift
    real(dp),         dimension(:),   pointer, save :: atoma => null()
    real(dp),         dimension(:),   pointer, save :: atomb => null()
    real(dp),         dimension(:,:), pointer, save :: twopot => null()            ! Parameter array for twobody potentials
    real(dp),         dimension(:),   pointer, save :: epsilon => null()
    real(dp),         dimension(:),   pointer, save :: eshift => null()            ! Energy shift value
    real(dp),         dimension(:),   pointer, save :: gshift => null()            ! Gradient shift value
    real(dp),         dimension(:),   pointer, save :: repcut => null()            ! Cut-off for repulsive part of potential
    real(dp),         dimension(:),   pointer, save :: rhopot => null()            ! For Buckingham potentials this is 1/rho
    real(dp),         dimension(:),   pointer, save :: rpot => null()              ! Maximum cut-off for potential
    real(dp),         dimension(:),   pointer, save :: rpot2 => null()             ! Minimum cut-off for potential
    real(dp),         dimension(:),   pointer, save :: scale14 => null()           ! 1-4 scale factor
    real(dp),         dimension(:),   pointer, save :: sigma => null()
    real(dp),         dimension(:,:), pointer, save :: tpot => null()
    real(dp),         dimension(:),   pointer, save :: tapergrad => null()
    real(dp),         dimension(:),   pointer, save :: taperpot => null()
    integer(i4),                               save :: maxpot = 10
    integer(i4),                               save :: natab
    integer(i4),                               save :: npote
    integer(i4),                               save :: nseps
    integer(i4),                               save :: tapertype
    logical,                                   save :: lPrintTwo = .false.
    real(dp),                                  save :: cutp
    real(dp),                                  save :: rpmax
    real(dp),                                  save :: tapermax
    real(dp),                                  save :: tapermin
    real(dp),                                  save :: taperm
    real(dp),                                  save :: pts0
    real(dp),                                  save :: pts1
    real(dp),                                  save :: pts2
    real(dp),                                  save :: pts3
    real(dp),                                  save :: pts4
    real(dp),                                  save :: pts5
    real(dp),                                  save :: pts0_7
    real(dp),                                  save :: pts1_7
    real(dp),                                  save :: pts2_7
    real(dp),                                  save :: pts3_7
    real(dp),                                  save :: pts4_7
    real(dp),                                  save :: pts5_7
    real(dp),                                  save :: pts6_7
    real(dp),                                  save :: pts7_7
  end module two
!
!  Unfreezing for defects data
!
  module freeze
    use datatypes
    integer(i4), dimension(:),   pointer, save :: iufree => null()
    logical,     dimension(:),   pointer, save :: lufree => null()
    real(dp),    dimension(:),   pointer, save :: rufree => null()
    real(dp),    dimension(:,:), pointer, save :: xufree => null()
  end module freeze
!
!  UFF species related data
!
  module uffdata
    use datatypes
    integer(i4),                               save :: maxUFFspec = 20
    character(len=5), dimension(:),   pointer, save :: symbolUFFspec => null()
    integer(i4),      dimension(:),   pointer, save :: natUFFspec => null()
    integer(i4),      dimension(:),   pointer, save :: ntypUFFspec => null()
    integer(i4),      dimension(:),   pointer, save :: nUFFtype => null()
    real(dp),         dimension(:),   pointer, save :: UFFr => null()
    real(dp),         dimension(:),   pointer, save :: UFFtheta => null()
    real(dp),         dimension(:),   pointer, save :: UFFtor => null()
    real(dp),         dimension(:),   pointer, save :: UFFx => null()
    real(dp),         dimension(:),   pointer, save :: UFFd => null()
    real(dp),         dimension(:),   pointer, save :: UFFzeta => null()
    real(dp),         dimension(:),   pointer, save :: UFFZeff => null()
    real(dp),         dimension(:),   pointer, save :: UFFKoop => null()
    real(dp),         dimension(:),   pointer, save :: UFFthetaoop => null()
    real(dp),         dimension(:),   pointer, save :: UFFchi => null()
    integer(i4),                               save :: nUFFspec
  end module uffdata
!
!  Vector module
!
  module vectors
    use datatypes

    type, public :: vector_pair
      integer(i4)                          :: maxdim_pair = 0
      real(dp),    dimension(:),   pointer :: distance_pair1 => null()
      real(dp),    dimension(:),   pointer :: xvector_pair1  => null()
      real(dp),    dimension(:),   pointer :: yvector_pair1  => null()
      real(dp),    dimension(:),   pointer :: zvector_pair1  => null()
      real(dp),    dimension(:),   pointer :: distance_pair2 => null()
      real(dp),    dimension(:),   pointer :: xvector_pair2  => null()
      real(dp),    dimension(:),   pointer :: yvector_pair2  => null()
      real(dp),    dimension(:),   pointer :: zvector_pair2  => null()
    end type vector_pair

  end module vectors
!
!  Velocities and accelerations
!
  module velocities
    use datatypes
    logical,                              save :: linputvelc
    logical,                              save :: linputvelxyz
    real(dp),    dimension(:),   pointer, save :: velx => null()
    real(dp),    dimension(:),   pointer, save :: vely => null()
    real(dp),    dimension(:),   pointer, save :: velz => null()
    real(dp),    dimension(:),   pointer, save :: x2 => null()
    real(dp),    dimension(:),   pointer, save :: y2 => null()
    real(dp),    dimension(:),   pointer, save :: z2 => null()
    real(dp),    dimension(:),   pointer, save :: x3 => null()
    real(dp),    dimension(:),   pointer, save :: y3 => null()
    real(dp),    dimension(:),   pointer, save :: z3 => null()
    real(dp),    dimension(:),   pointer, save :: x4 => null()
    real(dp),    dimension(:),   pointer, save :: y4 => null()
    real(dp),    dimension(:),   pointer, save :: z4 => null()
    real(dp),    dimension(:),   pointer, save :: x5 => null()
    real(dp),    dimension(:),   pointer, save :: y5 => null()
    real(dp),    dimension(:),   pointer, save :: z5 => null()
    real(dp),                             save :: velc(9)
    real(dp),                             save :: c2(9)
    real(dp),                             save :: c3(9)
    real(dp),                             save :: c4(9)
    real(dp),                             save :: c5(9)
  end module velocities
!
!  Coulomb matrix element data for COSMO/COSMIC
!
  module wolfcosmo
    use datatypes
    integer(i4), save :: maxloopc(3)
    logical,     save :: lPureCoulomb0D = .false.
    real(dp),    save :: cutwc = 20.0_dp
    real(dp),    save :: etawc = 0.05_dp
    real(dp),    save :: rmax2c
    real(dp),    save :: selfwolfc
    real(dp),    save :: tweatpic
  end module wolfcosmo
!
!  xc and gc module
!
  module xcgc
    use datatypes
    logical,                              save :: lnudgegc
    real(dp),    dimension(:),   pointer, save :: gc => null()
    real(dp),    dimension(:),   pointer, save :: xc => null()
    real(dp),    dimension(:),   pointer, save :: xcother => null()
  end module xcgc
!
! ChemShell module
!
  module gulpchemsh
    use datatypes
    logical,                              save :: loprt
    integer,                              save :: ichemsh_qm
    real(dp),    dimension(:,:), pointer, save :: shell_force => null()! 3,maxat
  end module gulpchemsh
