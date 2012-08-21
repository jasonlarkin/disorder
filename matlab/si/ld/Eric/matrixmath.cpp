//******************************************************************************
//** SCATMECH: Polarized Light Scattering C++ Class Library
//** 
//** File: matrixmath.cpp
//**
//** Thomas A. Germer
//** Optical Technology Division, National Institute of Standards and Technology
//** 100 Bureau Dr. Stop 8443; Gaithersburg, MD 20899-8443
//** Phone: (301) 975-2876; FAX: (301) 975-6991
//** Email: thomas.germer@nist.gov
//**
//** Version: 6.00 (February 2008)
//**
//******************************************************************************
#include <vector>
#include <cmath>
#include "scatmech.h"

#include "matrixmath.h"

using namespace std;

#define THROW(x) throw SCATMECH_exception(x);

namespace SCATMECH {

    int eigen(vector<COMPLEX>& A,
              vector<COMPLEX>& Q,
              vector<COMPLEX>& W,int nmat)
    {
        int info;
        std::vector<double> work(3*nmat);

        CMLIB::CGEEV((double*)(&A[0]),nmat,nmat,(double*)(&Q[0]),(double*)(&W[0]),nmat,(double*)(&work[0]),0,info);
        if (info!=0) THROW("Eigenvalue solver failed to converge");
        return info;
    }

    void LUdecompose(std::vector<COMPLEX>& matrix,int nmat,std::vector<int>& pivot)
    {
        int info;
        CMLIB::CGEFA(matrix,nmat,nmat,pivot,info);
        if (info!=0) THROW("Singular matrix in LUdecompose");
    }

    void LUbacksubstitute(std::vector<COMPLEX>& matrix,int nmat,std::vector<int>& pivot,std::vector<COMPLEX>& b)
    {
        CMLIB::CGESL(matrix,nmat,nmat,pivot,b,0);
    }

    void LUImprove(vector<COMPLEX>& a, vector<COMPLEX>& alud, int n, vector<int>& indx, vector<COMPLEX>& b, vector<COMPLEX>& x)
    {
        vector<COMPLEX> r(n);

        for (int i=0;i<n;++i) {
            r[i] = -b[i];
            for (int j=0;j<n;++j) r[i] += a[i+n*j]*x[j];
        }
        LUbacksubstitute(alud,n,indx,r);
        for (int ii=1;ii<n;++ii) x[ii] -= r[ii];
    }


    void Inverse(std::vector<COMPLEX>& matrix, int nmat)
    {
        using namespace CMLIB;

        CFARRAY work;
        work.allocate(nmat);
        IFARRAY pivot;
        pivot.allocate(nmat);
        CFARRAY det;
        det.allocate(2);
        int info;
        CGEFA(matrix,nmat,nmat,pivot,info);
        if (info!=0) THROW("Singular matrix in LUdecompose");
        CGEDI(matrix,nmat,nmat,pivot,det,work,1); 
    }

    namespace CMLIB {

        void SCOPY(int N,DFARRAY SX,int INCX,DFARRAY SY,int INCY);
        void CBABK2(int& NM,int& N,int& LOW,int& IGH,DFARRAY SCALE,int& M,DFARRAY ZR,DFARRAY ZI);
        void CBAL(int& NM,int& N,DFARRAY AR,DFARRAY AI,int& LOW,int& IGH,DFARRAY SCALE);
        void COMQR(int& NM,int& N,int& LOW,int& IGH,DFARRAY HR,DFARRAY HI,DFARRAY WR,DFARRAY WI,int& IERR);
        void COMQR2(int& NM,int& N,int& LOW,int& IGH,DFARRAY ORTR,DFARRAY ORTI,
                    DFARRAY HR,DFARRAY HI,DFARRAY WR,DFARRAY WI,DFARRAY ZR,DFARRAY ZI,int& IERR);
        void CORTH(int& NM,int& N,int& LOW,int& IGH,DFARRAY AR,DFARRAY AI,DFARRAY ORTR,DFARRAY ORTI);
        void CSROOT(const double& XR, const double& XI, DFARRAY YR,DFARRAY YI);
        double PYTHAG(const double& A,const double& B);
        void CDIV(const double& AR,const double& AI,const double& BR,const double& BI,DFARRAY CR,DFARRAY CI);
        void CAXPY(int N, COMPLEX& CA, FARRAY<COMPLEX> CX, int INCX, FARRAY<COMPLEX> CY, int INCY);
        void CSCAL(int N, COMPLEX CA, FARRAY<COMPLEX> CX, int INCX);
        int ICAMAX(int N, FARRAY<COMPLEX> CX, int INCX);
        COMPLEX CDOTC(int N, FARRAY<COMPLEX> CX, int INCX, FARRAY<COMPLEX> CY, int INCY);
        void CSWAP(int N,CFARRAY CX, int INCX, CFARRAY CY, int INCY);

        static double  CABS1(COMPLEX ZDUM) {     return fabs(real(ZDUM)) + fabs(imag(ZDUM)); }

        template <class T> inline T mIn(const T a,const T b) {return (a>b)? b : a;}
        template <class T> inline T mAx(const T a,const T b) {return (a<b)? b : a;}

        void CGEEV(DFARRAY A,int LDA, int N, DFARRAY E, 
                   DFARRAY V, int LDV, DFARRAY WORK, int JOB, int& INFO)
        {

            //C***BEGIN PROLOGUE  CGEEV
            //C***DATE WRITTEN   800808   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D4A4
            //C***KEYWORDS  COMPLEX,EIGENVALUE,EIGENVECTOR,GENERAL MATRIX
            //C***AUTHOR  KAHANER, D. K., (NBS)
            //C           MOLER, C. B., (U. OF NEW MEXICO)
            //C           STEWART, G. W., (U. OF MARYLAND)
            //C***PURPOSE  To compute the eigenvalues and, optionally, the eigen-
            //C            vectors of a GENERAL COMPLEX matrix.
            //C***DESCRIPTION
            //C
            //C     LICEPACK.    This version dated 08/08/80.
            //C     David Kahner, Cleve Moler, G. W. Stewart
            //C       N.B.S.         U.N.M.      N.B.S./U.MD.
            //C
            //C     Abstract
            //C      CGEEV computes the eigenvalues and, optionally,
            //C      the eigenvectors of a general complex matrix.
            //C
            //C     Call Sequence Parameters-
            //C       (The values of parameters marked with * (star) will be changed
            //C         by CGEEV.)
            //C
            //C        A*      COMPLEX(LDA,N)
            //C                complex nonsymmetric input matrix.
            //C
            //C        LDA     INTEGER
            //C                set by the user to
            //C                the leading dimension of the complex array A.
            //C
            //C        N       INTEGER
            //C                set by the user to
            //C                the order of the matrices A and V, and
            //C                the number of elements in E.
            //C
            //C        E*      COMPLEX(N)
            //C                on return from CGEEV E contains the eigenvalues of A.
            //C                See also INFO below.
            //C
            //C        V*      COMPLEX(LDV,N)
            //C                on return from CGEEV if the user has set JOB
            //C                = 0        V is not referenced.
            //C                = nonzero  the N eigenvectors of A are stored in the
            //C                first N columns of V.  See also INFO below.
            //C                (if the input matrix A is nearly degenerate, V
            //C                 will be badly conditioned, i.e. have nearly
            //C                 dependent columns.)
            //C
            //C        LDV     INTEGER
            //C                set by the user to
            //C                the leading dimension of the array V if JOB is also
            //C                set nonzero.  In that case N must be  <=  LDV.
            //C                if JOB is set to zero LDV is not referenced.
            //C
            //C        WORK*   REAL(3N)
            //C                temporary storage vector.  Contents changed by CGEEV.
            //C
            //C        JOB     INTEGER
            //C                set by the user to
            //C                = 0        eigenvalues only to be calculated by CGEEV.
            //C                           neither V nor LDV are referenced.
            //C                = nonzero  eigenvalues and vectors to be calculated.
            //C                           In this case A & V must be distinct arrays.
            //C                           Also,  if LDA > LDV,  CGEEV changes all the
            //C                           elements of A thru column N.  if LDA < LDV,
            //C                           CGEEV changes all the elements of V through
            //C                           column N.  if LDA = LDV only A(I,J) and V(I,
            //C                           J) for I,J = 1,...,N are changed by CGEEV.
            //C
            //C        INFO*   INTEGER
            //C                on return from CGEEV the value of INFO is
            //C                = 0  normal return, calculation successful.
            //C                = K  if the eigenvalue iteration fails to converge,
            //C                     eigenvalues K+1 through N are correct, but
            //C                     no eigenvectors were computed even if they were
            //C                     requested (JOB nonzero).
            //C
            //C      Error Messages
            //C           No. 1  recoverable  N is greater than LDA
            //C           No. 2  recoverable  N is less than one.
            //C           No. 3  recoverable  JOB is nonzero and N is greater than LDV
            //C           No. 4  warning      LDA > LDV,  elements of A other than the
            //C                               N by N input elements have been changed
            //C           No. 5  warning      LDA < LDV,  elements of V other than the
            //C                               N by N output elements have been changed
            //C
            //C
            //C     Subroutines Used
            //C
            //C     EISPACK-  CBABK2, CBAL, COMQR, COMQR2, CORTH
            //C     BLAS-  SCOPY
            //C     SLATEC- XERROR
            //C***REFERENCES  (NONE)
            //C***ROUTINES CALLED  CBABK2,CBAL,COMQR,COMQR2,CORTH,SCOPY,XERROR
            //C***END PROLOGUE  CGEEV

            int I,IHI,ILO,J,K,L,MDIM,M;

            //C***FIRST EXECUTABLE STATEMENT  CGEEV
            if (N > LDA) THROW("CGEEV-N  >  LDA.");
            if (N < 1 ) THROW("CGEEV-N  <  1");
            if (N == 1 && JOB == 0) goto L35;
            MDIM = 2 * LDA;

            if (JOB == 0) goto L5;
            if (N > LDV) THROW("CGEEV-JOB != 0, AND N  >  LDV.");
            if (N == 1) goto L35;
            //C
            //C       REARRANGE A if NECESSARY WHEN LDA > LDV AND JOB !=0
            //C
            MDIM = mIn(MDIM,2 * LDV);
            if (LDA < LDV) THROW("CGEEV-LDA < LDV,  ELEMENTS OF V OTHER THAN THE N BY N OUTPUT ELEMENTS HAVE BEEN CHANGED.");
            if (LDA <= LDV) goto L5;
            //SCATMECH_output << "CGEEV-LDA > LDV, ELEMENTS OF A OTHER THAN THE N BY N INPUT ELEMENTS HAVE BEEN CHANGED." << endl;
	    L = N - 1;
            for (J=1;J<=L;++J) {
                I = 2 * N;
                M = 1+J*2*LDV;
                K = 1+J*2*LDA;
                SCOPY(I,A(K),1,A(M),1);
            }
            L5:   
            //C
            //C     SEPARATE REAL AND IMAGINARY PARTS
            //C
            for (J=1;J<=N;++J) {
                      K = (J-1) * MDIM +1;
                      L = K + N;
                      SCOPY(N,A(K+1),2,WORK(1),1);
                      SCOPY(N,A(K),2,A(K),1);
                      SCOPY(N,WORK(1),1,A(L),1);
            }
            //C
            //C     SCALE AND ORTHOGONAL REDUCTION TO HESSENBERG.
            //C
             CBAL(MDIM,N,A(1),A(N+1),ILO,IHI,WORK(1));
            CORTH(MDIM,N,ILO,IHI,A(1),A(N+1),WORK(N+1),WORK(2*N+1));
            if (JOB != 0) goto L10;
            //C
            //C     EIGENVALUES ONLY
            //C
            COMQR(MDIM,N,ILO,IHI,A(1),A(N+1),E(1),E(N+1),INFO);
            goto L30;
            //C
            //C     EIGENVALUES AND EIGENVECTORS.
            //C
        L10:  COMQR2(MDIM,N,ILO,IHI,WORK(N+1),WORK(2*N+1),A(1),A(N+1),E(1),E(N+1),V(1),V(N+1),INFO);
              if (INFO != 0) goto L30;
              CBABK2(MDIM,N,ILO,IHI,WORK(1),N,V(1),V(N+1));
              //C
              //C     CONVERT EIGENVECTORS TO COMPLEX STORAGE.
              //C
              for (J=1;J<=N;++J) {
                  K = (J-1) * MDIM + 1;
                  I = (J-1) * 2 * LDV + 1;
                  L = K + N;
                  SCOPY(N,V(K),1,WORK(1),1);
                  SCOPY(N,V(L),1,V(I+1),2);
                  SCOPY(N,WORK(1),1,V(I),2);
              }
              //C
              //C     CONVERT EIGENVALUES TO COMPLEX STORAGE.
              //C
        L30:  SCOPY(N,E(1),1,WORK(1),1);
              SCOPY(N,E(N+1),1,E(2),2);
              SCOPY(N,WORK(1),1,E(1),2);
              return;
              //C
              //C     TAKE CARE OF N=1 CASE
              //C
        L35:  E(1) = A(1);
              E(2) = A(2);
              INFO = 0;
              if (JOB == 0) return;
              V(1) = A(1);
              V(2) = A(2);
              return;
        }


        void SCOPY(int N,DFARRAY SX,int INCX,DFARRAY SY,int INCY)
        {
            //C***BEGIN PROLOGUE  SCOPY
            //C***DATE WRITTEN   791001   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C
            //C***CATEGORY NO.  D1A5
            //C***KEYWORDS  BLAS,COPY,LINEAR ALGEBRA,VECTOR
            //C***AUTHOR  LAWSON, C. L., (JPL)
            //C           HANSON, R. J., (SNLA)
            //C           KINCAID, D. R., (U. OF TEXAS)
            //C           KROGH, F. T., (JPL)
            //C***PURPOSE  Copy s.p. vector y = x
            //C***DESCRIPTION
            //C
            //C                B L A S  Subprogram
            //C    Description of Parameters
            //C
            //C     --Input--
            //C        N  number of elements in input vector(s)
            //C       SX  single precision vector with N elements
            //C     INCX  storage spacing between elements of SX
            //C       SY  single precision vector with N elements
            //C     INCY  storage spacing between elements of SY
            //C
            //C     --Output--
            //C       SY  copy of vector SX (unchanged if N  <=  0)
            //C
            //C     Copy single precision SX to single precision SY.
            //C     For I = 0 to N-1, copy  SX(LX+I*INCX) to SY(LY+I*INCY),
            //C     where LX = 1 if INCX  >=  0, else LX = (-INCX)*N, and LY is
            //C     defined in a similar way using INCY.
            //C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
            //C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
            //C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
            //C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  SCOPY
            //C
            //C***FIRST EXECUTABLE STATEMENT  SCOPY
            int IX,IY,MP1,NS,I,M;

            if (N == 0) return;
            if (INCX == INCY) {
                if (INCX-1<0) goto L5;
                if (INCX-1==0) goto L20;
                else goto L60;
            }
        L5:
            //C
            //C        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
            //C
            IX = 1;
            IY = 1;
            if (INCX < 0) IX = (-N+1)*INCX + 1;
            if (INCY < 0) IY = (-N+1)*INCY + 1;
            for (I=1;I<=N;++I) {
                SY[IY-1] = SX[IX-1];
                IX = IX + INCX;
                IY = IY + INCY;
            }
            return;
            //C
            //C        CODE FOR BOTH INCREMENTS EQUAL TO 1
            //C
            //C
            //C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7.
            //C
        L20: M = N%7;
             if ( M  ==  0 ) goto L40;
             for (I=1;I<=M;++I) {
                SY[I-1] = SX[I-1];
             }
             if ( N  <  7 ) return;
        L40: MP1 = M + 1;
             for (I=MP1;I<=N;I+=7) {
                SY[I-1] = SX[I-1];
                SY[I] = SX[I];
                SY[I + 1] = SX[I + 1];
                SY[I + 2] = SX[I + 2];
                SY[I + 3] = SX[I + 3];
                SY[I + 4] = SX[I + 4];
                SY[I + 5] = SX[I + 5];
             }
             return;
            //C
            //C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
            //C
        L60:
              NS = N*INCX;
              for (I=1;I<=NS;I+=INCX) {
                  SY[I-1] = SX[I-1];
              }
              return;
        }


        void CBABK2(int& NM,int& N,int& LOW,int& IGH,DFARRAY SCALE,int& M,DFARRAY ZR,DFARRAY ZI)
        {
            //C***BEGIN PROLOGUE  CBABK2
            //C***DATE WRITTEN   760101   (YYMMDD)
            //C***REVISION DATE  830518   (YYMMDD)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D4C4
            //C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
            //C***AUTHOR  SMITH, B. T., ET AL.
            //C***PURPOSE  Forms eigenvectors of complex general matrix from
            //C            eigenvectors of matrix output from CBAL.
            //C***DESCRIPTION
            //C
            //C     This subroutine is a translation of the ALGOL procedure
            //C     CBABK2, which is a complex version of BALBAK,
            //C     NUM. MATH. 13, 293-304(1969) by Parlett and Reinsch.
            //C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
            //C
            //C     This subroutine forms the eigenvectors of a COMPLEX GENERAL
            //C     matrix by back transforming those of the corresponding
            //C     balanced matrix determined by  CBAL.
            //C
            //C     On INPUT
            //C
            //C        NM must be set to the row dimension of two-dimensional
            //C          array parameters as declared in the calling program
            //C          dimension statement.
            //C
            //C        N is the order of the matrix.
            //C
            //C        LOW and IGH are integers determined by  CBAL.
            //C
            //C        SCALE contains information determining the permutations
            //C          and scaling factors used by  CBAL.
            //C
            //C        M is the number of eigenvectors to be back transformed.
            //C
            //C        ZR and ZI contain the real and imaginary parts,
            //C          respectively, of the eigenvectors to be
            //C          back transformed in their first M columns.
            //C
            //C     On OUTPUT
            //C
            //C        ZR and ZI contain the real and imaginary parts,
            //C          respectively, of the transformed eigenvectors
            //C          in their first M columns.
            //C
            //C     Questions and comments should be directed to B. S. Garbow,
            //C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
            //C     ------------------------------------------------------------------
            //C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
            //C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
            //C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
            //C                 1976.
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CBABK2
            //C
              int I,J,K,II;
              ZR.array(NM,M);
              ZI.array(NM,M);
              double S;
            //C
            //C***FIRST EXECUTABLE STATEMENT  CBABK2
              if (M  ==  0) goto L200;
              if (IGH  ==  LOW) goto L120;

              for (I=LOW;I<=IGH;++I) {
                 S = SCALE(I);
                //C     .......... LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED
                //C                if THE FOREGOING STATEMENT IS REPLACED BY
                //C                S=1.0E0/SCALE(I). ..........
                 for (J=1;J<=M;++J) {
                    ZR(I,J) = ZR(I,J) * S;
                    ZI(I,J) = ZI(I,J) * S;
                 }
              }
            //C     .......... FOR I=LOW-1 STEP -1 UNTIL 1,
            //C                IGH+1 STEP 1 UNTIL N DO -- ..........
        L120: for (II=1;II<=N;++II) {
                 I = II;
                 if (I  >=  LOW  && I  <=  IGH) goto L140;
                 if (I  <  LOW) I = LOW - II;
                 K = (int)SCALE(I);
                 if (K  ==  I) goto L140;

                 for (J=1;J<=M;++J) {
                    S = ZR(I,J);
                    ZR(I,J) = ZR(K,J);
                    ZR(K,J) = S;
                    S = ZI(I,J);
                    ZI(I,J) = ZI(K,J);
                    ZI(K,J) = S;
                 }
        L140:     continue;
              }

        L200: return;
        }

        void CBAL(int& NM,int& N,DFARRAY AR,DFARRAY AI,int& LOW,int& IGH,DFARRAY SCALE)
        {
            //C***BEGIN PROLOGUE  CBAL
            //C***DATE WRITTEN   760101   (YYMMDD)
            //C***REVISION DATE  830518   (YYMMDD)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D4C1A
            //C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
            //C***AUTHOR  SMITH, B. T., ET AL.
            //C***PURPOSE  Balances a complex general matrix and isolates eigenvalues
            //C            whenever possible.
            //C***DESCRIPTION
            //C
            //C     This subroutine is a translation of the ALGOL procedure
            //C     CBALANCE, which is a complex version of BALANCE,
            //C     NUM. MATH. 13, 293-304(1969) by Parlett and Reinsch.
            //C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
            //C
            //C     This subroutine balances a COMPLEX matrix and isolates
            //C     eigenvalues whenever possible.
            //C
            //C     On INPUT
            //C
            //C        NM must be set to the row dimension of two-dimensional
            //C          array parameters as declared in the calling program
            //C          dimension statement.
            //C
            //C        N is the order of the matrix.
            //C
            //C        AR and AI contain the real and imaginary parts,
            //C          respectively, of the complex matrix to be balanced.
            //C
            //C     On OUTPUT
            //C
            //C        AR and AI contain the real and imaginary parts,
            //C          respectively, of the balanced matrix.
            //C
            //C        LOW and IGH are two integers such that AR(I,J) and AI(I,J)
            //C          are equal to zero if
            //C           (1) I is greater than J and
            //C           (2) J=1,...,LOW-1 or I=IGH+1,...,N.
            //C
            //C        SCALE contains information determining the
            //C           permutations and scaling factors used.
            //C
            //C     Suppose that the principal submatrix in rows LOW through IGH
            //C     has been balanced, that P(J) denotes the index interchanged
            //C     with J during the permutation step, and that the elements
            //C     of the diagonal matrix used are denoted by D(I,J).  Then
            //C        SCALE(J) = P(J),    for J = 1,...,LOW-1
            //C                 = D(J,J)       J = LOW,...,IGH
            //C                 = P(J)         J = IGH+1,...,N.
            //C     The order in which the interchanges are made is N to IGH+1,
            //C     then 1 to LOW-1.
            //C
            //C     Note that 1 is returned for IGH if IGH is zero formally.
            //C
            //C     The ALGOL procedure EXC contained in CBALANCE appears in
            //C     CBAL  in line.  (Note that the ALGOL roles of identifiers
            //C     K,L have been reversed.)
            //C
            //C     Questions and comments should be directed to B. S. Garbow,
            //C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
            //C     ------------------------------------------------------------------
            //C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
            //C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
            //C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
            //C                 1976.
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CBAL
            //C
              int I,J,K,L,M,JJ,IEXC;
              AR.array(NM,N);
              AI.array(NM,N);
              double C,F,G,R,S,B2,RADIX;
              bool NOCONV;
            //C
            //C     THE FOLLOWING PORTABLE VALUE OF RADIX WORKS WELL ENOUGH
            //C     FOR ALL MACHINES WHOSE BASE IS A POWER OF TWO.
            //C
            //C***FIRST EXECUTABLE STATEMENT  CBAL
              RADIX = 16;

              B2 = RADIX * RADIX;
              K = 1;
              L = N;
              goto L100;
            //C     .......... IN-LINE PROCEDURE FOR ROW AND
            //C                COLUMN EXCHANGE ..........
        L20: SCALE(M) = J;
             if (J  ==  M) goto L50;
         
             for (I=1;I<=L;++I) {
                 F = AR(I,J);
                 AR(I,J) = AR(I,M);
                 AR(I,M) = F;
                 F = AI(I,J);
                 AI(I,J) = AI(I,M);
                 AI(I,M) = F;
             }
         
             for (I=K;I<=N;++I) {
                 F = AR(J,I);
                 AR(J,I) = AR(M,I);
                 AR(M,I) = F;
                 F = AI(J,I);
                 AI(J,I) = AI(M,I);
                 AI(M,I) = F;
             }
         
        L50: if (IEXC==1) goto L80;
             if (IEXC==2) goto L130;
         
             //C     .......... SEARCH FOR ROWS ISOLATING AN EIGENVALUE
             //C                AND PUSH THEM DOWN ..........
        L80: if (L  ==  1) goto L280;
             L = L - 1;
             //C     .......... FOR J=L STEP -1 UNTIL 1 DO -- ..........
        L100: for (JJ=1;JJ<=L;++JJ) {
                  J = L + 1 - JJ;
              
                  for (I=1;I<=L;++I) {
                      if (I  ==  J) goto L110;
                      if (AR(J,I) != 0.0E0  ||  AI(J,I) != 0.0E0) goto L120;
        L110:          continue;
                  }
              
                  M = L;
                  IEXC = 1;
                  goto L20;
        L120:  continue;
              }
          
              goto L140;
              //C     .......... SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE
              //C                AND PUSH THEM LEFT ..........
        L130: K = K + 1;
          
        L140: for (J=K;J<=L;++J) {
              
                  for (I=K;I<=L;++I) {
                      if (I  ==  J) goto L150;
                      if (AR(I,J) != 0.0E0  ||  AI(I,J) != 0.0E0) goto L170;
        L150:        continue;
                  }
              
                  M = K;
                  IEXC = 2;
                  goto L20;
        L170:    continue;
              }
              //C     .......... NOW BALANCE THE SUBMATRIX IN ROWS K TO L ..........
              for (I=K;I<=L;++I) {
                  SCALE(I) = 1.0E0;
              }
              //C     .......... ITERATIVE LOOP FOR NORM REDUCTION ..........
        L190: NOCONV = false;
          
              for (I=K;I<=L;++I) {
                  C = 0.0E0;
                  R = 0.0E0;
              
                  for (J=K;J<=L;++J) {
                      if (J  ==  I) goto L200;
                      C = C + fabs(AR(J,I)) + fabs(AI(J,I));
                      R = R + fabs(AR(I,J)) + fabs(AI(I,J));
        L200:         continue;
                  }
                  //C     .......... GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW ..........
                  if (C  ==  0.0E0  ||  R  ==  0.0E0) goto L270;
                  G = R / RADIX;
                  F = 1.0E0;
                  S = C + R;
        L210:    if (C  >=  G) goto L220;
                 F = F * RADIX;
                 C = C * B2;
                 goto L210;
        L220:    G = R * RADIX;
        L230:    if (C  <  G) goto L240;
                 F = F / RADIX;
                 C = C / B2;
                 goto L230;
                 //C     .......... NOW BALANCE ..........
        L240:    if ((C + R) / F  >=  0.95E0 * S) goto L270;
                 G = 1.0E0 / F;
                 SCALE(I) = SCALE(I) * F;
                 NOCONV = true;
             
                 for (J=K;J<=N;++J) {
                     AR(I,J) = AR(I,J) * G;
                     AI(I,J) = AI(I,J) * G;
                 }
             
                 for (J=1;J<=L;++J) {
                     AR(J,I) = AR(J,I) * F;
                     AI(J,I) = AI(J,I) * F;
                     continue;
                 }
             
        L270:    continue;
              }
          
              if (NOCONV) goto L190;
          
        L280: LOW = K;
              IGH = L;
              return;
        }

        void COMQR(int& NM,int& N,int& LOW,int& IGH,DFARRAY HR,DFARRAY HI,DFARRAY WR,DFARRAY WI,int& IERR)
        {
            //C***BEGIN PROLOGUE  COMQR
            //C***DATE WRITTEN   760101   (YYMMDD)
            //C***REVISION DATE  830518   (YYMMDD)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D4C2B
            //C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
            //C***AUTHOR  SMITH, B. T., ET AL.
            //C***PURPOSE  Computes eigenvalues of complex upper Hessenberg matrix
            //C            using the QR method.
            //C***DESCRIPTION
            //C
            //C     This subroutine is a translation of a unitary analogue of the
            //C     ALGOL procedure  COMLR, NUM. MATH. 12, 369-376(1968) by Martin
            //C     and Wilkinson.
            //C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).
            //C     The unitary analogue substitutes the QR algorithm of Francis
            //C     (COMP. JOUR. 4, 332-345(1962)) for the LR algorithm.
            //C
            //C     This subroutine finds the eigenvalues of a COMPLEX
            //C     upper Hessenberg matrix by the QR method.
            //C
            //C     On INPUT
            //C
            //C        NM must be set to the row dimension of two-dimensional
            //C          array parameters as declared in the calling program
            //C          dimension statement.
            //C
            //C        N is the order of the matrix.
            //C
            //C        LOW and IGH are integers determined by the balancing
            //C          subroutine  CBAL.  if  CBAL  has not been used,
            //C          set LOW=1, IGH=N.
            //C
            //C        HR and HI contain the real and imaginary parts,
            //C          respectively, of the complex upper Hessenberg matrix.
            //C          Their lower triangles below the subdiagonal contain
            //C          information about the unitary transformations used in
            //C          the reduction by  CORTH, if performed.
            //C
            //C     On OUTPUT
            //C
            //C        The upper Hessenberg portions of HR and HI have been
            //C          destroyed.  Therefore, they must be saved before
            //C          calling  COMQR  if subsequent calculation of
            //C          eigenvectors is to be performed.
            //C
            //C        WR and WI contain the real and imaginary parts,
            //C          respectively, of the eigenvalues.  if an error
            //C          exit is made, the eigenvalues should be correct
            //C          for indices IERR+1,...,N.
            //C
            //C        IERR is set to
            //C          ZERO       for normal return,
            //C          J          if the J-th eigenvalue has not been
            //C                     determined after a total of 30*N iterations.
            //C
            //C     Calls CSROOT for complex square root.
            //C     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
            //C     Calls CDIV for complex division.
            //C
            //C     Questions and comments should be directed to B. S. Garbow,
            //C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
            //C     ------------------------------------------------------------------
            //C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
            //C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
            //C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
            //C                 1976.
            //C***ROUTINES CALLED  CDIV,CSROOT,PYTHAG
            //C***END PROLOGUE  COMQR
            //C
          
            int I,J,L,EN,LL,ITN,ITS,LP1,ENM1;
            HR.array(NM,N);
            HI.array(NM,N);
            WR.array(N);
             WI.array(N);
              double SI,SR,TI,TR,XI,XR,YI,YR,ZZI,ZZR,NORM,S1,S2;

              IERR = 0;
              if (LOW  ==  IGH) goto L180;
            //C     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
              L = LOW + 1;

              for (I=L;I<=IGH;++I) {
                 LL = mIn(I+1,IGH);
                 if (HI(I,I-1)  ==  0.0E0) goto L170;
                 NORM = PYTHAG(HR(I,I-1),HI(I,I-1));
                 YR = HR(I,I-1) / NORM;
                 YI = HI(I,I-1) / NORM;
                 HR(I,I-1) = NORM;
                 HI(I,I-1) = 0.0E0;

                 for (J=I;J<=IGH;++J) {
                    SI = YR * HI(I,J) - YI * HR(I,J);
                    HR(I,J) = YR * HR(I,J) + YI * HI(I,J);
                    HI(I,J) = SI;
                    continue;
                 }

                 for (J=LOW;J<=LL;++J) {
                    SI = YR * HI(J,I) + YI * HR(J,I);
                    HR(J,I) = YR * HR(J,I) - YI * HI(J,I);
                    HI(J,I) = SI;
                    continue;
                 }
        L170:    continue;
            }
            //C     .......... STORE ROOTS ISOLATED BY CBAL ..........
        L180: for (I=1;I<=N;++I) {
                 if (I  >=  LOW  && I  <=  IGH) goto L200;
                 WR(I) = HR(I,I);
                 WI(I) = HI(I,I);
        L200:    continue;
              }

              EN = IGH;
              TR = 0.0E0;
              TI = 0.0E0;
              ITN = 30*N;
            //C     .......... SEARCH FOR NEXT EIGENVALUE ..........
        L220: if (EN  <  LOW) goto L1001;
              ITS = 0;
              ENM1 = EN - 1;
            //C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
            //C                FOR L=EN STEP -1 UNTIL LOW E0 -- ..........
        L240: for (LL = LOW;LL<=EN;++LL) {
                 L = EN + LOW - LL;
                 if (L  ==  LOW) goto L300;
                 S1 = fabs(HR(L-1,L-1)) + fabs(HI(L-1,L-1))+ fabs(HR(L,L)) +fabs(HI(L,L));
                 S2 = S1 + fabs(HR(L,L-1));
                 if (S2  ==  S1) goto L300;
                 continue;
              }
            //C     .......... FORM SHifT ..........
        L300: if (L  ==  EN) goto L660;
              if (ITN  ==  0) goto L1000;
              if (ITS  ==  10  ||  ITS  ==  20) goto L320;
              SR = HR(EN,EN);
              SI = HI(EN,EN);
              XR = HR(ENM1,EN) * HR(EN,ENM1);
              XI = HI(ENM1,EN) * HR(EN,ENM1);
              if (XR  ==  0.0E0  && XI  ==  0.0E0) goto L340;
              YR = (HR(ENM1,ENM1) - SR) / 2.0E0;
              YI = (HI(ENM1,ENM1) - SI) / 2.0E0;
              CSROOT(sqr(YR)-sqr(YI)+XR,2.0E0*YR*YI+XI,ZZR,ZZI);
              if (YR * ZZR + YI * ZZI  >=  0.0E0) goto L310;
              ZZR = -ZZR;
              ZZI = -ZZI;
        L310: CDIV(XR,XI,YR+ZZR,YI+ZZI,XR,XI);
              SR = SR - XR;
              SI = SI - XI;
              goto L340;
            //C     .......... FORM EXCEPTIONAL SHifT ..........
        L320: SR = fabs(HR(EN,ENM1)) + fabs(HR(ENM1,EN-2));
              SI = 0.0E0;

        L340: for (I=LOW;I<=EN;++I) {
                 HR(I,I) = HR(I,I) - SR;
                 HI(I,I) = HI(I,I) - SI;
                 continue;
              }

              TR = TR + SR;
              TI = TI + SI;
              ITS = ITS + 1;
              ITN = ITN - 1;
            //C     .......... REDUCE TO TRIANGLE (ROWS) ..........
              LP1 = L + 1;

              for (I=LP1;I<=EN;++I) {
                 SR = HR(I,I-1);
                 HR(I,I-1) = 0.0E0;
                 NORM = PYTHAG(PYTHAG(HR(I-1,I-1),HI(I-1,I-1)),SR);
                 XR = HR(I-1,I-1) / NORM;
                 WR(I-1) = XR;
                 XI = HI(I-1,I-1) / NORM;
                 WI(I-1) = XI;
                 HR(I-1,I-1) = NORM;
                 HI(I-1,I-1) = 0.0E0;
                 HI(I,I-1) = SR / NORM;

                 for (J=I;J<=EN;++J) {
                    YR = HR(I-1,J);
                    YI = HI(I-1,J);
                    ZZR = HR(I,J);
                    ZZI = HI(I,J);
                    HR(I-1,J) = XR * YR + XI * YI + HI(I,I-1) * ZZR;
                    HI(I-1,J) = XR * YI - XI * YR + HI(I,I-1) * ZZI;
                    HR(I,J) = XR * ZZR - XI * ZZI - HI(I,I-1) * YR;
                    HI(I,J) = XR * ZZI + XI * ZZR - HI(I,I-1) * YI;
                    continue;
                 }
                 continue;
              }

              SI = HI(EN,EN);
              if (SI  ==  0.0E0) goto L540;
              NORM = PYTHAG(HR(EN,EN),SI);
              SR = HR(EN,EN) / NORM;
              SI = SI / NORM;
              HR(EN,EN) = NORM;
              HI(EN,EN) = 0.0E0;
            //C     .......... INVERSE OPERATION (COLUMNS) ..........
        L540:  for (J=LP1;J<=EN;++J) { //L600
                 XR = WR(J-1);
                 XI = WI(J-1);

                 for (I=L;I<=J;++I) { //L580
                    YR = HR(I,J-1);
                    YI = 0.0E0;
                    ZZR = HR(I,J);
                    ZZI = HI(I,J);
                    if (I  ==  J) goto L560;
                    YI = HI(I,J-1);
                    HI(I,J-1) = XR * YI + XI * YR + HI(J,J-1) * ZZI;
        L560:       HR(I,J-1) = XR * YR - XI * YI + HI(J,J-1) * ZZR;
                    HR(I,J) = XR * ZZR + XI * ZZI - HI(J,J-1) * YR;
                    HI(I,J) = XR * ZZI - XI * ZZR - HI(J,J-1) * YI;
                    continue;
                 }

            continue;
               }

              if (SI  ==  0.0E0) goto L240;

              for (I=L;I<=EN;++I) { // L630
                 YR = HR(I,EN);
                 YI = HI(I,EN);
                 HR(I,EN) = SR * YR - SI * YI;
                 HI(I,EN) = SR * YI + SI * YR;
                 continue;
              }

              goto L240;
            //C     .......... A ROOT FOUND ..........
        L660: WR(EN) = HR(EN,EN) + TR;
              WI(EN) = HI(EN,EN) + TI;
              EN = ENM1;
              goto L220;
            //C     .......... SET ERROR -- NO CONVERGENCE TO AN
            //C                EIGENVALUE AFTER 30*N ITERATIONS ..........
        L1000: IERR = EN;
        L1001: return;
        }


        void COMQR2(int& NM,int& N,int& LOW,int& IGH,DFARRAY ORTR,DFARRAY ORTI,
                    DFARRAY HR,DFARRAY HI,DFARRAY WR,DFARRAY WI,DFARRAY ZR,DFARRAY ZI,int& IERR)
        {
            //C***BEGIN PROLOGUE  COMQR2
            //C***DATE WRITTEN   760101   (YYMMDD)
            //C***REVISION DATE  830518   (YYMMDD)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D4C2B
            //C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
            //C***AUTHOR  SMITH, B. T., ET AL.
            //C***PURPOSE  Computes eigenvalues and eigenvectors of complex upper
            //C            Hessenberg matrix.
            //C***DESCRIPTION
            //C
            //C     This subroutine is a translation of a unitary analogue of the
            //C     ALGOL procedure  COMLR2, NUM. MATH. 16, 181-204(1970) by Peters
            //C     and Wilkinson.
            //C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
            //C     The unitary analogue substitutes the QR algorithm of Francis
            //C     (COMP. JOUR. 4, 332-345(1962)) for the LR algorithm.
            //C
            //C     This subroutine finds the eigenvalues and eigenvectors
            //C     of a COMPLEX UPPER Hessenberg matrix by the QR
            //C     method.  The eigenvectors of a COMPLEX GENERAL matrix
            //C     can also be found if  CORTH  has been used to reduce
            //C     this general matrix to Hessenberg form.
            //C
            //C     On INPUT
            //C
            //C        NM must be set to the row dimension of two-dimensional
            //C          array parameters as declared in the calling program
            //C          dimension statement.
            //C
            //C        N is the order of the matrix.
            //C
            //C        LOW and IGH are integers determined by the balancing
            //C          subroutine  CBAL.  if  CBAL  has not been used,
            //C          set LOW=1, IGH=N.
            //C
            //C        ORTR and ORTI contain information about the unitary trans-
            //C          formations used in the reduction by  CORTH, if performed.
            //C          Only elements LOW through IGH are used.  if the eigenvectors
            //C          of the Hessenberg matrix are desired, set ORTR(J) and
            //C          ORTI(J) to 0.0E0 for these elements.
            //C
            //C        HR and HI contain the real and imaginary parts,
            //C          respectively, of the complex upper Hessenberg matrix.
            //C          Their lower triangles below the subdiagonal contain further
            //C          information about the transformations which were used in the
            //C          reduction by  CORTH, if performed.  if the eigenvectors of
            //C          the Hessenberg matrix are desired, these elements may be
            //C          arbitrary.
            //C
            //C     On OUTPUT
            //C
            //C        ORTR, ORTI, and the upper Hessenberg portions of HR and HI
            //C          have been destroyed.
            //C
            //C        WR and WI contain the real and imaginary parts,
            //C          respectively, of the eigenvalues.  if an error
            //C          exit is made, the eigenvalues should be correct
            //C          for indices IERR+1,...,N.
            //C
            //C        ZR and ZI contain the real and imaginary parts,
            //C          respectively, of the eigenvectors.  The eigenvectors
            //C          are unnormalized.  if an error exit is made, none of
            //C          the eigenvectors has been found.
            //C
            //C        IERR is set to
            //C          Zero       for normal return,
            //C          J          if the J-th eigenvalue has not been
            //C                     determined after a total of 30*N iterations.
            //C
            //C     Calls CSROOT for complex square root.
            //C     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
            //C     Calls CDIV for complex division.
            //C
            //C     Questions and comments should be directed to B. S. Garbow,
            //C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
            //C     ------------------------------------------------------------------
            //C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
            //C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
            //C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
            //C                 1976.
            //C***ROUTINES CALLED  CDIV,CSROOT,PYTHAG
            //C***END PROLOGUE  COMQR2
            //C
              int I,J,K,L,M,EN,II,JJ,LL,NN,IP1;
              int ITN,ITS,LP1,ENM1,IEND;
              HR.array(NM,N);
              HI.array(NM,N);
              WR.array(N);
              WI.array(N);
              ZR.array(NM,N);
              ZI.array(NM,N);
              ORTR.array(IGH);
              ORTI.array(IGH);
              double SI,SR,TI,TR,XI,XR,YI,YR,ZZI,ZZR,NORM,S1,S2;

              IERR = 0;
            //C     .......... INITIALIZE EIGENVECTOR MATRIX ..........
              for (I=1;I<=N;++I) { // L100
                  for (J=1;J<=N;++J) { // L100
                    ZR(I,J) = 0.0E0;
                    ZI(I,J) = 0.0E0;
                    if (I  ==  J) ZR(I,J) = 1.0E0;
                    continue;
                  }
              }
            //C     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS
            //C                FROM THE INFORMATION LEFT BY CORTH ..........
              IEND = IGH - LOW - 1;
              if (IEND<0) goto L180;
              if (IEND==0) goto L150;
              if (IEND>0) goto L105;
            //C     .......... FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........
        L105: for (II=1;II<=IEND;++II) { // L140
                 I = IGH - II;
                 if (ORTR(I)  ==  0.0E0  && ORTI(I)  ==  0.0E0) goto L140;
                 if (HR(I,I-1)  ==  0.0E0  && HI(I,I-1)  ==  0.0E0) goto L140;
            //C     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
                 NORM = HR(I,I-1) * ORTR(I) + HI(I,I-1) * ORTI(I);
                 IP1 = I + 1;

                 for (K=IP1;K<=IGH;++K) { // L110
                    ORTR(K) = HR(K,I-1);
                    ORTI(K) = HI(K,I-1);
                    continue;
                 }

                 for (J=I;J<=IGH;++J) { // L130 
                    SR = 0.0E0;
                    SI = 0.0E0;

                    for (K=I;K<=IGH;++K) { // L115
                       SR = SR + ORTR(K) * ZR(K,J) + ORTI(K) * ZI(K,J);
                       SI = SI + ORTR(K) * ZI(K,J) - ORTI(K) * ZR(K,J);
                       continue;
                    }

                    SR = SR / NORM;
                    SI = SI / NORM;

                    for (K=I;K<=IGH;++K) { // L120
                       ZR(K,J) = ZR(K,J) + SR * ORTR(K) - SI * ORTI(K);
                       ZI(K,J) = ZI(K,J) + SR * ORTI(K) + SI * ORTR(K);
                       continue;
                    }

                    continue;
                 }

        L140:    continue;
              }
            //C     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
        L150: L = LOW + 1;

              for (I=L;I<=IGH;++I) { // L170
                 LL = mIn(I+1,IGH);
                 if (HI(I,I-1)  ==  0.0E0) goto L170;
                 NORM = PYTHAG(HR(I,I-1),HI(I,I-1));
                 YR = HR(I,I-1) / NORM;
                 YI = HI(I,I-1) / NORM;
                 HR(I,I-1) = NORM;
                 HI(I,I-1) = 0.0E0;

                 for (J=I;J<=N;++J) { // L155
                    SI = YR * HI(I,J) - YI * HR(I,J);
                    HR(I,J) = YR * HR(I,J) + YI * HI(I,J);
                    HI(I,J) = SI;
                    continue;
                 }

                 for (J=1;J<=LL;++J) { // L160
                    SI = YR * HI(J,I) + YI * HR(J,I);
                    HR(J,I) = YR * HR(J,I) - YI * HI(J,I);
                    HI(J,I) = SI;
                    continue;
                 }

                 for (J=LOW;J<=IGH;++J) { // L165
                    SI = YR * ZI(J,I) + YI * ZR(J,I);
                    ZR(J,I) = YR * ZR(J,I) - YI * ZI(J,I);
                    ZI(J,I) = SI;
                    continue;
                 }

        L170:     continue;
              }
            //C     .......... STORE ROOTS ISOLATED BY CBAL ..........
        L180: for (I=1;I<=N;++I) { // L200
                 if (I  >=  LOW  && I  <=  IGH) goto L200;
                 WR(I) = HR(I,I);
                 WI(I) = HI(I,I);
        L200:    continue;
              }

              EN = IGH;
              TR = 0.0E0;
              TI = 0.0E0;
              ITN = 30*N;
              //C     .......... SEARCH FOR NEXT EIGENVALUE ..........
        L220: if (EN  <  LOW) goto L680;
              ITS = 0;
              ENM1 = EN - 1;
              //C     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
              //C                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
        L240: for (LL=LOW;LL<=EN;++LL) { // L260
                 L = EN + LOW - LL;
                 if (L  ==  LOW) goto L300;
                 S1 = fabs(HR(L-1,L-1)) + fabs(HI(L-1,L-1)) + fabs(HR(L,L)) +fabs(HI(L,L));
                 S2 = S1 + fabs(HR(L,L-1));
                 if (S2  ==  S1) goto L300;
                 continue;
              }
            //C     .......... FORM SHifT ..........
        L300: if (L  ==  EN) goto L660;
              if (ITN  ==  0) goto L1000;
              if (ITS  ==  10  ||  ITS  ==  20) goto L320;
              SR = HR(EN,EN);
              SI = HI(EN,EN);
              XR = HR(ENM1,EN) * HR(EN,ENM1);
              XI = HI(ENM1,EN) * HR(EN,ENM1);
              if (XR  ==  0.0E0  && XI  ==  0.0E0) goto L340;
              YR = (HR(ENM1,ENM1) - SR) / 2.0E0;
              YI = (HI(ENM1,ENM1) - SI) / 2.0E0;
              CSROOT(sqr(YR)-sqr(YI)+XR,2.0E0*YR*YI+XI,ZZR,ZZI);
              if (YR * ZZR + YI * ZZI  >=  0.0E0) goto L310;
              ZZR = -ZZR;
              ZZI = -ZZI;
        L310: CDIV(XR,XI,YR+ZZR,YI+ZZI,XR,XI);
              SR = SR - XR;
              SI = SI - XI;
              goto L340;
            //C     .......... FORM EXCEPTIONAL SHifT ..........
        L320: SR = fabs(HR(EN,ENM1)) + fabs(HR(ENM1,EN-2));
              SI = 0.0E0;

        L340: for (I=LOW;I<=EN;++I) { // L360
                 HR(I,I) = HR(I,I) - SR;
                 HI(I,I) = HI(I,I) - SI;
                 continue;
              }

              TR = TR + SR;
              TI = TI + SI;
              ITS = ITS + 1;
              ITN = ITN - 1;
            //C     .......... REDUCE TO TRIANGLE (ROWS) ..........
              LP1 = L + 1;

              for (I=LP1;I<=EN;++I) { // L500
                 SR = HR(I,I-1);
                 HR(I,I-1) = 0.0E0;
                 NORM = PYTHAG(PYTHAG(HR(I-1,I-1),HI(I-1,I-1)),SR);
                 XR = HR(I-1,I-1) / NORM;
                 WR(I-1) = XR;
                 XI = HI(I-1,I-1) / NORM;
                 WI(I-1) = XI;
                 HR(I-1,I-1) = NORM;
                 HI(I-1,I-1) = 0.0E0;
                 HI(I,I-1) = SR / NORM;

                 for (J=I;J<=N;++J) { // L490
                    YR = HR(I-1,J);
                    YI = HI(I-1,J);
                    ZZR = HR(I,J);
                    ZZI = HI(I,J);
                    HR(I-1,J) = XR * YR + XI * YI + HI(I,I-1) * ZZR;
                    HI(I-1,J) = XR * YI - XI * YR + HI(I,I-1) * ZZI;
                    HR(I,J) = XR * ZZR - XI * ZZI - HI(I,I-1) * YR;
                    HI(I,J) = XR * ZZI + XI * ZZR - HI(I,I-1) * YI;
                    continue;
                 }
                 continue;
              }

              SI = HI(EN,EN);
              if (SI  ==  0.0E0) goto L540;
              NORM = PYTHAG(HR(EN,EN),SI);
              SR = HR(EN,EN) / NORM;
              SI = SI / NORM;
              HR(EN,EN) = NORM;
              HI(EN,EN) = 0.0E0;
              if (EN  ==  N) goto L540;
              IP1 = EN + 1;

              for (J=IP1;J<=N;++J) { // L520
                 YR = HR(EN,J);
                 YI = HI(EN,J);
                 HR(EN,J) = SR * YR + SI * YI;
                 HI(EN,J) = SR * YI - SI * YR;
                 continue;
              }
            //C     .......... INVERSE OPERATION (COLUMNS) ..........

        L540: for (J=LP1;J<=EN;++J) { // L600
                 XR = WR(J-1);
                 XI = WI(J-1);

                 for (I=1;I<=J;++I) { // L580
                    YR = HR(I,J-1);
                    YI = 0.0E0;
                    ZZR = HR(I,J);
                    ZZI = HI(I,J);
                    if (I  ==  J) goto L560;
                    YI = HI(I,J-1);
                    HI(I,J-1) = XR * YI + XI * YR + HI(J,J-1) * ZZI;
        L560:       HR(I,J-1) = XR * YR - XI * YI + HI(J,J-1) * ZZR;
                    HR(I,J) = XR * ZZR + XI * ZZI - HI(J,J-1) * YR;
                    HI(I,J) = XR * ZZI - XI * ZZR - HI(J,J-1) * YI;
                    continue;
                 }

                 for (I=LOW;I<=IGH;++I) { // L590
                    YR = ZR(I,J-1);
                    YI = ZI(I,J-1);
                    ZZR = ZR(I,J);
                    ZZI = ZI(I,J);
                    ZR(I,J-1) = XR * YR - XI * YI + HI(J,J-1) * ZZR;
                    ZI(I,J-1) = XR * YI + XI * YR + HI(J,J-1) * ZZI;
                    ZR(I,J) = XR * ZZR + XI * ZZI - HI(J,J-1) * YR;
                    ZI(I,J) = XR * ZZI - XI * ZZR - HI(J,J-1) * YI;
                    continue;
                 }

                 continue;
              }

              if (SI  ==  0.0E0) goto L240;

              for (I=1;I<=EN;++I) { // L630
                 YR = HR(I,EN);
                 YI = HI(I,EN);
                 HR(I,EN) = SR * YR - SI * YI;
                 HI(I,EN) = SR * YI + SI * YR;
                 continue;
              }

              for (I=LOW;I<=IGH;++I) { // L640 
                 YR = ZR(I,EN);
                 YI = ZI(I,EN);
                 ZR(I,EN) = SR * YR - SI * YI;
                 ZI(I,EN) = SR * YI + SI * YR;
                 continue;
              }

              goto L240;
              //C.......... A ROOT FOUND ..........
        L660: HR(EN,EN) = HR(EN,EN) + TR;
              WR(EN) = HR(EN,EN);
              HI(EN,EN) = HI(EN,EN) + TI;
              WI(EN) = HI(EN,EN);
              EN = ENM1;
              goto L220;
            //C     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
            //C                VECTORS OF UPPER TRIANGULAR FORM ..........
        L680: NORM = 0.0E0;

              for (I=1;I<=N;++I) { // L720
                  for (J=I;J<=N;++J) { // L720
                    NORM = NORM + fabs(HR(I,J)) + fabs(HI(I,J));
                    continue;
                  }
              }

              if (N  ==  1  ||  NORM  ==  0.0E0) goto L1001;
            //C     .......... FOR EN=N STEP -1 UNTIL 2 DO -- ..........
              for (NN=2;NN<=N;++NN) { // L800
                 EN = N + 2 - NN;
                 XR = WR(EN);
                 XI = WI(EN);
                 ENM1 = EN - 1;
                  //C     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
                 for (II=1;II<=ENM1;++II) { // L780
                    I = EN - II;
                    ZZR = HR(I,EN);
                    ZZI = HI(I,EN);
                    if (I  ==  ENM1) goto L760;
                    IP1 = I + 1;

                    for (J=IP1;J<=ENM1;++J) { // L740
                       ZZR = ZZR + HR(I,J) * HR(J,EN) - HI(I,J) * HI(J,EN);
                       ZZI = ZZI + HR(I,J) * HI(J,EN) + HI(I,J) * HR(J,EN);
                       continue;
                    }

        L760:       YR = XR - WR(I);
                    YI = XI - WI(I);
                    if (YR != 0.0E0  ||  YI != 0.0E0) goto L775;
                    YR = NORM;
        L770:       YR = 0.5E0*YR;
                    if (NORM + YR  >  NORM) goto L770;
                    YR = 2.0E0*YR;
        L775:       CDIV(ZZR,ZZI,YR,YI,HR(I,EN),HI(I,EN));
                    continue;
                 }
                 continue;
              }
               //C     .......... END BACKSUBSTITUTION ..........
              ENM1 = N - 1;
              //C     .......... VECTORS OF ISOLATED ROOTS ..........
              for (I=1;I<=ENM1;++I) { // L840
                 if (I  >=  LOW  && I  <=  IGH) goto L840;
                 IP1 = I + 1;

                 for (J=IP1;J<=N;++J) { // L820
                    ZR(I,J) = HR(I,J);
                    ZI(I,J) = HI(I,J);
                    continue;
                 }
        L840:    continue;
              }
              //C     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
              //C                VECTORS OF ORIGINAL FULL MATRIX.
              //C                FOR J=N STEP -1 UNTIL LOW+1 DO -- ..........
              for (JJ=LOW;JJ<=ENM1;++JJ) { // L880
                 J = N + LOW - JJ;
                 M = mIn(J-1,IGH);

                 for (I=LOW;I<=IGH;++I) { // L880
                    ZZR = ZR(I,J);
                    ZZI = ZI(I,J);

                    for (K=LOW;K<=M;++K) { // L860
                       ZZR = ZZR + ZR(I,K) * HR(K,J) - ZI(I,K) * HI(K,J);
                       ZZI = ZZI + ZR(I,K) * HI(K,J) + ZI(I,K) * HR(K,J);
                       continue;
                    }

                    ZR(I,J) = ZZR;
                    ZI(I,J) = ZZI;
                    continue;
                 }
              }
              goto L1001;
      
              //C     .......... SET ERROR -- NO CONVERGENCE TO AN
              //C                EIGENVALUE AFTER 30*N ITERATIONS ..........
        L1000: IERR = EN;
        L1001: return;
        }


        void CORTH(int& NM,int& N,int& LOW,int& IGH,DFARRAY AR,DFARRAY AI,DFARRAY ORTR,DFARRAY ORTI)
        {
            //C***BEGIN PROLOGUE  CORTH
            //C***DATE WRITTEN   760101   (YYMMDD)
            //C***REVISION DATE  830518   (YYMMDD)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D4C1B2
            //C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
            //C***AUTHOR  SMITH, B. T., ET AL.
            //C***PURPOSE  Reduces complex general matrix to complex upper Hessenberg
            //C            using unitary similarity transformations.
            //C***DESCRIPTION
            //C
            //C     This subroutine is a translation of a complex analogue of
            //C     the ALGOL procedure ORTHES, NUM. MATH. 12, 349-368(1968)
            //C     by Martin and Wilkinson.
            //C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
            //C
            //C     Given a COMPLEX GENERAL matrix, this subroutine
            //C     reduces a submatrix situated in rows and columns
            //C     LOW through IGH to upper Hessenberg form by
            //C     unitary similarity transformations.
            //C
            //C     On INPUT
            //C
            //C        NM must be set to the row dimension of two-dimensional
            //C          array parameters as declared in the calling program
            //C          dimension statement.
            //C
            //C        N is the order of the matrix.
            //C
            //C        LOW and IGH are integers determined by the balancing
            //C          subroutine  CBAL.  if  CBAL  has not been used,
            //C          set LOW=1, IGH=N.
            //C
            //C        AR and AI contain the real and imaginary parts,
            //C          respectively, of the complex input matrix.
            //C
            //C     On OUTPUT
            //C
            //C        AR and AI contain the real and imaginary parts,
            //C          respectively, of the Hessenberg matrix.  Information
            //C          about the unitary transformations used in the reduction
            //C          is stored in the remaining triangles under the
            //C          Hessenberg matrix.
            //C
            //C        ORTR and ORTI contain further information about the
            //C          transformations.  Only elements LOW through IGH are used.
            //C
            //C     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
            //C
            //C     Questions and comments should be directed to B. S. Garbow,
            //C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
            //C     ------------------------------------------------------------------
            //C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
            //C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
            //C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
            //C                 1976.
            //C***ROUTINES CALLED  PYTHAG
            //C***END PROLOGUE  CORTH
            //C
              int I,J,M,II,JJ,LA,MP,KP1;
              AR.array(NM,N);
              AI.array(NM,N);
              ORTR.array(IGH);
              ORTI.array(IGH);
              double F,G,H,FI,FR,SCALE;

            //C
            //C***FIRST EXECUTABLE STATEMENT  CORTH
              LA = IGH - 1;
              KP1 = LOW + 1;
              if (LA  <  KP1) goto L200;

              for (M=KP1;M<=LA;++M) { // L180
                 H = 0.0E0;
                 ORTR(M) = 0.0E0;
                 ORTI(M) = 0.0E0;
                 SCALE = 0.0E0;
            //C     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
                 for (I=M;I<=IGH;++I) { // L90
                    SCALE = SCALE + fabs(AR(I,M-1)) + fabs(AI(I,M-1));
                 }

                 if (SCALE  ==  0.0E0) goto L180;
                 MP = M + IGH;
            //C     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
                 for (II=M;II<=IGH;++II) { // L100
                    I = MP - II;
                    ORTR(I) = AR(I,M-1) / SCALE;
                    ORTI(I) = AI(I,M-1) / SCALE;
                    H = H + ORTR(I) * ORTR(I) + ORTI(I) * ORTI(I);
                    continue;
                 }

                 G = sqrt(H);
                 F = PYTHAG(ORTR(M),ORTI(M));
                 if (F  ==  0.0E0) goto L103;
                 H = H + F * G;
                 G = G / F;
                 ORTR(M) = (1.0E0 + G) * ORTR(M);
                 ORTI(M) = (1.0E0 + G) * ORTI(M);
                 goto L105;

        L103:    ORTR(M) = G;
                 AR(M,M-1) = SCALE;
                 //C     .......... FORM (I-(U*UT)/H) * A ..........
        L105:    for (J=M;J<=N;++J) { // L130
                    FR = 0.0E0;
                    FI = 0.0E0;
                    //C     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
                    for (II=M;II<=IGH;++II) { // L110
                       I = MP - II;
                       FR = FR + ORTR(I) * AR(I,J) + ORTI(I) * AI(I,J);
                       FI = FI + ORTR(I) * AI(I,J) - ORTI(I) * AR(I,J);
                       continue;
                    }

                    FR = FR / H;
                    FI = FI / H;

                    for (I=M;I<=IGH;++I) { // L120
                       AR(I,J) = AR(I,J) - FR * ORTR(I) + FI * ORTI(I);
                       AI(I,J) = AI(I,J) - FR * ORTI(I) - FI * ORTR(I);
                       continue;
                    }

                    continue;
                 }
                 //C     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
                 for (I=1;I<=IGH;++I) { // L160
                    FR = 0.0E0;
                    FI = 0.0E0;
                    //C     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
                    for (JJ=M;JJ<=IGH;++JJ) { // L140
                       J = MP - JJ;
                       FR = FR + ORTR(J) * AR(I,J) - ORTI(J) * AI(I,J);
                       FI = FI + ORTR(J) * AI(I,J) + ORTI(J) * AR(I,J);
                       continue;
                    }

                    FR = FR / H;
                    FI = FI / H;

                    for (J=M;J<=IGH;++J) { // L150
                       AR(I,J) = AR(I,J) - FR * ORTR(J) - FI * ORTI(J);
                       AI(I,J) = AI(I,J) + FR * ORTI(J) - FI * ORTR(J);
                       continue;
                    }

                    continue;
                 }

                 ORTR(M) = SCALE * ORTR(M);
                 ORTI(M) = SCALE * ORTI(M);
                 AR(M,M-1) = -G * AR(M,M-1);
                 AI(M,M-1) = -G * AI(M,M-1);
        L180:    continue;
              }

        L200: return;
        }


        void CSROOT(const double& XR, const double& XI, DFARRAY YR, DFARRAY YI)
        {
            //C***BEGIN PROLOGUE  CSROOT
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***REFER TO  EISDOC
            //C
            //C     (YR,YI) = complex sqrt(XR,XI)
            //C***ROUTINES CALLED  PYTHAG
            //C***END PROLOGUE  CSROOT
              double S,TR,TI;
            //C
            //C     BRANCH CHOSEN SO THAT YR  >=  0.0 AND SIGN(YI)  ==  SIGN(XI)
            //C***FIRST EXECUTABLE STATEMENT  CSROOT
              TR = XR;
              TI = XI;
              S = sqrt(0.5E0*(PYTHAG(TR,TI) + fabs(TR)));
              if (TR  >=  0.0E0) YR = S;
              if (TI  <  0.0E0) S = -S;
              if (TR  <=  0.0E0) YI = S;
              if (TR  <  0.0E0) YR = 0.5E0*(TI/YI);
              if (TR  >  0.0E0) YI = 0.5E0*(TI/YR);
              return;
        }

        double PYTHAG(const double& A,const double& B)
        {
            //C***BEGIN PROLOGUE  PYTHAG
            //C***REFER TO  EISDOC
            //C
            //C     Finds sqrt(A**2+B**2) without overflow or destructive underflow
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  PYTHAG

              double P,Q,R,S,T;

              P = mAx(fabs(A),fabs(B));
              Q = mIn(fabs(A),fabs(B));
              if (Q  ==  0.0E0) goto L20;
        L10:  
              R = sqr(Q/P); 
              T = 4.0E0 + R;
              if (T  ==  4.0E0) goto L20;
              S = R/T;
              P = P + 2.0E0*P*S;
              Q = Q*S;
              goto L10;
        L20:  return (P);
        }

      
        void CDIV(const double& AR,const double& AI,const double& BR,const double& BI,DFARRAY CR,DFARRAY CI)
        {
            //C***BEGIN PROLOGUE  CDIV
            //C***REFER TO  EISDOC
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C
            //C     Complex division, (CR,CI) = (AR,AI)/(BR,BI)
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CDIV
              double S,ARS,AIS,BRS,BIS;

              S = fabs(BR) + fabs(BI);
              ARS = AR/S;
              AIS = AI/S;
              BRS = BR/S;
              BIS = BI/S;
              S = sqr(BRS) + sqr(BIS);
              CR = (ARS*BRS + AIS*BIS)/S;
              CI = (AIS*BRS - ARS*BIS)/S;
              return;
        }

        void CGEFA(FARRAY<COMPLEX> A, int LDA, int N, FARRAY<int> IPVT, int& INFO) 
        {
            //C***BEGIN PROLOGUE  CGEFA
            //C***DATE WRITTEN   780814   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D2C1
            //C***KEYWORDS  COMPLEX,FACTOR,LINEAR ALGEBRA,LINPACK,MATRIX
            //C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
            //C***PURPOSE  Factors a COMPLEX matrix by Gaussian elimination.
            //C***DESCRIPTION
            //C
            //C     CGEFA factors a complex matrix by Gaussian elimination.
            //C
            //C     CGEFA is usually called by CGECO, but it can be called
            //C     directly with a saving in time if  RCOND  is not needed.
            //C     (Time for CGECO) = (1 + 9/N)*(Time for CGEFA) .
            //C
            //C     On Entry
            //C
            //C        A       COMPLEX(LDA, N)
            //C                the matrix to be factored.
            //C
            //C        LDA     INTEGER
            //C                the leading dimension of the array  A .
            //C
            //C        N       INTEGER
            //C                the order of the matrix  A .
            //C
            //C     On Return
            //C
            //C        A       an upper triangular matrix and the multipliers
            //C                which were used to obtain it.
            //C                The factorization can be written  A = L*U  where
            //C                L  is a product of permutation and unit lower
            //C                triangular matrices and  U  is upper triangular.
            //C
            //C        IPVT    INTEGER(N)
            //C                an integer vector of pivot indices.
            //C
            //C        INFO    INTEGER
            //C                = 0  normal value.
            //C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
            //C                     condition for this subroutine, but it does
            //C                     indicate that CGESL or CGEDI will divide by zero
            //C                     if called.  Use  RCOND  in CGECO for a reliable
            //C                     indication of singularity.
            //C
            //C     LINPACK.  This version dated 08/14/78 .
            //C     Cleve Moler, University of New Mexico, Argonne National Lab.
            //C
            //C     Subroutines and Functions
            //C
            //C     BLAS CAXPY,CSCAL,ICAMAX
            //C     Fortran ABS,AIMAG,REAL
            //C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
            //C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
            //C***ROUTINES CALLED  CAXPY,CSCAL,ICAMAX
            //C***END PROLOGUE  CGEFA
            A.array(LDA,N);
            IPVT.array(N);

            COMPLEX T;
            int J,K,KP1,L,NM1;
            COMPLEX ZDUM;

            INFO = 0;
            NM1 = N - 1;
            if (NM1 >= 1) {
                for (K = 1;K<=NM1;++K) { 
                    KP1 = K + 1;
                    L = ICAMAX(N-K+1,A(K,K),1) + K - 1;
            
                    IPVT(K) = L;
                    if (CABS1(A(L,K)) == 0.0E0) {
                        INFO = K;
                    } else {
                        if (L != K) {
                            T = A(L,K);
                            A(L,K) = A(K,K);
                            A(K,K) = T;
                        }
                        T = -COMPLEX(1.0E0,0.0E0)/A(K,K);
                        CSCAL(N-K,T,A(K+1,K),1);
                        for (J=KP1;J<=N;++J) { 
                            T = A(L,J);
                            if (L != K) {
                                A(L,J) = A(K,J);
                                A(K,J) = T;
                            }
                            CAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1);
                        }
                    }        
                }
            }
            IPVT(N) = N;
            if (CABS1(A(N,N)) == 0.0E0) INFO = N;
            return;
        }
 
        void CAXPY(int N, COMPLEX& CA, FARRAY<COMPLEX> CX, int INCX, FARRAY<COMPLEX> CY, int INCY)
        { 
            //C***BEGIN PROLOGUE  CAXPY
            //C***DATE WRITTEN   791001   (YYMMDD)
            //C***REVISION DATE  840425   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C
            //C***CATEGORY NO.  D1A7
            //C***KEYWORDS  BLAS,COMPLEX,LINEAR ALGEBRA,TRIAD,VECTOR
            //C***AUTHOR  LAWSON, C. L., (JPL)
            //C           HANSON, R. J., (SNLA)
            //C           KINCAID, D. R., (U. OF TEXAS)
            //C           KROGH, F. T., (JPL)
            //C***PURPOSE  Complex computation y = a*x + y
            //C***DESCRIPTION
            //C
            //C                B L A S  Subprogram
            //C    Description of Parameters
            //C
            //C     --Input--
            //C        N  number of elements in input vector(s)
            //C       CA  complex scalar multiplier
            //C       CX  complex vector with N elements
            //C     INCX  storage spacing between elements of CX
            //C       CY  complex vector with N elements
            //C     INCY  storage spacing between elements of CY
            //C
            //C     --Output--
            //C       CY  complex result (unchanged if N .LE. 0)
            //C
            //C     Overwrite complex CY with complex  CA*CX + CY.
            //C     For I = 0 to N-1, replace  CY(LY+I*INCY) with CA*CX(LX+I*INCX) +
            //C       CY(LY+I*INCY), where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N
            //C       and LY is defined in a similar way using INCY.
            //C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
            //C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
            //C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
            //C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CAXPY
            //C
            //C***FIRST EXECUTABLE STATEMENT  CAXPY
            int KX,KY,I;
            COMPLEX CANORM = fabs(real(CA)) + fabs(imag(CA));
            if (N<=0||CANORM==0.E0) return;
            if (INCX==INCY&&INCX>0) {
                int NS = N*INCX;
                for (I=1;I<=NS;I+=INCX) { 
                    CY(I) = CA*CX(I) + CY(I);
                }
                return;
            }
            KX = 1;
            KY = 1;
            if (INCX<0) KX = 1+(1-N)*INCX;
            if (INCY<0) KY = 1+(1-N)*INCY;
            for (I=1;I<=N;++I) { 
                CY(KY) = CY(KY) + CA*CX(KX);
                KX = KX + INCX;
                KY = KY + INCY;
            }
            return;
        }


        void CSCAL(int N, COMPLEX CA, FARRAY<COMPLEX> CX, int INCX) 
        {
            //C***BEGIN PROLOGUE  CSCAL
            //C***DATE WRITTEN   791001   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C
            //C***CATEGORY NO.  D1A6
            //C***KEYWORDS  BLAS,COMPLEX,LINEAR ALGEBRA,SCALE,VECTOR
            //C***AUTHOR  LAWSON, C. L., (JPL)
            //C           HANSON, R. J., (SNLA)
            //C           KINCAID, D. R., (U. OF TEXAS)
            //C           KROGH, F. T., (JPL)
            //C***PURPOSE  Complex vector scale x = a*x
            //C***DESCRIPTION
            //C
            //C                B L A S  Subprogram
            //C    Description of Parameters
            //C
            //C     --Input--
            //C        N  number of elements in input vector(s)
            //C       CA  complex scale factor
            //C       CX  complex vector with N elements
            //C     INCX  storage spacing between elements of CX
            //C
            //C     --Output--
            //C    CSCAL  complex result (unchanged if N .LE. 0)
            //C
            //C     replace complex CX by complex CA*CX.
            //C     For I = 0 to N-1, replace CX(1+I*INCX) with  CA * CX(1+I*INCX)
            //C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
            //C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
            //C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
            //C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CSCAL
            //C
            //C***FIRST EXECUTABLE STATEMENT  CSCAL
            if (N <= 0) return;
            int NS = N*INCX;
            for (int I=1;I<=NS;I+=INCX) {
                  CX(I) = CA*CX(I);
            }
            return;
        }

        int ICAMAX(int N, FARRAY<COMPLEX> CX, int INCX)
        {
            //C***BEGIN PROLOGUE  ICAMAX
            //C***DATE WRITTEN   791001   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C
            //C***CATEGORY NO.  D1A2
            //C***KEYWORDS  BLAS,COMPLEX,LINEAR ALGEBRA,MAXIMUM COMPONENT,VECTOR
            //C***AUTHOR  LAWSON, C. L., (JPL)
            //C           HANSON, R. J., (SNLA)
            //C           KINCAID, D. R., (U. OF TEXAS)
            //C           KROGH, F. T., (JPL)
            //C***PURPOSE  Find largest component of complex vector
            //C***DESCRIPTION
            //C
            //C                B L A S  Subprogram
            //C    Description of Parameters
            //C
            //C     --Input--
            //C        N  number of elements in input vector(s)
            //C       CX  complex vector with N elements
            //C     INCX  storage spacing between elements of CX
            //C
            //C     --Output--
            //C   ICAMAX  smallest index (zero if N .LE. 0)
            //C
            //C      Returns the index of the component of CX having the
            //C      largest sum of magnitudes of real and imaginary parts.
            //C     ICAMAX = first I, I = 1 to N, to minimize
            //C        ABS(REAL(CX(1-INCX+I*INCX))) + ABS(IMAG(CX(1-INCX+I*INCX)))
            //C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
            //C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
            //C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
            //C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  ICAMAX
            //C
            //C***FIRST EXECUTABLE STATEMENT  ICAMAX
            int _ICAMAX = 0;
            if (N<=0) return _ICAMAX;
            _ICAMAX = 1;
            if (N <= 1) return _ICAMAX;
            int NS = N*INCX;
            int II = 1;
            double SUMMAX = fabs(real(CX(1))) + fabs(imag(CX(1)));
            for (int I=1;I<=NS;I+=INCX) {
                double SUMRI = fabs(real(CX(I))) + fabs(imag(CX(I)));
                if (SUMMAX<SUMRI) {
                    SUMMAX = SUMRI;
                    _ICAMAX = II;
                }
                II = II + 1;
            }
            return _ICAMAX;
        }

        void CGESL(FARRAY<COMPLEX> A, int LDA, int N, FARRAY<int> IPVT, FARRAY<COMPLEX> B, int JOB)
        {
            //C***BEGIN PROLOGUE  CGESL
            //C***DATE WRITTEN   780814   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C***CATEGORY NO.  D2C1
            //C***KEYWORDS  COMPLEX,LINEAR ALGEBRA,LINPACK,MATRIX,SOLVE
            //C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
            //C***PURPOSE  Solves the COMPLEX system  A*X=B  or  CTRANS(A)*X=B
            //C            using the factors computed by CGECO or CGEFA.
            //C***DESCRIPTION
            //C
            //C     CGESL solves the complex system
            //C     A * X = B  or  CTRANS(A) * X = B
            //C     using the factors computed by CGECO or CGEFA.
            //C
            //C     On Entry
            //C
            //C        A       COMPLEX(LDA, N)
            //C                the output from CGECO or CGEFA.
            //C
            //C        LDA     INTEGER
            //C                the leading dimension of the array  A .
            //C
            //C        N       INTEGER
            //C                the order of the matrix  A .
            //C
            //C        IPVT    INTEGER(N)
            //C                the pivot vector from CGECO or CGEFA.
            //C
            //C        B       COMPLEX(N)
            //C                the right hand side vector.
            //C
            //C        JOB     INTEGER
            //C                = 0         to solve  A*X = B ,
            //C                = nonzero   to solve  CTRANS(A)*X = B  where
            //C                            CTRANS(A)  is the conjugate transpose.
            //C
            //C     On Return
            //C
            //C        B       the solution vector  X .
            //C
            //C     Error Condition
            //C
            //C        A division by zero will occur if the input factor contains a
            //C        zero on the diagonal.  Technically this indicates singularity
            //C        but it is often caused by improper arguments or improper
            //C        setting of LDA .  It will not occur if the subroutines are
            //C        called correctly and if CGECO has set RCOND .GT. 0.0
            //C        or CGEFA has set INFO .EQ. 0 .
            //C
            //C     To compute  INVERSE(A) * C  where  C  is a matrix
            //C     with  P  columns
            //C           CALL CGECO(A,LDA,N,IPVT,RCOND,Z)
            //C           IF (RCOND is too small) GO TO ...
            //C           DO 10 J = 1, P
            //C              CALL CGESL(A,LDA,N,IPVT,C(1,J),0)
            //C        10 CONTINUE
            //C
            //C     LINPACK.  This version dated 08/14/78 .
            //C     Cleve Moler, University of New Mexico, Argonne National Lab.
            //C
            //C     Subroutines and Functions
            //C
            //C     BLAS CAXPY,CDOTC
            //C     Fortran CONJG
            //C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
            //C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
            //C***ROUTINES CALLED  CAXPY,CDOTC
            //C***END PROLOGUE  CGESL
            A.array(LDA,N);
            B.array(N);
            IPVT.array(N);

            COMPLEX T;
            int K,KB,L,NM1;
            NM1 = N - 1;
            if (JOB == 0) {
                //C
                //C        JOB = 0 , SOLVE  A * X = B
                //C        FIRST SOLVE  L*Y = B
                //C
                if (NM1 >= 1) {
                    for (K=1;K<=NM1;++K) { // line20
                        L = IPVT(K);
                        T = B(L);
                        if (L != K) {
                            B(L) = B(K);
                            B(K) = T;
                        }
                        CAXPY(N-K,T,A(K+1,K),1,B(K+1),1);
                    }
            
                    //C
                    //C        NOW SOLVE  U*X = Y
                    //C
                    for (KB=1;KB<=N;++KB) {
                        K = N + 1 - KB;
                        B(K) = B(K)/A(K,K);
                        T = -B(K);
                        CAXPY(K-1,T,A(1,K),1,B(1),1);
                    }
                }
            }   else {
        
                //C
                //C        JOB = NONZERO, SOLVE  CTRANS(A) * X = B
                //C        FIRST SOLVE  CTRANS(U)*Y = B
                //C
                for (K=1;K<=N;++K) {
                    T = CDOTC(K-1,A(1,K),1,B(1),1);
                    B(K) = (B(K) - T)/conj(A(K,K));
                } 
                //C
                //C        NOW SOLVE CTRANS(L)*X = Y
                //C
                if (NM1 >= 1) { 
                    for (KB=1;KB<=NM1;++KB) {
                        K = N - KB;
                        B(K) = B(K) + CDOTC(N-K,A(K+1,K),1,B(K+1),1);
                        L = IPVT(K);
                        if (L != K) {
                            T = B(L);
                            B(L) = B(K);
                            B(K) = T;
                        }
                    }
                }
            }
        }

        COMPLEX CDOTC(int N, FARRAY<COMPLEX> CX, int INCX, FARRAY<COMPLEX> CY, int INCY)
        {
            //C***BEGIN PROLOGUE  CDOTC
            //C***DATE WRITTEN   791001   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C   061020  Converted to C++ by Thomas Germer for the SCATMECH library (NIST)
            //C
            //C***CATEGORY NO.  D1A4
            //C***KEYWORDS  BLAS,COMPLEX,INNER PRODUCT,LINEAR ALGEBRA,VECTOR
            //C***AUTHOR  LAWSON, C. L., (JPL)
            //C           HANSON, R. J., (SNLA)
            //C           KINCAID, D. R., (U. OF TEXAS)
            //C           KROGH, F. T., (JPL)
            //C***PURPOSE  Dot product of complex vectors, uses complx conjugate of
            //C            first vector
            //C***DESCRIPTION
            //C
            //C                B L A S  Subprogram
            //C    Description of Parameters
            //C
            //C     --Input--
            //C        N  number of elements in input vector(s)
            //C       CX  complex vector with N elements
            //C     INCX  storage spacing between elements of CX
            //C       CY  complex vector with N elements
            //C     INCY  storage spacing between elements of CY
            //C
            //C     --Output--
            //C    CDOTC  complex result (zero if N .LE. 0)
            //C
            //C     Returns the dot product for complex CX and CY, uses CONJUGATE(CX)
            //C     CDOTC = SUM for I = 0 to N-1 of CONJ(CX(LX+I*INCX))*CY(LY+I*INCY)
            //C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
            //C     defined in a similar way using INCY.
            //C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
            //C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
            //C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
            //C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CDOTC
            //C
            //C***FIRST EXECUTABLE STATEMENT  CDOTC
            COMPLEX _CDOTC(0.,0.);
            int I,KX,KY;
            if (N < 0) return _CDOTC;
            if (INCX==INCY&&INCX>0) {
                int NS = N*INCX;
                for (I=1;I<=NS;I+=INCX) {
                    _CDOTC = conj(CX(I))*CY(I) + _CDOTC;
                }
                return _CDOTC;
            } else {
                KX = 1;
                KY = 1;
                if (INCX<0) KX = 1+(1-N)*INCX;
                if (INCY<0) KY = 1+(1-N)*INCY;
                for (I=1;I<=N;++I) {
                    _CDOTC = _CDOTC + conj(CX(KX))*CY(KY);
                    KX = KX + INCX;
                    KY = KY + INCY;
                }
                return _CDOTC;
            }
        }

        void CGEDI(CFARRAY A, int LDA, int N, IFARRAY IPVT, CFARRAY DET, CFARRAY WORK, int JOB) 
        {
            //C***BEGIN PROLOGUE  CGEDI
            //C***DATE WRITTEN   780814   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C***CATEGORY NO.  D2C1,D3C1
            //C***KEYWORDS  COMPLEX,DETERMINANT,FACTOR,INVERSE,LINEAR ALGEBRA,LINPACK,
            //C             MATRIX
            //C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
            //C***PURPOSE  Computes the determinant and inverse of a COMPLEX matrix
            //C            using the factors computed by CGECO or CGEFA.
            //C***DESCRIPTION
            //C
            //C     CGEDI computes the determinant and inverse of a matrix
            //C     using the factors computed by CGECO or CGEFA.
            //C
            //C     On Entry
            //C
            //C        A       COMPLEX(LDA, N)
            //C                the output from CGECO or CGEFA.
            //C
            //C        LDA     INTEGER
            //C                the leading dimension of the array  A .
            //C
            //C        N       INTEGER
            //C                the order of the matrix  A .
            //C
            //C        IPVT    INTEGER(N)
            //C                the pivot vector from CGECO or CGEFA.
            //C
            //C        WORK    COMPLEX(N)
            //C                work vector.  Contents destroyed.
            //C
            //C        JOB     INTEGER
            //C                = 11   both determinant and inverse.
            //C                = 01   inverse only.
            //C                = 10   determinant only.
            //C
            //C     On Return
            //C
            //C        A       inverse of original matrix if requested.
            //C                Otherwise unchanged.
            //C
            //C        DET     COMPLEX(2)
            //C                determinant of original matrix if requested.
            //C                Otherwise not referenced.
            //C                Determinant = DET(1) * 10.0**DET(2)
            //C                with  1.0 .LE. CABS1(DET(1)) .LT. 10.0
            //C                or  DET(1) .EQ. 0.0 .
            //C
            //C     Error Condition
            //C
            //C        A division by zero will occur if the input factor contains
            //C        a zero on the diagonal and the inverse is requested.
            //C        It will not occur if the subroutines are called correctly
            //C        and if CGECO has set RCOND .GT. 0.0 or CGEFA has set
            //C        INFO .EQ. 0 .
            //C
            //C     LINPACK.  This version dated 08/14/78 .
            //C     Cleve Moler, University of New Mexico, Argonne National Lab.
            //C
            //C     Subroutines and Functions
            //C
            //C     BLAS CAXPY,CSCAL,CSWAP
            //C     Fortran ABS,AIMAG,CMPLX,MOD,REAL
            //C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
            //C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
            //C***ROUTINES CALLED  CAXPY,CSCAL,CSWAP
            //C***END PROLOGUE  CGEDI
            A.array(LDA,N);
            WORK.array(N);
            IPVT.array(N);

            //C
            COMPLEX T;
            double TEN;
            int I,J,K,KB,KP1,L,NM1;
            COMPLEX ZDUM;
            //C
            //C     COMPUTE DETERMINANT
            //C
            //C***FIRST EXECUTABLE STATEMENT  CGEDI
            if (JOB/10 != 0) {
                DET(1) = COMPLEX(1.0E0,0.0E0);
                DET(2) = COMPLEX(0.0E0,0.0E0);
                TEN = 10.0E0;
                for (I=1;I<=N;++I) { // L50
                    if (IPVT(I) != I) DET(1) = -DET(1);
                    DET(1) = A(I,I)*DET(1);
                    //C        ...EXIT
                    if (CABS1(DET(1)) == 0.0E0) break;
                    while (CABS1(DET(1)) < 1.0E0) {
                        DET(1) = COMPLEX(TEN,0.0E0)*DET(1);
                        DET(2) = DET(2) - COMPLEX(1.0E0,0.0E0);
                    }
                    while (CABS1(DET(1)) >= TEN) {
                        DET(1) = DET(1)/COMPLEX(TEN,0.0E0);
                        DET(2) = DET(2) + COMPLEX(1.0E0,0.0E0);
                    }
                }
            }
            //C
            //C     COMPUTE INVERSE(U)
            //C
            if (JOB%10 != 0) {
                for (K=1;K<=N;++K) {
                    A(K,K) = COMPLEX(1.0E0,0.0E0)/A(K,K);
                    T = -A(K,K);
                    CSCAL(K-1,T,A(1,K),1);
                    KP1 = K + 1;
                    if (N >= KP1) {
                        for (J = KP1;J<=N;++J) {
                            T = A(K,J);
                            A(K,J) = COMPLEX(0.0E0,0.0E0);
                            CAXPY(K,T,A(1,K),1,A(1,J),1);
                        }
                    }
                }
                //C
                //C        FORM INVERSE(U)*INVERSE(L)
                //C
                NM1 = N - 1;
                if (NM1 >= 1) {
                    for (KB=1;KB<=NM1;++KB) {
                        K = N - KB;
                        KP1 = K + 1;
                        for (I=KP1;I<=N;++I) {
                            WORK(I) = A(I,K);
                            A(I,K) = COMPLEX(0.0E0,0.0E0);
                        }
                        for (J=KP1;J<=N;++J) {
                            T = WORK(J);
                            CAXPY(N,T,A(1,J),1,A(1,K),1);
                        }
                        L = IPVT(K);
                        if (L != K) CSWAP(N,A(1,K),1,A(1,L),1);
                    }
                }
            }
        }
 

        void CSWAP(int N,CFARRAY CX, int INCX, CFARRAY CY, int INCY) 
        {
            //C***BEGIN PROLOGUE  CSWAP
            //C***DATE WRITTEN   791001   (YYMMDD)
            //C***REVISION DATE  820801   (YYMMDD)
            //C***REVISION HISTORY  (YYMMDD)
            //C   000330  Modified array declarations.  (JEC)
            //C
            //C***CATEGORY NO.  D1A5
            //C***KEYWORDS  BLAS,COMPLEX,INTERCHANGE,LINEAR ALGEBRA,VECTOR
            //C***AUTHOR  LAWSON, C. L., (JPL)
            //C           HANSON, R. J., (SNLA)
            //C           KINCAID, D. R., (U. OF TEXAS)
            //C           KROGH, F. T., (JPL)
            //C***PURPOSE  Interchange complex vectors
            //C***DESCRIPTION
            //C
            //C                B L A S  Subprogram
            //C    Description of Parameters
            //C
            //C     --Input--
            //C        N  number of elements in input vector(s)
            //C       CX  complex vector with N elements
            //C     INCX  storage spacing between elements of CX
            //C       CY  complex vector with N elements
            //C     INCY  storage spacing between elements of CY
            //C
            //C     --Output--
            //C       CX  input vector CY (unchanged if N .LE. 0)
            //C       CY  input vector CX (unchanged if N .LE. 0)
            //C
            //C     Interchange complex CX and complex CY
            //C     For I = 0 to N-1, interchange  CX(LX+I*INCX) and CY(LY+I*INCY),
            //C     where LX = 1 if INCX .GT. 0, else LX = (-INCX)*N, and LY is
            //C     defined in a similar way using INCY.
            //C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
            //C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
            //C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
            //C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
            //C***ROUTINES CALLED  (NONE)
            //C***END PROLOGUE  CSWAP
            //C
            COMPLEX CTEMP;
            int KX,KY,I,NS;
            //C***FIRST EXECUTABLE STATEMENT  CSWAP
            if (N <= 0) return;
            if (INCX==INCY&&INCX>0) {
                NS = N*INCX;
                for (I=1;I<=NS;I+=INCX) {
                    CTEMP = CX(I);
                    CX(I) = CY(I);
                    CY(I) = CTEMP;
                }
        
            } else {
                KX = 1;
                KY = 1;
                if (INCX<0) KX = 1+(1-N)*INCX;
                if (INCY<0) KY = 1+(1-N)*INCY;
                for (I=1;I<=N;++I) {
                    CTEMP = CX(KX);
                    CX(KX) = CY(KY);
                    CY(KY) = CTEMP;
                    KX = KX + INCX;
                    KY = KY + INCY;
                }
            }
        }


    } // namespace SCATMECH::CMLIB


} // namespace SCATMECH
