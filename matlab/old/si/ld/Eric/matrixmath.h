//******************************************************************************
//** SCATMECH: Polarized Light Scattering C++ Class Library
//** 
//** File: matrixmath.h
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
#ifndef EIGEN_CGEEV_H
#define EIGEN_CGEEV_H

#include "scatmech.h"
#include <vector>

namespace SCATMECH {
	
	int eigen(std::vector<COMPLEX>& A,
			  std::vector<COMPLEX>& Q,
			  std::vector<COMPLEX>& W,int nmat);

	void LUdecompose(std::vector<COMPLEX>& matrix,int nmat,std::vector<int>& pivot);
	void LUbacksubstitute(std::vector<COMPLEX>& matrix,int nmat,std::vector<int>& pivot,std::vector<COMPLEX>& b);
	void LUImprove(std::vector<COMPLEX>& a, std::vector<COMPLEX>& alud, int n, std::vector<int>& indx, std::vector<COMPLEX>& b, std::vector<COMPLEX>& x);

	void Inverse(std::vector<COMPLEX>& matrix, int nmat);

	namespace CMLIB {
		
		// 
		// The template class FARRAY is a Fortran-like array.
		//
		template <class T>
			class FARRAY
		{
			public:
				FARRAY() {p = NULL; step=0; dealloc_on_destroy=false;}
				FARRAY(T* t) {p = t; step=0; dealloc_on_destroy=false;}
				FARRAY(T& t) {p = &t; step=0; dealloc_on_destroy=false;}
				FARRAY(std::vector<T>& t) {p = &t[0],step=0;dealloc_on_destroy=false;}
				FARRAY(const FARRAY& a) {p = a.p; step = a.step; dealloc_on_destroy=false;}
				
				~FARRAY() {if (dealloc_on_destroy) delete[] p;}
				
				T& operator()(int i) {return p[i-1];}
				T& operator()(int i,int j) {return p[(i-1)+step*(j-1)];}
				
				T& operator[](int i) {return p[i];}
				
				void array(int i,int j) {
					step = i;
				}
				void array(int i) {
					step = 0;
				}
				
				void allocate(int i,int j) {
					if (dealloc_on_destroy) delete[] p;
					step = i;
					p = new T[i*j];
					dealloc_on_destroy=true;
				}
				
				void allocate(int i) {
					if (dealloc_on_destroy) delete[] p;
					step = 0;
					p = new T[i];
					dealloc_on_destroy=true;
				}
				
				void deallocate() {
					if (dealloc_on_destroy) delete[] p;
				}
				
				T& operator=(const T& v) {*p = v; return *p;}
				
				operator T() const {return *p;}
				
			private:
				bool dealloc_on_destroy;
				int step;
				T* p;
			};
		
			typedef FARRAY<double> DFARRAY;
		    typedef FARRAY<COMPLEX> CFARRAY;
		    typedef FARRAY<int> IFARRAY;
			
			void CGEEV(DFARRAY A,int LDA, int N, DFARRAY E0,DFARRAY V, int LDV, DFARRAY WORK, int JOB, int& INFO);
			void CGEFA(CFARRAY A, int LDA, int N, IFARRAY IPVT, int& INFO);
			void CGESL(CFARRAY A, int LDA, int N, IFARRAY IPVT, CFARRAY B, int JOB);
			void CGEDI(CFARRAY A, int LDA, int N, IFARRAY IPVT, CFARRAY DET, CFARRAY WORK, int JOB); 
	}
}

#endif

