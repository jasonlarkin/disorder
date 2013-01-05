#include <iostream>
#include <fstream>
#include <math.h>
#include <iomanip>
#include <time.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_sort_double.h>
#include <gsl/gsl_rng.h>
using namespace std;
#include <complex>
#include <vector>
#include <stdlib.h>
#include "matrixmath.h"
using namespace SCATMECH;

void fc_SW_numerical(int ii, int jj, double ****fc, double **x_disp, int *type, int **neighbor_list, int N);
double pe_i(int atom_i, int *type, double **x_disp, int **neighbor_list, int N);
void chime(int time, double **x, double *m, int N, int N_sc);		// output for Chime visualization
double pe_ij(int atom_i, int atom_j, int *type, double **x_disp, int **neighbor_list, int N);
void build_structure(double **x, double *m, int *type, double lc[3], double mass, int atom_type, int nx, int ny, int nz, int N);
void find_neighbors(double **x, int *type, int **neighbor_list, int N);
void freq_eig_vel(double kappa[3], double **** fc, double **x, double lc[3], int Natoms_cell, int N_cells, double **inv_sqrtm, double frequency[], double velocity[][3], gsl_matrix_complex * evec);
void kappa_z(double kappax, double kappay, double omega, double **** fc, double **x, double lc[3], int Natoms_cell, int N_cells, double **inv_sqrtm, gsl_complex allowed_kz[], double velocity[][3], gsl_matrix_complex * evec, int side, int err[1]);
int check_kappa(double kappa[3], double lc[3]);
double distance2BZB(double kappa[3], double lc[3]);

ofstream temp_out("temp.txt");
ofstream error_out("error_log.txt");
ofstream chime_out("structure.xyz");							// Chime output stream
ofstream transmission_out;
ofstream fc_output("fc.txt");
ofstream progress_out("progress.txt");
ifstream fc_input("fc_in.txt");
ifstream junction_in("junction.txt");
ifstream wv_in("wv_in.txt");

// some switches
bool wv_file = false;		// whether or not the wavevectors are to be read from an input file (false = random)
bool all_modes = true;		// do full calculation for all modes, even if totally reflected

//***********************************************************************************************************************

// Stillinger-Weber variables
double b = 1.8;	// cutoff
double b2 = b*b;						
double epsilon[2][2], sigma[2][2], inv_sigma[2][2], sigma2[2][2], lamda[2], sqrt_epsilon[2][2], sqrt_lamda[2][2];
double sqrt_sqrt_lamda[2][2], epsilon_over_sigma[2][2], C[2][2][2], epsilon_Si, sigma_Si, mass_Si;
const int num_neighbors = 16;   		// number of atoms in neighbor list
double h = 0.001;						// step used in numerical derivative

// Some constants
#define kB 1.38065E-23						
double const Pie = atan(1) * 4.;
double precision = 1.E-6;
double var_precision = 1.E-6;
double low_precision = 1.E-3;

// NUMBER OF WAVEVECTORS USED IN CALCULATION
int num_k = 100000;

int main() {
	
	// MORE VARIABLE DECLARATION
	char filename[255];
	int i, j, j_prime, k, ii, jj, ff, gg, hh, oo, kk, mm, nn, ee, l, l_prime, alpha, beta, zz, count;
	double r2, r8, rij[3];
	double kappa[3];		// real wave vector
	int err_L[1]; err_L[0] = 0;
	int err_R[1]; err_R[0] = 0;
	double dot_product;
	
	// INFORMATION FOR STILLINGER-WEBER POTENTIAL (as implemented in the MD code)
	// Silicon is species 0 and germanium is species 1
	epsilon_Si = 2.17*1.60217733E-19; 	// J 
	sigma_Si = 2.0951E-10;  			// m 
	mass_Si = 4.66371E-26;    			// kg (mass of Si, average of isotopes according to natural abundance)
	epsilon[0][0] = 1.;	epsilon[1][1] = 1.93/2.17; epsilon[0][1] = epsilon[1][0] = 2.0427/2.17;  
	sigma[0][0] = 1.; sigma[1][1] = 2.181/2.0951; sigma[0][1] = sigma[1][0] = 0.5*(sigma[0][0] + sigma[1][1]);   
	lamda[0] = 21.0; lamda[1] = 31.0; 
	for (i = 0; i < 2; i++) {
		for (j = 0; j < 2; j++) {
			inv_sigma[i][j] = 1./sigma[i][j]; sigma2[i][j] = sigma[i][j]*sigma[i][j]; epsilon_over_sigma[i][j] = epsilon[i][j]/sigma[i][j];
			sqrt_epsilon[i][j] = sqrt(epsilon[i][j]); sqrt_lamda[i][j] = sqrt(lamda[i]*lamda[j]); sqrt_sqrt_lamda[i][j] = pow(lamda[i]*lamda[j], 0.25);
			for (k = 0; k < 2; k++) C[j][i][k] = sqrt(epsilon[i][j]*epsilon[i][k])*pow(lamda[j]*lamda[i]*lamda[i]*lamda[k],0.25);
		}
	} 
	
	// SCALING FACTORS
	double f_scale = 0.5*(1./Pie)*1.E-12*sqrt(epsilon_Si/(sigma_Si*sigma_Si*mass_Si)); 	// convert from nondimensional angular frequency to THz
	double R_scale = sigma_Si*sigma_Si*sigma_Si*(1./kB)*sqrt(mass_Si/epsilon_Si)*1.E9;	// convert from nondimensional R to 1E-9 m2-K/W
	
	
	// LEAD DEFINITIONS
	int Natoms_cell = 2;								// number of atoms per unit cell
	int nx, ny, nz; nx = ny = nz = 1;					// number of times to +- translate unit cell in each direction
	int Natoms_tot = Natoms_cell*(2*nx + 1)*(2*ny + 1)*(2*nz + 1);	// total number of atoms
	int N_cells = Natoms_tot/Natoms_cell;				// number of unit cells
	double ML, MR; 										// mass of atoms in left/right lead
	int TL, TR;											// atom type in left/right lead
	double lcL[3], lcR[3];								// lattice constants
	junction_in >> TL >> ML >> lcL[0] >> lcL[1] >> lcL[2];
	junction_in >> TR >> MR >> lcR[0] >> lcR[1] >> lcR[2];
	
	// SUPERCELL INFORMATION
	int N_file;											// number of atoms in the junction input file
	int N_sc;											// number of atoms in the supercell
	int N_overlap;
	double b_L, b_R;
	double x_int, x_overlap_l, x_overlap_r;
	junction_in >> N_sc >> N_file >> b_L >> b_R;
	
	// VARIABLE DECLARATION
	// 1D arrays
	int * typeL; typeL = new int[Natoms_tot];	
	double * mL; mL = new double[Natoms_tot];
	int * typeR; typeR = new int[Natoms_tot];	
	double * mR; mR = new double[Natoms_tot];
	int * typeJ; typeJ = new int[N_file];	
	int * positionJ; positionJ = new int[N_file];
	double * mJ; mJ = new double[N_file];
	
	// 2D arrays
	double ** xL; xL = new double * [Natoms_tot]; for (i = 0; i < Natoms_tot; i++) xL[i] = new double[3];
	double ** xR; xR = new double * [Natoms_tot]; for (i = 0; i < Natoms_tot; i++) xR[i] = new double[3];
	double ** xJ; xJ = new double * [N_file]; for (i = 0; i < N_file; i++) xJ[i] = new double[3];
	int ** neighbor_listL; neighbor_listL = new int * [Natoms_tot]; for (i = 0; i < Natoms_tot; i++) neighbor_listL[i] = new int[num_neighbors];
	int ** neighbor_listR; neighbor_listR = new int * [Natoms_tot]; for (i = 0; i < Natoms_tot; i++) neighbor_listR[i] = new int[num_neighbors];
	int ** neighbor_listJ; neighbor_listJ = new int * [N_file]; for (i = 0; i < N_file; i++) neighbor_listJ[i] = new int[num_neighbors];
	double ** inv_sqrtmL; inv_sqrtmL = new double * [Natoms_cell]; for (i = 0; i < Natoms_cell; i++) inv_sqrtmL[i] = new double[Natoms_tot];
	double ** inv_sqrtmR; inv_sqrtmR = new double * [Natoms_cell]; for (i = 0; i < Natoms_cell; i++) inv_sqrtmR[i] = new double[Natoms_tot];
	double ** wv; wv = new double * [num_k]; for (i = 0; i < num_k; i++) wv[i] = new double[3];
		
	// 4D arrays
	double **** fcL = new double *** [Natoms_cell];
	for (i = 0; i < Natoms_cell; i++) {
		fcL[i] = new double ** [Natoms_tot];
		for (j = 0; j < Natoms_tot; j++) { fcL[i][j] = new double * [3]; for (k = 0; k<3; k++) fcL[i][j][k] = new double[3]; }
	}
	double **** fcR = new double *** [Natoms_cell];
	for (i = 0; i < Natoms_cell; i++) {
		fcR[i] = new double ** [Natoms_tot];
		for (j = 0; j < Natoms_tot; j++) { fcR[i][j] = new double * [3]; for (k = 0; k<3; k++) fcR[i][j][k] = new double[3]; }
	}
	double **** fcJ = new double *** [N_sc];
	for (i = 0; i < N_sc; i++) {
		fcJ[i] = new double ** [N_file];
		for (j = 0; j < N_file; j++) { fcJ[i][j] = new double * [3]; for (k = 0; k<3; k++) fcJ[i][j][k] = new double[3]; }
	}

	// BUILD LEADS AND READ IN JUNCTION INFORMATION FROM INPUT FILE
	build_structure(xL, mL, typeL, lcL, ML, TL, nx, ny, nz, Natoms_tot);	// left lead 
	build_structure(xR, mR, typeR, lcR, MR, TR, nx, ny, nz, Natoms_tot); 	// right lead
	for (i = 0; i < N_file; i++) junction_in >> typeJ[i] >> mJ[i] >> positionJ[i] >> xJ[i][0] >> xJ[i][1] >> xJ[i][2];
	double center = 0.5*(b_L + b_R); for (i = 0; i < N_file; i++) xJ[i][2] -= center; b_L -= center; b_R -= center;
	
	// OUTPUT JUNCTION TO CHIME
	chime(0, xJ, mJ, N_file, N_sc);
		
	// FIND NEIGHBORS	
	find_neighbors(xL, typeL, neighbor_listL, Natoms_tot);
	find_neighbors(xR, typeR, neighbor_listR, Natoms_tot);
	find_neighbors(xJ, typeJ, neighbor_listJ, N_file);
	
	// GENERATE FORCE CONSTANT MATRICES
	cout<< "Generating force constant matrices ... ";
	for (i = 0; i<Natoms_cell; i++) {
		for (j = 0; j<Natoms_tot; j++) {
			inv_sqrtmL[i][j] = 1./sqrt(mL[i]*mL[j]);
			inv_sqrtmR[i][j] = 1./sqrt(mR[i]*mR[j]);
			fc_SW_numerical(i, j, fcL, xL, typeL, neighbor_listL, Natoms_tot);
			fc_SW_numerical(i, j, fcR, xR, typeR, neighbor_listR, Natoms_tot);
		}
	}
	for (i = 0; i<N_sc; i++) { 
		cout<< i << endl; 
		for (j = 0; j<N_file; j++) fc_SW_numerical(i, j, fcJ, xJ, typeJ, neighbor_listJ, N_file); 
	}	
	cout<< "done." << endl;
	
	// FIND THE DEFINTION OF EACH ATOM
	int definition[N_file];	// 0 - junction atom
							// 1 - lead atom that can see the junction
							// 2 - lead atom that can see an atom that sees the junction
							// 3 - lead atom that can't see the junction or any atom that can see the junction
	for (i = 0; i < N_file; i++) definition[i] = -1; // just setting an initial value
	int n0, n1L, n1R, n2L, n2R, n3L, n3R; n0 = n1L = n1R = n2L = n2R = n3L = n3R = 0;
	
	// find def-0 atoms
	for (i = 0; i < N_file; i++) if (xJ[i][2] > b_L && xJ[i][2] < b_R) definition[i] = 0;
	
	// find def-1 atoms
	bool interaction;
	for (i = 0; i < N_sc; i++) {
		if (xJ[i][2] < b_L || xJ[i][2] > b_R) {
			interaction = false;
			for (j = 0; j < N_file; j++) {
				if (xJ[j][2] > b_L && xJ[j][2] < b_R) {
					for (alpha = 0; alpha < 3; alpha++) {
						for (beta = 0; beta < 3; beta++) {
							if (fabs(fcJ[i][j][alpha][beta]) > precision) interaction = true;
						}
					}
				}
			}
			if (interaction == true) definition[i] = 1;
		}
	}
	for (i = 0; i < N_file; i++) definition[i] = definition[i%N_sc];
	
	// find def-2 atoms and def-3 atoms
	for (i = 0; i < N_sc; i++) {
		if (definition[i] != 0 && definition[i] != 1) {
			interaction = false;
			j = 0;
			while (interaction == false && j < N_file) {
				if (definition[j] == 1) {
					for (alpha = 0; alpha < 3; alpha++) {
						for (beta = 0; beta < 3; beta++) {
							if (fabs(fcJ[i][j][alpha][beta]) > precision) interaction = true;
						}
					}
				}
				j++;
			}
			if (interaction == true) definition[i] = 2;
			else definition[i] = 3;
		}
	} 
	for (i = 0; i < N_file; i++) definition[i] = definition[i%N_sc];
	for (i = 0; i < N_sc; i++) {
		if (definition[i] == 0) n0++;
		if (definition[i] == 1 & xJ[i][2] < b_L) n1L++;
		if (definition[i] == 1 & xJ[i][2] > b_R) n1R++;
		if (definition[i] == 2 & xJ[i][2] < b_L) n2L++;
		if (definition[i] == 2 & xJ[i][2] > b_R) n2R++;
		if (definition[i] == 3 & xJ[i][2] < b_L) n3L++;
		if (definition[i] == 3 & xJ[i][2] > b_R) n3R++;
	}
	
	cout << "atom definitions" << endl;
	for (i = 0; i < N_sc; i++) cout << definition[i] << " "; cout << endl;
	bool test = false;
	for (i = 0; i < N_file; i++) if (definition[i] == 3) test = true;
	if (test == false) cout << "There are no def-3 atoms. This is fine if you've tested on a larger size." << endl;
	
	// READ IN WAVEVECTORS FROM FILE (if applicable)
	if (wv_file == true) for (i = 0; i < num_k; i++) wv_in >> wv[i][0] >> wv[i][1] >> wv[i][2]; 
		
	// EVEN MORE VARIABLE DECLARATION
	double f_incident[3*Natoms_cell], v_incident[3*Natoms_cell][3], v_reflected[3*Natoms_cell][3], v_transmitted[3*Natoms_cell][3];
 	gsl_matrix_complex * e_incident = gsl_matrix_complex_alloc(3*Natoms_cell, 3*Natoms_cell);
 	gsl_matrix_complex * e_reflected = gsl_matrix_complex_alloc(3*Natoms_cell, 3*Natoms_cell);
 	gsl_matrix_complex * e_transmitted = gsl_matrix_complex_alloc(3*Natoms_cell, 3*Natoms_cell);
 	gsl_complex kz_R[3*Natoms_cell], kz_T[3*Natoms_cell], tempRef, tempInc, tempTran, complex_dot_product, temp;
 	double T[3*Natoms_cell], R[3*Natoms_cell]; 
 	int numR, numT; int energy_errors = 0; int kappa_errors = 0;
	double Tcont[3*Natoms_cell][4], Rcont[3*Natoms_cell][4], kzT_saved[3*Natoms_cell][4];
 	double vzT_saved[3*Natoms_cell][4], kzR_saved[3*Natoms_cell][4], vzR_saved[3*Natoms_cell][4];
 	
 	// SET UP THE RANDOM NUMBER GENERATOR
	const gsl_rng_type * type_random = gsl_rng_default;
	gsl_rng * random = gsl_rng_alloc (type_random);
	gsl_rng_set(random, (unsigned)time(NULL));
	gsl_rng_env_setup();
	
	double min_lc; bool rand_k_found;
	
 	// START OF CALCULATION //
	for (ii = 0; ii < 1; ii++) {
				
		int begin = time(0);
		
		if (ii != 0) transmission_out.close();
		sprintf(filename, "transmission_%d.txt", ii); transmission_out.open(filename);
		
		energy_errors = kappa_errors = 0;
		 		
		for (ee = 0; ee < num_k; ee++) {
			
		 	// select a wavevector in the FBZ (from a file or randomly generated)
		 	if (wv_file == true ) { for (kk = 0; kk < 3; kk++) kappa[kk] = wv[ee][kk]; }
		 	else {
				min_lc = 10000.; for (kk = 0; kk < 3; kk++) { if (lcL[kk] < min_lc) min_lc = lcL[kk]; }
				rand_k_found = false;			
				while (rand_k_found == false) {
					for (kk = 0; kk < 3; kk++) kappa[kk] = (gsl_rng_uniform(random) - 0.5)*(4.*Pie/min_lc);
			 		if (check_kappa(kappa, lcL) == 0) rand_k_found = true; 
		 		}
			}	 	
			
			// calculate the mode shapes, velocities, and frequencies of all modes at that wavevector
			freq_eig_vel(kappa, fcL, xL, lcL, Natoms_cell, N_cells, inv_sqrtmL, f_incident, v_incident, e_incident);
			
			err_L[0] = err_R[0] = 0; // clear error log
			
			for (hh = 0; hh < 3*Natoms_cell; hh++) R[hh] = T[hh] = 10000.;
			
			for (hh = 0; hh < 3*Natoms_cell; hh++) {
					
				if (v_incident[hh][2] > 0.) { 		// only do calculation for phonons traveling towards the interface
				
					// Calculate the allowed reflected and transmitted wavevectors
					kappa_z(kappa[0], kappa[1], f_incident[hh], fcL, xL, lcL, Natoms_cell, N_cells, inv_sqrtmL, kz_R, v_reflected, e_reflected, 0, err_L);		
					kappa_z(kappa[0], kappa[1], f_incident[hh], fcR, xR, lcR, Natoms_cell, N_cells, inv_sqrtmR, kz_T, v_transmitted, e_transmitted, 1, err_R);
					
					// Check to see if there were any errors in calculating the allowed wavevectors
					if (err_L[0] == 0 && err_R[0] == 0) {
					
						// count the number of reflected and transmitted modes
						numT = numR = 0;
						int num_real_T = 0;			
						for (i = 0; i < 3*Natoms_cell; i++) {
							if (GSL_REAL(kz_R[i]) != 10000.) numR++; 
							if (GSL_REAL(kz_T[i]) != 10000.) numT++;
							if (fabs(GSL_IMAG(kz_T[i])) < precision) num_real_T++;
						}
						
						// only do calculation if there are real transmitted modes
						if (num_real_T > 0 || all_modes == true) {
						
							// num_eq is #EOM + #SBE
							int num_eq = 3*(n0 + n1L + n1R) + 3*(n1L + n1R + n2L + n2R);
							int num_var = 3*(n0 + n1L + n1R + n2L + n2R) + numR + numT;
							if (num_var > num_eq) {cout<< "There are more variables than equations." << endl; exit(0);}
							gsl_matrix_complex * A = gsl_matrix_complex_calloc(num_eq, num_var);
							gsl_vector_complex * b = gsl_vector_complex_calloc(num_eq);
							for (i = 0; i<num_eq; i++) {for (j = 0; j<num_var; j++) gsl_matrix_complex_set(A, i, j, gsl_complex_rect(1E-30,1E-30));}				
							
							// EQUATIONS OF MOTION FOR DEF-0 and DEF-1 ATOMS IN SUPERCELL
							for (i = 0; i < N_sc; i++) { 
								if (definition[i] == 0 || definition[i] == 1) {
									for (j = 0; j < N_file; j++) { 
										if (definition[j] != 3) { // for all atoms that interact with i
											for (kk = 0; kk < 2; kk++) rij[kk] = xJ[j][kk] - xJ[j%N_sc][kk];
											temp = gsl_complex_exp(gsl_complex_rect(0., kappa[0]*rij[0] + kappa[1]*rij[1]));
											for (alpha=0; alpha<3; alpha++) {
												for (beta=0; beta<3; beta++) {
													gsl_matrix_complex_set(A, 3*(i - n3L - n2L) + alpha, 3*(j%N_sc - n3L) + beta, gsl_complex_add(gsl_matrix_complex_get(A, 3*(i - n3L - n2L) + alpha, 3*(j%N_sc - n3L) + beta), gsl_complex_mul_real(temp, fcJ[i][j][alpha][beta])));
												}
											}
										}
									} // end of j loop
									for (alpha = 0; alpha < 3; alpha++) gsl_matrix_complex_set(A, 3*(i - n3L - n2L) + alpha, 3*(i%N_sc - n3L) + alpha, gsl_complex_sub_real(gsl_matrix_complex_get(A, 3*(i - n3L - n2L) + alpha, 3*(i%N_sc - n3L) + alpha), mJ[i]*f_incident[hh]*f_incident[hh]));
								}
							} // end of i loop
							int equation = 3*(n0 + n1L + n1R); 
								
							//  SCATTERING BOUNDARY EQUATIONS FOR DEF-1 AND DEF-2 ATOMS IN SUPERCELL 
							for (i = 0; i < N_sc; i++) {
								if (definition[i] == 1 || definition[i] == 2) {
									if (xJ[i][2] < b_L) { // incident and reflected side
										dot_product = kappa[0]*xJ[i][0] + kappa[1]*xJ[i][1] + kappa[2]*xJ[i][2];
										tempInc = gsl_complex_exp(gsl_complex_rect(0., dot_product));
										tempInc = gsl_complex_div_real(tempInc, sqrt(mJ[i]));
										for (alpha = 0; alpha < 3; alpha++) {
											gsl_matrix_complex_set(A, equation, 3*(i - n3L) + alpha, gsl_complex_rect(1.,0.));  // 1's
											gsl_vector_complex_set(b, equation, gsl_complex_mul(gsl_matrix_complex_get(e_incident, 3*positionJ[i] + alpha, hh), tempInc)); // b vector
											equation++;
										}
										equation -= 3;
										for (j = 0; j < numR; j++) {
											complex_dot_product = gsl_complex_rect(kappa[0]*xJ[i][0] + kappa[1]*xJ[i][1] + GSL_REAL(kz_R[j])*xJ[i][2], GSL_IMAG(kz_R[j])*xJ[i][2]);
											tempRef = gsl_complex_exp(gsl_complex_mul(complex_dot_product, gsl_complex_rect(0.,1.)));
											tempRef = gsl_complex_div_real(tempRef, -1.*sqrt(mJ[i]));	
											for (alpha = 0; alpha < 3; alpha++) {
												temp = gsl_complex_mul(gsl_matrix_complex_get(e_reflected, 3*positionJ[i] + alpha, j), tempRef);
												gsl_matrix_complex_set(A, equation, 3*(n0 + n1L + n1R + n2L + n2R) + j, temp);
												equation++;
											}
											equation -= 3;
										}
										equation += 3;
									}
									if (xJ[i][2] > b_R) { // transmitted side
										for (alpha = 0; alpha < 3; alpha++) {
											gsl_matrix_complex_set(A, equation, 3*(i - n3L) + alpha, gsl_complex_rect(1.,0.));  // 1's
											equation++;
										}
										equation -= 3;
										for (j = 0; j < numT; j++) {
											complex_dot_product = gsl_complex_rect(kappa[0]*xJ[i][0] + kappa[1]*xJ[i][1] + GSL_REAL(kz_T[j])*xJ[i][2], GSL_IMAG(kz_T[j])*xJ[i][2]);
											tempTran = gsl_complex_exp(gsl_complex_mul(complex_dot_product, gsl_complex_rect(0.,1.)));
											tempTran = gsl_complex_div_real(tempTran, -1.*sqrt(mJ[i]));	
											for (alpha = 0; alpha < 3; alpha++) {
												temp = gsl_complex_mul(gsl_matrix_complex_get(e_transmitted, 3*positionJ[i] + alpha, j), tempTran);
												gsl_matrix_complex_set(A, equation, 3*(n0 + n1L + n1R + n2L + n2R) + numR + j, temp);
												equation++;
											}
											equation -= 3;
										}
										equation += 3;
									}
								}
							} 
						
							// solve the system of equations (rectangular matrix)
		 					gsl_matrix * Ahuge = gsl_matrix_calloc(2*num_eq, 2*num_var);
							gsl_vector * bhuge = gsl_vector_calloc(2*num_eq);
							for (i = 0; i< num_eq; i++) {
								for (j = 0; j < num_var; j++) {
									gsl_matrix_set(Ahuge, i, j, GSL_REAL(gsl_matrix_complex_get(A, i, j)));
									gsl_matrix_set(Ahuge, i, j + num_var, -1.*GSL_IMAG(gsl_matrix_complex_get(A, i, j)));
									gsl_matrix_set(Ahuge, i + num_eq, j, GSL_IMAG(gsl_matrix_complex_get(A, i, j)));
									gsl_matrix_set(Ahuge, i + num_eq, j + num_var, GSL_REAL(gsl_matrix_complex_get(A, i, j)));
								}
								gsl_vector_set(bhuge, i, GSL_REAL(gsl_vector_complex_get(b, i)));
								gsl_vector_set(bhuge, i + num_eq, GSL_IMAG(gsl_vector_complex_get(b, i)));
							} 				
							gsl_matrix * V = gsl_matrix_alloc(2*num_var, 2*num_var);
							gsl_vector * S = gsl_vector_alloc(2*num_var);
							gsl_vector * work = gsl_vector_alloc(2*num_var);
							gsl_vector * solu = gsl_vector_alloc(2*num_var);				
							gsl_linalg_SV_decomp(Ahuge, V, S, work);
							gsl_linalg_SV_solve(Ahuge, V, S, bhuge, solu); 
							gsl_matrix_free(V);
							gsl_vector_free(S);
							gsl_vector_free(work);
							gsl_matrix_complex_free(A);
							gsl_vector_complex_free(b);
							gsl_matrix_free(Ahuge);
							gsl_vector_free(bhuge); 
								
							// CALCULATE R AND T
							R[hh] = T[hh] = 0.; double soluR, soluI, contribution;
							count = 0;
							for (j = 0; j < numR; j++) {
								if (fabs(GSL_IMAG(kz_R[j])) < precision) { 
									soluR = gsl_vector_get(solu, num_var - numR - numT + j);
									soluI = gsl_vector_get(solu, 2*num_var - numR - numT + j);
									contribution = (soluR*soluR + soluI*soluI)*fabs((v_reflected[j][2]/v_incident[hh][2]));
									R[hh] += contribution;
									Rcont[hh][count] = contribution;
									kzR_saved[hh][count] = GSL_REAL(kz_R[j]);
									vzR_saved[hh][count] = v_reflected[j][2];	
									count++;
								}
							}
							for (j = count; j < 4; j++) {
								Rcont[hh][j] = 0.;
								kzR_saved[hh][j] = 1.;
								vzR_saved[hh][j] = 1.;
							}	
							count = 0;
							for (j = 0; j < numT; j++) {
								if (fabs(GSL_IMAG(kz_T[j])) < precision) { 
									soluR = gsl_vector_get(solu, num_var - numT + j);
									soluI = gsl_vector_get(solu, 2*num_var - numT + j);
									contribution = (soluR*soluR + soluI*soluI)*fabs((v_transmitted[j][2]/v_incident[hh][2]))*(lcL[2]/lcR[2]);
									T[hh] += contribution;
									Tcont[hh][count] = contribution;
									kzT_saved[hh][count] = GSL_REAL(kz_T[j]);
									vzT_saved[hh][count] = v_transmitted[j][2];	
									count++;
								}
							}
							for (j = count; j < 4; j++) {
								Tcont[hh][j] = 0.;
								kzT_saved[hh][j] = -1.;
								vzT_saved[hh][j] = -1.;
							}	
								
							// CHECK TO SEE IF ENERGY IS CONSERVED
							// When energy is not conserved, it is most likely due to the incident phonon velocity being too small.
							// If energy is not conserved and the velocity is less than some cutoff, T[hh] is just set to zero because
							// these modes don't contribute much to the thermal transport anyway.
							// NOTE: these things are rare
							if (v_incident[hh][2] > low_precision && fabs(1 - R[hh] - T[hh]) > 0.02) err_L[0] = err_R[0] = 1;
							if (v_incident[hh][2] < low_precision && fabs(1 - R[hh] - T[hh]) > 0.1) { 
								R[hh] = 1.; 
								T[hh] = 0.; 
								for (j = 0; j < 4; j++) {
									Rcont[hh][j] = 0.;
									kzR_saved[hh][j] = 1.;
									vzR_saved[hh][j] = 1.;
								}	
								for (j = 0; j < 4; j++) {
									Tcont[hh][j] = 0.;
									kzT_saved[hh][j] = -1.;
									vzT_saved[hh][j] = -1.;
								}	
							}
							gsl_vector_free(solu);
						} // if there are transmitted phonons
						else {             	// if all_modes == false and there are no possible transmitted modes, the code 
							R[hh] = 1.; 	// doesn't calculate the detailed information about the reflected modes and 
							T[hh] = 0.; 	// instead just says that R = 1, T=0
							for (j = 0; j < 4; j++) {
								Rcont[hh][j] = 0.;
								kzR_saved[hh][j] = 1.;
								vzR_saved[hh][j] = 1.;
							}	
							for (j = 0; j < 4; j++) {
								Tcont[hh][j] = 0.;
								kzT_saved[hh][j] = -1.;
								vzT_saved[hh][j] = -1.;
							}	
						} // if there are no transmitted phonons
					} // errors 
				} // incident phonons
			} // hh loop
			
			// OUTPUT INFORMATION
			double incidence_angle;
			if (err_L[0] == 0 && err_R[0] == 0) { // if there are no errors, output transmission information
				for (hh = 0; hh < 3*Natoms_cell; hh++) {
					if (v_incident[hh][2] > 0.) {
						incidence_angle = acos(v_incident[hh][2]/sqrt(v_incident[hh][0]*v_incident[hh][0] + v_incident[hh][1]*v_incident[hh][1] + v_incident[hh][2]*v_incident[hh][2]));
						transmission_out << kappa[0] << "\t" << kappa[1] << "\t" << kappa[2] << "\t" << f_incident[hh] << "\t" << v_incident[hh][2] << "\t" << incidence_angle << "\t\t" << T[hh] << "\t" << T[hh]+R[hh] << "\t\t";
						for (i = 0; i < 4; i++) transmission_out << kzR_saved[hh][i] << "\t" << vzR_saved[hh][i] << "\t" << Rcont[hh][i] << "\t\t"; 
						for (i = 0; i < 4; i++) transmission_out << kzT_saved[hh][i] << "\t" << vzT_saved[hh][i] << "\t" << Tcont[hh][i] << "\t\t"; 
						transmission_out << endl;
					}
				}
			}
			else { // (THIS SHOULD BE VERY INFREQUENT, usually has trouble with less than 1/1000 of the wavevectors dues to numerical tolerances)
				if (err_L[0] == 1 && err_R[0] == 1) energy_errors++;
				else kappa_errors++;
				double BZscale; BZscale = distance2BZB(kappa, lcL);
				for (hh = 0; hh < 3*Natoms_cell; hh++) {
					if (v_incident[hh][2] > 0.) {
						error_out << kappa[0] << "\t" << kappa[1] << "\t" << kappa[2] << "\t" << BZscale << "\t" << f_incident[hh] << "\t" << v_incident[hh][2] << "\t" << T[hh]+R[hh] << "\t" << err_L[0] << "\t" << err_R[0] << endl;
					}
				}
				if (wv_file == false) ee--; // do another wavevector
			}
		//	progress_out << ii << "\t" << ee << "\t" << energy_errors << "\t" << kappa_errors << endl;
			cout << ii << "\t" << ee << "\t" << energy_errors << "\t" << kappa_errors << "\t" << kappa[0] << "\t" << kappa[1] << "\t" << kappa[2] << endl;
		} // ee
		cout << "Calculation " << ii << " finished after " << double(time(0) - begin)/3600. << " hours." << endl;
	} // ii
	
 	///////////////////////////////////////////////////////////////////////////////////////////////////////////
 	///////////////////////////////////////////////////////////////////////////////////////////////////////////

	cout<< "Deleting dynamic memory .... " << endl;
	gsl_matrix_complex_free(e_incident);
	gsl_matrix_complex_free(e_reflected);
	gsl_matrix_complex_free(e_transmitted);
	gsl_rng_free (random);

	// delete dynamic memory
	delete [] typeL; delete [] typeR; delete [] typeJ;
	delete [] mL; delete [] mR; delete [] mJ;
	delete [] positionJ;
	
	for (i = 0; i<Natoms_tot; i++) {delete [] xL[i]; delete [] xR[i]; delete [] neighbor_listL[i]; delete [] neighbor_listR[i];}
	for (i = 0; i<Natoms_cell;i++) {delete [] inv_sqrtmL[i]; delete [] inv_sqrtmR[i];}
	for (i = 0; i<num_k;i++) {delete [] wv[i];}	
	delete [] xL; delete [] xR;
	delete [] neighbor_listL; delete [] neighbor_listR;
	delete [] inv_sqrtmL; delete [] inv_sqrtmR;
	delete [] wv;
	for (i = 0; i<N_file; i++) { delete [] xJ[i]; delete [] neighbor_listJ[i];}
	delete [] xJ; delete [] neighbor_listJ;
	
	for (i = 0; i < Natoms_cell; i++) {for (j = 0; j < Natoms_tot; j++) {for (k = 0; k<3; k++) {delete [] fcL[i][j][k]; delete [] fcR[i][j][k];}}}
	for (i = 0; i < Natoms_cell; i++) {for (j = 0; j < Natoms_tot; j++) {delete [] fcL[i][j]; delete [] fcR[i][j];}}
	for (i = 0; i < Natoms_cell; i++) {delete [] fcL[i]; delete [] fcR[i];}
	delete [] fcL; delete [] fcR;
	
	for (i = 0; i<N_sc; i++) {for (j = 0; j < N_file; j++) {for (k = 0; k<3; k++) delete [] fcJ[i][j][k];}}
	for (i = 0; i<N_sc; i++) {for (j = 0; j < N_file; j++) delete [] fcJ[i][j];}
	for (i = 0; i<N_sc; i++) delete [] fcJ[i];
	delete [] fcJ;
}	// end of program
/////////////////////////////////////////////////////////////////////////////////////////////////////
void chime(int time, double **x, double *m, int N, int N_sc) {
	// create Chime file
	int i;
	double scale = 2.5; 
	char letter;
	chime_out << N << endl<< time << endl;
	for(i=0;i<N;i++) {
		if (m[i] < 1.1) letter = 'S';
		else letter = 'C';
		if (i < N_sc) letter = 'O';
		chime_out<<letter<<"\t"<<scale*x[i][0]<<"\t"<<scale*x[i][1]<<"\t"<<scale*x[i][2]<<endl;
	}
}
//////////////////////////////////////////////////////////////////////////////////////////////
void fc_SW_numerical(int ii, int jj, double ****fc, double **x_disp, int *type, int **neighbor_list, int N) {
	// potential energy calculation
	double PHI1, PHI2, PHI3, PHI4;
	int i, j, k, alpha, beta, m;
	PHI1 = PHI2 = PHI3 = PHI4 = 0.;
	double inv_4h2 = 0.25/(h*h);
	for (alpha =  0; alpha <3; alpha++) {
		for (beta = 0; beta < 3; beta++) {
			if (jj == ii) {
				PHI1 = PHI2 = PHI3 = PHI4 = 0.;
				x_disp[ii][alpha] += h;  
				x_disp[jj][beta] += h;
				PHI1 = pe_i(ii, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] -= 2.*h;
				PHI2 = pe_i(ii, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] += 2.*h;
				x_disp[jj][beta] -= 2.*h;
				PHI3 = pe_i(ii, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] -= 2*h;
				PHI4 = pe_i(ii, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] += h;
				x_disp[jj][beta] += h;
				fc[ii][jj][alpha][beta] = inv_4h2*(PHI1 - PHI2 - PHI3 + PHI4);
			}
			else {
				PHI1 = PHI2 = PHI3 = PHI4 = 0.;
				x_disp[ii][alpha] += h;  
				x_disp[jj][beta] += h;
				PHI1 = pe_ij(ii, jj, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] -= 2.*h;
				PHI2 = pe_ij(ii, jj, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] += 2.*h;
				x_disp[jj][beta] -= 2.*h;
				PHI3 = pe_ij(ii, jj, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] -= 2*h;
				PHI4 = pe_ij(ii, jj, type, x_disp, neighbor_list, N);
				x_disp[ii][alpha] += h;
				x_disp[jj][beta] += h;
				fc[ii][jj][alpha][beta] = inv_4h2*(PHI1 - PHI2 - PHI3 + PHI4);
			}
		}
	}
}
/////////////////////////////////////////////////////////////////////////////////////////////////////
double pe_ij(int atom_i, int atom_j, int *type, double **x_disp, int **neighbor_list, int N){
	// calculates the potential energy of atom i	
	int i, j, k, m, kk, typei, typej, typek;
	double Ei;
	double A = 7.049556277;
	double B = 0.6022245584;
	double one_third = 1./3.;
	double rij[3], rik[3], rjk[3], Rij, Rik, Rjk, yij, yik, yjk, cos_theta_one_third, inv_yijb;
	double inv_Rij, inv_Rik, inv_Rjk, inv_yij, r2, inv_yij4, exp_inv_yijb, exp_inv_yikb, exp_inv_yjkb;
	bool neighbor_ij;
	bool neighbor_ik = false;
	bool neighbor_jk = false;
	i = atom_i;
	j = atom_j;
	typei = type[i];
	typej = type[j];
	Ei = 0.;
	// TWO-BODY TERM
	// rij
	neighbor_ij = false;
	for (m = 0; m<num_neighbors; m++) {
		if (neighbor_list[i][m] == j) {
			for (kk=0; kk<3; kk++) rij[kk] = x_disp[i][kk] - x_disp[j][kk];
			r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
			if (r2 < b2*sigma2[typei][typej]) {
				neighbor_ij = true;
				Rij = sqrt(r2);
				yij = Rij*inv_sigma[typei][typej];
				inv_Rij = 1./Rij;
				inv_yij = inv_Rij*sigma[typei][typej];
				inv_yij4 = inv_yij*inv_yij;
				inv_yij4 = inv_yij4*inv_yij4;
				inv_yijb = 1./(yij - b);
				exp_inv_yijb = exp(1.2*inv_yijb);
				Ei += epsilon[typei][typej]*A*(B*inv_yij4 - 1)*exp(inv_yijb);
			}
		}
	}
	// THREE-BODY TERM, need to check all k's that could be neighbors to i or j
	for (k = 0; k<N; k++) {
		if (k != j && k != i) {
			// calculate rij, rik, and rjk
			// rik
			typek = type[k];
			neighbor_ik = false;
			for (m = 0; m<num_neighbors; m++) {
				if (neighbor_list[i][m] == k) {
					for (kk=0; kk<3; kk++) rik[kk] = x_disp[i][kk] - x_disp[k][kk];
					r2 = rik[0]*rik[0] + rik[1]*rik[1] + rik[2]*rik[2];
					if (r2 < (b2*sigma2[typei][typek])) {
						neighbor_ik = true;
						Rik = sqrt(r2);
						inv_Rik = 1./Rik;
						yik = Rik*inv_sigma[typei][typek];
						exp_inv_yikb = exp(1.2/(yik - b));
					}
				}
			}
			// rjk
			neighbor_jk = false;
			for (m = 0; m<num_neighbors; m++) {
				if (neighbor_list[j][m] == k) {
					for (kk=0; kk<3; kk++) rjk[kk] = x_disp[j][kk] - x_disp[k][kk];
					r2 = rjk[0]*rjk[0] + rjk[1]*rjk[1] + rjk[2]*rjk[2];
					if (r2 < (b2*sigma2[typej][typek])){
						neighbor_jk = true;
						Rjk = sqrt(r2);
						inv_Rjk = 1./Rjk;
						yjk = Rjk*inv_sigma[typej][typek];
						exp_inv_yjkb = exp(1.2/(yjk - b));
					}
				}
			}
			/// THREE-BODY TERMS
			// h(rij, rik, theta_jik)				
			if (neighbor_ij == true && neighbor_ik == true) {
				cos_theta_one_third = one_third + (rij[0]*rik[0] + rij[1]*rik[1] + rij[2]*rik[2])*inv_Rij*inv_Rik;
				Ei += C[typej][typei][typek]*exp_inv_yijb*exp_inv_yikb*cos_theta_one_third*cos_theta_one_third;
			}
			// h(rji, rjk, theta_ijk)				
			if (neighbor_ij == true && neighbor_jk == true) {
				cos_theta_one_third = one_third - (rij[0]*rjk[0] + rij[1]*rjk[1] + rij[2]*rjk[2])*inv_Rij*inv_Rjk;
				Ei += C[typei][typej][typek]*exp_inv_yijb*exp_inv_yjkb*cos_theta_one_third*cos_theta_one_third;	
			}
			// h(rki, rkj, theta_ikj)				
			if (neighbor_ik == true && neighbor_jk == true) {
				cos_theta_one_third = one_third + (rik[0]*rjk[0] + rik[1]*rjk[1] + rik[2]*rjk[2])*inv_Rik*inv_Rjk;
				Ei += C[typei][typek][typej]*exp_inv_yikb*exp_inv_yjkb*cos_theta_one_third*cos_theta_one_third;	
			}
		}
	} 
	return Ei;
} 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
double pe_i(int atom_i, int *type, double **x_disp, int **neighbor_list, int N){
	// calculates the potential energy of atom i	
	
	int i, j, k, m, kk, typei, typej, typek;
	double Ei;
	double A = 7.049556277;
	double B = 0.6022245584;
	double one_sixth = 1./6.;
	double one_third = 1./3.;
	double rij[3], rik[3], rjk[3], Rij, Rik, Rjk, yij, yik, yjk, cos_theta_one_third, inv_yijb;
	double inv_Rij, inv_Rik, inv_Rjk, inv_yij, r2, inv_yij4, exp_inv_yijb, exp_inv_yikb, exp_inv_yjkb;
	bool neighbor = false;
	i = atom_i;
	typei = type[i];
	Ei = 0.;
	for (j = 0; j<N; j++) {
		if (j != i) {
			typej = type[j];
			// rij
			for (m = 0; m<num_neighbors; m++) {
				if (neighbor_list[i][m] == j) neighbor = true; // check to see if j is a neighbor to i
			}
			if (neighbor == true) {
				for (kk=0; kk<3; kk++) rij[kk] = x_disp[i][kk] - x_disp[j][kk];
				r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
				// check to see if the distance is within the cutoff
				if (r2 < (b2*sigma2[typei][typej])){
					Rij = sqrt(r2);
					yij = Rij*inv_sigma[typei][typej];
					inv_Rij = 1./Rij;
					inv_yij = inv_Rij*sigma[typei][typej];
					inv_yij4 = inv_yij*inv_yij;
					inv_yij4 = inv_yij4*inv_yij4;
					inv_yijb = 1./(yij - b);
					exp_inv_yijb = exp(1.2*inv_yijb);
					// TWO-BODY TERM
					Ei += epsilon[typei][typej]*A*(B*inv_yij4 - 1)*exp(inv_yijb);
				}
				else yij = b+100;
			}
			else yij = b+100;
			for (k = 0; k<N; k++) {
				if (k != j && k != i) {
					typek = type[k];
					// calculate rij, rik, and rjk
					// rik
					neighbor = false;
					for (m = 0; m<num_neighbors; m++) {
						if (neighbor_list[i][m] == k) neighbor = true; // check to see if k is a neighbor to i
					}
					if (neighbor == true) {
						for (kk=0; kk<3; kk++) rik[kk] = x_disp[i][kk] - x_disp[k][kk];
						r2 = rik[0]*rik[0] + rik[1]*rik[1] + rik[2]*rik[2];
						// check to see if the distance is within the cutoff
						if (r2 < (b2*sigma2[typei][typek])){
							Rik = sqrt(r2);
							inv_Rik = 1./Rik;
							yik = Rik*inv_sigma[typei][typek];
							exp_inv_yikb = exp(1.2/(yik - b));
						}
						else yik = b+100;
					}
					else yik = b+100;
					// rjk
					neighbor = false;
					for (m = 0; m<num_neighbors; m++) {
						if (neighbor_list[j][m] == k) neighbor = true; // check to see if k is a neighbor to j
					}
					if (neighbor == true) {
						for (kk=0; kk<3; kk++) rjk[kk] = x_disp[j][kk] - x_disp[k][kk];
						r2 = rjk[0]*rjk[0] + rjk[1]*rjk[1] + rjk[2]*rjk[2];
						// check to see if the distance is within the cutoff
						if (r2 < (b2*sigma2[typej][typek])){
							Rjk = sqrt(r2);
							inv_Rjk = 1./Rjk;
							yjk = Rjk*inv_sigma[typej][typek];
							exp_inv_yjkb = exp(1.2/(yjk - b));
						}
						else yjk = b+100;
					}
					else yjk = b+100; 
					/// THREE-BODY TERMS
					// h(rij, rik, theta_jik)				
					if (yij < b && yik < b) {
						cos_theta_one_third = one_third + (rij[0]*rik[0] + rij[1]*rik[1] + rij[2]*rik[2])*inv_Rij*inv_Rik;
						Ei += 0.5*C[typej][typei][typek]*exp_inv_yijb*exp_inv_yikb*cos_theta_one_third*cos_theta_one_third;
					}
					// h(rji, rjk, theta_ijk)				
					if (yij < b && yjk < b) {
						cos_theta_one_third = one_third - (rij[0]*rjk[0] + rij[1]*rjk[1] + rij[2]*rjk[2])*inv_Rij*inv_Rjk;
						Ei += 0.5*C[typei][typej][typek]*exp_inv_yijb*exp_inv_yjkb*cos_theta_one_third*cos_theta_one_third;	
					}
					// h(rki, rkj, theta_ikj)				
					if (yik < b && yjk < b) {
						cos_theta_one_third = one_third + (rik[0]*rjk[0] + rik[1]*rjk[1] + rik[2]*rjk[2])*inv_Rik*inv_Rjk;
						Ei += 0.5*C[typei][typek][typej]*exp_inv_yikb*exp_inv_yjkb*cos_theta_one_third*cos_theta_one_third;	
					}
				}
			}
		}
	}
	return Ei;
} 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
void build_structure(double **x, double *m, int *type, double lc[3], double mass, int atom_type, int nx, int ny, int nz, int N){
	int i, j, k, kk;
	int count = 0;
	bool test;
	// translational vectors
	double a1[3], a2[3], a3[3];
	a1[0] = 0.; a1[1] = 0.5*lc[0]; a1[2] = 0.5*lc[2];
	a2[0] = 0.5*lc[0]; a2[1] = 0.; a2[2] = 0.5*lc[2];
	a3[0] = 0.5*lc[0]; a3[1] = 0.5*lc[1]; a3[2] = 0.;
	
	// reference cell
	for (kk = 0; kk < 3; kk++) {
		x[count][kk] = 0.; 
		x[count+1][kk] = x[count][kk] + 0.25*lc[kk];
	}
	count += 2;
	
	for (i = -nx; i <= nx; i++) {
		for (j = -ny; j <= ny; j++) {
			for (k = -nz; k <= nz; k++) {
				test = false;
				if (i == 0 && j == 0 && k == 0) test = true;
				if (test == false) {
					for (kk = 0; kk < 3; kk++) {
						x[count][kk] = i*a1[kk] + j*a2[kk] + k*a3[kk];
						x[count+1][kk] = x[count][kk] + 0.25*lc[kk];
					}
					count += 2;
				}
			}
		}
	}
	for (i = 0; i<N; i++) {
		type[i] = atom_type;
		m[i] = mass;
	}
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void find_neighbors(double **x, int *type, int **neighbor_list, int N) {
	int i, j, k, kk;
	double rij[3], r2;
	double max;				// of the neighbors in the list, this is the maximum distance
	int max_location;		// location of neighbor with max distance
	int neig;
	for (i=0; i<N; i++)	{	//
		// temporary values (for first time through)
		neig = 0;
		max = 1000000.;	// max distance (of atoms in neighbor list)
		max_location = 0;
		for (k = 0; k<num_neighbors; k++) neighbor_list[i][k] = 1000000;
		for (j = 0; j<N; j++) {	
			if (j!=i) {
				// calculate rij
				for (kk=0; kk<3; kk++) rij[kk] = x[j][kk] - x[i][kk];
				r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
				if (r2 < ((1.8*1.8)*sigma2[type[i]][type[j]])) neig++;
				if (r2 < max) { // if the distance is less than the maximum one stored, find replace it
					neighbor_list[i][max_location] = j;	
					// find the maximum distance of the atoms currently stored in the neighbor list
					max = 0.;
					for (k = 0; k < num_neighbors; k++) {
						if (neighbor_list[i][k] != 1000000) {
							for (kk=0; kk<3; kk++) rij[kk] = x[neighbor_list[i][k]][kk] - x[i][kk];
							r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
							if (r2 > max) {
								max = r2;
								max_location = k;
							}
						}
						else {
							max = 1000000.;
							max_location = k;
						}
					}
				}
			}
		}
		if (neig > num_neighbors) {
			cout << "The neighbor list is too small." << endl;
			exit(0);
		}
	}
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
void freq_eig_vel(double kappa[3], double **** fc, double **x, double lc[3], int Natoms_cell, int N_cells, double **inv_sqrtm, double frequency[], double velocity[][3], gsl_matrix_complex * evec){
	/////////////// CALCULATE THE FREQUENCIES, VELOCITIES, AND EIGENVECTORS FOR A GIVEN WAVEVECTOR
	int j, j_prime, kk, alpha, beta, l, l_prime, ii, i;
	double rij[3], dot_product, dot_product_plus, dot_product_minus;
	gsl_complex temp, temp2, temp3, temp4, invmexpikR;
	gsl_matrix_complex * D = gsl_matrix_complex_calloc(3*Natoms_cell, 3*Natoms_cell);
	gsl_matrix_complex * dDdkx = gsl_matrix_complex_calloc(3*Natoms_cell, 3*Natoms_cell);
	gsl_matrix_complex * dDdky = gsl_matrix_complex_calloc(3*Natoms_cell, 3*Natoms_cell);
	gsl_matrix_complex * dDdkz = gsl_matrix_complex_calloc(3*Natoms_cell, 3*Natoms_cell);
	for (j=0; j < Natoms_cell; j++) {
		for (j_prime = 0; j_prime < Natoms_cell; j_prime++) {
			for (l_prime=0; l_prime<N_cells; l_prime++) {	
				for (kk=0; kk<3; kk++) rij[kk] = x[l_prime*Natoms_cell + j_prime][kk] - x[j][kk];
				dot_product = kappa[0]*rij[0] + kappa[1]*rij[1] + kappa[2]*rij[2];
				invmexpikR = gsl_complex_mul_real(gsl_complex_exp(gsl_complex_rect(0., dot_product)), inv_sqrtm[j][l_prime*Natoms_cell + j_prime]);
				for (alpha=0; alpha<3; alpha++) {
					for (beta=0; beta<3; beta++) {
						temp = gsl_complex_mul_real(invmexpikR, fc[j][l_prime*Natoms_cell + j_prime][alpha][beta]);
						temp2 = gsl_complex_mul(temp, gsl_complex_rect(0., rij[0]));
						temp3 = gsl_complex_mul(temp, gsl_complex_rect(0., rij[1]));
						temp4 = gsl_complex_mul(temp, gsl_complex_rect(0., rij[2]));
						gsl_matrix_complex_set(D, 3*j + alpha, 3*j_prime + beta, gsl_complex_add(gsl_matrix_complex_get(D, 3*j + alpha, 3*j_prime + beta), temp));
						gsl_matrix_complex_set(dDdkx, 3*j + alpha, 3*j_prime + beta, gsl_complex_add(gsl_matrix_complex_get(dDdkx, 3*j + alpha, 3*j_prime + beta), temp2));
						gsl_matrix_complex_set(dDdky, 3*j + alpha, 3*j_prime + beta, gsl_complex_add(gsl_matrix_complex_get(dDdky, 3*j + alpha, 3*j_prime + beta), temp3));
						gsl_matrix_complex_set(dDdkz, 3*j + alpha, 3*j_prime + beta, gsl_complex_add(gsl_matrix_complex_get(dDdkz, 3*j + alpha, 3*j_prime + beta), temp4));
					}
				}
			} // end of l' loop		
		} // end of j' loop
	} // end of j loop
	gsl_eigen_hermv_workspace * z = gsl_eigen_hermv_alloc (3*Natoms_cell);
  	gsl_vector * eval = gsl_vector_alloc (3*Natoms_cell);
  	gsl_eigen_hermv(D, eval, evec, z);
  	// chop the eigenvalues (don't want to take the square root of a small negative number)
 	for (i = 0; i<3*Natoms_cell; i++) { if (gsl_vector_get(eval, i) < precision*precision && gsl_vector_get(eval, i) > -precision*precision) gsl_vector_set(eval, i, 0.);}
 	// sort eigenvalues and eigenvectors
  	gsl_eigen_hermv_sort(eval, evec, GSL_EIGEN_SORT_VAL_ASC);
  	// take the square root of the eigenvalues to get the frequencies
  	for (i = 0; i<3*Natoms_cell; i++) gsl_vector_set(eval, i, sqrt(gsl_vector_get(eval,i)));
  	// calculate group velocities for each mode
  	gsl_vector_complex * vect = gsl_vector_complex_alloc(3*Natoms_cell);
   	gsl_vector_complex * resultx = gsl_vector_complex_alloc(3*Natoms_cell);
  	gsl_vector_complex * resulty = gsl_vector_complex_alloc(3*Natoms_cell);
 	gsl_vector_complex * resultz = gsl_vector_complex_alloc(3*Natoms_cell);
  	gsl_complex * answerx; gsl_complex * answery; gsl_complex * answerz;
  	answerx = new gsl_complex; answery = new gsl_complex; answerz = new gsl_complex;
 	for (alpha = 0; alpha<3*Natoms_cell; alpha++) {
	  	frequency[alpha] = gsl_vector_get(eval, alpha);
	  	gsl_matrix_complex_get_col(vect, evec, alpha);
		gsl_blas_zgemv(CblasNoTrans, gsl_complex_rect(1.0,0.0), dDdkx, vect, gsl_complex_rect(0.0,0.0), resultx);
		gsl_blas_zgemv(CblasNoTrans, gsl_complex_rect(1.0,0.0), dDdky, vect, gsl_complex_rect(0.0,0.0), resulty);
		gsl_blas_zgemv(CblasNoTrans, gsl_complex_rect(1.0,0.0), dDdkz, vect, gsl_complex_rect(0.0,0.0), resultz);
		gsl_blas_zdotc(vect, resultx, answerx);
		gsl_blas_zdotc(vect, resulty, answery);
		gsl_blas_zdotc(vect, resultz, answerz);
		velocity[alpha][0] = GSL_REAL(answerx[0])/(2.*frequency[alpha]);
		velocity[alpha][1] = GSL_REAL(answery[0])/(2.*frequency[alpha]);
		velocity[alpha][2] = GSL_REAL(answerz[0])/(2.*frequency[alpha]);
  	}
  	gsl_vector_complex_free(vect);
  	gsl_vector_complex_free(resultx);
  	gsl_vector_complex_free(resulty);
  	gsl_vector_complex_free(resultz);
	gsl_eigen_hermv_free(z);
	gsl_vector_free(eval);
	gsl_matrix_complex_free(D);
	gsl_matrix_complex_free(dDdkx);
	gsl_matrix_complex_free(dDdky);
	gsl_matrix_complex_free(dDdkz);
  	delete answerx; delete answery; delete answerz;
}
////////////////////////////////////////////////////////////////////////////////////////////
void kappa_z(double kappax, double kappay, double omega, double **** fc, double **x, double lc[3], int Natoms_cell, int N_cells, double **inv_sqrtm, gsl_complex allowed_kz[], double velocity[][3], gsl_matrix_complex * evec, int side, int err[1]){
	// FINDS ALL kappa[2] with given kappa[0], kappa[1], and omega
	bool testing = false;
	int j, j_prime, kk, alpha, beta, l, l_prime, ii, i, k;
	double rij[3], dot_product;
	
	// CALCULATE THE DYNAMICAL MATRIX
	gsl_complex * D0; D0 = new gsl_complex[3*Natoms_cell*3*Natoms_cell];
	gsl_complex * D1; D1 = new gsl_complex[3*Natoms_cell*3*Natoms_cell];
	gsl_complex * D2; D2 = new gsl_complex[3*Natoms_cell*3*Natoms_cell];
	for (ii = 0; ii < 3*3*Natoms_cell*Natoms_cell; ii++) D0[ii] = D1[ii] = D2[ii] = gsl_complex_rect(0.,0.);
	double kappa[2]; kappa[0] = kappax; kappa[1] = kappay;	
	gsl_complex temp, invmexpikR;;
	for (j = 0; j < Natoms_cell; j++) {
		for (j_prime = 0; j_prime < Natoms_cell; j_prime++) {
			for (l_prime = 0; l_prime < N_cells; l_prime++) {	
				for (kk=0; kk<3; kk++) rij[kk] = x[l_prime*Natoms_cell][kk] - x[0][kk];
				dot_product = kappa[0]*rij[0] + kappa[1]*rij[1];
				invmexpikR = gsl_complex_mul_real(gsl_complex_exp(gsl_complex_rect(0., dot_product)), inv_sqrtm[j][l_prime*Natoms_cell + j_prime]);				
				for (alpha = 0; alpha < 3; alpha++) {
					for (beta = 0; beta < 3; beta++) {
						temp = gsl_complex_mul_real(invmexpikR, fc[j][l_prime*Natoms_cell + j_prime][alpha][beta]);
						if (rij[2] < -precision*lc[2]) D0[(3*j + alpha)*3*Natoms_cell + 3*j_prime + beta] = gsl_complex_add(D0[(3*j + alpha)*3*Natoms_cell + 3*j_prime + beta], temp);
						if (rij[2] > -precision*lc[2] && rij[2] < precision*lc[2]) D1[(3*j + alpha)*3*Natoms_cell + 3*j_prime + beta] = gsl_complex_add(D1[(3*j + alpha)*3*Natoms_cell + 3*j_prime + beta], temp);
						if (rij[2] > precision*lc[2]) D2[(3*j + alpha)*3*Natoms_cell + 3*j_prime + beta] = gsl_complex_add(D2[(3*j + alpha)*3*Natoms_cell + 3*j_prime + beta], temp);
					}
				}
			} // end of l' loop		
		} // end of j' loop
	} // end of j loop
	
	// SET UP THE MATRICES FOR THE GENERALIZED EIGENSYSTEM
	gsl_matrix_complex * Atmp = gsl_matrix_complex_calloc(2*3*Natoms_cell, 2*3*Natoms_cell);
	gsl_matrix_complex * Btmp = gsl_matrix_complex_calloc(2*3*Natoms_cell, 2*3*Natoms_cell);
	gsl_matrix_complex * invBtmp = gsl_matrix_complex_calloc(2*3*Natoms_cell, 2*3*Natoms_cell);
	gsl_matrix_complex * invBtmp_Atmp = gsl_matrix_complex_calloc(2*3*Natoms_cell, 2*3*Natoms_cell);
	for (alpha = 0; alpha < 3*Natoms_cell; alpha++) {
		gsl_matrix_complex_set(Atmp, alpha + 3*Natoms_cell, alpha + 3*Natoms_cell, gsl_complex_rect(1., 0.)); 
		gsl_matrix_complex_set(Btmp, alpha + 3*Natoms_cell, alpha, gsl_complex_rect(1., 0.)); 
		for (beta = 0; beta < 3*Natoms_cell; beta++) {
			gsl_matrix_complex_set(Atmp, alpha, beta, D0[alpha*3*Natoms_cell + beta]);
			if (alpha == beta) gsl_matrix_complex_set(Btmp, alpha, beta, gsl_complex_sub(gsl_complex_rect(omega*omega, 0.), D1[alpha*3*Natoms_cell + beta]));
			else gsl_matrix_complex_set(Btmp, alpha, beta, gsl_complex_mul_real(D1[alpha*3*Natoms_cell + beta], -1.));
			gsl_matrix_complex_set(Btmp, alpha, beta + 3*Natoms_cell, gsl_complex_mul_real(D2[alpha*3*Natoms_cell + beta], -1.)); 
		}
	}
	delete [] D0; delete [] D1; delete [] D2;
	
	// MULTIPLY A BY THE INVERSE OF MATRIX Btmp FROM THE LEFT
	gsl_permutation * p = gsl_permutation_alloc (2*3*Natoms_cell); int signum;
	gsl_linalg_complex_LU_decomp(Btmp, p, &signum); gsl_linalg_complex_LU_invert(Btmp, p, invBtmp);
	gsl_blas_zgemm(CblasNoTrans, CblasNoTrans, gsl_complex_rect(1.,0.), invBtmp, Atmp, gsl_complex_rect(0.,0.), invBtmp_Atmp);
	gsl_matrix_complex_free(Atmp); gsl_matrix_complex_free(Btmp); gsl_matrix_complex_free(invBtmp); gsl_permutation_free(p);
	
	// FIND THE EIGENVALUES (they will be stored in the first 2*3*Natoms_cell entries of vector Q)
	// (first convert matrix invBtmp_Atmp from GSL format to a format suitable for matrixmath.cpp)
	vector<COMPLEX> INVBtmp_Atmp(sqr(2*3*Natoms_cell)); vector<COMPLEX> Q(sqr(2*3*Natoms_cell)); vector<COMPLEX> W(sqr(2*3*Natoms_cell));
	for (alpha = 0; alpha < 2*3*Natoms_cell; alpha++) { for (beta = 0; beta < 2*3*Natoms_cell; beta++) INVBtmp_Atmp[alpha*(2*3*Natoms_cell) + beta] = COMPLEX(GSL_REAL(gsl_matrix_complex_get(invBtmp_Atmp, alpha, beta)), GSL_IMAG(gsl_matrix_complex_get(invBtmp_Atmp, alpha, beta))); }
	eigen(INVBtmp_Atmp, Q, W, 2*3*Natoms_cell);
	
	// CALCULATE THE ALLOWED WAVEVECTORS FROM THE EIGENVALUES, TRASH THE COMPLEX WAVEVECTORS WITH THE WRONG 
	// IMAGINARY COMPONENT (when a wavevector is trashed, it is just set to 10000. + 10000.i), TRASH THE
	// ZERO AND INFINITE EIGENVALUES, AND COUNT THE NUMBER OF REAL EIGENVALUES
	gsl_vector_complex * wavevectors = gsl_vector_complex_calloc (2*3*Natoms_cell); gsl_vector_complex * Lamda = gsl_vector_complex_calloc (2*3*Natoms_cell);
	gsl_complex eig_lamda, wv; int num_real = 0; double eig_mag;		
	for (i = 0; i < 2*3*Natoms_cell; i++) {
		eig_lamda = gsl_complex_rect(real(Q[i]), imag(Q[i]));
		eig_mag = gsl_complex_abs(eig_lamda);
		wv = gsl_complex_div(gsl_complex_log(eig_lamda), gsl_complex_rect(0., 0.5*lc[2]));					
		if (fabs(eig_mag - 1.) < low_precision) { // real
			wv = gsl_complex_rect(GSL_REAL(wv), 0.); 
			num_real++;
		} 
		else {		// complex
			if (eig_mag > 100.) wv = gsl_complex_rect(10000., 10000.); 
			if (eig_mag < (1./100.)) wv = gsl_complex_rect(10000., 10000.); 
			if (side == 0 && GSL_IMAG(wv) > 0.) wv = gsl_complex_rect(10000., 10000.); 
			if (side == 1 && GSL_IMAG(wv) < 0.) wv = gsl_complex_rect(10000., 10000.);		
			if (fabs(GSL_IMAG(wv)) < precision) wv = gsl_complex_rect(10000., 10000.);	
			if (fabs(GSL_REAL(wv)) < precision) wv = gsl_complex_rect(0., GSL_IMAG(wv));
		}					
		gsl_vector_complex_set(wavevectors, i, wv);
		gsl_vector_complex_set(Lamda, i, eig_lamda);		
	} 
	
	// CALCULATE THE EIGENVECTORS FOR THE COMPLEX WAVEVECTORS THAT ARE STILL LEFT,
	// CONVERT THE EIGENVECTOR BACK TO THE NORMAL DEFINITION, AND NORMALIZE
	gsl_matrix_complex * eigen_temp = gsl_matrix_complex_alloc(3*Natoms_cell, 2*3*Natoms_cell); gsl_complex entry; 
	gsl_matrix_complex * invBtmp_Atmp_lamda = gsl_matrix_complex_calloc(2*3*Natoms_cell - 1, 2*3*Natoms_cell - 1);
	gsl_vector_complex * gen_evec = gsl_vector_complex_alloc(2*3*Natoms_cell - 1);
	gsl_vector_complex * b_vect = gsl_vector_complex_alloc(2*3*Natoms_cell - 1);
  	gsl_vector_complex * vect = gsl_vector_complex_alloc(3*Natoms_cell); 	
	gsl_complex complex_dot_product;
	gsl_permutation * perm = gsl_permutation_alloc (2*3*Natoms_cell  - 1);
	double mag;
	for (i = 0; i < 2*3*Natoms_cell; i++) {
		if (fabs(GSL_REAL(gsl_vector_complex_get(wavevectors, i)) - 10000.) > precision) {
			eig_lamda = gsl_vector_complex_get(Lamda, i);
			for (alpha = 0; alpha < 2*3*Natoms_cell - 1; alpha++) {
				for (beta = 0; beta < 2*3*Natoms_cell - 1; beta++) {
					if (alpha == beta) gsl_matrix_complex_set(invBtmp_Atmp_lamda, alpha, beta, gsl_complex_sub(gsl_matrix_complex_get(invBtmp_Atmp, alpha, beta), eig_lamda));
					else gsl_matrix_complex_set(invBtmp_Atmp_lamda, alpha, beta, gsl_matrix_complex_get(invBtmp_Atmp, alpha, beta));
				}
			}
			for (j = 0; j < 2*3*Natoms_cell - 1; j++) gsl_vector_complex_set(b_vect, j, gsl_complex_mul_real(gsl_matrix_complex_get(invBtmp_Atmp, j, 2*3*Natoms_cell - 1), -1.));
			gsl_linalg_complex_LU_decomp(invBtmp_Atmp_lamda, perm, &signum);
			gsl_linalg_complex_LU_solve(invBtmp_Atmp_lamda, perm, b_vect, gen_evec);
			for (j = 0; j < 3*Natoms_cell; j++) gsl_vector_complex_set(vect, j, gsl_vector_complex_get(gen_evec, j));
			for (j = 0; j < Natoms_cell; j++) {
				for (kk = 0; kk < 3; kk++) rij[kk] = x[j][kk] - x[0][kk]; 
				complex_dot_product = gsl_complex_rect(kappa[0]*rij[0] + kappa[1]*rij[1] + GSL_REAL(gsl_vector_complex_get(wavevectors, i))*rij[2], GSL_IMAG(gsl_vector_complex_get(wavevectors, i))*rij[2]);
				complex_dot_product = gsl_complex_mul(complex_dot_product, gsl_complex_rect(0.,-1.));
				complex_dot_product = gsl_complex_exp(complex_dot_product);
				for (kk = 0; kk < 3; kk++) gsl_vector_complex_set(vect, 3*j + kk, gsl_complex_mul(gsl_vector_complex_get(vect, 3*j + kk), complex_dot_product));
			} 
			mag = gsl_blas_dznrm2(vect);
			for (j = 0; j < 3*Natoms_cell; j++) gsl_vector_complex_set(vect, j, gsl_complex_div_real(gsl_vector_complex_get(vect, j), mag));
			gsl_matrix_complex_set_col(eigen_temp, i, vect);
		}
	}
	gsl_matrix_complex_free(invBtmp_Atmp_lamda); gsl_vector_complex_free(gen_evec); gsl_vector_complex_free(b_vect); gsl_permutation_free(perm);
	
	// MAKE A LIST OF JUST THE REAL WAVEVECTORS AND SORT IT; CALCULATE THE EIGENVECTORS AND VELOCITIES FOR
	// EACH REAL WAVEVECTOR; ELIMINATE HALF OF THE REAL WAVEVECTORS BASED ON THE SIGN OF THE VELOCITY
	double velocities[2*3*Natoms_cell][3]; for (i = 0; i < 2*3*Natoms_cell; i++) {for (kk = 0; kk < 3; kk++) velocities[i][kk] = 10000.;}
	double v_dummy[3*Natoms_cell][3]; double f_dummy[3*Natoms_cell]; gsl_matrix_complex * e_dummy = gsl_matrix_complex_alloc(3*Natoms_cell, 3*Natoms_cell);
	int count; kappa[0] = kappax; kappa[1] = kappay;
	bool found; int jump; int found_count;
	if (num_real > 0) {
		
	 	count = 0;
		gsl_vector * real_wv = gsl_vector_alloc (num_real);
	 	for (i = 0; i < 2*3*Natoms_cell; i++) {
		  	if (fabs(GSL_IMAG(gsl_vector_complex_get(wavevectors, i))) < precision) { gsl_vector_set(real_wv, count, GSL_REAL(gsl_vector_complex_get(wavevectors, i))); count++; }
		}
		gsl_sort_vector(real_wv);
		
		for (i = 0; i < num_real; i += jump) {
			kappa[2] = gsl_vector_get(real_wv, i);
		
			if (i != (num_real - 1)) { if (fabs(gsl_vector_get(real_wv, i+1) - kappa[2]) < precision) jump = 2; else jump = 1; }
			else jump = 1;
				
			freq_eig_vel(kappa, fc, x, lc, Natoms_cell, N_cells, inv_sqrtm, f_dummy, v_dummy, e_dummy);
			
			// find the indices of the modes w/ frequencies closest to omega
			int min_j1, min_j2;
			double min_dist1 = 10000.; 
			for (j = 0; j < 3*Natoms_cell; j++) {
				if (fabs(f_dummy[j] - omega) < min_dist1) { min_dist1 = fabs(f_dummy[j] - omega); min_j1 = j; }
			}
			if (jump == 2) {
				double min_dist2 = 10000.;
				for (j = 0; j < 3*Natoms_cell; j++) {
					if (fabs(f_dummy[j] - omega) < min_dist2 && j != min_j1) { min_dist2 = fabs(f_dummy[j] - omega); min_j2 = j; }
				}
			}
			kk = 0; 
			int found_count = 0;
			while (kk < 2*3*Natoms_cell && found_count < jump) {
				if (fabs(GSL_REAL(gsl_vector_complex_get(wavevectors, kk)) - kappa[2]) < precision) {
					if (found_count == 0) { 
						for (k = 0; k < 3; k++) velocities[kk][k] = v_dummy[min_j1][k]; 
						gsl_matrix_complex_get_col(vect, e_dummy, min_j1);
					}
					if (found_count == 1) { 
						for (k = 0; k < 3; k++) velocities[kk][k] = v_dummy[min_j2][k]; 
						gsl_matrix_complex_get_col(vect, e_dummy, min_j2);
					}
					gsl_matrix_complex_set_col(eigen_temp, kk, vect);
					found_count++;
				}
				kk++;
			}
		}
	  	gsl_vector_free(real_wv);
	}
  	gsl_matrix_complex_free(e_dummy);
  	
	// ELIMINATE HALF OF THE WAVEVECTORS BASED ON VELOCITY
 	for (i = 0; i < 2*3*Natoms_cell; i++) {
	  	wv = gsl_vector_complex_get(wavevectors, i);
	  	if (side == 0 && fabs(GSL_IMAG(wv)) < precision && velocities[i][2] > 0.) gsl_vector_complex_set(wavevectors, i, gsl_complex_rect(10000., 10000.));
	  	if (side == 1 && fabs(GSL_IMAG(wv)) < precision && velocities[i][2] < 0.) gsl_vector_complex_set(wavevectors, i, gsl_complex_rect(10000., 10000.));
	}
	
  	// COUNT THE NUMBER OF REAL WAVEVECTORS STILL LEFT (should be half of the number before)
  	count = 0; 
	for (i = 0; i < 2*3*Natoms_cell; i++) {
		wv = gsl_vector_complex_get(wavevectors, i);
		if (fabs(GSL_IMAG(wv)) < precision) count++;
	}
	if (count != num_real/2) { err[0] = 2; return; }
 	
  	// COUNT THE NUMBER OF ALLOWED WAVEVECTORS STILL LEFT (should be less than 3*Natoms_cell)
  	count = 0;
	for (i = 0; i < 2*3*Natoms_cell; i++) {
		wv = gsl_vector_complex_get(wavevectors, i);
		if (fabs(GSL_REAL(wv) - 10000.) > precision) count++;
	}
	if (count > 3*Natoms_cell) { err[0] = 3; return; }
  	
	// RETURN THE ALLOWED REFLECTED AND TRANSMITTED MODE INFORMATION BACK TO THE MAIN PART OF THE CODE
	count = 0; 
	for (i = 0; i < 2*3*Natoms_cell; i++) {
		wv = gsl_vector_complex_get(wavevectors, i);
		if (fabs(GSL_REAL(wv) - 10000.) > precision) {
			for (k = 0; k < 3; k++) velocity[count][k] = velocities[i][k];
			allowed_kz[count] = wv;
			gsl_matrix_complex_get_col(vect, eigen_temp, i); gsl_matrix_complex_set_col(evec, count, vect);
			count++;
		}
	} 
	for (i = count; i < 3*Natoms_cell; i++) { for (k = 0; k < 3; k++) velocity[i][k] = 10000.; allowed_kz[i] = gsl_complex_rect(10000., 10000.); } 
	
	gsl_vector_complex_free(Lamda);
	gsl_vector_complex_free(vect);
	gsl_matrix_complex_free(eigen_temp);
	gsl_vector_complex_free(wavevectors);
}
////////////////////////////////////////////////////////////////////////////////////////////
int check_kappa(double kappa[3], double lc[3]) {
	// Check to see if this random wavevector is in the FBZ using the procedure outlined in Omini & Sparavigna (PRB Vol 53 1996 9064)
 	int kk;
 	double kappa_test[3]; for (kk = 0; kk < 3; kk++) kappa_test[kk] = kappa[kk]/(4.*Pie/lc[kk]); 
	double eta, theta, M, eta1, eta2, H, min_lc;
	bool rand_k_found = false;
	eta = sqrt(kappa_test[0]*kappa_test[0] + kappa_test[1]*kappa_test[1]);
	if (kappa_test[0] >= 0. && kappa_test[1] >= 0.) theta = atan(kappa_test[1]/kappa_test[0]); // 1st quadrant
	if (kappa_test[0] < 0. && kappa_test[1] >= 0.) theta = 0.5*Pie + atan(-kappa_test[0]/kappa_test[1]); // 2nd
	if (kappa_test[0] < 0. && kappa_test[1] < 0.) theta = Pie + atan(kappa_test[1]/kappa_test[0]); // 3rd
	else theta = 1.5*Pie + atan(kappa_test[0]/(-kappa_test[1]));
	eta1 = eta*cos(theta);
	eta2 = eta*sin(theta);
	if (theta >= 0. && theta <= atan(0.5)) H = 0.5/cos(theta);
	if (theta > atan(0.5) && theta <= atan(2.)) H = 0.75/(cos(theta)+sin(theta));
	if (theta > atan(2.) && theta <= ((0.5*Pie) + atan(0.5))) H = 0.5/sin(theta);
	if (theta > ((Pie/2) + atan(0.5)) && theta <= (Pie - atan(0.5))) H = 0.75/(sin(theta)-cos(theta));
	if (theta > (Pie - atan(0.5)) && theta <= (Pie + atan(0.5))) H = -0.5/cos(theta);
	if (theta > (Pie + atan(0.5)) && theta <= ((1.5*Pie)-atan(0.5))) H = -0.75/(cos(theta)+sin(theta));
	if (theta > ((1.5*Pie)-atan(0.5)) && theta <= ((1.5*Pie) + atan(0.5))) H = -0.5/sin(theta);
	if (theta > ((1.5*Pie) + atan(0.5)) && theta <= ((2*Pie) - atan(0.5))) H = 0.75/(cos(theta)-sin(theta));
	if (theta > ((2*Pie) - atan(0.5)) && theta <= 2*Pie) H = 0.5/cos(theta);
	if (eta < H) {
		M = 0.5;
		if (fabs((0.75 + eta1 + eta2)) < M) M = fabs((0.75 + eta1 + eta2));
		if (fabs((0.75 + eta1 - eta2)) < M) M = fabs((0.75 + eta1 - eta2));
		if (fabs((0.75 - eta1 + eta2)) < M) M = fabs((0.75 - eta1 + eta2));
		if (fabs((0.75 - eta1 - eta2)) < M) M = fabs((0.75 - eta1 - eta2));
		if (fabs(kappa_test[2]) < M) rand_k_found = true;
	}
	if (rand_k_found == true) return 0;
	else return 1;
}
////////////////////////////////////////////////////////////////////////////////////////////
double distance2BZB(double kappa[3], double lc[3]) {
	// finds the (approximate) distance to the BZ boundary
 	int kk; double eta, theta, M, eta1, eta2, H, min_lc; bool inside = true; double a[3], kappa_test[3]; double scale, step; step = 1.E-2; scale = 1.;
	while (inside == true) {
		scale -= step;
		for (kk = 0; kk < 3; kk++) kappa_test[kk] = kappa[kk];
		for (kk = 0; kk < 3; kk++) a[kk] = lc[kk]/scale;		
		for (kk = 0; kk < 3; kk++) kappa_test[kk] /= (4.*Pie/a[kk]); 
		eta = sqrt(kappa_test[0]*kappa_test[0] + kappa_test[1]*kappa_test[1]);
		if (kappa_test[0] >= 0. && kappa_test[1] >= 0.) theta = atan(kappa_test[1]/kappa_test[0]); // 1st quadrant
		if (kappa_test[0] < 0. && kappa_test[1] >= 0.) theta = 0.5*Pie + atan(-kappa_test[0]/kappa_test[1]); // 2nd
		if (kappa_test[0] < 0. && kappa_test[1] < 0.) theta = Pie + atan(kappa_test[1]/kappa_test[0]); // 3rd
		else theta = 1.5*Pie + atan(kappa_test[0]/(-kappa_test[1]));
		eta1 = eta*cos(theta);
		eta2 = eta*sin(theta);
		if (theta >= 0. && theta <= atan(0.5)) H = 0.5/cos(theta);
		if (theta > atan(0.5) && theta <= atan(2.)) H = 0.75/(cos(theta)+sin(theta));
		if (theta > atan(2.) && theta <= ((0.5*Pie) + atan(0.5))) H = 0.5/sin(theta);
		if (theta > ((Pie/2) + atan(0.5)) && theta <= (Pie - atan(0.5))) H = 0.75/(sin(theta)-cos(theta));
		if (theta > (Pie - atan(0.5)) && theta <= (Pie + atan(0.5))) H = -0.5/cos(theta);
		if (theta > (Pie + atan(0.5)) && theta <= ((1.5*Pie)-atan(0.5))) H = -0.75/(cos(theta)+sin(theta));
		if (theta > ((1.5*Pie)-atan(0.5)) && theta <= ((1.5*Pie) + atan(0.5))) H = -0.5/sin(theta);
		if (theta > ((1.5*Pie) + atan(0.5)) && theta <= ((2*Pie) - atan(0.5))) H = 0.75/(cos(theta)-sin(theta));
		if (theta > ((2*Pie) - atan(0.5)) && theta <= 2*Pie) H = 0.5/cos(theta);
		if (eta < H) {
			M = 0.5;
			if (fabs((0.75 + eta1 + eta2)) < M) M = fabs((0.75 + eta1 + eta2));
			if (fabs((0.75 + eta1 - eta2)) < M) M = fabs((0.75 + eta1 - eta2));
			if (fabs((0.75 - eta1 + eta2)) < M) M = fabs((0.75 - eta1 + eta2));
			if (fabs((0.75 - eta1 - eta2)) < M) M = fabs((0.75 - eta1 - eta2));
			if (fabs(kappa_test[2]) > M) inside = false; 
		} else inside = false;		
	}
	return scale;
}
////////////////////////////////////////////////////////////////////////////////////////////
