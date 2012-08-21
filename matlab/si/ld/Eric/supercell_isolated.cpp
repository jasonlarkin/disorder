// Generates the supercell for the SBM code
// works for Stillinger-Weber materials

#include <iostream>
#include <fstream>
#include <math.h>
#include <iomanip>
#include <time.h>
using namespace std;

void chime(int N_sc, int N, int type[], double x[][3]);	// writes inital particle positions to Chime file
double partpe(int atom_i, int N, int neighbor_list[], int type[], double x[][3]); // calculates the potential energy of atom i
void find_neighbors(int N, int neighbor_list[], double x[][3]); // update the neighbor list
void generate_sc(int N_sc, int N, int var_sep, double a_par, double a_perp_L, double a_perp_R, double mL, double mR, int TL, int TR, int number, double d[], double x[][3], double m[], int type[], int uc_position[]);
void eval_grad(int N_sc, int N, int var_sep, double a_par, double a_perp_L, double a_perp_R, double mL, double mR, int TL, int TR, int number, double d[], double x[][3], double m[], int type[], int uc_position[], double grad_d[], int neighbor_list[]);

ifstream dimensions_in("d_isolated.txt"); 
ofstream SBM_out("junction.txt");
ofstream chime_out("chime_isolated_full.xyz");

double precision = 1.E-6;

// Stillinger-Weber parameters for Si and Ge
double b = 1.80;							// cutoff
double b2 = b*b;							// cutoff^2
double epsilon[2][2];
double sigma[2][2];
double inv_sigma[2][2]; // inverse of sigma (useful in later calculations)
double sigma2[2][2];	// sigma squared
double lamda[2];
double sqrt_epsilon[2][2];
double sqrt_lamda[2][2];
double sqrt_sqrt_lamda[2][2];
double epsilon_over_sigma[2][2];
double C[2][2][2];
#define kB 1.38065E-23						// (J/K)
double epsilon_Si, sigma_Si, mass_Si;
const int num_neighbors = 16;   // number of atoms in neighbor list
								// for diamond crystal, 4 atoms in 1st n.n. shell, 12 atoms in 2nd n.n. shell
								// 12 atoms in 3rd n.n. shell, and 6 atoms in 4th n.n. shell

int main() {
	
	// variable declarations
    int i, j, k, ii, kk, L, var_sep, temp, number; 
    double xL, xR;
    bool real_Ge;
    
    // read information from input file
    dimensions_in >> xL; dimensions_in.ignore(80, '\n');
    dimensions_in >> xR; dimensions_in.ignore(80, '\n');
    dimensions_in >> L; dimensions_in.ignore(80, '\n');
    dimensions_in >> var_sep; dimensions_in.ignore(80, '\n');
    dimensions_in >> number; dimensions_in.ignore(80, '\n');
    dimensions_in >> temp; dimensions_in.ignore(80, '\n');
    if (temp == 1) real_Ge = true; 
    else real_Ge = false;   
    
	// STILLINGER-WEBER PARAMETERS
	epsilon_Si = 2.17*1.60217733E-19; // J 
	sigma_Si = 2.0951E-10;  // m 
	mass_Si = 4.66371E-26;    // kg (mass of Si, average of isotopes according to natural abundance)
	epsilon[0][0] = 1.;	// (Si)
	sigma[0][0] = 1.;   // (Si)
	lamda[0] = 21.0; // ND (Si)
	if (real_Ge == true) {
		epsilon[1][1] = (1.93*1.60217733E-19)/epsilon_Si;  // (Ge) 
		epsilon[0][1] = (2.0427*1.60217733E-19)/epsilon_Si;  //(cross-terms) 
		epsilon[1][0] = epsilon[0][1];
		sigma[1][1] = 2.181E-10/sigma_Si;   // (Ge)
		sigma[0][1] = 0.5*(sigma[0][0] + sigma[1][1]);   // (cross-terms)
		sigma[1][0] = sigma[0][1];  
		lamda[1] = 31.0; // ND (Ge)
	}
	else {
		epsilon[1][1] = 1.;  // (Ge) 
		epsilon[0][1] = 1.;  //(cross-terms) 
		epsilon[1][0] = 1.;
		sigma[1][1] = 1.;   // (Ge)
		sigma[0][1] = 1.;   // (cross-terms)
		sigma[1][0] = 1.;  
		lamda[1] = 21.0; // ND (Ge)
	}
	for (i = 0; i < 2; i++) {
		for (j = 0; j < 2; j++) {
			inv_sigma[i][j] = 1./sigma[i][j];
			sigma2[i][j] = sigma[i][j]*sigma[i][j];
			epsilon_over_sigma[i][j] = epsilon[i][j]/sigma[i][j];
			sqrt_epsilon[i][j] = sqrt(epsilon[i][j]);
			sqrt_lamda[i][j] = sqrt(lamda[i]*lamda[j]);
			sqrt_sqrt_lamda[i][j] = pow(lamda[i]*lamda[j], 0.25);
			for (k = 0; k < 2; k++) C[j][i][k] = sqrt(epsilon[i][j]*epsilon[i][k])*pow(lamda[j]*lamda[i]*lamda[i]*lamda[k],0.25);
		}
	}
    
	// lattice constants at 0K determined from PE minimization 
	double a_bulk_L, a_bulk_R, a_par, a_perp_L, a_perp_R, a_perp_avg, mL, mR;
	int TL, TR;
	if (real_Ge == false) { a_bulk_L = a_bulk_R = a_par = a_perp_L = a_perp_R = a_perp_avg = 2.592215; TL = TR = 0; }
	else {
		a_bulk_L = 2.592215 + (2.698497 - 2.592215)*xL; 
		a_bulk_R = 2.592215 + (2.698497 - 2.592215)*xR; 
		a_par = 0.5*(a_bulk_L + a_bulk_R);
		a_perp_L = 2.5384473 + 0.19902100*xL;
		a_perp_R = 2.5384473 + 0.19902100*xR;
		a_perp_avg = 0.5*(a_perp_L + a_perp_R);
		if (fabs(xL) < precision) TL = 0; else TL = 1;
		if (fabs(xR) < precision) TR = 0; else TR = 1;
	}
	mL = 1. + (2.586382532 - 1.)*xL;
	mR = 1. + (2.586382532 - 1.)*xR;
	// variables with initial guess
	double d[var_sep], grad_d[var_sep]; 
	for (i = 0; i < var_sep; i++) {
		if (i < (var_sep - 1)/2) d[i] = 0.25*a_perp_L;
		if (i == (var_sep - 1)/2) d[i] = 0.25*a_perp_avg;
		if (i > (var_sep - 1)/2) d[i] = 0.25*a_perp_R;
	}
	int N_sc = 2*L;
	int N = (2*number + 1)*(2*number + 1)*N_sc;
	double x[N][3]; 
	double m[N];
	int type[N], uc_position[N];	
	
	// OPTIMIZE THE STRUCTURE ///
	// generate replicated supercell based on initial positions, determine nearest neighbors, and evaluate grad_d
	generate_sc(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position);
	int neighbor_list[N*num_neighbors];	
	find_neighbors(N, neighbor_list, x);
  	eval_grad(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position, grad_d, neighbor_list);
  	double PE1 = 0.; double PE2 = 0.;
	for (j = 0; j < N_sc; j++) PE1 += partpe(j, N, neighbor_list, type, x);
		
  	// find the magnitude of the gradient vector
  	double grad_mag = 0.; for (i = 0; i < var_sep; i++) grad_mag += grad_d[i]*grad_d[i]; grad_mag = sqrt(grad_mag);
 	
  	// relax structure using Steepest Descent
  	double d_stop = 1.E-8; // convergence criterion
  	double step = 1.E-2;
  	double d_change = step*grad_mag; 
  	
	while (d_change > d_stop) {
		cout << d_change*1E8 << " x E8" << endl;
		for (i = 0; i < var_sep; i++) d[i] -= step*grad_d[i]; 
		eval_grad(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position, grad_d, neighbor_list);
		grad_mag = 0.; for (i = 0; i < var_sep; i++) grad_mag += grad_d[i]*grad_d[i]; grad_mag = sqrt(grad_mag);
		d_change = step*grad_mag; 			
	}
	for (i = 0; i < var_sep; i++) cout << d[i] << "\t"; cout << endl;

	generate_sc(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position);
	
	// OUTPUT STRUCTURE FOR THE SBM CODE
	
	// lead information
	SBM_out << setiosflags(ios::fixed) << setprecision(10) << TL << "\t" << mL << "\t" << a_par << "\t" << a_par << "\t" << a_perp_L << endl;
	SBM_out << setiosflags(ios::fixed) << setprecision(10) << TR << "\t" << mR << "\t" << a_par << "\t" << a_par << "\t" << a_perp_R << endl;
	
	// supercell information
	// calculate and output junction boundaries
	double b_L, b_R; b_L = b_R = 0.;
	b_L = (((2*L - 1) - var_sep)/2 - 0.5)*0.25*a_perp_L;
	b_R = (((2*L - 1) - var_sep)/2)*0.25*a_perp_L; for (i = 0; i < var_sep; i++) b_R += d[i]; b_R += 0.5*0.25*a_perp_R;
	SBM_out << setiosflags(ios::fixed) << setprecision(10) << N_sc << "\t" << N << "\t" << b_L << "\t" << b_R << endl;
	
	// output replicated supercell
	for (i = 0; i < N; i++) SBM_out << setiosflags(ios::fixed) << setprecision(10) << type[i] << "\t" << m[i] << "\t" << uc_position[i] << "\t" << x[i][0] << "\t" << x[i][1] << "\t" << x[i][2] << endl;
	chime(N_sc, N, type, x);
}	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void eval_grad(int N_sc, int N, int var_sep, double a_par, double a_perp_L, double a_perp_R, double mL, double mR, int TL, int TR, int number, double d[], double x[][3], double m[], int type[], int uc_position[], double grad_d[], int neighbor_list[]){
	int i, j, k;
	double PHI2, PHI1;
	double h = 0.0001;						
	for (i = 0; i < var_sep; i++) {
		PHI2 = PHI1 = 0.;
		d[i] += h;
		generate_sc(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position);
		for (j = 0; j < N_sc; j++) PHI2 += partpe(j, N, neighbor_list, type, x);
		d[i] -= 2*h;
		generate_sc(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position);
		for (j = 0; j < N_sc; j++) PHI1 += partpe(j, N, neighbor_list, type, x);
		grad_d[i] = (PHI2 - PHI1)/(2.*h);
		d[i] += h;
		generate_sc(N_sc, N, var_sep, a_par, a_perp_L, a_perp_R, mL, mR, TL, TR, number, d, x, m, type, uc_position);
	}
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void generate_sc(int N_sc, int N, int var_sep, double a_par, double a_perp_L, double a_perp_R, double mL, double mR, int TL, int TR, int number, double d[], double x[][3], double m[], int type[], int uc_position[]) {
	
	int i, j, k,kk;
	bool condition;
	int atom;
	int type_sc[N_sc], uc_position_sc[N_sc];
	double x_sc[N_sc][3], m_sc[N_sc]; 
	double z_pos = 0.;
	int temp;
	// generate the supercell
	for (i = 0; i < N_sc; i++) {
		if (i % 4 == 0) { x_sc[i][0] = x_sc[i][1] = 0.; uc_position_sc[i] = 0; }
		if (i % 4 == 1) { x_sc[i][0] = x_sc[i][1] = 0.25*a_par; uc_position_sc[i] = 1; }
		if (i % 4 == 2) { x_sc[i][0] = 0.; x_sc[i][1] = 0.5*a_par; uc_position_sc[i] = 0; }
		if (i % 4 == 3) { x_sc[i][0] = 0.25*a_par; x_sc[i][1] = 0.75*a_par;	uc_position_sc[i] = 1; }
		x_sc[i][2] = z_pos;
		if (i < N_sc/2) { type_sc[i] = TL; m_sc[i] = mL; }
		else { type_sc[i] = TR; m_sc[i] = mR; }
		temp = i - (N_sc - 1 - var_sep)/2; // i - 4
		if (temp < 0) z_pos += 0.25*a_perp_L;
		if (temp >= var_sep) z_pos += 0.25*a_perp_R;
		if (temp >= 0 && temp < var_sep) z_pos += d[temp];
	}
	
	double a1[2], a2[2]; // translational vectors for supercell
	a1[0] = 0.5*a_par; a1[1] = 0.5*a_par;
	a2[0] = -0.5*a_par; a2[1] = 0.5*a_par;
	
	// replicate the supercell
	atom = 0;
	for (i = 0; i < N_sc; i++) {
		for (kk = 0; kk < 3; kk++) x[atom][kk] = x_sc[i][kk];
		m[atom] = m_sc[i]; type[atom] = type_sc[i]; uc_position[atom] = uc_position_sc[i]; atom++;
	}
	for (i = -number; i <= number; i++) {
		for (j = -number; j <= number; j++) {
			if (i == 0 && j == 0) condition = false;
			else condition = true;
			if (condition == true) {
				for (k = 0; k < N_sc; k++) {
					for (kk = 0; kk < 2; kk++) x[atom][kk] = x_sc[k][kk] + i*a1[kk] + j*a2[kk];
					x[atom][2] = x_sc[k][2];
					m[atom] = m_sc[k];
					type[atom] = type_sc[k];
					uc_position[atom] = uc_position_sc[k];
					atom++;
				}
			}
		}
	}
		
	

}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void chime(int N_sc, int N, int type[] , double x[][3]) {
	// create Chime file
	int i;
	double scale = 2.5; 
	char letter;
	chime_out << N << endl<< 0 << endl;
	for(i=0;i<N;i++) {
		
		if (type[i] == 0) letter = 'S';
		if (type[i] == 1) letter = 'C';
		if (i < N_sc) letter = 'O';
		chime_out<<letter<<"\t"<<scale*x[i][0]<<"\t"<<scale*x[i][1]<<"\t"<<scale*x[i][2]<<endl;
	}
}
/////////////////////////////////////////////////////////////////////////////////////
void find_neighbors(int N, int neighbor_list[], double x[][3]){ // update the neighbor list
// NOTE: The way this is implemented is VERY inefficient and should be cleaned up. 
// Its only used at the start of the code so being slow is ok for now.
	int i, j, k, kk;
	double rij[3], r2;
	double max;				
	int max_location;	
	for (i=0; i<N; i++)	{	// 
		// temporary values (for first time through)
		max = 1000000.;	// max distance (of atoms in neighbor list)
		max_location = i*num_neighbors;
		for (k = i*num_neighbors; k<((i+1)*num_neighbors); k++) neighbor_list[k] = 1000000;
		for (j = 0; j<N; j++) {
			if (j!=i) {
				// calculate rij
				for (kk=0; kk<3; kk++) rij[kk] = x[j][kk] - x[i][kk];
				r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
				if (r2 < max) { // if the distance is less than the maximum one stored, find replace it
					neighbor_list[max_location] = j;	
					// find the maximum distance of the atoms currently stored in the neighbor list
					max = 0.;
					for (k = i*num_neighbors; k<((i+1)*num_neighbors); k++) {
						if (neighbor_list[k] != 1000000) {
							for (kk=0; kk<3; kk++) rij[kk] = x[neighbor_list[k]][kk] - x[i][kk];
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
		} // end of j
	} // end of i
}
/////////////////////////////////////////////////////////////////////////////////////
double partpe(int atom_i, int N, int neighbor_list[], int type[], double x[][3]){
	// calculates the potential energy of atom i	
	int i, j, k, m, kk;
	double Ei;
	double A = 7.049556277;
	double B = 0.6022245584;
	double one_sixth = 1./6.;
	double one_third = 1./3.;
	double rij[3], rik[3], rjk[3], Rij, Rik, Rjk, yij, yik, yjk, cos_theta_one_third, inv_yijb;
	double inv_Rij, inv_Rik, inv_Rjk, inv_yij, r2, inv_yij4, exp_inv_yijb, exp_inv_yikb, exp_inv_yjkb;
	bool neighbor = false;

	i = atom_i;
	Ei = 0.;
	for (j = 0; j<N; j++) {
		if (j != i) {
			// rij
			for (m = 0; m<num_neighbors; m++) {
				if (neighbor_list[i*num_neighbors + m] == j) neighbor = true; // check to see if j is a neighbor to i
			}
			if (neighbor == true) {
				for (kk=0; kk<3; kk++) rij[kk] = x[i][kk] - x[j][kk];
				r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
				// check to see if the distance is within the cutoff
				if (r2 < (b2*sigma2[type[i]][type[j]])){
					Rij = sqrt(r2);
					yij = Rij*inv_sigma[type[i]][type[j]];
					inv_Rij = 1./Rij;
					inv_yij = inv_Rij*sigma[type[i]][type[j]];
					inv_yij4 = inv_yij*inv_yij;
					inv_yij4 = inv_yij4*inv_yij4;
					inv_yijb = 1./(yij - b);
					exp_inv_yijb = exp(1.2*inv_yijb);
					
					// TWO-BODY TERM
					Ei = Ei + 0.5*epsilon[type[i]][type[j]]*A*(B*inv_yij4 - 1)*exp(inv_yijb);
				}
				else yij = b+100;
			}
			else yij = b+100;

			for (k = 0; k<N; k++) {
				if (k != j && k != i) {
					// calculate rij, rik, and rjk
					// rik
					neighbor = false;
					for (m = 0; m<num_neighbors; m++) {
						if (neighbor_list[i*num_neighbors + m] == k) neighbor = true; // check to see if k is a neighbor to i
					}
					if (neighbor == true) {
						for (kk=0; kk<3; kk++) rik[kk] = x[i][kk] - x[k][kk];
						r2 = rik[0]*rik[0] + rik[1]*rik[1] + rik[2]*rik[2];
		
						// check to see if the distance is within the cutoff
						if (r2 < (b2*sigma2[type[i]][type[k]])){
							Rik = sqrt(r2);
							inv_Rik = 1./Rik;
							yik = Rik*inv_sigma[type[i]][type[k]];
							exp_inv_yikb = exp(1.2/(yik - b));
						}
						else yik = b+100;
					}
					else yik = b+100;
				
					
					// rjk
					neighbor = false;
					for (m = 0; m<num_neighbors; m++) {
						if (neighbor_list[j*num_neighbors + m] == k) neighbor = true; // check to see if k is a neighbor to j
					}
					if (neighbor == true) {
						for (kk=0; kk<3; kk++) rjk[kk] = x[j][kk] - x[k][kk];
						r2 = rjk[0]*rjk[0] + rjk[1]*rjk[1] + rjk[2]*rjk[2];
						// check to see if the distance is within the cutoff
						if (r2 < (b2*sigma2[type[j]][type[k]])){
							Rjk = sqrt(r2);
							inv_Rjk = 1./Rjk;
							yjk = Rjk*inv_sigma[type[j]][type[k]];
							exp_inv_yjkb = exp(1.2/(yjk - b));
						}
						else yjk = b+100;
					}
					else yjk = b+100; 
					
					/// THREE-BODY TERMS
					// h(rij, rik, theta_jik)				
					if (yij < b && yik < b) {
						cos_theta_one_third = one_third + (rij[0]*rik[0] + rij[1]*rik[1] + rij[2]*rik[2])*inv_Rij*inv_Rik;
						Ei = Ei + one_sixth*C[type[j]][type[i]][type[k]]*exp_inv_yijb*exp_inv_yikb*cos_theta_one_third*cos_theta_one_third;
					}
					// h(rji, rjk, theta_ijk)				
					if (yij < b && yjk < b) {
						cos_theta_one_third = one_third - (rij[0]*rjk[0] + rij[1]*rjk[1] + rij[2]*rjk[2])*inv_Rij*inv_Rjk;
						Ei = Ei + one_sixth*C[type[i]][type[j]][type[k]]*exp_inv_yijb*exp_inv_yjkb*cos_theta_one_third*cos_theta_one_third;	
					}
					// h(rki, rkj, theta_ikj)				
					if (yik < b && yjk < b) {
						cos_theta_one_third = one_third + (rik[0]*rjk[0] + rik[1]*rjk[1] + rik[2]*rjk[2])*inv_Rik*inv_Rjk;
						Ei = Ei + one_sixth*C[type[i]][type[k]][type[j]]*exp_inv_yikb*exp_inv_yjkb*cos_theta_one_third*cos_theta_one_third;	
					}
				}
			} // end of 3-body term
		}
	}
	return Ei;
} 

