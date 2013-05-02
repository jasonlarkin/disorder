//*************************************************************************************************************************
// silica.cpp
//
// written by Alan McGaughey
//
// last modified Summer 2007 (one program for all silica structures)
//
//*************************************************************************************************************************


#include <iostream>
#include <fstream>
#include <math.h>
#include <time.h>
using namespace std;


//*************************************************************************************************************************


// declare functions

double partsep(double deltaR[3]);	// finds the length of a vector with orthogonal components deltaR[i]
void totenergy (double t, double xn[][3], double pn[][3], double m[], double Lo2[3], double eta, double Q, double Tset, double s, double kramer[4][4][5], int potiden[], double charge[], double pressure);	// calculates the toal energy and writes it to a file
void xyzfile(char partiden[], double xn[][3], int tstep);	// writes particle positions to a .xyz file for visualization with Charm
double LJforce(double r, double A, double b, double c, double qi, double qj, double epsilon, double sigma); // computes the short range force
double LJenergy(double r, double A, double b, double c, double qi, double qj, double epsilon, double sigma);
double KE(double pn[][3], double m[]);	// computes the kinetic energy
void momenta(double pn[][3], double t);	// outputs the distribution of the momenta
void heatcurrent(double xn[][3], double pn[][3], double t, double Lo2[3], double m[], double kramer[4][4][5], int potiden[], double charge[]); // computes the heat current vector
double erfc(double x); // returns the complementary error function of argument x using a lookup table
void radial(double xn[][3], char partiden[], int tstep, int oxygen_mark[]);
void partenergy(double t, double xn[][3], double pn[][3], double m[], double Lo2[3], double kramer[4][4][5], int potiden[], double charge[]);
void velocity(double pn[][3], double m[]);
void positions(double xn[][3]);
void positions_time(double xn[][3], int tstep);
void dipole(double xn[][3], double pn[][3], double m[], double charge[], double t);

//*************************************************************************************************************************

// global variables

	const int N = 1152; // the number of particles in the system (for SOD, N = 288 (2), 972 (3), for LTA & FAU, N = 576,  RHO, N = 1152, for Q, N = 576)
	int NC = 1; // 2 for SOD, LTA, 1 for FAU, RHO, Q
        int nx = 5; // for quartz (N = 9 nx ny nz)
        int ny = 6;
        int nz = 5;
	double pi = 4.*atan(1.);	// pi
	double rootpi = sqrt(pi);
	double a, a6, a24;	//cutoff radius (should be a function of simulation cell size - maybe half)
	double alpha, alpha2; // a measure of the width of the Guassian charge fields
	double r6_cutoff, erfc_cutoff, short_force_constant, short_force_wolf; // constants used in force and energy calculations
	double L[3], l_c_2[3];
	double estar = 4.3886E-19;	// energy scale (J)
	double rstar = 1.39E-10; //length scale (m)
	double mstar =4.6495E-26; // mass scale (kg) //one silicon atom
	double kB = 1.3806E-23;	// Boltmann constant
	double lookup_erfc[301]; // lookup table, input from a text file in main program
	double self; // self interaction term
	double xn_zero[N][3];

//*************************************************************************************************************************

	
// output files

ofstream energy_out("energy.txt");
ofstream positions1_out("positions1.xyz");
ofstream positions2_out("positions2.xyz");
ofstream temp_out("temp.txt");
ofstream heatcurrent_out("heatcurrent.txt");
ofstream momenta_out("momenta.txt");
ofstream check_out("check.txt");
ofstream finaldata_out("finaldata.txt");
ofstream part_vel[N/2];
ofstream part_pos[N/2];
ofstream part_energy[N/2];
ofstream part_pos_t;
ofstream dipole_out("dipole.txt");
ofstream dipole_dot_out("dipoledot.txt");
ofstream heatcurrent_conv_out("heatcurrent_conv.txt");
ofstream heatcurrent_cond_out("heatcurrent_cond.txt");
ofstream radial_out("radial.txt");


//*************************************************************************************************************************

// input files

ifstream erfc_in("erfcdata.txt");
ifstream initialdata_S_in("initialdata_S3.txt");
ifstream initialdata_A_in("initialdata_A.txt");
ifstream initialdata_F_in("initialdata_F.txt");
ifstream initialdata_R_in("initialdata_R.txt");
ifstream initialdata_M_in("initialdata_M.txt");
ifstream initialdata_Q_in("initialdata_Q200.txt");
ifstream unitcell_in;

ifstream faujasite_in("fau_intial.txt");



//*************************************************************************************************************************


int main(){

	char zeolite = 'R'; // choose the type of zeolite to be modeled: S = sodalite, A = zeolite A (LTA), F = faujasite, R = RHO, Q = quartz (N and NC above must be correct!)
	bool initial_data = true; // specify if initial data is to be read in from a file
	bool amorphous = true;
					
	int i,j,k;		// loop counters
	double t, delt;		// time parameters
	int tstep, tstep_total, lennard; // more time parameters

	t= 0.;
	tstep = 0;
	tstep_total =3000000;
	lennard = 100;
	delt = 0.02;

	// read in erfc data

	double erfcy;
	for(i=0;i<301;i++) {
		if(erfc_in >> erfcy) lookup_erfc[i]=erfcy;
	}

	// define output files

//	char filename[255];

//	for (i=0;i<N/2;i++) {
//		sprintf (filename, "particle velocities\\vel%d.txt",i);
//		part_vel[i].open (filename);
//		sprintf (filename, "C:\\Documents and Settings\\alan\\My Documents\\research\\zeolite\\particle positions\\pos%d.txt",i);
//		part_pos[i].open (filename);
//		sprintf (filename, "C:\\Documents and Settings\\alan\\My Documents\\research\\zeolite\\particle energies\\energy%d.txt",i);
//		part_energy[i].open (filename);
//	}


	double m[N];		// particle masses
	double charge[N];	// particle charges
    char partiden[N];	// particle identification for CHIME
	int potiden[N];		// particle identification for potential
	int oxygen_mark[N];

	for (i=0;i<N;i++) oxygen_mark[i] = 0;  // value for silicon

	// parameters for Buckingham portion of the potential	
	
	double kramer[4][4][5]; // contains potential data

	// initilaze kramer

	for(i=0;i<4;i++) {
		for(j=0;j<4;j++) {
			for (k=0;k<5;k++) kramer[i][j][k]=0.;
		}
	}

	// conversion factors
	
	double J_to_eV = 1.60219E-19;
	double m_to_A = 1E-10;

	// O-O
	kramer[0][0][0] = 1388.773/(estar/J_to_eV);					// A
	kramer[0][0][1] = 2.76*rstar/m_to_A;						// b
	kramer[0][0][2] = 175./(pow(rstar/m_to_A,6)*estar/J_to_eV);	// C
	if(amorphous == true){
		kramer[0][0][3] = 0.0344*1000/6.022E23/estar;				// epsilon
		kramer[0][0][4] = 2.20/rstar*m_to_A;						// sigma
	}

	// Si-O
	kramer[0][1][0] = kramer[1][0][0] = 18003.7572/(estar/J_to_eV);
	kramer[0][1][1] = kramer[1][0][1] = 4.87318*rstar/m_to_A;
	kramer[0][1][2] = kramer[1][0][2] = 133.5381/(pow(rstar/m_to_A,6)*estar/J_to_eV);
	if(amorphous == true){
		kramer[0][1][3] = kramer[1][0][3] = 1.083*1000/6.022E23/estar;	// epsilon
		kramer[0][1][4] = kramer[1][0][4] = 1.31/rstar*m_to_A;	// sigma
	}

	//Si-Si
	if(amorphous == true){
		kramer[1][1][3] = 1219.45*1000/6.022E23/estar;	// epsilon
		kramer[1][1][4] = 0.42/rstar*m_to_A;			// sigma
	}


	// note that the Si-Si interaction is due only to Coulombic forces
		
	double r, dF;
	double deltaR[3];

	// charges (normalized)

	double Sicharge = 2.4*J_to_eV/(sqrt(1.11265E-10*rstar*estar));		// denominator contains 4 * pi *  epsilon_0
	double Ocharge = -1.2*J_to_eV/(sqrt(1.11265E-10*rstar*estar));		// assume that the given charges take into account epsilon

	// thermostat parameters

	double Tset = (kB/estar)*150.;	// set temperature - number outside brackets is in Kelvin
	double tauT = 0.5;	// thermostat time constant (should be larger than time step)
	double s = 1.0;		// dynamical varible used to check thermostat
	double s1;			// storage of s for energy calculations
	double eta = 0.;	// damping factor
	double Q = 3.*Tset*tauT*tauT/2.;		// used to check thermostat

	// barostat parameters

	bool pressure_on = false;
	double Pset = (rstar*rstar*rstar)/estar*(0.); // set pressure - number outside brackets is in Pascals
	double tauP =   10.0; // barostat time constant (should be larger than time step)
	double epsilon[3];
	double epsilon_bar[3];

        for(i=1;i<3;i++) {
          epsilon[i] = 0.;
          epsilon_bar[i] = 0.;
        }

	// Set the initial values of the coordinates and their momenta. (these may be over written later)
	// Each particle has the same momenta as the rest, with a pseudo-random direction.

	double xn[N][3], pn[N][3], pn1[N][3], F[N][3]; // position, momentum and force arrays
	double l_c[3], Lo2[3], volume;

	char data1;
	int data2;
	double data3, data4, data5, data6, data7, data8, data9, data10;

	
	// set the particle momenta (done later)
	// totally random, with a scaling factor, force total to zero after

	double scale = 5.2E-6;
	double mom[3];
	double rn;


	// read in all initial data from a file if so desired - zeolite structure must be specified, boolean set at beginning of main loop
	// read in equilibrium atomic positions as well

	if (initial_data == true) {

		if (zeolite == 'S') {
		
			for(i=0;i<N;i++) {
		
				if(initialdata_S_in >> data1 >> data2 >> data3 >> data4 >> data5 >> data6 >> data7 >> data8 >> data9 >> data10) {
					partiden[i] = data1;
					potiden[i] = data2;
					charge[i] = data3;
					m[i] = data4;
					xn[i][0] = data5;
					xn[i][1] = data6;
					xn[i][2] = data7;
					pn[i][0] = data8;
					pn[i][1] = data9;
					pn[i][2] = data10;
				}

			

				if (oxygen_mark[i] == 0) partiden[i] = 'S';
				if (oxygen_mark[i] == 2) partiden[i] = 'O';
				if (oxygen_mark[i] == 3) partiden[i] = 'O';
			}
		}

		if (zeolite == 'A') {
		
			for(i=0;i<N;i++) {
		
				if(initialdata_A_in >> data1 >> data2 >> data3 >> data4 >> data5 >> data6 >> data7 >> data8 >> data9 >> data10) {
					partiden[i] = data1;
					potiden[i] = data2;
					charge[i] = data3;
					m[i] = data4;
					xn[i][0] = data5;
					xn[i][1] = data6;
					xn[i][2] = data7;
					pn[i][0] = data8;
					pn[i][1] = data9;
					pn[i][2] = data10;
				}

				

				if (oxygen_mark[i] == 0) partiden[i] = 'S';
				if (oxygen_mark[i] == 1) partiden[i] = 'O';
				if (oxygen_mark[i] == 2) partiden[i] = 'O';
				if (oxygen_mark[i] == 3) partiden[i] = 'O';
			}
		}

		if (zeolite == 'F') {
		
			for(i=0;i<N;i++) {
		
				if(initialdata_F_in >> data1 >> data2 >> data3 >> data4 >> data5 >> data6 >> data7 >> data8 >> data9 >> data10) {
					partiden[i] = data1;
					potiden[i] = data2;
					charge[i] = data3;
					m[i] = data4;
					xn[i][0] = data5;
					xn[i][1] = data6;
					xn[i][2] = data7;
					pn[i][0] = data8;
					pn[i][1] = data9;
					pn[i][2] = data10;
				}

				
				if (oxygen_mark[i] == 0) partiden[i] = 'S';
				if (oxygen_mark[i] == 1) partiden[i] = 'C';
				if (oxygen_mark[i] == 2) partiden[i] = 'O';
				if (oxygen_mark[i] == 3) partiden[i] = 'N';
				if (oxygen_mark[i] == 4) partiden[i] = 'B';
			}
		}

		if (zeolite == 'R') {
		
			for(i=0;i<N;i++) {
		
				if(initialdata_R_in >> data1 >> data2 >> data3 >> data4 >> data5 >> data6 >> data7 >> data8 >> data9 >> data10) {
					partiden[i] = data1;
					potiden[i] = data2;
					charge[i] = data3;
					m[i] = data4;
					xn[i][0] = data5;
					xn[i][1] = data6;
					xn[i][2] = data7;
					pn[i][0] = data8;
					pn[i][1] = data9;
					pn[i][2] = data10;
				}
			}
		}
		
		if (zeolite == 'M') {
		
			for(i=0;i<N;i++) {
		
				if(initialdata_M_in >> data1 >> data2 >> data3 >> data4 >> data5 >> data6 >> data7 >> data8 >> data9 >> data10) {
					partiden[i] = data1;
					potiden[i] = data2;
					charge[i] = data3;
					m[i] = data4;
					xn[i][0] = data5;
					xn[i][1] = data6;
					xn[i][2] = data7;
					pn[i][0] = data8;
					pn[i][1] = data9;
					pn[i][2] = data10;
				}
			}
		}
                
                if (initial_data == true) {

		if (zeolite == 'Q') {
		
			for(i=0;i<N;i++) {
		
				if(initialdata_Q_in >> data1 >> data2 >> data3 >> data4 >> data5 >> data6 >> data7 >> data8 >> data9 >> data10) {
					partiden[i] = data1;
					potiden[i] = data2;
					charge[i] = data3;
					m[i] = data4;
					xn[i][0] = data5;
					xn[i][1] = data6;
					xn[i][2] = data7;
					pn[i][0] = data8;
					pn[i][1] = data9;
					pn[i][2] = data10;
				}
			}
		}
	}

	}

	for(i=0;i<N;i++) {
		for(j=0;j<3;j++) xn[i][j] = xn[i][j]/1.188;
	}
	

// more variables
	
	double pressure, p[3], force;
//	bool extra = false;

	pressure=0.;
    for(i=1;i<3;i++) p[i]=0.;

// write the initial particle positions and system energy to output files					
	
	xyzfile(partiden, xn, tstep);
	energy_out<<"time"<<'\t'<<"short"<<'\t'<<"self"<<'\t'<<"kinetic"<<'\t'<<"total"<<'\t'<<"temperature"<<'\t'<<"pressure"<<endl; // header for energy file

//	heatcurrent_out<<"time"<<'\t'<<"qx"<<'\t'<<"qy"<<'\t'<<"qz"<<endl; // header for heatcurrent file
//	totenergy(tstep, xn, pn, m, Lo2[3], eta, Q, Tset, s, kramer, potiden, charge, pressure);
//	radial(xn, partiden);

//	for(i=0;i<N;i++) {
//		for (j=0;j<3;j++) xn_zero[i][j] = 0.;
//	}

	// quench parameters	

	bool quench = true;
	double Tset1, Tset2, time1, ramp;
	Tset1 = (kB/estar)*10000.;  //start of ramp
	Tset2 = (kB/estar)*200.;  // end of ramp
	time1 = 10000; //wait time
	ramp = 2980000; //ramp time

	double Tact; // actual temperature
	bool TempSet = true; // boolean used to switch from NVT to NVE
	int block = 3000000; //length of block in time steps
	int block_NVT = 3000000; // length of intial NVT block
	int block_count = -1; // current block number
	double shortrange, shortrange_set; //actual short range energy and desired value

//	double aphi[9][3];

//	sprintf (filename, "unit cell data/uc%c.txt", zeolite);
//	unitcell_in.open (filename);

  //      if(zeolite == 'Q') { // quartz
    //         for(i=0;i<9;i++) {
	  ///      	if(unitcell_in >> data5 >> data6 >> data7) {
		 //       	aphi[i][0] = data5; // potential energy
		//	        aphi[i][1] = data6; // unit cell - a
		//	        aphi[i][2] = data7; // unit cell - c
//		        }
  //           }
//	}
//       else { // the zeolites
//	       for(i=0;i<9;i++) {
//		         if(unitcell_in >> data5 >> data6) {
//			        aphi[i][0] = data5; // potential energy
//		        	aphi[i][1] = data6; // unit cell
//                              check_out<<aphi[i][0]<<'\t'<<aphi[i][1]<<endl;
//	        	}
//	     }
  //    }

	int Treal = 10000;

	Tset = (kB/estar)*Treal;
//    cout<<Tset<<endl;


// Integate the equations of motion using a Verlet leapfrog algorithm.
	
	while (tstep < tstep_total) {
		
		if (tstep%block==0) {
			block_count = block_count + 1;
			Treal = Treal + 10;
			Tset = Tset + 10.*(kB/estar);

		//	shortrange_set = aphi[(int)(Treal-200)/50][0];
        //         if(zeolite=='Q') {
        //                 L[0]= aphi[(int)(Treal-200)/50][1];
		//	       	 L[1] = (double)(ny)/(double)(nx)*L[0]*sqrt(3.)/2.;
		//	       	 L[2]= aphi[(int)(Treal-200)/50][2];
        //                 cout<<L[0]<<'\t'<<L[1]<<'\t'<<L[2]<<endl;
         //          }
         //          else {
	      //               L[0]= aphi[(int)(Treal-200)/50][1];
           //              L[1]=L[0];
           //              L[2]=L[0];
           //              cout<<L[0]<<endl;
           //        }
           
           L[0] = 18.2335;
           L[1] = L[0];
           L[2] = L[0];
		
			for(i=0;i<3;i++) {
                          Lo2[i] = L[i]/2.;
                          l_c[i] = L[i]/NC;
                          l_c_2[i] = l_c[i]/2.;
                        }
			alpha = 0.48;
			alpha2 = alpha*alpha;
			short_force_constant = 2.*alpha/rootpi;
			a = 12./(rstar/m_to_A);
			a6 = a*a*a*a*a*a;
			a24 = a6*a6*a6*a6;
			r6_cutoff = 1./pow(a,6);
			erfc_cutoff = 1./a*erfc(alpha*a);
			short_force_wolf = erfc(alpha*a)/(a*a) + short_force_constant*exp(-alpha2*a*a)/a;
			volume = L[0]*L[1]*L[2];	
			self = 0;
			for (i=0;i<N;i++) self = self - charge[i]*charge[i];
			self = self*(erfc(alpha*a)/(2.*a)*1. + alpha/rootpi);
                        totenergy(tstep, xn, pn, m, Lo2, eta, Q, Tset, s, kramer, potiden, charge, pressure);
		}

	//	if (tstep%block==0) { // close and open the position and velocity output files

	//		if(tstep !=0) part_pos_t.close();

	//		sprintf (filename, "pos_%c%d.txt", zeolite, Treal);
	//		part_pos_t.open (filename);

	//	}


		if (tstep%block == 0) { // randomize velocities
			
			srand(time(NULL));
			rand();

			for (i=0;i<N;i++) {
				for (j=0;j<3;j++) {
					rn = 0.0000000001*rand();
                                  
					if(rn<=RAND_MAX/2) pn[i][j] = scale*rn;
					else pn[i][j] = -1.*scale*rn;
				}
			}

			for (i=0;i<3;i++) mom[i] =0.;

			for(i=0;i<N;i++) {
				for (j=0;j<3;j++) mom[j] = mom[j] + pn[i][j];
			}
			
			
			for (i=0;i<3;i++) mom[i] =mom[i]/N;

			for(i=0;i<N;i++) {
				for (j=0;j<3;j++) pn[i][j] = pn[i][j] - mom[j];
			}
		}


		// set the temperature close to the desired value

		if (tstep < block*block_count + block_NVT && tstep >= block*block_count) eta = eta + delt/(tauT*tauT)*(KE(pn,m)/(1.5*(N-1)*Tset)-1.); //NVT
//		if(tstep > 10000) eta =0.08;

		if (tstep == block*block_count + block_NVT) TempSet = false; //start temperature setting procedure, set to true to deactivate (also uncomment else line above)

		if (tstep < block*(block_count+1) && tstep > block*block_count + block_NVT && TempSet == false){

			shortrange = 0.;
			
			for (i=0;i<N;i++) {
				for(j=i+1;j<N;j++){ 
					for(k=0;k<3;k++) {
						deltaR[k] = xn[i][k]-xn[j][k];
						if (deltaR[k] > Lo2[k]) deltaR[k] = deltaR[k]-L[k]; // periodic boundary conditions
						if (deltaR[k] < (-1.*Lo2[k])) deltaR[k] = deltaR[k] + L[k]; 
					}

					if ((deltaR[0] < a) && (deltaR[1] < a) && (deltaR[2] < a)) r=partsep(deltaR);
					else r = 10.*a;
			
				if (r<a) shortrange = shortrange  + LJenergy(r,kramer[potiden[i]][potiden[j]][0],kramer[potiden[i]][potiden[j]][1],kramer[potiden[i]][potiden[j]][2],charge[i], charge[j],kramer[potiden[i]][potiden[j]][3],kramer[potiden[i]][potiden[j]][4]);
				}
			}

			if(fabs(shortrange - shortrange_set) <= 0.001) {

				Tact = 2*KE(pn,m)/(3.*(N-1));

				for(i=0;i<N;i++){
					for(j=0;j<3;j++) pn[i][j] = pn[i][j]*sqrt(Tset/Tact); //scale momenta
				}
				TempSet = true;
				eta = 0.;  //switch to NVE
			}
			else eta = eta + delt/(tauT*tauT)*(KE(pn,m)/(1.5*(N-1)*Tset)-1.); //stay in NVT if tolerance not met

			momenta_out<<tstep<<'\t'<<eta<<'\t'<<shortrange<<'\t'<<shortrange_set<<'\t'<<TempSet<<endl;
		}

		//

		t = t + delt;
		tstep = tstep +1;

		if (quench == true) {
			if (tstep < time1) Tset = Tset1;
			if (tstep < time1+ramp && tstep >= time1) Tset = Tset + (Tset2-Tset1)/ramp;
			if (tstep >= time1+ramp) Tset = Tset2;
		}


		if (pressure_on == false) {
			for(i=1;i<3;i++) {
                           epsilon[i] = 0.;
			   epsilon_bar[i] = 0.;
                        }
		}

		// find the new positions

		for(i=0;i<N;i++) {
			for(j=0;j<3;j++) {
				xn[i][j] = xn[i][j]*(1.+epsilon[j]*delt/2.)/(1.-epsilon[j]*delt/2.) + (pn[i][j]*delt/m[i])/(1.-epsilon[j]*delt/2.);
				// periodic boundary conditions - check if particle has left box
				// note that (0,0,0) is not at the lower left hand corner of the simulation cell, but at the centre of the first unit cell
				// this accounts for the appearance of the l_c_2 terms
				if (xn[i][j] < 0.) xn[i][j] = xn[i][j] + L[j]; 
				if (xn[i][j] > L[j]) xn[i][j] = xn[i][j] - L[j];
			}
		}

//		for(i=0;i<N;i++) {
//			for (j=0;j<3;j++) {
//				if(xn[i][j] < -3.) xn_zero[i][j] = xn_zero[i][j] + xn[i][j] + L[j];
//				else xn_zero[i][j] = xn_zero[i][j]+ xn[i][j];
//			}
//		}

		if (pressure_on == true) {	// re-calculate constants if barostat in use                     
                
			for(i=0;i<3;i++) {
				L[i] = L[i]*(1.+epsilon[i]*delt/2.)/(1.-epsilon[i]*delt/2.);
				Lo2[i] = L[i]/2.;
				l_c[i] = L[i]/NC;
				l_c_2[i] = l_c[i]/2.;
			}

			alpha2 = alpha*alpha;
			short_force_constant = 2.*alpha/rootpi;
//			a = Lo2[1];
			a = 12./(rstar/m_to_A);
			a6 = a*a*a*a*a*a;
			a24 = a6*a6*a6*a6;
			r6_cutoff = 1./pow(a,6);
			erfc_cutoff = 1./a*erfc(alpha*a);
			short_force_wolf = erfc(alpha*a)/(a*a) + short_force_constant*exp(-alpha2*a*a)/a;
			volume = L[0]*L[1]*L[2];	
			self = 0;
			for (i=0;i<N;i++) self = self - charge[i]*charge[i];
			self = self*(erfc(alpha*a)/(2.*a)*1. + alpha/rootpi);
			if (tstep%10 == 0) momenta_out<<L[0]<<'\t'<<L[1]<<'\t'<<L[2]<<'\t'<<endl;

//
		}
		
		
		// set the forces to zero

		for(i=0;i<N;i++) {
			for (j=0;j<3;j++) F[i][j]=0.;
		}

		pressure=0.; // reset pressure measurement to zero

		// calculate the forces

		for(i=0;i<N;i++) {
			for(j=i+1;j<N;j++) {
				
				for(k=0;k<3;k++) {
					deltaR[k] = xn[i][k]-xn[j][k];
					if (deltaR[k] > Lo2[k]) deltaR[k] = deltaR[k]-L[k]; // periodic boundary conditions
					if (deltaR[k] < (-1.*Lo2[k])) deltaR[k] = deltaR[k] + L[k]; 
				}

				// only calculate r if all three dimensions are within the cutoff (when a = Lo2, this step is not necessary as the periodic boundary conditions will ensure that deltaR[i] <= a

				if ((deltaR[0] < a) && (deltaR[1] < a) && (deltaR[2] < a)) r=partsep(deltaR);

				// otherwise, give 'r' a value greater than the cutoff

				else r = 10.*a;

				// short range forces

				if(r<a){
					force=LJforce(r,kramer[potiden[i]][potiden[j]][0],kramer[potiden[i]][potiden[j]][1],kramer[potiden[i]][potiden[j]][2], charge[i], charge[j],kramer[potiden[i]][potiden[j]][3],kramer[potiden[i]][potiden[j]][4]);				
					for(k=0;k<3;k++){
						dF=deltaR[k]*force;
                                                p[k] = p[k] + dF*deltaR[k];
						F[i][k] = F[i][k] + dF;
						F[j][k] = F[j][k] - dF;
					}
					pressure = pressure + r*r*force;
				}
			}
		}

		pressure = pressure/(3.*volume)+ N*2.*KE(pn, m)/(3.*(N-1))/volume;
        p[0] = p[0]/(3.*volume) + N*2.*KE(pn, m)/(3.*(N-1))/volume/3.;
		p[1] = p[1]/(3.*volume) + N*2.*KE(pn, m)/(3.*(N-1))/volume/3.;
		p[2] = p[2]/(3.*volume) + N*2.*KE(pn, m)/(3.*(N-1))/volume/3.;

        if (pressure_on == true) {  // calculate epsilon
			for(i=0;i<3;i++) {
				epsilon_bar[i] = epsilon[i];
				epsilon[i] = epsilon[i] + delt/(tauP*tauP)*volume/(N*Tset)*(p[i]-Pset);
				epsilon_bar[i] = (epsilon[i]+epsilon_bar[i])/2.;
			}
		}


		// Step the momenta 

		// for energy and heat current calculations

		if (tstep%lennard == 0) {

			s1 = s*exp(eta*delt);
			s=(s+s1)/2.;

			for(i=0;i<N;i++) {
				for(j=0;j<3;j++) {
					pn1[i][j] = pn[i][j]*(1.-(eta+epsilon_bar[j])*delt/2.)/(1.+(eta+epsilon_bar[j])*delt/2.) + F[i][j]*delt/(1.+(eta+epsilon_bar[j])*delt/2.);
					pn[i][j] = (pn[i][j]+pn1[i][j])/2.;
				}
			}

			
			if(tstep%100 == 0) totenergy(tstep, xn, pn, m, Lo2, eta, Q, Tset, s, kramer, potiden, charge, pressure);
//			if(tstep%5000 == 0) radial(xn, partiden, tstep, oxygen_mark);
			if(tstep%100000 == 0) xyzfile(partiden, xn, tstep);
//			if (tstep <= block*block_count + block_NVT + 500000 && tstep > block*block_count + block_NVT + 200000) dipole(xn, pn, m, charge, tstep);
//			if (tstep <= block*(block_count+1) && tstep > block*block_count + block_NVT + 200000) heatcurrent(xn, pn, tstep, Lo2, m, kramer, potiden, charge);
//			momenta(pn, tstep);
//			partenergy(tstep, xn, pn, m, Lo2, kramer, potiden, charge);
//			if (tstep > 200000) velocity(pn, m);
//			if (tstep > 500000) positions(xn);
//			if (tstep > 200000) partenergy(tstep, xn, pn, m, Lo2, kramer, potiden, charge);
//			if (tstep > block*block_count + block_NVT + 100000  && tstep <= block*(block_count+1)) positions_time(xn, tstep-(block*block_count + block_NVT + 100000)-1);

			// update momenta for next loop
			
			for(i=0;i<N;i++) {
				for (j=0;j<3;j++) pn[i][j]=pn1[i][j];
			}
			s=s1;

			cout<<tstep<<endl;

		}

		// otherwise (normal loop - don't need to average momenta and s - no calculations are being made)

		else {
			s = s*exp(eta*delt);

			for(i=0;i<N;i++) {
				for(j=0;j<3;j++) pn[i][j] = pn[i][j]*(1.-(eta+epsilon_bar[j])*delt/2.)/(1.+(eta+epsilon_bar[j])*delt/2.) + F[i][j]*delt/(1.+(eta+epsilon_bar[j])*delt/2.);
			}
		}

	

	}

		// when the simulation is done, write final data to an output file

//		for(i=0;i<N;i++) {
//			for (j=0;j<3;j++){
//				xn_zero[i][j] = xn_zero[i][j]/tstep_total;
//				check_out<<xn_zero[i][j]<<'\t';
//			}
//			check_out<<endl;
//		}
	
//		finaldata_out << "partiden"<<'\t'<<"potiden"<<'\t'<<"charge"<<'\t'<<"mass"<<'\t'<<"x"<<'\t'<<"y"<<'\t'<<"z"<<'\t'<<"px"<<'\t'<<"py"<<'\t'<<"pz"<<endl;
		for(i=0;i<N;i++) finaldata_out <<partiden[i]<<'\t'<<potiden[i]<<'\t'<<charge[i]<<'\t'<<m[i]<<'\t'<<xn[i][0]<<"\t"<<xn[i][1]<<"\t"<<xn[i][2]<<'\t'<<pn[i][0]<<"\t"<<pn[i][1]<<"\t"<<pn[i][2]<< endl;
}


//*******************************************************************************************************************
//
// SUBROUTINES
//
//*******************************************************************************************************************

double partsep(double deltaR[3]) {	// finds the length of a vector with components deltaR[i] 
	
	int k;
	double rij;

	rij =0.;

	for(k=0;k<3;k++) rij = rij + deltaR[k]*deltaR[k];

	rij = sqrt(rij);

	return rij;

}

//********************************************************************************************************************

void totenergy (double t,double xn[][3], double pn[][3], double m[], double Lo2[3], double eta, double Q, double Tset, double s, double kramer[4][4][5], int potiden[], double charge[], double pressure) {
	// calculates the potential, kinetic and total energies of the system and writes the result to a file with a time stamp

	double kinenergy, energy, r, deltaR[3], Nose, Tmeas, shortrange;
	int i,j,k;
	double kinx, kiny, kinz, Tx, Ty, Tz;

	energy = kinenergy = shortrange = kinx = kiny = kinz =  0.;

	for (i=0;i<N;i++) {
		for(j=i+1;j<N;j++){ 
			for(k=0;k<3;k++) {
					deltaR[k] = xn[i][k]-xn[j][k];
					if (deltaR[k] > Lo2[k]) deltaR[k] = deltaR[k]-L[k]; // periodic boundary conditions
					if (deltaR[k] < (-1.*Lo2[k])) deltaR[k] = deltaR[k] + L[k]; 
				}

			// only calculate r if all three dimensions are within the cutoff

			if ((deltaR[0] < a) && (deltaR[1] < a) && (deltaR[2] < a)) r=partsep(deltaR);

			// otherwise, give 'r' a value that will always be greater than the cutoff

			else r = 10.*a;
			
			if (r<a) {
				shortrange = shortrange  + LJenergy(r,kramer[potiden[i]][potiden[j]][0],kramer[potiden[i]][potiden[j]][1],kramer[potiden[i]][potiden[j]][2],charge[i], charge[j],kramer[potiden[i]][potiden[j]][3],kramer[potiden[i]][potiden[j]][4]);
				}
		}
	}

	kinenergy = KE(pn, m);

	for (i=0;i<N;i++) {
		kinx = kinx + 1./(2.*m[i])*pn[i][0]*pn[i][0];
		kiny = kiny + 1./(2.*m[i])*pn[i][1]*pn[i][1];
		kinz = kinz + 1./(2.*m[i])*pn[i][2]*pn[i][2];
	}

	energy = shortrange + self + kinenergy;
//	pressure_elec = (shortrange_elec + self)/(3.*volume);
	
	Tmeas = 2.*kinenergy/(3.*(N-1))*estar/kB;
	Tx = 2.*kinx/(N-1)*estar/kB;
	Ty = 2.*kiny/(N-1)*estar/kB;
	Tz = 2.*kinz/(N-1)*estar/kB;

//	check_out<<pressure<<endl;

	pressure = pressure*estar/(rstar*rstar*rstar);

	Nose = shortrange + kinenergy + eta*eta*(N-1)*Q + 3.*(N-1)*Tset*s;
	
	energy_out<<t<<'\t'<<shortrange<<'\t'<<self<<'\t'<<kinenergy<<'\t'<<energy<<'\t'<<Tmeas<<'\t'<<pressure<<endl;

	temp_out<<t<<'\t'<<Tset*estar/kB<<'\t'<<Tmeas<<'\t'<<Tx<<'\t'<<Ty<<'\t'<<Tz<<'\t'<<eta<<'\t'<<s<<'\t'<<Nose<<endl;
}

//*************************************************************************************************************************

void xyzfile (char partiden[], double xn[][3], int tstep) {

// writes the particle positions to a .xyz file for visualization with Charm

		int i;
		double scale = 1.5; // 1.5 generates a nice image with the appropriate bonds

		positions1_out << N << endl << tstep << endl;
//		positions2_out << 2*N/3 << endl << endl;

		for(i=0;i<N;i++) {
			positions1_out << partiden[i] << "\t" << scale*xn[i][0] << "\t" <<scale*xn[i][1] << "\t" << scale*xn[i][2] << endl;
//			else positions2_out << partiden[i] << "\t" << 1.*xn[i][0] << "\t" <<1.*xn[i][1] << "\t" << 1.*xn[i][2] << endl;
		}
//		count = count+1;
}

//*************************************************************************************************************************

double LJforce(double r, double A, double b, double c, double qi, double qj, double epsilon, double sigma) {

	// calculates the total short range force between two particles separated by a distance r

	double dF;
	double r2=r*r;
	double sigma_over_r = sigma/r;
	double s_r_6 = sigma_over_r*sigma_over_r*sigma_over_r*sigma_over_r*sigma_over_r*sigma_over_r;
	
	dF = (qi*qj*(erfc(alpha*r)/r2+short_force_constant*exp(-alpha2*r2)/r - short_force_wolf) + A*b*exp(-b*r) - 6.*c/(r2*r2*r2*r) + 1.*24.*epsilon/sigma*(4.*s_r_6*s_r_6*s_r_6*s_r_6*sigma_over_r - s_r_6*sigma_over_r) )*(1./r);

	return dF;
}

//*************************************************************************************************************************

double LJenergy(double r, double A, double b, double c, double qi, double qj, double epsilon, double sigma) {

	// calculates the short range energy between two particles separated by a distance r
	// includes effect of the cutoff radius rc, does not include a correction for the tail

	double dE;
	double r2=r*r;
	double sigma3 = sigma*sigma*sigma;
	double sigma6 = sigma3*sigma3;
	double sigma_over_r = sigma/r;
	double s_r_6 = sigma_over_r*sigma_over_r*sigma_over_r*sigma_over_r*sigma_over_r*sigma_over_r;

	dE = qi*qj*(1./r*erfc(alpha*r) - erfc_cutoff) + A*(exp(-b*r)-exp(-b*a)) - c*(1./(r2*r2*r2) - r6_cutoff) + 1.*4.*epsilon*(s_r_6*s_r_6*s_r_6*s_r_6 - s_r_6 - sigma6*sigma6*sigma6*sigma6/a24 + sigma6/a6);

	return dE;

}


//*************************************************************************************************************************

double KE(double pn[][3], double m[]) {

	// calculates the kinetic energy of the system
	// N is global

	double kin=0.;
	int i;

	for (i=0;i<N;i++) kin = kin + 1./(2.*m[i])*(pn[i][0]*pn[i][0] + pn[i][1]*pn[i][1] + pn[i][2]*pn[i][2]);

	return kin;
}

//*************************************************************************************************************************

void heatcurrent(double xn[][3], double pn[][3], double t, double Lo2[3], double m[], double kramer[4][4][5], int potiden[], double charge[]) {

	// calculates the components of the heat current vector for thermal conductivity calculations
	// some global variables

	int i,j,k;
	double qconv[3], qcond[3], deltaR[3], r;
	double PHI[N], vn[N][3], vsum[3];
	double dE;
	double Fdot_plus;
//	double vE[3];
//	double momSi[3], momO[3];

//	for (i=0;i<3;i++) {
//		momSi[i] =0.;
//		momO[i] = 0.;
//	}

//	for(i=0;i<N;i++) {
//		if (potiden[i] == 1) {
//			for (j=0;j<3;j++) momSi[j] = momSi[j] + pn[i][j];
//		}
//		else {
//			for (j=0;j<3;j++) momO[j] = momO[j] + pn[i][j];
//		}
//
//	}
//
//	for (i=0;i<3;i++) {
//		momSi[i] =momSi[i]/(N/3.);
//		momO[i] = momO[i]/(2.*N/3.);
//	}

//	for(i=0;i<N;i++) {
//		if (potiden[i] == 1) {
//			for (j=0;j<3;j++) pn[i][j] = pn[i][j] - momSi[j];
//		}
//		else {
//			for (j=0;j<3;j++) pn[i][j] = pn[i][j] - momO[j];
//		}
//	}

	// establish velocity matrix

	for (i=0;i<N;i++) {
		for (j=0;j<3;j++) vn[i][j] = pn[i][j]/m[i];
	}

	// initialize heat current and energies

	for (k=0;k<3;k++){
	//	q[k]=0.;
		qcond[k]=0.;
		qconv[k]=0.;
//		vE[k]=0.;
		vsum[k]=0.;
	}

	for(i=0;i<N;i++) PHI[i] = 0.;


	for(i=0;i<N;i++) {
		for(j=i+1;j<N;j++) {

			for(k=0;k<3;k++) vsum[k] = vn[i][k] + vn[j][k];
	
			for(k=0;k<3;k++) {
				deltaR[k] = xn[i][k]-xn[j][k];
				if (deltaR[k] > Lo2[k]) deltaR[k] = deltaR[k] - L[k]; // periodic boundary conditions
				if (deltaR[k] < (-1.*Lo2[k])) deltaR[k] = deltaR[k] + L[k]; 
			}
				
			if ((deltaR[0] < a) && (deltaR[1] < a) && (deltaR[2] < a)) r=partsep(deltaR);

			// otherwise, give 'r' a value that will always be greater than the cutoff

			else r = 10.*a;

			// short range contribution

			if(r<a) {

				// force

				Fdot_plus=0.;
				for(k=0;k<3;k++) Fdot_plus = Fdot_plus + deltaR[k]*vsum[k];
				Fdot_plus = Fdot_plus*LJforce(r,kramer[potiden[i]][potiden[j]][0],kramer[potiden[i]][potiden[j]][1],kramer[potiden[i]][potiden[j]][2], charge[i], charge[j],kramer[potiden[i]][potiden[j]][3],kramer[potiden[i]][potiden[j]][4]);
				for(k=0;k<3;k++) qcond[k] = qcond[k] + deltaR[k]*Fdot_plus;

				// energy

				dE = LJenergy(r,kramer[potiden[i]][potiden[j]][0],kramer[potiden[i]][potiden[j]][1],kramer[potiden[i]][potiden[j]][2], charge[i], charge[j],kramer[potiden[i]][potiden[j]][3],kramer[potiden[i]][potiden[j]][4]);

				PHI[i] = PHI[i] + dE;
				PHI[j] = PHI[j] + dE;
			}
		}
	}

	// final calculation for energy terms

	for(i=0;i<N;i++) {
		PHI[i] = PHI[i]/2. - charge[i]*charge[i]*(erfc(alpha*a)/(2.*a) + alpha/rootpi) + 1./(2.*m[i])*(pn[i][0]*pn[i][0] + pn[i][1]*pn[i][1] + pn[i][2]*pn[i][2]);
		for(j=0;j<3;j++) qconv[j] = qconv[j] + PHI[i]*vn[i][j];
	}

//	for(i=0;i<3;i++) q[i] = 0.5*q[i] + vE[i];

//	heatcurrent_out<<t<<'\t'<<q[0]<<'\t'<<q[1]<<'\t'<<q[2]<<endl;

	heatcurrent_conv_out<<t<<'\t'<<qconv[0]<<'\t'<<qconv[1]<<'\t'<<qconv[2]<<endl;
	heatcurrent_cond_out<<t<<'\t'<<qcond[0]<<'\t'<<qcond[1]<<'\t'<<qcond[2]<<endl;

}

//*************************************************************************************************************************

void momenta(double pn[][3], double t) {

	int i, j, pplace;
	double pmax, delp, ptemp;

	const int Npplot = 50;
	pmax = 2.5;
	delp = pmax/Npplot;

	int pplot[Npplot];
	
	for (i=0;i<Npplot;i++) pplot[i] = 0;

	for(i=0;i<N;i++) {

		ptemp=0.;

		for(j=0;j<3;j++) ptemp = ptemp + pow(pn[i][j],2);

		ptemp = sqrt(ptemp);
	
		pplace = int(floor(ptemp/delp));		// round down for placement

		pplot[pplace]=pplot[pplace]+1;
	}

	momenta_out<<endl<<t<<endl<<endl;

	for (i=0;i<Npplot;i++) momenta_out<<delp*(2.*i+1.)/2.<<'\t'<<pplot[i]<<endl;
}

//*************************************************************************************************************************

double erfc(double x) { 

	// lookup[] is global

	int pointer;
	double erfcx, normal;

	if (x>3) erfcx = 0.;

	else {
		normal = x*100;
		pointer = int(floor(normal));
		erfcx = lookup_erfc[pointer] + (normal-pointer)*(lookup_erfc[pointer+1]-lookup_erfc[pointer]);
	}

	return erfcx;
}

//*************************************************************************************************************************

void radial(double xn[][3], char partiden[], int tstep, int oxygen_mark[]) {

	// calculates the radial distribution function
	// some terms must be adjusted based on the atom types being considered and the size of the simulation cell

	int i,j,k;
	double deltaR[3];
	double Lo2[3], r;
        for(i=0;i<3;i++) Lo2[i] = L[i]/2.;
	int lower;
	const int number = 40;
	double width = 0.25; // dr
	double gr[number][2];

	for(i=0;i<number;i++) {
		gr[i][0] = width*i;
		gr[i][1] = 0.;
	}

	for (i=0;i<N;i++) {
		for(j=0;j<N;j++){ 

			if( partiden[i]=='O' && partiden[j] == 'O') {

				for(k=0;k<3;k++) {
						deltaR[k] = xn[i][k]-xn[j][k];
						if (deltaR[k] > Lo2[k]) deltaR[k] = deltaR[k]-L[k]; // periodic boundary conditions
						if (deltaR[k] < (-1.*Lo2[k])) deltaR[k] = deltaR[k] + L[k]; 
					}

				r=partsep(deltaR);

				if ((r<a) && (i!=j)) {
//					check_out<<r<<endl;
					lower = int(floor(r/width));
					gr[lower][1] = gr[lower][1] + 1.;
				}
			}
		}
	}

	for(i=0;i<(number-1);i++) gr[i][1] = gr[i][1]/(2.*N/3.*4.*pi/3.*(gr[i+1][0]*gr[i+1][0]*gr[i+1][0] - gr[i][0]*gr[i][0]*gr[i][0]))/(2.*N/3./(L[0]*L[1]*L[2]));
	for(i=0;i<(number-1);i++) gr[i][0] = (gr[i][0]+gr[i+1][0])/2.;
	
	radial_out<<tstep<<endl;
	for(i=0;i<number;i++) radial_out<<gr[i][0]<<'\t'<<gr[i][1]<<endl;
	radial_out<<endl;
}

//*************************************************************************************************************************

void partenergy(double t, double xn[][3], double pn[][3], double m[], double Lo2[3], double kramer[4][4][5], int potiden[], double charge[]) {

	// calculates the energy associated with individual particles

	double r, deltaR[3], dE, PHI[N];
	int i,j,k;

	for (i=0;i<N;i++) PHI[i] = 0.;

	for (i=0;i<N;i++) {
		for(j=i+1;j<N;j++){ 
			for(k=0;k<3;k++) {
					deltaR[k] = xn[i][k]-xn[j][k];
					if (deltaR[k] > Lo2[k]) deltaR[k] = deltaR[k]-L[k]; // periodic boundary conditions
					if (deltaR[k] < (-1.*Lo2[k])) deltaR[k] = deltaR[k] + L[k]; 
				}

			// only calculate r if all three dimensions are within the cutoff

			if ((deltaR[0] < a) && (deltaR[1] < a) && (deltaR[2] < a)) r=partsep(deltaR);

			// otherwise, give 'r' a value that will always be greater than the cutoff

			else r = 10.*a;
			
			if (r<a) {
				dE = LJenergy(r,kramer[potiden[i]][potiden[j]][0],kramer[potiden[i]][potiden[j]][1],kramer[potiden[i]][potiden[j]][2], charge[i], charge[j],kramer[potiden[i]][potiden[j]][3],kramer[potiden[i]][potiden[j]][4]);
				PHI[i] = PHI[i] + dE;
				PHI[j] = PHI[j] + dE;
			}
		}
	}

//	for (i=0;i<N;i++) PHI[i] = PHI[i] + 1/(2.*m[i])*(pn[i][0]*pn[i][0] + pn[i][1]*pn[i][1] + pn[i][2]*pn[i][2]);

	for(i=0;i<N/2;i++) part_energy[i]<<PHI[2*i]<<'\t'<<PHI[2*i+1]<<endl;
//	for(i=0;i<N;i++) part_energy[i]<<PHI[i]<<endl;
}

//*************************************************************************************************************************

void velocity(double pn[][3], double m[]) {

	int i;

	for(i=0;i<N/2;i++) part_vel[i]<<pn[i][0]/m[i]<<'\t'<<pn[i][1]/m[i]<<'\t'<<pn[i][2]/m[i]<<endl;

}

//*************************************************************************************************************************

void positions(double xn[][3]) {

	int i, j;
	double p1[3], p2[3];

	for(i=0;i<N/2;i++) {
		
		for(j=0;j<3;j++) {
			if (xn[2*i][j]-xn_zero[2*i][j] < -1.*L[j]/2.) p1[j] = xn[2*i][j] + L[j];
			else if (xn[2*i][j]-xn_zero[2*i][j] > L[j]/2.) p1[j] = xn[2*i][j] - L[j];
			else p1[j] = xn[2*i][j];

			if (xn[2*i+1][j]-xn_zero[2*i+1][j] < -1.*L[j]/2.) p2[j] = xn[2*i+1][j] + L[j];
			else if (xn[2*i+1][j]-xn_zero[2*i+1][j] > L[j]/2.) p2[j] = xn[2*i+1][j] - L[j];
			else p2[j] = xn[2*i+1][j];
		}

	part_pos[i]<<p1[0]<<'\t'<<p1[1]<<'\t'<<p1[2]<<'\t'<<p2[0]<<'\t'<<p2[1]<<'\t'<<p2[2]<<endl;
	}
}

//*************************************************************************************************************************


void dipole(double xn[][3], double pn[][3], double m[], double charge[], double t) {

	int i,j;
	double sumr[3], sumv[3];
	double correction;

	correction = 0.2715 + 26.8319*L[0]/2*1.39; // for SOD, to get an average dipole moment of zero

	for(i=0;i<3;i++) {
		sumr[i] = 0.;
		sumv[i] = 0.;
	}

	for(i=0;i<N;i++) {
		for (j=0;j<3;j++){
			if (xn[i][j] > L[j]*(0.75-0.0421)) xn[i][j] = xn[i][j] - L[j]; // need to worry about PBC  - for SOD, should work for T < 300 K
//			if (xn[i][j] > L[j]*(0.75-0.0328)) xn[i][j] = xn[i][j] - L[j]; // for LTA
			sumr[j] = sumr[j] + xn[i][j]*charge[i];
			sumv[j] = sumv[j] + pn[i][j]/m[i]*charge[i];
		}
	}

		dipole_out<<t<<'\t'<<sumr[0]+correction<<'\t'<<sumr[1]+correction<<'\t'<<sumr[2]+correction<<endl;
		dipole_dot_out<<t<<'\t'<<sumv[0]<<'\t'<<sumv[1]<<'\t'<<sumv[2]<<endl;

}

//*************************************************************************************************************************

void positions_time(double xn[][3], int tstep) {

	int i,j;

	for(i=0;i<N;i++){
		for(j=0;j<3;j++) part_pos_t<<xn[i][j]<<'\t';
		part_pos_t<<endl;	
	}

}

//*************************************************************************************************************************



