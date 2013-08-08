//DESCRIPTION
//HW 2
//Author: Jason Larkin
//Date: 2/10/10
//Purpose: This code is for MD Class HW3.
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
//HEADER
//--------------------------------------------------------------------------
#include <iostream> 
#include <fstream>
#include <iomanip>
#include <stdlib.h>
#include <math.h>
#include <time.h>
using namespace std;
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
//FUNCTIONS
//--------------------------------------------------------------------------
double pe( double r2);            //Evaluate scalar potential energy for r2.
double force_eval( double r2);    //Evaluate scalar force for r2.
void force(double a2, double f_cutoff, double a, bool cutoff);
void half_momentum(double t_step);
void position(double t_step, double Pset);
void momentum(double t_step);
void momentum_total(void);
double Pot_Energy(double pe_cutoff, double a, double a2, double f_cutoff, bool cutoff); 
double periodic(double r,double L);
double temperature(double KE);
double ke(void);
//Initial velocity functions
void initial_vel(int N);
void vel_rescale(int N,double Tset);
//Thermo/Barostat functions
void baro_param(bool barostat,bool thermostat,double t_step);
void baro_size(double t_step,bool barostat);
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
//GLOBAL VARIABLES
//--------------------------------------------------------------------------
const int ncell=4;       //number of unit cells
const int N=256;         //number of atoms in system.
double L[3];             //simulation cell size.
double pi = atan(1)*4;
double kb = 1.3806E-23; // (J/K)

//LJ Parameters (Ar)
double epsilon_Ar = 1.67E-21; // (J)
double sigma_Ar = 3.4E-10; // (m)
double mass_Ar = 6.63E-26/6.63E-26; // (kg/kg)		                                                       
double a_0 = 5.30E-10/sigma_Ar; //lattice constant Ar.
double a=2.5; //cutoff radius
double a2=a*a;
bool cutoff = true; //bool for using cutoff

//Main Arrays (NxM)
double x[N][3]; //positions
double x_0[N][3]; //initial positions
double p[N][3]; //momentum
double p_half[N][3]; //half momentum
double m[N]; //masses
int id[N];
double F[N][3]; //Forces
double p_sum[3]; //array for total momenta                                                                 

bool thermostat = true;
bool barostat = false;
bool quench = false;
bool restart = false;
bool rescale = false;

double eta_t=0.0;
double tau_t=0.05;

double Tset_K=100.0;
double Tset = Tset_K*kb/epsilon_Ar;
double T=100.0;

double eps_p=0.0;
double tau_p=1.0;
double V=0.0;
double P_ideal=0.0;
double P_virial=0.0;
double P=0.0;
double Pset=0.0;
double eta = 0.0;
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
//IO STREM
//--------------------------------------------------------------------------
ifstream input1("liquid256.txt"); // input data
ifstream input2("input.txt");          
ofstream props_out; // energy, temperature, pressure, momentum 
ofstream output; // extra output stream
ofstream chime_out; //chime output
//ofstream chime_out; // chime
//ofstream atomeye_out;	// atomeye
//ofstream gr_out; // rdf
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
//MAIN
//--------------------------------------------------------------------------
int main() {

//time program
clock_t begin, end;
double time_spent;

begin = clock();
	
//Main variables
   double f_cutoff=force_eval(a2); double pe_cutoff=pe(a2);
   double t_step=0.002; int t_equil=1000; 
   int t_total=5000; int t=0; int t_stats=10;       
   double KE=0.0; double PE=0.0; double TE=0.0;
   int i,k;  
   char hold;              //character to pause the program.
   char filename[255];	   //text string used to open/close files
   
   //cout<<P<<"\t"<<Pset<<endl;
   //cin.get(hold);

//--------------------------------------------------------------------------
//Input/Output Initialization
//--------------------------------------------------------------------------	
//Open output streams                                                                                  
		sprintf(filename, "props.txt");
		props_out.open(filename);
		sprintf(filename, "positions.xyz");
		chime_out.open(filename);
		sprintf(filename, "output.txt");
		output.open(filename);
		
//		stats_out << "Time\tPE/N\tKE/N\tTE/N\tT(K)\tP(MPa)\tpx\tpy\tpz\tL" << endl;
//		stats_out << "0\t"<<PE/N<<"\t"<<KE/N<<"\t"<<TE/N<<"\t"<<T*(epsilon_Ar/kB)<<"\t"<<(P*(epsilon_Ar/(sigma_Ar*sigma_Ar*sigma_Ar)))/1000000<<"\t"<<p_sum[0]<<"\t"<<p_sum[1]<<"\t"<<p_sum[2]<<"\t"<<L[0]<<endl;

//--------------------------------------------------------------------------
//Program Initialization
//--------------------------------------------------------------------------	
    //Read in positions from file	
//            if(!input){
//              cout << "While opening a file an error is encountered" << endl;
//            }
//            else{
//              cout << "File is successfully opened" << endl;
//            }

          if (restart==true){
          //        for (i=0; i<N; i++){
          //        input2 >> x[i][0] >> x[i][1] >> x[i][2] >> p[i][0] >> p[i][1] >> p[i][2];
          //        x_0[i][0] = x[i][0]; x_0[i][1] = x[i][1]; x_0[i][2] = x[i][2];
          //        m[i] = mass_Ar;   //set mass to 1.0;
          //        cout << x[i][0] << "\t" << x[i][1] << "\t" << x[i][2] << endl;}
          //          PE = Pot_Energy(pe_cutoff, a, a2, f_cutoff, cutoff);
          //          KE = ke();
          //          T = temperature(KE);
			}
          else{
                 for (i=0; i<N; i++){
                  input1 >> x[i][0] >> x[i][1] >> x[i][2];
                  //input1 >> id[i] >> m[i] >> x[i][0] >> x[i][1] >> x[i][2];
                  x_0[i][0] = x[i][0]; x_0[i][1] = x[i][1]; x_0[i][2] = x[i][2];
                  m[i] = mass_Ar;   //set mass to 1.0;
                  id[i] = i;
                  cout << id[i] << "\t" << m[i] << "\t" << x[i][0] << "\t" << x[i][1] << "\t" << x[i][2] << endl;}
                    PE = Pot_Energy(pe_cutoff, a, a2, f_cutoff, cutoff); //Pot Eneryg
                    initial_vel(N); //sets initial velocities randomly and rescales them
                    vel_rescale(N,Tset); //to desired temperature.
                    KE = ke();
                    T = temperature(KE);}
                    
          for (k=0; k<3; k++){ 
                    L[k] = 7.7;}
                    V = L[0]*L[1]*L[2];
                    //Find initial Forces
                    force(a2,f_cutoff,a,cutoff);
                    P=Pset;
                    //cin.get(hold);

cout <<"Equllibration"<<endl;

//--------------------------------------------------------------------------
//Equillibration Loop
//--------------------------------------------------------------------------	

    for(t=0;t<=t_equil;t++){
            Pset=P;
            P=Pset;
    
            half_momentum(t_step);
         // baro_size(t_step,barostat);
            position(t_step,Pset);
         // baro_param(barostat,thermostat,t_step);
            force(a2,f_cutoff,a,cutoff);
            Pset=P;
            P=Pset;
            momentum(t_step);
            vel_rescale(N,Tset);
            momentum_total();
            
            PE = Pot_Energy(pe_cutoff, a, a2, f_cutoff, cutoff);
            KE = ke();
            TE = PE + KE;
            T = temperature(KE);
            
            if (t%(t_stats*10) == 0) {
                  cout << t<<"\t"<< T*(epsilon_Ar/kb)<<"\t"<< P << "\t" << L[0]<<"\t" <<p_sum[0]<<"\t"<<p_sum[1]<<"\t"<<p_sum[2]<<"\t"<<KE<<"\t"<<PE<<"\t"<< TE <<endl;
                }
}

cout <<"Simulation"<<endl;

//--------------------------------------------------------------------------
//Simulation Loop
//--------------------------------------------------------------------------	

    for(t=0;t<=t_total;t++){

            Pset=P;
            P=Pset;
            
            
    
            half_momentum(t_step);
          baro_size(t_step,barostat);
            position(t_step,Pset);
          baro_param(barostat,thermostat,t_step);
            force(a2,f_cutoff,a,cutoff);
            Pset=P;
            P=Pset;
            momentum(t_step);
            momentum_total();
            
            PE = Pot_Energy(pe_cutoff, a, a2, f_cutoff, cutoff);
            KE = ke();
            TE = PE + KE;
            T = temperature(KE);
            
            // output to screen (to monitor simulation)
    
    			if (t%(t_stats*10) == 0) {
                  cout << t<<"\t"<< T*(epsilon_Ar/kb)<<"\t"<< P << "\t" << L[0]<<"\t" <<p_sum[0]<<"\t"<<p_sum[1]<<"\t"<<p_sum[2]<<"\t"<<KE<<"\t"<<PE<<"\t"<< TE <<endl;
      	        //output data
		               props_out<< t<<"\t"<< T*(epsilon_Ar/kb)<<"\t"<< P << "\t"<< P_ideal << "\t"<< P_virial << "\t" << L[0]<<"\t" <<p_sum[0]<<"\t"<<p_sum[1]<<"\t"<<p_sum[2]<<"\t"<<KE<<"\t"<<PE<<"\t"<< TE <<endl;		
    			//spit out chime file 
                       chime_out<< N-1 <<endl;
                 for (i=0;i<N;i++){
                     chime_out<< "Ar" << " " << x[i][0] << " " << x[i][1] << " " << x[i][2] <<endl;
                     }
                }
    				    
    }
    
//--------------------------------------------------------------------------
//Spit out positions/Momenta
//--------------------------------------------------------------------------	    
    for (i=0;i<N;i++){
    output<< x[i][0] << " " << x[i][1] << " " << x[i][2] << p[i][0] << " " << p[i][1] << " " << p[i][2] <<endl;
}
    
end = clock();
time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
cout << "Total Time: " << time_spent << " (s)" << endl;
}
//--------------------------------------------------------------------------
//END MAIN
//--------------------------------------------------------------------------



//--------------------------------------------------------------------------
//FUNCTIONS
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
double pe(double r2) {
 //FUNCTION: evaluate the potential energy for a given separation r2
     double inv_r6 = 1./(r2*r2*r2);
     double phi;
     phi = 4.*(inv_r6*inv_r6 - inv_r6);
 return phi;
}

//--------------------------------------------------------------------------
double force_eval(double r2){
 //FUNCTION: evaluate magnitude of the force for given separation r2
    //using the method derived here:
    //http://www.pages.drexel.edu/~cfa22/msim/node36.html
    double f;
     double r4 = r2*r2; 
     double r8 = r4*r4; 
     double r14i = 1/(r8*r4*r2);
     f = 48*r14i - 24*r4*r2*r14i;
return f;
}

//--------------------------------------------------------------------------
void force(double a2, double f_cutoff,double a,bool cutoff){
 //FUNCTION: find forces acting on all atoms in system.
     int i=0;
     int j=0;
     int k=0;
     double r2;
     double f;
     double rij[3];
     P_virial = 0.0;
     P_ideal = 0.0;
     //clear old Forces
     for(i=0;i<N;i++){F[i][0]=0.0; F[i][1]=0.0; F[i][2]=0.0;}

     for (i=0;i<N;i++){
         for (j=i+1;j<N;j++){    
             for (k=0;k<3;k++){
                 rij[k] = x[i][k] - x[j][k];
                 rij[k] = periodic(rij[k],L[k]);  //periodic boundaries
             }
             r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
             if (r2<a2) {
                 if (cutoff==true){
                     f = force_eval(r2) - f_cutoff*a*(1/sqrt(r2));
                     }
                 else{
                     f = force_eval(r2);
                 }
                 for (k=0;k<3;k++){  
                     F[i][k] = F[i][k] + rij[k]*f;
                     F[j][k] = F[j][k] - rij[k]*f;
                     }
                 P_virial = P_virial + r2*f;
             }
         }
         }
         P_ideal = ((N*T)/V);
    P = (P_virial/(3*V)) + ((N*T)/V);
    P_virial = (P_virial/(3*V));
}

//--------------------------------------------------------------------------
void half_momentum(double t_step){
 //FUNCTION: find the momentum at the half time step.
      int i=0;
      int k=0;
     //p_half(1:N,1:3)=zeros(N,3); 
          //clear old momenta
     for(i=0;i<N;i++){p_half[i][0]=0.0; p_half[i][1]=0.0; p_half[i][2]=0.0;}
     
     for (i=0; i<N; i++){
         for (k=0;k<3;k++){
             p_half[i][k] = p[i][k] + (F[i][k] - (eta_t+eps_p)*p[i][k])*t_step*0.5;
         }
     }
 }    

//-------------------------------------------------------------------------- 
void position(double t_step, double Pset){
    // FUNCTION: evolve the positions.
          int i=0;
      int k=0;
      double term1 = eps_p*t_step;
      double term2 = (t_step*t_step/(2*(tau_p*tau_p)))*(P-Pset);
      double term3 = (t_step*eps_p)*(t_step*eps_p)*0.5;
     
    for(i=0;i<N;i++){
         for (k=0;k<3;k++){
             x[i][k] = (1 + term1 + term2 + term3)*x[i][k] + p_half[i][k]*t_step/m[i] + (t_step*t_step)*eps_p*p[i][k]*0.5/m[i];
             //x(i,k) = (1 + term1 + term2 + term3)*x(i,k) + p_half(i,k)*t_step/m(1,i) + (t_step*t_step)*eps_p*p(i,k)*0.5/m(1,i);
          // x_p(i,k) = (1 + term1 + term2 + term3)*x_p(i,k) + (t_step*t_step)*eps_p*p(i,k)*0.5/m(1,i);
             if (x[i][k]>L[k]){
                 x[i][k] = x[i][k] - L[k];
             //  x(i,k) = x(i,k) - L(1,k);
             }
             if (x[i][k]<0.0){
                 x[i][k] = x[i][k] + L[k];
              // x(i,k) = x(i,k) + L(1,k);
             }
         }
         }
 }

//-------------------------------------------------------------------------- 
void momentum(double t_step){
     // FUNCTION: calcualte the momentum at the full time step.
          //clear old momenta
          int i,k;
     for(i=0;i<N;i++){p[i][0]=0.0; p[i][1]; p[i][2]=0.0;}
     
     double bottom = 1 + (eta_t + eps_p)*t_step*0.5;
     for(i=0;i<N;i++){
         for (k=0;k<3;k++){
             p[i][k] = (p_half[i][k] + F[i][k]*0.5*t_step)/bottom;
            //   p(i,k) = (p_half(i,k) + F(i,k)*0.5*t_step)/bottom;
         }
         }
}

//--------------------------------------------------------------------------
void momentum_total(void){
     // FUNCTION: calcualte the total momentum.
     int i=0;
     int k=0;
     
     p_sum[0]=0.0; 
     p_sum[1]=0.0;
     p_sum[2]=0.0;
     
     for (k=0;k<3;k++){
         for(i=0;i<N;i++){
             p_sum[k] = p_sum[k] + p[i][k];
         }
     }
//     p_tot[0] = p_sum[0]+p_sum[1]+p_sum[2];
//return p_tot;
 }
 
//-------------------------------------------------------------------------- 
double Pot_Energy(double pe_cutoff, double a, double a2, double f_cutoff, bool cutoff){
       // FUNCTION: calcualte the total Potential Energy in the system.
       double PE=0.0;
       int i=0;
       int j=0;
       int k=0;
       double rij[3];
       double r2;
     for(i=0;i<N;i++){
         for (j=i+1;j<N;j++){ 
             for (k=0;k<3;k++){
                  rij[k] = x[i][k] - x[j][k];
                  rij[k] = periodic(rij[k],L[k]);
                 }
                 
             r2 = rij[0]*rij[0] + rij[1]*rij[1] + rij[2]*rij[2];
             if (r2<a2){
                 if (cutoff==true){
                     PE = PE + pe(r2) - pe_cutoff -(f_cutoff*a*(sqrt(r2)-a)); }
                 else{
                     PE = PE + pe(r2);
                 }
             }
         }
     }
return PE;
}

//-------------------------------------------------------------------------- 
double periodic(double r, double Length){
//determine periodic boundary conditions for rectangular cell
            if (r > (Length/2)){     //%periodic boundaries
                r = r - Length;
                }
            if (r < (-Length/2)){    //%periodic boundaries
                r = r + Length;
            }
 return r;
}
 
//-------------------------------------------------------------------------- 
double ke(void){
   //calculate Kinetic Energy
double kinetic=0.0;
int i;
    for(i=0;i<N;i++){
                     kinetic = kinetic + 0.5*(1/m[i])*(p[i][0]*p[i][0] + p[i][1]*p[i][1] + p[i][2]*p[i][2]);
    //  kinetic = kinetic + 0.5*(1/m(1,i))*(p(i,1)*p(i,1) +p(i,2)*p(i,2) + p(i,3)*p(i,3));
    }
return kinetic;
}

//--------------------------------------------------------------------------
double temperature(double KE){
T = (2./3.)*((KE)/(N-1));
return T;
}

//--------------------------------------------------------------------------
void initial_vel(int N){

int i; 
int j; 
int k;
double p_rand;

// initial seed for random function
		srand( time(NULL) ); 	
		for (i=0; i<N; i++){
			for (k=0; k<3; k++){
				p_rand = ((2*( rand() % 10000+1)) - 10000.0)/10000.0;
				p[i][k] = p_rand;
			}
		} 
}

//--------------------------------------------------------------------------
void vel_rescale(int N,double Tset){

double p_sum[3]; double p_sub[3];
double mass;
double KE_set; double alpha; double KE;
int i; int j;

//I use this for single species.  Multiple species would need v=p./m
mass=m[1];
//% tfac = 3*N*kb*Tset/mass;
//% sumvsq=sum(sum(v.*v));
//% fac = sqrt(tfac/sumvsq);
//% v=v*fac;
KE = ke();
KE_set = 1.5*(N-1)*Tset;
alpha = (KE_set)/KE;
alpha = sqrt(alpha);

    for (i=0; i<N; i++){
        for (j=0; j<3; j++){
            p[i][j] = p[i][j]*alpha;
        }
    }
    
    for (j=0; j<3; j++) p_sum[j] = 0.0;			// sum momentum
		for (i=0; i<N; i++){
			for (j=0; j<3; j++) p_sum[j] = p_sum[j] + p[i][j];
		}
	for (j=0; j<3; j++) p_sub[j] = p_sum[j]/N;	// subtract average from every atom
		for (i=0; i<N; i++){						// subtract momentum
			for (j=0; j<3; j++) p[i][j] = p[i][j] - p_sub[j];
        }
    
    
}

//--------------------------------------------------------------------------
void baro_param(bool barostat,bool thermostat,double t_step){

    if (thermostat ==true){
        eta_t = eta_t + (t_step/(tau_t*tau_t))*((T/Tset)-1);}
    else{
        eta_t = 0.0;
    }

    if (barostat ==true){
        eps_p = eps_p + (t_step/(tau_p*tau_p))*(P-Pset);}
    else {
        eps_p = 0;
    }

}

//--------------------------------------------------------------------------
void baro_size(double t_step,bool barostat){
     int j;
     int k;
     
for (j=0; j<3; j++){
    for (k=0; k<3; k++){
        if (barostat==true) {
        L[k] = L[k]*pow((1.0+(3.0*eps_p*t_step)),(1./3.));
        }
    }
}
    V = L[0]*L[1]*L[2];
    
}
