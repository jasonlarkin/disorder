
%function SEDfit_JL_040711

%--------------INPUT-------------------------------------------------------

%LJ Potential and Material Parameters
epsilon_Ar = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
sigma_Ar = 3.4E-10;                 %Angstroms 3.4E-10 meters
a_0 = 5.2686E-10/sigma_Ar;            %the lattice constant of Ar: http://www.infoplease.com/periodictable.php?id=18
mass_Ar = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
%mass_Ar = mass_Ar/mass_Ar;
tau_Ar = sqrt((mass_Ar*(sigma_Ar^2))/epsilon_Ar);
kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
hbar = 1.054E-34;                %J/s

NUM_ATOMS_UCELL = 4;

i = sqrt(-1);

%Set initial positions for id matching
x0 = [1	1	0	0	0
2	1	0.7748	0.7748	0
3	1	0.7748	0	0.7748
4	1	0	0.7748	0.7748
5	1	0	0	1.5496
6	1	0.7748	0.7748	1.5496
7	1	0.7748	0	2.3244
8	1	0	0.7748	2.3244
9	1	0	0	3.0992
10	1	0.7748	0.7748	3.0992
11	1	0.7748	0	3.874
12	1	0	0.7748	3.874
13	1	0	0	4.6488
14	1	0.7748	0.7748	4.6488
15	1	0.7748	0	5.4236
16	1	0	0.7748	5.4236
17	1	0	1.5496	0
18	1	0.7748	2.3244	0
19	1	0.7748	1.5496	0.7748
20	1	0	2.3244	0.7748
21	1	0	1.5496	1.5496
22	1	0.7748	2.3244	1.5496
23	1	0.7748	1.5496	2.3244
24	1	0	2.3244	2.3244
25	1	0	1.5496	3.0992
26	1	0.7748	2.3244	3.0992
27	1	0.7748	1.5496	3.874
28	1	0	2.3244	3.874
29	1	0	1.5496	4.6488
30	1	0.7748	2.3244	4.6488
31	1	0.7748	1.5496	5.4236
32	1	0	2.3244	5.4236
33	1	0	3.0992	0
34	1	0.7748	3.874	0
35	1	0.7748	3.0992	0.7748
36	1	0	3.874	0.7748
37	1	0	3.0992	1.5496
38	1	0.7748	3.874	1.5496
39	1	0.7748	3.0992	2.3244
40	1	0	3.874	2.3244
41	1	0	3.0992	3.0992
42	1	0.7748	3.874	3.0992
43	1	0.7748	3.0992	3.874
44	1	0	3.874	3.874
45	1	0	3.0992	4.6488
46	1	0.7748	3.874	4.6488
47	1	0.7748	3.0992	5.4236
48	1	0	3.874	5.4236
49	1	0	4.6488	0
50	1	0.7748	5.4236	0
51	1	0.7748	4.6488	0.7748
52	1	0	5.4236	0.7748
53	1	0	4.6488	1.5496
54	1	0.7748	5.4236	1.5496
55	1	0.7748	4.6488	2.3244
56	1	0	5.4236	2.3244
57	1	0	4.6488	3.0992
58	1	0.7748	5.4236	3.0992
59	1	0.7748	4.6488	3.874
60	1	0	5.4236	3.874
61	1	0	4.6488	4.6488
62	1	0.7748	5.4236	4.6488
63	1	0.7748	4.6488	5.4236
64	1	0	5.4236	5.4236
65	1	1.5496	0	0
66	1	2.3244	0.7748	0
67	1	2.3244	0	0.7748
68	1	1.5496	0.7748	0.7748
69	1	1.5496	0	1.5496
70	1	2.3244	0.7748	1.5496
71	1	2.3244	0	2.3244
72	1	1.5496	0.7748	2.3244
73	1	1.5496	0	3.0992
74	1	2.3244	0.7748	3.0992
75	1	2.3244	0	3.874
76	1	1.5496	0.7748	3.874
77	1	1.5496	0	4.6488
78	1	2.3244	0.7748	4.6488
79	1	2.3244	0	5.4236
80	1	1.5496	0.7748	5.4236
81	1	1.5496	1.5496	0
82	1	2.3244	2.3244	0
83	1	2.3244	1.5496	0.7748
84	1	1.5496	2.3244	0.7748
85	1	1.5496	1.5496	1.5496
86	1	2.3244	2.3244	1.5496
87	1	2.3244	1.5496	2.3244
88	1	1.5496	2.3244	2.3244
89	1	1.5496	1.5496	3.0992
90	1	2.3244	2.3244	3.0992
91	1	2.3244	1.5496	3.874
92	1	1.5496	2.3244	3.874
93	1	1.5496	1.5496	4.6488
94	1	2.3244	2.3244	4.6488
95	1	2.3244	1.5496	5.4236
96	1	1.5496	2.3244	5.4236
97	1	1.5496	3.0992	0
98	1	2.3244	3.874	0
99	1	2.3244	3.0992	0.7748
100	1	1.5496	3.874	0.7748
101	1	1.5496	3.0992	1.5496
102	1	2.3244	3.874	1.5496
103	1	2.3244	3.0992	2.3244
104	1	1.5496	3.874	2.3244
105	1	1.5496	3.0992	3.0992
106	1	2.3244	3.874	3.0992
107	1	2.3244	3.0992	3.874
108	1	1.5496	3.874	3.874
109	1	1.5496	3.0992	4.6488
110	1	2.3244	3.874	4.6488
111	1	2.3244	3.0992	5.4236
112	1	1.5496	3.874	5.4236
113	1	1.5496	4.6488	0
114	1	2.3244	5.4236	0
115	1	2.3244	4.6488	0.7748
116	1	1.5496	5.4236	0.7748
117	1	1.5496	4.6488	1.5496
118	1	2.3244	5.4236	1.5496
119	1	2.3244	4.6488	2.3244
120	1	1.5496	5.4236	2.3244
121	1	1.5496	4.6488	3.0992
122	1	2.3244	5.4236	3.0992
123	1	2.3244	4.6488	3.874
124	1	1.5496	5.4236	3.874
125	1	1.5496	4.6488	4.6488
126	1	2.3244	5.4236	4.6488
127	1	2.3244	4.6488	5.4236
128	1	1.5496	5.4236	5.4236
129	1	3.0992	0	0
130	1	3.874	0.7748	0
131	1	3.874	0	0.7748
132	1	3.0992	0.7748	0.7748
133	1	3.0992	0	1.5496
134	1	3.874	0.7748	1.5496
135	1	3.874	0	2.3244
136	1	3.0992	0.7748	2.3244
137	1	3.0992	0	3.0992
138	1	3.874	0.7748	3.0992
139	1	3.874	0	3.874
140	1	3.0992	0.7748	3.874
141	1	3.0992	0	4.6488
142	1	3.874	0.7748	4.6488
143	1	3.874	0	5.4236
144	1	3.0992	0.7748	5.4236
145	1	3.0992	1.5496	0
146	1	3.874	2.3244	0
147	1	3.874	1.5496	0.7748
148	1	3.0992	2.3244	0.7748
149	1	3.0992	1.5496	1.5496
150	1	3.874	2.3244	1.5496
151	1	3.874	1.5496	2.3244
152	1	3.0992	2.3244	2.3244
153	1	3.0992	1.5496	3.0992
154	1	3.874	2.3244	3.0992
155	1	3.874	1.5496	3.874
156	1	3.0992	2.3244	3.874
157	1	3.0992	1.5496	4.6488
158	1	3.874	2.3244	4.6488
159	1	3.874	1.5496	5.4236
160	1	3.0992	2.3244	5.4236
161	1	3.0992	3.0992	0
162	1	3.874	3.874	0
163	1	3.874	3.0992	0.7748
164	1	3.0992	3.874	0.7748
165	1	3.0992	3.0992	1.5496
166	1	3.874	3.874	1.5496
167	1	3.874	3.0992	2.3244
168	1	3.0992	3.874	2.3244
169	1	3.0992	3.0992	3.0992
170	1	3.874	3.874	3.0992
171	1	3.874	3.0992	3.874
172	1	3.0992	3.874	3.874
173	1	3.0992	3.0992	4.6488
174	1	3.874	3.874	4.6488
175	1	3.874	3.0992	5.4236
176	1	3.0992	3.874	5.4236
177	1	3.0992	4.6488	0
178	1	3.874	5.4236	0
179	1	3.874	4.6488	0.7748
180	1	3.0992	5.4236	0.7748
181	1	3.0992	4.6488	1.5496
182	1	3.874	5.4236	1.5496
183	1	3.874	4.6488	2.3244
184	1	3.0992	5.4236	2.3244
185	1	3.0992	4.6488	3.0992
186	1	3.874	5.4236	3.0992
187	1	3.874	4.6488	3.874
188	1	3.0992	5.4236	3.874
189	1	3.0992	4.6488	4.6488
190	1	3.874	5.4236	4.6488
191	1	3.874	4.6488	5.4236
192	1	3.0992	5.4236	5.4236
193	1	4.6488	0	0
194	1	5.4236	0.7748	0
195	1	5.4236	0	0.7748
196	1	4.6488	0.7748	0.7748
197	1	4.6488	0	1.5496
198	1	5.4236	0.7748	1.5496
199	1	5.4236	0	2.3244
200	1	4.6488	0.7748	2.3244
201	1	4.6488	0	3.0992
202	1	5.4236	0.7748	3.0992
203	1	5.4236	0	3.874
204	1	4.6488	0.7748	3.874
205	1	4.6488	0	4.6488
206	1	5.4236	0.7748	4.6488
207	1	5.4236	0	5.4236
208	1	4.6488	0.7748	5.4236
209	1	4.6488	1.5496	0
210	1	5.4236	2.3244	0
211	1	5.4236	1.5496	0.7748
212	1	4.6488	2.3244	0.7748
213	1	4.6488	1.5496	1.5496
214	1	5.4236	2.3244	1.5496
215	1	5.4236	1.5496	2.3244
216	1	4.6488	2.3244	2.3244
217	1	4.6488	1.5496	3.0992
218	1	5.4236	2.3244	3.0992
219	1	5.4236	1.5496	3.874
220	1	4.6488	2.3244	3.874
221	1	4.6488	1.5496	4.6488
222	1	5.4236	2.3244	4.6488
223	1	5.4236	1.5496	5.4236
224	1	4.6488	2.3244	5.4236
225	1	4.6488	3.0992	0
226	1	5.4236	3.874	0
227	1	5.4236	3.0992	0.7748
228	1	4.6488	3.874	0.7748
229	1	4.6488	3.0992	1.5496
230	1	5.4236	3.874	1.5496
231	1	5.4236	3.0992	2.3244
232	1	4.6488	3.874	2.3244
233	1	4.6488	3.0992	3.0992
234	1	5.4236	3.874	3.0992
235	1	5.4236	3.0992	3.874
236	1	4.6488	3.874	3.874
237	1	4.6488	3.0992	4.6488
238	1	5.4236	3.874	4.6488
239	1	5.4236	3.0992	5.4236
240	1	4.6488	3.874	5.4236
241	1	4.6488	4.6488	0
242	1	5.4236	5.4236	0
243	1	5.4236	4.6488	0.7748
244	1	4.6488	5.4236	0.7748
245	1	4.6488	4.6488	1.5496
246	1	5.4236	5.4236	1.5496
247	1	5.4236	4.6488	2.3244
248	1	4.6488	5.4236	2.3244
249	1	4.6488	4.6488	3.0992
250	1	5.4236	5.4236	3.0992
251	1	5.4236	4.6488	3.874
252	1	4.6488	5.4236	3.874
253	1	4.6488	4.6488	4.6488
254	1	5.4236	5.4236	4.6488
255	1	5.4236	4.6488	5.4236
256	1	4.6488	5.4236	5.4236];


%SED Parameters
NUM_ATOMS = 256; NUM_ATOMS_UCELL = 4; NUM_UCELL_COPIES=64;
N_wmax = 2^6; N_wstep = 2^16; t_total = 2^16; t_step = 2^4; dt=0.002;
    %TOTAL_STEPS = 2^20; SEG_LEN = 2^16; NUM_SEG= TOTAL_STEPS/SEG_LEN ;
    %SAMPLE_RATE = 2^4;
    w_step = 2*pi/(N_wstep*dt); w_max = 2*pi/(N_wmax*dt);
    NUM_TSTEPS = t_total/t_step; NUM_OMEGAS = N_wstep/N_wmax;  
%Initialize SED object to do averaging over seeds
    str_main=strcat('E:\CMU\work\Phonons\LJArgon\Solid\crystal\perfect\SED\1\');
    str_read=strcat(str_main,'dump_1_1.vel');
    fid=fopen(str_read);
    dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
%Figure out how many atoms and timesteps there are
    %scan = scandump(str_read);
    %dump = readdumpvel_one(str_read,0,4);
    %NUM_ATOMS = dump.Natoms;
    %DUMP_SEG_LEN = dump.position;
%Pick out atomic velocity 1 atom at a time
%FFT_LEN = 



%Load kpt list
    str_read=strcat(str_main,'kptlist_full.dat');
    SED.kptlist(:,1:3) = load(str_read); NUM_KPTS = length(SED.kptlist(:,1:3));
    
%Store velocity data of all atoms: subtract off the last time step
    vel = zeros(1:NUM_ATOMS,1:NUM_TSTEPS,1:3);
        for iatom = 1:NUM_ATOMS  
            vel(iatom,1:NUM_TSTEPS,1) = dummy{1}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
            vel(iatom,1:NUM_TSTEPS,2) = dummy{2}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));
            vel(iatom,1:NUM_TSTEPS,3) = dummy{3}(iatom:NUM_ATOMS:(length(dummy{1}(:))-NUM_ATOMS));  
        end
%Save dummy velocity data for now
    SED.vel = dummy;
    clear dummy
   
%Zero RE_X
    RE_X(1:NUM_KPTS,1:NUM_TSTEPS,iaucell) = 0.0; IM_X[ikpt][iw][iaucell] = 0.0;
    RE_Y[ikpt][iw][iaucell] = 0.0; IM_Y[ikpt][iw][iaucell] = 0.0;
    RE_Z[ikpt][iw][iaucell] = 0.0; IM_Z[ikpt][iw][iaucell] = 0.0;
    
tic     
%Loop over kpts
for ikpt = 1:NUM_KPTS
    for iaucell = 1:NUM_ATOMS_UCELL
        for iucell = 1:NUM_UCELL_COPIES
            
            X_comp = fft(vel(iaucell+(NUM_ATOMS_UCELL-1)*iucell,1:(NUM_TSTEPS),1));
            Y_comp = fft(vel(iaucell+(NUM_ATOMS_UCELL-1)*iucell,1:(NUM_TSTEPS),2));
            Z_comp = fft(vel(iaucell+(NUM_ATOMS_UCELL-1)*iucell,1:(NUM_TSTEPS),3));
            
            
            Fs = 1000;                    % Sampling frequency
            T = 1/Fs;                     % Sample time
            L = 1000;                     % Length of signal
            t = (0:L-1)*T;                % Time vector
            % Sum of a 50 Hz sinusoid and a 120 Hz sinusoid
            x = 0.7*sin(2*pi*50*t) + sin(2*pi*120*t); 
            y = x + 2*randn(size(t));     % Sinusoids plus noise
            plot(Fs*t(1:50),y(1:50))
            title('Signal Corrupted with Zero-Mean Random Noise')
            xlabel('time (milliseconds)')
            
            NFFT = 2^nextpow2(L); % Next power of 2 from length of y
            Y = fft(y,NFFT)/L;
            f = Fs/2*linspace(0,1,NFFT/2+1);

            % Plot single-sided amplitude spectrum.
            plot(f,2*abs(Y(1:NFFT/2+1))) 
            title('Single-Sided Amplitude Spectrum of y(t)')
            xlabel('Frequency (Hz)')
            ylabel('|Y(f)|')
            
            %FP_X = exp(i*x0(iaucell+NUM_ATOMS_UCELL*iucell,1)*SED.kptlist(ikpt,1));
        end
    end
end

toc


pause









































%Johns Code
%     for				for(OMEGA_COUNT=0;OMEGA_COUNT<OMEGA_COUNT_MAX;OMEGA_COUNT++){
% 					OMEGA_CURRNET=double(double(OMEGA_COUNT)*OMEGA_RES)+OMEGA_MIN;					
% 					for(K_COUNT=0;K_COUNT<K_COUNT_MAX;K_COUNT++){
% 						K_CURRENT=double(double(K_COUNT)*K_RES+K_MIN);
% 						for(p=0;p<FROZEN_TOTAL;p++){						
% 							for(nb=0;nb<BASIS_NUM;nb++){
% 								ndivN=double(double(nb)/double(BASIS_NUM));
% 								ARG_TEMP=ndivN*K_CURRENT-1.*OMEGA_CURRNET*T_CURRENT*DT;	
% 								COSINE_TERM=cos(ARG_TEMP);
% 								SINE_TERM=sin(ARG_TEMP);							
% 								(*RE_X)[p][OMEGA_COUNT][K_COUNT]=(*RE_X)[p][OMEGA_COUNT][K_COUNT]+CVX[CARBON_P[nb][p]]*COSINE_TERM;
% 								(*IM_X)[p][OMEGA_COUNT][K_COUNT]=(*IM_X)[p][OMEGA_COUNT][K_COUNT]+CVX[CARBON_P[nb][p]]*SINE_TERM;
% 								(*RE_Y)[p][OMEGA_COUNT][K_COUNT]=(*RE_Y)[p][OMEGA_COUNT][K_COUNT]+CVY[CARBON_P[nb][p]]*COSINE_TERM;
% 								(*IM_Y)[p][OMEGA_COUNT][K_COUNT]=(*IM_Y)[p][OMEGA_COUNT][K_COUNT]+CVY[CARBON_P[nb][p]]*SINE_TERM;							
% 								(*RE_Z)[p][OMEGA_COUNT][K_COUNT]=(*RE_Z)[p][OMEGA_COUNT][K_COUNT]+CVZ[CARBON_P[nb][p]]*COSINE_TERM;
% 								(*IM_Z)[p][OMEGA_COUNT][K_COUNT]=(*IM_Z)[p][OMEGA_COUNT][K_COUNT]+CVZ[CARBON_P[nb][p]]*SINE_TERM;																
% 							}
% 						}
% 					}
% 				}
% 				T_CURRENT=T_CURRENT+1.;
% 				if(t>DISP_START&&t%DISP_INT==0){
% 					for(OMEGA_COUNT=0;OMEGA_COUNT<OMEGA_COUNT_MAX;OMEGA_COUNT++){
% 						for(K_COUNT=0;K_COUNT<K_COUNT_MAX;K_COUNT++){					
% 							for(p=0;p<FROZEN_TOTAL;p++){DISP_MAG[OMEGA_COUNT][K_COUNT]=DISP_MAG[OMEGA_COUNT][K_COUNT]+(*RE_X)[p][OMEGA_COUNT][K_COUNT]*(*RE_X)[p][OMEGA_COUNT][K_COUNT]+(*IM_X)[p][OMEGA_COUNT][K_COUNT]*(*IM_X)[p][OMEGA_COUNT][K_COUNT]+(*RE_Y)[p][OMEGA_COUNT][K_COUNT]*(*RE_Y)[p][OMEGA_COUNT][K_COUNT]+(*IM_Y)[p][OMEGA_COUNT][K_COUNT]*(*IM_Y)[p][OMEGA_COUNT][K_COUNT]+(*RE_Z)[p][OMEGA_COUNT][K_COUNT]*(*RE_Z)[p][OMEGA_COUNT][K_COUNT]+(*IM_Z)[p][OMEGA_COUNT][K_COUNT]*(*IM_Z)[p][OMEGA_COUNT][K_COUNT];}
% 							K_CURRENT=double(double(K_COUNT)*K_RES+K_MIN);
% 							OMEGA_CURRNET=double(double(OMEGA_COUNT)*OMEGA_RES)+OMEGA_MIN;					
% 							disp_out<<OMEGA_CURRNET/(2.*PI*TSTAR*1.E12)<<'\t'<<K_CURRENT<<'\t'<<DT*DISP_MAG[OMEGA_COUNT][K_COUNT]*M_C/2./double(C)/double(C)<<endl;
% 						}
% 					}
% 				}
% 			}    


% for ikpt = 1:length(SED.kptlist)
%     for iatom = 1:NUM_ATOMS_UCELL
%         for iucell = iatom:NUM_ATOMS_UCELL:NUM_ATOMS
%             range = iatom:NUM_ATOMS_UCELL:NUM_ATOMS;
%             PHI_X = PHI_X + exp(i*SED.kptlist(ikpt,1)*x0(iucell,2))*fft(vel(iucell,:,1));
%             PHI_Y = PHI_Y + exp(i*SED.kptlist(ikpt,1)*x0(iucell,2))*fft(vel(iucell,:,2));
%             PHI_Z = PHI_Z + exp(i*SED.kptlist(ikpt,1)*x0(iucell,2))*fft(vel(iucell,:,3));
%         end
%         SED.PHI() = PHI_X.*PHI_X + PHI_Y.*PHI_Y + PHI_Y.*PHI_Y;
%     end
% end









    

% 
% 
%         SAMPLE_RATE = input('Enter sample rate: ');
%         NUM_UCELLS = input('Enter number of unit cells: ');
%         NUM_MODES = input('Enter number of modes to expect: ');
% 
%         %dummy is a dummy array to store all the SED over all kpts.  It has size
%         %dummy((1024+1)*numkpts,3).  The portion of the column data that contains
%         %the SED for different frequencies will have 3 columns, 1 with actual SED
%         %data the other 2 with NaNs.  Just omit these.
% 
%         %Initialize SED object to do averaging over seeds
%         str_main=strcat('E:\CMU\work\Phonons\SED\perfect\8x8x8\5K\');
%         str=strcat(str_main,'seed1\SpectralEnergy.txt');
%         %ALEX INPUT
%         %str_main=strcat('E:\CMU\work\Phonons\SED\perfect\alex\20K\');
%         %str=strcat(str_main,'SEDseed_avg.txt');
% 
%         dummy = importdata(str,'\t');  
%             %This figures out how many kpts are being read in by checking the length
%             %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
%             %we've been having SAMPLE_RATE=1024;
%         NUM_KPTS = length(dummy(:,1)) / (SAMPLE_RATE+1);
%         SED.redkpt.sed(1:SAMPLE_RATE,1:NUM_KPTS) = 0.0;
%         
%     %------AVG OVER SEEDS------------------------------------------------------
%         for i1=1:NUM_SEEDS
%                 %ME
%                 str=strcat(str_main,'seed',int2str(i1),'\SpectralEnergy.txt');
%                 %ALEX
%                 %str=strcat(str_main,'SEDseed_avg.txt');
% 
%                 dummy = importdata(str,'\t');  
%                 %This figures out how many kpts are being read in by checking the length
%                 %of dummy and dividing by the number of kpt sections SAMPLE_RATE+1.  
%                 %we've been having SAMPLE_RATE=1024;
%                 NUM_KPTS = length(dummy(:,1)) / (SAMPLE_RATE+1);
% 
%                 %This takes the SED data in dummy and puts it in the object SED.freq.  The
%                 %data is stored SED.freq(1:1024,1:NUM_KPTS).  The kpt data is stored in
%                 %SED.kpt(1:NUM_KPTS,1:3) => [kx ky kz];
%                     for i2=1:NUM_KPTS
%                     SED.redkpt.sed(:,i2) = SED.redkpt.sed(:,i2) + dummy((i2-1)*(SAMPLE_RATE+1)+2:(i2)*(SAMPLE_RATE+1),1);
%                     SED.redkpt.kpt(i2,1:3) = dummy((i2-1)*(SAMPLE_RATE+1)+1,1:3);
%                     end
%         end
%         SED.redkpt.sed(:,:) = SED.redkpt.sed(:,:)/NUM_SEEDS;
% 
% 
%     %------FIND NUMBER IRR KPTS------------------------------------------------ 
% 
%         % Identify irreducible k-points, store in SED.irrkpt
%         % The first kpt is automatically a irreducible point.
%         SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
%         %str_write=strcat(str_main,'redkpt.dat');
%         %dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append');
%         for i1=2:NUM_KPTS
%             tempcnt = 0.0;
%             %dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
%             for i2=1:SED.irrkpt.numirr 
%                 if issym(SED.irrkpt.kpt(i2,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                      tempcnt = 1.0;
%                      degen = i2;
%                 end
%             end
%             if tempcnt==0.0
%             SED.irrkpt.numirr = SED.irrkpt.numirr +1;
%             SED.irrkpt.kpt(SED.irrkpt.numirr,1:3) = SED.redkpt.kpt(i1,1:3);
%             else
%             SED.redkpt.degen(i1)=1;
%             end  
%         end
%         
%         
% %--------------------OUTPUT KPTLIST----------------------------------------
% 
%         str_write=strcat(str_main,'redkpt.dat');
%         dlmwrite(str_write,SED.redkpt.kpt(1,1:3),'-append');
%         for kpt_cnt=1:SED.irrkpt.numirr
%                 %------FIND RED KPTS------------------------------------------------ 
%                     % Identify irreducible k-points, store in SED.irrkpt
%                     % The first kpt is automatically a irreducible point.
%                     SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
%                     for i1=1:NUM_KPTS
%                         if issym(SED.irrkpt.kpt(kpt_cnt,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                             dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
%                         end
%                     end
%         end
%                    
%         
%         pause
% 
%     %------AVG DEGEN KPTS------------------------------------------------------
%         SED.irrkpt.numdegen(1:SED.irrkpt.numirr)=0;
%         SED.irrkpt.sedavg(1:SAMPLE_RATE,1:SED.irrkpt.numirr)=0;
%         for i1=1:SED.irrkpt.numirr
%             for i2=1:NUM_KPTS
%                 if issym(SED.irrkpt.kpt(i1,1:3),SED.redkpt.kpt(i2,1:3)) == 1.0;
%                      SED.irrkpt.numdegen(i1) = SED.irrkpt.numdegen(i1) +1.0;
%                      SED.irrkpt.sedavg(:,i1) = SED.irrkpt.sedavg(:,i1) + SED.redkpt.sed(:,i2);
%                 end
%             end
%             SED.irrkpt.sedavg(:,i1) = SED.irrkpt.sedavg(:,i1)/SED.irrkpt.numdegen(i1);
%         end
%     %pause   
% 
% 
% %RE-DO LIST
%     REDO=0; 
%     AUTO=1;
% %8x m3 0.01    
%     %redo_list=  [7 10 14 15 18 19 20 21 23 24 26 27 28 30 33 34];
%     %redo_list=  [35];
% %6x m3 0.01
%     %redo_list = 1:SED.irrkpt.numirr;
% %Perfect 5K
%     %HLD_SCALING_PCT=1.0;
%     HLD_SCALING_PCT=1.017;
%     LC =5.2795e-10/sigma_Ar;        % For HLD peak calculation
% %Perfect 20K
%     %HLD_SCALING_PCT=1.0;
%     %HLD_SCALING_PCT=1.03;
%     %LC = 1.56328;        % For HLD peak calculation
% %Perfect 40K
%     %HLD_SCALING_PCT=1.04;
%     %LC = 1.57931;        % For HLD peak calculation
% %m3 0.01
% %    HLD_SCALING_PCT=0.8255;
% %    LC =5.27905e-10/sigma_Ar;        % For HLD peak calculation
% %m3 0.05
%     %HLD_SCALING_PCT=0.8;
% %m10 0.01
%     %HLD_SCALING_PCT=0.87;
%     
% %SET VOLUME FOR KAPPA    
%     VOLUME = (NUM_UCELLS*LC*sigma_Ar)^3;
%     
% %BUILD MAIN LIST OF IRRKPTS TO RUN OVER    
%     kpt_list = 1:SED.irrkpt.numirr;
%     RUN=1;
% 
% while RUN==1
% 
%     for kpt_cnt=1:length(kpt_list)
%         
%         %PRINT CURRENT KPT
%                     SED.irrkpt.kpt(kpt_list(kpt_cnt),1:3)
%         %CALCULATE HLD INFO
%         KPT_TOL = 10E-6; 
%         DEGEN_WIDTH=1.0;
%                     kx = SED.irrkpt.kpt(kpt_list(kpt_cnt),1)/NUM_UCELLS;
%                     ky = SED.irrkpt.kpt(kpt_list(kpt_cnt),2)/NUM_UCELLS;
%                     kz = SED.irrkpt.kpt(kpt_list(kpt_cnt),3)/NUM_UCELLS;
%                     
%                     if kx==0.5; kx = kx-KPT_TOL; end
%                     if ky==0.5; ky = ky-KPT_TOL; end
%                     if kz==0.5; kz = kz-KPT_TOL; end
%                     
%                     [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],[kx ky kz]);
%                     %Convert to rads/s
%                     SED.irrkpt.HLDfreq(1:length(HLDpks),kpt_list(kpt_cnt)) = HLDpks/tau_Ar;
%                     %Convert to m/s
%                     SED.irrkpt.HLDvel(1:length(HLDpks),1:3,kpt_list(kpt_cnt)) = vel*(sigma_Ar/tau_Ar);
%                     %Print HLD freqs
%                     ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT)
% 
%         %FIND # OF NON_DEGENERATE HLD PEAKS
%                     %[HLDndpks,numndpks] = ndpks_HLD(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
%                     
%         semilogy(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)),'.') 
%         
%         %SPECIFY LOCATION OF PEAKS AUTO OR MANUAL (REDO)        
%                     
%         if AUTO==1                       
%                     %FIND # OF NON_DEGENERATE PEAKS
%                     HLDdegenlist = group_peaks(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
%                     [HLDndpks,numndpks] = ndpks_HLD(ceil(HLDpks/tau_Ar/w_step*HLD_SCALING_PCT),DEGEN_WIDTH);
%                     loc=HLDndpks
%                     %Don't look for 0 freq at the gamma pt
%                     if SED.irrkpt.kpt(kpt_list(kpt_cnt),1)==0 & SED.irrkpt.kpt(kpt_list(kpt_cnt),2)==0 & SED.irrkpt.kpt(kpt_list(kpt_cnt),3)==0
%                         loctemp=loc; clear loc
%                         loc=(loctemp(2:length(loctemp)));
%                     end   
%                     
%         elseif REDO==1
%             semilogy(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)),'.')
%             number_pks = input('Enter the number of peaks: ');
%             number_pks = 12;
%             for i2=1:number_pks
%                 str_input = strcat('Enter peak  ',int2str(i2),' location: ');
%                 loc(i2) = input(str_input);
%             end
%                     %FIND # OF NON_DEGENERATE PEAKS
%                     [HLDndpks,numndpks] = ndpks_HLD(loc,DEGEN_WIDTH);
%                     %loc=HLDndpks
%                     HLDdegenlist = group_peaks(loc,DEGEN_WIDTH);
%                     loc = HLDndpks
%         end
%      
%         
% %Set freq to 1:sample_size
%                 w(:,1)=(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))));   
% %Run over all peak locations found
%                 str_func= '';
% %GROUP LOCATION WIDTH
%                 if REDO==1
%                     GROUP_LOCATION_WIDTH=input('Enter GROUP LOCATION WIDTH: ');
%                 else
%                     GROUP_LOCATION_WIDTH=30;
%                 end
%                 %GROUP PEAKS FOR SINGLE OR MULTIPLE FITS        
%                 group_loc = group_peaks(loc,GROUP_LOCATION_WIDTH);
% %BUILD LORENTZIAN FUNCTIONS SINGLE AND MULTIPLE     
%     %Pad the frequencies and lifetimes by the 0 freqs from HLD
%                 HLD_FREQ_TOL = 0.01;
%                 Ipad = find(abs(SED.irrkpt.HLDfreq(:,kpt_list(kpt_cnt)))<HLD_FREQ_TOL);
%                 SED.irrkpt.sedfreq(1:length(Ipad),kpt_list(kpt_cnt)) = SED.irrkpt.HLDfreq(1:length(Ipad),kpt_list(kpt_cnt));
%                 
% %Loop over grouped peaks and fit 
%         %Keep track of pk count w.r.t. HLD degen
%         pk_cnt=1;
%         for group_id = 1:group_loc(length(group_loc))               
%             Igroup = find(group_loc==group_id);
%             str_func= '';
%             for i3=1:length(Igroup)
%                 if length(Igroup)==1
%                 strtemp= strcat('(c(',int2str(i3),'))./((w - c(',int2str(i3+2),')).^2 + (c(',int2str(i3+1),')/2).^2)');
%                 str_func = strcat(str_func,strtemp);
%                 else
%                 strtemp= strcat(' + ( c(',int2str(3*(i3-1)),'))./((w - c(',int2str(3*(i3-1)+2),')).^2 + (c(',int2str(3*(i3-1)+1),')/2).^2)');
%                 str_func = strcat(str_func,strtemp);
%                 end  
%             end
% 
%         %LORENTZIAN FUNCTION FOR SINGLE AND MULTIPLE PEAKS            
%             lor_func = inline(str_func,'c','w');
% %PERCENT PAST PEAKS
%         if REDO==1
%         PT_PERC=input('Enter PERCENT PAST PEAKS: ');
%         else
%             if length(Igroup) == 1
%                 PT_PERC = 0.5;
%             else
%                 PT_PERC = 0.5;
%             end
%         end
% 
%             %Find wleft    
%                     [I,J] = find(PT_PERC*SED.irrkpt.sedavg(loc(Igroup(1)),kpt_list(kpt_cnt))>SED.irrkpt.sedavg(1:loc(Igroup(1)),kpt_list(kpt_cnt)) );
% %                     loc(Igroup(1))
% %                     SED.irrkpt.sedavg(loc(Igroup(1)),kpt_list(kpt_cnt))
% %                     PT_PERC*SED.irrkpt.sedavg(loc(Igroup(1)),kpt_list(kpt_cnt))
%                     wleft = w(I(length(I)));
%             %Find wright
%                     [I,J] = find(PT_PERC*SED.irrkpt.sedavg(loc(Igroup(length(Igroup))),kpt_list(kpt_cnt))>SED.irrkpt.sedavg(loc(Igroup(length(Igroup))):length(SED.irrkpt.sedavg),kpt_list(kpt_cnt)) );
%                     wright = w(loc(Igroup(length(Igroup)))+I(1)  );
%                     gamma_guess = wright-wleft; 
%                     buffer = ceil(gamma_guess*0.25);
%             %Build intial guess array
%                     for i3=1:length(Igroup)   
%                         if length(Igroup)==1
%                         c0(1+(i3-1)*3:i3*3) = [ SED.irrkpt.sedavg(loc(Igroup(i3)),kpt_list(kpt_cnt)), gamma_guess, w(loc(Igroup(i3))) ];
%                         else
%                         %make gamma_guess/length(Igroup) to make a guess that all
%                         %multiple lorentzians have 1/N (1/3 for N=3 example) the width
%                             if REDO==1  
%                                 gamma_guess = input('Input gamma guess: ');
%                             c0(1+(i3-1)*3:i3*3) = [ SED.irrkpt.sedavg(loc(Igroup(i3)),kpt_list(kpt_cnt)), gamma_guess, w(loc(Igroup(i3))) ];    
%                             else
%                                 gamma_guess = 5;
%                             c0(1+(i3-1)*3:i3*3) = [ SED.irrkpt.sedavg(loc(Igroup(i3)),kpt_list(kpt_cnt)), gamma_guess, w(loc(Igroup(i3))) ];    
%                             end
%                         end
%                     end 
%                     
%             %FIT THE LORENTZIAN(S)
%             
%             [c_fit,r,j] = nlinfit(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt)),lor_func,c0);
% 
%             %Store separate liftimes and frequencies for single and MULTIPLE FITS
%             %Hold on to plot multiple peaks
%             hold on
%                     Igroup = find(group_loc==group_id);
%                         for i3=1:length(Igroup)  
%                             %if length(Ipad)==0
%                                 Idegen = find(HLDdegenlist==pk_cnt);
%                             %else
%                             %    Idegen = find(HLDdegenlist==pk_cnt+1);
%                             %end
%                             center=c_fit(3*i3)*w_step;
%                             lifetime=1/c_fit(3*i3-1)/w_step;
%                             %Store in rads/ps
%                             SED.irrkpt.sedfreq( Idegen ,kpt_list(kpt_cnt)) = center;
%                             %Store in ps
%                             SED.irrkpt.life( Idegen ,kpt_list(kpt_cnt)) = lifetime;
%                             %Keep track of pk position taking
%                             %into account degen w.r.t. HLD
%                             pk_cnt = pk_cnt +1;
%                         end
%             %Plot each fit, single and multiple
%                         plot(w(wleft:wright),SED.irrkpt.sedavg(wleft:wright,kpt_list(kpt_cnt)),w(wleft:wright),lor_func(c_fit,w(wleft:wright)))
%                         axis([0 1.0*length(w) 0.0001*max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))) max(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)))]);
%         end
%                 disp(sprintf('%s', 'SED FREQ:'));
%                 SED.irrkpt.sedfreq(:,kpt_list(kpt_cnt))
%                 disp(sprintf('%s', 'SED LIFE:'));
%                 SED.irrkpt.life(:,kpt_list(kpt_cnt))
%                 loc
%         %1=yes, 0=no
%         SED.irrkpt.flag(kpt_list(kpt_cnt)) = input('Flag?: ');
%         %Clear current plot
%         clf
%         hold off
%     clear I J buffer wleft wright c0 pks loc str_func lifetime center
%     end
%     
% %Check for any flags, if so redo those kpts manually
%     for i1=1:length(SED.irrkpt.flag(:))
%         Iflag = find(SED.irrkpt.flag==1);
%         if length(Iflag)==0;
%             RUN=0;
%         else
%             RUN=1; AUTO=0; REDO=1; kpt_list = Iflag;
%             %disp(sprintf('%s', 'REDO:'));
%         end
%     end
%     
% end
% 
% 
% 
% %--------------------OUTPUT DATA-------------------------------------------  
% 
%     str_loop = input('Write data to file?: ');
%     str_loop=1;
%     str_write=strcat(str_main,'SED_data.dat');
%     str_write_life=strcat(str_main,'SED_life.dat');
%     if str_loop ==1
%         NUM_MODES = length(SED.irrkpt.HLDfreq(:,1));
%         for kpt_cnt=1:SED.irrkpt.numirr
%                 %------FIND RED KPTS------------------------------------------------ 
%                     % Identify irreducible k-points, store in SED.irrkpt
%                     % The first kpt is automatically a irreducible point.
%                     SED.irrkpt.kpt(1,1:3) = SED.redkpt.kpt(1,1:3); SED.irrkpt.numdegen(1)=0; SED.irrkpt.numirr=1;
%                     for i1=1:NUM_KPTS
%                         if issym(SED.irrkpt.kpt(kpt_cnt,1:3),SED.redkpt.kpt(i1,1:3)) == 1.0;
%                             %Write the wavevector first
%                             dlmwrite(str_write,SED.redkpt.kpt(i1,1:3),'-append');
%                             %Write the number of degenerate kpts for mult factor
%                             %dlmwrite(str,SED.irrkpt.numdegen(kpt_list(kpt_cnt)),'-append')
%                             %;
%                             %CALCULATE HLD DATA FOR RED KPT
%                                 kx = SED.redkpt.kpt(i1,1)/NUM_UCELLS;
%                                 ky = SED.redkpt.kpt(i1,2)/NUM_UCELLS;
%                                 kz = SED.redkpt.kpt(i1,3)/NUM_UCELLS;
%                                 if kx==0.5; kx = kx-KPT_TOL; end
%                                 if ky==0.5; ky = ky-KPT_TOL; end
%                                 if kz==0.5; kz = kz-KPT_TOL; end
% 
%                                 [HLDpks,eig,vel] = LDAr_FCC([LC LC LC],[kx ky kz]);
%                                 %Convert to rads/s
%                                 HLDpks = HLDpks/tau_Ar;
%                                 %Convert to m/s
%                                 vel = vel*(sigma_Ar/tau_Ar);
% 
%                                     for i2=1:NUM_MODES
%                                         whld = HLDpks(i2);
%                                         wsed = SED.irrkpt.sedfreq(i2,kpt_cnt);
%                                         lifetau = SED.irrkpt.life(i2,kpt_cnt);
%                                         velx = vel(i2,1);
%                                         vely = vel(i2,2);
%                                         velz = vel(i2,3);
%                                         svelx = velx*(wsed/whld);
%                                         svely = vely*(wsed/whld);
%                                         svelz = velz*(wsed/whld);
%                                         kappax = (kb/VOLUME)*lifetau*(svelx^2);
%                                         kappay = (kb/VOLUME)*lifetau*(svely^2);
%                                         kappaz = (kb/VOLUME)*lifetau*(svelz^2);
% 
% 
%                                         SED.redkpt.HLDfreq(i2,i1) = whld;
%                                         SED.redkpt.sedfreq(i2,i1) = wsed;
%                                         SED.redkpt.HLDfreq(i2,i1) = lifetau;
%                                         SED.redkpt.HLDvel(i2,1,i1) = velx;
%                                         SED.redkpt.HLDvel(i2,2,i1) = vely;
%                                         SED.redkpt.HLDvel(i2,3,i1) = velz;
%                                         SED.redkpt.svel(i2,1,i1) = svelx;
%                                         SED.redkpt.svel(i2,2,i1) = svely;
%                                         SED.redkpt.svel(i2,3,i1) = svelz;
%                                         SED.redkpt.kappa(i2,1,i1) = kappax;
%                                         SED.redkpt.kappa(i2,2,i1) = kappay;
%                                         SED.redkpt.kappa(i2,3,i1) = kappaz;
% 
%                                     output_format = [whld wsed lifetau velx vely velz svelx svely svelz,kappax,kappay,kappaz];
%                                     output_life = [whld wsed lifetau velx vely velz svelx svely svelz,kappax,kappay,kappaz];
%                                     dlmwrite(str_write,output_format,'-append');
%                                     dlmwrite(str_write_life,output_life,'-append');
% %                                 if wsed<(w_max*10^12) && wsed>(w_step) && lifetau<(10^-10) && lifetau>(10^-13)
% %                                 output_format = [whld wsed lifetau velx vely velz svelx svely svelz]; 
% %                                 else
% %                                 output_format = [0 0 0 0 0 0 0 0 0]; 
%                                     end
%                         end
%                     end
%         end
%     end
% %END MAIN FUNCTION    
% end
% 
% 
% 
% %-------------FUNCTIONS----------------------------------------------------
% 
% 
% function istrue=issym(kpt1,kpt2)
% istrue=0.0;
% istrue1=0.0; istrue2=0.0;
%         for i2=-1:2:1
%             for i3=-1:2:1
%                 for i4=-1:2:1
%                     
%                     temp(1) = i2*kpt2(1); temp(2) = i3*kpt2(2); temp(3) = i4*kpt2(3);
% 
%                     if kpt1(1) == temp(1) && kpt1(2) == temp(2) && kpt1(3) == temp(3)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(3) && kpt1(2) == temp(1) && kpt1(3) == temp(2)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(2) && kpt1(2) == temp(3) && kpt1(3) == temp(1)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(1) && kpt1(2) == temp(3) && kpt1(3) == temp(2)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(3) && kpt1(2) == temp(2) && kpt1(3) == temp(1)
%                         istrue=1.0;
%                     elseif kpt1(1) == temp(2) && kpt1(2) == temp(1) && kpt1(3) == temp(3)
%                         istrue=1.0;
%                     end
%                 end
%             end
%         end
% end
% 
% 
%      
% function group_loc = group_peaks(loc,groupdis)
%     sup_peak_dist = groupdis;
%     clear fitpks
%     for i=1:length(loc)-1
%         dist(i) = loc(i+1) - loc(i);
%     end
% 
%     id = 1;
%     for i=1:length(loc) - 1
%         fitpks(i) = id;
%         if (dist(i) > sup_peak_dist)
%             id = id + 1;
%         end
%     end
%     fitpks(i + 1) = id;
%     group_loc = fitpks; 
% end
% 
% function [HLDndpks,numndpks] = ndpks_HLD(HLDpks,DEGEN_WIDTH)
%     numndpks = 1;
%     HLDndpks(1) = HLDpks(1);
%     for j=2:length(HLDpks) %should be equal to 12
%         degencount = 0;
%         for k=1:numndpks
%             if abs(HLDpks(j)-HLDndpks(k)) < DEGEN_WIDTH
%                 degencount = 1;
%             end
%         end
%         if degencount == 0
%             numndpks = numndpks +1;
%             HLDndpks(numndpks) = HLDpks(j);
%         end
%     end
% end
% 

