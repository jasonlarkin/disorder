
%LJ Potential and Material Parameters
epsilon_Ar = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
sigma_Ar = 3.4E-10;                 %Angstroms 3.4E-10 meters
a_0 = 5.2686E-10/sigma_Ar;            %the lattice constant of Ar: http://www.infoplease.com/periodictable.php?id=18
mass_Ar = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
%mass_Ar = mass_Ar/mass_Ar;
tau_Ar = sqrt((mass_Ar*(sigma_Ar^2))/epsilon_Ar);
kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
hbar = 1.054E-34;                %J/s

w_step = 2*pi/(2^16)/(5E-15);
w_max = 2*pi/(2^6)/(5E-15);

NUM_UCELLS = 6;
NUM_MODES = 12;
dk = 10E-6;
%LC = 5.27905e-10/sigma_Ar;
LC=1.563;

%ix_list=[4 3 2 1 0 -1 -2 -3];
%iy_list=[4 3 2 1 0 -1 -2 -3];
%iz_list=[4 3 2 1 0 -1 -2 -3];

str_main=strcat('E:\CMU\work\Phonons\SED\Joe_Data\');

kpts = dlmread('E:\CMU\work\Phonons\SED\Joe_Data\redkpt.dat');

str_write=strcat(str_main,'Vg_5K_8x.dat');

for i1=1:length(kpts)
            k(1) = kpts(i1,1)/NUM_UCELLS; k(2) = kpts(i1,2)/NUM_UCELLS; k(3) = kpts(i1,3)/NUM_UCELLS;
            k
            for idim = 1:3
                if k(idim)==0.5 
                    [HLDpks1] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    k(idim) = k(idim) - dk;
                    [HLDpks2] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    VEL(:,idim) = (((HLDpks1/tau_Ar)-(HLDpks2/tau_Ar))/(dk/sigma_Ar))/4;
                    k(idim) = k(idim) + dk;
                elseif k(idim)==-0.5
                    [HLDpks1] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    k(idim) = k(idim) + dk;
                    [HLDpks2] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    VEL(:,idim) = (((HLDpks1/tau_Ar)-(HLDpks2/tau_Ar))/(dk/sigma_Ar))/4;
                    k(idim) = k(idim) - dk;
                elseif k(idim)==0.0
                    k(idim) = k(idim) + dk;
                    [HLDpks1] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    k(idim) = k(idim) - dk;
                    [HLDpks2] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    VEL(:,idim) = (((HLDpks1/tau_Ar)-(HLDpks2/tau_Ar))/(dk/sigma_Ar))/4;
                else
                    k(idim) = k(idim) + dk;
                    [HLDpks1] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    k(idim) = k(idim) - 2*dk;
                    [HLDpks2] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    VEL(:,idim) = (((HLDpks1/tau_Ar)-(HLDpks2/tau_Ar))/(2*dk/sigma_Ar))/4;
                    k(idim) = k(idim) + dk;
                end
            end
            
                for i1 = 1:NUM_MODES
                    output = [VEL(i1,1) VEL(i1,2) VEL(i1,3)];
                    dlmwrite(str_write,output,'-append');
                end

end

