
%--------------------------------------------------------------------------
%-------------------------INPUT--------------------------------------------
%--------------------------------------------------------------------------
constant.kb = 1.3806E-23;           %aJ/k (1.3806E-23 J/K)
constant.hbar = 1.054E-34;          %J/s
constant.i = sqrt(-1);
constant.c = 29979245800.00019;     %cm/s
constant.s2ps = 10E11;
constant.ang2m = 10E-11;
constant.mass_Si = 28.0855/(6.022 * 10^23)/(10^3);   %kg
constant.eV2J = 1.60217646E-19;
constant.Ang2m = 1E10;
constant.avo = 6.023E23;

%--------------------------------------------------------------------------
            str.main=strcat('D:\CMU\work\Si\phonopy\CO2\vasp\');
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
            format long
%--------------------------------------------------------------------------


%KPTLIST: Load kpt list
            %Linux
            %str.read=strcat(str.main,'kptlist.dat');
            %LD.kptlist(:,1:3) = load(str.read); [LD.NUM_KPTS, blank] = size(LD.kptlist(:,1:3));

%Define number of atoms
            %LD.NUM_ATOMS = x0(1,1); LD.NUM_ATOMS_UCELL = x0(1,2); LD.NUM_UCELL_COPIES=LD.NUM_ATOMS/LD.NUM_ATOMS_UCELL; 
	     %LD.NUM_MODES = 3*LD.NUM_ATOMS_UCELL;
%Define box size and conventional cell lattice parameters
           % LD.L(1) = x0(1,3); LD.L(2) = x0(1,4); LD.L(3) = x0(1,5); LD.alat = LD.L/((LD.NUM_ATOMS/8)^(1/3));
            %LD.NUM_SUPERCELL = LD.L/LD.alat;
            %chop off first line of input structure
            %LD.x0 = x0(2:length(x0),:);
            %LD.m = x0(:,2)*constant.mass_Si;
%Define finite difference increment
		%LD.dk = 0.000001;
        
        
        
%INITIAL POSITIONS: Set initial positions for id matching
        %Linux
%         str.read=strcat(str.main,'SPOSCAR');
%         LD.SPOSCAR = importdata(str.read);
%         LD.SPOSCAR.LATVEC = LD.SPOSCAR(1:3,:);
%         LD.SPOSCAR.XS(:,1) = LD.SPOSCAR(4:length(LD.SPOSCAR),1)*LD.SPOSCAR.LATVEC(1,1) + LD.SPOSCAR(4:length(LD.SPOSCAR),2)*LD.SPOSCAR.LATVEC(2,1)+ LD.SPOSCAR(4:length(LD.SPOSCAR),3)*LD.SPOSCAR.LATVEC(3,1) ;
%         LD.SPOSCAR.XS(:,2) = LD.SPOSCAR(4:length(LD.SPOSCAR),1)*LD.SPOSCAR.LATVEC(1,2) + LD.SPOSCAR(4:length(LD.SPOSCAR),2)*LD.SPOSCAR.LATVEC(2,2)+ LD.SPOSCAR(4:length(LD.SPOSCAR),3)*LD.SPOSCAR.LATVEC(3,2) ;
%         LD.SPOSCAR.XS(:,3) = LD.SPOSCAR(4:length(LD.SPOSCAR),1)*LD.SPOSCAR.LATVEC(1,3) + LD.SPOSCAR(4:length(LD.SPOSCAR),2)*LD.SPOSCAR.LATVEC(2,3)+ LD.SPOSCAR(4:length(LD.SPOSCAR),3)*LD.SPOSCAR.LATVEC(3,3) ;


        
%READ FORCE CONSTANTS
        %Linux
        str.read=strcat(str.main,'FORCE_CONSTANTS');
        dummy = dlmread(str.read);    
        id_cnt=1;
        for i1=1:4:length(dummy)
            FC.id(id_cnt,1) = dummy( i1,1);         %i
            FC.id(id_cnt,2) = dummy( i1,2);         %j
            FC.FC(id_cnt,1:3,1:3) = dummy( (i1+1) : (i1+3),1:3)*(constant.eV2J)*(constant.Ang2m^2)  ;      %eV/Ang^2
        id_cnt=id_cnt+1;
        end

        pause
        
%BUILD DYNAM MATRIX

        LD.NUM_ATOMS_UCELL = 3;
        kappa(1:3) = 0.0;
        LD.alat(1:3) = 1.0;
        
        
        FC.vec(1:length(FC.id),1:3) = 0.0; 
        
        LD.m(1) = (12.0107/1000)/constant.avo;      %C
        LD.m(2:3) = (15.9994/1000)/constant.avo;    %O
            
     DYNAM(1:3*LD.NUM_ATOMS_UCELL,1:3*LD.NUM_ATOMS_UCELL) = 0.0;
        for i1=1:length(FC.id)  
            dot_product = (pi)*( kappa(1)*FC.vec(i1,1)/LD.alat(1) + kappa(2)*FC.vec(i1,2)/LD.alat(2) + kappa(3)*FC.vec(i1,3)/LD.alat(3) ); 
            invmexpikR = exp(sqrt(-1)*dot_product)*(1/sqrt(LD.m(FC.id(i1,1))*LD.m(FC.id(i1,1)))); 
            if FC.id(i1,1)==FC.id(i1,2)
                %[I,J]=find(FC.id(i1,1) == FC.id(:,2));
                %Phi(1:3,1:3) = invmexpikR*sum(FC.FC(I,1:3,1:3),1);
                Phi(1:3,1:3) = invmexpikR*FC.FC(i1,1:3,1:3);
            else
                Phi(1:3,1:3) = invmexpikR*FC.FC(i1,1:3,1:3);
            end
            ii = (FC.id(i1,1)-1)*3 + 1 : (FC.id(i1,1))*3;
            jj = (FC.id(i1,2)-1)*3 + 1 : (FC.id(i1,2))*3;
        DYNAM( ii , jj  ) = DYNAM(  ii , jj ) + Phi ; %real(Phi)+sqrt(-1)*imag(Phi);
        end
        
%5) Calculate freqs, eigV, and v_g
            [eigV,W2]=eig(DYNAM); W=sqrt(abs(W2));
%CREATE FREQ VECTOR          
            for i=1:length(W)
            w(i) = W(i,i);
            end   
%SORT FREQS AND EIGV
              [freq,I]=sort(abs(w));
              eigVsorted(1:3*LD.NUM_ATOMS_UCELL,1:3*LD.NUM_ATOMS_UCELL) = eigV(:,I); 
freq/(2*pi*constant.c)
              
%             
% %OUTPUT
%         %Windoze
%         str_write_vec=strcat(str_main,'1atom\ME\eigvec.dat');
%         str_write_freq=strcat(str_main,'1atom\ME\freq.dat');
%         str_write_vel=strcat(str_main,'1atom\ME\vel.dat');
%         %Linux
%         %str_write=strcat(str_main,'1atom/SED_',int2str(iseed),'.txt');          
% 
%             for ikpt = 1:NUM_KPTS
%                 SED.kptlist(ikpt,1:3)
%                 [freq,eigVsorted,velocity] = PHON_Si_LD(alat,SED.kptlist(ikpt,1:3));
%                     dlmwrite(str_write_vec,eigVsorted,'-append','delimiter',' ');
%                     dlmwrite(str_write_freq,freq,'-append','delimiter',' ');
%                     dlmwrite(str_write_vel,velocity,'-append','delimiter',' ');
%             end
