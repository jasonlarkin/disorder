%%%%%1D Chain - check on effective mass correction
%%%%%Au--C12H25SH
%%%%%All in SI units
%%%%%kg/m/s
clear;

%%%%%NanoCrystals
%%%%%Assume spherical - correction for Dia<2nm?
    NUnits = 3; %Number of different atoms in a basis should be >2
    
%%%%%Uniform Distribution
    %RangeD_Au = [1e-9,8e-9];
    %D_Au = RangeD_Au(1)+ rand(NUnits,1)*(RangeD_Au(2)-RangeD_Au(1));
    
%%%%%Normal Distribution
    D_AuMean = 2.00E-09; %%%Comment out the one for uniform distribution
    D_AuSD = 0.1*D_AuMean; %Dispersion = 0.08 - coefficient of variation?
    D_Au = D_AuMean + randn(NUnits,1)*(D_AuSD);

    D_AuMean = 0;
for iD=1:1:NUnits
    D_AuMean = D_AuMean + D_Au(iD);%(RangeD_Au(1)+RangeD_Au(2))/2;%%%changed 12 Oct
end
D_AuMean = D_AuMean/NUnits;

    Density_Au = 19263.66;
    Vol_Au(NUnits) = 0;
    M_Au(NUnits) = 0;
for i1=1:1:NUnits
    Vol_Au(i1) = 4/3*pi*(D_Au(i1)/2)^3;
    M_Au(i1) = Density_Au*Vol_Au(i1);
end

%%%%%%%Ligands
    M_Au_atom = 196.96655e-3/6.022e23;
    M_Org = 6.72E-25;
    Len_Org = 1.7e-9;
    N_OrgPerAu = 0.5;%%%No of ligands in proportion per Au atom
    N_OrgInteract = 0.05;%%%No of ligands assumed to interact forming bonds between 2 NC
    Ntotal(i1) = 0;
    Nattach(i1) = 0;
    Nfree(i1) = 0;    
for i1=1:1:NUnits
    Ntotal(i1) = round(N_OrgPerAu*(M_Au(i1)/M_Au_atom));%mass of a Au Nanocrystal/mass of a Au atom
    Nattach(i1) = round(N_OrgInteract*Ntotal(i1));
    Nfree(i1) = Ntotal(i1) - Nattach(i1);
end
NtotalM = round(0.5*(4/3*pi*((D_AuMean/2)^3)*Density_Au/M_Au_atom));
NattachM = round(N_OrgInteract*NtotalM);
    
%%%%%%%Parameters from nanocrystals and ligands
M_NC(i1)=0;
Dist_2AuC(i1)=0;
SpringC(i1)=0;
for i1=1:1:NUnits
    %M_NC(i1) = M_Au(i1)+1/3*M_Org;
    M_NC(i1) = M_Au(i1)+Nfree(i1)*M_Org+1/3*Nattach(i1)*M_Org;
    YoungE = 4e9; %constant?
    
    %%%changed 12 Oct Start
    if(i1==NUnits)
        Dist_2AuC(i1) = D_Au(i1)/2 + D_Au(1)/2 + 2*Len_Org;
    else
        Dist_2AuC(i1) = D_Au(i1)/2 + D_Au(i1+1)/2 + 2*Len_Org;
    end
    %%%changed 12 Oct End
    
    SpringC(i1) = YoungE*(D_Au(i1)^2)/(Dist_2AuC(i1)); %assume vol constant - might be problematic
end
M_NCMean = mean(M_NC);%4/3*pi*((D_AuMean/2)^3)*Density_Au + (NtotalM-NattachM)*M_Org + 1/3*NattachM*M_Org;
Dist_2AuCMean = D_AuMean + 2*Len_Org;
%SpringCMean = YoungE*(D_AuMean^2)/(Dist_2AuCMean); %assume vol constant
SpringCMean = YoungE*(mean(D_Au.^2))/(Dist_2AuCMean); %assume vol constant

a=0;
%%%%%1st Brillouin Zone !!!!!!!!! Using the mean value of the diameters
%%%%%population distribution
%%%actual dist%%%changed 12 Oct Start
for ia=1:1:NUnits
    a = a + Dist_2AuC(ia); %%%mean(Dist_2AuCMean)*(NUnits);
end
%%%changed 12 Oct End
aMean = mean(Dist_2AuCMean);

%%%%%Governing Eqn
sampleX = 300; %%%Number of sampling points in the Brillouin Zone
sampleXD2 = 300*2; %%%Number of sampling points for central difference
GovernM(NUnits,NUnits)= 0;
%K=eps:(pi/a/No_unit/n):(pi/a/No_unit);
K = 0:(pi/a/sampleX):(pi/a);
KD2 = 0:(pi/a/sampleXD2):(pi/a);
KMean = 0:(pi/aMean/(sampleX*NUnits)):(pi/aMean);

%%%%%For storing w
omega(NUnits,sampleX+1)=0;
omegaNS(NUnits,sampleX+1)=0;
omegaRight(2,NUnits*(sampleX+1))=0;
%For storing instantaneous group velocity
domega_dk(NUnits,sampleX+1)=0;
domega_dkC(NUnits,sampleX+1)=0;
domega_dkRight(2,NUnits*(sampleX+1))=0;

%%%%%For storing wMean
omegaMean(sampleX*NUnits+1)=0;
%%%%%For storing instantaneous group velocity for mean
domega_dkMean(sampleX*NUnits+1)=0;

%%%%%For storing wMean
%%%omegaMean2(2,sampleX*2+1)=0;
%%%%%For storing instantaneous group velocity for mean
%%%domega_dkMean2(2,sampleX*2)=0;

%%%%%Generate Matrix -> eigenvalues -> w using different K
for i2=1:1:NUnits 
    for p=1:1:NUnits
        if(i2==p)
            if (p-1==0)
                GovernM(i2,p)= (SpringC(p)+SpringC(NUnits))/M_NC(i2);
            else
                GovernM(i2,p)= (SpringC(p)+SpringC(p-1))/M_NC(i2);
            end
        %elseif (i2==1 && p==NUnits)
            %GovernM(i2,p)= -(SpringC(NUnits)+(SpringC(NUnits)*exp(-1i*K(k1)*a)));
            %GovernM(i2,p)= -((SpringC(NUnits)*exp(-1i*K(k1)*a)));
        %elseif (i2==NUnits && p==1)
            %GovernM(i2,p)= -(SpringC(NUnits)+(SpringC(NUnits)*exp(1i*K(k1)*a)));
            %GovernM(i2,p)= -((SpringC(NUnits)*exp(1i*K(k1)*a)));
        elseif (p==i2+1)
            GovernM(i2,p)= -SpringC(i2)/M_NC(i2);
        elseif (p==i2-1)
            GovernM(i2,p)= -SpringC(p)/M_NC(i2);
        end
    end   
end
    
for k1=1:1:sampleX+1
    %%%%%%%%Generate different matrices with changing exp(+-iKa) thru different K
    GovernM(1,NUnits)= -((SpringC(NUnits)*exp(-1i*K(k1)*a))/M_NC(1));
    GovernM(NUnits,1)= -((SpringC(NUnits)*exp(1i*K(k1)*a))/M_NC(NUnits));        
    %%%%%%%Solve for W^2
    [v,lambda]=eig(GovernM);%,NUnits);%%Remember to change the bottom 2 in the central difference loop
    for i3=1:1:NUnits
        %%%%%%%X-value for Omega-unfolded
        omegaNS(i3,k1) = sqrt((abs(lambda(i3,i3))))/(2*pi);
        if(i3==1)
            omegaRight(1,k1+(i3-1)*(sampleX+1))= K(k1);
        else
            omegaRight(1,k1+(i3-1)*(sampleX+1))= K(k1)+(i3-1)*K(sampleX+1);
        end
    end
            omega(:,k1) = sort(omegaNS(:,k1));
end

%omega = sortrows(omega);

%%%%Unfolded Omega
for i3=1:1:NUnits
    for k1=1:1:sampleX+1
        if(rem(i3,2)~=0)
            omegaRight(2,k1+(i3-1)*(sampleX+1)) = omega(i3,k1);
        else
            omegaRight(2,k1+(i3-1)*(sampleX+1)) = omega(i3,sampleX+1-k1+1);
        end
   end
end

%%%%Calculate domega/dk
for k1=1:1:sampleX+1
    for i3=1:1:NUnits
        if(k1>2)%(k1>1)
            if(rem(i3,2)~=0)
                %%%%%using forward difference
                domega_dk(i3,k1-1) = (omega(i3,k1)-omega(i3,k1-1))/(K(k1)-K(k1-1));
                %%%%%using central difference 12 Oct
                %domega_dk(i3,k1-1) = (omega(i3,k1)-omega(i3,k1-2))/(K(k1)-K(k1-2));
                
            else
                %%%%%using forward difference
                domega_dk(i3,k1-1) = -(omega(i3,k1)-omega(i3,k1-1))/(K(k1)-K(k1-1));
                %%%%%using central difference 12 Oct
                %domega_dk(i3,k1-1) = -(omega(i3,k1)-omega(i3,k1-2))/(K(k1)-K(k1-2));
            end
        end
    end
end

%%%%Calculate domega/dk using more accurate central differences
for k1=2:2:sampleXD2-2
    %%%%%%%%Generate different matrices with changing exp(+-iKa) thru different K
    GovernM(1,NUnits)= -((SpringC(NUnits)*exp(-1i*KD2(k1)*a))/M_NC(1));
    GovernM(NUnits,1)= -((SpringC(NUnits)*exp(1i*KD2(k1)*a))/M_NC(NUnits));        
    %%%%%%%Solve for W^2
    [v1,lambda1]=eig(GovernM);%,NUnits);%%Remember change if change the top for Omega
    
    %%%%%%%%Generate different matrices with changing exp(+-iKa) thru different K
    GovernM(1,NUnits)= -((SpringC(NUnits)*exp(-1i*KD2(k1+2)*a))/M_NC(1));
    GovernM(NUnits,1)= -((SpringC(NUnits)*exp(1i*KD2(k1+2)*a))/M_NC(NUnits));        
    %%%%%%%Solve for W^2
    [v2,lambda2]=eig(GovernM);%,NUnits);%%Remember change if change the top for Omega    
                
    for i3=1:1:NUnits
        %%%%%%%Solve for W ??? Can lambda be negative => eignvalue be -ve??
        lambda1(i3,i3) = sqrt((abs(lambda1(i3,i3))))/(2*pi);
        lambda2(i3,i3) = sqrt((abs(lambda2(i3,i3))))/(2*pi);
    end
   
    lambda1 = sort(lambda1);
    lambda1 = sort(lambda1);
    lambda1 = lambda1(NUnits,:);
    lambda1 = sort(lambda1);
    lambda1 = diag(lambda1);
    
    lambda2 = sort(lambda2);
    lambda2 = sort(lambda2);
    lambda2 = lambda2(NUnits,:);
    lambda2 = sort(lambda2);
    lambda2 = diag(lambda2);
    
        for i3=1:1:NUnits
            %if(rem(i3,2)~=0)
                %%%%%using central difference 12 Oct
                domega_dkC(i3,(k1+2)/2) = abs((lambda2(i3,i3)-lambda1(i3,i3))/(KD2(k1+2)-KD2(k1)));                
            %else
                %%%%%using central difference 12 Oct
                %domega_dkC(i3,(k1+2)/2) = (lambda2(i3,i3)-lambda1(i3,i3))/(KD2(k1+2)-KD2(k1));
            %end
        end
end

%domega_dkC = sortrows(domega_dkC);

for k1=1:1:sampleX+1
    for i3=1:1:NUnits
        %%%%%%%X-value for the domega_dk
        if(i3==1)
            domega_dkRight(1,k1+(i3-1)*(sampleX+1))= K(k1);
        else
            domega_dkRight(1,k1+(i3-1)*(sampleX+1))= K(k1)+(i3-1)*K(sampleX+1);
        end
    end
end
    
for k1=1:1:sampleX+1
   for i3=1:1:NUnits
        if(rem(i3,2)~=0)
            domega_dkRight(2,k1+(i3-1)*(sampleX+1)) = domega_dkC(i3,k1);
        else
            domega_dkRight(2,k1+(i3-1)*(sampleX+1)) = domega_dkC(i3,sampleX+1-k1+1);%NUnits-i3+1
        end
   end
end

%%%%%Dispersion Relationship is it is monodispersed at the mean sample
%%%%%diameter
for k1=1:1:sampleX*NUnits+1
    %%%%%%%Solve for Wmean and domega_dkMean
    omegaMean(k1)= sqrt(4*SpringCMean/M_NCMean*(sin(0.5*KMean(k1)*aMean))^2)/(2*pi);
    if(k1>2)
        domega_dkMean(k1-1) =  (omegaMean(k1)-omegaMean(k1-2))/((KMean(k1)-KMean(k1-2)));
    end
end

%%%for k1=1:1:sampleX*2+1
    %%%%%%%Solve for Wmean2 and domega_dkMean2
    %%%omegaMean2(1,k1)= (2*SpringCMean/M_NCMean + 1/M_NCMean^2*sqrt((2*SpringCMean*M_NCMean)^2 - 2*(SpringCMean*M_NCMean)^2*(1-cos(KMean(k1)/2*aMean*2))))/(2*pi);
    %%%omegaMean2(2,k1)= (2*SpringCMean/M_NCMean - 1/M_NCMean^2*sqrt((2*SpringCMean*M_NCMean)^2 - 2*(SpringCMean*M_NCMean)^2*(1-cos(KMean(k1)/2*aMean*2))))/(2*pi);
    %%%if(k1~=1)
        %%%domega_dkMean2(1,k1-1) =  (omegaMean(1,k1)-omegaMean(1,k1-1))/((KMean(k1)-KMean(k1-1)));
        %%%domega_dkMean2(2,k1-1) =  (omegaMean(2,k1)-omegaMean(2,k1-1))/((KMean(k1)-KMean(k1-1)));
    %%%end
%%%end

figure(1);
%%%%%%%% w vs K
hold;
get(gca,'ColorOrder');
for i3=1:1:NUnits
    %for i4=1:1:sampleX
        %plot(K(i4),omega(i3,i4));
    %end
    plot(K,omega(i3,:));
end
hold;

figure(2);
%%%%%%%% w vs K - unfolded
hold;
get(gca,'ColorOrder');

%%%%Use Matlab Plotting of array - has line in the jump region
%plot(omegaRight(1,:),omegaRight(2,:));

for i3=1:1:NUnits*(sampleX+1)
    %plot point by point not as connected line
    plot(omegaRight(1,i3),omegaRight(2,i3));   
end

for i3=1:1:NUnits
    %if(rem(i3,2)~=0)
    %    for i4=1:1:sampleX+1
    %        plot(K(i4)+(i3-1)*pi/a,omega(i3,i4));
    %    end
    %else
    %    for i4=1:1:sampleX+1
    %        plot(K(i4)+(i3-1)*pi/a,omega(i3,sampleX-i4+2));
    %    end
    %end
    
    for i4=1:1:sampleX+1
        plot(K(i4)+(i3-1)*pi/a,omegaMean(i4+(i3-1)*(sampleX)),'red');
        %plot(K(i4)+(i3-1)*pi/a,omegaMean(i4+(i3-1)*(sampleX)),'red');
    end
end
    
hold;

figure(3);
%%%%%%%% dw/dk vs K 
hold;
get(gca,'ColorOrder');
for i3=1:1:NUnits
    for i4=1:1:sampleX
        plot(K(i4),domega_dk(i3,i4));
        plot(K(i4)+(i3-1)*pi/a,domega_dkMean(i4+(i3-1)*(sampleX)),'red');
    end
end
    %%%%for i4=1:1:sampleX
    %%%%    plot(KMean(i4)*(NUnits),domega_dkMean(i4),'red');        
    %%%%end
hold;

figure(4);
%%%%%%%% dw/dk vs K - unfolded
hold;
get(gca,'ColorOrder');

for i3=1:1:NUnits*(sampleX+1)
    %plot point by point not as connected line
    plot(domega_dkRight(1,i3),domega_dkRight(2,i3));   
end

%for i3=1:1:NUnits
%    if(rem(i3,2)~=0)
%        for i4=1:1:sampleX+1
%            plot(K(i4)+(i3-1)*pi/a,domega_dk(i3,i4));
%        end
%    else
%        for i4=1:1:sampleX+1
%            %plot(K(i4)+(i3-1)*pi/a,domega_dk(i3,sampleX-i4+1));
%            plot(K(i4)+(i3-1)*pi/a,domega_dk(i3,sampleX+1-i4+1));
%        end 
%    end
%end

for i3=1:1:NUnits
    for i4=1:1:sampleX
        plot(KMean(i4)+(i3-1)*pi/a,domega_dkMean(i4+(i3-1)*(sampleX)),'red');
    end
end
hold;
