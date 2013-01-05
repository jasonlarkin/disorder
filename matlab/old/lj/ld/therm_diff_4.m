function [Diff,S] = therm_diff_4(D,eigV,W,V,hbar,epsilon_Ar,tau_Ar,a2,x,x_ucell,m,kappa)
%FUNCTION: find forces acting on all atoms in system.
%update: tried to save Rij and PHI, but that ran out of memory
%update: trying to vectorize everything. version 3 was able to calcualte
%the diff of perfect crystal to be 0, seems like it should be!
%comment: changing that internal loop over alpha beta to 9 separate
%expressions actually SLOWED the calculation down!

%For force constant matrix
   A=size(x);
   N=A(1,1);
   I=sqrt(-1);
   i=sqrt(-1);
   Phi(1:3,1:3)=0;
%   D( 1:3*N, 1:3*N ) = 0;
   cnt=0;
%For thermal diffusivity    
    S(1:length(W),1:length(W))=0;
    vij(1:length(W),1:length(W),1:3)=0;

tic    
    for i1=1:length(W)
        for i2=1:length(W)
            wi = W(i1,i1);
            wj = W(i2,i2);
            %Sum over k and k'
            %sizeR=size(Rij);
            
            
            
            for i3=1:N
                for i4=1:N
                    eig1 = eigV(1+(i3-1):i3+2,i1);          %kth atom, wi's frequency
                    eig2 = eigV(1+(i4-1):i4+2,i2);          %k'th atom, wj's frequency
                    
                    clear rij r2
                    rij(length(x_ucell)/N,1:3)=0;
                    rij(:,1) = x_ucell(i4:N:length(x_ucell),1) - x(i3,1);
                    rij(:,2) = x_ucell(i4:N:length(x_ucell),2) - x(i3,2);
                    rij(:,3) = x_ucell(i4:N:length(x_ucell),3) - x(i3,3);
                    r2 = rij(:,1).^2 + rij(:,2).^2 + rij(:,3).^2;
                    clear I
                    [I] = find(r2<a2 & r2~=0); Isize = size(I);
                    for i5=1:Isize(1,1)
                        Dij(1:3,1:3)=0;
                        [Dij] = (1/sqrt(m(i3)*m(i4)))*(-1*make_force_matrix_2(r2(I(i5)),rij(I(i5),1:3),rij(I(i5),1:3)));

                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(1)*Dij(1,1)*rij(1,1)*eig2(1);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(1)*Dij(1,1)*rij(1,2)*eig2(1);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(1)*Dij(1,1)*rij(1,3)*eig2(1);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(1)*Dij(1,2)*rij(1,1)*eig2(2);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(1)*Dij(1,2)*rij(1,2)*eig2(2);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(1)*Dij(1,2)*rij(1,3)*eig2(2);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(1)*Dij(1,3)*rij(1,1)*eig2(3);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(1)*Dij(1,3)*rij(1,2)*eig2(3);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(1)*Dij(1,3)*rij(1,3)*eig2(3);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(2)*Dij(2,1)*rij(1,1)*eig2(1);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(2)*Dij(2,1)*rij(1,2)*eig2(1);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(2)*Dij(2,1)*rij(1,3)*eig2(1);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(2)*Dij(2,2)*rij(1,1)*eig2(2);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(2)*Dij(2,2)*rij(1,2)*eig2(2);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(2)*Dij(2,2)*rij(1,3)*eig2(2);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(2)*Dij(2,3)*rij(1,1)*eig2(3);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(2)*Dij(2,3)*rij(1,2)*eig2(3);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(2)*Dij(2,3)*rij(1,3)*eig2(3);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(3)*Dij(3,1)*rij(1,1)*eig2(1);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(3)*Dij(3,1)*rij(1,2)*eig2(1);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(3)*Dij(3,1)*rij(1,3)*eig2(1);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(3)*Dij(3,2)*rij(1,1)*eig2(2);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(3)*Dij(3,2)*rij(1,2)*eig2(2);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(3)*Dij(3,2)*rij(1,3)*eig2(2);
                                
                                vij(i1,i2,1) = vij(i1,i2,1) + eig1(3)*Dij(3,3)*rij(1,1)*eig2(3);
                                vij(i1,i2,2) = vij(i1,i2,2) + eig1(3)*Dij(3,3)*rij(1,2)*eig2(3);
                                vij(i1,i2,3) = vij(i1,i2,3) + eig1(3)*Dij(3,3)*rij(1,3)*eig2(3);

                    end
                end
            end
            vij(i1,i2,1:3) = vij(i1,i2,1:3)*(i/(2*sqrt(wi*wj)));
            S(i1,i2,1:3) = ((hbar*tau_Ar/epsilon_Ar)/(2*V))*vij(i1,i2,1:3)*(wi+wj);
            i1
            i2
            toc
        end
    end
    
    
    
 
   
%Measure average frequency separation    
    
    for i1=2:length(W)
        dw(i1) = W(i1,i1) - W(i1-1,i1-1);
    end
    gamma = mean(dw);           %set full-width half max equal to average frequency separation
    
%Measure mode diffusivity

Diff(1:length(W),1:2) = 0;
    for i1=1:length(W)
        wi = W(i1,i1);
        for i2=1:length(W)
            wj = W(i2,i2);
            lor(i2) = (1/pi)*(gamma/( (wj-wi)^2 + gamma^2));        %Lorentzian
            Diff(i1,2) = Diff(i1,2) + (sqrt(S(i1,i2,1)^2+S(i1,i2,2)^2+S(i1,i2,3)^2)^2)*lor(i2);
        end
        Diff(i1,1) = wi;
        Diff(i1,2) = Diff(i1,2)*((pi*V^2)/(3* ((hbar*tau_Ar/epsilon_Ar)^2)*wi^2));
    end
            
        
        
    
    
    