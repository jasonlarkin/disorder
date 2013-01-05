function [Diff] = therm_diff(D,eigV,W,V,hbar,epsilon_Ar,tau_Ar,a2,x,x_ucell,m,kappa)
%FUNCTION: find forces acting on all atoms in system.
    %clear old force constants
    I=sqrt(-1);
    i=sqrt(-1);
    Phi(1:3,1:3)=0;
    cnt=0;
    
    S(1:length(W),1:length(W))=0;
    vij(1:length(W),1:length(W))=0;
    
    i1=length(W);
    
    for i1=1:length(W)
        for i2=1:length(W)
            wi = W(i1,i1);
            wj = W(i2,i2); 
            for i3=1:3:length(D)     
                for i4=1:3:length(D)                  
                    eig1 = eigV(1+(i3-1):i3+2,i1);          %kth atom, wi's frequency
                    eig2 = eigV(1+(i4-1):i4+2,i2);          %k'th atom, wj's frequency
                    Dij = D(1+(i3-1):i3+2,1+(i4-1):i4+2);   %kth and k'th part of D
                    for i5=1:3
                        for i6=1:3 
                            vij(i1,i2) = vij(i1,i2) + eig1(i5)*Dij(i5,i6)*eig2(i6);
                        end
                    end
                end
            end
           vij(i1,i2) = vij(i1,i2)*(i/(2*sqrt(wi*wj))); 
           S(i1,i2) = ((hbar*tau_Ar/epsilon_Ar)/(2*V))*vij(i1,i2)*(wi+wj);   
        end
    end
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     A=size(x);
    N=A(1,1);
    I=sqrt(-1);
    i=sqrt(-1);
    Phi(1:3,1:3)=0;
    D( 1:3*N, 1:3*N ) = 0;
    cnt=0;
    for i1=1:N
        for j=1:N
            for k=j:N:length(x_ucell)    
                Phi(1:3,1:3)=0;
                rij(1,1:3) = x_ucell(k,1:3) - x(i1,1:3);
                %pause
                r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;
                ralpha(1,1:3) =  rij(1,1:3);
                rbeta(1,1:3) =  rij(1,1:3);  
                
                    if r2<a2
                        if x(i1,1)==x_ucell(k,1) & x(i1,2)==x_ucell(k,2) & x(i1,3)==x_ucell(k,3)
                                %skip over the same atom in unit cell and images  
                                Phi(1:3,1:3)=0;
                                    for i2 = 1:length(x_ucell)
                                        rij(1,1:3) = x_ucell(i2,1:3) - x(i1,1:3);
                                        r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;
                                        ralpha(1,1:3) =  rij(1,1:3);
                                        rbeta(1,1:3) =  rij(1,1:3);  
                                            if x(i1,1)==x_ucell(i2,1) & x(i1,2)==x_ucell(i2,2) & x(i1,3)==x_ucell(i2,3) 
                                                % don't use self-term
                                            elseif r2<a2
                                                Phi = Phi + make_force_matrix(r2,ralpha,rbeta);
                                            end
                                    end
                                    rij(1,1:3) = x_ucell(k,1:3) - x(i1,1:3);
                                    D( 1+(i1-1)*3:3*i1, 1+(j-1)*3:3*j ) = D( 1+(i1-1)*3:3*i1, 1+(j-1)*3:3*j ) + Phi;
                                    Phi(1:3,1:3)=0;
                        else
                                    Phi(1:3,1:3)=0;
                                    Phi = -1*(make_force_matrix(r2,ralpha,rbeta));
                                    D( 1+(i1-1)*3:3*i1, 1+(j-1)*3:3*j ) = D( 1+(i1-1)*3:3*i1, 1+(j-1)*3:3*j ) + Phi*exp(sqrt(-1)*dot(kappa,rij));
                                    Phi(1:3,1:3)=0;
                        end
                    else
                        %do nothing
                    end
            end
            D( 1+(i1-1)*3:3*i1, 1+(j-1)*3:3*j ) = ( (1/sqrt((m(i1)*m(j)))) )*D( 1+(i1-1)*3:3*i1, 1+(j-1)*3:3*j );
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
            Diff(i1,2) = Diff(i1,2) + S(i1,i2)*lor(i2);
        end
        Diff(i1,1) = wi;
        Diff(i1,2) = ((pi*V^2)/(3* ((hbar*tau_Ar/epsilon_Ar)^2)*wi^2)) * Diff(i1,2);
    end
            
        
        
    
    
    