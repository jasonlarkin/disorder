function [C]=force_matrix(N,L,a2,f_cutoff,a,x,cutoff,cell_index,x_ucell);
%FUNCTION: find forces acting on all atoms in system.
    %clear old force constants
    
    A = size(x_ucell);
    C(1:3*A(1,1),1:3*A(1,1))=zeros(3*A(1,1),3*A(1,1));
    
    for i=1:A(1,1)        
        for j=1:A(1,1)            
            rij(1,1:3) = x_ucell(i,1:3) - x_ucell(j,1:3);
%                rij(1,1:3) = periodic(rij(1,k),L(1,k));   %periodic boundaries
            r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;
            ralpha(1,1:3) = x_ucell(i,1:3);
            rbeta(1,1:3) = x_ucell(j,1:3);
                if r2<a2
                    if i==j & x_ucell(i,4) == x_ucell(j,4) 
                        for i2 = 1:A(1,1) 
                            if i2~=i  
                                rij(1,1:3) = x_ucell(i,1:3) - x_ucell(i2,1:3);
                                r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;
                                ralpha(1,1:3) = x_ucell(i,1:3);
                                rbeta(1,1:3) = x_ucell(i2,1:3);
                                [Phi] = make_force_matrix(r2,rij,ralpha,rbeta);
                                C( 1+(i-1)*3 : i*3, 1+(j-1)*3 : j*3) = C( 1+(i-1)*3 : i*3, 1+(j-1)*3 : j*3) + Phi; 
                            end
                        end
                    else                       
                                [Phi] = make_force_matrix(r2,rij,ralpha,rbeta);
                                C( 1+(i-1)*3 : i*3, 1+(j-1)*3 : j*3) = Phi; 
                    end
                else
                    C( 1+(i-1)*3 : i*3, 1+(j-1)*3 : j*3) = 0;
                end
        end
    end
        
    
%     
%     for i=1:N
%         for j=1:N
%                         for k=1:3
%                             rij(1,k) = x(i,k) - x(j,k);
%                             rij(1,k) = periodic(rij(1,k),L(1,k));   %periodic boundaries
%                         end
%                         r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;
%                         rij;
%                         pause
%                             if r2<a2
%                                 if cutoff == 1
%                                     f = force_eval(r2);
%                                     k_spring = curv_eval(r2);
%                                     %f = force_eval(r2) - f_cutoff*a*(1/sqrt(r2));
%                                 else
%                                     %f = force_eval(r2) - f_cutoff*a*(1/sqrt(r2));
%                                     f = force_eval(r2);
%                                     k_spring = curv_eval(r2);
%                             end
%                                 for k=1:3    
%                                     F(i,k) = F(i,k) + rij(1,k)*f;
%                                     F(j,k) = F(j,k) - rij(1,k)*f;
%                                     for l=1:3
%                                         C((i-1)*3+k,(j-1)*3+l) = (k_spring/r2)*(rij(1,k)*rij(1,l));  
%                                     end                       
%                                 end
%             end
%         end
% end