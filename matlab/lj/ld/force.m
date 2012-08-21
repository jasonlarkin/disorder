function [F,P,P_virial]=force(N,L,a2,f_cutoff,a,x,cutoff,V)
%FUNCTION: find forces acting on all atoms in system.
    %clear old Forces
    F(1:N,1:3)=zeros(N,3);
    P_virial = 0;
    for i=1:N
        for j=(i+1):N
                for k=1:3
                    rij(1,k) = x(i,k) - x(j,k);
                    rij(1,k) = periodic(rij(1,k),L(1,k));   %periodic boundaries
                end
                    r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;
                    if r2<a2
                        if cutoff == 1
                            f = force_eval(r2);
                            %f = force_eval(r2) - f_cutoff*a*(1/sqrt(r2));
                        else
                            %f = force_eval(r2) - f_cutoff*a*(1/sqrt(r2));
                            f = force_eval(r2);
                        end
                        for k=1:3    
                            F(i,k) = F(i,k) + rij(1,k)*f;
                            F(j,k) = F(j,k) - rij(1,k)*f;
                        end
                        P_virial = P_virial +r2*f;
                    end
        end
    end
    %P = (P_viral/(3*V)) + ((N*T)/V);
    %Set P and Pset to 1 for the purpose of NanoMD
    P=1;
end