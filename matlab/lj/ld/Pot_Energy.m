function PE=Pot_Energy(N,L,pe_cutoff,a,a2,f_cutoff,cutoff,x)
%FUNCTION: calcualte the total Potential Energy in the system.
    PE=0;
    for i=1:N
        for j=(i+1):N
            for k=1:3
                rij(1,k) = x(i,k) - x(j,k);
                rij(1,k) = periodic(rij(1,k),L(1,k));  %periodic boundaries
            end
            r2 = rij(1,1)^2 + rij(1,2)^2 + rij(1,3)^2;

            if (r2<a2)
                if cutoff==1
                    PE = PE + pe(r2) - pe_cutoff;
                else
                    PE = PE + pe(r2);
                end
            end
        end
    end
end