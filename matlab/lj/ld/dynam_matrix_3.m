function [D] = dynam_matrix_3(a2,x,x_ucell,m,kappa)
%FUNCTION: find dynamical matrix
%update: trying to get rid of C-style programing to speed up matlab
%update: saving force constants (PHI) and atomic separations (Rij) in arrays to use for
%thermal diff calcualtion
    %clear old force constants
    A=size(x); N=A(1,1); i=sqrt(-1);
    Phi(1:3,1:3)=0; D( 1:3*N, 1:3*N ) = 0; 
%    Rij(1:N,1:3,1:N)=0;
    %PHI(3*N, 3*N, 3*N)=0;
    
    for i1=1:N
        for i2=1:N        
            %k,k',m
            clear rij r2
            rij(length(x_ucell)/N,1:3)=0;
            rij(:,1) = x_ucell(i2:N:length(x_ucell),1) - x(i1,1);
            rij(:,2) = x_ucell(i2:N:length(x_ucell),2) - x(i1,2);
            rij(:,3) = x_ucell(i2:N:length(x_ucell),3) - x(i1,3);
            r2 = rij(:,1).^2 + rij(:,2).^2 + rij(:,3).^2;
            clear I
            [I] = find(r2<a2 & r2~=0); Isize = size(I);
            for i3=1:Isize(1,1)
                Phi(1:3,1:3)=0;
                Phi = -1*(make_force_matrix(r2(I(i3)),rij(I(i3),1:3),rij(I(i3),1:3)));
                D( 1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2 ) = D( 1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2 ) + Phi*exp(sqrt(-1)*dot(kappa,rij(I(i3),1:3)));
                %PHI(1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2, i3)=(1/sqrt(m(i1)*m(i2)))*Phi;        %save indivdual force constant matrices 
                %Rij(i1,i2,1,1) = Isize(1,1);                                                   %save number of neighbors
                %Rij(i1,i2,i3+1,1:3) = rij(I(i3),1:3);                                          %save atomic separations
            end
            %The self term
            if i1==i2
                clear rij r2
                rij(length(x_ucell),1:3)=0;
                rij(:,1) = x_ucell(:,1) - x(i1,1); rij(:,2) = x_ucell(:,2) - x(i1,2); rij(:,3) = x_ucell(:,3) - x(i1,3);
                r2 = rij(:,1).^2 + rij(:,2).^2 + rij(:,3).^2;
                clear I
                [I] = find(r2<a2 & r2~=0); Isize = size(I); Phi(1:3,1:3)=0;
                for i3=1:Isize(1,1)
                    Phi = Phi + (make_force_matrix(r2(I(i3)),rij(I(i3),1:3),rij(I(i3),1:3)));
                end
                D( 1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2 ) = D( 1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2 ) + Phi;
            end
            D( 1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2 ) = ( (1/sqrt((m(i1)*m(i2)))) )*D( 1+(i1-1)*3:3*i1, 1+(i2-1)*3:3*i2 );
        end
    end
