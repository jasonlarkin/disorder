function [Phi] = make_force_matrix_2(r2,ralpha,rbeta)
%FUNCTION: make force constant matrix using finite difference
         phi1 = first_deriv(r2);
         phi2 = second_deriv(r2);
         
         for i=1:3
             for j=1:3
                 if i==j
                    Phi(i,j) =  (ralpha(1,i)*rbeta(1,j)/r2)*(phi2 - phi1) + phi1; 
                 else
                    Phi(i,j) =  (ralpha(1,i)*rbeta(1,j)/r2)*(phi2 - phi1);
                 end
             end
         end

end