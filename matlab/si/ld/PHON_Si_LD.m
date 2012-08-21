function [freq,eigVsorted,velocity] = PHON_Si_LD(LD,FC,constant,kappa)

%4) Calculate dynamical matrix 
            [freq,eigVsorted] = phon_dynam_matrix(LD,FC,constant,kappa);
            
%CALCULATE VG numerically              
         
            for idim = 1:3
                if kappa(idim)==0.5
                    %[HLDpks1] = LDAr_Prim([LC LC LC],[kappa(1) kappa(2) kappa(3)]);
                    [HLDpks1] = phon_dynam_matrix(LD,FC,constant,kappa);
                    kappa(idim) = kappa(idim) - LD.dk;
                    %[HLDpks2] = LDAr_Prim([LC LC LC],[k(1) k(2) k(3)]);
                    [HLDpks2] = phon_dynam_matrix(LD,FC,constant,kappa);
                    velocity(:,idim) = (( HLDpks1 - HLDpks2 )/ LD.dk );
                    kappa(idim) = kappa(idim) + LD.dk;
                elseif kappa(idim)==-0.5
                    %[HLDpks1] = LDAr_Prim([LC LC LC],[k(1) k(2) k(3)]);
                    [HLDpks1] = phon_dynam_matrix(LD,FC,constant,kappa);
                    kappa(idim) = kappa(idim) + LD.dk;
                    %[HLDpks2] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    [HLDpks2] = phon_dynam_matrix(LD,FC,constant,kappa);
                    velocity(:,idim) = (( HLDpks1 - HLDpks2 )/LD.dk);
                    kappa(idim) = kappa(idim) - LD.dk;
                elseif kappa(idim)==0.0
                    kappa(idim) = kappa(idim) + LD.dk;
                    %[HLDpks1] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    [HLDpks1] = phon_dynam_matrix(LD,FC,constant,kappa);
                    kappa(idim) = kappa(idim) - LD.dk;
                    %[HLDpks2] = LDAr_FCC([LC LC LC],[k(1) k(2) k(3)]);
                    [HLDpks2] = phon_dynam_matrix(LD,FC,constant,kappa);
                    velocity(:,idim) = (( HLDpks1 - HLDpks2 )/LD.dk);
                else
                    kappa(idim) = kappa(idim) + LD.dk;
                    [HLDpks1] = phon_dynam_matrix(LD,FC,constant,kappa);
                    kappa(idim) = kappa(idim) - 2*LD.dk;
                    [HLDpks2] = phon_dynam_matrix(LD,FC,constant,kappa);
                    velocity(:,idim) = (( HLDpks1 - HLDpks2 )/(2*LD.dk));
                    kappa(idim) = kappa(idim) + LD.dk;
                end
            end
            
            
            
            
function [freq,eigVsorted] = phon_dynam_matrix(LD,FC,constant,kappa)
%FUNCTION: find dynamical matrix
 DYNAM(1:3*LD.NUM_ATOMS_UCELL,1:3*LD.NUM_ATOMS_UCELL) = 0.0;
    for i1=1:length(FC.id)  
        dot_product = (pi)*( kappa(1)*FC.vec(i1,1)/LD.alat(1) + kappa(2)*FC.vec(i1,2)/LD.alat(2) + kappa(3)*FC.vec(i1,3)/LD.alat(3) ); 
        invmexpikR = exp(sqrt(-1)*dot_product)*(1/sqrt(LD.m(FC.id(i1,1))*LD.m(FC.id(i1,1)))); 
        if FC.id(i1,1)==FC.id(i1,2)
            Phi(1:3,1:3) = invmexpikR*FC.FC(i1,1:3,1:3);
        else
            Phi(1:3,1:3) = -invmexpikR*FC.FC(i1,1:3,1:3);
        end
        ii = (FC.id(i1,1)-1)*3 + 1 : (FC.id(i1,1))*3;
        jj = (FC.id(i1,2)-1)*3 + 1 : (FC.id(i1,2))*3;
        
    DYNAM( ii , jj  ) = DYNAM(  ii , jj ) + Phi ; %real(Phi)+sqrt(-1)*imag(Phi);
    
%     real(DYNAM)
%     imag(DYNAM)
%     pause
    
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
              %freq = w;
              %eigVsorted = eigV;
end

end
