function [direct cart latvec latvec_rec] = m_si_unit_cell_conv( alat )
%expects

latvec = [1.0 0.0 0.0 
          0.0 1.0 0.0 
          0.0 0.0 1.0];
                    
direct = [0.00 0.00 0.00
          0.50 0.50 0.00
          0.25 0.25 0.25
          0.75 0.75 0.25
          0.50 0.00 0.50 
          0.00 0.50 0.50
          0.75 0.25 0.75
          0.25 0.75 0.75];

cart = m_direct2cart( direct , (alat)*latvec );

latvec_rec = [ 1.0  0.0  0.0 
               0.0  1.0  0.0 
               0.0  0.0  1.0];

end