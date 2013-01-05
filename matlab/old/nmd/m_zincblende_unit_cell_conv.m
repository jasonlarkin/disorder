function x0 = m_wurzite_unit_cell_prim( x0 )
%m_wurzite_unit_cell_prim  Histogram.
%   N = HIST(Y) bins the elements of Y into 10 equally spaced containers
%   and returns the number of elements in each container.  If Y is a
%   matrix, HIST works down the columns.
%
%   N = HIST(Y,M), where M is a scalar, uses M bins.
%
%   N = HIST(Y,X), where X is a vector, returns the distribution of Y
%   among bins with centers specified by X. The first bin includes
%   data between -inf and the first center and the last bin
%   includes data between the last bin and inf. Note: Use HISTC if
%   it is more natural to specify bin edges instead. 
%
%   Class support for inputs Y, X: 
%      float: double, single
%
%   See also HISTC, MODE.

x0.LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
x0.LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
x0.LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
x0.LJ.tau = sqrt((x0.LJ.mass*(x0.LJ.sigma^2))/x0.LJ.eps);
x0.constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
x0.constant.hbar = 1.054E-34;                %J/s
x0.constant.i = sqrt(-1);
x0.constant.c = 29979245800.00019;      %cm/s
x0.constant.s2ps = 1E-12;
x0.constant.ang2m = 1E-10;
x0.constant.eV2J = 1.60217646E-19;

x0.latvec = 	[   0.0     0.5     0.5 
                    0.5     0.0     0.5 
                    0.5     0.5     0.0     ];
                
x0.latvec_int = 	[   0.0     0.5     0.5 
                        0.5     0.0     0.5 
                        0.5     0.5     0.0     ];
            
x0.ucell.direct = 	[   0.0     0.0     0.0
                        0.25    0.25    0.25];
x0.ucell.type = [1;2];
x0.ucell.

x0.latvec = bsxfun(@times,x0.latvec,x0.alat);

x0.ucell.cart = m_direct2cart( x0.ucell.direct , x0.latvec );

x0.latvec_rec = [1.0 0.0  0.0 
                    0.0  1.0 0.0 
                    0.0  0.0  1.0];

end
