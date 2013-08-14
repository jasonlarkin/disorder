clear

con = m_constant;

scaling(1).baldi_emergence_2013 = 9; %meV
scaling(1).baldi_emergence_2013*con.meV2rads
pause
scaling(2).baldi_sound_2010 = 1.5e12; %THz
scaling(2).baldi_sound_2010*2*pi
pause
scaling(3).baldi_elastic_2011 = 4.2; %meV
scaling(3).baldi_elastic_2011*con.meV2rads
pause
scaling(4).mas_evidence_2006.w24 = 100e9; %GHz
scaling(4).mas_evidence_2006.w24*2*pi
pause
scaling(4).mas_evidence_2006.w42 = 140e9; %GHz
scaling(4).mas_evidence_2006.w42*2*pi
pause