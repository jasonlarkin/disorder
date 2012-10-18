function alat = m_lj_alat
%VC = m_lj_alloy_nmd_vc_results
%returns the vc thermal conductivity results @ 10K for 4-12x systems
%--------------------------------------------------------------------------

lj = m_lj; constant = m_constant;

%c=0.0
alat(1,1) = 5.269/(lj.sigma/constant.ang2m);       %0K 
alat(1,2) = 5.290/(lj.sigma/constant.ang2m);       %10K
alat(1,3) = 5.315/(lj.sigma/constant.ang2m);       %20K
alat(1,4) = 5.341/(lj.sigma/constant.ang2m);       %30K
alat(1,5) = 5.370/(lj.sigma/constant.ang2m);       %40K
alat(1,6) = 5.401/(lj.sigma/constant.ang2m);       %50K
alat(1,7) = 5.436/(lj.sigma/constant.ang2m);       %60K
alat(1,8) = 5.476/(lj.sigma/constant.ang2m);       %70K
alat(1,9) = 5.527/(lj.sigma/constant.ang2m);       %80K

%c=0.05
alat(2,1) = 5.269/(lj.sigma/constant.ang2m);       %0K 
alat(2,2) = 6.22415/4;                             %10K
alat(2,3) = 5.315/(lj.sigma/constant.ang2m);       %20K
alat(2,4) = 5.341/(lj.sigma/constant.ang2m);       %30K
alat(2,5) = 5.370/(lj.sigma/constant.ang2m);       %40K
alat(2,6) = 5.401/(lj.sigma/constant.ang2m);       %50K
alat(2,7) = 5.436/(lj.sigma/constant.ang2m);       %60K
alat(2,8) = 5.476/(lj.sigma/constant.ang2m);       %70K
alat(2,9) = 5.527/(lj.sigma/constant.ang2m);       %80K

%c=0.15
alat(3,1) = 5.269/(lj.sigma/constant.ang2m);       %0K 
alat(3,2) = 6.22462/4;                             %10K
alat(3,3) = 5.315/(lj.sigma/constant.ang2m);       %20K
alat(3,4) = 5.341/(lj.sigma/constant.ang2m);       %30K
alat(3,5) = 5.370/(lj.sigma/constant.ang2m);       %40K
alat(3,6) = 5.401/(lj.sigma/constant.ang2m);       %50K
alat(3,7) = 5.436/(lj.sigma/constant.ang2m);       %60K
alat(3,8) = 5.476/(lj.sigma/constant.ang2m);       %70K
alat(3,9) = 5.527/(lj.sigma/constant.ang2m);       %80K

%c=0.15
alat(4,1) = 5.269/(lj.sigma/constant.ang2m);       %0K 
alat(4,2) = 6.22599/4;                             %10K
alat(4,3) = 5.315/(lj.sigma/constant.ang2m);       %20K
alat(4,4) = 5.341/(lj.sigma/constant.ang2m);       %30K
alat(4,5) = 5.370/(lj.sigma/constant.ang2m);       %40K
alat(4,6) = 5.401/(lj.sigma/constant.ang2m);       %50K
alat(4,7) = 5.436/(lj.sigma/constant.ang2m);       %60K
alat(4,8) = 5.476/(lj.sigma/constant.ang2m);       %70K
alat(4,9) = 5.527/(lj.sigma/constant.ang2m);       %80K

end
