%--------------INPUT-------------------------------------------------------
%LJ Potential and Material Parameters
LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
LJ.tau = sqrt((LJ.mass*(LJ.sigma^2))/LJ.eps);
constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
constant.hbar = 1.054E-34;                %J/s
constant.i = sqrt(-1);
constant.c = 29979245800.00019;      %cm/s
constant.s2ps = 1E-12;
constant.ang2m = 1E-10;
%--------------------------------------------------------------------------
[tmp,str.main] = system('pwd');
%str.main = strcat(str.main,'/amorphous/4x/prepare/');
%str.cmd = ['cd ' str.main];
%system(str.cmd);

%load the positions
iseed = 1;
x = dlmread(strcat(str.main,'/LJ_amor_',int2str(iseed),'.pos'));

%GULP format

%prepare GULP template

GULP.str.dummy = 'POSTION';
GULP.str.end1 = 'kpoints 1';
GULP.str.end2 = '0.0 0.0 0.0';


system(strcat('cp amor_dynam_template.gin amor_dynam_template2.gin'));

%add some dummy POSITIONiatom tags
for iatom = 1:size(x,1)-1
    output = [GULP.str.dummy int2str(iatom) ' '];
    dlmwrite(strcat(str.main,'/amor_dynam_template2.gin'),output,'-append',...
        'delimiter','','precision',8);
end

%replace POSITION tags with acutal positions
for iatom = 1:size(x,1)-1      
    str.orig = [GULP.str.dummy int2str(iatom) ' '];
    str.change = ['Si core ' num2str(x(iatom+1,3:5)) ' 0 1 1 1'];
    str.cmd = strcat('sed ''s/\<',str.orig,'\>/',str.change,...
        '/g'' amor_dynam_template2.gin > amor_dynam_template3.gin');
    system(str.cmd);
%replace     
    system('cp amor_dynam_template3.gin amor_dynam_template2.gin');    
end

%replace LX,Y,Z tags 
        str.orig = strcat('LX');
    str.change = num2str(x(1,3));
    str.cmd = strcat('sed ''s/',str.orig,'/',str.change,...
        '/g'' amor_dynam_template3.gin > amor_dynam_template4.gin');
    system(str.cmd);
    
        str.orig = strcat('LY');
    str.change = num2str(x(1,4));
    str.cmd = strcat('sed ''s/',str.orig,'/',str.change,...
        '/g'' amor_dynam_template4.gin > amor_dynam_template5.gin');
    system(str.cmd);
    
        str.orig = strcat('LZ');
    str.change = num2str(x(1,5));
    str.cmd = strcat('sed ''s/',str.orig,'/',str.change,...
        '/g'' amor_dynam_template5.gin > amor_dynam.gin');
    system(str.cmd);

%add end string for dispersion  
output = '';
dlmwrite(strcat(str.main,'/amor_dynam.gin'),output,'-append',...
    'delimiter','','precision',8,'newline', 'unix');   
output = strcat(GULP.str.end1);
dlmwrite(strcat(str.main,'/amor_dynam.gin'),output,'-append',...
    'delimiter','','precision',8);  
output = strcat(GULP.str.end2);
dlmwrite(strcat(str.main,'/amor_dynam.gin'),output,'-append',...
    'delimiter','','precision',8);      
    
str_cmd = 'gulp amor_dynam amor_dynam';
system(str_cmd);

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------


