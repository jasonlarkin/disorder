
Nx = 12;

NUM_ATOMS = Nx^3*(4);
NUM_SEEDS = 5;


[tmp,str.main] = system('pwd'); str.main_write = str.main;
str.main = strcat(str.main,'/12x/prepare');

% %4x 10K
% alat =[...
%     6.32303
%     6.32167
%     6.27243
%     6.27761
%     6.33021...
%     ];

% %8x 10K
% alat =[...
%     12.6458
%     12.6444
%     12.4932
%     12.6567
%     12.6491...
%     ];

% %10x 10K
% alat =[...
%     15.791
%     15.8059
%     15.8093
%     15.8092
%     15.8143...
%     ];

%12x 10K
alat =[...
    18.9686
    18.975
    18.9716
    18.9547
    18.9744...
    ];



for iseed=1:NUM_SEEDS
    
%grep the positions
	str.str1 = 'grep -A ';
    str.str2 = int2str(NUM_ATOMS);
	str.str3 = ' "ITEM: ATOMS x y z " ';
    str.str4 = [str.main '/quenched_annelead_' int2str(iseed) '.pos '];
    str.str5 = ['> ' str.main '/LJ_amor.pos'];
	str.cmd = [str.str1 str.str2 str.str3 str.str4 str.str5];
    
system(str.cmd);

	str.str1 = 'sed ''s/ITEM: ATOMS x y z //g'' ';
	str.str2 = [str.main '/LJ_amor.pos '];
	str.str3 = [' > ' str.main  '/LJ_amor2.pos'];
	str.cmd = [str.str1 str.str2 str.str3];
    
system(str.cmd);
    
%grep the bounding box    
	str.str1 = 'grep -A ';
    str.str2 = int2str(3);
	str.str3 = ' "ITEM: BOX BOUNDS" ';
    str.str4 = [str.main '/quenched_annelead_' int2str(iseed) '.pos '];
    str.str5 = ['> ' str.main '/LJ_box.pos'];
	str.cmd = [str.str1 str.str2 str.str3 str.str4 str.str5];
    
system(str.cmd);

	str.str1 = 'sed ''s/ITEM: BOX BOUNDS//g'' ';
	str.str2 = [' ' str.main '/LJ_box.pos '];
	str.str3 = [' > ' str.main '/LJ_box2.pos'];
	str.cmd = [str.str1 str.str2 str.str3];
    %str.cmd = [str.str1 str.str2];
    
system(str.cmd);
    
%Read in positions		
    str.read=[str.main '/LJ_amor2.pos'];
    fid=fopen(str.read);
    dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
    fclose(fid);
    
    x(:,1) = 1:NUM_ATOMS;
    x(:,2) = 1;
    x(:,3) = dummy{1}(1:NUM_ATOMS,:); x(:,4) = dummy{2}(1:NUM_ATOMS,:);
    x(:,5) = dummy{3}(1:NUM_ATOMS,:);
%shift coordinates
    x(:,3:5) = bsxfun(@minus, x(:,3:5), min(x(:,3:5)));

%Read in box		
    str.read=[str.main '/LJ_box2.pos'];
    fid=fopen(str.read);
    dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
    fclose(fid);    
    
    box(:,1) = dummy{1}(:); box(:,2) = dummy{2}(:);
%rescale to 0-press 10K alat
    x(:,3:5) = x(:,3:5)*( alat(iseed)/(box(1,2)-box(1,1)) ); 
%output
    output = [NUM_ATOMS NUM_ATOMS...
 alat(iseed) alat(iseed) alat(iseed)];

    dlmwrite([str.main '/LJ_amor_',int2str(iseed),'.pos'],...
        output ,'-append','delimiter',' ');
    dlmwrite([str.main '/LJ_amor_',int2str(iseed),'.pos'],...
        x ,'-append','delimiter',' ');
    
    plot3(x(:,3),x(:,4),x(:,5),'.')
    
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------    

end




