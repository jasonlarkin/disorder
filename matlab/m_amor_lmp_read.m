clear
%--------------------------------------------------------------------------
%     [tmp,str.main]=system('pwd');
str.main='/home/jason/disorder2/lj/amor/4x/prepare/tmp/';
%--------------------------------------------------------------------------



Nx = 4;

NUM_ATOMS = Nx^3*(4);
NUM_SEEDS = 5;

NMD.Lx = 6.34543;

%--------------------------------------------------------------------------
% [tmp,str.main] = system('pwd'); str.main_write = str.main;
% str.main = strcat(str.main,'/4x/prepare/');


for iseed=1:NUM_SEEDS
    
%grep the positions
	str.str1 = 'grep -A ';
    str.str2 = int2str(NUM_ATOMS);
	str.str3 = ' "ITEM: ATOMS x y z " ';
    str.str4 = [str.main 'quenched_annelead_' int2str(iseed) '.pos '];
    str.str5 = ['> ' str.main 'LJ_amor.pos'];
	str.cmd = [str.str1 str.str2 str.str3 str.str4 str.str5];
    
system(str.cmd);

	str.str1 = ['sed ''s/ITEM: ATOMS x y z //g'' '];
	str.str2 = [' ' str.main 'LJ_amor.pos '];
	str.str3 = [' > ' str.main 'LJ_amor2.pos'];
	str.cmd = [str.str1 str.str2 str.str3];
    
system(str.cmd);
    
%grep the bounding box    
	str.str1 = 'grep -A ';
    str.str2 = int2str(3);
	str.str3 = ' "ITEM: BOX BOUNDS" ';
    str.str4 = [str.main 'quenched_annelead_' int2str(iseed) '.pos '];
    str.str5 = ['> ' str.main 'LJ_box.pos'];
	str.cmd = [str.str1 str.str2 str.str3 str.str4 str.str5];
    
system(str.cmd);

	str.str1 = 'sed ''s/ITEM: BOX BOUNDS//g'' ';
	str.str2 = [' ' str.main 'LJ_box.pos '];
	str.str3 = [' > ' str.main 'LJ_box2.pos'];
	str.cmd = [str.str1 str.str2 str.str3];
    %str.cmd = [str.str1 str.str2];
    
system(str.cmd);
    
%Read in positions		
    str.read=[str.main 'LJ_amor2.pos'];
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
    str.read=[str.main 'LJ_box2.pos'];
    fid=fopen(str.read);
    dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
    fclose(fid);    
    
    box(:,1) = dummy{1}(:); box(:,2) = dummy{2}(:);


    
%output
    output = [NUM_ATOMS NUM_ATOMS...
 box(1,2)-box(1,1) box(1,2)-box(1,1) box(1,2)-box(1,1)];

    dlmwrite([str.main 'x0K_' int2str(iseed) '.data'],...
        output ,'-append','delimiter',' ');
    dlmwrite([str.main './x0K_' int2str(iseed) '.data'],...
        x ,'-append','delimiter',' ');

	x(:,3) = (x(:,3))*( NMD.Lx / (box(1,2)-box(1,1)) );
	x(:,4) = (x(:,4))*( NMD.Lx / (box(1,2)-box(1,1)) );
	x(:,5) = (x(:,5))*( NMD.Lx / (box(1,2)-box(1,1)) );
%output 2
    output = [NUM_ATOMS NUM_ATOMS NMD.Lx NMD.Lx NMD.Lx];

    dlmwrite([str.main 'x0_',int2str(iseed),'.data'],...
        output ,'-append','delimiter',' ');
    dlmwrite([str.main 'x0_',int2str(iseed),'.data'],...
        x ,'-append','delimiter',' ');

end




