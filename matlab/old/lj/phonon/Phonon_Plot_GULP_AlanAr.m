
hold off

numbands=2; numdir=4;
%order of vectors: G_X, X_K, K_G, G_L
vectors=[sqrt( (0.5-0.0)^2+(0.0-0.0)^2+(0.0-0.0)^2);
         sqrt( (0.5-0.375)^2+(0.0-0.375)^2+(0.0-0.0)^2);
         sqrt( (0.0-0.375)^2+(0.0-0.375)^2+(0.0-0.0)^2);
         sqrt( (0.0-0.25)^2+(0.0-0.25)^2+(0.0-0.25)^2);];

text_label = [strcat('G');
               strcat('X');
               strcat('K');
               strcat('G');
               strcat('L');];
    
    str='D:\CMU\work\Phonons\LJArgon\Phonon_Disp\rhom\';
    str2 = strcat(str,'G_X.disp');   
    A(:,1:2) = dlmread(str2);
    str2 = strcat(str,'X_K.disp');    
    A(:,3:4) = dlmread(str2);
    str2 = strcat(str,'K_G.disp');    
    A(:,5:6) = dlmread(str2);
    str2 = strcat(str,'G_L.disp');    
    A(:,7:8) = dlmread(str2);
    pause
    numpoints=length(A)/numbands;
    length_vec=0;
for i1=1:numdir

    cnt=1;
    for i2=1:numbands:length(A)
        band(i1,1:numbands,cnt)=A(i2:(i2-1)+numbands,i1*2);
        cnt=cnt+1;
    end

    pause

    hold on
    for i=1:numbands
        C(1:numpoints)=band(i1,i,1:numpoints);
        plot((((1:numpoints)/numpoints)*vectors(i1))+length_vec,C,'.');
    end
        line([vectors(i1)+length_vec;vectors(i1)+length_vec],[0;max(max(A))]);
        text(length_vec,-5.0,text_label(i1));
    length_vec=length_vec+vectors(i1);
end


% bandi=1;
% for k=1:numdir
% for i=1:numbands
%     for j=1:numpoints
%         band2(j+(k-1)*(numpoints),1) = bandi;
%         band2(j+(k-1)*(numpoints),i+1) = A(i+1+(j-1)*(numbands+1)+(numdir-1)*(numbands+1)*numpoints,2);
%     end
%     bandi=bandi+1;
% end
% end
% 
% 

    
% for i=1:20
% 
%     str = strcat('D:\CMU\vasp\Juarez\GeTe\GeTe_Pot_Structures\GeTeIRS\RelaxFCC\band\bands\band',int2str(i),'.txt'); 
% A = dlmread(str);
% 
% str2 = strcat('D:\CMU\vasp\Juarez\GeTe\GeTe_Pot_Structures\GeTeIRS\RelaxFCC\band\bands\band',int2str(i+20),'.txt');
% 
% dlmwrite(str2,A);
% 
% end