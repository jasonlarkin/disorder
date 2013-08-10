clear
            
prim.Nx = 8; prim.Ny = 8; prim.Nz = 8;             
prim.lat_vec = [1 1 0;1 0 1;0 1 1];
prim.recip_lat_vec = [1 -1 1;-1 1 1;1 1 -1];

%make 
cnt=1;
for ix=(-(prim.Nx/2)+1):1:(prim.Nx/2)
    for iy=(-(prim.Ny/2)+1):1:(prim.Ny/2)
        for iz=(-(prim.Nz/2)+1):1:(prim.Nz/2)
            prim.kptlist(cnt,1:3) = ix/prim.Nx*prim.recip_lat_vec(1,:) ...
                + iy/prim.Ny*prim.recip_lat_vec(2,:) ...
                + iz/prim.Nz*prim.recip_lat_vec(3,:) ;
            prim.NUM_KPTS = cnt;
            cnt=cnt+1;
        end
    end
end

conv.Nx = 8; conv.Ny = 8; conv.Nz = 8;  
conv.lat_vec = [1 0 0;0 1 0;0 0 1];
conv.recip_lat_vec = [1 0 0;0 1 0;0 0 1];

cnt=1;
for ix=(-(conv.Nx)+1):1:(conv.Nx)
    for iy=(-(conv.Ny)+1):1:(conv.Ny)
        for iz=(-(conv.Nz)+1):1:(conv.Nz)
            conv.kptlist(cnt,1:3) = ix/conv.Nx*conv.recip_lat_vec(1,:) ...
                + iy/conv.Ny*conv.recip_lat_vec(2,:) ...
                + iz/conv.Nz*conv.recip_lat_vec(3,:);
            conv.NUM_KPTS = cnt;
            cnt=cnt+1;
        end
    end
end

%Build Reciprocal BCC Lattice

cnt=1;
for ix=-1:2:1
    for iy=-1:2:1
        for iz=-1:2:1
            BZ.kptlist(cnt,1:3) = [ ix iy iz ];
            BZ.NUM_KPTS = cnt;
            cnt=cnt+1;
        end
    end
end
%Build BC images
BZ.kptlist(cnt,1:3) = [ -2 0 0 ]; cnt=cnt+1;
BZ.kptlist(cnt,1:3) = [ 0 -2 0 ]; cnt=cnt+1;
BZ.kptlist(cnt,1:3) = [ 0 0 -2 ]; cnt=cnt+1;

BZ.kptlist(cnt,1:3) = [ 2 0 0 ]; cnt=cnt+1;
BZ.kptlist(cnt,1:3) = [ 0 2 0 ]; cnt=cnt+1;
BZ.kptlist(cnt,1:3) = [ 0 0 2 ]; cnt=cnt+1;
    
    BZ.NUM_KPTS = cnt-1;

plot3(BZ.kptlist(:,1),BZ.kptlist(:,2),BZ.kptlist(:,3),'.',...
    prim.kptlist(:,1),prim.kptlist(:,2),prim.kptlist(:,3),'.')

pause

plot3(BZ.kptlist(:,1),BZ.kptlist(:,2),BZ.kptlist(:,3),'.',...
    conv.kptlist(:,1),conv.kptlist(:,2),conv.kptlist(:,3),'.')


%Move KPTS into 1st BZ
prim.kptlist_fold = prim.kptlist;
prim.dis_gamma = sqrt( ( prim.kptlist(:,1) - 0 ).^2 ...
    + ( prim.kptlist(:,2) - 0 ).^2 + ( prim.kptlist(:,3) - 0 ).^2 ) ;
for ikpt = 1:BZ.NUM_KPTS
    prim.dis(:,ikpt) = sqrt( ( prim.kptlist_fold(:,1) ...
        - BZ.kptlist(ikpt,1) ).^2 + ( prim.kptlist_fold(:,2) ...
        - BZ.kptlist(ikpt,2) ).^2 + ( prim.kptlist_fold(:,3) ...
        - BZ.kptlist(ikpt,3) ).^2 ) ;
    prim.dis_gamma = sqrt( ( prim.kptlist_fold(:,1) - 0 ).^2 ...
        + ( prim.kptlist_fold(:,2) - 0 ).^2 ...
        + ( prim.kptlist_fold(:,3) - 0 ).^2 ) ;
    [I,J] = find( prim.dis(:,ikpt) < prim.dis_gamma );
    if length(I)>0
    prim.kptlist_fold(I,1) = prim.kptlist_fold(I,1) - BZ.kptlist(ikpt,1);
    prim.kptlist_fold(I,2) = prim.kptlist_fold(I,2) - BZ.kptlist(ikpt,2);
    prim.kptlist_fold(I,3) = prim.kptlist_fold(I,3) - BZ.kptlist(ikpt,3);
    end
end

%Move KPTS into 1st BZ
conv.kptlist_fold = conv.kptlist;
conv.dis_gamma = sqrt( ( conv.kptlist(:,1) - 0 ).^2 ...
    + ( conv.kptlist(:,2) - 0 ).^2 + ( conv.kptlist(:,3) - 0 ).^2 ) ;
for ikpt = 1:BZ.NUM_KPTS
    conv.dis(:,ikpt) = sqrt( ( conv.kptlist_fold(:,1) ...
        - BZ.kptlist(ikpt,1) ).^2 + ( conv.kptlist_fold(:,2) ...
        - BZ.kptlist(ikpt,2) ).^2 + ( conv.kptlist_fold(:,3) ...
        - BZ.kptlist(ikpt,3) ).^2 ) ;
    conv.dis_gamma = sqrt( ( conv.kptlist_fold(:,1) - 0 ).^2 ...
        + ( conv.kptlist_fold(:,2) - 0 ).^2 ...
        + ( conv.kptlist_fold(:,3) - 0 ).^2 ) ;
    [I,J] = find( conv.dis(:,ikpt) < conv.dis_gamma );
    if length(I)>0
    conv.kptlist_fold(I,1) = conv.kptlist_fold(I,1) - BZ.kptlist(ikpt,1);
    conv.kptlist_fold(I,2) = conv.kptlist_fold(I,2) - BZ.kptlist(ikpt,2);
    conv.kptlist_fold(I,3) = conv.kptlist_fold(I,3) - BZ.kptlist(ikpt,3);
    end
end

%Remove degenerate points from folded in list
cnt=1;
conv.kptlist_degen(cnt,:) = conv.kptlist_fold(1,:);
conv.NUM_KPTS_DEGEN = cnt;
for ikpt = 2:conv.NUM_KPTS
    sorted = sort( sqrt( (conv.kptlist_fold(ikpt,1) ...
        - conv.kptlist_degen(:,1)).^2 + (conv.kptlist_fold(ikpt,2) ...
        - conv.kptlist_degen(:,2)).^2 + (conv.kptlist_fold(ikpt,3) ...
        - conv.kptlist_degen(:,3)).^2 ));
    [I,J] = find(sorted==0);
    sorted(I);
    %pause
    if length(I)>0
    else
        conv.kptlist_degen(cnt,:) = conv.kptlist_fold(ikpt,:);
        conv.NUM_KPTS_DEGEN = cnt;
        cnt=cnt+1;
    end
end

%Remove degenerate points from above list by checking for recip lat vec
%translations
cnt=1;
conv.kptlist_degen_2(cnt,:) = conv.kptlist_degen(1,:); ...
    %degen currently has 302 in a 256 4x, so there are degen pts left.
conv.NUM_KPTS_DEGEN_2 = cnt;
for ikpt=2:conv.NUM_KPTS_DEGEN
    degen_check=0;
    ikpt2=1;
    while ikpt2<BZ.NUM_KPTS && degen_check==0
        sorted = sort( sqrt( (conv.kptlist_degen(ikpt,1) ...
            - BZ.kptlist(ikpt2,1) - conv.kptlist_degen_2(:,1)).^2  ...
            + (conv.kptlist_degen(ikpt,2) - BZ.kptlist(ikpt2,2) ...
            - conv.kptlist_degen_2(:,2)).^2 ...
            + (conv.kptlist_degen(ikpt,3) - BZ.kptlist(ikpt2,3) ...
            - conv.kptlist_degen_2(:,3)).^2 ) );
        [I,J] = find(sorted==0);
        if length(I)>0
            degen_check=1;
        else
            degen_check=0;
        end
        ikpt2=ikpt2+1;
    end
    if degen_check==0
            cnt=cnt+1;
            conv.kptlist_degen_2(cnt,:) = conv.kptlist_degen(ikpt,:);
            conv.NUM_KPTS_DEGEN_2 = cnt;
    else
    end
end
        

        

pause

plot3(BZ.kptlist(:,1),BZ.kptlist(:,2),BZ.kptlist(:,3),'.',...
    prim.kptlist_fold(:,1),prim.kptlist_fold(:,2),...
    prim.kptlist_fold(:,3),'.')

pause

plot3(BZ.kptlist(:,1),BZ.kptlist(:,2),BZ.kptlist(:,3),'.',...
    conv.kptlist_degen_2(:,1),conv.kptlist_degen_2(:,2),...
    conv.kptlist_degen_2(:,3),'.')

pause

%Compare Alan list

% plot3(BZ.kptlist(:,1),BZ.kptlist(:,2),BZ.kptlist(:,3),'.',...
% alan.kptlist(:,1),alan.kptlist(:,2),alan.kptlist(:,3),'.')
% 
% pause
% 
% %Check Alan vs Conv
% cnt=1;
% for ikpt=1:conv.NUM_KPTS_DEGEN_2
%     [I,J] = find ( sqrt( (conv.kptlist_degen_2(ikpt,1) ...
% - alan.kptlist(:,1) ).^2  + (conv.kptlist_degen_2(ikpt,2) ...
% - alan.kptlist(:,2) ).^2 + (conv.kptlist_degen_2(ikpt,3) ...
% - alan.kptlist(:,3) ).^2 ) == 0 );
%     if length(I)==0
%         conv.kptlist_noalan(cnt,:) = conv.kptlist_degen_2(ikpt,:);
%         conv.NUM_KPTS_NOALAN = cnt;
%         cnt=cnt+1;
%     end
% end
% 
% %Check Conv vs Alan
% cnt=1;
% for ikpt=1:alan.NUM_KPTS
%     [I,J] = find ( sqrt( (alan.kptlist(ikpt,1) ...
% - conv.kptlist_degen_2(:,1) ).^2  + (alan.kptlist(ikpt,2) ...
% - conv.kptlist_degen_2(:,2) ).^2 + (alan.kptlist(ikpt,3) ...
% - conv.kptlist_degen_2(:,3) ).^2 ) == 0 );
%     if length(I)==0
%         conv.kptlist_noconv(cnt,:) = alan.kptlist(ikpt,:);
%         conv.NUM_KPTS_NOCONV = cnt;
%         cnt=cnt+1;
%     end
% end
% 
% plot3(BZ.kptlist(:,1),BZ.kptlist(:,2),BZ.kptlist(:,3),'.',...
% conv.kptlist_noalan(:,1),conv.kptlist_noalan(:,2),...
% conv.kptlist_noalan(:,3),'.',conv.kptlist_noconv(:,1),...
% conv.kptlist_noconv(:,2),conv.kptlist_noconv(:,3),'.')
% 
% pause
% 
% %Check NOALAN vs NOCONV
% for ikpt=1:conv.NUM_KPTS_NOCONV
%     no_no_check = 0;
%     check = [ conv.kptlist_noconv(ikpt,1) - conv.kptlist_noalan(:,1) ...
% conv.kptlist_noconv(ikpt,2) - conv.kptlist_noalan(:,2) ...
% conv.kptlist_noconv(ikpt,3) - conv.kptlist_noalan(:,3) ];
%     [I,J] = find( check(:,1)==1 & check(:,2)==1 & check(:,3)==1 );
%     
%     if length(I)==0
%         no_no_check = 1;
%     end
% end
% 
% pause
%         
%     
% 
% %CHECK BOUNDARY CONDITIONS
% 
% %Create the periodic positions in simulation cell images
% 
% cnt=1;
% for ix = 0:1
%     for iy = 0:1
%         for iz = 0:1
%         images.x(cnt,1:3) = [NUM_SUPERCELL*ix*alat(1) ...
% NUM_SUPERCELL*iy*alat(2) NUM_SUPERCELL*iz*alat(3)]; 
%         cnt=cnt+1;
%         end
%     end
% end
% 
% %Find the index m of the periodic image positions
% %CONV
% for iatom = 1:length(images.x)
%     position(1:3,1) = (1/alat(1))*images.x(iatom,1:3);    
%necessary scaling for prim lat vector compared to conv
%     conv.images_m(iatom,1:3) = conv.lat_vec\position;
% end   
% for ikpt=1:conv.NUM_KPTS
%     for iatom = 1:length(images.x)
%         conv.images_kptcheck(ikpt,iatom) = exp( (1i*2*pi/alat(1)) ...
% * dot( conv.kptlist(ikpt,1:3) , images.x(iatom,1:3) ) );
%     end
%     kpt(1:3,1) = conv.kptlist(ikpt,1:3);
%     conv.l(ikpt,1:3) = conv.recip_lat_vec\kpt;
% end
% for iatom = 1:length(x0)
%     position(1:3,1) = (1/alat(1))*x0(iatom,3:5);    
%necessary scaling for prim lat vector compared to conv
%     conv.m(iatom,1:3) = conv.lat_vec\position;
% end 
%     
% %PRIM
% for iatom = 1:length(images.x)
%     position(1:3,1) = (2/alat(1))*images.x(iatom,1:3);    
%necessary scaling for prim lat vector compared to conv
%     prim.images_m(iatom,1:3) = prim.lat_vec\position;
% end   
% for ikpt=1:conv.NUM_KPTS
%     for iatom = 1:length(images.x)
%         prim.images_kptcheck(ikpt,iatom) = exp( (1i*2*pi/alat(1)) ...
% * dot( prim.kptlist(ikpt,1:3) , images.x(iatom,1:3) ) );
%     end
%     kpt(1:3,1) = prim.kptlist(ikpt,1:3);
%     prim.l(ikpt,1:3) = prim.recip_lat_vec\kpt;
% end
% for iatom = 1:length(x0)
%     position(1:3,1) = (2/alat(1))*x0(iatom,3:5);    
%necessary scaling for prim lat vector compared to conv
%     prim.m(iatom,1:3) = prim.lat_vec\position;
% end 
% 
% 
% 
% %Check [1 0 0] direction
% % x100.x=[0 0 0
% % alat(1)/2 alat(1)/2 0
% % 1*alat(1) 0 0
% % 3*alat(1)/2 alat(1)/2 0
% % 2*alat(1) 0 0
% % 5*alat(1)/2 alat(1)/2 0
% % 3*alat(1) 0 0
% % 7*alat(1)/2 alat(1)/2 0
% % 4*alat(1) 0 0];
% % for iatom = 1:length(x100.x)
% %     position(1:3,1) = (2/alat(1))*x100.x(iatom,1:3);
% %     x100.m(iatom,1:3) = prim_lat_vec\position;
% % end
% %PRIM
% %exp( (i*2*pi./alat(1))*( x0(:,3)*SED.kptlist(ikpt,1)
% %CONV
% %exp( i* (x0(:,3)*(2*pi./L(1)).*(SED.kptlist(ikpt,1)))
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% pause
% % 
% % %Alan's FCC BZ algorithm to find allowed kpts
% % 
% % n=4;   %number of unit cells in x,y,z
% % 
% % Q = 341;
% % 
% % qG(1:3) = 0; qall(1:Q,1:3) = 0.0; G(1:14,1:3) = 0.0; pi = 4*atan(1.0);
% % count=1;
% % 
% % 	for i=-n:1:n
% % 		qx = i/n;
% % 		for j=-n:1:n
% % 			qy = j/n;			
% % 			if (abs(qx) + abs(qy)) <= 1.5
% % 				for k=-n:1:n
% % 					qz = k/n;
% % 					qzb = abs(1.5+qx+qy);
% % 					qzb1 = abs(1.5+qx-qy);
% %                         if qzb1 < qzb 
% %                             qzb = qzb1; 
% %                         end
% % 					qzb1 = abs(1.5-qx+qy);
% %                         if qzb1 < qzb 
% %                             qzb = qzb1;
% %                         end
% % 					qzb1 = abs(1.5-qx-qy);
% %                         if qzb1 < qzb 
% %                             qzb = qzb1; 
% %                         end
% %                         if qzb > 1.0 
% %                             qzb = 1.0;
% %                         end
% % 
% % 					qzb = qzb + 0.000000001; 
% % to account for possible rounding errors
% % 
% %                         if abs(qz) <= qzb 
% %                             check_out(count,1:3) = [qx qy qz];
% %                             count=count+1; 
% %                         end
% %                 end
% %             end
% %         end
% %     end
% %     
% %     count
% % %     
% % %     q_in = check_out;
% % %     
% % %     for i=1:Q
% % %         qall(i,1) = check_out(i,1);
% % %         qall(i,2) = check_out(i,2);
% % %         qall(i,3) = check_out(i,3);
% % %     end
% % %     
% % %   G(1,1) = 2.0;
% % % 	G(1,2) = 0.0;
% % % 	G(1,3) = 0.0;
% % % 
% % % 	G(2,1) = 0.;
% % % 	G(2,2) = 2.;
% % % 	G(2,3) = 0.;
% % % 
% % % 	G(3,1) = 0.;
% % % 	G(3,2) = 0.;
% % % 	G(3,3) = 2.;
% % % 
% % % 	G(4,1) = -2.;
% % % 	G(4,2) = 0.;
% % % 	G(4,3) = 0.;
% % % 
% % % 	G(5,1) = 0.;
% % % 	G(5,2) = -2.;
% % % 	G(5,3) = 0.;
% % % 
% % % 	G(6,1) = 0.;
% % % 	G(6,2) = 0.;
% % % 	G(6,3) = -2.;
% % % 
% % % 	G(7,1) = -1.;
% % % 	G(7,2) = -1.;
% % % 	G(7,3) = -1.;
% % % 
% % % 	G(8,1) = -1.;
% % % 	G(8,2) = -1.;
% % % 	G(8,3) = 1.;
% % % 
% % % 	G(9,1) = -1.;
% % % 	G(9,2) = 1.;
% % % 	G(9,3) = -1.;
% % % 
% % % 	G(10,1) = -1.;
% % % 	G(10,2) = 1.;
% % % 	G(10,3) = 1.;
% % % 
% % % 	G(11,1) = 1.;
% % % 	G(11,2) = -1.;
% % % 	G(11,3) = -1.;
% % % 
% % % 	G(12,1) = 1.;
% % % 	G(12,2) = -1.;
% % % 	G(12,3) = 1.;
% % % 
% % % 	G(13,1) = 1.;
% % % 	G(13,2) = 1.;
% % % 	G(13,3) = -1.;
% % % 
% % % 	G(14,1) = 1.;
% % % 	G(14,2) = 1.;
% % % 	G(14,3) = 1.;
% % %     
% % %     count = 1;
% % %     
% % %     for i=1:Q
% % %         qcheck = 1;
% % %         
% % %         for j=1:14
% % %             for k=1:3
% % %                 qG(k) = qall(i,k) + G(j,k);
% % %             end
% % %             
% % %             for k=i+1:Q
% % %                 if qG(1) >= qall(k,1) - 0.0000001 && qG(1) <= qall(k,1) + 0.0000001 && qG(2) >= qall(k,2) - 0.0000001 && qG(2) <= qall(k,2) + 0.0000001 && qG(3) >= qall(k,3) - 0.0000001 && qG(3) <= qall(k,3) + 0.0000001 
% % %                     qcheck = 0;
% % %                 end
% % %             end
% % %         end
% % %         
% % %         if qcheck == 1
% % %             data_out(count,1:3) = [qall(i,1) qall(i,2) qall(i,3)];
% % %             count=count+1;
% % %         end
% % %     end
% % %     
% % %     count
% % %     4*n*n*n
% %                 
% %                 
% %                
% %     