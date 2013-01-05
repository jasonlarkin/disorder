str.main='D:\CMU\work\matlab\steven\';
kpt.NUM_CHUNKS = 5;    %SPECIFY THE NUMBER OF CHUNKS
load kptlist.dat;         
kpt.list=kptlist(:,:);
%kpt.list(:,4)=1:length(kpt.list); kpt.list(:,5)=1:length(kpt.list);
kpt.length = length(kpt.list);
kpt.mixup = randperm(kpt.length);
kpt.listmix = kpt.list(kpt.mixup,:);
%Up to KptMix, everything works!
kpt.CHUNK_SIZE=floor(length(kpt.listmix)/kpt.NUM_CHUNKS);    %DENOTE SIZE OF CHUNKS
str.last='.dat';
for iN=1:kpt.NUM_CHUNKS
    %The part below works. Just the writing of KptMix from ChunkedKpt that needs work    
    if iN ==kpt.NUM_CHUNKS
        kpt.CHUNK_BUFFER = kpt.length-floor(kpt.length/(kpt.NUM_CHUNKS))*kpt.NUM_CHUNKS;
        kpt.chunk=kpt.listmix((iN-1)*kpt.CHUNK_SIZE+1:(iN)*kpt.CHUNK_SIZE+kpt.CHUNK_BUFFER,:);
    else
        kpt.chunk=kpt.listmix((iN-1)*kpt.CHUNK_SIZE+1:(iN)*kpt.CHUNK_SIZE,:);
    end
    str.write = strcat(str.main,'kptlist_',int2str(iN),'.dat');
    dlmwrite(str.write,kpt.chunk(:,1:3));
end

