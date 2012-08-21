%THIS ONE DOES NOT WORK

str_main='I:\research';
NUM_CHUNKS = 10;    %SPECIFY THE NUMBER OF CHUNKS
load kptlist.dat         
A=kptlist(:,:)
n = length(A);
ordering = randperm(n);
C = A(ordering,:)
CHUNK_SIZE=round(length(C)/NUM_CHUNKS);    %DENOTE SIZE OF CHUNKS
str_last='.m';
for iN=1:NUM_CHUNKS
    B=C((iN-1)*CHUNK_SIZE+1:iN*CHUNK_SIZE);
    str_write = [str_main int2str(iN) str_last];
    dlmwrite(str_write,B);
end

%[I,J] = find(C(1,1)==A(:,1) & C(1,2)==A(:,2) & C(1,3)==A(:,3));

%[I] = find(X(:,1)~=0);