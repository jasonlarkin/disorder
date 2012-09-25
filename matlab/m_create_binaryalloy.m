function [id,m] = m_create_binaryalloy( mass, pos, alloy )
%randomize masses: only set up to do 2 species
    id(:,1) = 1:size(pos,1);
    m(1:size(pos,1),1) = 1;
%rng(NMD.seed.alloy(NMD.seed.alloy));
    rng( alloy.seed );
    [I,J] =...
        find(...
        randperm(size(pos,1))'<ceil(alloy.conc*size(pos,1)));
    m(I,1) = 2;
%set mass vector
for imass = 1:size(mass,2)
Imass = find(m==imass); m(Imass) = imass;
end

end
