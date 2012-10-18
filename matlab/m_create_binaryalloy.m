function [id,m] = m_create_binaryalloy( mass, NUM_ATOMS, alloy_conc, alloy_seed )

%randomize masses: only set up to do 2 species
    id(:,1) = 1:NUM_ATOMS;
    m(1:NUM_ATOMS,1) = 1;
%rng(NMD.seed.alloy(NMD.seed.alloy));
    rng( alloy_seed );
    [I,J] =...
        find(...
        randperm(NUM_ATOMS)'<ceil(alloy_conc*NUM_ATOMS));
    m(I,1) = 2;
%set mass vector
for imass = 1:size(mass,2)
Imass = find(m==imass); m(Imass) = imass;
end

end
