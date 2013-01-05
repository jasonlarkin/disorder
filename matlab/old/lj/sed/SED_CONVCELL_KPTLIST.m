
NUM_CELL_COPIES = 6;
IKPT = 1;
for i1=-(NUM_CELL_COPIES/2-1):1:(NUM_CELL_COPIES/2)
    for i2=-(NUM_CELL_COPIES/2-1):1:(NUM_CELL_COPIES/2)
        for i3=-(NUM_CELL_COPIES/2-1):1:(NUM_CELL_COPIES/2)
            kptlist(IKPT,1) = i1;
            kptlist(IKPT,2) = i2;
            kptlist(IKPT,3) = i3;
            IKPT = IKPT+1;
        end
    end
end

            