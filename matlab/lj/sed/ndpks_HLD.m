function [HLDndpks,numndpks] = ndpks_HLD(HLDpks,DEGEN_WIDTH)
    numndpks = 1;
    HLDndpks(1) = HLDpks(1);
    for j=2:length(HLDpks) %should be equal to 12
        degencount = 0;
        for k=1:numndpks
            if abs(HLDpks(j)-HLDndpks(k)) < DEGEN_WIDTH
                degencount = 1;
            end
        end
        if degencount == 0
            numndpks = numndpks +1;
            HLDndpks(numndpks) = HLDpks(j);
        end
    end
end