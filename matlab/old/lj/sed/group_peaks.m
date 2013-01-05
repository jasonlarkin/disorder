function group_loc = group_peaks(loc,groupdis)
    sup_peak_dist = groupdis;
    clear fitpks
    for i=1:length(loc)-1
        dist(i) = loc(i+1) - loc(i);
    end

    id = 1;
    for i=1:length(loc) - 1
        fitpks(i) = id;
        if (dist(i) > sup_peak_dist)
            id = id + 1;
        end
    end
    fitpks(i + 1) = id;
    group_loc = fitpks; 
end