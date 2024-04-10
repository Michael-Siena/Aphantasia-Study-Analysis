function pseudorandData = Pseudorand(data, nReps, nConds, condInds)
    %% Get trial number
    [nTrials, ~] = size(data);
    
    %% Initialise sliding window
    windowSize = nReps + 1;
    windowSeq = 0:windowSize;
    
    %% Seed randomisation
    pseudorandSeed = data(randperm(nTrials), :);
    
    %% Pseudorand algorithm
    trialInd = 1;
    while trialInd <= (nTrials - nReps)        
        pseudorandTmp = pseudorandSeed;
                
        % Populate sliding window
        window = cell(windowSize, nConds);
        for i = 1:windowSize
            window(i, :) = pseudorandTmp(trialInd + windowSeq(i), condInds);
        end
        % Check whether number of allowed repetitions has been exceeded
        condInd = 1;
        while condInd <= nConds
            isUnique = (windowSize - nReps) == length(unique(string(window(:, condInd))));
            if isUnique
                pseudorandSeed = data(randperm(nTrials), :);
                trialInd = 1;
                break
            end
            condInd = condInd + 1;
        end
        if trialInd == (nTrials - nReps)
            pseudorandData = pseudorandTmp;
            return
        end
        trialInd = trialInd + 1;
    end
end

