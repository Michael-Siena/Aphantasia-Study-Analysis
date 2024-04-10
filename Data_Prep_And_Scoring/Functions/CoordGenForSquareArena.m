function coords = CoordGenForSquareArena(nPairs, sideLength)
    coords = cell(nPairs, 2);
    
    x = rand(1, nPairs) * sideLength;
    y = rand(1, nPairs) * sideLength;
    
    scatter(x, y);
    
    coords(:, 1) = num2cell(x);
    coords(:, 2) = num2cell(y);
end