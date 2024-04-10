function coords = GenerateCoordinates(x0, z0, sectors, radius, nCoords)    
    coords = cell(nCoords/sectors, 2, sectors);

    for sect = 1:sectors
        angle1 = deg2rad((sect - 1) * (360/sectors));
        angle2 = deg2rad(sect * (360/sectors));

        t = (angle2 - angle1) * rand(nCoords/sectors, 1) + angle1;
        r = radius*sqrt(rand(nCoords/sectors, 1));
        coord_x = x0 + r.*cos(t);
        coord_z = z0 + r.*sin(t);

        coords(:, :, sect) = num2cell([coord_x, coord_z]);
    end
end