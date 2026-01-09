component {

    variables.cubes = {};
    variables.DIRECTIONS = [
        {dx: 1, dy: 0, dz: 0},
        {dx: -1, dy: 0, dz: 0},
        {dx: 0, dy: 1, dz: 0},
        {dx: 0, dy: -1, dz: 0},
        {dx: 0, dy: 0, dz: 1},
        {dx: 0, dy: 0, dz: -1}
    ];

    public function init() {
        parseInput();
        return this;
    }

    private function makeKey(required numeric x, required numeric y, required numeric z) {
        return "#x#,#y#,#z#";
    }

    private function parseInput() {
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputPath = scriptDir & "../input.txt";
        var content = fileRead(inputPath);
        var lines = listToArray(trim(content), chr(10));

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var parts = listToArray(line, ",");
            var x = int(parts[1]);
            var y = int(parts[2]);
            var z = int(parts[3]);
            var key = makeKey(x, y, z);
            variables.cubes[key] = {x: x, y: y, z: z};
        }
    }

    public function part1() {
        var surfaceArea = 0;

        for (var key in variables.cubes) {
            var cube = variables.cubes[key];

            for (var dir in variables.DIRECTIONS) {
                var neighborKey = makeKey(cube.x + dir.dx, cube.y + dir.dy, cube.z + dir.dz);
                if (!structKeyExists(variables.cubes, neighborKey)) {
                    surfaceArea++;
                }
            }
        }

        return surfaceArea;
    }

    public function part2() {
        // Find bounding box with 1 unit padding
        var minX = 999999;
        var maxX = -999999;
        var minY = 999999;
        var maxY = -999999;
        var minZ = 999999;
        var maxZ = -999999;

        for (var key in variables.cubes) {
            var cube = variables.cubes[key];
            minX = min(minX, cube.x);
            maxX = max(maxX, cube.x);
            minY = min(minY, cube.y);
            maxY = max(maxY, cube.y);
            minZ = min(minZ, cube.z);
            maxZ = max(maxZ, cube.z);
        }

        minX--;
        maxX++;
        minY--;
        maxY++;
        minZ--;
        maxZ++;

        // BFS to find all exterior air cells
        var exterior = {};
        var queue = [];

        var startKey = makeKey(minX, minY, minZ);
        exterior[startKey] = true;
        arrayAppend(queue, {x: minX, y: minY, z: minZ});

        while (arrayLen(queue) > 0) {
            var current = queue[1];
            arrayDeleteAt(queue, 1);

            for (var dir in variables.DIRECTIONS) {
                var nx = current.x + dir.dx;
                var ny = current.y + dir.dy;
                var nz = current.z + dir.dz;

                // Stay within bounds
                if (nx < minX || nx > maxX || ny < minY || ny > maxY || nz < minZ || nz > maxZ) {
                    continue;
                }

                var neighborKey = makeKey(nx, ny, nz);

                // Skip cubes and already visited
                if (structKeyExists(variables.cubes, neighborKey) || structKeyExists(exterior, neighborKey)) {
                    continue;
                }

                exterior[neighborKey] = true;
                arrayAppend(queue, {x: nx, y: ny, z: nz});
            }
        }

        // Count faces touching exterior air
        var surfaceArea = 0;
        for (var key in variables.cubes) {
            var cube = variables.cubes[key];

            for (var dir in variables.DIRECTIONS) {
                var neighborKey = makeKey(cube.x + dir.dx, cube.y + dir.dy, cube.z + dir.dz);
                if (structKeyExists(exterior, neighborKey)) {
                    surfaceArea++;
                }
            }
        }

        return surfaceArea;
    }

}
