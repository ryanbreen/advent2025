<cfscript>
// Day 8: Playground - ColdFusion (CFML) Solution

// Parse input file
function parseInput(filename) {
    var points = [];
    var fileContent = fileRead(filename);
    var lines = listToArray(fileContent, chr(10) & chr(13));

    for (var line in lines) {
        line = trim(line);
        if (len(line) > 0) {
            var parts = listToArray(line, ",");
            arrayAppend(points, {
                x: int(parts[1]),
                y: int(parts[2]),
                z: int(parts[3])
            });
        }
    }

    return points;
}

// Union-Find helper functions
function ufFindRoot(uf, x) {
    if (uf.parent[x] != x) {
        uf.parent[x] = ufFindRoot(uf, uf.parent[x]);
    }
    return uf.parent[x];
}

function ufUnion(uf, x, y) {
    var px = ufFindRoot(uf, x);
    var py = ufFindRoot(uf, y);

    if (px == py) {
        return false;
    }

    // Union by rank
    if (uf.rank[px] < uf.rank[py]) {
        var temp = px;
        px = py;
        py = temp;
    }

    uf.parent[py] = px;
    uf.size[px] += uf.size[py];

    if (uf.rank[px] == uf.rank[py]) {
        uf.rank[px]++;
    }

    return true;
}

function ufGetComponentSizes(uf) {
    var sizes = [];
    for (var i = 1; i <= uf.n; i++) {
        if (uf.parent[i] == i) {
            arrayAppend(sizes, uf.size[i]);
        }
    }
    return sizes;
}

// Create Union-Find structure
function createUnionFind(n) {
    var uf = {
        parent: [],
        rank: [],
        size: [],
        n: n
    };

    for (var i = 1; i <= n; i++) {
        arrayAppend(uf.parent, i);
        arrayAppend(uf.rank, 0);
        arrayAppend(uf.size, 1);
    }

    return uf;
}

// Calculate squared Euclidean distance
function euclideanDistanceSq(p1, p2) {
    var dx = p1.x - p2.x;
    var dy = p1.y - p2.y;
    var dz = p1.z - p2.z;
    return dx * dx + dy * dy + dz * dz;
}

// Part 1: Connect 1000 closest pairs
function part1(points) {
    var n = arrayLen(points);
    var pairs = [];

    // Generate all pairs with distances
    for (var i = 1; i <= n; i++) {
        for (var j = i + 1; j <= n; j++) {
            var distSq = euclideanDistanceSq(points[i], points[j]);
            arrayAppend(pairs, {
                distSq: distSq,
                i: i,
                j: j
            });
        }
    }

    // Sort by distance
    arraySort(pairs, function(a, b) {
        if (a.distSq < b.distSq) return -1;
        if (a.distSq > b.distSq) return 1;
        return 0;
    });

    // Union-Find to connect closest pairs
    var uf = createUnionFind(n);
    var connections = 0;

    for (var pair in pairs) {
        ufUnion(uf, pair.i, pair.j);
        connections++;
        if (connections == 1000) {
            break;
        }
    }

    // Get component sizes and find the 3 largest
    var sizes = ufGetComponentSizes(uf);
    arraySort(sizes, "numeric", "desc");

    return sizes[1] * sizes[2] * sizes[3];
}

// Part 2: Connect until all in one circuit
function part2(points) {
    var n = arrayLen(points);
    var pairs = [];

    // Generate all pairs with distances
    for (var i = 1; i <= n; i++) {
        for (var j = i + 1; j <= n; j++) {
            var distSq = euclideanDistanceSq(points[i], points[j]);
            arrayAppend(pairs, {
                distSq: distSq,
                i: i,
                j: j
            });
        }
    }

    // Sort by distance
    arraySort(pairs, function(a, b) {
        if (a.distSq < b.distSq) return -1;
        if (a.distSq > b.distSq) return 1;
        return 0;
    });

    // Union-Find to connect until all in one circuit
    var uf = createUnionFind(n);
    var numComponents = n;

    for (var pair in pairs) {
        if (ufUnion(uf, pair.i, pair.j)) {
            numComponents--;
            if (numComponents == 1) {
                // Last connection - return product of X coordinates
                return points[pair.i].x * points[pair.j].x;
            }
        }
    }

    return 0;
}

// Main execution
try {
    // Get input file from command-line arguments or use default
    var inputFile = "../input.txt";

    // CommandBox CLI arguments are available in the arguments scope
    if (structKeyExists(arguments, "1")) {
        inputFile = arguments["1"];
    } else if (structKeyExists(url, "1")) {
        // Alternative way to access CLI args in CommandBox
        inputFile = url["1"];
    } else if (structKeyExists(form, "inputFile")) {
        inputFile = form.inputFile;
    }

    var points = parseInput(inputFile);

    writeOutput("Part 1: " & part1(points) & chr(10));
    writeOutput("Part 2: " & part2(points) & chr(10));
} catch (any e) {
    writeOutput("Error: " & e.message & chr(10));
    writeOutput("Detail: " & e.detail & chr(10));
}
</cfscript>
