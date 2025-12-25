<cfscript>
function parseInput(filename) {
    var graph = {};
    var lines = fileRead(filename).split(chr(10));

    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        var parts = line.split('-');
        var a = parts[1];
        var b = parts[2];

        if (!structKeyExists(graph, a)) {
            graph[a] = {};
        }
        if (!structKeyExists(graph, b)) {
            graph[b] = {};
        }

        graph[a][b] = true;
        graph[b][a] = true;
    }

    return graph;
}

function findTriangles(graph) {
    var triangles = {};

    for (var a in graph) {
        for (var b in graph[a]) {
            if (a < b) {  // Only process each edge once
                // Find common neighbors (intersection of graph[a] and graph[b])
                for (var c in graph[a]) {
                    if (structKeyExists(graph[b], c)) {
                        // Create sorted triangle key
                        var nodes = [a, b, c];
                        nodes.sort("textnocase");
                        var key = nodes.toList(",");
                        triangles[key] = true;
                    }
                }
            }
        }
    }

    return triangles;
}

function part1(graph) {
    var triangles = findTriangles(graph);
    var count = 0;

    for (var tri in triangles) {
        var nodes = tri.split(',');
        for (var node in nodes) {
            if (left(node, 1) == 't') {
                count++;
                break;
            }
        }
    }

    return count;
}

function setIntersection(set1, set2) {
    var result = {};
    for (var key in set1) {
        if (structKeyExists(set2, key)) {
            result[key] = true;
        }
    }
    return result;
}

function setUnion(set1, set2) {
    var result = duplicate(set1);
    for (var key in set2) {
        result[key] = true;
    }
    return result;
}

function setDifference(set1, set2) {
    var result = {};
    for (var key in set1) {
        if (!structKeyExists(set2, key)) {
            result[key] = true;
        }
    }
    return result;
}

function isSetEmpty(set) {
    return structCount(set) == 0;
}

function bronKerbosch(graph, r, p, x, cliques) {
    if (isSetEmpty(p) && isSetEmpty(x)) {
        arrayAppend(cliques, duplicate(r));
        return;
    }

    // Find pivot with maximum neighbors in p
    var unionPX = setUnion(p, x);
    var pivot = "";
    var maxNeighbors = 0;

    for (var v in unionPX) {
        var neighbors = setIntersection(graph[v], p);
        if (structCount(neighbors) > maxNeighbors) {
            maxNeighbors = structCount(neighbors);
            pivot = v;
        }
    }

    // p - graph[pivot]
    var pivotNeighbors = structKeyExists(graph, pivot) ? graph[pivot] : {};
    var pMinusPivotNeighbors = setDifference(p, pivotNeighbors);

    // Need to iterate over a copy since we modify p
    var nodesToProcess = [];
    for (var v in pMinusPivotNeighbors) {
        arrayAppend(nodesToProcess, v);
    }

    for (var v in nodesToProcess) {
        var newR = duplicate(r);
        newR[v] = true;

        var newP = setIntersection(p, graph[v]);
        var newX = setIntersection(x, graph[v]);

        bronKerbosch(graph, newR, newP, newX, cliques);

        structDelete(p, v);
        x[v] = true;
    }
}

function part2(graph) {
    var cliques = [];
    var allNodes = {};

    for (var node in graph) {
        allNodes[node] = true;
    }

    bronKerbosch(graph, {}, allNodes, {}, cliques);

    // Find largest clique
    var largest = [];
    var maxSize = 0;

    for (var clique in cliques) {
        var size = structCount(clique);
        if (size > maxSize) {
            maxSize = size;
            largest = [];
            for (var node in clique) {
                arrayAppend(largest, node);
            }
        }
    }

    // Sort and join
    largest.sort("textnocase");
    return largest.toList(",");
}

function main() {
    var graph = parseInput('../input.txt');

    writeOutput('Part 1: ' & part1(graph) & chr(10));
    writeOutput('Part 2: ' & part2(graph) & chr(10));
}

main();
</cfscript>
