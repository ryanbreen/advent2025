component {

    /**
     * Advent of Code 2023 - Day 25: Snowverload
     * Find the minimum cut of 3 edges that divides the graph into two components.
     *
     * Uses edge betweenness centrality: edges that form the cut between two large
     * components will have high betweenness (many shortest paths pass through them).
     */

    function run() {
        // Get the directory of this file and construct path to input.txt
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";

        writeOutput("Part 1: " & part1(inputFile) & chr(10));
        writeOutput("Part 2: " & part2(inputFile) & chr(10));
    }

    function part1(required string filename) {
        var graph = parseInput(filename);
        return findCutEdges(graph);
    }

    function part2(required string filename) {
        return "Push the big red button!";
    }

    /**
     * Parse the input file into an adjacency list representation.
     */
    function parseInput(required string filename) {
        var graph = {};
        var lines = fileRead(filename).listToArray(chr(10));

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var parts = line.split(": ");
            var left = parts[1];
            var neighbors = parts[2].split(" ");

            if (!structKeyExists(graph, left)) {
                graph[left] = {};
            }

            for (var neighbor in neighbors) {
                neighbor = trim(neighbor);
                if (!structKeyExists(graph, left)) {
                    graph[left] = {};
                }
                if (!structKeyExists(graph, neighbor)) {
                    graph[neighbor] = {};
                }
                graph[left][neighbor] = true;
                graph[neighbor][left] = true;
            }
        }

        return graph;
    }

    /**
     * BFS to find component size, ignoring excluded edges.
     */
    function bfsComponentSize(required struct graph, required string start, required struct excludedEdges) {
        var visited = {};
        visited[start] = true;
        var queue = [start];

        while (arrayLen(queue) > 0) {
            var currentNode = queue[1];
            arrayDeleteAt(queue, 1);

            if (!structKeyExists(graph, currentNode)) continue;

            var neighbors = structKeyArray(graph[currentNode]);
            for (var i = 1; i <= arrayLen(neighbors); i++) {
                var neighbor = neighbors[i];
                var edge = makeEdgeKey(currentNode, neighbor);

                if (!structKeyExists(visited, neighbor) && !structKeyExists(excludedEdges, edge)) {
                    visited[neighbor] = true;
                    arrayAppend(queue, neighbor);
                }
            }
        }

        return structCount(visited);
    }

    /**
     * Create a normalized edge key (sorted nodes).
     */
    function makeEdgeKey(required string node1, required string node2) {
        if (compare(node1, node2) < 0) {
            return node1 & "|" & node2;
        } else {
            return node2 & "|" & node1;
        }
    }

    /**
     * Compute approximate edge betweenness centrality.
     * Higher values indicate edges that many shortest paths pass through.
     */
    function computeEdgeBetweenness(required struct graph, numeric sampleNodes = 100) {
        var edgeCount = {};
        var nodes = structKeyArray(graph);

        // Sample nodes for efficiency
        if (sampleNodes > 0 && arrayLen(nodes) > sampleNodes) {
            // Seed random for reproducibility
            var random = createObject("java", "java.util.Random").init(42);
            var sampled = [];
            var nodesCopy = duplicate(nodes);

            for (var i = 1; i <= sampleNodes; i++) {
                var idx = random.nextInt(arrayLen(nodesCopy)) + 1;
                arrayAppend(sampled, nodesCopy[idx]);
                arrayDeleteAt(nodesCopy, idx);
            }
            nodes = sampled;
        }

        for (var sourceIdx = 1; sourceIdx <= arrayLen(nodes); sourceIdx++) {
            var source = nodes[sourceIdx];

            // BFS to find shortest paths
            var dist = {};
            dist[source] = 0;
            var pred = {};
            var queue = [source];

            while (arrayLen(queue) > 0) {
                var currentNode = queue[1];
                arrayDeleteAt(queue, 1);

                if (!structKeyExists(graph, currentNode)) continue;

                var neighbors = structKeyArray(graph[currentNode]);
                for (var nIdx = 1; nIdx <= arrayLen(neighbors); nIdx++) {
                    var neighbor = neighbors[nIdx];

                    if (!structKeyExists(dist, neighbor)) {
                        dist[neighbor] = dist[currentNode] + 1;
                        pred[neighbor] = [currentNode];
                        arrayAppend(queue, neighbor);
                    } else if (dist[neighbor] == dist[currentNode] + 1) {
                        arrayAppend(pred[neighbor], currentNode);
                    }
                }
            }

            // Number of shortest paths to each node
            var numPaths = {};
            numPaths[source] = 1.0;

            // Sort nodes by distance
            var sortedNodes = structKeyArray(dist);
            arraySort(sortedNodes, function(a, b) {
                return dist[a] - dist[b];
            });

            for (var snIdx = 1; snIdx <= arrayLen(sortedNodes); snIdx++) {
                var snode = sortedNodes[snIdx];

                if (!structKeyExists(numPaths, snode)) {
                    numPaths[snode] = 0.0;
                }
                if (structKeyExists(pred, snode)) {
                    for (var pIdx = 1; pIdx <= arrayLen(pred[snode]); pIdx++) {
                        var p = pred[snode][pIdx];
                        numPaths[snode] += numPaths[p];
                    }
                }
            }

            // Accumulate edge betweenness (reverse order)
            var dependency = {};
            arraySort(sortedNodes, function(a, b) {
                return dist[b] - dist[a];
            });

            for (var snIdx2 = 1; snIdx2 <= arrayLen(sortedNodes); snIdx2++) {
                var snode2 = sortedNodes[snIdx2];

                if (!structKeyExists(dependency, snode2)) {
                    dependency[snode2] = 0.0;
                }
                if (structKeyExists(pred, snode2)) {
                    for (var pIdx2 = 1; pIdx2 <= arrayLen(pred[snode2]); pIdx2++) {
                        var p2 = pred[snode2][pIdx2];
                        var edge = makeEdgeKey(p2, snode2);
                        var frac = numPaths[p2] / numPaths[snode2];
                        var contrib = frac * (1 + dependency[snode2]);

                        if (!structKeyExists(edgeCount, edge)) {
                            edgeCount[edge] = 0.0;
                        }
                        edgeCount[edge] += contrib;

                        if (!structKeyExists(dependency, p2)) {
                            dependency[p2] = 0.0;
                        }
                        dependency[p2] += contrib;
                    }
                }
            }
        }

        return edgeCount;
    }

    /**
     * Find the 3 edges to cut using edge betweenness.
     */
    function findCutEdges(required struct graph) {
        // Compute edge betweenness with sampling for speed
        var edgeBetweenness = computeEdgeBetweenness(graph, 100);

        // Sort edges by betweenness (highest first)
        var edges = structKeyArray(edgeBetweenness);
        arraySort(edges, function(a, b) {
            return edgeBetweenness[b] - edgeBetweenness[a] > 0 ? 1 : -1;
        });

        var totalNodes = structCount(graph);

        // Try removing top candidate edges
        var topEdges = [];
        for (var i = 1; i <= min(20, arrayLen(edges)); i++) {
            arrayAppend(topEdges, edges[i]);
        }

        // Try combinations of 3 edges
        for (var i = 1; i <= arrayLen(topEdges); i++) {
            for (var j = i + 1; j <= arrayLen(topEdges); j++) {
                for (var k = j + 1; k <= arrayLen(topEdges); k++) {
                    var excluded = {};
                    excluded[topEdges[i]] = true;
                    excluded[topEdges[j]] = true;
                    excluded[topEdges[k]] = true;

                    var startNode = structKeyArray(graph)[1];
                    var size1 = bfsComponentSize(graph, startNode, excluded);

                    if (size1 < totalNodes) {
                        // Graph is disconnected!
                        var size2 = totalNodes - size1;
                        return size1 * size2;
                    }
                }
            }
        }

        return 0;
    }

}
