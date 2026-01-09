import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir")).resolve("../input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        Map<String, Long> dirSizes = parseFilesystem(lines);

        System.out.println("Part 1: " + part1(dirSizes));
        System.out.println("Part 2: " + part2(dirSizes));
    }

    private static Map<String, Long> parseFilesystem(List<String> lines) {
        List<String> path = new ArrayList<>();
        Map<String, Long> dirSizes = new HashMap<>();

        for (String line : lines) {
            if (line.startsWith("$ cd")) {
                String target = line.substring(5);
                if (target.equals("/")) {
                    path.clear();
                    path.add("/");
                } else if (target.equals("..")) {
                    path.remove(path.size() - 1);
                } else {
                    path.add(target);
                }
            } else if (line.startsWith("$ ls") || line.startsWith("dir ")) {
                continue;
            } else {
                // It's a file with size
                String[] parts = line.split(" ");
                long size = Long.parseLong(parts[0]);

                // Add size to current directory and all parent directories
                for (int i = 0; i < path.size(); i++) {
                    String dirPath = String.join("/", path.subList(0, i + 1));
                    if (dirPath.isEmpty()) {
                        dirPath = "/";
                    }
                    dirSizes.merge(dirPath, size, Long::sum);
                }
            }
        }

        return dirSizes;
    }

    private static long part1(Map<String, Long> dirSizes) {
        return dirSizes.values().stream()
            .filter(size -> size <= 100000)
            .mapToLong(Long::longValue)
            .sum();
    }

    private static long part2(Map<String, Long> dirSizes) {
        long totalSpace = 70000000;
        long neededSpace = 30000000;
        long usedSpace = dirSizes.get("/");
        long freeSpace = totalSpace - usedSpace;
        long needToFree = neededSpace - freeSpace;

        return dirSizes.values().stream()
            .filter(size -> size >= needToFree)
            .mapToLong(Long::longValue)
            .min()
            .orElse(0);
    }
}
