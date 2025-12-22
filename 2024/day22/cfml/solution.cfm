<cfscript>
/**
 * Day 22: Monkey Market - Pseudorandom number generation for market prices
 */

/**
 * Generate the next secret number using mix and prune operations.
 */
function nextSecret(secret) {
    // Step 1: multiply by 64 (left shift by 6), mix (XOR), prune (AND with 0xFFFFFF)
    secret = BitXor(secret, BitSHLN(secret, 6));
    secret = BitAnd(secret, 16777215);

    // Step 2: divide by 32 (right shift by 5), mix (XOR), prune
    secret = BitXor(secret, BitSHRN(secret, 5));
    secret = BitAnd(secret, 16777215);

    // Step 3: multiply by 2048 (left shift by 11), mix (XOR), prune
    secret = BitXor(secret, BitSHLN(secret, 11));
    secret = BitAnd(secret, 16777215);

    return secret;
}

/**
 * Generate a sequence of secret numbers.
 */
function generateSecrets(initial, count) {
    var secrets = [initial];
    var secret = initial;

    for (var i = 1; i <= count; i++) {
        secret = nextSecret(secret);
        arrayAppend(secrets, secret);
    }

    return secrets;
}

/**
 * Part 1: Sum of the 2000th secret number for each buyer.
 */
function part1(initialSecrets) {
    var total = 0;

    for (var initial in initialSecrets) {
        var secret = initial;
        for (var i = 1; i <= 2000; i++) {
            secret = nextSecret(secret);
        }
        total += secret;
    }

    return total;
}

/**
 * Part 2: Find the best sequence of 4 price changes to maximize bananas.
 */
function part2(initialSecrets) {
    // Map from sequence (change1,change2,change3,change4) -> total bananas
    var sequenceTotals = {};

    for (var initial in initialSecrets) {
        // Generate 2001 secrets (initial + 2000 new)
        var secrets = generateSecrets(initial, 2000);

        // Calculate prices (last digit of each secret)
        var prices = [];
        for (var secret in secrets) {
            arrayAppend(prices, secret mod 10);
        }

        // Calculate changes
        var changes = [];
        for (var i = 1; i <= arrayLen(prices) - 1; i++) {
            arrayAppend(changes, prices[i + 1] - prices[i]);
        }

        // Track first occurrence of each 4-change sequence for this buyer
        var seen = {};
        for (var i = 1; i <= arrayLen(changes) - 3; i++) {
            // Create sequence key
            var seqKey = changes[i] & "," & changes[i+1] & "," & changes[i+2] & "," & changes[i+3];

            if (!structKeyExists(seen, seqKey)) {
                seen[seqKey] = true;

                // Price we get is after these 4 changes
                var price = prices[i + 4];

                if (structKeyExists(sequenceTotals, seqKey)) {
                    sequenceTotals[seqKey] += price;
                } else {
                    sequenceTotals[seqKey] = price;
                }
            }
        }
    }

    // Find maximum value
    var maxBananas = 0;
    for (var key in sequenceTotals) {
        if (sequenceTotals[key] > maxBananas) {
            maxBananas = sequenceTotals[key];
        }
    }

    return maxBananas;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputFile = scriptDir & "../input.txt";
var inputText = fileRead(inputFile);
var lines = listToArray(inputText, chr(10));

// Parse initial secrets
var initialSecrets = [];
for (var line in lines) {
    line = trim(line);
    if (len(line) > 0) {
        arrayAppend(initialSecrets, val(line));
    }
}

writeOutput("Part 1: " & part1(initialSecrets) & chr(10));
writeOutput("Part 2: " & part2(initialSecrets) & chr(10));
</cfscript>
