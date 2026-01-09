component {

    /**
     * Convert a SNAFU digit to its decimal value
     */
    private numeric function snafuDigitValue(required string digit) {
        switch (digit) {
            case "2": return 2;
            case "1": return 1;
            case "0": return 0;
            case "-": return -1;
            case "=": return -2;
            default: throw("Invalid SNAFU digit: #digit#");
        }
    }

    /**
     * Convert a SNAFU string to decimal number
     */
    public numeric function snafuToDecimal(required string snafu) {
        var result = 0;
        var chars = listToArray(snafu, "");

        for (var char in chars) {
            result = result * 5 + snafuDigitValue(char);
        }

        return result;
    }

    /**
     * Convert a decimal number to SNAFU string
     */
    public string function decimalToSnafu(required numeric n) {
        if (n == 0) {
            return "0";
        }

        var digits = [];
        var num = n;

        while (num != 0) {
            var remainder = num mod 5;

            if (remainder <= 2) {
                arrayAppend(digits, toString(remainder));
                num = int(num / 5);
            } else if (remainder == 3) {
                arrayAppend(digits, "=");
                num = int(num / 5) + 1;
            } else {
                // remainder == 4
                arrayAppend(digits, "-");
                num = int(num / 5) + 1;
            }
        }

        // Reverse the array and join
        var result = "";
        for (var i = arrayLen(digits); i >= 1; i--) {
            result &= digits[i];
        }

        return result;
    }

    /**
     * Solve Part 1: Sum all SNAFU numbers and return as SNAFU
     */
    public string function part1(required string input) {
        var lines = listToArray(trim(input), chr(10));
        var total = 0;

        for (var line in lines) {
            line = trim(line);
            if (len(line) > 0) {
                total += snafuToDecimal(line);
            }
        }

        return decimalToSnafu(total);
    }

}
