use std::fs;

fn is_invalid_id(num: u64) -> bool {
    let s = num.to_string();
    let len = s.len();

    // Try all possible pattern lengths (1 to len/2)
    // A pattern repeated at least twice means pattern_len * repetitions = len, where repetitions >= 2
    for pattern_len in 1..=len/2 {
        // Check if the length is divisible by the pattern length
        if len % pattern_len != 0 {
            continue;
        }

        let repetitions = len / pattern_len;
        if repetitions < 2 {
            continue;
        }

        let pattern = &s[..pattern_len];

        // Check for leading zeroes
        if pattern.starts_with('0') {
            continue;
        }

        // Check if the entire string is this pattern repeated
        let mut is_repeated = true;
        for i in 1..repetitions {
            let chunk = &s[i * pattern_len..(i + 1) * pattern_len];
            if chunk != pattern {
                is_repeated = false;
                break;
            }
        }

        if is_repeated {
            return true;
        }
    }

    false
}

fn count_invalid_ids_in_range(start: u64, end: u64) -> u64 {
    let mut sum = 0;
    for id in start..=end {
        if is_invalid_id(id) {
            sum += id;
        }
    }
    sum
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let input = input.trim();

    // Parse the ranges
    let mut total_sum = 0u64;

    for range in input.split(',') {
        let range = range.trim();
        if range.is_empty() {
            continue;
        }

        let parts: Vec<&str> = range.split('-').collect();
        if parts.len() != 2 {
            eprintln!("Invalid range format: {}", range);
            continue;
        }

        let start: u64 = parts[0].parse().expect("Failed to parse start");
        let end: u64 = parts[1].parse().expect("Failed to parse end");

        let range_sum = count_invalid_ids_in_range(start, end);
        total_sum += range_sum;
    }

    println!("Part 2: {}", total_sum);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_invalid_id() {
        // Part 2 examples - patterns repeated at least twice
        assert!(is_invalid_id(11)); // "1" repeated 2 times
        assert!(is_invalid_id(22)); // "2" repeated 2 times
        assert!(is_invalid_id(99)); // "9" repeated 2 times
        assert!(is_invalid_id(111)); // "1" repeated 3 times
        assert!(is_invalid_id(999)); // "9" repeated 3 times
        assert!(is_invalid_id(1010)); // "10" repeated 2 times
        assert!(is_invalid_id(6464)); // "64" repeated 2 times
        assert!(is_invalid_id(123123)); // "123" repeated 2 times
        assert!(is_invalid_id(123123123)); // "123" repeated 3 times
        assert!(is_invalid_id(1212121212)); // "12" repeated 5 times
        assert!(is_invalid_id(1111111)); // "1" repeated 7 times
        assert!(is_invalid_id(222222)); // "222" repeated 2 times or "22" repeated 3 times or "2" repeated 6 times
        assert!(is_invalid_id(446446)); // "446" repeated 2 times
        assert!(is_invalid_id(1188511885)); // "11885" repeated 2 times
        assert!(is_invalid_id(38593859)); // "3859" repeated 2 times
        assert!(is_invalid_id(565656)); // "56" repeated 3 times
        assert!(is_invalid_id(824824824)); // "824" repeated 3 times
        assert!(is_invalid_id(2121212121)); // "21" repeated 5 times

        assert!(!is_invalid_id(101)); // not a repeated pattern
        assert!(!is_invalid_id(12)); // not a repeated pattern
        assert!(!is_invalid_id(1234)); // not a repeated pattern
    }

    #[test]
    fn test_example() {
        // Test example ranges for Part 2
        assert_eq!(count_invalid_ids_in_range(11, 22), 11 + 22); // 11, 22
        assert_eq!(count_invalid_ids_in_range(95, 115), 99 + 111); // 99, 111
        assert_eq!(count_invalid_ids_in_range(998, 1012), 999 + 1010); // 999, 1010
        assert_eq!(count_invalid_ids_in_range(222220, 222224), 222222); // 222222
        assert_eq!(count_invalid_ids_in_range(1698522, 1698528), 0); // none
        assert_eq!(count_invalid_ids_in_range(446443, 446449), 446446); // 446446
        assert_eq!(count_invalid_ids_in_range(565653, 565659), 565656); // 565656
        assert_eq!(count_invalid_ids_in_range(824824821, 824824827), 824824824); // 824824824
        assert_eq!(count_invalid_ids_in_range(2121212118, 2121212124), 2121212121); // 2121212121
    }
}
