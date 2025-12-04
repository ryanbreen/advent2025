import { chromium } from 'playwright';
import { createAuthenticatedContext, hasAuthState } from './session.js';

// CLI argument parsing
const args = process.argv.slice(2);
let year = new Date().getFullYear();
let day = null;
let part = 1;
let answer = null;

for (let i = 0; i < args.length; i++) {
  if (args[i] === '--year' || args[i] === '-y') {
    year = parseInt(args[i + 1], 10);
    i++;
  } else if (args[i] === '--day' || args[i] === '-d') {
    day = parseInt(args[i + 1], 10);
    i++;
  } else if (args[i] === '--part' || args[i] === '-p') {
    part = parseInt(args[i + 1], 10);
    i++;
  } else if (args[i] === '--answer' || args[i] === '-a') {
    answer = args[i + 1];
    i++;
  } else {
    // Positional arguments: [year] day [part] answer
    // Year must be 2015-2030 range to distinguish from answers
    const num = parseInt(args[i], 10);
    if (!isNaN(num)) {
      if (num >= 2015 && num <= 2030 && !day) {
        // This looks like a year (only if we haven't set day yet)
        year = num;
      } else if (!day) {
        day = num;
      } else if (num <= 2 && !answer) {
        // Part number (1 or 2)
        part = num;
      } else {
        // Must be the answer
        answer = args[i];
      }
    } else {
      // Non-numeric answer
      answer = args[i];
    }
  }
}

if (!day || day < 1 || day > 25 || !answer) {
  console.log('Usage: node submit.js --year <year> --day <1-25> --part <1|2> --answer <answer>');
  console.log('   or: node submit.js <year> <day> <part> <answer>');
  console.log('   or: node submit.js <day> <part> <answer>  (uses current year)');
  console.log('   or: node submit.js <day> <answer>  (uses current year, part 1)');
  console.log('');
  console.log('Examples:');
  console.log('   node submit.js 2024 1 1 2000468    # 2024 Day 1 Part 1');
  console.log('   node submit.js 1 2 12345          # Current year Day 1 Part 2');
  console.log('   node submit.js --year 2024 --day 1 --part 1 --answer 2000468');
  process.exit(1);
}

if (year < 2015 || year > new Date().getFullYear()) {
  console.log(`Invalid year: ${year}. AoC started in 2015.`);
  process.exit(1);
}

if (!hasAuthState()) {
  console.error('No saved session. Run "node session.js login" first.');
  process.exit(1);
}

console.log(`Submitting Year ${year} Day ${day} Part ${part}: ${answer}`);

const browser = await chromium.launch({ headless: true });
const context = await createAuthenticatedContext(browser);
const page = await context.newPage();

await page.goto(`https://adventofcode.com/${year}/day/${day}`);

// Find the answer input and submit
const answerInput = page.locator('input[name="answer"]');
if (await answerInput.count() === 0) {
  console.log('No answer input found - puzzle may already be solved or not available.');
  await browser.close();
  process.exit(1);
}

await answerInput.fill(answer);
await page.click('input[type="submit"]');

// Wait for response and get the result
await page.waitForSelector('article');
const resultText = await page.locator('article p').first().textContent();

if (resultText.includes("That's the right answer")) {
  console.log('✓ CORRECT!', resultText);
} else if (resultText.includes("That's not the right answer")) {
  console.log('✗ INCORRECT.', resultText);
} else if (resultText.includes("You gave an answer too recently")) {
  console.log('⏳ Rate limited.', resultText);
} else if (resultText.includes("already complete")) {
  console.log('✓ Already solved.', resultText);
} else {
  console.log('Result:', resultText);
}

await browser.close();
