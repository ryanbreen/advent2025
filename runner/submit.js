import { chromium } from 'playwright';
import { createAuthenticatedContext, hasAuthState } from './session.js';

const day = parseInt(process.argv[2], 10);
const part = parseInt(process.argv[3], 10) || 1;
const answer = process.argv[4];

if (!day || !answer) {
  console.log('Usage: node submit.js <day> <part> <answer>');
  console.log('   or: node submit.js <day> <answer>  (defaults to part 1)');
  process.exit(1);
}

if (!hasAuthState()) {
  console.error('No saved session. Run "node session.js login" first.');
  process.exit(1);
}

console.log(`Submitting Day ${day} Part ${part}: ${answer}`);

const browser = await chromium.launch({ headless: true });
const context = await createAuthenticatedContext(browser);
const page = await context.newPage();

await page.goto(`https://adventofcode.com/2025/day/${day}`);

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
} else {
  console.log('Result:', resultText);
}

await browser.close();
