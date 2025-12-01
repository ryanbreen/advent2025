import { chromium } from 'playwright';
import { existsSync, readFileSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const AUTH_STATE_PATH = join(__dirname, 'auth-state.json');
const AOC_URL = 'https://adventofcode.com/2025';

export function hasAuthState() {
  return existsSync(AUTH_STATE_PATH);
}

export function loadAuthState() {
  if (!hasAuthState()) {
    return null;
  }
  return JSON.parse(readFileSync(AUTH_STATE_PATH, 'utf-8'));
}

export function saveAuthState(state) {
  writeFileSync(AUTH_STATE_PATH, JSON.stringify(state, null, 2));
}

export async function createAuthenticatedContext(browser) {
  if (hasAuthState()) {
    return browser.newContext({ storageState: AUTH_STATE_PATH });
  }
  return browser.newContext();
}

export async function launchLoginSession() {
  console.log('Launching browser for Advent of Code login...');
  console.log('Please log in with your GitHub account.');
  console.log('Session will be saved automatically once login is detected.\n');

  const browser = await chromium.launch({
    headless: false,
    args: ['--start-maximized']
  });

  const context = await browser.newContext({
    viewport: null // Use full window size
  });

  const page = await context.newPage();

  // Navigate to AoC
  await page.goto(AOC_URL);

  console.log('Waiting for login (polling every 2 seconds)...');

  // Poll for login state
  let loggedIn = false;
  while (!loggedIn) {
    await new Promise(r => setTimeout(r, 2000));
    loggedIn = await page.locator('.user').count() > 0;
    if (!loggedIn) {
      process.stdout.write('.');
    }
  }

  const username = await page.locator('.user').textContent();
  console.log(`\n\nLogged in as: ${username}`);

  // Save the storage state
  const state = await context.storageState();
  saveAuthState(state);
  console.log(`Session saved to ${AUTH_STATE_PATH}`);

  await browser.close();
  console.log('Browser closed. Session saved successfully!');
}

export async function verifySession() {
  if (!hasAuthState()) {
    console.log('No saved session found. Run with "login" to authenticate.');
    return false;
  }

  console.log('Verifying saved session...');

  const browser = await chromium.launch({ headless: true });
  const context = await createAuthenticatedContext(browser);
  const page = await context.newPage();

  await page.goto(AOC_URL);

  const loggedIn = await page.locator('.user').count() > 0;

  if (loggedIn) {
    const username = await page.locator('.user').textContent();
    console.log(`Session valid. Logged in as: ${username}`);
  } else {
    console.log('Session expired or invalid. Run with "login" to re-authenticate.');
  }

  await browser.close();
  return loggedIn;
}

// CLI handling - only run if this is the main module
const isMainModule = process.argv[1] === fileURLToPath(import.meta.url);

if (isMainModule) {
  const command = process.argv[2];

  if (command === 'login') {
    await launchLoginSession();
  } else if (command === 'verify') {
    await verifySession();
  } else {
    console.log('Usage: node session.js [login|verify]');
  }
}
