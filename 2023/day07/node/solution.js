import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Card strength order (higher index = stronger)
const CARD_STRENGTH = '23456789TJQKA';
const CARD_STRENGTH_JOKER = 'J23456789TQKA'; // J is weakest in Part 2

function getHandType(hand) {
  const counts = {};
  for (const c of hand) {
    counts[c] = (counts[c] || 0) + 1;
  }
  const sorted = Object.values(counts).sort((a, b) => b - a);

  if (sorted[0] === 5) return 6; // Five of a kind
  if (sorted[0] === 4) return 5; // Four of a kind
  if (sorted[0] === 3 && sorted[1] === 2) return 4; // Full house
  if (sorted[0] === 3) return 3; // Three of a kind
  if (sorted[0] === 2 && sorted[1] === 2) return 2; // Two pair
  if (sorted[0] === 2) return 1; // One pair
  return 0; // High card
}

function handKey(hand) {
  const handType = getHandType(hand);
  const cardValues = [...hand].map(c => CARD_STRENGTH.indexOf(c));
  return [handType, ...cardValues];
}

function compareHands(a, b) {
  const keyA = handKey(a);
  const keyB = handKey(b);
  for (let i = 0; i < keyA.length; i++) {
    if (keyA[i] !== keyB[i]) return keyA[i] - keyB[i];
  }
  return 0;
}

// Part 1
function part1() {
  const hands = lines.map(line => {
    const [hand, bid] = line.split(' ');
    return { hand, bid: parseInt(bid) };
  });

  hands.sort((a, b) => compareHands(a.hand, b.hand));

  let total = 0;
  for (let i = 0; i < hands.length; i++) {
    total += (i + 1) * hands[i].bid;
  }
  return total;
}

function getHandTypeWithJokers(hand) {
  const jokerCount = [...hand].filter(c => c === 'J').length;
  if (jokerCount === 0) return getHandType(hand);
  if (jokerCount === 5) return 6; // Five of a kind

  // Count non-joker cards
  const counts = {};
  for (const c of hand) {
    if (c !== 'J') counts[c] = (counts[c] || 0) + 1;
  }
  const sorted = Object.values(counts).sort((a, b) => b - a);

  // Add jokers to the highest count
  sorted[0] += jokerCount;

  if (sorted[0] === 5) return 6; // Five of a kind
  if (sorted[0] === 4) return 5; // Four of a kind
  if (sorted[0] === 3 && sorted[1] === 2) return 4; // Full house
  if (sorted[0] === 3) return 3; // Three of a kind
  if (sorted[0] === 2 && sorted[1] === 2) return 2; // Two pair
  if (sorted[0] === 2) return 1; // One pair
  return 0; // High card
}

function handKeyWithJokers(hand) {
  const handType = getHandTypeWithJokers(hand);
  const cardValues = [...hand].map(c => CARD_STRENGTH_JOKER.indexOf(c));
  return [handType, ...cardValues];
}

function compareHandsWithJokers(a, b) {
  const keyA = handKeyWithJokers(a);
  const keyB = handKeyWithJokers(b);
  for (let i = 0; i < keyA.length; i++) {
    if (keyA[i] !== keyB[i]) return keyA[i] - keyB[i];
  }
  return 0;
}

// Part 2
function part2() {
  const hands = lines.map(line => {
    const [hand, bid] = line.split(' ');
    return { hand, bid: parseInt(bid) };
  });

  hands.sort((a, b) => compareHandsWithJokers(a.hand, b.hand));

  let total = 0;
  for (let i = 0; i < hands.length; i++) {
    total += (i + 1) * hands[i].bid;
  }
  return total;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
