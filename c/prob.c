#include <stdlib.h>
#include "prob.h"
#include "gmp.h"

// In this programme we calculate the exact odds for Betfair's
// gambling card game Exchange Hi Lo
// (https://games.betfair.com/exchange-hi-lo/standard).

// OUTLINE OF BETFAIR'S EXCHANGE HI LO AND BETTING ON IT
//
// Betfair's Exchange Hi Lo is a game played with a shuffled deck of
//  13 cards from a single suit. Betfair's suit of choice is
//  spades. Ace is high. The betting in Betfair's Exchange Hi Lo
//  varies from that of the normal Hi Lo version, but the game
//  proceeds in the same way. The game is split into 13 stages. At
//  each stage, betting occurs, followed by dealing a card without
//  replacement. These 13 stages are named on Betfair with the names
//  <Start, 1, 2, ..., 11, Last>. In this programme, we name these
//  stages differently, with the numbers <0, ..., 12>. Because the
//  last stage, whose name in this programme would be 12, is uniquely
//  determined by the previously dealt 12 cards, we do not actually
//  bet on this final stage. This stage is not referred to in this
//  programme.
//
// In the normal version of Hi Lo, the player bets on whether the next
// dealt card will be higher or lower than the last dealt card. In
// Betfair's version, the prediction of the next card is instead
// performed by the computer (dealer) based on a very simple
// heuristic. It is up to the player to predict how long the computer
// will be correct based on this heuristic. The heuristic works as
// follows: From stage 1 (the stage after the first card is played at
// stage 0) onwards, before dealing the card at the current stage, if
// there is an equal number of cards left in the deck which are higher
// than the last played card than the number of cards that are lower,
// predict that the next played card will be higher. Otherwise predict
// that it will be lower. This heuristic maximises correct predictions
// from the side of the computer. The question remains how to maximise
// our own correct predictions.
//
// At stage 0 (using this programme's stage naming scheme), Betfair
// provides the following ordered list of outcomes to bet on: <Card 1
// or futher, Card 2 or further, ..., Card 10 or further, Card
// 11>. The name "Card n" in Betfair corresponds to the card played at
// stage n in this programme. Note that Card 1 is the card played in
// stage 1, after the initial card played at stage 0 in this
// programme.
//
// The outcomes of the form "Card n or further" correspond to the case
// where the computer predicts correctly at least up to and including
// the dealing of "Card n", which is dealt at stage n in this
// programme. The outcome "Card 11" corresponds to the case that the
// computer predicts correctly for all cards dealt up to and including
// "Card 11". At stage 0, we can still bet on all previously listed
// outcomes, starting at "Card 1 or further". Starting at stage 1 <= n
// <= 10, after dealing the card at that stage, the outcome "Card n or
// further" is removed from the available outcomes to bet on. After
// the card at stage 11 is dealt, betting ends.
//
// The question now is how to, given a current game state (the
// sequence of the dealt cards in each previous stage, compute the
// probability of each available remaining outcome. This initially
// seems to be a computationally complex problem in time and space,
// given that there are 13! ways to shuffle 13 cards. Monte Carlo
// simulation can approximate the solution, but we do not stop
// here. The key observation to make is that to calculate the
// probabilities exactly, we can partition the game states into a
// characterisation based on the number of cards lower (and thus also
// higher) than the last played card. This set of characterisations
// happens to fit in a very small amount of memory, and using this
// characterisation, we can compute very quickly the probabilities of
// the remaining outcomes at a given stage, given the probabilities of
// the outcomes at previous stages. This is done using a dynamic
// algorithm as outlined below.
//
// This explanation assumes an initial game state where no cards have
// been played, with a deck of size 13. In the programme as follows,
// the initial state is characterised by the number of remaining cards
// (denoted by the variable `size`), and the number of cards lower
// than the last card played (denoted by the variable
// `numberLower`). When no cards have been played, `size` is 13 and
// `numberLower` is set to 0.

// Define a triangular matrix where each value
// matrix[stage][numberLower], once populated, will be equal to the
// number of paths (defined following) leading from the initial game
// state to a game state with stage `stage` and with `numberLower`
// cards lower than the last card played. A path to the state
// associated with matrix[stage][numberLower] is a unique dealing of
// cards from the initial game state, leading to a state defined by
// that matrix index, such that the computer would have predicted
// correctly using its heuristic before each dealing of a successive
// card.
//
// The matrix is later used to calculate the probabilities of game
// outcomes, the subject of this programme.
static int** createMatrix(int size) {
  // We compute the matrix for (size - 1) stages.
  int** matrix = calloc(size - 1, sizeof(int*));

  for (int i = 0; i < size - 1; i++) {
    // After dealing a card at stage i, there are ((size - 1) - i)
    // cards left. This means that a maximum of ((size - 1) - i) cards
    // can be lower than this dealt card. We therefore need (size - i)
    // spaces to encode each case of how many cards are lower than the
    // card dealt in this stage.
    matrix[i] = calloc(size - i, sizeof(int));
  }

  return matrix;
}

// Given a deck of `size` remaining cards, there are (size - 1)
// outcomes which are interesting to us.
int getLengthOfProbabilities(int size) {
  return size - 1;
}

// Create an array to hold the probabilities which we are interested
// in computing.
static mpq_t* createProbabilities(int size) {
  int lengthOfProbabilities = getLengthOfProbabilities(size);
  mpq_t* probabilities = calloc(lengthOfProbabilities, sizeof(mpq_t));

  for (int i = 0; i < lengthOfProbabilities; i++) {
    mpq_init(probabilities[i]);
  }

  return probabilities;
}

// Create a container to hold either the numerators or denominators of
// the calculated probabilities.
unsigned long int* createProbabilitiesResult(int size) {
  return calloc(getLengthOfProbabilities(size), sizeof(unsigned long int));
}

void freeProbabilitiesResult(unsigned long int* probabilitiesResult) {
  free(probabilitiesResult);
}

// Given the initial game state where there are `size` cards remaining
// in the deck and `numberLower` cards remaining in the deck which are
// lower than the last card played (if no card has been played, set
// `numberLower` to 0), initialise the first stage (first row in the
// `matrix`), which corresponds to the first card played after the
// initial game state.
//
// Assume that we have not yet played the card in the first stage
// yet. Given `numberLower` remaining cards lower than the last card
// played, set `numberHigher` = `size` - `numberLower`. The value
// `matrix[0][i]` correponds to the number of ways to play the first
// card such that there would be i cards lower than this card, and
// such that the computer would have predicted this outcome correctly.
//
// Suppose that `numberHigher` >= `numberLower`. The computer would
// predict that the card played in this stage will be higher than the
// previous card. If, after playing the first card, there are still
// `numberLower` cards or more remaining which are lower than this
// card, then the computer was correct in its prediction. There is
// exactly one way to arrive at each outcome `matrix[0][i]`. The
// correct predictions correspond to `i` with `numberLower` <= `i` <=
// `size` - 1. Set the values for `i` within that range to 1,
// corresponding to correct predictions, and the others to 0.
//
// Suppose instead that `numberLower` > `numberHigher`. The computer
// would predict that the card played in this stage will be lower than
// the previous card. If, after playing the first card, there are
// still at most `numberLower` - 1 cards remaining that are lower,
// then the computer was correct. Set `matrix[0][i]` to 1 for 0 <= `i`
// <= `numberLower` - 1, and the others to 0.
static void initialiseFirstStage(int** matrix, int size, int numberLower) {
  int numberHigher = size - numberLower;
  int k;
  int l;

  if (numberHigher >= numberLower) {
    k = numberLower;
    l = size;
  } else {
    k = 0;
    l = numberLower;
  }

  for (int i = k; i < l; i++) {
    matrix[0][i] = 1;
  }
}

// How many remain after dealing the card at the stage denoted by
// `stage`, having started with an initial state of `size` cards?
static int getNumberCardsLeftAfterDealing(int size, int stage) {
  return size - (stage + 1);
}

// This is the inductive step in computing the values of the
// matrix. Given the values in the matrix for the previous stage, we
// compute the values for the current stage, denoting the number of
// paths leading to each possible game state in the current stage.
//
// The current game state is defined by the stage `stage`, and the
// number of cards lower than the card dealt in this stage,
// `numberLower`. To compute the number of paths leading to this game
// state, we sum over all matrix values for the game states in the
// previous stage which could have led to the current game state.
//
// In order to determine which previous game states can lead to the
// current game state, we split the problem space over the following:
//
// 1. Whether or not the number of cards left _before_ dealing in this stage
// (`numberCardsLeftBeforeDealing`) is even.
// 2. Whether or not the number of cards left _after_ dealing in this stage
// (`numberLower`) is less than, equal to or greater than the value `limit`, which
// is explained following.
//
// The relation of `numberLower` to `limit` as computed in this
// function determines whether the current game state could have been
// reached from a previous game state by a prediction by the computer
// of a higher card, or a lower card. This relation varies based on
// whether `numberCardsLeftBeforeDealing` is even.
//
// In each combination of the cases in 1. and 2., we can compute two
// values `k` and `l`. These `k` and `l` define the two sets of game
// states in the previous stage which can lead to the current game
// stage. Take the sum over the matrix[previousStage][i] for 0 <= i <
// k or l <= i <= numberCardsLeftBeforeDealing to obtain
// matrix[stage][numberLower], the value for the current game state.
//
// A game state is arrived at by dealing a card either higher or lower
// than the last dealt card. The game states for
// matrix[previousStage][i] with 0 <= i < k are the ones from which a
// higher next card would have been predicted and could be dealt to
// arrive at the current game state. The game states for
// matrix[previousStage][i] with l <= i <=
// numberCardsLeftBeforeDealing are the states from which a lower card
// would have been predicted and could be dealt to arrive at the
// current game state.
//
// In conclusion and repetition: Remember that the current dealt card
// is either higher or lower than the last dealt card. By analying
// each pair of assignments to `k` and `l`, see the following:
//
// 1. `k` is the maximal `k` such that for each 0 <= i < k, at game
// state matrix[previousStage][i], a higher card is predicted by the
// computer, and there is exactly one card higher than the last dealt
// card that can be dealt to arrive at the current game state.
// 2. `l` is the minimal `l` such that for each
// l <= i <= numberCardsLeftBeforeDealing, at game state
// matrix[previousStage][i], a lower card is predicted by the
// computer, and there is exactly one card lower than the last dealt
// card that can be dealt to arrive at the current game state.
//
// The value matrix[stage][numberLower] for the current game state is
// therefore equal to the sum of values corresponding to the game
// states in the previous stage as constrained by the above two
// ranges.
static int getNumberPathsLeadingTo(int** matrix, int size, int stage, int numberLower) {
  int previousStage = stage - 1;
  int numberCardsLeftBeforeDealing = getNumberCardsLeftAfterDealing(size, previousStage);
  int limit = (numberCardsLeftBeforeDealing + 1) / 2;

  int k;
  int l;

  int sum = 0;

  if (numberCardsLeftBeforeDealing % 2 == 0) {
    if (numberLower <= limit) {
      k = numberLower + 1;
      l = limit + 1;
    } else {
      k = limit + 1;
      l = numberLower + 1;
    }
  } else {
    if (numberLower < limit) {
      k = numberLower + 1;
      l = limit;
    } else if (numberLower == limit) {
      k = limit;
      l = limit + 1;
    } else {
      k = limit;
      l = numberLower + 1;
    }
  }

  for (int i = 0; i < k; i++) {
    sum += matrix[previousStage][i];
  }

  for (int i = l; i <= numberCardsLeftBeforeDealing; i++) {
    sum += matrix[previousStage][i];
  }

  return sum;
}

// Compute each value corresponding to a game state in stage `stage`
// individually.
static void initialiseStage(int** matrix, int size, int stage) {
  int numberCardsLeft = getNumberCardsLeftAfterDealing(size, stage);

  for (int i = 0; i <= numberCardsLeft; i++) {
    matrix[stage][i] = getNumberPathsLeadingTo(matrix, size, stage, i);
  }
}

// To calculate the whole matrix, initialise the first stage, and
// compute each following stage successively.
static void calculateMatrix(int** matrix, int size, int numberPlayable) {
  initialiseFirstStage(matrix, size, numberPlayable);

  for (int i = 1; i < size - 1; i++) {
    initialiseStage(matrix, size, i);
  }
}

// See the documentation for calculatePermutations to understand what
// permutations is.
static int getLengthOfPermutations(int size) {
  return size - 2;
}

// The number of ways to deal `size - 1` cards from a deck of size
// `size`.
static long getNumberShuffles(long* permutations, int size) {
  int lengthOfPermutations = getLengthOfPermutations(size);

  return permutations[lengthOfPermutations - 1];
}

// How many remaining cards, when played next, would result in an incorrect
// prediction by the computer, given how many cards are now left, and
// how many cards are lower than the last card played?
static int numberFailingCards(int numberCardsLeft, int numberLower) {
  int numberHigher = (numberCardsLeft - numberLower) - 1;

  return numberHigher >= numberLower ? numberLower : numberHigher;
}

// We now calculate the probabilities of the outcomes mentioned in the
// outline, based on the populated matrix. As described before, we are
// interested in the probabilities of the outcomes of the form: <Card
// 1 or further, ..., Card (size - 3) or further, Card (size - 2)>. n
// in "Card n" means the nth card played after the initial game state,
// with n starting from 0. Card 0 is the first card dealt, and on
// which the first computer prediction is made. The dealing of Card 1
// is the first test of the computer's prediction, which is the
// subject of the first outcome in the list.
//
// The outcomes of the form "Card n or futher" mean that the
// computer's prediction is correct for _at least_ each dealing of the
// cards Card 1 until n inclusive. The predictions following the
// dealing of Card n may or may not be successful. The probabilities
// of these outcomes are called the initial probabilities. The outcome
// "Card (size - 2)" means that the computer correctly predicted the
// dealing of Card (size - 2), the final card whose value on dealing
// is uncertain. The probability of this outcome is called the final
// probability.
//
// The probabilities of these outcomes can be computed in terms of the
// probabilities of the following outcomes, which are independent of
// each other: Independent = <CorrectUntilAndFailsAfter(1),
// CorrectUntilAndFailsAfter(2), ..., CorrectUntilAndFailsAfter(size -
// 3), CorrectAt(size - 2)>. CorrectUntilAndFailsAfter(n) means that
// the computer was correct in its prediction for the dealing of each
// card i for 1 <= i <= n, and incorrectly predicted the dealing of
// Card (n + 1). CorrectAt(size - 2) means that the computer correctly
// predicted the dealing of Card (size - 2).
//
// We temporarily revise the meaning of "initial probabilities" to
// mean the probabilities of the outcomes of the form
// CorrectUntilAndFailsAfter(n). These are computed in this
// function. The outcomes "Card (size - 2)" and "CorrectAt(size - 2)"
// are the same, and its probability is still called the final
// probability. This probability is calculated in
// `calculateFinalProbability`.
//
// The initial probabilities, those of the outcomes of the form "Card
// n or further", are equal to the sum of the probablity of the
// outcome CorrectUntilAndFailsAfter(n) plus the probabilities of all
// outcomes following that outcome in the list named "Independent"
// above.
//
// We calculate the appropriate sums of the independent probabilities
// in `accumulateProbabilities`, which gives us our final result.
static void calculateInitialProbabilities(int** matrix,
                                         mpq_t* probabilities,
                                         long* permutations,
                                         int size) {
  for (int n = 0; n < size - 2; n++) {
    int numberCardsLeft = size - n;
    long sum = 0;

    for (int numberLower = 0; numberLower < numberCardsLeft; numberLower++) {
      // How many ways are there to successfully predict each card up
      // to and including Card n, and then play a failing card after?
      sum += matrix[n][numberLower] * numberFailingCards(numberCardsLeft, numberLower);
    }

    // This sets probabilities[n] to (sum / permutations[n]), where
    // permutations[n] is the number of ways to deal (n + 2) cards
    // from a deck of size `size`. This is because after dealing the
    // card at stage n and then dealing a failing card, we have dealt
    // (n + 2) cards.
    mpq_set_si(probabilities[n], sum, permutations[n]);
    mpq_canonicalize(probabilities[n]);
  }
}

// See documentation for `calculateInitialProbabilities`
static void calculateFinalProbability(int** matrix,
                                      mpq_t* probabilities,
                                      long* permutations,
                                      int size) {
  int lengthOfProbabilities = getLengthOfProbabilities(size);

  // After dealing the penultimate card in stage (size - 2), the one
  // remaining card is either higher or lower than the card dealt. Sum
  // over the values matrix[size - 2][0] and matrix[size - 2][1] to
  // encapsulate both cases.
  long sum = matrix[size - 2][0] + matrix[size - 2][1];

  // The number of ways to deal `size - 1` cards from a deck of size `size`.
  long numberShuffles = getNumberShuffles(permutations, size);

  // This sets probabilities[n] to (sum / numberShuffles).
  mpq_set_si(probabilities[lengthOfProbabilities - 1], sum, numberShuffles);
  mpq_canonicalize(probabilities[lengthOfProbabilities - 1]);
}

// See documentation for `calculateInitialProbabilities` These are the
// rational probabilities calculated internally. This programme
// returns the probabilities split into their numerators and
// denominators to the outside world.
static void calculateInternalProbabilities(int** matrix,
                                           mpq_t* probabilities,
                                           long* permutations,
                                           int size) {
  calculateInitialProbabilities(matrix, probabilities, permutations, size);
  calculateFinalProbability(matrix, probabilities, permutations, size);
}

// See documentation for `calculateInitialProbabilities`
static void accumulateProbabilities(mpq_t* probabilities, int size) {
  mpq_t sum, oldProbability;
  int lengthOfProbabilities = getLengthOfProbabilities(size);

  mpq_init(sum);
  mpq_init(oldProbability);

  for (int n = lengthOfProbabilities - 1; n >= 0; n--) {
    mpq_set(oldProbability, probabilities[n]);

    mpq_add(probabilities[n], probabilities[n], sum);
    mpq_add(sum, sum, oldProbability);
  }

  mpq_clear(sum);
  mpq_clear(oldProbability);
}

// Unzip the mpq_t probabilities into their numerators and denominators.
static void convertToNumeratorsAndDenominators(unsigned long int* numeratorsResult,
                                               unsigned long int* denominatorsResult,
                                               mpq_t* probabilities,
                                               int size) {
  int lengthOfProbabilities = getLengthOfProbabilities(size);

  for (int i = 0; i < lengthOfProbabilities; i++) {
    unsigned long int numerator = mpz_get_ui(mpq_numref(probabilities[i]));
    unsigned long int denominator = mpz_get_ui(mpq_denref(probabilities[i]));

    numeratorsResult[i] = numerator;
    denominatorsResult[i] = denominator;
  }
}

static void freeProbabilities(mpq_t* probabilities, int size) {
  int lengthOfProbabilities = getLengthOfProbabilities(size);

  for (int i = 0; i < lengthOfProbabilities; i++) {
    mpq_clear(probabilities[i]);
  }

  free(probabilities);
}

// See documentation for calculatePermutations.
static long* createPermutations(int size) {
  return calloc(getLengthOfPermutations(size), sizeof(long));
}

// The number of ways to deal 2 <= n <= (size - 1) cards from a deck
// with _size_ cards for each n.
static void calculatePermutations(long* permutations, int size) {
  int lengthOfPermutations = getLengthOfPermutations(size);

  if (size > 0) {
    permutations[0] = size * (size - 1);

    for (int i = 1; i < lengthOfPermutations; i++) {
      permutations[i] = permutations[i - 1] * ((size - i) - 1);
    }
  }
}

void calculateProbabilities(unsigned long int* numeratorsResult,
                            unsigned long int* denominatorsResult,
                            int size,
                            int numberLower) {
  int** matrix = createMatrix(size);
  mpq_t* probabilities = createProbabilities(size);
  long* permutations = createPermutations(size);

  calculateMatrix(matrix, size, numberLower);
  calculatePermutations(permutations, size);
  calculateInternalProbabilities(matrix, probabilities, permutations, size);
  accumulateProbabilities(probabilities, size);
  convertToNumeratorsAndDenominators(numeratorsResult,
                                     denominatorsResult,
                                     probabilities,
                                     size);

  free(matrix);
  freeProbabilities(probabilities, size);
  free(permutations);
}
