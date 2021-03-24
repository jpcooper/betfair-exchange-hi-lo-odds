#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "prob.h"

#define MAX_SIZE 13

void printOdds(unsigned long int numerator, unsigned long int denominator);

int main(void) {
  unsigned long int * numeratorsResult = createProbabilitiesResult(MAX_SIZE);
  unsigned long int * denominatorsResult = createProbabilitiesResult(MAX_SIZE);

  int size;
  int numberLower;

  while(scanf("%d %d", &size, &numberLower) == 2) {
    assert(size <= MAX_SIZE);

    int lengthOfProbabilities = getLengthOfProbabilities(size);

    calculateProbabilities(numeratorsResult, denominatorsResult, size, numberLower);

    for (int i = 0; i < lengthOfProbabilities; i++) {
      printOdds(numeratorsResult[i], denominatorsResult[i]);
    }
  }

  return 0;
}

void printOdds(unsigned long int numerator, unsigned long int denominator) {
  printf("%.3f -- %.3f\n", (double) numerator / denominator, (double) denominator / numerator);
}
