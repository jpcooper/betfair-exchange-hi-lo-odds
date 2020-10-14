#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "prob.h"

#define MAX_SIZE 13

void printOdds(double probability);

int main(void) {
  double* probabilitiesResult = createProbabilitiesResult(MAX_SIZE);

  int size;
  int numberLower;

  while(scanf("%d %d", &size, &numberLower) == 2) {
    assert(size <= MAX_SIZE);

    int lengthOfProbabilities = getLengthOfProbabilities(size);

    calculateProbabilities(probabilitiesResult, size, numberLower);

    for (int i = 0; i < lengthOfProbabilities; i++) {
      printOdds(probabilitiesResult[i]);
    }
  }

  return 0;
}

void printOdds(double probability) {
  printf("%.3f -- %.3f\n", probability, 1.0 / probability);
}
