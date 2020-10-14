// Create a container to hold either the numerators or denominators of
// the calculated probabilities.
unsigned long int* createProbabilitiesResult(int size);

void freeProbabilitiesResult(unsigned long int* probabilitiesResult);

int getLengthOfProbabilities(int size);

void calculateProbabilities(unsigned long int* numeratorsResult,
                            unsigned long int* denominatorsResult,
                            int size,
                            int numberLower);
