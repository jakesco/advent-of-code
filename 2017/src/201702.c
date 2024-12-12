#include <string.h>
#include "advent.h"



int
row_diff(char *row)
{
    int x;
    const char delim[2] = " ";
    char *token;

    token = strtok(row, delim);
    int min, max;
    min = max = atoi(token);

    token = strtok(NULL, delim);
    while( token != NULL) {
        x = atoi(token);
        if (x < min) {
            min = x;
        }
        if (x > max) {
            max = x;
        }
        token = strtok(NULL, delim);
    }

    return max - min;
}

Solution
solve201702(FILE *input)
{
    char line[256];
    Solution solution = {0, 0};

    while(fgets(line, sizeof(line), input)) {
        solution.part1 += row_diff(line);
    }
    return solution;
}

