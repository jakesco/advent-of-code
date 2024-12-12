#include <stdio.h>
#include <stdlib.h>
#include "advent.h"

// probably big enough
#define BUFSIZE 4096


/* Builds an array of ints from input
 * return the number of ints parsed
 */
static int
build_buffer(int *buffer, int bufsize, FILE *input)
{
    int count = 0;
    char c;

    while (count < bufsize) {
        c = fgetc(input);

        if (c == '\n' || feof(input)) { break; }

        buffer[count++] = c - '0';  // acii to digit
    }

    return count;
}


static int
solve(int *buffer, int count, int inc)
{
    int x, y, sum = 0;

    for (int i = 0; i < count; i++) {
        x = buffer[i];
        y = buffer[(i + inc) % count];
        if (x == y)
            sum += x;
    }
    return sum;
}


Solution
solve201701(FILE *input)
{
    int buffer[BUFSIZE];
    int count = build_buffer(buffer, BUFSIZE, input);
    Solution solution = {
        .part1=solve(buffer, count, 1),
        .part2=solve(buffer, count, count / 2),
    };
    return solution;
}

