/* Main entrypoint for Adevent of Code solutions.
*  Just handles command line args and handing off
*  input the the right solution function.
*/
#include "advent.h"


void usage(void)
{
    char *usage =
        "usage: aoc puzzle [filename]\n\n"
        "Solutions for Advent of Code\n\n"
        "positional arguments:\n"
        "   puzzle      puzzle to solve, format is YYYYDD\n"
        "               (e.g. 201701) for advent of code 2017 day 1\n"
        "   filename    path to puzzle input, will also accept stdin\n\n"
        "options:\n"
        "   -h, --help  show this help message and exit\n";
    printf("%s", usage);
}


int main(int argc, char *argv[]) {
    if (argc < 2 || argc > 3) {
        fprintf(stderr, "aoc: error: invalid number of arguments\n");
        goto fail;
    }

    int puzzle;
    if ((puzzle = atoi(argv[1])) == 0) {
        fprintf(stderr, "aoc: error: invalid puzzle name (%s)\n", argv[1]);
        goto fail;
    }

    FILE *input = NULL;
    if (argc == 3) {
        input = fopen(argv[2], "r");
        if (input == NULL) {
            fprintf(stderr, "aoc: error: could not open input file (%s)\n", argv[2]);
            goto fail;
        }
    } else {
        input = stdin;
    }

    Solution solution;
    switch (puzzle) {
        case 201701:
            solution = solve201701(input);
            break;
        case 201702:
            solution = solve201702(input);
            break;
        default:
            fprintf(stderr, "aoc: error: no solution for puzzle (%d)\n", puzzle);
            goto fail;
    }

    printf("Part 1: %d\nPart 2: %d\n", solution.part1, solution.part2);

    if (input != stdin && input != NULL) {
        fclose(input);
    }

    return EXIT_SUCCESS;

fail:
    usage();
    return EXIT_FAILURE;
}
