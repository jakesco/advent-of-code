aoc: src/*.c src/*.h
	gcc -Wall -Wextra -std=c2x -pedantic -o aoc src/*.c

test: src/ds/*.c tests/*.c
	gcc -Wall -Wextra -std=c2x -pedantic -o test src/ds/*.c tests/*.c

run: aoc
	./aoc

clean:
	rm aoc

