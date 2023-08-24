from .shared import Solution


class Board:
    def __init__(self, numbers: list[int]):
        self.numbers = numbers
        self.marked = [False] * len(numbers)
        self.width = 5
        self.win = False

    def mark(self, number: int):
        try:
            self.marked[self.numbers.index(number)] = True
        except ValueError:
            pass

    def check_horizontal(self):
        for row in range(5):
            pos = row * self.width
            if all(self.marked[pos : pos + self.width]):
                return True
        return False

    def check_vertical(self):
        for col in range(5):
            if all(
                [
                    self.marked[col],
                    self.marked[col + 5],
                    self.marked[col + 10],
                    self.marked[col + 15],
                    self.marked[col + 20],
                ]
            ):
                return True
        return False

    def check_win(self):
        self.win = self.check_horizontal() or self.check_vertical()
        return self.win

    def score(self, last_num: int):
        both = zip(self.numbers, self.marked)
        unmarked = [int(n) for n, m in both if not m]
        return sum(unmarked) * int(last_num)

    def __str__(self):
        output = []
        i = 0
        for n in self.numbers:
            if i % 5 == 0:
                output.append("\n")
            output.append(str(n))
            i += 1

        i = 0
        for m in self.marked:
            if i % 5 == 0:
                output.append("\n")
            output.append("x" if m else "o")
            i += 1
        return " ".join(output)

    def __repr__(self):
        return f"Board({self.numbers}, {self.marked})"


def read_input(lines: list[str]) -> (list[int], list[Board]):
    deck = [l.rstrip("\n").split(",") for l in lines]
    with open("test.txt", "r") as f:
        deck = f.readline()
        boards = []

        f.readline()

        acc = []
        while line := f.readline():
            l = line.split()
            if l:
                acc = acc + l
            else:
                boards.append(Board(acc))
                acc = []
        boards.append(Board(acc))

    return (deck, boards)


def main(input_: list[str]) -> Solution:
    deck, boards = read_input(input_)

    for n in deck:
        print(f"Call: {n}")
        for b in boards:
            b.mark(n)
            print(b)
            b.check_win()
            if all([w.win for w in boards]):
                print(f"Win: {b.score(n)}")

    return Solution()
