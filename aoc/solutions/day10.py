from dataclasses import dataclass, field

from .shared import Solution


@dataclass
class CPU:
    cycles: int = 0
    x: int = 1
    signal: int = 0
    screen: list[str] = field(default_factory=lambda: [" "] * 40 * 6)

    def cycle(self):
        self.cycles += 1
        self.draw()
        if self.cycles in range(20, 221, 40):
            self.signal += self.cycles * self.x

    def add(self, v: int):
        self.x += v

    def window(self) -> tuple[int, int, int]:
        return self.x - 1, self.x, self.x + 1

    def draw(self):
        pixel = self.cycles - 1
        if pixel % 40 in self.window():
            self.screen[pixel] = "#"
        else:
            self.screen[pixel] = "."

    def __str__(self) -> str:
        output = []
        for idx, n in enumerate(self.screen, start=1):
            output.append(n)
            if idx % 40 == 0:
                output.append("\n")
        return "".join(output)


def main(ops: list[str]) -> Solution:
    cpu = CPU()
    for op in ops:
        match op.split():
            case ["noop"]:
                cpu.cycle()
            case ["addx", amount]:
                cpu.cycle()
                cpu.cycle()
                cpu.add(int(amount))
    return Solution(cpu.signal, f"\n{cpu}")
