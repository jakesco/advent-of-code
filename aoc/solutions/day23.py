from .shared import Solution, Grid


def main(input_: list[str]) -> Solution:
    grid = Grid.from_lines(input_, convert=str)
    grid.render_graph(range(7), range(7))
    return Solution()
