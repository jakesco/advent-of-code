from .shared import Grid, P, Solution


def main(input_: list[str]) -> Solution:
    grid = Grid.from_lines(input_)
    visible = 0
    for tree in grid.keys():
        if grid.is_edge(tree) or is_visible(grid, tree):
            visible += 1

    return Solution(visible)


def is_visible(g: Grid, p: P) -> bool:
    h = g.get(p)
    row = g.row(p.y)
    col = g.col(p.x)
    return (
        all(n < h for n in row[: p.x])
        or all(n < h for n in row[p.x + 1 :])
        or all(n < h for n in col[: p.y])
        or all(n < h for n in col[p.y + 1 :])
    )
