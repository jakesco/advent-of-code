from .shared import Grid, P, Solution


def main(input_: list[str]) -> Solution:
    grid = Grid.from_lines(input_)
    visible = 0
    max_score = 0
    for tree in grid.keys():
        score, vis = visit(grid, tree)
        if vis:
            visible += 1
        if score > max_score:
            max_score = score

    return Solution(visible, max_score)


def visit(g: Grid, p: P) -> (int, bool):
    height = g.get(p)
    row = g.row(p.y)
    col = g.col(p.x)

    left_score, left_visible = view(height, row[: p.x], reverse=True)
    right_score, right_visible = view(height, row[p.x + 1 :])
    top_score, top_visible = view(height, col[: p.y], reverse=True)
    bottom_score, bottom_visible = view(height, col[p.y + 1 :])

    visible = left_visible or right_visible or top_visible or bottom_visible
    score = left_score * right_score * top_score * bottom_score
    return score, visible


def view(height: int, trees: list[int], *, reverse: bool = False) -> (int, bool):
    max_ = len(trees)
    if reverse:
        trees = reversed(trees)

    for idx, h in enumerate(trees, start=1):
        if h >= height:
            return idx, False
    return max_, True
