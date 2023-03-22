from pathlib import Path
from urllib import request

CACHE_DIR = Path(".input")
TOKEN_FILE = Path(".token")


def cache_puzzle_input(year: int, day: int) -> Path:
    """Download the puzzle input for the given day and year and save it to a file."""
    CACHE_DIR.mkdir(parents=True, exist_ok=True)  # ensure the cache directory exists

    filename = Path(f"{CACHE_DIR}/{year:04d}{day:02d}.txt")
    if filename.exists():
        print("Cached puzzle input found. Skipping download.")
        return filename

    session_cookie = _get_token()
    puzzle_input = _download_puzzle_input(year, day, session_cookie)
    with open(filename, "w") as f:
        f.write(puzzle_input)

    return filename


def _get_token() -> str:
    """Get the session cookie from the token file."""
    if not TOKEN_FILE.exists():
        raise FileNotFoundError(
            "no token file found.\n"
            "Please create a file named '.token' containing your session cookie."
        )
    with open(TOKEN_FILE, "r") as f:
        session_cookie = f.read().strip()
    return session_cookie


def _download_puzzle_input(year: int, day: int, session_cookie: str) -> str:
    """Download the puzzle input for the given day and year."""
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    req = request.Request(url, headers={"Cookie": session_cookie})
    with request.urlopen(req) as response:
        return response.read().decode("utf-8")
