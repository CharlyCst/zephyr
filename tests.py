#!/usr/bin/env python3
# encoding: utf-8
"""
Test-suite script for the Zephyr language (https://github.com/CharlyCst/zephyr).
This script provides all capabilities to compile the zephyr compiler from rust,
then compile all of the zephyr test files and execute the tests written in
zephyr.
"""
# ───────────────────────────────── headers ────────────────────────────────── #
__author__ = "Simon Lassourreuille (Slyces)"
__credits__ = ["Simon Lassourreille", "Charly Castes"]
__version__ = "0.1.0"
__maintainer__ = "Simon Lassourreuille"
__email__ = "slyces.coding@gmail.com"
__status__ = "Development"

# ───────────────────────────────── imports ────────────────────────────────── #
# Standard Library
from typing import List
from functools import reduce
from pathlib import Path
import argparse
import collections
import logging
import os
import re
import shutil
import subprocess
import sys
import textwrap
# ──────────────────────────────────────────────────────────────────────────── #

# Constants
WORKDIR = Path(__file__).resolve().parent
TEST_DIR = './test'
ZEPHYR_PATH = "./target/debug/zephyr"

# ────────────────────────────────── utils ─────────────────────────────────── #
# Colors shortcuts through ANSI escape codes
RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"
CYAN = "\x1b[36m"
MAGENTA = "\x1b[35m"
BLUE = "\x1b[34m"
BOLD = "\x1b[1m"
ITA = "\x1b[3m"
NC = "\x1b[0m"  # no-color, or reset

# Print small ✓ / ✗
YES = f"{GREEN}✓{NC}"
NO = f"{RED}✗{NC}"

# Logging shortcuts
info = logging.info
debug = logging.debug
warn = logging.warning


# ─────────────────────────────── dependencies ─────────────────────────────── #
def check_dependency(command: str, exit: bool = True) -> bool:
    """Checks the dependencies for testing the current version of the
    compiler. The dependency will be checked as a path to a valid executable,
    then as a command available in the $PATH.
    Returns a boolean of the command's validity.

    If not indicated otherwise, a fail exits the program with a code [127].
    """
    valid = (
        # Check the command as an executable path
        os.path.exists(command) and os.access(command, os.X_OK)
        # Check the command in the current environment
        or shutil.which(command)
    )


    if exit and not valid:
        info(
            f"{RED}[ERROR]{NC} {BOLD}%s{NC} is not a valid command/executable",
            command,
        )
        sys.exit(127)
    return valid

def is_zephyr(path: str):
    return path[-4:] == '.zph'

def get_zephyr():
    """Gets the zephyr executable, from the following sources, by priority:
    - ZEPHYR_PATH: a constant pointing to the relative path of zephyr if built
      in the repo
    - zephyr: the command, if zephyr is installed on your system

    If not found, exits with code 127
    """
    if check_dependency(ZEPHYR_PATH, exit=False):
        zephyr = ZEPHYR_PATH
    elif check_dependency('zephyr', exit=False):
        zephyr = 'zephyr'
    else:
        warn(
            f"{RED}[ERROR]{NC} both local (%s) and global (zephyr) executable were not found",
            ZEPHYR_PATH,
        )
        exit(127)
    return zephyr

# ───────────────────────────────── printing ───────────────────────────────── #
def table(*columns, paddings: List[str] = None, insert_separation = None):
    """Given columns as separate lists, prints a nice layout, similar to the
    unix tool column(1).

    Args:
        columns (Tuple[List[str]]):
            Any number of columns, a column being a list of printable objects
            (preferably strings). All columns should have the same length.
            The first element of the column will be used as a title.
        paddings (List[str], optional):
            List of padding for the display of each columns. Titles are always
            centered.
        insert_separation (optional):
            This argument should be a callable of the following signature:
            > Callable(last_row: List[str], current_row: List[str]) -> bool

            For each consecutive rows, decide if a separator should be inserted
            between them by returning a boolean value.
    """
    # Padd using the custom length
    pad_functions = {
        "left":   lambda s, n: s.ljust(n + len(s) - length(s)),
        "right":  lambda s, n: s.rjust(n + len(s) - length(s)),
        "center": lambda s, n: s.center(n + len(s) - length(s)),
    }
    # Default padding: all left
    paddings = paddings or ["left"] * len(columns)
    assert all([pad in pad_functions for pad in paddings])
    pads = [pad_functions[p] for p in paddings]

    # Default separation insertion: never
    insert_separation = insert_separation or (lambda r1, r2: False)

    # Deal with string length/padding with ansi escape codes
    strip_ansi = re.compile(r"\x1b\[[;\d]*[A-Za-z]")

    # Compute the lenght without escape code (displayed chars)
    length = lambda string: len(strip_ansi.sub('', str(string)))


    # Compute the right size for each column (size of the maximal element)
    sizes = [max(map(length, col)) for col in columns]

    # Subfunction formatting a row using
    def format_row(sep: str, row: List[str], paddings: List[str] = None):
        return sep.join( pads[i](str(cell), sizes[i]) for (i, cell) in enumerate(row) )


    # Pack rows together, extract the titles
    rows = list(zip(*columns))
    titles = rows.pop(0)

    # Print the headers
    print(format_row(' │ ', titles, ['center'] * len(columns)))
    print(format_row('═╪═', ['═' * sizes[i] for i,_ in enumerate(columns)]))

    # Gather full lines as string first, to insert separations conditionally
    lines = []
    for (i, _) in enumerate(rows):
        # Check on consecutive rows if we should insert a separation line
        if i > 0 and insert_separation(rows[i - 1], rows[i]):
            lines.append(
                format_row("─┼─", ["─" * sizes[i] for i, _ in enumerate(columns)])
            )

        # Always add the current row
        lines.append(format_row(' │ ', rows[i], ['center'] * len(columns)))

    # Print the result lines
    print('\n'.join(lines))


# ────────────────────────────── main functions ────────────────────────────── #
def build_zephyr():
    """Builds the zephyr language for the current language using a separate
    process.
    """
    check_dependency('cargo', exit=True)

    # Blocking call to cargo
    warn("→ building zephyr")
    completed = subprocess.run(
        ["cargo", "build", "--verbose"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    stdout = completed.stdout.decode('utf-8')
    stderr = completed.stderr.decode('utf-8')

    warn(stdout)
    warn(stderr)

    if completed.returncode != 0:
        warn(f"{RED}[ERROR]{NC} Compiling zephyr failed.")
        sys.exit(1)


def gather_tests(base: str) -> list:
    """Gathers all test file as Path objects"""
    test_files = []
    for root, dirs, files in os.walk(base):
        # We take the root path relative to the working directory
        root_path = Path(root).resolve().relative_to(WORKDIR)
        for filename in filter(is_zephyr, files):
            test_files.append(root_path / filename)
    return test_files

TestBuild = collections.namedtuple(
    "TestBuild", ["success", "path", "stdout", "stderr"]
)
def build_tests(base, tests) -> List[TestBuild]:
    """Recursively walks the test directory to compile every file.
    """
    zephyr = get_zephyr()
    # -------------------------- compile each test --------------------------- #
    depth = len(Path(base).parts)  # depth of the base folder
    out_dir = Path(base) / 'out'

    test_builds = []
    for test_file in tests:
        out_path = reduce(Path.joinpath, test_file.parts[depth:], out_dir).with_suffix(".wasm")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        completed = subprocess.run(
            [zephyr, str(test_file), '-o', out_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        test_builds.append(
            TestBuild(
                success=(completed.returncode == 0),
                path=out_path,
                stdout=completed.stdout.decode("utf-8"),
                stderr=completed.stderr.decode("utf-8"),
            )
        )
    return test_builds


TestRun = collections.namedtuple(
    "TestRun", ["success", "stdout", "stderr"]
)
def run_tests(tests_built: List[TestBuild]) -> List[TestRun]:
    check_dependency('wasmtime', exit=True)

    # ---------------------------- run each test ----------------------------- #
    tests_run = []
    for test in tests_built:
        if test.success:
            completed = subprocess.run(
                ['wasmtime', str(test.path)],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                check=False,
            )
            stdout = completed.stdout.decode('utf-8')
            tests_run.append(TestRun(
                success=(stdout.strip() == "42"),
                stdout=completed.stdout.decode('utf-8'),
                stderr=completed.stderr.decode('utf-8'),
            ))
        else:
            tests_run.append(TestRun(
                success=False,
                stdout='',
                stderr='',
            ))
    return tests_run

# ─────────────────────────────────── CLI ──────────────────────────────────── #
def cli_parser() -> argparse.ArgumentParser:
    """Define the CLI interface of this testing script. Returns an argparse
    parser.
    """
    parser = argparse.ArgumentParser(
        description=textwrap.dedent(
            """
            Zephyr's compilator test-suite. Provides different utilities to test
            the language's main functionalities against different runtimes.
            If the test-suite doesn't find the compiled zephyr in the current
            repository, it will try using a system-wide zephyr command.
            """
        )
    )
    parser.add_argument(
        '-d', '--debug',
        help="maximal level of verbosity, for advanced debugging",
        action="store_const", dest="loglevel", const=logging.DEBUG,
        default=logging.WARNING,
    )
    parser.add_argument(
        '-v', '--verbose',
        help="increase the program's verbosity",
        action="store_const", dest="loglevel", const=logging.INFO,
    )
    parser.add_argument(
        "--dir", type=Path, help="path to the test directory",
        default=TEST_DIR, dest="test_dir",
    )
    parser.add_argument(
        "--build", help="build the zephyr compiler", action="store_true"
    )
    parser.add_argument(
        "--list", help="only list the tests", action="store_true"
    )
    return parser


# ────────────────────────── CLI parsing + logging ─────────────────────────── #
if __name__ == "__main__":
    # Parse arguments, setup the logger accordingly
    args = cli_parser().parse_args()
    logging.basicConfig(level=args.loglevel, format="%(message)s")
    debug(f"{MAGENTA}Arguments:{NC}\n  %s", args)

    # Build the toolkit if needed
    debug(f"{MAGENTA}Build:{NC}")
    build_zephyr() if args.build else debug("  → skipped (pass --build to build)")

    # Log zephyr path/version
    run = subprocess.run(
        [get_zephyr(), "--version"], stdout=subprocess.PIPE, check=True
    )
    debug(f"{MAGENTA}Zephyr:{NC}\n  → %s", run.stdout.decode('utf-8'))

    # Gather tests and retrieve informations for later display
    tests_list = gather_tests(args.test_dir)
    folders = [str(path.parent.name) for path in tests_list]
    test_names = [str(path.name) for path in tests_list]

    # Empty line for prettier separation of log/output
    debug("")

    # Small funtion to group tables by folders: separate if 2d element (folder)
    # of the table is different on consecutive rows
    separate_folders = lambda r1, r2: r1[1] != r2[1]

    # If just listing, print the tests and exit
    if args.list:
        table(
            ["id"] + [i for i, _ in enumerate(tests_list)],
            ["folder"] + folders,
            ["test"] + test_names,
            paddings=['left', 'left', 'left'],
            insert_separation=separate_folders,
        )
        sys.exit(0)

    # Build the tests, save informations
    tests_built = build_tests(args.test_dir, tests_list)

    # Run the tests, save informations
    tests_ran = run_tests(tests_built)

    # Symbols for each test on 'compiled' and 'passed'
    compiled = [YES if test.success else NO for test in tests_built]
    passed = [YES if run.success else NO for run in tests_ran]

    # Display the final summary
    # id | folder | test | compiles | passes
    table(
        ["id"] + [i for i, _ in enumerate(passed)],
        ["folder"] + folders,
        ["test"] + test_names,
        ["compiles"] + compiled,
        ["passes  "] + passed,
        paddings=['left', 'left', 'left', 'center', 'center'],
        insert_separation=separate_folders,
    )
