#!/usr/bin/env python3
# encoding: utf-8
"""
Test-suite script for the Fork language (https://github.com/CharlyCst/fork).
This script provides all capabilities to compile the fork compiler from rust,
then compile all of the fork test files and execute the tests written in fork.
"""
# ───────────────────────────────── headers ────────────────────────────────── #
__author__ = "Simon Lassourreuille (Slyces)"
__credits__ = ["Simon Lassourreille", "Charly Castes"]
__version__ = "0.1.0"
__maintainer__ = "Simon Lassourreuille"
__email__ = "slyces.coding@gmail.com"
__status__ = "Development"

# ───────────────────────────────── imports ────────────────────────────────── #
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

# Constants
FORK = "./target/debug/fork"

# Colors shortcuts through ANSI escape codes
RED     = "\x1b[31m"
GREEN   = "\x1b[32m"
YELLOW  = "\x1b[33m"
CYAN    = "\x1b[36m"
MAGENTA = "\x1b[35m"
BLUE    = "\x1b[34m"
BOLD    = "\x1b[1m"
ITA     = "\x1b[3m"
NC      = "\x1b[0m"  # no-color, or reset

YES = f"{GREEN}✓{NC}"
NO = f"{RED}✗{NC}"

info = logging.info
debug = logging.debug
warn = logging.warning

def check_dependency(command: str, exit: bool=True) -> bool:
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

    debug(f"%s dependency: %s", command, YES)
    return valid


def table(*columns, paddings: List[str] = None):
    """Given columns as separate lists, prints a nice layout, similar to the
    unix tool column(1).
    """
    # Deal with string length/padding with ansi escape codes
    strip_ansi = re.compile(r"\x1b\[[;\d]*[A-Za-z]")
    # Compute the lenght without escape code (displayed chars)
    length = lambda string: len(strip_ansi.sub('', str(string)))
    # Padd using the custom length
    pad_functions = {
        "left":   lambda s, n: s.ljust(n + len(s) - length(s)),
        "right":  lambda s, n: s.rjust(n + len(s) - length(s)),
        "center": lambda s, n: s.center(n + len(s) - length(s)),
    }
    assert paddings is None or all(pad in pad_functions for pad in paddings)

    # Built rows padded with the right size for each col
    sizes = [max(map(length, col)) for col in columns]

    def format_row(sep: str, row: List[str], paddings: List[str] = None):
        pad = (
            [pad_functions[p] for p in paddings]
            if paddings
            else [pad_functions["left"]] * len(columns)
        )
        return sep.join(pad[i](str(cell), sizes[i]) for (i, cell) in enumerate(row))

    rows = list(zip(*columns))
    print(format_row(' │ ', rows[0], ['center'] * len(columns)))
    print(format_row('═╪═', ['═' * sizes[i] for i,_ in enumerate(columns)]))
    print('\n'.join(format_row(' │ ', r, paddings) for r in rows[1:]))


def build_fork():
    """Builds the fork language for the current language using a separate
    process.
    """
    check_dependency('cargo', exit=True)

    # Blocking call to cargo
    warn("→ building fork")
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
        warn(f"{RED}[ERROR]{NC} Compiling fork failed.")
        sys.exit(1)


    # base = Path(base)
    # for root, dirs, files in os.walk(base):
        # proot = Path(root)
        # depth = len(proot.parents) - len(base.parents)
        # print("   " * depth, proot.name)
        # for fpath in files:
            # print("   " * (depth + 1), fpath)

def gather_tests(base: str) -> list:
    test_files = []
    for root, dirs, files in os.walk(base):
        for filename in files:
            if filename[-4:] == '.frk':
                test_files.append(Path(root) / filename)
    return test_files

TestBuild = collections.namedtuple(
    "TestBuild", ["success", "path", "stdout", "stderr"]
)
def build_tests(base, tests) -> List[TestBuild]:
    """Recursively walks the test directory to compile every file.
    """
    # ------------------------- find fork executable ------------------------- #
    if check_dependency(FORK, exit=False):
        fork_exec = FORK
    elif check_dependency('fork', exit=False):
        fork_exec = 'fork'
    else:
        warn(f"{RED}[ERROR]{NC} both local (%s) and global (fork) executable were not found", FORK)
        exit(127)

    # -------------------------- compile each test --------------------------- #
    n = len(Path(base).parts)  # depth of the base folder
    out_dir = Path(base) / 'out'

    test_builds = []
    for test_file in tests:
        out_path = reduce(Path.joinpath, test_file.parts[n:], out_dir).with_suffix(".wasm")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        completed = subprocess.run(
            [fork_exec, str(test_file), '-o', out_path],
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


def cli_parser() -> argparse.ArgumentParser:
    """Define the CLI interface of this testing script. Returns an argparse
    parser.
    """
    parser = argparse.ArgumentParser(
        description=textwrap.dedent(
            """
            Fork's compilator test-suite. Provides different utilities to test
            the language's main functionalities against different runtimes.
            If the test-suite doesn't find the compiled fork in the current
            repository, it will try using a system-wide fork command.
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
        "--build", help="build the fork compiler", action="store_true"
    )
    parser.add_argument(
        "--list", help="only list the tests", action="store_true"
    )
    return parser



if __name__ == "__main__":
    args = cli_parser().parse_args()
    logging.basicConfig(level=args.loglevel, format="%(message)s")

    debug(f"{MAGENTA}Arguments:{NC} %s", args)

    if args.build:
        build_fork()
    else:
        debug("→ not building fork")

    # tests = build_tests('tests', {})

    tests_list = gather_tests('tests')
    if args.list:
        warn(f"→ Gathered {MAGENTA}%d{NC} tests", len(tests_list))

    tests_built = build_tests('tests', tests_list)
    tests_ran = run_tests(tests_built)

    folders = [str(path.parent)[6:] for path in tests_list]
    test_names = [str(path.name) for path in tests_list]
    compiled = [YES if test.success else NO for test in tests_built]
    passed = [YES if run.success else NO for run in tests_ran]

    table(
        ["id"] + [i for i,_ in enumerate(passed)],
        ["folder"] + folders,
        ["test"] + test_names,
        ["compiles"] + compiled,
        ["passes  "] + passed,
        paddings=['left', 'left', 'left', 'center', 'center'],
    )
