#!/usr/bin/env sh

ZEPHYR="./target/debug/zephyr"
RUNTIME="wasmtime"
TEST_PATH="test"
TEST_OUTPUT_PATH="test_out"

# Colors
BOLD='\033[1m'
BLACK='\033[30m'
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN="\033[0;36m"
NC='\033[0m'

# Reading arguments
verbose=false
compiler_verbose=false
while getopts ":hvc" opt; do
    case ${opt} in
        h ) # help
            printf "Usage: tests.sh [-h] [-v]\n"
            ;;
        v ) # verbose
            verbose=true
            ;;
        c ) # verbose
            compiler_verbose=true
            ;;
    esac
done

# Header
printf "${CYAN}/// Fork integration tests ///${NC}\n"
printf "Using $RUNTIME as runtime\n\n"

if ! [ -x "$(command -v $RUNTIME)" ]; then
    printf "${RED}Error: $RUNTIME is not installed${NC}\n" >&2
    exit 1
fi

if [ ! -f "$ZEPHYR" ]; then
    printf "Could not find zephyr"
    exit 1
fi

# Compile tests
printf "${BOLD}Compiling tests ...${NC}\n"
for source in "$TEST_PATH"/*.zph; do
    output="$TEST_OUTPUT_PATH/$(basename $source .zph).wasm"

    printf "> $source"

    # Set the verbose flag if the compile-verbose mode is selected
    flags=""
    if [[ "$compiler_verbose" = true ]]; then
        flags="-v"
    fi

    trace=$($ZEPHYR $flags $source -o $output)
    if ! [ "$?" = "0" ]; then
        printf " ${RED}✗${NC}\n"
        printf "${RED}Failed to compile $(echo $source)${NC}\n\n"
        if [ "$verbose" = true ]; then
            printf "${CYAN}%s${NC}\n" "-------------------------------------- trace --------------------------------------"
            printf "%s\n" "$trace"
            printf "${CYAN}%s${NC}\n" "-----------------------------------------------------------------------------------"
        else
            printf "→ use -v (verbose) to see the trace\n"
        fi
        exit 1
    fi
    printf " ${GREEN}✓${NC}\n"
done
printf "\n"

# Run in $RUNTIME
printf "${BOLD}Running tests ...${NC}\n"
for source in "$TEST_OUTPUT_PATH"/*.wasm; do
    printf "> $source"
    if ! $RUNTIME $source 2> /dev/null | grep -q 42 ; then
        printf " ${RED}✗${NC}\n"
        printf "${RED}Failed test $(echo $source)${NC}\n"
        if [ "$verbose" = true ]; then
            # Temporary work-around: run the command several times
            # @TODO: run the command once, split stdout/stderr, cache them
            printf "\n"
            printf "${CYAN}%s${NC}\n" "------------------------------------- stderr --------------------------------------"
            printf "%s" "$($RUNTIME $source 1> /dev/null)"
            printf "${CYAN}%s${NC}\n" "-----------------------------------------------------------------------------------"
            printf "\n"
            printf "${CYAN}%s${NC}\n" "------------------------------------- stdout --------------------------------------"
            printf "%s\n" "$($RUNTIME $source 2> /dev/null)"
            printf "${CYAN}%s${NC}\n" "-----------------------------------------------------------------------------------"
        else
            printf "→ use -v (verbose) to see the stderr and stdout\n\n"
        fi
        exit 1;
    fi
    printf " ${GREEN}✓${NC}\n"
done

printf "\n${GREEN}Success${NC}\n"
