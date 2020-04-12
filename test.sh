#!/usr/bin/env sh

FORK="./target/debug/fork"
RUNTIME="wasmtime"
TEST_PATH="test"
TEST_OUTPUT_PATH="test_out"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN="\033[0;36m"
NC='\033[0m'

echo ""
echo -e "${CYAN}/// Fork integration tests ///${NC}"
echo ""
echo "Using $RUNTIME as runtime"

if ! [ -x "$(command -v $RUNTIME)" ]; then
  echo -e "${RED}Error: $RUNTIME is not installed${NC}" >&2
  exit 1
fi

if [ ! -f "$FORK" ]; then
    echo "Could not find fork"
    exit 1
fi

# Compile tests
echo "Compiling..."
for source in "$TEST_PATH"/*.frk; do
    output="$TEST_OUTPUT_PATH/$(basename $source .frk).wasm"
    $FORK $source $output > /dev/null
    if ! [ "$?" == "0" ]; then
        echo -e "${RED}Failed to compile $(echo $source)={NC}"
        exit 1;
    fi
done

# Run in $RUNTIME
echo "Running tests..."
for source in "$TEST_OUTPUT_PATH"/*.wasm; do
    if ! $RUNTIME $source 2> /dev/null | grep -q 42 ; then 
        echo -e "${RED}Failed test $(echo $source)${NC}"
        exit 1;
    fi
done

echo ""
echo -e "${GREEN}Success${NC}"
echo ""