#!/bin/bash

CUR_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)"
BASE_DIR="$CUR_DIR/.."
UC=${UC:-"$BASE_DIR/uc"}
EXAMPLES_DIR="$BASE_DIR/examples"
ACTUALS_DIR="$BASE_DIR/tests/actual"
EXPECTED_DIR="$BASE_DIR/tests/expected"
RESOURCES_DIR="$BASE_DIR/tests/resources"
BOOTSTRAP=0
FILTER=""
DIFF=diff

if command -v delta >/dev/null; then
  DIFF="delta -s --word-diff-regex='.'"
fi

[[ -d "$ACTUALS_DIR" ]] || mkdir -p "$ACTUALS_DIR"

FAILED=0
ERROR=0
OK=0
SKIP=0

check() {
  local name=$1
  local actual="$ACTUALS_DIR/$1.out"
  local expected="$EXPECTED_DIR/$1.out"

  [[ $BOOTSTRAP -eq 1 ]] && ( echo "Bootstrapping $name"; cp $actual $expected )

  if [[ ! -f "$expected" ]]; then
    echo "** ERROR: $name missing expected $expected";
    ERROR=$((ERROR + 1))
    return 0
  fi

  local diff=$(diff "$expected" "$actual")

  if [[ -n "$diff" ]]; then
    echo "** FAILED: $name"
    $DIFF "$actual" "$expected"
    echo
    FAILED=$((FAILED + 1))
  else
    echo "** OK: $name"
    OK=$((OK + 1))
  fi
}

run_test() {
  local name=$1
  local file=$2
  local input=$3
  local extra_args=$4
  local out="$ACTUALS_DIR/$name.out"

  if [[ -n "$FILTER" && ! "$name" =~ $FILTER ]]; then
    echo "** SKIP: $name"
    SKIP=$((SKIP + 1))
    return 0
  fi

  local cmd="$UC run $extra_args $file"

  [[ -f "$input" ]] && input=$(cat "$input")

  echo "$input" | $cmd > "$out" 2>&1
  status=$?
  echo $status >> "$out"

  if [[ $status -ne 0 && ! "$name" =~ ^err.* ]]; then
    echo "** ERROR: $name exited with $status"
    ERROR=$((ERROR + 1))
  else 
    check "$name"
  fi
}

# skips the one that seem to require input
suite_auto() {
  for f in "$EXAMPLES_DIR"/*; do
    if ! grep -q input $f; then
      run_test $(basename $f) "$f"
    fi
  done
}

suite_manual() {
  run_test "fac-iter-5" "$EXAMPLES_DIR/fac-iter.uc" 5 
  run_test "fac-rec-5" "$EXAMPLES_DIR/fac-rec.uc" 5
  run_test "fib-iter-42" "$EXAMPLES_DIR/fib-iter.uc" 42
  run_test "fib-rec-42" "$EXAMPLES_DIR/fib-rec.uc" 42
  run_test "fib-rec-slow-10" "$EXAMPLES_DIR/fib-rec-slow.uc" 10
  run_test "bf-helloworld" "$EXAMPLES_DIR/bf.uc" "$RESOURCES_DIR/helloworld.bf" --ascii
  run_test "bf-sierpinski" "$EXAMPLES_DIR/bf.uc" "$RESOURCES_DIR/sierpinski.bf" --ascii
}

suite() {
  suite_auto
  suite_manual
  echo "-----"
  echo "OK: $OK, FAILED: $FAILED, ERROR: $ERROR, SKIP: $SKIP"
}

usage() {
cat << EOF
usage: $0 [options]

Options:
  -h | -help        show this help
  --bootstrap       bootstrap the output
  --filter REGEX    only test cases that matches REGEX
EOF
}

while (( "$#" )); do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    --bootstrap)
      BOOTSTRAP=1
      shift
      ;;
    --filter)
      FILTER="$2"
      [[ -n "$FILTER" ]] || ( echo "Missing filter"; exit 1 )
      shift 2
      ;;
    *)
      echo "Unknown option $1"
      exit 1
      ;;
  esac
done

suite
