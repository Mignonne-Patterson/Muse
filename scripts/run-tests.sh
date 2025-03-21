#!/bin/bash

echo "Running Muse tests..."

# Navigate into test directory if not already there.
cd "$(dirname "$0")/../tests" || { echo 'Test folder does not exist.'; exit 1; }

# Look for .asd files and load them as dependencies in quickload
for file in *.asd; do
  echo "Loading ${file}"
  sbcl --eval "(ql:quickload :${file%.*})"
done

# Execute any Lisp test script present within the tests directory.
if [ -f "run_tests.lisp" ]; then
  sbcl --load "run_tests.lisp"
else
  echo 'No Lisp test files found.'
  exit 1
fi

echo "Tests completed successfully."

exit 0
