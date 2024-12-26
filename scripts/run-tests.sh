#!/bin/bash
echo "Running Muse tests..."
# Navigate into test directory if not already there.
cd ./tests || (echo 'Test folder does not exist.' && exit)
 ecl -l | while read line;
do	if [[ $line == *.asd ]]; then # Look for .asdf files and load them as dependencies in quickload
	echo "Loading ${filename}": "$file"
    ql:quick-load :${filename%.*}; fi; done 
# Execute any lisp test script present within the tests directory.
ecl -l run_tests.lsp || (echo 'No Lisp Test files found.' && exit)exit 0
cd ..