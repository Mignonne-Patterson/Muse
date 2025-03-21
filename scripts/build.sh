#!/bin/bash
# This script builds Muse with Common Lisp.

set -e # Exit on error

echo "Starting build process for Muse..."

# Navigate to the Muse directory (update the path if necessary)
cd "$(dirname "$0")/.." || exit 1

# Compile the Muse system using Quicklisp's quickload command
sbcl --eval "(ql:quickload 'muse)" --eval "(sb-ext:save-lisp-and-die \"musesynth\" :executable t :toplevel #'muse:start)"

echo "Build completed successfully. Binary available as 'musesynth'"

exit 0
