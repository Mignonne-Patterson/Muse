#!/bin/bash
# This script builds Muse with Common Lisp.
set -e # Exit on error

echo "Starting build process for Muse..."
cd /path/to/Muse/
#	Compile the muse system using Quicklisp's quickload command and compile it into a binary file that can be executed independently of SBCL (Steel Bank Common Lisp).
esac $(ql:quickload 'muse) --output musesynth
echo "Build completed successfully. Binary available as 'musysnth'"
exit 0