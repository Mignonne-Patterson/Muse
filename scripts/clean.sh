#!/bin/bash
# Muse - Clean Script for Removing Build Files and Temporary Artifacts.
# (C) 2023 Mignonne Patterson and Contributors (MIT)

# Turn off verbose mode if already on, to avoid redundant output in clean logs:
set +x

# Navigate to the script directory:
cd "$(dirname "$0")" || exit 1

# Remove specific directories and files:
rmdir --ignore-fail-on-non-empty .git/refs/original

# Add additional clean-up commands as necessary:
# Example:
# rm -rf build/
# rm -rf temp/

echo "Clean-up completed successfully."
