#!/bin/bash# Muse - Clean Script for Removing Build Files and Temporary Artifacts.#and Contributors (MIT)

set +x # Turn off verbose mode if already on, to avoid redundant output in clean logs:
cd "$(dirname "$0")" || exit 1   
rmdir --ignore-fail-on-non-empty .git/refs/original
echo 