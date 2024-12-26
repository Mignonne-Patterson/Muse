# composition.lisp - Composition Tools for Muse Music Synthesis System (Common Lisp)

## Overview
This module provides a comprehensive set of tools to aid in algorithmic and procedural music generation within the Common Lisp environment. It enables users to create complex musical compositions by defining sequences, patterns, harmonies dynamically.

### Key Components:
- **Sequence**: Represents ordered sets that can be played sequentially or looped through based on specific parameters like duration, tempo changes etc., allowing for multi-track composition where tracks have distinct timings and overlapping parts without manual adjustment of every note's timing individually. \-	**Pattern Generator:** Generates musical patterns using deterministic (e.g.: Markov chains), stochastic algorithms as well as rule-based methods; making it easy to produce intricate rhythmic or melodic motifs which can be utilized in larger compositions by varying their tempos, pitches and timbres.

## Usage:
The following examples demonstrate how the composition tools provided within this module could potentially integrate with your muse project. For simplicity we will focus on using sequences alongside pattern generators for creating a basic yet dynamic score that changes its harmonic structure based upon time or other external triggers without intervention after initial setup by user (automation).

### Example 1: Creating and Playing Simple Melodic Sequences:```lisp(ql:defsystem :muse-composition)
defpackage muse/compositions (:nicknames 