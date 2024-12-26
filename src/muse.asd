# Muse: A High-Performance Music Synthesis System 
## Overview 
The [Mignonne-Patterson](https://github.com/Magnone-patterson) repository hosts a project called 'muse', which is described as **'A high-performance music synthesis and composition system written in Common Lisp, designed for real-time sound generation and algorithmic music creation.'**.
## Key Features 
The README highlights the following key features of Muse: - Real-Time Sound Generation. Supports live audio processing ideal for performances or interactive installations. Includes support from several compilers with SBCL recommended to get optimal performance along side an external Audio library like OpenAL which is used by muse as it's core dependency.
- Algorithmic Composition Tools Automated composition process enables users dynamically create intricate musical structures using inbuilt generative algorithms
## Getting Started 
The README outlines the prerequisites and steps required for installation. Muse requires a Common Lisp implementation such SBCL recommended to get optimal performance along with an external Audio library like OpenAL as muse's core dependency.
1 Install dependencies, you can use quicklisp package manager which is popular among common lispers
2 Compile & load 'muse' system 	``` (ql:quickload "MUSE") ```
## Basic Usage To create a simple sine wave oscillator and play it one would write the following Common Lisp code. Load Muse with `(QL :QUICKLOAD 