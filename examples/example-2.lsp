# Example 2: FM Synthesis in Muse 
This example demonstrates how to create a Frequency Modulation (FM) synthesis sound using muse. In frequency modulation, one waveform modulates the amplitude or frequency of another wave.

```lisp ;; Load Muse library require 'muse' ``` ## Define an Operator and Carrier Oscillator First we will define two oscillators: - A carrier oscillator which is what you hear as your main tone - An operator that modifies its pitch over time 
define_carrier = muse::make_osc(40, 1.5) # Frequency in Hz (C3), amplitude of .7
define_modulator= Muse.make_sawtooth(
 frequency:2,
amplitude:.8)
declare modulated_wave
``` ## Create Modulation Now we will apply the FM synthesis by applying our saw wave as a modulation to carrier's pitch. ```lisp declare fm_sound = muse::create_fm(define_carrier, define_modulator) with-sound ((duration 5)) (playfm Sound ) end 