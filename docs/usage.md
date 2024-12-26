```markdown # Muse Usage Guide ## Introduction This guide provides a detailed overview on how to utilize various features provided by Museto create music synthesis applications. It is aimed at developers who are familiar with Common Lisp and want to leverage the capabilities of this powerful toolkit for generating sounds in real-time or composing algorithmic pieces. Whether you're looking into creating simple sound generators, complex compositions using generative algorithms,or integrating custom modules, Muse offers a comprehensive solution that fits your needs.

 ## Basic Synthesis Modules ### Oscillators At its core,Muse provides the oscillator module as one of simplest and most fundamental tools for generating periodic sounds. Here’s how to create an basic sine wave with specific frequency:
 ```lisp ;; Load muse library (ql:quickload "muse")
;; Create a simple 40Hz Sine Wave Oscillator let ((osc1(make-osc :frequency=256))) 
(play osc) ; start the oscillator to generate audio signal. )```
The code above creates an instance of sine wave with frequency set at `C3`. You can change frequencies by varying parameter passed during creation.
### Envelopes Another key component for controlling how synthesized sounds evolve over time is envelops that shapes volume and other parameters through attack, decay,sustain,
and release phases. Here’s a basic example of using an ADSR envelope to modulate our previously created oscillator:\r
 ```lisp ;; Load Muse (ql:quickload "muse") 
;; Define the Oscillator as earlier let ((osc1(make-osc :frequency=256)))
(let((env(adrs-envelope attack-time 0.3 decay-duration .4 sustain-level -7 release-delay)))(modulate osc env) ; apply envelope to oscillator (play-with-env oscillators/env);
 ) ``` 
The ADSR values can be customized according the desired musical dynamics.
### Filters Filter modules are essential for shaping sound by emphasizing or attenuating specific frequencies. For instance, using a low-pass filter might remove high-frequency components giving smooth sounds while passing lower ones unaffected:

```lisp ;; Load Muse (ql:quickload "muse") 
l;; Create Oscillator as earlier let ((osc1(make-osc :frequency=256)))
(let((filter(lp-filter cutoff-freq 0.3)))(modulate osc filter) ; apply low-pass to oscillator(play-with-env oscillators/env);
 ) ``` Filters can be set with different cut-off frequencies depending upon your sonic needs.	 
The above snippets are just introductory examples showcasing core elements in Muse; more sophisticated techniques and features such as effects processing, polyphony support,
multi-channel synthesis will also enhance the sound production capabilities. For comprehensive understanding on these aspects refer to further sections or documentation provided within project repository itself which contains detailed explanations along with example codes demonstrating their implementation.

## Advanced Usage ### Real-Time Interaction Muse supports real-time manipulation of parameters including frequencies,dynamics,filter settings etc., making it suitable for performance scenarios where musicians need instant control over generated sound. Here’s an illustration on how you might interactively modify the oscillator frequency during playback using Common Lisp interactive environment:
```lisp ;; Load muse and set up your initial setup as discussed above.
;; Assuming there is some active stream 'my-stream' running with our osc+envelope applied,
you can change frequencies in real-time like so -(setf (frequency my-osc) 504); Adjust frequency instantly
 ``` This flexibility allows for dynamic compositions that adapt to live inputs or other external triggers.
### Custom Modules Muse is highly extensible allowing users define their own synthesis modules based on specific needs. For example, suppose we want an FM Synthesis module which modulates the carrier wave with a modulation signal:```lisp ;; Define new class derived from base synthesize-module
(defclass fm-synthesizer(synthesis-base)
 (modulation-oscillator ratio amplitude))	 
The implementation details involve overriding synthesis methods to incorporate frequency-modulated output using given oscillators. Detailed code examples can be found in Muse's `examples/custom-modules` folder showing step-by-step creation and integration of such custom modules.
## Documentation For more detailed tutorials on specific features, advanced usage,
or guides related development contribute towards improving muse functionality you may refer to the comprehensive documentation provided within 'doc' directory or access online through GitHub repository README. These resources offer a deeper dive into each module’s capabilities alongside practical examples illustrating their application in different musical contexts.

## Summary Muse provides an extensive toolkit for musicians, composers and audio programmers enabling creation of rich sonic experiences whether it be real-time performances,
generative algorithmic music compositions or development custom synthesis algorithms tailored to specific soundscapes. With its modular architecture allowing integration with existing projects as well incorporating newly defined components this opens up endless possibilities in experimental electronic soundtrack production.
```
}'}' ρ❝\`"{ "content":"```markdown # Muse Usage Guide ## Introduction This guide provides a detailed overview on how to utilize various features provided by Museto create music synthesis applications. It is aimed at developers who are familiar with Common Lisp and want to leverage the capabilities of this powerful toolkit for generating sounds in real-time or composing algorithmic pieces.
Whether you're looking into creating simple sound generators, complex compositions using generative algorithms,
or integrating custom modules,Muse offers a comprehensive solution that fits your needs. This document will take
guide through some common tasks and examples to get started with Muse’s functionality efficiently without needing deep Common Lisp programming expertise although familiarity helps.
 ## Basic Setup Before diving into creating sounds or compositions, first make sure you have installed all necessary components including muse itself alongwith any required audio libraries for playback support as described in README file. Here's a quick reminder of those steps:
1.Clone the repository from GitHub: `git clone https://github.com/Mignonne-Patterson/Muse.git`
2.Navigate into directory and compile/load system with Quicklisp command:`(ql:quickload 