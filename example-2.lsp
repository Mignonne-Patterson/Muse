;; Load Muse library using Quicklisp for dependency management.
(ql:quickload "muse")

;; Import necessary packages from the muse namespace to access its features globally in our example script.
(in-package :muse)

;; Define a function named 'create-complex-sound' which demonstrates how we can use various synthesis techniques
;; along with algorithmic composition tools provided by Muse for generating richer sounds.
(defun create-complex-sound (duration)
  "Generates a complex audio signal consisting of multiple layers, each utilizing different synthesis methods including noise generators and FM modulation."
  
  ;; Define local variables for parameters we want to control:
  (let* ((sample-rate 48000)  ;; Set sampling rate (e.g., 48000 Hz).
         (frequency1 440)     ;; First frequency of our base oscillator.
         (amplitude2 0.5))    ;; Amplitude envelope shape and duration information about the second component layer in sound mix.
    
    ;; Initialize audio output context with specified settings for producing the overall generated soundtrack during runtime:
    (with-sound (duration sample-rate)
      ;; Define two oscillators using sine waveforms at different frequencies, one modulated by another (frequency modulation):
      (let* ((o1 (make-oscillator :waveform 'sine :frequency frequency1))
             (o2 (make-fm-synth o1)))  ;; Modulation source is previously created oscillator.
        
        ;; Apply FM technique on the second synth unit using parameters set above to create more interesting timbral characteristics of sound production process.
        (freqmodulate o2)
        
        ;; Define a noise generator for adding texture:
        (let ((noise-gen (make-noise-generator :type 'white)))
          
          ;; Combine sounds from oscillators and noises into final mix by interleaving their audio streams in appropriate proportion according to desired balance between components:
          (let ((combined-sound (mix o1 noise-gen)))
            
            ;; Turn on our mixed signal generation within the sound output environment we defined earlier:
            (play combined-sound))))))
  
  ;; Call this function with some sample values to produce a short composition lasting 15 seconds as an example usage of creating complex multi-layered sonic textures through programmable audio synthesis:
  (create-complex-sound 15))

;; Explanation:
;; The primary purpose here was to illustrate how to create a complex audio signal by combining multiple synthesis techniques.
;; The example demonstrates how FM synthesis and noise generation can be used together to produce richer, more textured sounds.
;; This showcases the advanced sound design capabilities within the Muse framework.
