# Delay Effect Module for Muse Synthesis System
This module provides a delay effect that can be used in real-time audio processing within the Muse system.
## OverviewThe `effect_delay` module allows users to add echoes or delays at various time intervals, enhancing musical textures and creating space effects. The implementation is based on feedback loops where an input signal's delayed version feeds back into itself with a certain amount of gain (reduction).
The delay effect can be configured for both mono (`1 channel`) as well stereo audio streams by adding two separate buffers that hold the left-channel data separately from right.
## Key Features- Adjustable **delay time** in seconds: controls how long it takes before an echo is produced.- Variable feedback amount between `0` (no delay) and less than or equal to `-1`, which results effectively infinite loops until silence due loss of amplitude at each iteration. The gain reduces with the number iterations creating a natural decay curve.
- Wet/Dry mix control: Allows blending in varying amounts from just dry signal through pure echo, providing creative possibilities for sound design.- Stereo capability offering independent delays on left and right channels (stereo mode).
The implementation also includes real-time buffer management to handle variable delay times efficiently without introducing latency or artifacts like clicks.
## Implementation DetailsThis section provides a high-level description of the code structure within `effect_delay.lisp`. The actual source files contain detailed comments explaining each part in detail but an overview is included here for clarity:
The main class, named **DELAY-EFFECT**, inherits from base effect classes provided by Muse which require overriding methods such as *process-buffer*.
```lisp
(defclass delay-effect (effect)
  ((delay-time :accessor get-delay-time :initarg :time :documentation "Delay time in seconds")
   ;; Additional parameters: feedback, mix level etc...
))```
The `DELAY-EFFECT` class has accessors for each parameter needed to control the effect and an initialization argument (`TIME`) that sets up how long before playback should start hearing echoes.
pseudocode of *process-buffer* method:
```lisp
(defmethod process ((delay delay-effect) inbuffer outbuffer)
  ;; Determine amount by which input buffer needs shifting based on sample rate & desired time offset (in seconds).
note: A circular queue or ring buffers are typically used for implementing the actual delaying mechanism.
pseudocode of internal *process-sample* method:
defmethod process-samples ((delay delay-effect) insample outsamp)
  ;; Write current input to buffer, then mix in delayed samples based on time offset
``` 
The module includes functionality such as dynamic re-sizing of buffers if required (for longer delays), and appropriate handling for stereo signals where separate processing is needed.
in summary: Delay effect offers flexible configuration options essential when wanting subtle or dramatic effects within live musical performances. The provided example should give a good idea about its usage alongside other components in the Muse system.