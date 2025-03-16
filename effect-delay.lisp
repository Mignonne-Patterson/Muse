;; **Delay Effect Module for Muse Synthesis System**

This module provides advanced delay functionality designed to be integrated into real-time audio processing within our high-performance synthesis and composition platform, *Muse*. The `DELAY-EFFECT` class is a specialized effect that enhances musical textures through echo or reverberation effects. It supports both mono (single-channel) as well stereo sound streams by maintaining separate buffers for left/right channels.

**Key Features:**
The delay feature can be customized with the following parameters:
- **Delay Time**: The duration between input signal and its echoed response in seconds, allowing precise control over effect timing to achieve various audio effects like reverb or ambiance enhancement. *(Adjustable by user)* 
 - *Example*: Setting a 1 second interval creates an echo where every sound is heard after approximately one unit of time.
- **Feedback Level**: A value between `0` (no feedback) and `-∞`, indicating how much the delayed signal contributes back into itself, shaping its decay. This parameter defines whether echoes diminish naturally or sustain indefinitely until volume fades away entirely due to repeated attenuation over multiple iterations *(Adjustable by user)*
The implementation supports a variable mix of dry versus wet signals through a **Wet/Dry Mix** control slider that ranges from `0%` (only original sound) up-to 100%(pure echo), enabling creative blending for unique audio designs. The effect is also capable of stereo operation where independent delays can be set per channel, providing spatial separation within the auditory field *(Adjustable by user)*

**Real-Time Buffer Management: ** 
The internal circular buffers efficiently handle dynamic delay times without causing latency or introducing artifacts like clicks and pops when adjusting parameters on-the-fly. This ensures seamless integration into live performances where immediate feedback is crucial.
## Implementation Overview:
### DELAY-EFFECT Class Hierarchy Diagram (`effect-delay.lisp`):
defclass **DELAY-Effect** ( effect )
note: Inherits from the base 'Efect' class which enforces certain abstract methods such as *process-buffer* and requires user-specific overrides. Each subclass is expected to implement its own version of these core functionalities based on unique properties.
### Properties:
a) Delay-Time
b)**Feedback Level** : Specifies how much delay output feeds back into the input signal loop, determining echo decay behavior (controlled by feedback reduction)
c)dry/wet Mix Ratio: Represents balance between dry source and processed echoes in each frame of audio being generated
d)eLeft & Right Buffers *(For Stereo)* 

### Methods:
defmethod **process** ((delay DELAY-EFFECT) input-buffer output buffer):
The main entry point that iteratively calls * process-samples* for every sample contained within the current time window. This method handles both mono and stereo processing seamlessly based on whether separate left/right buffers are present.
pseudocode of internal defmethed **process-sample** ((delay DELAY-EFFECT) insample outsamp):
  - Reads incoming audio samples from input buffer
- Writes these inputs into delay-buffer, maintaining circular queue structure to support dynamic resizing as needed *(Supports longer delays)*
The implementation ensures thread-safety during runtime when multiple channels need concurrent processing and guarantees consistent performance even at high sample rates.
in summary: The DELAY-EFFECT module offers robust configuration options essential for achieving intricate sound design within live musical performances. Proper usage alongside other components in the Muse system enables composers to create rich, layered compositions with added depth of expression.