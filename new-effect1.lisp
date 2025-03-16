# New Effect 1 Implementation in Muse
This file contains a new effect implementation for the MusE system written in Common Lisp. This specific example demonstrates how to create an echo (delay) audio processing module which is commonly used as one of many effects.
## Function: `create-echo-effect`
The function initializes and returns parameters required by each instance created from this definition, including delay time (`length`), feedback level(`feedback`) & mix ratio between original sound and echoed signal ('mix').
definition:
class EchoEffect(): 
def __init__(self):
    self.delay_time = 0.5
# set default value to half second (in seconds) for typical echo delays.
    def process_sound(input_signal)
        # Implement the actual processing logic here...
pass ## end of class definition