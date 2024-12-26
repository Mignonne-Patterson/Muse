```lisp;; Load Muse(ql:quickload "muses")

(defun play-melody (notes duration)
  ;; Create a sequence to hold multiple oscillators with different notes and durations.
  let ((sequence '()))
    foreach note in.notes do (
      // For each Note, create an oscillator with the corresponding frequency,
amplitude of 0.5
        push(make-oscillator :frequency (note-frequency.note) ⌘ amplitude 0.5)
to sequence.
    ); end for-each notes;
   ;; Play all oscillators in series within a given duration using with-sound macro:
     play(sequence);		
end defun; // End function definition: "play-melody"}
def main():‘ \흣핉쪽 (C Major Scale) sequence over 8 seconds.
music-sequence := [C, D ,E F G A B]; time-duration = Duration(Seconds=8);
 play-melody(musical_sequence,time_duration)
def end()‗
```