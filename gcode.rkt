#lang racket

(define gcode% 
  (class object%
    (init filename)

    ;; TODO: add tooling option (T0 default)

    (init-field [speed 2500]
                [move-speed 2500]
                [base-temp 0]
                [extruder-temp 210])

    (field [extruder-pos 0]
           [cur-x 0]
           [cur-y 0]
           [cur-z 0]
           [out (open-output-file filename #:mode 'text #:exists 'replace)])

    (super-new)

    (define/public (dwell seconds)
      (fprintf out "G4 P~a (Dwell ~as)~n" (exact-round (* seconds 1000)) seconds))

    (define/public (pause seconds)
      (fprintf out "M103 (Extrude off)~nG4 P~a (Dwell ~as)~nM101 (Extrude on)~n"
               (exact-round (* seconds 1000)) seconds))

    (define/public (go x y z [comment ""])
      (fprintf out "G4 P~a (Dwell ~as)~n" (exact-round (* seconds 1000)) seconds))
    

    (define/public (comment . args)
      (fprintf out "(")
      (apply fprintf (append (list out) args))
      (fprintf out ")~n"))

    (define/public (done)
      (close-output-port out))

    ))

(define (test-gcode)
  (let ((gcode (new gcode% [filename "test.gcode"])))
    (send gcode comment "starting...")
    (send gcode dwell 1.5)
    (send gcode pause 5.25)
    (send gcode comment "done.")
    (send gcode done)))
