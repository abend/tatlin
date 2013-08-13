#lang racket
(require srfi/13)

(define gcode%
  (class object%
    (init filename)

    ;; TODO: add tooling option (T0 default)

    (init-field [speed 2500]
                [move-speed 2500]
                [base-temp #f]
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

    (define/public (go x y z [comment #f])
      (set! extruder-pos (distance (- cur-x x) (- cur-y y) (- cur-z z)))
      (fprintf out "G1 X~a Y~a Z~a F~a E~a~a~n"
               x y z speed (number->format-decimal extruder-pos 3)
               (if comment (string-append " (" comment ")") ""))
      (set! cur-x x)
      (set! cur-y y)
      (set! cur-z z))

    (define/public (move x y z [mv-speed (or move-speed speed)] [comment #f])
      (fprintf out "M103 (Extrude off)~nG1 X~a Y~a Z~a F~a E~a~a~nM101 (Extrude on)~n"
               x y z speed (number->format-decimal extruder-pos 3)
               (if comment (string-append " (" comment ")") ""))
      (set! cur-x x)
      (set! cur-y y)
      (set! cur-z z))

    (define/public (comment . args)
      (fprintf out "(")
      (apply fprintf (append (list out) args))
      (fprintf out ")~n"))

    (define/public (done)
      (close-output-port out))

    (define/public (header x y z)
      ;; start the anchor in the proper corner
      (let ((anchor-x (if (< x 0) 118 -118))
            (anchor-y (if (< y 0) 75 -75)))
        (set! cur-x anchor-x)
        (set! cur-y anchor-y)
        (set! cur-z z)
        (fprintf out "(**** start.gcode for The Replicator, Dualstrusion! ****)
M73 P0 (enable build progress)
G21 (set units to mm)
G90 (set positioning to absolute)
M108 T0 (set extruder max speed)
")
        (when base-temp 
          (fprintf out "M109 S~a T0 (set HBP temperature)~n" base-temp))

        (fprintf out "
M104 S~a T0 (set extruder temperature)

 (**** begin homing ****)
G162 X Y F2500 (home XY axes maximum)
G161 Z F1100 (home Z axis minimum)
G92 Z-5
G1 Z0.0
G161 Z F100 (home Z axis minimum)
M132 X Y Z A B (Recall stored home offsets for XYZAB axis)
 (**** end homing ****)

G1 X~a Y~a Z150 F3300.0 (move to waiting position)
G130 X20 Y20 Z20 A20 B20 (Lower stepper Vrefs while heating)
M6 T0 (wait for toolhead, and HBP to reach temperature)
G130 X127 Y127 Z40 A127 B127 (Set Stepper motor Vref to defaults)
M108 R3.0 T0 (set extruder max speed)
G0 X~a Y~a (Position Nozzle)
G0 Z~a      (Position Height)
M108 R4.0    (Set Extruder Speed)
M101         (Start Extruder Fwd)
G4 P1500     (Dwell 1.5 sec)
 (**** end of start.gcode ****)

" extruder-temp
anchor-x anchor-y
anchor-x anchor-y
z)
        (go x y z "go to first point")))

    (define/public (footer)
      (fprintf out "
 (Finish up)
G1 F1200.0
G1 E~a
 (G1 F~a prob unnecessary)
M103
G4 P15000 (Dwell - cool off)

 (back up extruder .5?)

 (******* End.gcode*******)
M73 P100 ( End  build progress )
G0 Z150 ( Send Z axis to bottom of machine )
M18 ( Disable steppers )
M109 S0 T0 ( Cool down the build platform )
M104 S0 T0 ( Cool down the Right Extruder )
G162 X Y F2500 ( Home XY endstops )
M18 ( Disable stepper motors )
M70 P5 ( Done )
M72 P1  ( Play Ta-Da song )
 (*********end End.gcode*******)
" extruder-pos speed))

))

(define (distance . args)
  (sqrt (apply + (map (lambda (a) (* a a)) args))))

;; from http://lists.racket-lang.org/users/archive/2004-May/005566.html
(define (number->format-decimal x num)
  (let ([split (regexp-match "([0-9]*)\\.([0-9]*)"
                             (real->decimal-string x))])
    (format "~a.~a" (cadr split) (string-pad-right (caddr split) num
                                                   #\0))))

(define (test-gcode)
  (let ((gcode (new gcode% [filename "test.gcode"])))
    (send gcode header 12 23 .5)
    (send gcode comment "starting...")
    (send gcode dwell 1.5)
    (send gcode go 0 0 .5 "crazy")
    (send gcode move 0 1 .5)
    (send gcode go 1 1 .5)
    (send gcode pause 5.25)
    (send gcode comment "done.")
    (send gcode footer)
    (send gcode done)))
