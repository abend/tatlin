#lang racket/gui

(require sgl
         sgl/gl-vectors)

(define pen-color '(.1 1 .1))

(define view-rotx -20.0)
(define view-roty -30.0)

(define view-dist 150.0)
(define min-view-dist 1)
(define max-view-dist 850)

(define build-plate-w 230) ;; in mm
(define build-plate-d 150) ;; in mm

(define gl-draw (lambda ()
                  (gl-clear-color 1.0 1.0 1.0 0.0)
                  (gl-clear 'color-buffer-bit 'depth-buffer-bit)

                  (gl-push-matrix)
                  (gl-look-at 0 0 (- view-dist) 0 0 0 0 1 0)
                  (gl-rotate view-rotx 1.0 0.0 0.0)
                  (gl-rotate view-roty 0.0 1.0 0.0)

                  (draw-model)

                  (draw-build-plate)

                  (gl-pop-matrix)

                  (gl-flush)))

(define (draw-model)
  (gl-line-width 3.5)
  (gl-begin 'line-strip)
  (apply gl-color pen-color) 
  (gl-vertex 0 0 0)
  (gl-vertex 1 1 0)
  (gl-vertex 1 2 0)
  (gl-vertex 2 3 0)
  (gl-vertex -1 2 0)
  (gl-end))

(define (draw-build-plate)
  (let* ((hw  (/ build-plate-w 2))
         (hd (/ build-plate-d 2))
         (y 0)
         (y-over (+ y .1)))
    ;; center dot
    (gl-begin 'polygon)
    (gl-color 0 0 0)
    (gl-vertex  1 y-over 1)
    (gl-vertex  1 y-over -1)
    (gl-vertex  -1 y-over -1)
    (gl-vertex  -1 y-over 1)
    (gl-end)

    ;; alignment tri
    (gl-begin 'polygon)
    (gl-color 0 0 0)
    (gl-vertex  5 y-over (- hd))
    (gl-vertex  -5 y-over (- hd))
    (gl-vertex  0 y-over (- 0 hd 5))
    (gl-end)

    ;; plate
    (gl-begin 'polygon)
    (gl-color 0 0 0 .2)
    (gl-vertex (- hw) y (- hd))
    (gl-vertex (- hw) y  hd)
    (gl-vertex  hw y  hd)
    (gl-vertex  hw y (- hd))
    (gl-end)

    ;; border
    (gl-line-width 1.5)
    (gl-begin 'line-loop)
    (gl-color 0 0 0) 
    (gl-vertex (- hw) y-over (- hd))
    (gl-vertex (- hw) y-over  hd)
    (gl-vertex hw y-over  hd)
    (gl-vertex hw y-over (- hd))
    (gl-end)))

(define gl-init 
  (lambda ()
    (gl-shade-model 'smooth)
    (gl-clear-color 0.0 0.0 0.0 0.5)
    (gl-clear-depth 1)
    (gl-enable 'depth-test)
    (gl-depth-func 'lequal)
    (gl-enable 'blend)
    (gl-blend-func 'src-alpha 'one-minus-src-alpha)

    (gl-enable 'line-smooth)
    (gl-enable 'polygon-smooth)
    (gl-hint 'line-smooth-hint 'nicest)
    (gl-hint 'polygon-smooth-hint 'nicest)

    ;; (gl-enable 'fog)
    ;; (gl-fog 'fog-mode 'exp2)
;;     (gl-fogfv 'fog-color fog-color)
;; glFogf (GL_FOG_DENSITY, density);
;; glHint (GL_FOG_HINT, GL_NICEST);

    (gl-hint 'perspective-correction-hint 'nicest)))

(define (gl-resize width height)
  (gl-viewport 0 0 width height)
  
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-perspective 45 (/ width height) 0.1 1000)
  
  (gl-matrix-mode 'modelview)
  (gl-load-identity))


;;; keyboard events

(define (recursive-handle-key list code)
  (cond
   ((empty? list) void)
   ((equal? (caar list) code) ((car (cdr (car list)))))
   (else (recursive-handle-key (rest list) code))))

(define *key-mappings* '())

(define (add-key-mapping key fn)
  (set! *key-mappings* (cons (list key fn) *key-mappings*)))

(define (clear-key-mappings)
  (set! *key-mappings* '()))

(define (gl-handlekey key)
  (recursive-handle-key *key-mappings* (send key get-key-code)))

(add-key-mapping 'escape
                 (lambda () (send gl-frame show #f)))
(add-key-mapping 'wheel-down
                 (lambda () 
                   (set! view-dist (max min-view-dist (- view-dist (max .25 (* view-dist .1)))))))
(add-key-mapping 'wheel-up
                 (lambda () 
                   (set! view-dist (min max-view-dist (+ view-dist (max .25 (* view-dist .1)))))
                   (displayln view-dist)))


;;; the gl canvas

(define glcanvas%
  (class canvas%
    (inherit refresh with-gl-context swap-gl-buffers)

    (field [last-mouse-x 0] 
           [last-mouse-y 0])

    (define init? #f)

    (define/override (on-paint)
      (with-gl-context 
       (lambda ()
         (unless init?
           (gl-init)
           (set! init? #t))
         (gl-draw)
         (swap-gl-buffers)))
      (queue-callback (lambda () (refresh)) #f))
    
    (define/override (on-size w h)
      (with-gl-context 
       (lambda ()
         (gl-resize w h)))
      (refresh))
    
    (define/override (on-char key)
      (gl-handlekey key)
      (refresh))

    (define/override (on-event event)
      (when (send event button-down?)
        (set! last-mouse-x (send event get-x))
        (set! last-mouse-y (send event get-y)))
      (when (send event dragging?)
        (let* ((x (send event get-x))
               (y (send event get-y))
               (xdiff (- x last-mouse-x))
               (ydiff (- y last-mouse-y)))
          (set! view-rotx (max -45 (min 45 (- view-rotx ydiff))))
          (set! view-roty (+ view-roty xdiff))
          (set! last-mouse-x x)
          (set! last-mouse-y y))))

    (super-new (style '(gl no-autoclear)))))

(define gl-frame #f)

(define (gl-run)
  (set! gl-frame (new frame% (label "OpenGL Window") 
                      (width 640) 
                      (height 480)))
  (let ((glcanvas (new glcanvas% (parent gl-frame))))
    (unless (send (send (send glcanvas get-dc) get-gl-context) ok?)
      (display "Error: OpenGL context failed to initialize")
      (newline)
      (exit))
    (send gl-frame show #t)))

(gl-run)
