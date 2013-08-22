#lang racket/gui

(require sgl
         sgl/gl-vectors)

(define view-rotx 20.0)
(define view-roty 30.0)
(define view-rotz 0.0)

(define gl-draw (lambda ()
                  (gl-clear-color 1.0 1.0 1.0 0.0)
                  (gl-clear 'color-buffer-bit 'depth-buffer-bit)

                  (gl-push-matrix)
                  (gl-rotate view-rotx 1.0 0.0 0.0)
                  (gl-rotate view-roty 0.0 1.0 0.0)
                  (gl-rotate view-rotz 0.0 0.0 1.0)

                  ;; (gl-begin 'triangles)
                  ;; (gl-vertex 1 2 3)
                  ;; (gl-vertex-v (gl-float-vector 1 2 3 4))
                  ;; (gl-end)

                  (gl-begin 'polygon)
                  (gl-color 1 0 0) (gl-vertex .5 -.5 -.5)
                  (gl-color 0 1 0) (gl-vertex .5  .5 -.5)
                  (gl-color 0 0 1) (gl-vertex -.5 .5 -.5)
                  (gl-color 1 0 1) (gl-vertex -.5 -.5 -.5)
                  (gl-end)

                  (gl-pop-matrix)

                  (gl-flush)))

(define gl-init 
  (lambda ()
    (gl-shade-model 'smooth)
    (gl-clear-color 0.0 0.0 0.0 0.5)
    (gl-clear-depth 1)
    (gl-enable 'depth-test)
    (gl-depth-func 'lequal)
    (gl-hint 'perspective-correction-hint 'nicest)))

(define (gl-resize width height)
  (gl-viewport 0 0 width height)
  
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-perspective 45 (/ width height) 0.1 100)
  
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

(add-key-mapping 'escape (lambda () (display "quit!") (newline) (send gl-frame show #f)))


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
          (set! view-rotx (- view-rotx ydiff))
          (set! view-roty (- view-roty xdiff))
          (set! last-mouse-x x)
          (set! last-mouse-y y)))
      ;; (refresh)
      )

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
