#lang racket

(require (planet jaymccarthy/gl-world:2:1)
         sgl
         sgl/init)

;; (define steps +inf.0)
(define rate 60)
;; (define dt (exact->inexact rate))

(define display-width 800)
(define display-height 600)

;; (require scheme/runtime-path)
;; (define-runtime-path texture-path '(lib "stop-32x32.png" "icons"))

(big-bang exact-integer? 0 
          #:height display-height
          #:width display-width
          #:on-tick 
          (lambda (i)
            (add1 i))
          #:tick-rate rate
          #:on-key 
          (lambda (i k)
            ;; (define force
            ;;   (match (send k get-key-code)
            ;;     ['down (cpv 0.0 (* -1 strength))]
            ;;     ['up (cpv 0.0 strength)]
            ;;     ['left (cpv (* -1 strength) 0.0)]
            ;;     ['right (cpv strength 0.0)]
            ;;     [else #f]))
            i)
          ;; #:draw-init
          ;; (lambda ()
          ;;   (set-box! stop-text (gl-load-texture texture-path)))
          #:on-draw 
          (lambda (i)
            (gl-clear-color 255 255 255 0)
            (gl-clear 'color-buffer-bit)
            
            ;; (gl-init display-width display-height
            ;;          width height
            ;;          (/ width 2) (/ height 2)
            ;;          0 0)
            
            ;; (gl-bind-texture (unbox stop-text))
            ;; (for ([t (in-list tiles)])
            ;;   (with-translate (body-x t) (body-y t)
            ;;     (with-rotation (* (cpBody-a t) (/ 180 pi))
            ;;       (gl-color 1 1 1 1)
            ;;       (gl-draw-rectangle/texture block-size block-size))))
            
            (gl-color 1 1 1 1)
            ;; (with-translate 5 5
            ;;                 (with-scale  5 5
            ;;                              (gl-draw-circle 'solid)))
)
          ;; #:stop-when 
          ;; (lambda (i)
          ;;   (i . >= . steps))
          ;; #:stop-timer
          ;; (lambda (i)
          ;;   (i . >= . steps))
          )
