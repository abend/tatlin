#lang racket

(define edge%
  (class object%
    (init-field vert-p vert-q) ;; two verts
    (field [face-1 #f])
    (field [face-2 #f])
    (field [visited #f])
    
    (define/public (add-face f)
      (cond ((null? face-1) (set! face-1 f))
            ((null? face-2) (set! face-2 f))
            (#t (error 'add-face "too many faces already" f))))
    
    (define (intersects test)
      ;; we don't want to intersect with an edge that lies parallel to the intersection plane
      (and
       (!= (get-field vert-p value) (get-field vert-q value))
       (or (and (<= p.value test) (<= test q.value))
           (and (<= q.value test) (<= test p.value)))))
    
    (define (intersection test)
      (let ((d (/ (- test p.value) (- q.value p.value)))
            (v (new vert% [x p.x] [y p.y] [z p.z])))
        (set! v.x (+ v.x (* d (- q.x p.x))))
        (set! v.y (+ v.y (* d (- q.y p.y))))
        (set! v.z (+ v.z (* d (- q.z p.z))))
        v))
    
    ))

(define vert%
  (class object%
    (init-field [x 0] [y 0] [z 0])
    (field [value 0])))
