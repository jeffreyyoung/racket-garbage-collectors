#lang plai/collector

; HEADER DEFINITION
; 0 - type
; 1 - marked
(define header-size 2)

(define heap-ptr 'uninitialized-heap-ptr)

(heap-size)

(define (mark-and-sweep)
  (begin (write (get-root-set))
         (#f)))

(define (init-allocator)
  (set! heap-ptr 0))

; Sets a header of the specified type at the address
(define (set-header a type)
  (begin
    (heap-set! a type)
    (heap-set! (+ 1 a) #f)))

(define (gc:alloc-flat p)
  (begin
    (when (> (+ heap-ptr 3) (heap-size))
      (error 'gc:cons "out of memory"))
    (set-header heap-ptr 'prim)
    (heap-set! (+ header-size heap-ptr) p)
    (set! heap-ptr (+ (+ header-size 1) heap-ptr))
    (- heap-ptr (+ header-size 1))))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 4) (heap-size))
      (error 'gc:cons "out of memory"))
    (set-header heap-ptr 'cons)
    (heap-set! (+ header-size heap-ptr) f)
    (heap-set! (+ (+ 1 header-size) heap-ptr) r)
    (set! heap-ptr (+ (+ 2 header-size) heap-ptr))
    (- heap-ptr (+ 2 header-size))))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ header-size a))
      (error 'gc:first "expects address of cons")))

(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ (+ header-size 1) a))
      (error 'gc:rest "expects address of cons")))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ header-size a) f)
      (error 'gc:set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ (+ header-size 1) a) r)
      (error 'gc:set-rest! "expects address of cons")))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (if (gc:flat? a)
      (heap-ref (+ header-size a))
      (error 'gc:deref "expects address of prim")))



;mark and sweep
; add value after type to heap for each object that contains reference count
; when it's time to mark and sweep, get root set, 
;call mark
;for each root in root set
;    mark root
;    if root is procedure
;        recurse over procedure-roots of root
;    if root is cons
;        recurse over pointers
