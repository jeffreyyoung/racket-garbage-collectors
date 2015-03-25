#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)

(heap-size)

(define (mark-and-sweep)
  (begin (write (get-root-set))
         (#f)))

(define (init-allocator)
  (set! heap-ptr 0))

(define (gc:alloc-flat p)
  (begin
    (when (> (+ heap-ptr 2) (heap-size))
      (mark-and-sweep))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 3) (heap-size))
      (error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ 1 a))
      (error 'gc:first "expects address of cons")))

(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ 2 a))
      (error 'gc:rest "expects address of cons")))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'gc:set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ 2 a) r)
      (error 'gc:set-rest! "expects address of cons")))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (if (gc:flat? a)
      (heap-ref (+ 1 a))
      (error 'gc:deref "expects address of prim")))
