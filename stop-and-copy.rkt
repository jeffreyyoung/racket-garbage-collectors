#lang plai/collector

; HEADER DEFINITION
; 0 - type
; 1 - marked
(define header-size 2)

(define heap-ptr 'uninitialized-heap-ptr)

(define isUsingFirstHeapFrame #t)

(define (update-heap-ptr)
  (begin
    (if isUsingFirstHeapFrame
        (set! heap-ptr (/ (heap-size) 2))
        (set! heap-ptr 0)))
  )

(define (get-end-of-heap)
  (if isUsingFirstHeapFrame
      (/ (heap-size) 2)
      (heap-size))
  )

(define (println in)
  (begin (displayln in)))


;use set-root to update root location on heap

(define (copy a)
  (case (heap-ref a)
    [(cons) 
     (if (false? (heap-ref (+ a 1))) ;if has not been copied
         (begin 
           (when (root? a)
             (set-root! (heap-ref a) heap-ptr));update root reference
           (heap-set! (+ a 1) #t) ; Mark frame as copied
           (gc:cons (heap-ref (+ a header-size)) (heap-ref (+ a header-size 1)));something is wrong here
           (copy (heap-ref (+ a (+ header-size 1))))) ; Mark rest of cons
         (void))]
    [(prim) 
     (if (false? (heap-ref (+ a 1)))
         (begin
           (when (root? a)
             (set-root! (heap-ref a) heap-ptr));update root reference
           (heap-set! (+ a 1) #t); Mark this frame
           (println "here")
           (gc:alloc-flat (heap-ref (+ a header-size))))
           ;(gc:alloc-flat "yayayaya"))
         (void))]
    [(free) (error 'mark "Free memory encounted during mark")]))

(define (stop-and-copy)
  (begin 
    (print (get-root-set))
    (update-heap-ptr) ;set the heap pointer to start of new heap section
    (set! isUsingFirstHeapFrame (not isUsingFirstHeapFrame));update bool
    (map ;recursively copy starting from root-set
     (lambda (x) 
       (begin
         (println (read-root x))
         (copy (read-root x)))) 
     (get-root-set))))

(define (init-allocator)
  (set! heap-ptr 0))

; Sets a header of the specified type at the address
(define (set-header a type)
  (begin
    (heap-set! a type)
    (heap-set! (+ 1 a) #f)))

(define (gc:alloc-flat p)
  (begin
    (when (> (+ heap-ptr 3) (get-end-of-heap))
      (stop-and-copy))
    (set-header heap-ptr 'prim)
    (heap-set! (+ header-size heap-ptr) p)
    (set! heap-ptr (+ (+ header-size 1) heap-ptr))
    (- heap-ptr (+ header-size 1))))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 4) (get-end-of-heap))
      (stop-and-copy))
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
