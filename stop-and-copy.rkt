#lang plai/collector

; HEADER DEFINITION
; 0 - type
; 1 - forward address
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

(define (debug-print a b)
  (begin
    (print "copying ")
    (print a)
    (print "to ")
    (println b)
  ))

(define (debug-root-ref a b)
  (begin
    (print "root-ref was ")
    (print a)
    (print " but now is ")
    (println b)
  ))
;use set-root to update root location on heap

(define (copy a)
  (case (heap-ref a)
    [(cons) 
     (if (false? (heap-ref (+ a 1))) ;if has not been copied
         (begin 
           (when (root? (heap-ref a))
             (set-root! (heap-ref a) heap-ptr));update root reference
           (heap-set! (+ a 1) heap-ptr) ; Mark forward address
           (gc:cons (heap-ref (+ a header-size)) (+ heap-ptr 4));(heap-ref (+ a header-size 1)));something is wrong here
           (debug-print a heap-ptr)
           (copy (heap-ref (+ a (+ header-size 1))))) ; Mark rest of cons
         (void))]
    [(prim) 
     (if (false? (heap-ref (+ a 1))) ;if object has not been copied
         (begin
           (when (root? (heap-ref a))
             (begin
               (set-root! (heap-ref a) heap-ptr));update root reference
               (debug-root-ref a heap-ptr)
             )
           (heap-set! (+ a 1) heap-ptr); mark forward address
           (debug-print a heap-ptr)
           (gc:alloc-flat (heap-ref (+ a header-size)))
           (when (procedure? (heap-ref (+ a header-size)))
             (begin
               (println "FOUND A PROCEDURE!!!")
               (stop-and-copy-roots (procedure-roots (heap-ref (+ a header-size)))))))
         (void))]
    ))

(define (stop-and-copy-roots rts)
  (begin 
    (print rts)
    (map ;recursively copy starting from root-set
     (lambda (x) 
       (begin
         (print "reading root ")
         (print x)
         (println (read-root x))
         (copy (read-root x)))) 
     rts)))
  

(define (stop-and-copy)
  (begin 
    (print (get-root-set))
    (update-heap-ptr) ;set the heap pointer to start of new heap section
    (set! isUsingFirstHeapFrame (not isUsingFirstHeapFrame));update bool
    (stop-and-copy-roots (get-root-set))))

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
