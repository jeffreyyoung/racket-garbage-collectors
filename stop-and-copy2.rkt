#lang plai/collector

; HEADER DEFINITION
; 0 - type
; 1 - forward address
(define header-size 2)

(define heap-ptr 'uninitialized-heap-ptr)

(define isUsingFirstHeapSpaceAsToSpace #t)

(define debug #t)

(define (set-heap-ptr-to-to-space)
  (begin
    (print "isUsingFirstHeapSpaceAsToSpace: ")
    (println isUsingFirstHeapSpaceAsToSpace)
    (if isUsingFirstHeapSpaceAsToSpace
        (set! heap-ptr (/ (heap-size) 2))
        (set! heap-ptr 0))
    (print "HEAP PTR ")
    (println heap-ptr);update bool)
    
  ))

(define (reset)
  (if isUsingFirstHeapSpaceAsToSpace
      (reset-from-space 0 (/ (heap-size) 2))
      (reset-from-space (/ (heap-size) 2) (heap-size))))

(define (reset-from-space start finish)
  (if (start < finish)
      (begin
        (heap-set! #f start)
        (reset-from-space (+ start 1) finish))
      (println "finished resetting")
  ))

(define (get-end-of-heap)
  (if isUsingFirstHeapSpaceAsToSpace
      (/ (heap-size) 2)
      (heap-size))
  )

(define (println in)
  (begin (displayln in)))

(define (print-debug a b)
  (begin
    (print "copying ")
    (print a)
    (print "to ")
    (println b)
  ))

(define (print-already a b)
  (begin
    (print "already copied ")
    (print a)
    (print " to ")
    (println b)
  ))

(define (copy a)
  (case (heap-ref a)
    [(cons) 
     (if (false? (gc:forward a)) ;if object has not yet been moved to the to-space
         (begin 
           (print-debug a heap-ptr)
           (when (root? (heap-ref a))
             (set-root! a heap-ptr));update root reference to refer to the new version in to-space
           (heap-set! (+ a 1) heap-ptr) ; set forward address
           (gc:cons (copy (gc:first a)) (copy (gc:rest a))))
         (begin
           (print-already a (gc:forward a))
           (gc:forward a)))] ;if object has already been copied, return forward address
    [(prim) 
     (if (false? (gc:forward a)) ;if object has not been copied
         (begin
           (print-debug a heap-ptr)
           (heap-set! (+ a 1) heap-ptr); set forward address
           (when (procedure? (heap-ref (+ a header-size)))
              (begin (stop-and-copy-roots (procedure-roots (gc:deref a))) (print "procuedre roots  ")(print a)(print (procedure-roots (gc:deref a)))))
           (when (root? (heap-ref a))
             (begin
               (set-root! a heap-ptr));set root reference
               )
           (gc:alloc-flat (heap-ref (+ a header-size))))
         (begin
           (print-already a (gc:forward a))
           (gc:forward a)))] ;if object has already been copied, return forward address
    ))

(define (stop-and-copy-roots rts)
  (begin 
    ;(print rts)
    (map ;recursively copy starting from root-set
     (lambda (x) 
       (begin
         ;(print x)
         ;(println (read-root x))
         
         (copy (read-root x)))) 
     rts)))
  
(define (stop-and-copy)
  (begin 
    (set-heap-ptr-to-to-space) ;set the heap pointer to start of new heap section
    (set! isUsingFirstHeapSpaceAsToSpace (not isUsingFirstHeapSpaceAsToSpace))
    (stop-and-copy-roots (get-root-set))
    ;(reset)
    (println "done copying")))
    ;(scan)));recursively copy all roots from root set


(define (init-allocator)
  (set! heap-ptr 0))

; Sets a header of the specified type at the address
(define (set-header a type)
  (begin
    (heap-set! a type)
    (heap-set! (+ 1 a) #f)));set forward to false

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
  (begin (println a)
  (if (gc:cons? a)
      (heap-ref (+ (+ header-size 1) a))
      (error 'gc:rest "expects address of cons"))))

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
  (begin ;(printf "deref a ~a~n" a)
  (if (gc:flat? a)
      (heap-ref (+ header-size a))
      (error 'gc:deref "expects address of prim"))))

(define (gc:forward a)
  (heap-ref (+ a 1))
  )