#lang plai/collector

; HEADER DEFINITION
; 0 - type
; 1 - marked
(define header-size 2)

; FREE FRAME DEFINITION
; 0 - 'free
; 1 - next pointer
; 2 - number of additional free frames (excl.)

(define heap-ptr 'uninitialized-heap-ptr)

(heap-size)

(define (mark-and-sweep! roots)
  (begin 
    (map 
     (lambda (root) 
       (begin
         (displayln (read-root root))
         (mark! (read-root root)))) 
     roots)
    (sweep! 3)))

; (mark! a) -> void?
;   a : integer?
; Recursively marks the frame at the given address
(define (mark! a)
  (displayln "Marking address")
  (displayln a)
  (case (heap-ref a)
    [(cons) 
     (if (false? (heap-ref (+ a 1)))
         (begin 
           (heap-set! (+ a 1) #t) ; Mark this frame
           (mark! (heap-ref (+ a header-size))) ; Mark first of cons
           (mark! (heap-ref (+ a (+ header-size 1))))) ; Mark rest of cons
         (void))]
    [(prim) 
     (if (heap-ref (+ a 1))
         (void)
         (begin
           (heap-set! (+ a 1) #t)
           (when (procedure? (heap-ref (+ a 2)))
             (map
              (lambda (root) 
                (begin 
                  (displayln "Marking procedure root in mark!")
                  (mark! (read-root root))))
              (procedure-roots (heap-ref (+ a 2)))))))]
    [(free) (error 'mark "Free memory location encounted during mark")]))

; (sweep! a) -> void?
;   a : integer?
; Sweeps memory, freeing all unmarked data
(define (sweep! a)
  (if (not (< a heap-ptr))
      (void) ; Return upon reaching end of heap data 
      (case (heap-ref a)
        [(free)
         (sweep! (+ a 3 (heap-ref (+ a 2))))]
        [(cons)
         (begin
           (if (not (marked? a))
               (free-heap-memory! a 1)
               (heap-set! (+ a 1) #f))
           (sweep! (+ a (+ header-size 2))))]
        [(prim)
         (begin
           (if (not (marked? a))
             (free-heap-memory! a 0)
             (heap-set! (+ a 1) #f))
           (sweep! (+ a (+ header-size 1))))])))

(define (marked? a)
  (heap-ref (+ a 1)))

(define (init-allocator)
  (begin
    ; Set up free space list root node
    (heap-set! 0 'free)
    (heap-set! 1 3)
    (heap-set! 2 0)
    ; Init heap pointer
    (set! heap-ptr 3)))

; (get-free-frames -> n roots) -> address?
;   n : number?
;   roots : (listof root?)
; Returns the first available address in the 
;  free memory list with n contiguous frames.
;  Given roots list is used in the event of a
;  mark and sweep algorithm.
(define (get-free-frames n roots)
  (with-handlers([(lambda (v) (eq? v "Out of memory"))
                  (lambda (v) (begin
                                (displayln "Caught out of memory exception")
                                (mark-and-sweep! roots)
                                (get-free-address 0 n)))])
    (get-free-address 0 n)))

; (get-free-frames -> n) -> address?
;   a : address? 
;   n : number?
; Recurses the free memory list and returns the first address
;  with n contiguous frames
(define (get-free-address a n)
  (begin
    (displayln "get-free-address")
    (displayln a)
    (displayln n)
    (displayln (heap-ref (+ a 1)))
    (displayln heap-ptr)
    (let [(next-a (heap-ref (+ a 1)))]
      (cond 
        ; If we reached the heap pointer, that's the new address
        [(eq? heap-ptr next-a) 
         (if (>= (+ heap-ptr n) (heap-size))
             (raise "Out of memory")
             (let [(ret-ptr heap-ptr)]
               (begin
                 (displayln "Returning head pointer")
                 (displayln ret-ptr)
                 (set! heap-ptr (+ heap-ptr n)) ; Update heap pointer
                 (heap-set! (+ a 1) heap-ptr) ; Update the previous free space's pointer to the new heap pointer
                 ret-ptr)))]
        ; Check for wrong header type
        [(not (eq? (heap-ref next-a) 'free)) 
         (error 'get-free-address "Expected free memory address")]
        ; Check if this memory segment is the right size
        [(eq? (+ 3 (heap-ref (+ next-a 2))) n) 
         (begin
           (heap-set! (+ a 1) (heap-ref (+ next-a 1))) ; Update the previous free space's next pointer
           next-a)]
        ; Check next in list
        [else (get-free-address next-a n)]))))

; (free-heap-memory! a n) -> void?
;   a : integer?
;   n : integer?
; Frees the specified address in memory for n frames of the heap
(define (free-heap-memory! a n)
  (begin
    ; Update header definition
    (heap-set! a 'free)
    (heap-set! (+ a 1) (heap-ref 1)) ; Point at whatever root node is pointing at
    (heap-set! (+ a 2) n)
    ; Insert memory at beginning of free memory list
    (heap-set! 1 a)
    (void)))

; Sets a header of the specified type at the address
(define (set-header a type)
  (begin
    (heap-set! a type)
    (heap-set! (+ 1 a) #f)))

(define (gc:alloc-flat p)
  (begin
    (displayln "Allocating!")
    (displayln p)
    (define roots (if (procedure? p)
                      (append (get-root-set) (procedure-roots p))
                      (get-root-set)))
    (define a (get-free-frames 3 roots))
    (set-header a 'prim)
    (heap-set! (+ a header-size) p)
    a))

(define (gc:cons f r)
  (begin
    (define a (get-free-frames 4 (get-root-set f r)))
    (set-header a 'cons)
    (heap-set! (+ a header-size) f)
    (heap-set! (+ a (+ 1 header-size)) r)
    a))

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
      (begin
        (displayln "Accessing flat")
        (displayln a)
        (heap-ref (+ header-size a)))
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
