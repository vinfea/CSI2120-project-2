#lang racket

;; Generate the file paths of all files within a given directory
;; into a list of strings -------------------------------------------------

(require (lib "13.ss" "srfi"))

;; Main function
(define (get-dir-paths dir)
  (let* ((path-list (directory-list dir))
        (string-list (pathlist->stringlist path-list))
        (full-list (concat string-list dir))) ; assignment
        (filter-txt full-list) ; exec
  )  
)

;; Helper functions
(define (pathlist->stringlist L)
  (if (null? L)
      '()
      (cons (path->string (car L)) (pathlist->stringlist (cdr L)))
  )    
)

(define (concat L str)
  (if (null? L)
      '()
      (cons (string-append str "/" (car L)) (concat (cdr L) str))
  )
)

(define (filter-txt L)
  (if (null? L)
      '()
      (if (string-contains (car L) "txt")
          (cons (car L) (filter-txt (cdr L)))
          (filter-txt (cdr L))
      )
  )
)

;; Read the contents of a txt file into a list
;; given the filename ----------------------------------------------------

(define (read-file filename)
  (let ((p (open-input-file filename)))
    (let f ((c (read p))) 
      (if (eof-object? c) 
          (begin
            (close-input-port p)
            '())
          (cons c (f (read p))))
    )
  )
)

;; Read the contents of a directory into a list
;; given the directory name ----------------------------------------------

(define (read-dir dir)
  (let f ((paths (get-dir-paths dir)))
    (if (null? paths)
        '()
        (cons (read-file (car paths)) (f (cdr paths)))
    )
  )
)

;; Histogram methods ----------------------------------------------------

(define (sum L)
  (if (null? L)
      0
      (+ (car L) (sum (cdr L)))
  )
)

(define (normalize H)
  (let f ((hist H)
          (sum (sum H)))
       (if (null? hist)
           '()
           (cons (exact->inexact (/ (car hist) sum)) (f (cdr hist) sum))
       )
  )
)

(define (histogram-intersection H1 H2)
  (if (not (equal? (length H1) (length H2)))
      'invalid-input
      (let f ((h1 H1) (h2 H2))
        (if (null? h1)
            0
            (+ (min (car h1) (car h2)) (f (cdr h1) (cdr h2)))
        )
      )          
  )
)

;; Main function for testing purposes (for now) ------------------------------

(define (main)
  (let* ((f1 "queryHistograms/Sample0.txt")
        (f2 "queryHistograms/Sample1.txt")
        (h1 (cdr (read-file f1)))
        (h2 (cdr (read-file f2)))) ; assignment
    (histogram-intersection (normalize h1) (normalize h2)) ; exec
  )
)

