#lang racket

;; Generate the relative path of all files within a given directory
;; into a list of strings -------------------------------------------------

(require (lib "13.ss" "srfi"))

;; --- Main function ---

; Input: name of a directory
; Output: list of strings containing the relative path to each file in the directory

(define (get-dir-paths dir)
  (let* ((path-list (directory-list dir))
        (string-list (pathlist->stringlist path-list))
        (full-list (concat string-list dir))) ; assignment
        (filter-txt full-list) ; exec
  )  
)

;; --- Helper functions ---

; Input: list of path objects
; Output: input list with each path converted to a string 

(define (pathlist->stringlist L)
  (if (null? L)
      '()
      (cons (path->string (car L)) (pathlist->stringlist (cdr L)))
  )    
)

; Input: list of strings, string
; Output: concatenates the given string to each string within the list

(define (concat L str)
  (if (null? L)
      '()
      (cons (string-append str "/" (car L)) (concat (cdr L) str))
  )
)

; Input: list of strings
; Output: input list filtered to only strings containing “txt”

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

; Input: filename as a string
; Output: contents of the file as a list

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

;; Histogram methods ----------------------------------------------------

;; --- Helper function ---

; Input: list of numbers
; Output: sum of the list

(define (sum L)
  (if (null? L)
      0
      (+ (car L) (sum (cdr L)))
  )
)

;; --- Histogram methods ---

; Input: list of numbers
; Output: input list with each number divided by the sum of the list

(define (normalize H)
  (let f ((hist H)
          (sum (sum H)))
       (if (null? hist)
           '()
           (cons (exact->inexact (/ (car hist) sum)) (f (cdr hist) sum))
       )
  )
)

; Input: list1 of numbers, list2 of numbers
; Restriction: list1 and list2 must be the same length
; Output: list where each element is min(list1, list2) at each index

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

;; Helper functions for main similarity search function---------------------

; Compares dataset images with query image, returns list with pairs (intersection value, filename)
(define (total query dir)
    (if (null? dir)
       '()
     (cons (cons
            (histogram-intersection query (normalize (cdr (read-file (car dir))))) (car dir))
           (total query (cdr dir))
      )
    )
)

; Sorts pair list from greatest to least
(define (sort-list ls)
  (sort ls
        (lambda (a b)
          (> (car a) (car b)))))

; Show top n values in list
(define (top ls n)
  (if (zero? n)
      '()
      (cons (car ls) (top (cdr ls) (- n 1)))
  )
)

; Similarity Search function ----------------------------------------------

(define (similaritySearch queryHistogramFilename imageDatasetDirectory)
  (let ((query (normalize (cdr (read-file queryHistogramFilename))))
        (dir (get-dir-paths imageDatasetDirectory)))
    (display (top (sort-list (total query dir)) 5))   
   )
)

(similaritySearch "queryHistograms/q00.txt" "imageDataset")




