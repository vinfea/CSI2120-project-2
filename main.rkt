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

;cdr built in
(define (get-cdr-from-file filename)
  (letrec ((read-file
            (lambda (filename)
              (let ((p (open-input-file filename)))
                (let f ((c (read p))) 
                  (if (eof-object? c) 
                      (begin
                        (close-input-port p)
                        '())
                      (cons c (f (read p))))
                )
              ))))
    (cdr (read-file filename))))


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

