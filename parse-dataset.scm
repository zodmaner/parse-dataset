;;;; parse-dataset.scm
;;;;
;;;; A simple utility for parsing graph datasets from [0] and [1]
;;;; into a format that is more suitable for Pregel+ framework.
;;;;
;;;; [0] https://an.kaist.ac.kr/traces/WWW2010.html
;;;; [1] http://snap.stanford.edu/data/index.html
;;;;
;;;; Author: Smith Dhumbumroong <zodmaner@gmail.com>

(use extras
     srfi-1
     srfi-13
     srfi-69)

(define (comment? line)
  (or (string-index line #\#)
      (string-index line #\%)))

(define (add-vertex v adjv ht)
  (define adjvs (if (hash-table-exists? ht v)
                    (hash-table-ref ht v)
                    '()))
  (when adjv
    (set! adjvs (lset-adjoin = adjvs adjv)))
  (set! (hash-table-ref ht v) adjvs))

(define (parse-line line)
  (map string->number (string-tokenize line)))

(define (write-vertex v adjvs output-port)
  (fprintf output-port "~A~A~A ~A" v #\tab v (length adjvs))
  (for-each (lambda (adjv)
              (fprintf output-port " ~A ~A" adjv v))
            adjvs)
  (fprintf output-port "~%"))

(define (parse-dataset src dst)
  (define ht (make-hash-table))
  (call-with-input-file src
    (lambda (in)
      (let loop ((line (read-line in)) (lr 0))
        (cond ((eof-object? line)
               (printf "done!~%Start writing to output file... ")
               (flush-output))
              ((comment? line)
               (loop (read-line in) (add1 lr)))
              (else
               (let-optionals (parse-line line) ((v #f) (adjv #f))
                 (add-vertex v adjv ht)
                 (add-vertex adjv #f ht))
               (printf "~A# of lines read: ~A... " #\return (add1 lr))
               (loop (read-line in) (add1 lr)))))))
  (call-with-output-file dst
    (lambda (out)
      (hash-table-for-each ht
                           (lambda (v adjvs)
                             (write-vertex v adjvs out)))))
  (printf "done!~%"))

(define (main)
  (define cmd-args (the (list-of string) (command-line-arguments)))
  (if (not (= (length cmd-args) 2))
      (printf "Usage: ~A original-file new-file~%" (program-name))
      (let ((src (first cmd-args))
            (dst (second cmd-args)))
        (parse-dataset src dst))))

(main)
