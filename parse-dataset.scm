;;;; parse-dataset.scm
;;;;
;;;; A simple utility for parsing the Twitter social graph dataset
;;;; from [0] into a format that is more suitable for Pregel+ framework.
;;;;
;;;; [0] https://an.kaist.ac.kr/traces/WWW2010.html
;;;;
;;;; Author: Smith Dhumbumroong <zodmaner@gmail.com>

(use extras)

(: parse-user-and-follower (string --> (list-of fixnum)))
(define (parse-user-and-follower line)
  (map string->number (string-split line)))

(: write-user-and-followers (fixnum (list-of fixnum) output-port -> *))
(define (write-user-and-followers user-id followers output-port)
  (fprintf output-port "~A~A~A " user-id #\tab (length followers))
  (let loop ((fids (the (list-of fixnum) followers)))
    (cond ((null? (cdr fids))
           (fprintf output-port "~A~%" (car fids)))
          (else
           (fprintf output-port "~A " (car fids))
           (loop (cdr fids))))))

(: parse-dataset (string string -> *))
(define (parse-dataset src dst)
  (call-with-input-file src
    (lambda (in)
      (call-with-output-file dst
        (lambda (out)
          (let loop ((line (the (or string eof) (read-line in)))
                     (current-uid (the fixnum 0))
                     (current-followers (the (or (list-of fixnum) null) '())))
            (if (eof-object? line)
                (write-user-and-followers current-uid current-followers out)
                (let* ((uid-and-fid (the (list-of fixnum) (parse-user-and-follower line)))
                       (uid (the fixnum (first uid-and-fid)))
                       (fid (the fixnum (second uid-and-fid))))
                  (cond ((and (not (= current-uid uid)) (not (null? current-followers)))
                         (write-user-and-followers current-uid current-followers out)
                         (loop line uid '()))
                        ((not (= current-uid uid))
                         (loop line uid '()))
                        (else
                         (loop (read-line in) current-uid (cons fid current-followers))))))))))))

(define (main)
  (let ((cmd-args (the (list-of string) (command-line-arguments))))
    (if (not (= (length cmd-args) 2))
        (printf "Usage: ~A original-file new-file~%" (program-name))
        (let ((src (the string (first cmd-args)))
              (dst (the string (second cmd-args))))
          (parse-dataset src dst)))))

(main)
