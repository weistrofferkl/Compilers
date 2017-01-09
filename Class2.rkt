#lang racket

;Is the input neewom? If so, return "neewom". Else, return false.

(define (tokenize-neewom in)
  (let ([c (read-char in)])
    (if (eq? c #\n)
        (let ([c (read-char in)])
          (if (eq? c #\e)
               (let ([c (read-char in)])
                 (if (eq? c #\e)
                     (let ([c (read-char in)])
                       (if (eq? c #\w)
                           (let ([c (read-char in)])
                             (if (eq? c #\o)
                                 (let ([c (read-char in)])
                                   (if (eq? c #\m) 'neewom #f))
                                 #f))
                           #f))
                     #f))
               #f))
        #f))
  )
                                    