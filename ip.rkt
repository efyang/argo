#lang racket
;(provide ip?)
;(define (ip? ipaddr))
(define (allaresame compare lst)
  (cond [(empty? lst) #t]
        [(compare (first lst)) (allaresame compare (rest lst))]
        [else #f]))
(allaresame byte? (map string->number (string-split "192.168.1.0" ".")))