#lang racket
(provide ip?)

(define (elem item lst)
  (cond [(empty? lst) #f]
        [(equal? item (first lst)) #t]
        [else (elem item (rest lst))]))

(define (allaresame compare lst)
  (cond [(empty? lst) #t]
        [(compare (first lst)) (allaresame compare (rest lst))]
        [else #f]))

(define (ip? ipaddr)
  (cond [(elem #\. (string->list ipaddr)) (local [(define numlst (map string->number (string-split ipaddr ".")))]
                                            (cond [(string-length numlst 4) (allaresame byte? numlst)]
                                                  [else #f]))]
        [else #f]))