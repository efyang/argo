#lang racket
(provide ipcheck)

(define (elem item lst)
  (cond [(empty? lst) #f]
        [(equal? item (first lst)) #t]
        [else (elem item (rest lst))]))


(define (remspaces str)
  (list->string (remspacelst (string->list str))))
(define (remspacelst strlst)
  (cond [(elem #\space strlst) (remspacelst (remove #\space strlst))]
        [else strlst]))

(define (allaresame compare lst)
  (cond [(empty? lst) #t]
        [(compare (first lst)) (allaresame compare (rest lst))]
        [else #f]))

(define (ip? ipaddr)
  (cond [(elem #\. (string->list ipaddr)) (cond [(not (= 3 (count (lambda (x) (equal? #\. x)) (string->list ipaddr)))) #f] 
                                                [else (local [(define numlst (map string->number (string-split ipaddr ".")))]
                                                        (cond [(= (length numlst) 4) (allaresame byte? numlst)]
                                                              [else #f]))])]
        [else #f]))

(define (ipcheck ipaddr)
  (local [(define cleaned (remspaces ipaddr))]
    (cond [(string-ci=? "local" cleaned) "127.0.0.1"]
          [(string-ci=? "localhost" cleaned) "127.0.0.1"]
          [(ip? cleaned) cleaned]
          [else "127.0.0.1"])))