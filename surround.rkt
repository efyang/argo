#lang racket
(define (mklist item num)
  (build-list num (lambda (x) item)))

(define (padlst 2dlst item w h)
  (local [(define vadd (list (mklist item (+ w 2))))
          (define hadd (list item))]
    (append vadd (map (lambda (x) (append hadd x hadd)) 
                      2dlst)
            vadd)))

(define (2dlstpad 2dlst item)
  (padlst 2dlst item (length (first 2dlst)) (length 2dlst)))

(define (getopposite ptype)
  (cond [(= ptype 1) 2]
        [(= ptype 2) 1]
        [else ptype]))

(define (surrounded? xc yc padboard doneboard)
  (local [(define padxc (+ x 1))
          (define padyc (+ y 1))
          (define ptype (list-ref (list-ref padboard padyc) padxc))
          (define optype (getopposite ptype))
          (define p1 (list-ref (list-ref (+ padyc 1)) (+ padxc 1)))
          (define p2 (list-ref (list-ref yc) (+ padxc 1)))
          (define p3 (list-ref (list-ref (+ padyc 1)) xc))
          (define p4 (list-ref (list-ref yc) xc))]
    (cond [(= optype p1 p2 p3 p4) #t]
          [else])))