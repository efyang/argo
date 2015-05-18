#lang racket
(provide surroundupdate baseboard)

(define (mklist item num)
  (build-list num (lambda (x) item)))

(define (baseboard size)
  (mklist (mklist 0 size) size))

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

;gets everything before pos
(define (lhead lst pos)
  (reverse (list-tail (reverse lst) (+ (- (length lst) pos) 1))))
;gets everything after pos
(define (ltail lst pos)
  (list-tail lst pos))
;replace item at lref index with newitem
(define (replace lst newitem lref)
  (append (lhead lst (+ lref 1)) (list newitem) (ltail lst (+ lref 1))))

;check if opponent pieces are surrounded
;CHANGE THIS
;board -> board
(define (surroundupdate playernum move board blocknum)
  (local [(define addedboard (replace board (replace (list-ref board (second move)) playernum (first move)) (second move)))
          (define padboard (padlst board 3 blocknum blocknum))
          (define doneboard (baseboard blocknum))]
    addedboard))

;(define (ptsurrounded? xc yc padboard doneboard)
;  (local [(define padxc (+ x 1))
;          (define padyc (+ y 1))
;          (define ptype (list-ref (list-ref padboard padyc) padxc))
;          (define optype (getopposite ptype))
;          (define p1 (list-ref (list-ref (+ padyc 1)) (+ padxc 1)))
;          (define p2 (list-ref (list-ref yc) (+ padxc 1)))
;          (define p3 (list-ref (list-ref (+ padyc 1)) xc))
;          (define p4 (list-ref (list-ref yc) xc))]
;    (cond [(= optype p1 p2 p3 p4) #t]
;          []
;          [else])))