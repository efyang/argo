#lang racket
(provide surroundupdate baseboard getwin mkendgamemsg)

(define (elem item lst)
  (cond [(empty? lst) #f]
	[(equal? item (first lst)) #t]
	[else (elem item (rest lst))]))

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

;1 is player 1
;2 is player 2
;3 is tie
;(map these to actual usernames later on)
(define (getwin piecenums)
  (local [(define p1n (second piecenums))
          (define p2n (third piecenums))]
    (cond [(> p1n p2n) 1]
          [(< p1n p2n) 2]
          [else 3])))

;creates the message based on message type and usernames
(define (mkendgamemsg msgtype p1u p2u)
  (cond [(= msgtype 1) (list (string-append p1u " (Black) beat ") (string-append p2u "(White)."))]
        [(= msgtype 2) (list (string-append p2u " (White) beat ") (string-append p1u "(Black)."))]
        [else (list (string-append p1u " (Black) tied with ") (string-append p2u "(White)."))]))

;check if opponent pieces are surrounded
;CHANGE THIS
;board -> board

(define (boardref x y board)
  (list-ref (list-ref board y) x))

(define (replace2d lst x y item)
  (replace lst (replace (list-ref lst y) item x) y))
(define (surroundupdate playernum move board blocknum)
  (local [(define addedboard (replace board (replace (list-ref board (second move)) playernum (first move)) (second move)))
          (define padboard (padlst addedboard 3 blocknum blocknum))
          (define doneboard (baseboard blocknum))]
    addedboard))
;fold through list; when checked, add to donelist; accumulator is (list curboard donelist)
;
(define (surround playerNum move board blockNum)
  (local [(define addedBoard (replace board (replace (list-ref board (second move)) playerNum (first move)) (second move)))
	  (define padBoard (padlst addedBoard 3 blockNum blockNum))
	  (define doneBoard (baseboard blockNum))]
    (rsurround playerNum 0 0 padBoard doneBoard)))

(define (rsurround playerNum xc yc board doneBoard )
  (local [(define uxc (+ xc 1))
	  (define uyc (+ yc 1))
	  (define bVal (boardref uxc uyc board))
	  ]))
;recursively go through list, updating as you go; if maybe and surround then is surround; update the ones that are connected too
;
;if pt is opponent <- checked in main func already
;and
;is not on donelist already -> if on donelist and is opponent then just get value from donelist
;donelist vals can be:
;0 - not checked
;1 - checked, not surrounded
;2 - checked, surrounded
;3 - checked, might be surrounded (dependent)
;foldl point; point should return 

#|(define (ptsurrounded? pnum xc yc padboard doneboard)|#
  #|(local [(define padxc (+ xc 1))|#
	  #|(define padyc (+ yc 1))|#
	  #|(define ptype (list-ref (list-ref padboard padyc) padxc))|#
	  #|(define optype (getopposite ptype))|#
	  #|(define p1 (list-ref (list-ref (+ padyc 1)) (+ padxc 1)))|#
	  #|(define p2 (list-ref (list-ref yc) (+ padxc 1)))|#
	  #|(define p3 (list-ref (list-ref (+ padyc 1)) xc))|#
	  #|(define p4 (list-ref (list-ref yc) xc))]|#
    #|(cond [(= optype p1 p2 p3 p4) #t]|#
	  #|[(not (= 0 (boardref xc yc doneboard))) #f]|#
	  #|[else #t])))|#
