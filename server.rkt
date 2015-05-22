#lang racket

;server 
(require 2htdp/universe)
(require "surround.rkt")

;server model is (list (list unknownworlds) (list unjoinedmodels) (list runninggames))
;unjoinedmodel is (list owner board boardsize)
;runninggame is (list p1 p2 board boardsize endgame?)


;appends new world to unknown list
(define (newworld curstate joiner)
  (make-bundle (list (append (first curstate) (list joiner)) 
                     (second curstate)
                     (third curstate))
               empty
               empty))

;makes a new game room (not completely full)
(define (newunjoinedmodel owner size)
  (list owner (baseboard size) size))

;unistate -> unistate
;removes the sender from the unknown list and creates a new game
(define (addnewgame curstate sender boardsize)
  (make-bundle (list (remove sender (first curstate))
                     (append (second curstate) (list (newunjoinedmodel sender boardsize)))
                     (third curstate))
               empty 
               empty))

;makes a new running game
(define (newrunninggame p1 p2 board boardsize)
  (list p1 p2 board boardsize #f))

;unistate -> unistate
;removes the first unjoinedmodel from the queue, makes a new running game
(define (addrunninggame curstate joiner)
  (local [(define gametojoin (first (second curstate)))
          (define p1 (first gametojoin))
          (define p2 joiner)
          (define newboard (second gametojoin))
          (define newboardsize (third gametojoin))]
    (make-bundle (list (first curstate)
                       (remove gametojoin (second curstate))
                       (append (third curstate) (list (newrunninggame p1 p2 newboard newboardsize))))
                 (list (make-mail p1 (list "newgame" newboardsize 1))
                       (make-mail p2 (list "newgame" newboardsize 2)))
                 empty)))

;gets the game that the sender is a part of
(define (sendersgame sender notcheckedgames)
  (cond [(empty? notcheckedgames) empty]
        [(local [(define curgame (first notcheckedgames))
                 (define curcheck1 (first curgame))
                 (define curcheck2 (second curgame))]
           (or (equal? curcheck1 sender) (equal? curcheck2 sender)) 
           curgame)]
        [else (sendersgame sender (rest notcheckedgames))]))

;check if part of list
(define (elem item lst)
  (cond [(empty? lst) #f]
        [(equal? item (first lst)) #t]
        [else (elem item (rest lst))]))
;gets pos of item in lst
(define (getpos item lst [curpos 0])
  (cond [(empty? lst) curpos]
        [(equal? item (first lst)) curpos]
        [else (getpos item (rest lst) (+ curpos 1))]))
;gets everything before pos
(define (lhead lst pos)
  (reverse (list-tail (reverse lst) (+ (- (length lst) pos) 1))))
;gets everything after pos
(define (ltail lst pos)
  (list-tail lst pos))
;replace item at lref index with newitem
(define (replace lst newitem lref)
  (append (lhead lst (+ lref 1)) (list newitem) (ltail lst (+ lref 1))))
;replace item in lst with newitem
(define (replacenoref lst item newitem)
  (cond [(elem item lst) (local [(define itempos (getpos item lst))]
                           (replace lst newitem itempos))]
        [else lst]))

;detects and increments each piece accordingly (countpieces helper)
(define (pcount piece curvals)
  (cond [(= 0 piece) (list (+ 1 (first curvals)) (second curvals) (third curvals))]
        [(= 1 piece) (list (first curvals) (+ 1 (second curvals)) (third curvals))]
        [(= 2 piece) (list (first curvals) (second curvals) (+ 1 (third curvals)))]))

;counts number of black, white, open pieces on the board currently
(define (countpieces boardmap)
  (foldl pcount (list 0 0 0) (flatten boardmap)))

;sets the game as ended
(define (setend game)
  (list (first game)
        (second game)
        (third game)
        (fourth game)
        #t))

;unistate -> unistate
;move is (list movetype x y)
(define (handlemove curstate sender move)
  ;move
  (local [(define curgame (sendersgame sender (third curstate)))
                 (define p1w (first curgame))
                 (define p2w (second curgame))
                 (define curboard (third curgame))
                 (define curboardsize (fourth curgame))
                 (define curplayer (cond [(equal? p1w sender) 1]
                                         [else 2]))]
    (cond [(= (first move) 1)
           (local [(define surroundedboard (surroundupdate curplayer (rest move) curboard curboardsize))
                   (define updatedgame (list p1w p2w surroundedboard curboardsize #f))
                   (define updmsg (list "updategame" surroundedboard move curplayer))
                   (define pnums (countpieces surroundedboard))]
             ;board is full, end game
             (cond [(= 0 (first pnums))
                   (local [(define endtype (getwin pnums))
                           (define p1u (iworld-name p1w))
                           (define p2u (iworld-name p2w))
                           (define endmsg (mkendgamemsg endtype p1u p2u))]
                     (make-bundle (list (first curstate) 
                                (second curstate)
                                (replacenoref (third curstate) curgame 
                                              (setend curgame)))
                                (list (make-mail p1w updmsg)
                                      (make-mail p2w updmsg)
                                      (make-mail p1w (list "endgame" (first endmsg) (second endmsg)))
                                      (make-mail p2w (list "endgame" (first endmsg) (second endmsg))))
                                empty
                                ))]
                   [else (make-bundle (list (first curstate) 
                                (second curstate)
                                (replacenoref (third curstate) curgame updatedgame))
                                (list (make-mail p1w updmsg)
                                      (make-mail p2w updmsg))
                                empty)]))]
          ;pass
          [else (local [(define updmsg (list "updategame" curboard move curplayer))]
                  (make-bundle curstate
                               (list (make-mail p1w updmsg)
                                     (make-mail p2w updmsg))
                               empty))])))

;message handler
(define (handlemessage curstate sender msg)
  (local [(define msgtype (first msg))
          (define msginfo (rest msg))]
    (cond 
      ;new game
      [(string=? msgtype "newgame") (addnewgame curstate sender (+ 1 (first msginfo)))]
      ;join a game
      [(string=? msgtype "joingame") (cond [(> (length (second curstate)) 0) (addrunninggame curstate sender)]
                                           ;default to new game creation
                                           [else (addnewgame curstate sender (+ 1 (first msginfo)))])]
      ;new move
      [(string=? msgtype "newmove") (handlemove curstate sender (first msginfo))]
      
      ;forfeit message 
      ;WORK ON THIS
      [(string=? msgtype "forfeit") (endremove curstate sender #t)]
      
      ;endgame message
      [(string=? msgtype "endgame") (local [(define sendergame (sendersgame sender (third curstate)))
                                            (define pnums (countpieces (third sendergame)))
                                            (define p1u (iworld-name (first sendergame)))
                                            (define p2u (iworld-name (second sendergame)))
                                            (define endtype (getwin pnums))
                                            (define endmsg (mkendgamemsg endtype p1u p2u))]
                                      (cond [(not (fifth sendergame)) (make-bundle (list (first curstate) 
                                                                                         (second curstate) 
                                                                                         (replacenoref (third curstate) sendergame 
                                                                                                       (setend sendergame)))
                                                                                   (list (make-mail (first sendergame) (list "endgame" (first endmsg) (second endmsg)))
                                                                                         (make-mail (second sendergame) (list "endgame" (first endmsg) (second endmsg))))
                                                                                   empty
                                                                                   )]
                                            [else curstate]))]
      [else curstate])))

(define (getop t)
  (cond [(string-ci=? t "white") "Black"]
        [else "White"]))
;removes and ends the disconnector's game
(define (endremove curstate disconnector [legitforfeit #f])
  (local [(define curgame (sendersgame disconnector (third curstate)))
          (define remainworld (cond [(equal? disconnector (first curgame)) (list "White" (second curgame))]
                                    [else (list "Black" (first curgame))]))
          (define fuser (iworld-name disconnector))
          (define ruser (iworld-name (second remainworld)))
          (define fusert (getop (first remainworld)))
          (define rusert (first remainworld))
          (define endmsg (list "endgame" 
                               (string-append fuser " (" fusert ") has forfeited,")
                               (string-append ruser " (" rusert ") wins.")))]
    (make-bundle (list (first curstate)
                       (second curstate)
                       (replacenoref (third curstate) curgame 
                                     (setend curgame)))
                 (cond [legitforfeit (list (make-mail (first curgame) endmsg)
                                           (make-mail (second curgame) endmsg))]
                       [else (list (make-mail (second remainworld) endmsg))])
                 empty
                 )))

(universe (list empty empty empty)
          (on-new newworld)
          (on-msg handlemessage)
          (on-disconnect endremove)
          )