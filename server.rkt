#lang racket

;server 
(require 2htdp/universe)
(require "surround.rkt")

;server model is (list (list unknownworlds) (list unjoinedmodels) (list runninggames))
;unjoinedmodel is (list owner board boardsize)
;runninggame is (list p1 p2 board boardsize)


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
  (list p1 p2 board boardsize))

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
                       (append (third curstate) (newrunninggame p1 p2 newboard newboardsize)))
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

;unistate -> unistate
(define (handlemove curstate sender move)
  (local [(define curgame (sendersgame sender (third curstate)))
          (define p1w (first curgame))
          (define p2w (second curgame))
          (define curboard (third curgame))
          (define curboardsize (fourth curgame))
          (define curplayer (first move))
          (define surroundedboard (surroundupdate curplayer (second move) curboard curboardsize))
          (define updatedgame (p1w p2w surroundedboard curboardsize))
          (define updmsg (list "updategame" surroundedboard curplayer))]
    (make-bundle (list (first curstate) 
                       (second curstate)
                       (replacenoref (third curstate) curgame updatedgame))
                 (list (make-mail p1w updmsg)
                       (make-mail p2w updmsg))
                 empty)))

;message handler
(define (handlemessage curstate sender msg)
  (local [(define msgtype (first msg))
          (define msginfo (rest msg))]
    (cond 
      ;new game
      [(string=? msgtype "newgame") (addnewgame curstate sender (first msginfo))]
      ;join a game
      [(string=? msgtype "joingame") (cond [(> (length (second curstate)) 0) (addrunninggame curstate sender)]
                                           ;default to new game creation
                                           [else (addnewgame curstate sender (first msginfo))])]
      ;new move
      [(string=? msgtype "newmove") (handlemove curstate sender (first msginfo))]
      [else (make-bundle curstate empty empty)])))

(universe (list empty empty empty)
          (on-new newworld)
          (on-msg handlemessage))