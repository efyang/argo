#lang racket

(require picturing-programs)
(provide (all-defined-out))


(define linecolor "white")
(define squarecolor "goldenrod")
(define textcolor "black")
(define bgcolor "goldenrod")
(define titlepart (text "Go" 30 "black"))
;add in endgame to client

;message to server is either:
;(list "newgame" boardsize)
;or
;(list "joingame")
;or
;(list "newmove" move)
;move is (list movetype (list x y))

;message from server can be either:
;(list "newgame" boardsize playernum)
;or
;(list "updategame" newboard pmove pplayernum)
;ptype is player color
;1 - black
;2 - white
;turn is based on player colors
;0 - not set
;1 - black
;2 - white

;starttype/sendstate can be either 
;0 - create new game
;1 - join game
;2 - message has already been sent

;main game model is (list ptype gamesize blocknum boardmap piecenums pmoveinfo <- previous move 
;| approxnew <- new position based on proximity hover (not permanent), both list and draw coords 
;| pieceinfos <- precalculated coordinates | blocksize, piecesize, turn, starttype, endgame?)

(define (gengame ptype blocknum gamesize starttype [turn ptype]) (list ptype 
                                                gamesize 
                                                (- blocknum 1)
                                                (basemap (- blocknum 1)) 
                                                (list (expt blocknum 2) 0 0) 
                                                (list 1 -200 -200) 
                                                (list -200 -200 -200 -200) 
                                                (getposns (basemap (- blocknum 1)) (- blocknum 1)(getblocksize gamesize (- blocknum 1)))
                                                (getblocksize gamesize (- blocknum 1))
                                                (getplen (getblocksize gamesize (- blocknum 1)))
                                                turn
                                                starttype
                                                #f))
;move is (list mtype xcoord ycoord)
;0 - pass (list 0 -200 -200)
;1 - set (list 1 x y)

;to work on:
;divine move cheat?
(define (getopposite ptype)
  (cond [(= ptype 1) 2]
        [else 1]))

;render functions

;Outlines image (thicker than normal outline)
;image-> image
(define (fulloutline img cl)
  (local [(define h (image-height img))
          (define w (image-width img))]
    (add-line (add-line (add-line (add-line img w 0 0 0 cl) w h w 0 cl) 0 h w h cl) 0 0 0 h cl)))

;generates an image of a normal square, to be used to make the game map
;Int Int -> Image
(define (normalblock gamesize blocknum)
  (local [(define blocklen (/ gamesize blocknum))]
    (fulloutline (square blocklen "solid" squarecolor) linecolor)))

;repeatedly accumulates the function f on items for num times
;Function Any Int -> Any
(define (duplifunc f item [num 1])
  (cond [(<= num 1) item]
        [else (f item (duplifunc f item (- num 1)))]))

;accumulate through lst with function f
(define (accum f lst)
  (cond [(= 1 (length lst)) (first lst)]
        [else (f (first lst) (accum f (rest lst)))]))

;generates the overall board based on number of squares selected
;Int Int -> Image
(define (genboard gamesize blocknum)
  (local [(define baseblock (normalblock gamesize blocknum))
          (define baserow (duplifunc beside baseblock blocknum))
          (define board (duplifunc above baserow blocknum))]
    board))

;builds a list of length num with item items
; Any Int -> [Any]
(define (buildlist item num)
  (build-list num (lambda (x) item)))

;generates a base map of all pieces
;0 = nothing
;1 = black
;2 = white
;Int -> [[Int]]
(define (basemap blocknum)
  (buildlist (buildlist 0 (+ 1 blocknum)) (+ 1 blocknum)))

(define (blkmap blocknum)
  (buildlist (buildlist 1 (+ 1 blocknum)) (+ 1 blocknum)))

;get piece block size
(define (getblocksize gamesize blocknum)
  (/ gamesize blocknum))

;gets piece length
(define (getplen blocksize)
  (/ (- blocksize 5) 2))

;gets piece length, fill from gamesize and blocknum
(define (getplena gamesize blocknum)
  (getplen (getblocksize gamesize blocknum)))

;pmap is list of all, plen is radius of each piece (- (/ gamesize blocknum) 2)
;renders individual pieces
;Int val, Int plen, Int blocklen -> Image
(define (renderpiece val plen blocklen)
  (cond [(= 0 val) (square blocklen "solid" (make-color 0 0 0 0))]
        [(= 1 val) (overlay (circle plen "outline" "gray") (circle plen "solid" "black") (square blocklen "solid" (make-color 0 0 0 0)))]
        [(= 2 val) (overlay (circle plen "outline" "gray") (circle plen "solid" "white") (square blocklen "solid" (make-color 0 0 0 0)))]))

;maps that render and accumulates it
;after making a map of all pieces
(define (renderpieces pmap plen blocklen)
  (local [(define (renderrow rlist) (accum beside (map (lambda (x) (renderpiece x plen blocklen)) rlist)))]
    (accum above (map renderrow pmap))))

;!!! this should run in the mouse handler on click if the click is a valid position set - very resource expensive
;detects and increments each piece accordingly (countpieces helper)
(define (pcount piece curvals)
  (cond [(= 0 piece) (list (+ 1 (first curvals)) (second curvals) (third curvals))]
        [(= 1 piece) (list (first curvals) (+ 1 (second curvals)) (third curvals))]
        [(= 2 piece) (list (first curvals) (second curvals) (+ 1 (third curvals)))]))

;counts number of black, white, open pieces on the board currently
(define (countpieces boardmap)
  (foldl pcount (list 0 0 0) (flatten boardmap)))

;renders the precompiled list of amounts of pieces (boardinfo)
(define (textcount boardinfo)
    (above (text (string-append "Open spaces on board: " (number->string (first boardinfo))) 18 textcolor)
           (text (string-append "Black pieces on board: " (number->string (second boardinfo))) 18 textcolor)
           (text (string-append "White pieces on board: " (number->string (third boardinfo))) 18 textcolor)))
;!!!

;previous move marker
(define (mkpmove psize)
  (circle (max 0.0001 (* psize 0.4)) "solid" "gray"))

;possible next move marker
(define (nextmove ptype psize)
  (cond [(= 1 ptype) (circle psize "solid" (make-color 0 0 0 200))]
        [else (circle psize "solid" (make-color 255 255 255 200))]))

;main render function
;PROBLEM CODE
(define (mainrender ptype gamesize blocknum boardmap piecenums pmoveinfo approxnew precalcposns bsize psize turn endgame?)
  (local [(define dgame (place-image (cond [(= turn ptype) (nextmove ptype psize)]
                                           [else (square 0 "solid" (make-color 0 0 0 0))])
                                     (first approxnew)
                                     (second approxnew)
                                     (above titlepart 
                                            (place-image (mkpmove psize) 
                                                         (second pmoveinfo) 
                                                         (third pmoveinfo) 
                                                         (overlay (renderpieces boardmap psize bsize) 
                                                                  (genboard gamesize blocknum))) 
                                            (textcount piecenums))))]
    (cond [(= 0 ptype) (overlay (text "Waiting for game..." 18 "black") (rectangle (image-width dgame) (image-height dgame) "solid" "white"))]
          [else dgame])))
        

;higher level render based on model architecture
(define (render model)
  ;end game
  ;FINISH THIS LATER
  (cond [(last model) (text "Game over." 18 "black")]
        ;normal game
        [else (local [(define game (mainrender (first model) (second model) (third model) (fourth model) (fifth model) (sixth model) (seventh model) (eighth model) (ninth model) (tenth model) (tenth (rest model)) (last model)))]
                (overlay game (rectangle (image-width game) (image-height game) "solid" bgcolor)))]))

;/render functions

;mouse/hitbox functions

(define ymodifier (image-height titlepart))

;check if within hitbox
;x, y , hitbox corner x, hitbox corner y, hitbox width, hitbox height -> bool 
(define (inhitbox? x y hx hy hw hh)
  (and (>= x hx) 
       (<= x (+ hx hw))
       (>= y hy)
       (<= y (+ hy hh))))

;gets the quadrant that the point is on in a graph of w * h where the origin is half w, half h (based on computer coordinates, y is flipped)
(define (getqd x y w h)
  (local [(define halfw (round (/ w 2)))
          (define halfh (round (/ h 2)))]
    (cond [(< y halfh) (cond [(> x halfw) 1]
                             [else 2])]
          [(> y halfh) (cond [(> x halfw) 4]
                             [else 3])]
          [else (random 5)])))

(define (init lst)
  (local [(define rlst (reverse lst))]
    (reverse (remove (first rlst) rlst))))

;gets the posns of all the points on the given board
;(list x y xinlist yinlist)
(define (posnrow lsty row blocksize)
  (local [(define alist (init row))]
    (fifth (foldr (lambda (x pres) (list (+ blocksize (first pres)) 
                                         (second pres) 
                                         (add1 (third pres)) 
                                         lsty 
                                         (append (fifth pres) (list (list (+ blocksize (first pres)) (second pres) (add1 (third pres)) lsty))))) 
                  (list (/ blocksize 2) (+ (* blocksize lsty) (/ blocksize 2) ymodifier) 0 lsty (list (list (/ blocksize 2) (+ (* blocksize lsty) (/ blocksize 2) ymodifier) 0 lsty))) 
                  alist))))

(define (getposns board blocknum blocksize)
  (first (foldr (lambda (x curres) (list (append (first curres) 
                                                       (list (posnrow (second curres) x blocksize))) 
                                               (add1 (second curres)))) 
                      (list empty 0) 
                      board)))

;get quadrant -> cut list based on quadrant -> sort based on distance

;gets everything before pos
(define (lhead lst pos)
  (reverse (list-tail (reverse lst) (+ (- (length lst) pos) 1))))
;gets everything after pos
(define (ltail lst pos)
  (list-tail lst pos))
;replace item at lref index with newitem
(define (replace lst newitem lref)
  (append (lhead lst (+ lref 1)) (list newitem) (ltail lst (+ lref 1))))

;cuts the board based on quadrants
(define (cutboard board quadrant boarddimension)
  (local [(define halfdimension (ceiling (/ boarddimension 2)))
          (define vsplitb (cond [(or (= quadrant 1) (= quadrant 2)) (lhead board (+ halfdimension 1))]
                                [(or (= quadrant 3) (= quadrant 4)) (ltail board (- halfdimension 1))]
                                [else board]))
          (define hsplitb (cond [(or (= quadrant 1) (= quadrant 4)) (map (lambda (x) (ltail x (- halfdimension 1))) vsplitb)]
                                [(or (= quadrant 2) (= quadrant 3)) (map (lambda (x) (lhead x (+ halfdimension 1))) vsplitb)]
                                [else vsplitb]))]
    hsplitb))


;distance formula
(define (distance x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(define (distance2 x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs(- y2 y1))))

;make blocks of blocklen length (unsafe, needs to fit perfectly, but faster)
(define (mkblocks lst blocklen)
  (cond [(empty? lst) empty]
        [else (append (list (lhead lst (+ blocklen 1))) (mkblocks (ltail lst blocklen) blocklen))]))

;reforms the board into a single-ish dimensional list
(define (reformcuts cutboard)
  (mkblocks (flatten cutboard) 4))

;gets the point with the shortest distance from the mouse coordinates, using the quadrants for speed
(define (sortdistances mx my points [cursmallest (list -200 -200 -200 -200)] [pdistance +inf.0])
  (cond [(empty? points) cursmallest]
        [else (local [(define curpt (first points))
                      (define d2cur (distance2 (first curpt) (second curpt) mx my))]
                (cond [(< d2cur pdistance) (sortdistances mx my (rest points) curpt d2cur)]
                      [else (sortdistances mx my (rest points) cursmallest pdistance)]))]))


;get the approximate list position OR x/y based on where the mouse is
; Int mx Int my Int gamesize Int Blocknum List board -> List (point)
(define (approxposn mx my gamesize blocknum board bsize)
  (local [(define mquadrant (getqd mx (- my ymodifier) gamesize gamesize))
          (define filteredboard (reformcuts (cutboard board mquadrant blocknum)))]
    (sortdistances mx my filteredboard)))

;bit messy, clean up later
(define (mousehandler model x y event)
  (local [(define ptype (first model))
          (define gamesize (second model))
          (define blocknum (third model))
          (define posnboard (eighth model))
          (define bsize (ninth model))
          (define board (fourth model))
          (define turn (tenth (rest model)))
          (define pmove (sixth model))
          (define inhit (inhitbox? (+ x bsize) (+ bsize(- y ymodifier)) bsize bsize (+ gamesize bsize) (+ gamesize bsize)))]
    (cond [(and inhit (= ptype turn)) 
           (cond [(string=? event "move") 
                  (local [(define gotposn (approxposn x y gamesize blocknum posnboard bsize))
                          (define posnvalue (list-ref (list-ref board (fourth gotposn)) (third gotposn)))]
                    (list (first model) gamesize blocknum board (fifth model) pmove
                          (cond [(= posnvalue 0) gotposn]
                                [else (list -200 -200 -200 -200)])
                          posnboard bsize (tenth model) turn (tenth (rest (rest model))) (last model)))]
                 [(string=? event "button-down") (local [(define gotposn (approxposn x y gamesize blocknum posnboard bsize))
                                                         (define replacey (fourth gotposn))
                                                         (define replacex (third gotposn))
                                                         (define ptype (first model))
                                                         (define posnvalue (list-ref (list-ref board replacey) replacex))]
                                                   (cond [(= posnvalue 0) (make-package model (list "newmove" (list 1 replacex replacey)))]
                                                         [else (list (first model) gamesize blocknum 
                                                                     board
                                                                     (fifth model) pmove (list -200 -200 -200 -200) posnboard bsize (tenth model) turn (tenth (rest (rest model))) (last model))]))]
                 [else model])]
          ;pass
          ;FINISH THIS LATER
          ;add pass button
          ;[(string=? event "button-down") model]
          [else model])))
;end mouse/hitbox functions

;sends starting message to server
(define (autosend model)
  (local [(define sendstate (tenth (rest (rest model))))]
    (cond
      ;new game
      [(= 0 sendstate) (make-package (append (init (init model)) (list 2) (list #f))
                                     (list "newgame" (third model)))]
      ;join game
      [(= 1 sendstate) (make-package (append (init (init model)) (list 2) (list #f))
                                     (list "joingame" (third model)))]
      ;already sent message before
      [else model])))

(define (addpiece piececount ptype)
  (cond [(= 1 ptype) (list (- (first piececount) 1) (+ (second piececount) 1) (third piececount))]
        [(= 2 ptype) (list (- (first piececount) 1) (second piececount) (+ (third piececount) 1))]
        [else piececount]))

(define (optype pnum)
  (cond [(= pnum 1) 2]
        [else 1]))

(define (mapmove coords precalc [fprecalc (reformcuts precalc)])
  (cond [(empty? fprecalc) (list 0 0)]
        [(and (= (first coords) (third (first fprecalc)))
              (= (second coords) (fourth (first fprecalc)))) (list (first (first fprecalc)) (- (second (first fprecalc)) ymodifier))]
        [else (mapmove coords precalc (rest fprecalc))]))

(define (handlemessage curstate msg)
  (local [(define msgtype (first msg))
          (define msginfo (rest msg))]
    (cond [(string=? msgtype "newgame") 
           (gengame (second msginfo)
                    (first msginfo)
                    (second curstate)
                    3
                    1)]
          ;problem code
          [(string=? msgtype "updategame")
           (local [(define newboard (first msginfo))
                   (define move (second msginfo))
                   (define moveplayer (third msginfo))
                   (define ptype (first curstate))
                   (define gotposns (eighth curstate))]
             (list ptype
                   (second curstate)
                   (third curstate)
                   newboard
                   (cond [(= 1 (first move)) 
                          (addpiece (fifth curstate) moveplayer)]
                         [else (fifth curstate)])
                   (cond [(not (= moveplayer ptype)) (append (list (first move)) (mapmove (rest move) gotposns))]
                         [else (sixth curstate)])
                   (seventh curstate)
                   gotposns
                   (ninth curstate)
                   (tenth curstate)
                   (optype moveplayer)
                   3
                   #f))]
          [(string=? msgtype "endgame") 
           (stop-with (append (init curstate) (list #t)))]
          [else curstate])))



(define (startgo gamesize blocknum ip starttype)
  (big-bang (gengame 0  
                     blocknum 
                     gamesize
                     starttype)
            (on-tick autosend)
            (on-mouse mousehandler)
            (on-draw render)
            (on-receive handlemessage)
            (register ip)))

;(big-bang (gengame 2 19 200)
;          (on-mouse mousehandler)
;          (on-draw render))