#lang racket

(require picturing-programs)

(define linecolor "DarkBlue")
(define squarecolor "NavajoWhite")
(define titlepart (text "Go" 30 "black"))

;ptype is player color
;0 - black
;1 - white
;main game model is (list ptype gamesize blocknum boardmap piecenums pmoveinfo <- previous move 
;| approxnew <- new position based on proximity hover (not permanent), both list and draw coords | pieceinfos <- precalculated coordinates | blocksize, piecesize)
(define (gengame ptype blocknum gamesize) (list ptype 
                                                gamesize 
                                                blocknum 
                                                (basemap blocknum) 
                                                (list 0 0 0) 
                                                (list 1 -200 -200) 
                                                (list -200 -200 -200 -200) 
                                                (getposns (basemap blocknum) blocknum (getblocksize gamesize blocknum))
                                                (getblocksize gamesize blocknum)
                                                (getplen (getblocksize gamesize blocknum))))
;move is (list mtype xcoord ycoord)
;0 - pass (list 0 -200 -200)
;1 - set (list 1 x y)

;to work on:
;hitboxes
;...
;divine move cheat?

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
        [(= 1 val) (overlay (circle plen "solid" "black") (square blocklen "solid" (make-color 0 0 0 0)))]
        [(= 2 val) (overlay (circle plen "solid" "white") (square blocklen "solid" (make-color 0 0 0 0)))]))

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
    (above (text (string-append "Open spaces on board: " (number->string (first boardinfo))) 18 "black")
           (text (string-append "Black pieces on board: " (number->string (second boardinfo))) 18 "black")
           (text (string-append "White pieces on board: " (number->string (third boardinfo))) 18 "black")))
;!!!

;previous move marker
(define (mkpmove psize)
  (circle (- psize 6) "solid" "gray"))

;possible next move marker
(define (nextmove ptype psize)
  (cond [(= 1 ptype) (circle psize "solid" (make-color 0 0 0 175))]
        [else (circle psize "solid" (make-color 255 255 255 175))]))

;main render function
(define (mainrender ptype gamesize blocknum boardmap piecenums pmoveinfo approxnew precalcposns bsize psize)
  (place-image (nextmove ptype psize)
               (first approxnew)
               (second approxnew)
               (above titlepart 
                      (place-image (mkpmove psize) 
                                   (second pmoveinfo) 
                                   (third pmoveinfo) 
                                   (overlay (renderpieces boardmap psize bsize) (genboard gamesize blocknum))) 
                      (textcount piecenums))))

;higher level render based on model architecture
(define (render model)
  (mainrender (first model) (second model) (third model) (fourth model) (fifth model) (sixth model) (seventh model) (eighth model) (ninth model) (tenth model)))

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
                  (list 0 (+ (* blocksize lsty) ymodifier) 0 lsty (list (list 0 (+ (* blocksize lsty) ymodifier) 0 lsty))) 
                  alist))))

(define (getposns board blocknum blocksize)
  (first (foldr (lambda (x curres) (list (append (first curres) 
                                                       (list (posnrow (second curres) x blocksize))) 
                                               (add1 (second curres)))) 
                      (list empty 0) 
                      board)))

;get quadrant -> cut list based on quadrant -> quicksort based on distance

;gets everything before pos
(define (lhead lst pos)
  (reverse (list-tail (reverse lst) (+ (- (length lst) pos) 1))))
;gets everything after pos
(define (ltail lst pos)
  (list-tail lst pos))

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
                      (define d2cur (distance (first curpt) (second curpt) mx my))]
                (cond [(< d2cur pdistance) (sortdistances mx my (rest points) curpt d2cur)]
                      [else (sortdistances mx my (rest points) cursmallest pdistance)]))]))


;get the approximate list position OR x/y based on where the mouse is
; Int mx Int my Int gamesize Int Blocknum List board -> List (point)
(define (approxposn mx my gamesize blocknum board)
    (cond [(inhitbox? mx (- my ymodifier) 0 0 gamesize gamesize) (list -200 -200 -200 -200)]
          [else (local [(define mquadrant (getqd mx (- my ymodifier) gamesize gamesize))
                        (define filteredboard (reformcuts (cutboard board mquadrant blocknum)))]
                  (sortdistances mx my filteredboard))]))

;checks if the previous move was a pass and if the current move is a pass
;(define (checkend ))

;end mouse/hitbox functions

;(getposns (blkmap 19) 19 20)
(cutboard (getposns (blkmap 19) 19 20) 2 19)
(render (gengame 1 19 500))
