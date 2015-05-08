#lang racket

(require picturing-programs)

(define linecolor "DarkBlue")
(define squarecolor "NavajoWhite")
(define titlepart (text "Go" 30 "black"))

;ptype is player color
;0 - black
;1 - white
;main game model is (list ptype boardsize blocknum piecemap pieceinfo previousopponentmove)
(define (gengame ptype blocknum) (list ptype 500 blocknum (basemap blocknum) (list 0 0 0) (list 1 -200 -200) (list 0 0 0)))
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

;main render function
(define (mainrender gamesize blocknum boardmap piecenums pmoveinfo approxnew)
  (local [(define bsize (getblocksize gamesize blocknum))
          (define psize (getplen bsize))]
    (above titlepart (place-image (mkpmove psize) (second pmoveinfo) (third pmoveinfo) (overlay (renderpieces boardmap psize bsize) (genboard gamesize blocknum))) (textcount piecenums))))

;higher level render based on model architecture
(define (render model)
  (mainrender (second model) (third model) (fourth model) (fifth model) (sixth model) (seventh model)))

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

;distance formula
(define (distance x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

;gets the quadrant that the point is on in a graph of w * h where the origin is half w, half h (based on computer coordinates, y is flipped)
(define (getqd x y w h)
  (local [(define halfw (round (/ w 2)))
          (define halfh (round (/ h 2)))]
    (cond [(< y halfh) (cond [(> x halfw) 1]
                             [else 2])]
          [(> y halfh) (cond [(> x halfw) 4]
                             [else 3])]
          [else (random 5)])))

;gets the posns of all the points on the given board
(define (getposn board blocknum blocksize)
  )

;get the approximate list position OR x/y based on where the mouse is
;returns either (list 0 0 0) <- not on board
;or (list 1 lstx lsty) <- on board, use coordinates to render
;(define (approxposn rmx rmy gamesize blocknum)
;  (local [(define mx rmx)
;          (define my (+ ymodifier rmy))]
;    (cond [(inhitbox? mx my 0 ymodifier gamesize gamesize) (list 0 0 0)]
;          [else ])))

;checks if the previous move was a pass and if the current move is a pass
;(define (checkend ))

;mouse/hitbox functions

(mainrender 500 19 (blkmap 19) (countpieces (blkmap 19)) (list 1 (/ (getblocksize 500 19) 2) (/ (getblocksize 500 19) 2)) (list 0 0 0))
(render (gengame 0 19))