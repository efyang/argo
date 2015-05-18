#lang racket/gui

(require "main.rkt")
(require "ip.rkt")
(require picturing-programs)

(define mgui (new frame% [label "Go"]))
(define header (new vertical-panel% [parent mgui]))
(define logomsg (new message% [parent header]             
                 [label "围棋"]
                 [font (make-object font% 40 'modern)]))
(define getip (new text-field% [parent header]
                   [init-value "127.0.0.1"]
                   [label "Server IP Address: "]))
(define wsize (new slider%
                   [parent header]
                   [label "Window Size"]
                   [min-value 200]
                   [max-value 1280]
                   [init-value 300]))
(new message% [parent header]
  [label "What do you want to do?"])
(define (converted ptype)
  (= (send createorjoin get-selection) ptype))
(define createorjoin (new radio-box% [parent header]
                          [label ""]
                          [choices (list "Create New Game" "Join a Game")]
                          [style (list 'horizontal)]
                          [callback (lambda (radbox cevnt)
                                      (send buttonpanel1 enable (converted 0))
                                      (send createpanel enable (converted 0))
                                      (send buttonpanel2 enable (converted 1))
                                      (send joinpanel enable (converted 1)))]))

(define splitpanel (new horizontal-panel% [parent header]))
;panel to create a new game
(define createpanel (new vertical-panel% [parent splitpanel]
                         [border 5]
                         [style (list 'border)]
                         [enabled #t]))
(new message% [parent createpanel]             
     [label "Creates a new game."])
(define gsize (new radio-box% [parent createpanel]
                   [label "Number of Lines"]
                   [choices (list "5" "7" "9" "11" "13" "15" "17" "19")]
                   [style (list 'vertical)]))

;panel to join a game
(define joinpanel (new vertical-panel% [parent splitpanel]
                       [border 5]
                       [style (list 'border)]
                       [enabled #f]))
(new message% [parent joinpanel]             
     [label "Joins a random game."])
(new message% [parent joinpanel]             
     [label "\nIf none exist,\ncreates a new one."])


(define buttonpanel (new horizontal-panel% [parent header]
                         [alignment (list 'center 'center)]))
(define buttonpanel1 (new horizontal-panel% [parent buttonpanel]
                          [alignment (list 'center 'center)]
                          [enabled #t]))
(define buttonpanel2 (new horizontal-panel% [parent buttonpanel]
                          [alignment (list 'center 'center)]
                          [enabled #f]))

;implement looped game later
(new button% [parent buttonpanel1]             
     [label "Create"]   
     [callback (lambda (button event) 
                 (send mgui show #f)
                 (startgo (send wsize get-value)
                          (string->number (send gsize 
                                                get-item-label
                                                (send gsize get-selection)))
                          (ipcheck (send getip get-value))
                          0))])

(new button% [parent buttonpanel2]             
     [label "Join"]   
     [callback (lambda (button event) 
                 (send mgui show #f)
                 (startgo (send wsize get-value)
                          (string->number (send gsize 
                                                get-item-label
                                                (send gsize get-selection)))
                          (ipcheck (send getip get-value))
                          1))])

(new button% [parent mgui]             
     [label "Cancel"]
     [callback (lambda (button event)                         
                 (send mgui show #f))])
(send mgui show #t)