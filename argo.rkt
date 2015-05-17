#lang racket/gui

(require "main.rkt")
(require "ip.rkt")
(require picturing-programs)

(define mgui (new frame% [label "Go"]))
(define midpanel (new vertical-panel% [parent mgui]))
(define logomsg (new message% [parent midpanel]             
                 [label "围棋"]
                 [font (make-object font% 40 'modern)]))
(define infomsg (new message% [parent midpanel]             
                 [label "More lines will result in a laggier game."]))
(define gsize (new radio-box% [parent midpanel]
                   [label "Number of Lines"]
                   [choices (list "5" "7" "9" "11" "13" "15" "17" "19")]
                   [style (list 'horizontal)]))
(define wsize (new slider%
                   [parent midpanel]
                   [label "Window Size"]
                   [min-value 200]
                   [max-value 1280]
                   [init-value 512]))
(define getip (new text-field% [parent midpanel]
                   [init-value "127.0.0.1"]
                   [label "IP Address: "]))
(define bottompanel (new horizontal-panel% [parent midpanel]
                         [alignment (list 'center 'center)]))

(new button% [parent bottompanel]             
     [label "Start"]   
     [callback (lambda (button event) 
                 (send mgui show #f)
                 (startgo (string->number (send gsize 
                                                get-item-label
                                                (send gsize get-selection)))
                          (send wsize get-value)
                          (ipcheck (send getip get-value))))])

(new button% [parent bottompanel]             
     [label "Cancel"]
     [callback (lambda (button event)                         
                 (send mgui show #f))])
(send mgui show #t)