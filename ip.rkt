#lang racket
(provide ip?)
;divide by the . and do byte? on each block
(define (ip? ipaddr) ipaddr)