#lang planet neil/sicp


(define (print-move from to)
  (write "FROM: ")
  (write from)
  (write " TO: ")
  (write to)
  (newline))

(define (move n from to spare)
  (cond ((= n 0) "DONE")
        (else
         (move (dec n) from spare to)
         (print-move from to)
         (move (dec n) spare to from))))

;; Iterative solution

(define (build-fifo cur size lst)
  (if (> cur size)
      lst
      (build-fifo (inc cur) size (cons cur lst))))

(define (fifo x)
  (build-fifo 1 x '()))

(define (can-move src dst src-str dst-str last-move)
  src)

(define (legal-move from to spare last-move)
  (if (finished from to spare)
      finished
      (cond ((can-nove from to "start" "finish" last-move)
             (legal-move
              (cdr from) (cons (car from) to) spare '("start" "finish")))
            ((can-move from spare "start" "spare")
             (legal-move
              (cdr from) to (cons (car from) spare) '("start" "spare")))
            ((can-move to spare "finish" "spare" last-move)
             (legal-move
              from (cdr to) (cons (car to) spare) '("finish" "spare")))
            ((can-move to from "finish" "start" last-move))
            ((can-move spare from "spare" "start" last-move))
            ((can-move spare to "spare" "finish" last-move)))))

(define (iterative-tower n)
  (let ([pieces (fifo n)])
    (if (even? n)
        (and (print-move "start" "spare")
             (legal-move
              (cdr pieces) '() (car pieces) '("start" "spare")))
        (and (print-move "start" "finish")
             (legal-move
              (cdr pieces) (car pieces) '() '("start" "finish"))))))
