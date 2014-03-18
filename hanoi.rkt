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
  (cond ((and (equal? src-str (cadr last-move))
              (equal? dst-str (car last-move)))
         #f)
        ((null? src)
         #f)
        ((null? dst)
         (print-move src-str dst-str)
         #t)
        ((< (car src) (car dst))
         #f)
        ((and (odd? (car src)) (odd? (car dst)))
         #f)
        ((and (even? (car src)) (even? (car dst)))
         #f)
        (else
         (print-move src-str dst-str)
         #t)))

(define (finished from spare)
  (and (null? from) (null? spare)))

(define (legal-move from to spare last-move)
  (if (finished from spare)
      "DONE"
      (cond ((can-move from to "start" "finish" last-move)
              (legal-move
               (cdr from) (cons (car from) to) spare '("start" "finish")))
             ((can-move from spare "start" "spare" last-move)
              (legal-move
               (cdr from) to (cons (car from) spare) '("start" "spare")))
             ((can-move to spare "finish" "spare" last-move)
              (legal-move
               from (cdr to) (cons (car to) spare) '("finish" "spare")))
             ((can-move to from "finish" "start" last-move)
              (legal-move
               (cons (car to) from) (cdr to) spare '("finish" "start")))
             ((can-move spare to "spare" "finish" last-move)
              (legal-move
               from (cons (car spare) to) (cdr spare) '("spare" "finish")))
             ((can-move spare from "spare" "start" last-move)
              (legal-move
               (cons (car spare) from) to (cdr spare) '("spare" "start"))))))

(define (iterative-tower n)
  (let ([pieces (fifo n)])
    (if (even? n)
        (and (print-move "start" "spare")
             (legal-move
              (cdr pieces) '() (cons (car pieces) '()) '("start" "spare")))
        (and (print-move "start" "finish")
             (legal-move
              (cdr pieces) (cons (car pieces) '()) '() '("start" "finish"))))))
