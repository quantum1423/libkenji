#lang racket
(require data/queue)

(define haha (make-queue))
(enqueue! haha 0)
(time
 (for ([i 100000])
  (enqueue! haha (add1 (dequeue! haha)))))