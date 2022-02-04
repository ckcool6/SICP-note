;;use DrRacket

;;;code

#lang racket/base
(require racket/gui)
(require sicp-pict)

;; einstein 
(paint einstein)

;; einstein-2
(define einstein-2
  (beside einstein (flip-vert einstein)))

(paint einstein-2)

;; einstein-4

(define einstein-4
  (below einstein-2 einstein-2))

(paint einstein-4)

;;
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define einstein4
  (flipped-pairs einstein))

(paint einstein4)

;;

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 8))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split einstein 4))
(paint (corner-split einstein 8))

;;Escher's art
(paint (escher))

;;
(define (square-of-four tl tr bl br)
  (λ (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs-2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                 identity flip-vert)))
  (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 4))

;;;
