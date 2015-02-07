#lang racket
; lista 9

; zadanie 3
; dostajemy liste, zliczamy liczbe atomow (nie par)
(define (count-atoms list) 
  (if (empty? list) 
      0 
      (if (pair? (car list)) 
          (+ (count-atoms (cdr list)) (count-atoms car list)) 
          (+ 1 (count-atoms (cdr list))))))


; zadanie 4

(require 2htdp/image)

(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))

(define (left-branch struct) (car struct))
(define (right-branch struct) (cdr struct))
(define (branch-length branch) (car branch))
(define (branch-struct branch) (cdr branch))

(define (calculate-weight struct)
  (if (number? struct)
      struct
      (+ (calculate-weight (branch-struct (left-branch struct))) (calculate-weight (branch-struct (right-branch struct))))
      )
  )

(define (balanced? struct)
  (if (number? struct)
      #t
  (let ([left (left-branch struct)]
        [right (right-branch struct)])
        (if (and (balanced? (branch-struct left)) (balanced? (branch-struct right))) 
            (eq? (* (calculate-weight (branch-struct left)) (branch-length left)) (* (calculate-weight (branch-struct right)) (branch-length right)))
            #f
            )
        )
    )
  )

(define (get-arm-end x y length direction)
	(if (eq? direction "right")
		(cons (+ x length) (+ y length))
		(cons (- x length) (+ y length))))

(define (draw-arm image x y length direction)
	(let ([end (get-arm-end x y length direction)]
		  [color (if (eq? direction "left") "yellowgreen" "darkolivegreen")])
		(add-line image x y (car end) (cdr end) (make-pen color 2 "solid" "round" "round"))))

(define (draw-weight img x y value) 
  (place-image (overlay (text (number->string value) 14 "darkslategrey") (star (max 25 value) "solid" "goldenrod")) x y img))

(define (draw-struct image x y struct)
	(let* ([left_len (branch-length (left-branch struct))]
		   [right_len (branch-length (right-branch struct))]
		   [lend (get-arm-end x y left_len "left")]
		   [rend (get-arm-end x y right_len "right")]
		   [image_with_arms (draw-arm (draw-arm image x y left_len "left") x y right_len "right")]
		   [image_left_complete (draw-on-image image_with_arms (car lend) (cdr lend) (branch-struct (left-branch struct)))])
		(draw-on-image image_left_complete (car rend) (cdr rend) (branch-struct (right-branch struct)))))


(define (draw-on-image image x y struct)
	(if (number? struct)
		(draw-weight image x y struct)
		(draw-struct image x y struct)))

(define (get-left-width struct)
  300)

(define (get-right-width struct)
  300)

(define (get-height struct)
  300)

(define (draw struct)
	(let ([left_width (get-left-width struct)]
		  [right_width (get-right-width struct)]
		  [height (get-height struct)])
		(let ([image (empty-scene (+ left_width right_width) (+ 100 height))])
			(draw-on-image image left_width 100 struct))))



(define structure (mk-mobile (mk-branch 20 30) (mk-branch 100 6)))


(define struct2 (mk-mobile (mk-branch 30 (mk-mobile (mk-branch 100 50) (mk-branch 20 10))) (mk-branch 100 5)))


(draw structure)
(calculate-weight structure)
(balanced? structure)

(draw struct2)
(calculate-weight struct2)
(balanced? struct2)

; zadanie 5

(define (simplify-numbers expr) ;expr == '(+ expr1 expr2) or expr == '(* expr1 expr2) or expr == (number or symbol)
  (if (list? expr)
      (if (and (number? (cadr expr)) (number? (caddr expr))) 
          (if (eq? '+ (car expr))
              (+ (cadr expr) (caddr expr))
              (* (cadr expr) (caddr expr))
              )
          expr
          )
      expr
      )
  )


(define (optimize expr) ; expr == '(+ expr1 expr2) or expr == '(* expr1 expr2) or expr == (number or symbol)
  (if (list? expr)
      (let ([a (cadr expr)]
            [b (caddr expr)])
        (if (eq? '* (car expr))
            (if (or (eq? a 0) (eq? b 0))
                0
                (if (eq? a 1)
                    b
                    (if (eq? b 1)
                        a
                        (simplify-numbers (list '* a b))
                        )
                    )
                )
            ; expr == '(+ expr1 expr2)
            (if (eq? a 0)
                b
                (if (eq? b 0)
                    a
                    (simplify-numbers (list '+ a b))
                    )
                )
            )
        )
        expr
      )
  )


(define (deriv expr var)
  (if (list? expr)
      (let ([a (optimize (deriv (cadr expr) var))]
            [b (optimize (deriv (caddr expr) var))])
        (if (eq? '* (car expr))
            (let* ([a-init (cadr expr)]
                  [b-init (caddr expr)]
                  [c (optimize (list '* a-init b))]
                  [d (optimize (list '* b-init a))])
              (optimize (list '+ c d))
              )
            (optimize (list '+ a b))
            )
        )
      (if (eq? expr var)
          1
          0
          )
      )
  )
           
(define (assert expr1 expr2)
  (if (and (list? expr1) (list? expr2))
      (foldl (lambda (a b result) (if (assert a b) result #f)) #t  expr1 expr2)
      (eq? expr1 expr2)
      )
  )
  

(assert 'y (deriv '(* x y) 'x))

(assert '(+ (* x y) (* (+ x 3) y)) (deriv '(* (* x y) (+ x 3)) 'x))

(assert (deriv '(+ (* 2 x) (* 5 x)) 'x) '7)

(assert (deriv '(+ (* 2 x) (* (* x x) 3)) 'x) '(+ 2 (* 3 (+ x x))))
