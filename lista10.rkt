#lang racket

;; zad 1

;; left-to-right evaluation
(define (list-of-values1 exps env)
  (if (no-operands? exps)
      '()
      (let ([first (eval (first-operand exps) env)])
      (cons first
            (list-of-values1 (rest-operands exps) env)))))

;; right-to-left evaluation
(define (list-of-values2 exps env)
  (if (no-operands? exps)
      '()
      (let ([rest (list-of-values2 (rest-operands exps) env)])
      (cons (eval (first-operand exps) env)
            rest))))