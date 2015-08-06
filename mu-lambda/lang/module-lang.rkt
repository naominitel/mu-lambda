#lang racket

(provide #%app)
(provide #%top)
(provide #%datum)

(provide +)

(provide (rename-out (define def)))
(provide (rename-out (lambda λ)))
(provide (rename-out (lambda \\)))
(provide (rename-out (lambda fun)))
(provide (rename-out (lambda function)))
(provide (rename-out (μλ-module-begin #%module-begin)))

(require (for-syntax "../typer.rkt"))

(define-syntax μλ-module-begin
  (lambda (stx)
    (let* ((stx (cdr (syntax-e stx)))
           (expanded (local-expand #`(#%plain-module-begin #,@stx) 'module-begin (list))))
      (run (cdr (syntax-e expanded)))
      expanded)))
