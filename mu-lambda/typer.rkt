#lang racket

(provide run)

(define (fresh-ty-var)
  (datum->syntax #f (gensym "tvar")))
  
(define (make-fun-ty ty-arg ty-ret)
  #`('-> #,ty-arg #,ty-ret))

;; builtin types

(define t-int #''int)

;; type inference

(define (env-lookup var env)
  (cond
    ((null? env) #f)
    ((equal? (caar env) var) (cdar env))
    (else (env-lookup var (cdr env)))))

(define (infer-const x)
  (cond
    ((number? x) t-int)))

(define (infer env stx)
  (syntax-case stx ()

    ; λ-abs
    ((kwd args body) (eq? (syntax-e #'kwd) 'lambda)
      (syntax-case #'args ()
        ((arg) (identifier? #'arg)
          (let*-values (((ty-arg) (fresh-ty-var))
                        ((ty-body sys) (infer (cons (cons (syntax-e #'arg) ty-arg) env) #'body)))
            (values (make-fun-ty ty-arg ty-body) sys)))
        (else (raise-syntax-error #f "only unary functions are authorized"))))

    ; application
    ((kwd fun arg) (eq? (syntax-e #'kwd) '#%app)
      (let*-values (((ty-fun s0) (infer env #'fun))
                    ((ty-arg s1) (infer env #'arg))
                    ((ty-ret) (fresh-ty-var)))
        (values ty-ret (cons (cons ty-fun (make-fun-ty ty-arg ty-ret)) (append s0 s1)))))
    ((kwd args ...) (eq? (syntax-e #'kwd) '#%app) (raise-syntax-error #f "n-ary application"))

    ; constants
    ((kwd x) (eq? (syntax-e #'kwd) 'quote) (values (infer-const (syntax-e #'x)) '()))

    ; variables
    (x (identifier? #'x)
      (cond
        ((env-lookup (syntax-e stx) env) => (λ (x) (values x '())))
        (else (raise-syntax-error #f (format "unbound variable: ~a" (syntax-e #'x))))))))

;; unification

; applies a given substitution to a single term
(define (apply-one var term t)
  (syntax-case t ()
    ((terms ...) #`(#,@(map (λ (x) (apply-one var term x)) (syntax-e #'(terms ...)))))
    (x (identifier? #'x) (if (equal? (syntax-e t) var) term t))))

; aoplies a given substitution to an equation system
(define (apply-subst subst sys)
  (map (lambda (x) (cons
                     (apply-one (car subst) (cdr subst) (car x))
                     (apply-one (car subst) (cdr subst) (cdr x))))
       sys))

; checks if the variable var occurs free in the term t
(define (occur-check var t)
  (syntax-case t ()
    ((terms ...) (foldl (λ (term acc) (or (occur-check var term) acc))
                        #f (syntax-e #'(terms ...))))
    (x (identifier? #'x) (equal? (syntax-e t) var))))

; checks if pred1 and pred2 correspond to a same predicate in the sense
; of unification. for now only constants are allowed as predicates
(define (predicate-equal? pred1 pred2)
  (syntax-case #`(#,pred1 . #,pred2) (quote)
    (((quote x) . (quote y))
      (and (identifier? #'x)
           (identifier? #'y)
           (equal? (syntax-e #'x) (syntax-e #'y))))
    (else #f)))

; unify an equation system
(define (unify eq-sys)
  (match eq-sys
    ('() '())
    (`((,t1 . ,t2) ,rest ...)
      (syntax-case #`(#,t1 #,t2) ()

        ; predicate elimination
        (((pred1 args1 ...) (pred2 args2 ...)) (predicate-equal? #'pred1 #'pred2)
          (unify (foldl (λ (a1 a2 acc) `((,a1 . ,a2) . ,acc)) rest
                        (syntax-e #'(args1 ...))
                        (syntax-e #'(args2 ...)))))

        ; variable substitution
        ((x term) (identifier? #'x)
          (let ((x (syntax-e #'x)))
            (if (occur-check x #'term)
              (raise (format "error: ~a must be equal to ~a (occur check failed)"
                             x #'term))
              (let* ((subst (cons x #'term))
                     (sys (apply-subst subst rest)))
              (cons subst (unify sys))))))
        ((term x) (identifier? #'x)
          (let ((x (syntax-e #'x)))
            (if (occur-check x #'term)
              (raise (format "error: ~a must be equal to ~a (occur check failed)"
                             x #'term))
              (let* ((subst (cons x #'term))
                     (sys (apply-subst subst rest)))
              (cons subst (unify sys))))))

        ; identity
        ((x y) (equal? (syntax->datum #'x) (syntax->datum #'y)) '())

        (else
          (raise-syntax-error #f (format "impossible to unify ~a with ~a"
                                         (syntax->datum t1) (syntax->datum t2))))))

    (else (raise (format "unify: bad system:\n~a" eq-sys)))))

; initial environment
(define ini-env `(
  (+ . ,#`((quote ->) #,t-int #,t-int))
))

(define (run stx)
  (display "μλ typer running\n")
  (foldl
    (lambda (stx acc) 
      (let*-values (((ty sys) (infer acc stx))
                    ((subst) (unify sys))
                    ((ty-ret) (foldl (lambda (subst ty)
                                       (apply-one (car subst) (cdr subst) ty))
                                     ty subst)))
        (display (format "type: ~a\n\n" (syntax->datum ty-ret)))
        acc))
    ini-env stx))
