(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        (switch-exp (exp1 exps2 exps3 exp4)
          (if (null? exps2)
            (value-of exp4 env)
            (let* (
              (zipped (zip exps2 exps3))
              (matched (match zipped exp1 env)))

              (if (null? matched)
                (value-of exp4 env)
              matched))))


        )))

  (define (zip lst1 lst2)
    (define (helper lst1 lst2 res)
      (if (or (null? lst1) (null? lst2))
        res
        (helper (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) res))
      )
    )
    (helper lst1 lst2 '())
  )

  (define (match zipped var env)
    (define (helper zipped value)
      (if (null? zipped)
        '()
        (let* (
          (curr (value-of (car (car zipped)) env))
          (curr-val (expval->value curr)))
          
          (begin 
            (if (equal? curr-val (expval->value value))
              (value-of (cdr (car zipped)) env)
              (helper (cdr zipped) value)
            ))
        )
      )
    )

    (let ((value (value-of var env)))
      (helper zipped value)
    )

  )




  )

