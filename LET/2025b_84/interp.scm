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

        (cast-to-bool-exp (bool) (cast-to-bool bool env))

        (cast-to-num-exp (num) (cast-to-num num env))

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
          (let ((val1 (cast-to-bool exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;; new implementation of let x = 10 y = 10 .... in ....
        (let-exp (identifiers expressions body)
          (let ((vals (map (lambda (exp) (value-of exp env)) expressions)))
            (let ((extended-env (extend-env-list identifiers vals env)))
              (value-of body extended-env))))

        ;\commentbox{\ma{\theletspecsplit}}
        ;; (let-exp (var exp1 body)
        ;;  (let ((val1 (value-of exp1 env)))
        ;;    (value-of body
        ;;      (extend-env var val1 env))))

        )))

    (define extend-env-list
      (lambda (vars vals env)
        (if (null? vars)
      env
      (let ((var1 (car vars))
            (val1 (car vals)))
        (extend-env-list (cdr vars) (cdr vals) (extend-env var1 val1 env))))))

    (define cast-to-bool
      (lambda (exp1 env)
        (let ((expval1 (value-of exp1 env)))
          (cases expval expval1
            (num-val (numval)
              (if (= 0 numval)
                (bool-val #f)
                (bool-val #t)))
            (bool-val (boolval)
              (bool-val boolval))))))
    
    (define cast-to-num
      (lambda (exp1 env)
        (let ((expval1 (value-of exp1 env)))
          (cases expval expval1
            (bool-val (boolval)
              (if boolval 
                (num-val 1)
                (num-val 0)))
            (num-val (numval)
              (num-val numval))))))
  )

