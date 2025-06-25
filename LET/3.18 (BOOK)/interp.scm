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

        (equal?-exp (exp1 exp2) 
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (if (equal? val1 val2)
              (bool-val #t)
              (bool-val #f))))

        (less?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val (< num1 num2)))))

        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val (> num1 num2)))))

        (emptylist-exp () 
          (emptylist-val))

        (cons-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (pair-val val1 val2)))

        (car-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (pair-val (car-val cdr-val)
                car-val)
              (else (expval-extractor-error 'pair val1)))))

        (cdr-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (pair-val (car-val cdr-val)
                cdr-val)
              (else (expval-extractor-error 'pair val1)))))

              
      (unpack-exp (identifiers lstval body)
        (let* ((val1 (value-of lstval env))
              (val-list (expval->list val1)))
          (if (equal? (length identifiers) (length val-list))
              (let ((new-env (extend-env-list identifiers val-list env)))
                (value-of body new-env))
              (eopl:error 'unpack "Length mismatch in unpack"))))
      )))

  (define (lst-len lst env)
    (let ((val1 (value-of lst env)))
      (cases expval val1
        (pair-val (car-val cdr-val)
            (+ 1 (lst-len cdr-val env)))
        (bool-val (bool) 1)
        (num-val (num) 1)
        (emptylist-val () 0))))

  (define (expval->list val)
    (cases expval val
      (emptylist-val () '())
      (pair-val (car-val cdr-val)
        (cons car-val (expval->list cdr-val)))
      (else (expval-extractor-error 'pair val))))

  (define extend-env-list
    (lambda (vars vals env)
      (if (null? vars)
    env
    (let ((var1 (car vars))
          (val1 (car vals)))
      (extend-env-list (cdr vars) (cdr vals) (extend-env var1 val1 env))))))

)



