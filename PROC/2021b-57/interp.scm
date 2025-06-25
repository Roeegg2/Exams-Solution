(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the procedural
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
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
        (let-exp (tmps exp1 body)
          (cases temps tmps
            (temp-single (var)
              (let ((val1 (value-of exp1 env)))
                (value-of body
                  (extend-env var val1 env)))
            )

            (temp-list (identifiers)
              (let* (
                (tuple-val (value-of exp1 env))
                (tuple-values (expval->tuple tuple-val)))
                
                (cond 
                  ((not (= (length identifiers) (length tuple-values)))  
                    (eopl:error 'let "Tuple extraction length mismatch"))
                  ((all-placeholders? identifiers) (eopl:error 'let "All placehlders"))
                  (else (let* ((zipped (zip identifiers tuple-values))
                              (filtered (filter zipped))
                              (extended-env (extend-env* env filtered)))
                    (value-of body extended-env))
                  )
                )
              )
            )
          ))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (tuple-exp (exps)
          (if (null? exps) 
            (eopl:error 'tuple "Can't create emtpy tuple.")
            (let ((expvals (map (lambda (x) (value-of x env)) exps)))
              (tuple-val expvals)
            )
          )
          
        )



        )))

  (define zip
    (lambda (lst1 lst2)
      (if (or (null? lst1) (null? lst2))
        '()
        (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))
      )
    )
  )

  (define (filter zipped)
    (define (helper zipped result)
      (if (null? zipped) result
        (cases optional-element (car (car zipped))
          (ident (identifier) 
            (helper (cdr zipped) (cons (cons identifier (cdr (car zipped))) result))
          )
          (else (helper (cdr zipped) result))
        )
      )
    )
  
    (helper zipped '())
  )

  (define extend-env* 
    (lambda (env lst)
      (if (null? lst)
        env
        (extend-env* (extend-env (car (car lst)) (cdr (car lst)) env) (cdr lst))
      )
    )
  )

  (define all-placeholders?
    (lambda (identifiers)
      (if (null? identifiers) #t
        (cases optional-element (car identifiers)
          (ident (identifier) #f)
          (else (all-placeholders? (cdr identifiers)))
        )
      )
    )
  )

  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (var body env)
      (lambda (val)
        (value-of body (extend-env var val env)))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc val)
      (proc val)))

  )
