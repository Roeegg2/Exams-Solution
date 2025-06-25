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
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (id1 id2 def body)
          (let* ((defval (value-of def env))
                 (new-env (extend-env id2 defval env)))
            (proc-val (procedure id1 id2 body new-env))
          ))

        (call-exp (exp1 exps)
          (cond 
            ((> (length exps) 2) (eopl:error 'call "Too many expressions for proc invoking"))
            ((= (length exps) 0) (eopl:error 'call "Too few expressions for proc invoking"))
            ((= (length exps) 1) 
              (let ((proc (expval->proc (value-of exp1 env)))
                (arg (value-of (car exps) env)))
                (apply-procedure proc arg '())))
            (else 
              (let ((proc (expval->proc (value-of exp1 env)))
                    (arg1 (value-of (car exps) env))
                    (arg2 (value-of (cadr exps) env)))
                (apply-procedure proc arg1 arg2))
            )
            )
              
        )

          

        )))


  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (var1 var2 body env)
      (lambda (val1 val2)
        (let ((env-1 (extend-env var1 val1 env)))
          (if (null? val2)
            (value-of body env-1)
            (value-of body (extend-env var2 val2 env-1))
          )
    ))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc val1 val2)
      (proc val1 val2)))

  )
