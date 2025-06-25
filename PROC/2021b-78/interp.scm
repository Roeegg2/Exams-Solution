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
        
        (proc-exp (ids defs body)
          (let ((extended-env (extend-env* ids (map (lambda (x) (value-of x env)) defs) env)))
            (proc-val (procedure ids body extended-env) ids))
        )

        (call-exp (rator parms exps)
          (let* ((proc (expval->proc (value-of rator env)))
                (proc-func (car proc))
                (proc-ids (cdr proc)))
            (if (not (subset parms proc-ids))
              (eopl:error 'params "Unknown arguments for proc")
              (let* ((vals (map (lambda (x) (value-of x env)) exps))
                     (env* (extend-env* parms vals env)))
                      (apply-procedure proc-func env*))
            )
          )
        )
        
    )))

  (define subset
    (lambda (parms ids)
      (if (null? parms)
        #t
        (if (member (car parms) ids)
          (subset (cdr parms) ids)
          #f))))

  (define zip
    (lambda (l1 l2)
      (if (or (null? l1) (null? l2))
        '()
        (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2))))))


  (define extend-env* 
    (lambda (ids defs env)
      (if (or (null? ids) (null? defs)) env
        (extend-env* (cdr ids) (cdr defs) (extend-env (car ids) (car defs) env))
    )
  ))

  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (vars body env)
      (lambda (new-env)
          (value-of body (combine-envs new-env env)))))
  
  (define combine-envs
    (lambda (env1 env2)
      (if (null? env1)
        env2
        (combine-envs (cdr env1)
          (extend-env (car (car env1)) (cadr (car env1)) env2))))
  )
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc env)
      (proc env)))

  )
