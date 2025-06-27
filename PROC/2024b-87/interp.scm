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
        
        (overload-proc-exp (ids bodies)
          (if (null? bodies)
            (eopl:error 'proc "Can't define a procedure without any body.")
            (let* ((zipped (zip ids bodies))
                   (procs (map (lambda (x) (procedure (car x) (cdr x) env)) zipped)))
              (proc-val ids procs)
            )
          )
        )
          ;;(proc-val (procedure var body env)))
          ;; (arg (value-of rand env))
        (call-exp (rator rands)
          (let* ((proc (expval->proc (value-of rator env)))
                (procids (car proc))
                (procs (cdr proc))
                (selected (find-proc procids procs rands))
                (args (map (lambda (x) (value-of x env)) rands)))
              (if (null? selected)
                (eopl:error 'proc "No procedure found for the given arguments.")
                (apply-procedure selected args)
              )
          ))


        )))

  (define zip
    (lambda (l1 l2)
      (if (or (null? l1) (null? l2))
        '()
        (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2))) 
      )
    )
  )

  (define find-proc
    (lambda (ids bodies rands)
      (if (null? ids) 
        '()
        (if (equal? (length (car ids)) (length rands))
          (car bodies)
          (find-proc (cdr ids) (cdr bodies) rands)
        )
      )
    )
  )

  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (vars body env)
      (lambda (vals)
        (value-of body (extend-env* vars vals env)))))

  (define extend-env*
    (lambda (vars vals env)
      (if (null? vars)
        env
        (extend-env* (cdr vars) (cdr vals) (extend-env (car vars) (car vals) env))
      )
    )
  )
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc val)
      (proc val)))

  )
