(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (begin
                (printf "1 in diff: ~a \n env: ~a \n" num1 (env->list env))
                (printf "2 in diff: ~a\n" num2)
              
              (num-val
                (- num1 num2))))))
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
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env #f)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        (apply-exp (id1 id2 expression)
          (let* ((old-proc-val (apply-env env id2))
                 (old-proc (expval->proc (deref old-proc-val))))
            (cases proc old-proc
              (procedure (var body saved-env is-applied)
                (let* ((new-env (extend-env var (apply-env env id1) env))
                       (new-proc (proc-val (procedure id1 body new-env #t)))
                       (final-env (extend-env id2 (newref new-proc) new-env)))
              
                    (begin
                     (printf "Env of running: ~a\n" (env->list final-env))
                     (value-of expression final-env)
                     (printf "value now of ->~a is ~a \n" var (deref (apply-env env var)))
                    )
                )
              )
            )
            ;; 1. We need to activate the value-of body with an environment in which x has a value of the pointer A, where A is the (apply-env env id1) value. 
          )
        )

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env is-applied)
          (begin
            (printf "Running proc with var: ~a \n" var)
          
            (if is-applied
              (let ((r (newref arg)))
                  (begin 
                  (printf "going to change ~a \n" (apply-env saved-env var))

                  (setref! 
                      (apply-env saved-env var)
                      arg
                    )
                  (printf "value now of ->~a is ~a \n" var arg)
                  (printf "running with env: ~a \n" (env->list saved-env))
                    
                    (value-of body saved-env)
                    (setref! 
                      (apply-env saved-env var)
                      (deref (apply-env saved-env var))
                    )
                  )
                )

                (let ((r (newref arg)))
              (let ((new-env (extend-env var r saved-env)))
                (value-of body new-env)))

            )
          )
        )
      )
    )
  )

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
