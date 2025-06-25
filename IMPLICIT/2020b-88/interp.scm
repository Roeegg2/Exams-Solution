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
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (dict-exp (keys values)
          (let* ((keys1 (map (lambda (v) (value-of v env)) keys))
                (values1 (map (lambda (v) (value-of v env)) values)))
            (if (set? keys1) 
              (dict-val keys1 values1)
              (eopl:error 'dict "Can't create dict with duplicates keys")
            )
          )
        )

        (dictKey-exp (dict key)
          (let* ((ref (apply-env env dict))
                 (dictval (expval->dict (deref ref)))
                 (dict-keys (car dictval))
                 (dict-values (cdr dictval)))
            (if (member (value-of key env) dict-keys)
              (find-in-dict dict-keys dict-values (value-of key env))
              (eopl:error 'dict "Provided key is not a key in the dict.")
            )
          )
           
        )

        (dictAdd-exp (dict key value)
          (let* ((ref (apply-env env dict))
                 (dictval (expval->dict (deref ref)))
                 (dict-keys (car dictval))
                 (dict-values (cdr dictval)))
            (if (member key dict-keys)
              (eopl:error 'dict "Provided key already exists in the dict.")
              (let ((new-dict (dict-val
                                (append dict-keys (list (value-of key env)))
                                (append dict-values (list (value-of value env))))))
              (begin
                (setref! 
                  ref
                  new-dict
                )
                (num-val 27) 
              )
            )
          )
        ))

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

        )))


  (define find-in-dict
    (lambda (keys values key)
      (if (null? keys) 
        (eopl:error 'dict "Can't find key")
        (if (equal? (car keys) key)
          (car values)
          (find-in-dict (cdr keys) (cdr values) key)
        )
      )
    )
  )


  (define set?
    (lambda (keys)
      (let loop ((ks keys) (seen '()))
        (cond
          ((null? ks) #t)
          ((member (car ks) seen) #f)
          (else (loop (cdr ks) (cons (car ks) seen)))))))

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
        (procedure (var body saved-env)
          (let ((r (newref arg)))
            (let ((new-env (extend-env var r saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    var)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))  

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
  


  
