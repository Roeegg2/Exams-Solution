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
        
        (call-exp (rator rands)
          (let* ((overload-proc (expval->overload-proc (value-of rator env)))
                (ids          (car overload-proc))
                (bodies       (cadr overload-proc))
                (zipped       (zip ids bodies '()))
                (args         (map (lambda (x) (value-of x env)) rands))
                (matched-pair (get-overload-proc zipped (length rands)))
                (extended-env (extend-env* (car matched-pair) args env)))
            
            (value-of (cdr matched-pair) extended-env)))

        ;; (proc-exp (var body)
        ;;  (proc-val (procedure var body env)))

        (overload-proc-exp (ids bodies)
          (overload-proc-val ids bodies))

        )))


  (define zip
    (lambda (lst1 lst2 result)
      (if (or (null? lst1) (null? lst2)) result
        (zip (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) result))
      )
    )
  )

  (define extend-env*
    (lambda (params args env)
      (if (null? params)
          env
          (extend-env* (cdr params) (cdr args)
                      (extend-env (car params) (car args) env)))))

  (define get-overload-proc 
    (lambda (zipped rands-length)
      (cond
        ((null? zipped)
        (eopl:error 'get-overload-proc "No matching overload found"))
        ((equal? (length (car (car zipped))) rands-length)
        (car zipped)) ; return the (param-list . body) pair
        (else
        (get-overload-proc (cdr zipped) rands-length)))))


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
