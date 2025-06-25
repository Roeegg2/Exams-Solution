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
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (enum-exp (ids)
          (if (null? ids)
            (eopl:error 'enum "Can't create empty enum.")
            (enum-val ids)
          )
        )

        (enum-elmt-exp (enm id)
          (let ((ids (expval->enum (value-of enm env))))
            (if (is-member id ids)
              (enum-elmt-val id)
              (eopl:error 'enum "id is not part of the given enum."))))

        (match-exp (enmid exp1 enmids exps)
          (let (
            (ids (expval->enum (apply-env env enmid)))
            (expval (expval->enum-elmt (value-of exp1 env)))
          )
            (if (is-member expval ids)
              (cond
                ((equal? enmids ids)
                  (let ((match (find-match expval ids exps)))
                    (value-of match env)))
                ((subset enmids ids)
                  (eopl:error 'enum "Not all ids are defined"))
                (else (eopl:error 'enum "Some element in the given expression are not defined in the original env")))
              (eopl:error 'enum "id is not part of the given enum.")
            )
          ))
          


        )))

  (define (is-member id ids)
    (define (recurse ids)
      (if (null? ids)
        #f
        (if (equal? id (car ids))
          #t
          (recurse (cdr ids)))))
    (recurse ids))

  (define (subset lst1 lst2)
    (define (recurse lst1 lst2)
      (if (null? lst1)
        #t
        (if (is-member (car lst1) lst2)
          (recurse (cdr lst1) lst2)
          #f)))
    (recurse lst1 lst2))

  (define (zip lst1 lst2)
    (define (recurse lst1 lst2 result)
      (if (or (null? lst1) (null? lst2))
        result
        (recurse (cdr lst1) (cdr lst2) 
          (cons (cons (car lst1) (car lst2)) result))))

    (recurse lst1 lst2 '()))


  (define (find-match exp1 ids exps)
    (define (recurse exp1 zipped)
      (cond 
        ((equal? exp1 (car (car zipped))) (cdr (car zipped)))
        (else 
          (recurse exp1 (cdr zipped))
        )
      )
    )

    (recurse exp1 (zip ids exps))
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
