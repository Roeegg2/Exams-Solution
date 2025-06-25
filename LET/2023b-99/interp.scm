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
        (var-exp (var) 
          (apply-env env var) ; apply-env is defined in environments.scm
        )

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (begin
              (num-val
                (- num1 num2))))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)
              ))))

          
              
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

        (do-exp (ids inits steps bools results)
          (when (is-valid ids inits bools)
            (main-recursive
              ids inits steps bools results (refresh-env ids inits env))
        ))


        )))

      (define main-recursive
        (lambda (ids inits steps bools results env)
          (let ((success? (search-for-match ids inits bools results env)))
            (if (null? success?)
              (let* (
                (zipped (zip ids steps))
                (new-vals (map (lambda (pair)
                        (let ((id (car pair))
                              (step (cdr pair)))
                            
                            (let* (
                              (val (expval->num (apply-env env id)))
                              (step-val (expval->num (value-of step env)))
                              (new-val (+ val step-val)))
                                (const-exp new-val)

                              )))
                    zipped)))

                  (main-recursive 
                    ids inits steps bools results
                    (refresh-env ids new-vals env)))

              (begin
                
                success?
              )
            )
          )
      ))

      (define search-for-match
        (lambda (ids inits bools results env)
          (let* (
            (extended-env (refresh-env ids inits env))
            (zipped (zip bools results))
            (match (find-match zipped env)))
              (if (not (null? match))
                match
                '())
            ))
        )

      (define zip
        (lambda (bools results)
          (if (null? bools)
            '()
            (cons (cons (car bools) (car results))
                  (zip (cdr bools) (cdr results))))))

      (define find-match
        (lambda (zipped env)
          (if (null? zipped)
           '()
            (let* ((val (value-of (car (car zipped)) env))
                  (bool-val (expval->bool val)))
                (begin
                  (if bool-val 
                    (value-of (cdr (car zipped)) env)
                    (find-match (cdr zipped) env))
                )
                
            )
          )
        )
      )

      (define refresh-env
        (lambda (ids values env)
          (if (null? ids)
            env
            (let ((val (value-of (car values) env)))
              (begin
                (refresh-env 
                  (cdr ids) 
                  (cdr values) 
                  (extend-env (car ids) val env)))))))

  (define is-valid
    (lambda (ids inits bools)
      (if (null? ids)
        (eopl:error 'do "No ids provided for do expression")
        (if (null? inits)
          (eopl:error 'do "No inits provided for do expression")
          (if (null? bools)
            (eopl:error 'do "No bools provided for do expression")
            (if (not (= (length ids) (length inits)))
              (eopl:error 'do "Number of ids and inits do not match")
              #t)))
      )
    )  
  )


  )

