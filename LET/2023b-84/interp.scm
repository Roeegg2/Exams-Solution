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

  (define env-contains?
    (lambda (env var)
      (if (null? env)
        #f
        (if (eq? (car (car env)) var) 
          #t
          (env-contains? (cdr env) var))
    )))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) 
          (if (env-contains? env var)
            (apply-env env var)
            (excp-val (Exception "environment"))))


        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (if (or (excp-val? val1) (excp-val? val2))
              (if (excp-val? val1)
                val1
                val2)
              (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))

                  (cond
                    ((and (number? num1) (number? num2)) (num-val (- num1 num2)))
                    ((number? num1) num2)
                    (else num1)
                  )
              ))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (if (excp-val? val1)
              val1
              (let ((num1 (expval->num val1)))
                (if (zero? num1)
                  (bool-val #t)
                  (bool-val #f))))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (excp-val? val1)
              val1
              (let ((bool-val (expval->bool val1)))
                (if (excp-val? bool-val)
                  bool-val
                  (if bool-val
                    (value-of exp2 env)
                    (value-of exp3 env))
                ))
                )))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (if (excp-val? val1)
              val1
              (value-of body
              (extend-env var val1 env)))))

        (throw-exp (excpt)
          (let ((content (symbol->string excpt)))
            (excp-val (Exception content))))
        
        (try-exp (exp1 excpts excptexps finexps) 
          (if (> (length finexps) 1) 
            (eopl:error 'try-exp "More than one finally expressions provided")
            (let ((value (value-of exp1 env)))
              (begin
                (when (excp-val? value)
                  (let* (
                    (zipped (reverse (zip excpts excptexps)))
                    (match (find-match zipped (expval->excp value))))

                    (if (not (null? match))
                      (value-of match env)

                      (when (not (null? finexps))
                        (let ((finalvalue (value-of (car finexps) env)))
                          value
                        )
                      )
                    )))


              )
             
              ))
  

        )))


  )

  (define (zip lst1 lst2)
    (define (recurse lst1 lst2 result)
      (if (or (null? lst1) (null? lst2))
        result
        (recurse (cdr lst1) (cdr lst2) (cons (cons (car lst1) (car lst2)) result))
    ))
    
    (recurse lst1 lst2 '())
  )

  (define (find-match zipped excp-name)
    (if (null? zipped)
      '()
      (if (equal? (symbol->string (car (car zipped))) excp-name)
        (begin
          (cdr (car zipped))
        )
        (find-match (cdr zipped) excp-name)
      )
    )
  )

)
