(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for let-lang.

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (Exception
      (value string?))
    (excp-val
      (exception Exception?))
    )



    (define Exception?
      (lambda (x)
        (cases expval x
          (Exception (value) #t)
          (else #f)
        )
      )
    )

  (define excp-val?
    (lambda (v)
      (cases expval v
          (excp-val (excp) #t)
          (else #f))))

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (excp-val (Exception "not a number"))))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (excp-val (Exception "not a boolean"))))))

  (define expval->Exception
    (lambda (v)
      (cases expval v
        (Exception (value) value)
        (else (eopl:error 'error "Not an exception")))))

  (define expval->excp
    (lambda (v)
      (cases expval v
        (excp-val (Exception) (expval->Exception Exception))
        (else (eopl:error 'error "Not an exception")))))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

)
