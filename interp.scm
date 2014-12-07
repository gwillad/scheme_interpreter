;; Adam Gwilliam
;; Lab 6
;; CS 220
;; Scheme Interpreter

(define global-env (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)
                         (cons 'list list) (cons '< <) (cons '<= <=) (cons '> >)
                         (cons '>= >=) (cons 'equal? equal?) (cons 'list? list?)
                         (cons 'procedure? procedure?) (cons 'cons cons)
                         (cons 'car car) (cons 'cdr cdr) (cons 'null? null?)
                         (cons 'set-cdr! set-cdr!) (cons 'set-car! set-car!)
                         (cons 'append append) (cons 'apply apply)
                         (cons 'display display) (cons 'write write)
                         (cons 'read read) (cons 'number? number?)
                         (cons 'string? string?) (cons 'boolean? boolean?)
                         (cons 'symbol? symbol?) (cons 'assoc assoc)))
;; global environment. most of the included proceedures are so that the
;;interpreter can interpret itself

;; a variety of cddrs and caddrs etc for use in the interpreter (i didn't
;; feel the need to iinclude these in the global environment
(define (caddadr lst) (car (cdr (cdr (car (cdr lst))))))
(define (caar lst) (car (car lst)))
(define (cdar lst) (cdr (car lst)))
(define (cadr lst) (car (cdr lst)))
(define (caddr lst) (car (cdr (cdr lst))))
(define (cadddr lst) (car (cdr (cdr (cdr lst)))))
(define (cadadr lst) (car (cdr (car (cdr lst)))))
(define (cddr lst) (cdr (cdr lst)))


;; used http://www.scheme.com/tspl3/io.html for info on file io
(define (rel input-port) ;;read eval loop on a given input-port
  (let ((expr (read input-port))) ;; read a line
    (if (eof-object? expr) ;; if its an eof object
        (close-input-port input-port) ;; close the port
        (begin ;; otherwise
          (evaluate expr global-env) ;; evaluate the expr
          (rel input-port))))) ;; contintue

(define (load filename)  ;; for loading files
  (begin
    (display "\n Loading ")
    (display filename)
    (rel (open-input-file filename))));;pass the buck to rel after opening file

(define (eval-list lst env) ;; used for begin and let (returns only last elt)
  (if (null? (cdr lst)) ;; if the rest of the list is null
      (evaluate (car lst) env) ;; eval the last elt
      (begin ;; otherwise
        (evalute (car lst) env) ;; eval the first elt
        (eval-list (cdr lst))))) ;; continue

(define (eval-and-return expr env) ;; evals a list and returns all the elts
  (if (null? (cdr expr)) ;; if there's nothing left in the lst
      (cons (evaluate (car expr) env) '()) ;; return a list containing the eval
      ;; of the last elt. 
      (cons (evaluate (car expr) env) (eval-and-return (cdr expr) env))))
      ;; otherwise cons the first elt onto the evaled list of the remaining elts

(define (eval-cond lst env) ;; evaluate a cond expresion
  (if (eqaul? (caar lst) 'else) ;; if the first elt is the else statement
      (evaluate (cdar lst) env) ;; evaluate it
      (if (caar lst) ;; otherwise if the first elt of the list is true
          (evaluate (cdar lst) env) ;;evaluate it and return 
          (eval-cond (cdr lst) env)))) ;; otherwsie keep on keeping on

(define (eval-define expr env) ;; evaluate define expression
  (set-cdr! global-env (cons (car global-env) ;; set the global env cdr to 
                             (cdr global-env))) ;; the current env
  (set-car! global-env (cons (car expr) ;; set the first elt to the new thing.
                             (evaluate (cadr expr) env)))
  (car expr)) ;; return the name of the new thing

(define (eval-func-define expr env) ;;special case of define used for functions
  (eval-define (cons (caar expr) ;; just changes it into a lambda expr
                     (cons (evaluate 
                            (cons 'lambda (cons (cdar expr) 
                                                (cons (cadr expr) 
                                                      '()))) env) '())) env))

(define (eval-if expr env) ;; evaluate if
  (if (< (length expr) 4) ;; if you innapropriately did it
      "Gwill says: incorrect call to 'if' special form" ;; tell you
      (let ((bexpr (cadr expr)) ;; otherwise 
            (then-expr (caddr exp))
            (else-expr (cadddr exp)))
        (if (evaluate bexpr env) ;;evaluate the if
            (evaluate then-expr env)
            (evaluate else-expr env)))))

(define (let-helper let-lst env) ;; adds a let-lst (a list of associations) 
  (if (null? let-lst)            ;; to env
      env
      (cons (cons (caar let-lst) (cdar let-lst)) 
            (let-helper (cdr let-lst) env))))

(define (eval-let expr env) ;; evaluates let
  (eval-list (cdr expr) (let-helper (car expr) env)))
  ;; esseintiall evaluates the contents of the list (with eval-list) 
  ;; with the let expressions added to the environment, 

;; taken from lab 5
(define (zip lst1 lst2) 
  (if (null? lst1) ;;if null
      '() ;; return empty list
      (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))
      )) ;; otherwise prepend the dotted pair of the first two items
;; onto the result of zipping the rest of the list

(define (apply-eval f parms env) ;; apply
  (if (procedure? f) ;;if its a proceedure 
      (apply f parms) ;; pass the buck
      (if (equal? (car f) 'closure) ;; if its a closure
          (evaluate (caddadr f) (append (zip (cadadr f) parms) (cddr f)))
          ;; evaluate the expression with the parms added to the env
          "Incorrect call to function application" ))) ;;otherwise you messed up

(define (evaluate expr env) ;; evaluate
  (cond ((number? expr) expr) ;; if number, return it 
        ((string? expr) expr)
        ((null? expr) expr)
        ((boolean? expr) expr)
        ((char? expr) expr) ;; etc
        ((symbol? expr) ;;if its a symbol
         (if (assoc expr env) ;; and in env
             (cdr (assoc expr env)) ;; return the assoc
             expr)) ;; otherwise return the expr
        ((list? expr) ;; if its a list
         (cond ((equal? 'if (car expr)) ;; if
                (eval-if expr env)) ;; use eval if
               ((equal? 'define (car expr)) ;; define
                (if (list? (cadr expr)) ;; and its a special define
                    (eval-func-define (cdr expr) env) ;; use the function define
                    (eval-define (cdr expr) env))) ;; use the regular define
               ((equal? 'cond (car expr)) ;; cond
                (eval-cond (cdr expr) env));; use eval cond
               ((equal? 'begin (car expr)) ;; begin
                (eval-list (cdr expr) env)) ;; use eval begin
               ((equal? 'let (car expr)) ;; let
                (eval-let (cdr expr) env)) ;; use eval let
               ((equal? 'quote (car expr)) ;; quote
                (cadr expr)) ;; return the expr
               ((equal? 'lambda (car expr)) ;; lambda
                (cons 'closure (cons expr env))) ;; return a closure
               ((equal? 'closure (car expr)) expr) ;;closure, return expr
               ((equal? 'apply (car expr)) ;; apply
                (apply-eval (evaluate (cadr expr) env) ;; evaluate it
                            (eval-and-return (caddr expr) env)))
               (else ;;otherwise its function evaluation
                (apply-eval (evaluate (car expr) env) ;;eval the cra first 
                            (eval-and-return (cdr expr) env) ;; then the rest
                            env))))
        (else ;;otherwise
         (begin
           (display "\nUnable to evaluate ")
           (write expr) ;; you made a mistake. the expr cannot be evaluated
           (display "\nPlease try again")))))

;;taken and changed from lab 5 used for testing.
(define (repl)
  (let ((expr (read)))
    (if (equal? expr 'exit)
        (display "Exiting interpreter. \nSadSadMiseryMisery")
        (begin
          (write (evaluate expr global-env))
          (display "\n")
          (repl)))))

;;(display "\nENTERING GWILL'S INTERPRETER\n")
