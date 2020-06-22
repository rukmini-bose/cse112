#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.13 2020-01-10 12:51:12-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is then executed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
        (if (not (input-port? inputfile))
            (die `(,*run-file* ": " ,filename ": open failed"))
            (let ((program (read inputfile)))
                (close-input-port inputfile)
                program))))

(define (interpret-program lines)
    ; Short-circuit if `lines` is empty 
    (unless (null? lines)
        ; Read first line
        (let ([line (car lines)])
            ; Is `line` just a line number?
            (if (null? (cdr line)) 
                ; Then skip line
                (interpret-program (cdr lines))
                ; Otherwise, identify node containing statement
                ; (might skip a label).
                (let ([stmnt-loc (if (symbol? (cadr line))
                                    (cddr line)
                                    (cdr line))])
                    ; Is that node empty?
                    (if (null? stmnt-loc)
                        ; Then skip line (tail)
                        (interpret-program (cdr lines))
                        ; Otherwise extract linenr, statemnt type/args
                        (let* ([lnr (car line)]
                                [stmnt (car stmnt-loc)]
                                [st (car stmnt)] ; type
                                [mnt (cdr stmnt)] ; args
                        ; Delegate to appropriate interpret- function.
                                [int-res
                                    (cond [(equal? st 'dim)
                                            (interpret-dim
                                                (cdar mnt) lnr)]
                                        [(equal? st 'let)
                                            (interpret-let mnt lnr)]
                                        [(equal? st 'goto)
                                            (interpret-goto (car mnt)
                                                lnr)]
                                        [(equal? st 'if)
                                            (interpret-if mnt lnr)]
                                        [(equal? st 'print)
                                            (interpret-print mnt lnr)]
                                        [(equal? st 'input)
                                            (interpret-input mnt lnr)]
                                        [else (die
                                                `("Invalid statement \""
                                                    ,st "\" at line "
                                                    ,(car line))
                                            )])])
                            ; If interpret- returns null
                            (if (null? int-res)
                                ; Then continue to next line (tail call)
                                (interpret-program (cdr lines))
                                ; Otherwise, jump to returned line
                                (interpret-program int-res)))))))))

(define (interpret-dim arr lnr)
    (hash-set! *array-table* (car arr)
        (make-vector (exact-round
                (evaluate-expression (cadr arr) lnr))))
    null
)

(define (interpret-let stmnt lnr)
    (let ([var (car stmnt)])
        (if (symbol? var)
            (hash-set!
                *variable-table* var
                (evaluate-expression (cadr stmnt) lnr))
            (update-array! (cadr var) (caddr var) (cadr stmnt) lnr)))
    null
)

(define (interpret-goto label lnr) 
    (if (hash-has-key? *label-table* label)
        (hash-ref *label-table* label)
        (die `("Invalid label \"" ,label "\" at line " ,lnr))
    )
)

(define (interpret-if path lnr)
    (if (evaluate-expression (car path) lnr)
        (interpret-goto (cadr path) lnr) ; (tail call)
        null)
)

(define (interpret-print lst lnr)
    (if (null? lst)
        (begin (printf "~n") null)
        (let ([head (car lst)])
            (if (string? head)
                (begin (display head)
                    (interpret-print (cdr lst) lnr)) ; (tail call)
                (begin (printf " ~a" (evaluate-expression head lnr))
                    (interpret-print (cdr lst) lnr)))))) ; (tail call)

(define (interpret-input mems lnr)
    (define (assign mem val)
        (cond [(symbol? mem) (hash-set! *variable-table* mem val)]
            [(pair? mem)
                (update-array! (cadr mem) (caddr mem) val lnr)]))
    (if (null? mems)
        null
        (let ([mem (car mems)] [val (read)])
            (if (number? val)
                (begin (assign mem (+ 0.0 val))
                    (interpret-input (cdr mems) lnr))
                (if (eof-object? val)
                    (begin (hash-set! *variable-table* 'eof 1.0)
                        (assign (car mems) (/ 0.0 0.0))
                        (interpret-input (cdr mems) lnr))
                    (begin (assign (car mems) (/ 0.0 0.0))
                        (interpret-input (cdr mems) lnr)))))))
 
(define (read-array a expr lnr)
    (if (hash-has-key? *array-table* a)
        (let* ([arr (hash-ref *array-table* a)]
                [alen (vector-length arr)]
                [idx (exact-round (evaluate-expression expr lnr))])
            (if (and (>= idx 0) (< idx alen))
                (vector-ref arr idx)
                (die `("Array index " ,idx
                    " out of bounds at line " ,lnr)
                )))
        (die `("Invalid array: " ,a))
    ))

(define (update-array! a idx-expr val-expr lnr)
    (if (hash-has-key? *array-table* a)
        (let* ([arr (hash-ref *array-table* a)]
                [alen (vector-length arr)]
                [idx (exact-round (evaluate-expression idx-expr lnr))])
            (if (and (>= idx 0) (< idx alen))
                (vector-set! arr idx (evaluate-expression val-expr lnr))
                (die `("Array index " ,idx
                    " out of bounds at line " ,lnr)
                )))
        (die `("Invalid array: " ,a))
    ))

(define (evaluate-expression expr lnr)
    (cond [(number? expr) (+ expr 0.0)] ; constant case
        [(symbol? expr) (hash-ref *variable-table* expr 0.0)] ; variable
        [pair? expr
            (let ([name (car expr)])
                ; If 'asub spotted
                (if (equal? name 'asub)
                    ; Then array
                    (read-array (cadr expr) (caddr expr) lnr)
                    ; Otherwise function
                    (if (hash-has-key? *function-table* name)
                        (apply (hash-ref *function-table* name)
                            ; Curry to enable lnr passing
                            (map (lambda (arg)
                                    (evaluate-expression arg lnr))
                                (cdr expr)))
                        (die `("Invalid function \""
                                ,name "\" at line " ,lnr)
                        ))))]
        ; Probably can't get here, but just in case...
        [else (die `("Invalid expression " ,expr " at line " ,lnr)
            )]))

;; Maps each function/operator name to its corresponding procedure
(define *function-table* (make-hash))
; Statically initialize *function-table* from a list of pairs.
(for-each
    (lambda (pair) (hash-set! *function-table* (car pair) (cdr pair)))
    `(
        (+ .            ,+)
        (- .            ,-)
        (* .            ,*)
        (/ .            ,/)
        (^ .            ,expt)
        (= .            ,equal?)
        (< .            ,<)
        (> .            ,>)
        (!= .           ,(lambda (x y) (not (equal? x y))))
        (>= .           ,>=)
        (<= .           ,<=)
        (abs .          ,abs)
        (acos .         ,acos)
        (asin .         ,asin)
        (atan .         ,atan)
        (ceiling .      ,ceiling)
        (cos .          ,cos)
        (exp .          ,exp)
        (floor .        ,floor)
        (log .          ,log)
        (round .        ,round)
        (sin .          ,sin)
        (sqrt .         ,sqrt)
        (tan .          ,tan)
        (truncate .     ,truncate))
)

;; Maps each (non-array) variable name to its value
(define *variable-table* (make-hash))
;; Statically prepopulate *variable-table* from a list of pairs.
(for-each
    (lambda (pair) (hash-set! *variable-table* (car pair) (cdr pair)))
    `(
        (nan .  ,(/ 0.0 0.0))
        (eof .  ,0.0)
        (pi .   ,(acos -1.0))
        (e .    ,(exp 1.0)))
)

;; Maps each array name to its values
(define *array-table* (make-hash))

;; Maps each label to its corresponding node in the list of lines
(define *label-table* (make-hash))

(define (main arglist)
    ;; Procedure to populate *label-table*
    (define (gen-labels line)
        (unless (null? line)
            (unless (or (null? (cdar line))
                        (not (symbol? (cadar line))))
                (hash-set! *label-table* (cadar line) line)
            )
            (gen-labels (cdr line))
        )
    )
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ([sbprogfile (car arglist)]
                [program (readlist-from-inputfile sbprogfile)] 
            )
            (gen-labels program) ; Populate `*label-table*`
            (interpret-program program)))) ; Leap of faith :)

(main (vector->list (current-command-line-arguments)))