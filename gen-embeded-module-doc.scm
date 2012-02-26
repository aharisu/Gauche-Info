(define default-modules (all-modules))

(use srfi-1)
(use srfi-13)
(use file.util)
(use ginfo)
(use ginfo.revert)

(use gauche.interactive)

(define (valid-exports exports)
  (filter
    (lambda (sym)
      (let1 first-ch (string-ref (symbol->string sym) 0)
        (not (or (eq? first-ch #\*)
               (eq? first-ch #\%)
               (char-upper-case? first-ch)))))
    exports))


(define (add-remove-supecial-module module-map)
  (define (get-module-exports m)
    (cons
      m
      (module-exports (find-module m))))
  (append
    (remove
      (lambda (m) (eq? (car m) 'user))
      module-map)
    (filter
      identity
      (append-map
        (lambda (s)
          (library-map 
            s 
            (lambda (m p) 
              (guard (e [else #f])
                (eval `(require ,(module-name->path m)) 'gauche)
                (cons
                  m
                  (module-exports (find-module m)))))))
        '(* *.* *.*.* *.*.*.* *.*.*.*.*)))))

(define (parent-module? module parent)
  (any
    (pa$ eq? parent)
    (map
      module-name
      (module-precedence-list (find-module module)))))

(define (module-map-to-hash-table module-map)
  (let1 table (make-hash-table)
    (for-each (lambda (m)
        (for-each
          (lambda (s)
            (unless (and (hash-table-exists? table s)
                      (parent-module? (car m) (hash-table-get table s)))
              (hash-table-put! table s (car m))))
          (cdr m)))
      module-map)
    table))

(define symbol-table 
  (module-map-to-hash-table
    (add-remove-supecial-module 
      (filter-map 
        (lambda (m)
          (let ([path (library-fold (module-name m) (lambda (m p a) p) #f)]
                [exports (hash-table-keys (module-table m))])
            (if (or path (null? exports))
              #f
              (cons (module-name m) (valid-exports exports)))))
        default-modules))))

#;(for-each
  print
  (hash-table-values symbol-table))


;;
;;parse texi
;;
(define (map-category-to-type category)
  (case (string->symbol category)
    [(|Builtin Class| Metaclass Class |Condition Type|) type-class]
    [(|Generic Function| Method) type-method]
    [(Parameter) type-parameter]
    [else #f]))

(define (valid-name? name) (not (eq? #\{ (string-ref name 0))))

(define (guarded-read :optional (port (current-input-port)))
  (guard (exc [(<read-error> exc) (guarded-read port)])
    (read port)))

(define (split-args args)
  (with-input-from-string
    (regexp-replace-all* args
                         #/@var{(\S*)}/ (cut <> 1)
                         #/@dots{}/ "...")
    (pa$ port-map 
         (lambda (x)
           (if (keyword? x)
             (string-append ":" (keyword->string x))
             (x->string x)))
         guarded-read)))

(define (split-category-and-args args)
  (let1 match (rxmatch #/^{(.+)}\s+(.*)\)?$/ args)
    (values (match 1) (split-args (match 2)))))


(define (add-params unit args)
  (for-each
    (lambda (arg)
      (add-unit-param! unit arg))
    args)
  unit)

(define (cmd-to-unit cmd args)
  (case (string->symbol cmd)
    [(@deffn @deffnx)
     (receive (category args) (split-category-and-args args) 
       (let1 type (map-category-to-type category)
         (if type
           (add-params (make <unit-proc> :type type :name (car args))
                       (cdr args))
           #f)))]
    [(@defun @defunx) 
     (let1 args (split-args args)
       (if (valid-name? (car args))
         (add-params (make <unit-proc> :name (car args))
                     (cdr args))
         #f))]
    [(@defspec @defspecx)
     (let1 args (split-args args)
       (add-params (make <unit-proc> :name (car args))
                   (cdr args)))] ;;fixme :type syntax
    [(@defmac @defmacx)
     (let1 args (split-args args)
       (add-params (make <unit-proc> :name (car args))
                   (cdr args)))] ;;fixme :type macro
    [(@deftp @deftpx) 
     (receive (category args) (split-category-and-args args) 
       (let1 type (map-category-to-type category)
         (if type
           (make <unit-class> :name (car args))
           #f)))]
    [(@defvar @defvarx) 
     (let1 args (split-args args)
       (make <unit-var> :name (car args)))]
    [else #f]))


(define-constant modsrfi-allow-symbol 
  '("require-extension"
    "every"
    "any"
    "find"
    "fold"
    "split-at"
    "null-list?"
    "dotted-list?"
    "circular-list?"
    "proper-list?"
    "cond-expand"
    "length+"))

(define (parse-all-line path filename)
  (filter 
    identity
    (with-input-from-file 
      path 
      (pa$ port-map
           (lambda (line)
             (let1 match (rxmatch #/^(@[[:alpha:]]+)\s+(.*)/ line)
               (if match
                 (let1 u (cmd-to-unit (match 1) (match 2))
                   (if (and u (string=? filename "modsrfi.texi"))
                     (if (any (pa$ string=? (ref u 'name)) modsrfi-allow-symbol)
                       u
                       #f)
                     u))
                 #f)))
           read-line))))

(define docs
  (map
    (lambda (file) (parse-all-line file file))
    (directory-list "doc/texi" :add-path? #t :children? #t
                    :filter (lambda (e) (string-suffix? ".texi" e)))))

(define doc-table (make-hash-table))
(for-each
  (cut
    for-each
    (lambda (unit)
      (when (hash-table-exists? symbol-table (string->symbol (ref unit 'name)))
        (let1 module (hash-table-get symbol-table (string->symbol (ref unit 'name)))
          (if (hash-table-exists? doc-table module)
            (hash-table-update! doc-table module (lambda (doc) (add-unit doc unit) doc))
            (let1 doc (make <doc>)
              (add-unit doc unit)
              (hash-table-put! doc-table module doc))))))
    <>)
  docs)

#;(hash-table-for-each
  doc-table
  (lambda (key entry)
    (print key ":" entry)))

(define file-header "
;;;;;
;;@type cmd
;;@@parse-relative #f

  ")

(hash-table-for-each
  doc-table
  (lambda (key entry)
    (with-output-to-file
      (build-path "doc/scm/" (x->string key))
      (lambda ()
        (print file-header)
        (revert-doc entry (current-output-port))))))

