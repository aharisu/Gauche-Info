(define-module geninfo
  (use srfi-11) ;;let-values
  (use srfi-13) ;;string util
  (use util.list)
  (use util.match)
  (use text.parse)
  (use gauche.interactive)
  ;(export <doc> <geninfo-warning>
  ;				<unit-top>
  ;				api geninfo)
  (export-all))

(select-module geninfo)

(define e->e (lambda (e) e))

;-------***************-----------
;data structure
;-------***************-----------

;;解析中に遭遇した例外を表すコンディションタイプ
(define-condition-type <geninfo-warning> <message-condition> #f)


;;一つのドキュメントを表すクラス
(define-class <doc> ()
  (
   (units :init-value '())
   ))

(define (add-unit doc unit)
  (slot-set! doc 'units (cons unit (slot-ref doc 'units)))
  unit)

(define (commit-doc doc)
  (slot-set! doc 'units (reverse (slot-ref doc 'units)))
  doc)


;;全てのドキュメントユニットの上位クラス
(define-class <unit-top> ()
  (
   (cur-tag :init-value 'description)

   (name :init-value #f)
   (type :init-value #f)

   (description :init-value '())
   ))

;;<unit-bottom>の各スロットから値をコピーする
;;これ以降unitを編集することはない
(define-method commit ((unit <unit-top>) original)
 (slot-set! unit 'name (slot-ref original 'name))
 (slot-set! unit 'type (slot-ref original 'type))
 (slot-set! unit 'description (reverse (slot-ref original 'description)))
 unit)


(define-method show ((unit <unit-top>))
  (format #t "type: ~S\n" (slot-ref unit 'type))
  (format #t "name: ~S\n" (slot-ref unit 'name))
  (format #t "description: ~S\n" (slot-ref unit 'description)))

;;ユニットのapi情報を標準出力に出力する
(define-method show-api ((unit <unit-top>))
  (format #t "API are\n")
  (format #t "  type        : ~a\n" (ref unit 'type))
  (format #t "  name        : ~a\n" (ref unit 'name))
  (unless (null? (ref unit 'description))
    (format #t "  description : ~a\n" (string-join (ref unit 'description) "\n                ")))
  )




;;unit-bottomクラスのためのメタクラス
;;全てのユニットクラスの下位クラスとして扱えるようにする
(define-class <unit-bottom-meta> (<class>) ())

;;スロットが見つからなかった場合はハッシュテーブル内からget or setする
(define-method slot-missing ((class <unit-bottom-meta>) obj slot . value)
  (if (null? value)
    (cond ;get
      [(hash-table-get (slot-ref obj '%slots) slot #f) => e->e]
      [else (next-method)])
    (begin  ;set
      (hash-table-put! (slot-ref obj '%slots) slot (car value))
      (undefined))))

;;スロットに対する更新関数
(define (slot-update! obj slot f)
  (cond
    [(find (lambda (s) (eq? (car s) slot)) (class-slots (class-of obj)))
     (slot-set! obj slot (f (slot-ref obj slot)))]
    [else (hash-table-update! (slot-ref obj '%slots) slot f)]))

;;全てのドキュメントユニットの下位クラス
;;存在しないスロットもハッシュテーブルとして持つことで疑似的に下位クラスのように扱う
;;method not support
(define-class <unit-bottom> (<unit-top>)
  (
   (%slots :init-value (make-hash-table))
   )
  :metaclass <unit-bottom-meta>)

;;unit-bottom初期化用関数
;;unit-bottomの疑似スロットに対してmake時に初期化する必要があるものはこのタイミングで行う
(define unit-bottom-initializer '())
(define (unit-bottom-initializer-add! proc)
  (set! unit-bottom-initializer (cons proc unit-bottom-initializer)))

(define-method initialize ((class <unit-bottom>) initargs)
  (let1 ret (next-method)
        (for-each
          (lambda (proc) (proc ret initargs))
          unit-bottom-initializer)
        ret))


;;functionやmethodタイプ用のunit
(define-class <unit-proc> (<unit-top>)
  (
   (param :init-value '())
   (return :init-value '())
   ))

(define-method commit ((unit <unit-proc>) original)
  (next-method)
  (slot-set! unit 'description (reverse (slot-ref original 'description)))
  (slot-set! unit 'return (reverse (slot-ref original 'return)))
  ;;この時点で(hidden new text1 text2 ...)のリスト構造から(new text1 text2 ...)のリスト構造に修正する
  ;;hiddenとnewは自動解析結果の引数名を手動で修正するためにある
  (slot-set! unit 'param (map 
                           (lambda (p)(cons (cadr p) (reverse (cddr p))))
                           (reverse (slot-ref original 'param))))
  unit)

(unit-bottom-initializer-add! 
  (lambda (unit initargs)
    (slot-set! unit 'param '())
    (slot-set! unit 'return '())))

(define-method show ((unit <unit-proc>))
  (next-method)
  (format #t "param: ~S\n" (slot-ref unit 'param))
  (format #t "return: ~S\n" (slot-ref unit 'return)))

(define-method show-api ((unit <unit-proc>))
  (next-method)
  (unless (null? (ref unit 'param))
    (begin
      (format #t "  param       : ~a\n" (fold-right
                                          (lambda (p acc) (string-append (car p) " " acc))
                                          ""
                                          (slot-ref unit 'param)))
      (for-each
        (lambda (p) 
          (unless (null? (cdr p))
            (format #t "- param#~a : ~a\n" (car p) (string-join (cdr p) " "))))
        (slot-ref unit 'param))))
  (unless (null? (ref unit 'return))
    (format #t "  return      : ~a\n" (string-join (ref unit 'return) "\n                ")))
  )

;;varやconstantタイプ用のunit
(define-class <unit-var> (<unit-top>) () )


;;classタイプ用のunit
(define-class <unit-class> (<unit-top>)
  (
   (supers :init-value '())
   (slots :init-value '())
   )
  )

(define-method commit ((unit <unit-class>) original)
  (next-method)
  (slot-set! unit 'supers (slot-ref original 'supers))
  (slot-set! unit 'slots (map
                           (lambda (s) (list (car s) (cadr s) (reverse (caddr s))))
                           (slot-ref original 'slots)))
  unit)

(unit-bottom-initializer-add!
  (lambda (unit initargs)
    (slot-set! unit 'supers '())
    (slot-set! unit 'slots '())))

(define-method show ((unit <unit-class>))
  (next-method)
  (format #t "supers: ~S\n" (slot-ref unit 'supers))
  (format #t "slots: ~S\n" (slot-ref unit 'slots)))

(define-method show-api ((unit <unit-class>))
  (next-method)
  (unless (null? (slot-ref unit 'supers))
    (format #t "  supers      : ~a\n" (string-join (ref unit 'supers) " ")))
  (for-each
    (lambda (s)
      (format #t "  slot        : ~a\n" (string-append (car s) " " (cadr s)))
      (unless (null? (caddr s))
        (format #t "    ~a\n" (string-join (caddr s) "\n    "))))
    (slot-ref unit 'slots))
  )


;;unit-bottomからtypeにあったunitクラスに変換する
(define-macro (or-equal? x . any)
              `(or ,@(map (lambda (y) `(equal? ,x ,y)) any)))
(define (spcify-unit type unit)
  (commit (cond
            [(or-equal? type type-fn type-method) (make <unit-proc> unit)]
            [(or-equal? type type-var type-const type-parameter) (make <unit-var> unit)]
            [(or-equal? type type-class) (make <unit-class> unit)]
            [else (raise (condition (<geninfo-warning> (message "unkown document unit type"))))]) ;TODO warning
          unit))


;;unitのプロパティへのアクセサ関数群
(define (get-tag unit) (slot-ref unit 'cur-tag))
(define (set-tag tag unit) (slot-set! unit 'cur-tag tag))

(define (set-unit-name name unit)
  (if (and ((get-allow-multiple "name") unit) ((get-valid "name") name unit))
    (slot-set! unit 'name name))
  unit)

(define (set-unit-type type unit)
  (if (and ((get-allow-multiple "type") unit) ((get-valid "type") type unit))
    (slot-set! unit 'type type))
  unit)

(define (append-text text unit)
  ((get-appender (get-tag unit)) text unit)
  unit)

(define (valid-unit? unit)
  (and (slot-ref unit 'name) (slot-ref unit 'type)))

(define (get-invalid-unit-reason unit)
  (string-append "invalid reason"
                 (if (slot-ref unit 'name) "" " [no specification of a name]")
                 (if (slot-ref unit 'type) "" " [no specification of a type]")))

(define (commit-unit unit)
  (if (valid-unit? unit)
    (spcify-unit (slot-ref unit 'type) unit)
    (raise (condition
             (<geninfo-warning> (message (get-invalid-unit-reason unit)))))))


;-------***************-----------
;;parse document
;-------***************-----------
(define (x->writable-string x)
  (match x
         [(? string? x) (string-append "\"" x "\"")]
         ;[(symbol? x) (string-append "'" (symbol->string x))]
         [(? keyword? x) (string-append ":" (keyword->string x))]
         [('quote x) (string-append "'" (x->writable-string x))]
         [(? list? x) (string-append "(" 
                                     (string-trim-right
                                       (fold-right
                                         (lambda (x acc) (string-append (x->writable-string x) " " acc))
                                         ""
                                         x))
                                     ")")]
         [x (x->string x)]))

(define tags '())

(define-macro (define-tag name allow-multiple? init valid-text? appender)
  `(set! tags (acons (symbol->string (quote ,name)) 
                     (cons ,allow-multiple? (cons ,init (cons ,valid-text? ,appender)))
                     tags)))

(define (get-allow-multiple tag)
  (cond 
    [(assoc-ref tags (x->string tag)) => car]
    [else #f]))
(define (get-init tag)
  (cond 
    [(assoc-ref tags (x->string tag)) => cadr]
    [else #f]))
(define (get-valid tag)
  (cond
    [(assoc-ref tags (x->string tag)) => caddr]
    [else #f]))
(define (get-appender tag)
  (cond
    [(assoc-ref tags (x->string tag)) => cdddr]
    [else #f]))

(define (tag-allow-multiple-ret-true unit) #t)

(define (tag-init first-line unit) first-line)

(define (tag-valid-ret-true text unit) #t)

(define (tag-append-text tag) 
  (lambda (text unit)
    (slot-update! unit tag (lambda (value) (cons text value)))))

(define (split-first-token line)
  (let* ([port (open-input-string line)]
         [first (read port)])
    (if (eof-object? first)
      #f
      (append (match first
                     [(hidden '>> new) (list (x->writable-string hidden) (x->writable-string new))]
                     [sym (list (x->writable-string sym) (x->writable-string sym))])
              (cons (string-trim (port->string port)) '())))))

;;define @description tag
(define-tag description
            tag-allow-multiple-ret-true
            tag-init
            tag-valid-ret-true
            (tag-append-text 'description))

;;define @param tag
(define-tag param
            tag-allow-multiple-ret-true
            (lambda (first-line unit)
              (cond
                [(split-first-token first-line) 
                 => (lambda (tokens)
                      (slot-update! unit 'param (lambda (v)
                                                  (cons (list (car tokens) (cadr tokens)) v)))
                      (caddr tokens))]
                [else (raise (condition (<geninfo-warning> (message "param name is required"))))]))
            tag-valid-ret-true
            (lambda (text unit)
              (slot-update! unit 'param 
                            (lambda (value)
                              (set-cdr! (cdar value) (cons text (cddar value)))
                              value))))

;;define @return tag
(define-tag return
            (lambda (unit) (null? (slot-ref unit 'return)))
            tag-init
            tag-valid-ret-true
            (tag-append-text 'return))


;;define @slot tag
(define-tag slot
            tag-allow-multiple-ret-true
            (lambda (first-line unit)
              (cond
                [(split-first-token first-line)
                 => (lambda (tokens)
                      (slot-update! unit 'slots (lambda (v) (cons (list (car tokens) "" '()) v)))
                      (caddr tokens))]
                [else (raise (condition (<geninfo-warning> (message "slot name is required"))))]))
            tag-valid-ret-true
            (lambda (text unit)
              (slot-update! unit 'slots
                            (lambda (v)
                              (set-car! (cddr (car v)) (cons text (caddr (car v))))
                              v))))


;;define @name tag
(define-tag name
            (lambda (unit) (not (slot-ref unit 'name)))
            tag-init
            (lambda (text unit) (not (slot-ref unit 'name)))
            (lambda (text unit) (slot-set! unit 'name text)))

;;define @type tag
(define-constant type-fn "Function")
(define-constant type-var "var")
(define-constant type-method "Method")
(define-constant type-const "Constant")
;Parameterは自動解析無理なので指定したいのなら自分で書いてね
(define-constant type-parameter "Parameter")
(define-constant type-class "Class")
(define-constant allow-types 
                 `(
                   ,type-fn
                   ,type-var
                   ,type-method
                   ,type-const
                   ,type-parameter
                   ,type-class
                   ))
(define-tag type
            (lambda (unit) (not (slot-ref unit 'type)))
            tag-init
            (lambda (text unit) 
              (and (not (slot-ref unit 'type))
                   (find (lambda (x) (string=? text x)) allow-types)))
            (lambda (text unit) (slot-set! unit 'type text)))


;;次の有効なドキュメントテキストを取得する
;;有効なドキュメントテキストがなければ#fを返す
(define (next-doc-text)
  (if (not (zero? (string-length (string-trim (next-token-of '(#\space #\tab #\;))))))
    (let ([line (read-line)])
      (if (zero? (string-length line))
        (next-doc-text) ;; read next line
        line))
    #f))

;;ドキュメントタグと本文を分解する
;;example: "@return hoge piyo" -> {"return" "hoge piyo"}
(define (split-tag-and-text token)
  (cond [(string-scan token #\space)
         => (lambda (index)
              (values
                (substring token 1 index)
                (substring token (+ index 1) (string-length token))))]
        [else (values
                (substring token 1 (string-length token))
                "")]))

;;次のタグかドキュメントの終わりまで本文のテキストをスキップする
(define (skip-current-tag)
  (let ([org-fp (port-seek (current-input-port) 0 SEEK_CUR)])
    (if (not (zero? (string-length (string-trim (next-token-of '(#\space #\tab #\;))))))
      (let ([line (read-line)])
        (if (zero? (string-length line))
          skip-current-tag) ; read next line
        (if (eq? #\@ (string-ref line 0))
          ;;return to origin point file pointer
          (port-seek (current-input-port) 
                     (- org-fp (port-seek (current-input-port) 0 SEEK_CUR))
                     SEEK_CUR)
          (skip-current-tag)))))) ; read next line

;;テキスト内にタグがあれば処理を行う
(define (process-tag text unit)
  (if (eq? #\@ (string-ref text 0))
    (let-values ([(tag text) (split-tag-and-text text)])
                (cond 
                  [(get-allow-multiple tag) 
                   => (lambda (pred) 
                        (if (pred unit)
                          (begin
                            (set-tag (string->symbol tag) unit)
                            ((get-init tag) text unit))
                          #f))]
                  [else (raise (condition
                                 (<geninfo-warning> (message #`"unkwon tag name [,tag]."))))]))
    text))

;;テキストを現在のタグ内に追加する
(define (process-text text unit)
  (if (not (string-null? text))
    (if ((get-valid (get-tag unit)) text unit)
      (append-text text unit)
      (format #t "warning: invalid document text [\"~s\"]." text)
      ))
  unit)

;;一つのドキュメントを最後までパースする
(define (parse-doc unit)
  (cond 
    [(next-doc-text)
     => (lambda (text) 
          (parse-doc (cond 
                       [(process-tag text unit)
                        => (lambda (text) (process-text text unit))]
                       ;;TODO Warning
                       [else (skip-current-tag) unit])))];skip tag text
    [else unit]))


;;------***************-----------
;;analyze define expression
;-------***************-----------

;;仮引数部をマッチングしながら再帰的に解析する
(define (parse-each-arg args func-get-var)
  (let ([unit (make <unit-proc>)]
        [init (get-init 'param)])
    (let loop ([args args])
      (match args
             [(:optional spec ...) 
              (init ":optional" unit)
              (loop spec)]
             [(:key spec ...) 
              (init ":key" unit)
              (loop spec)]
             [(:allow-other-keys spec ...)
              (init ":allow-other-keys" unit)
              (loop spec)]
             [(:rest var spec ...)
              (init ":rest" unit)
              (init (symbol->string (func-get-var var)) unit)
              (loop spec)]
             [(((keyword var) init-exp) spec ...) 
              (init 
                #`"((,(x->writable-string keyword) ,(x->writable-string (func-get-var var))) ,(x->writable-string init-exp))"
                unit)
              (loop spec)]
             [((var init-exp) spec ...) 
              (init #`"(,(symbol->string (func-get-var var)) ,(x->writable-string init-exp))" unit)
              (loop spec)]
             [(var args ...) 
              (init (symbol->string (func-get-var var)) unit)
              (loop args)]
             [() (slot-ref unit 'param)]))))


;;lambda式の仮引数部を解析する
;;さらに手書きparamのドキュメントとマージする
(define (analyze-args args func-get-var unit)
  (let ([org-param (slot-ref unit 'param)]
        [gen-param (parse-each-arg args func-get-var)])
    (for-each
      (lambda (p) 
        (cond
          [(assoc (car p) gen-param)
           => (lambda (param) (set-cdr! param (cdr p)))]
          [else (format #t "warning.")])) ;TODO warging
      org-param)
    (slot-set! unit 'param gen-param)))


;;define, define-constantの解析を行う
(define (analyze-normal-define l unit)
  (let ([constant? (eq? (car l) 'define-constant)])
    (match l
           [(_ (symbol args ...) _ ...) ;; lambda -> function
            (set-unit-name (symbol->string symbol) unit)
            (set-unit-type type-fn unit) 
            (analyze-args args e->e unit)]

           [(_ symbol exp) 
            (set-unit-name (symbol->string symbol) unit)
            (match exp
                   [(or ('lambda (args ...) _ ...) ;; lambda -> function
                        ('^ (args ...) _ ...))
                    (set-unit-type type-fn unit)
                    (analyze-args args e->e unit)]
                   [(or ('lambda arg _ ...) ;; lambda -> function
                        ('^ arg _ ...))
                    (set-unit-type type-fn unit)
                    (analyze-args (list :rest arg) e->e unit)]
                   [else (set-unit-type (if constant? type-const type-var) unit)])];; other -> var or constant

           [(_ symbol) ;; other -> var or constant
            (set-unit-name (symbol->string symbol) unit)
            (set-unit-type (if constant? type-const type-var) unit)]

           [(_) #f])))

;;define-methodの解析を行う
;;TODO エラー処理
(define (analyze-method-define l unit)
  (if (null? (cdr l))
    #f);TODO warning
  (set-unit-name (symbol->string (cadr l)) unit)
  (set-unit-type type-method unit)
  (if (null? (caddr l))
    #f);TODO warning
  (analyze-args (caddr l) e->e unit))


;;stub用 define-cprocの解析を行う
;;TODO エラー処理
(define (analyze-stub-proc-define l unit)
  (set-unit-name (symbol->string (cadr l)) unit)
  (set-unit-type type-fn unit)
  (analyze-args (caddr l) 
                (lambda (var)(string->symbol (car (string-split (symbol->string var) "::"))))
                unit))

;;defime-classの解析を行う
(define (analyze-class-define l unit)
  (set-unit-name (symbol->string (cadr l)) unit)
  (set-unit-type type-class unit)
  (slot-set! unit 'supers (map x->string (caddr l)))
  (let ([org-slots (slot-ref unit 'slots)]
        [gen-slots (map 
                     (lambda (s)
                       (list (symbol->string (car s))
                             (string-trim-right (fold-right
                                                  (lambda (x acc) (string-append (x->writable-string x) " " acc))
                                                  ""
                                                  (cdr s)))
                             '()))
                     (cadddr l))])
    (for-each
      (lambda (s)
        (cond
          [(assoc (car s) gen-slots)
           => (lambda (slot) (set-car! (cddr slot) (caddr s)))]
          [else (format #t "warning.")]))
      org-slots)
    (slot-set! unit 'slots gen-slots)))



;;解析可能な式のリスト
(define-constant analyzable-symbols 
                 `(
                   (define . ,analyze-normal-define)
                   (define-constant . ,analyze-normal-define)
                   (define-cproc . ,analyze-stub-proc-define)
                   (define-method . ,analyze-method-define)
                   (define-class . ,analyze-class-define)
                   ))

;;解析可能な式か?
(define (analyzable? exp)
  (if (list? exp)
    (boolean (assq (car exp) analyzable-symbols))
    #f))

;;ドキュメントの直下にある式が定義であれば
;;ドキュメントと関連するものとして解析を行う
(define (parse-related-define unit)
  (let* ([org-fp (port-seek (current-input-port) 0 SEEK_CUR)]
         [exp (read)])
    ;;seek to origin point file pointer
    (port-seek (current-input-port) 
               (- org-fp (port-seek (current-input-port) 0 SEEK_CUR))
               SEEK_CUR)
    (if (analyzable? exp)
      ((assq-ref  analyzable-symbols (car exp)) exp unit))
    unit))


;-------***************-----------
;;read file
;-------***************-----------

;;ドキュメントの開始マーカーがあるか?
(define (doc-start-line? line)
  (boolean (rxmatch #/^\s*\;\;\;\;\;/ line)))

(define (read-all-doc filename)
  (let ([doc (make <doc>)])
    (with-input-from-file 
      filename
      (lambda ()
        (port-for-each
          (lambda (line)
            (when (doc-start-line? line)
              (port-seek (current-input-port) (- (string-size line)) SEEK_CUR)
              (show (add-unit doc 
                              (commit-unit 
                                (parse-related-define 
                                  (parse-doc (make <unit-bottom>))))))))
          read-line)))
    (commit-doc doc)))


;-------***************-----------
;;Entry point
;-------***************-----------

;;一度解析したファイルのドキュメントをキャッシュしておく
(define-constant docs (make-hash-table 'string=?))

(define (geninfo-from-file path no-cache)
  (cond
    [(and (not no-cache) (hash-table-get docs path #f)) => e->e]
    [else (let ([doc (read-all-doc path)])
            (if (not no-cache)
              (hash-table-put! docs path doc))
            doc)]))

(define (geninfo-from-module symbol no-cache)
  (let ([path (library-fold symbol (lambda (l p acc) (cons p acc)) '())])
    (if (null? path)
      (raise (condition
               (<geninfo-warning> (message "module not found"))))
      (geninfo-from-file (car path) no-cache))))

;;;;;
;;ファイルを解析しドキュメントユニットを生成する
;; @param from シンボルであれば、モジュール名として扱われ現在のロードパスからファイルを検索して解析する
;;				文字列であれば、ファイルへのパス名として扱われそのパスに存在するファイルを解析する
(define (geninfo from :optional (no-cache #f))
  (cond
    [(symbol? from) (geninfo-from-module from no-cache)]
    [(string? from) (geninfo-from-file from no-cache)]
    [else #f])); TODO warging

;;ドキュメントの中からnameがsymbolのユニットを探す
(define (find-unit str-symbol doc)
  (find (lambda (unit) (string=? str-symbol (ref unit 'name))) (ref doc 'units)))

;;fromが指定してある場合はそのドキュメントの中から
;;fromが指定していない場合は読み込み済みの全てのドキュメントの中から
;;symbolと同じ名前を持つユニットを探す
(define (find-doc-unit symbol from)
  (if from
    (find-unit symbol (geninfo-from-file from #f))
    (call/cc (lambda (c)
               (hash-table-for-each docs (lambda (name doc) (cond [(find-unit symbol doc) => c])))
               #f))))

;;現在読み込んでいるモジュールから調べたいシンボルを探し、
;;見つかったモジュールのドキュメントを生成してユニットを返す
(define (find-doc-unit-in-modules symbol)
  (cond
    [(call/cc (lambda (c)
                (for-each 
                  (lambda (m)
                    (if (find 
                          (lambda (s) (eq? s symbol))
                          (hash-table-keys (module-table m)))
                      (c m)))
                  (all-modules))
                #f))
     => (lambda (m) (find-unit (x->string symbol) (geninfo-from-module (module-name m) #f)))]
    [else #f]))


;;;;;
;;symbolのドキュメントユニットを探し、api情報を出力する
(define (api symbol :key from)
  (define (show-unit unit) (show-unit-api unit) #t)
  (guard (e
           ;;TODO もうちょっとましな警告表示
           [(<geninfo-warning> e) (format #t "~s\n" (slot-ref e 'message))])
         (cond
           [(find-doc-unit (x->string symbol) (if (undefined? from) #f from)) => show-api]
           [(find-doc-unit-in-modules symbol) => show-api]
           [else #f])))

