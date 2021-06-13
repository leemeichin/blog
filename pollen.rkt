#lang racket

(provide (all-defined-out))

(require txexpr pollen/pagetree pollen/core pollen/setup pollen/decode pollen/cache pollen/file racket/string pollen/tag pollen/unstable/pygments)

(provide highlight)

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html)))

(define (root . elements)
  (decode (txexpr 'root empty elements)
    #:txexpr-elements-proc smart-paragraphs
    #:string-proc (compose1 smart-ellipses smart-quotes smart-dashes)
    #:exclude-tags '(pre code)))

(define (smart-paragraphs elements)
  (decode-paragraphs elements
    #:linebreak-proc (λ (elems) (decode-linebreaks elems #f))))

(define (include-files folder extension)
   (map (λ (str) (string->symbol (path->string (simplify-path (format "~a/~a" folder (string-replace str extension "html"))))))
        (filter (λ (str) (string-suffix? str extension))
                (map path->string (directory-list folder)))))

(define (this-pagetree folder) `(@ ,@(include-files folder "poly.pm")))

(define (latest-posts)
  (sort (children 'posts (get-pagetree "index.ptree"))
        #:key post->date
        string>?))

(define (post->path post) (get-source (path->complete-path (symbol->string post) (current-project-root))))
(define (post->title post) (select-from-metas 'title post))
(define (post->date post) (select-from-metas 'date post))
(define (post->published? post) (select-from-metas 'published post))
(define (post->size post) (number->string (file-size (post->path post))))

(define average-word-length 4.7)
(define words-per-minute 250)
(define (post->ert post)  (exact-round (/ (/ (string->number (post->size post)) average-word-length) words-per-minute)))

(define posthistory '())
(define (post->history [post null])
    (when (empty? posthistory)
      (let ([gitlog (string-split (with-output-to-string 
                                  (λ () (system (format "git log --format='~a' --max-count=~a --follow -- ~a"
                                                        "%h;%s;%ai"
                                                        10
                                                        (if (null? post) "." (post->path post))))))
                                "\n")])
      (for/list ([logline (in-list gitlog)])
        (let ([log (string-split logline ";")])
          (set! posthistory 
                (append posthistory 
                        (list `#hash([commit . ,(first log)] 
                                     [message . ,(second log)] 
                                     [date . ,(third log)]))))))))
    posthistory)
(define (log->giturl log) (format "https://git.sr.ht/~~mrlee/www.kamelasa.dev/commit/~a" (hash-ref log 'commit)))
(define (log->commit log) (hash-ref log 'commit))
(define (log->message log) (hash-ref log 'message))
(define (log->date log) (hash-ref log 'date))

(define (q author date . body) `(blockquote ,@body (p ,(format "--~a, ~a" author date))))
(define (<> url) `(a ((href ,url)) ,url))
(define tag-time (default-tag-function 'time))

(define (footnotes . refs)
  `(section [(class "footnotes") (role "doc-endnotes")]
    (ol ,@refs)))

(define (^ ref-num . footnote)
  (if (empty? footnote)
      `(a [(class "footnote-ref") 
          (role "doc-noteref") 
          (id ,(format "fnref~a" ref-num))
          (href ,(format "#fn~a" ref-num))]
          (sup ,(number->string ref-num)))
    `(li [(id ,(format "fn~a" ref-num)) (role "doc-endnote")]
      ,@footnote
      (a [(class "footnote-back")
          (role "doc-backlink")
          (href ,(format "#fnref~a" ref-num))]
          "↩︎"))))

(define (page-url pagenode)
  (string-replace (symbol->string pagenode) "\\" "/"))

(define-syntax (for/published-posts stx)
  (syntax-case stx ()
    [(_ #:as binding result-expr ...)
      #'(for/splice
          ([binding (in-list (latest-posts))] #:when (post->published? binding))
          result-expr ...)]))

(define-syntax (codeblock stx)
  (syntax-case stx ()
    [(_ lang code ...)
      #'(highlight #:python-executable (if (equal? (system-type) 'windows) "python.exe" "python3") 
                   #:line-numbers? #f lang code ...)]))
