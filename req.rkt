#lang racket

(require net/http-client
         net/url
         net/head
         net/uri-codec
         json)

(define (url-components path)
  (let [(url (string->url path))]
    (list
     (url-host url)
     (url-port url)
     (string-join
      (map (λ (x) (path/param-path x))
           (url-path url))
      "/")
     (url-query url)
     (url-scheme url))))

(define (data-or-params data params)
  (if (not data)
      (alist->form-urlencoded params)
      (string->bytes/utf-8 data)))

(define (headers->alist headers)
  (map (λ (p) (cons (bytes->string/utf-8 (car p))
                    (bytes->string/utf-8 (cdr p))))
       (apply append (map extract-all-fields headers))))

(define (alist->headers alist)
  (map (λ (x) (format "~a: ~a" (car x) (cdr x)))
       alist))

(define (bytes->status res)
  (string->number
   (list-ref (string-split
              (bytes->string/utf-8 res)
              " ")
             1)))

(define (request url
                 #:method [method #"GET"]
                 #:params [params '()]
                 #:data [data #f]
                 #:headers [headers '()])
  (let* ([components (url-components url)]
         [host (first components)]
         [port (second components)]
         [path (third components)]
         [qparams (fourth components)]
         [scheme (fifth components)])
    (let-values (([res headers iport]
                  (http-sendrecv (format "~a" host)
                                 (format "/~a" path)
                                 #:ssl? (if (string=? scheme "https") #t #f)
                                 #:method method
                                 #:data (data-or-params
                                         data
                                         (append params qparams))
                                 #:headers (alist->headers headers)
                                 #:port port)))
      (let ([content (port->string iport)])
        (close-input-port iport)
        (list (bytes->status res)
              (headers->alist headers)
              content)))))

(define (ok? res)
  (= (first res)
     200))

(define (json-of res)
  (with-input-from-string
      (third res)
    (λ () (read-json))))

(define (content-of res)
  (third res))

(define (headers-of res)
  (second res))

(provide request ok? json-of content-of headers-of)
