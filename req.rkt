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
                 #:headers [header-values '()])
  ;  (displayln (format "url = ~a~nmethod = ~a~nparams = ~a~ndata = ~a~nheaders = ~a~n"
  ;                     url
  ;                     method
  ;                     params
  ;                     data
  ;                     header-values))
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
                                 #:headers (alist->headers header-values)
                                 #:port port)))
      (let* ([content (port->string iport)]
             [status (bytes->status res)]
             [headers-alist (headers->alist headers)])
        (close-input-port iport)
        (if (= status 301)
            (let* ([moved-path (cdr (assoc "Location" headers-alist))]
                   [new-url (format "~a://~a:~a~a" scheme host port moved-path)])
              (request new-url
                       #:method method
                       #:params params
                       #:data data
                       #:headers header-values))
            (list status headers-alist content))))))

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

(define (status-of res)
  (first res))

(provide request ok? json-of content-of headers-of status-of)
