#lang web-server/insta
 


(require "spellcheck.rkt")

(struct post (body))
 
; BLOG: blog
; The static blog.
(define BLOG
  (list (post " ")
        (post " ")))
 
; start: request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (define a-blog
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request))
                 BLOG)]
          [else
           BLOG]))
  (render-blog-page a-blog request))
 
 
; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-post? bindings)
  
       (exists-binding? 'body bindings))
 
 
; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-post bindings)
  (post
        (ap (suggest (extract-binding/single 'body bindings) spellcheck?))))
 
; render-blog-page: blog request -> response
; Consumes a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "Auto Correct"))
          (body
           (h1 "Auto Correct")
           
           (form
            
            (input ((name "body")))
            (input ((type "submit"))))
           ,(render-posts a-blog)))))
 
; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-post a-post)
  `(div ((class "post"))
        
        (p ,(post-body a-post))))
 
 
; render-posts: blog -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))



(define (insert-letters s)
  (foldr (lambda (x y)
           (cons (list->string x) y)) empty
                                      (foldr append empty (insert-letters1 s))))

(define (insert-letters1 s1)
  (build-list (length (string->list s1))
              (lambda (i) (insert-at i (string->list s1)))))

(define (insert-at i s1)
  (build-list 26 (lambda (x1)
                   (ifoldr (lambda (k x y)
                             (cond[(= i k)
                                   (append (list (integer->char (+ 97 x1)))
                                           (list x) y)]
                                  [else (cons x y)])) empty s1))))

(define (trailing-letters s)
  (foldr (lambda (x y)
           (cons (list->string x) y)) empty (trailing-letters1 s)))

(define (trailing-letters1 s)
  (build-list 26 (lambda (x) (append (string->list s)
                                     (list (integer->char (+ 97 x)))))))


(define (replace-letters s)
  (foldr (lambda (x y)
           (cons (list->string x) y)) empty
                                      (foldr append empty (replace-letters1 s))))


;; (replace-letters1 s1) produces a new word with each letter replaced with every possible letter.
(define (replace-letters1 s1)
  (build-list (length (string->list s1))
              (lambda (i) (replace-at i (string->list s1)))))

;; (replace-at i s1) replaces each letter at position i with every possible letter
;; replace-at: Nat (listof Char) -> (listof (listof Char))
(define (replace-at i s1)
  (build-list 26 (lambda (x1)
                   (ifoldr (lambda (k x y)
                             (cond[(= i k)
                                   (append (list (integer->char (+ 97 x1))) y)]
                                  [else (cons x y)])) empty s1))))


(define (swap-letters s)
  (foldr (lambda (x y) (cons (list->string x) y)) empty (swap-letters1 s)))

(define (swap-letters1 s)
  (local[(define loc (string->list s))]
    (build-list (- (length loc) 1)
                (lambda (i) (swap-at i loc)))))

;; (swap-at i loc) swaps the letters at position i
;; swap-at: Nat (listof Char) -> (listof (listof Char))
(define (swap-at i loc)
  (ifoldr (lambda (k x y)
            (cond[(= k i) (append (list (first y)) (list x) (rest y))]
                 [else (cons x y)])) empty loc))




(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))

(define (remove-dups slst)
  (foldr (lambda (x y)
           (cond[(empty? (filter (lambda (x1) (equal? x1 x)) y))
                 (cons x y)]
                [else y])) empty slst))
 

(define (ifoldr combine base lst)
  (ifoldr-helper combine base lst 0))

(define (ifoldr-helper combine base lst count)
  (cond[(empty? lst) base]
       [else (combine count (first lst)
                      (ifoldr-helper combine base (rest lst) (add1 count)))]))

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))


(define (ap list1)
  (cond[(empty? list1) ""]
       [else (string-append (first list1) " " (ap (rest list1)))]))


