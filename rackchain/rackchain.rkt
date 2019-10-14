#lang racket

(require file/sha1)
(require web-server/dispatch
         web-server/http
         web-server/servlet-env)
(require json)

(define target-bits 20)  ;; proof-of-work difficulity
(define target (arithmetic-shift 1 (- 256 target-bits)))

(struct block (timestamp data prev-block hashcode nonce) #:transparent)
(struct blockchain (blocks) #:transparent)

(define (proof-of-work block nonce)
  (let* ([data (string-append*
                (block-prev-block block)
                (list
                 (block-data block)
                 (number->string (block-timestamp block) 16)
                 (number->string target-bits 16)
                 (number->string nonce 16)))]
         [data-hash (string->number
                     (bytes->hex-string
                      (sha256-bytes (open-input-string data))) 16)])
    (cond
      [(< data-hash target) (cons (number->string data-hash 16) nonce)]
      [else (proof-of-work block (+ 1 nonce))])))

(define (validate-block block)
 (let* ([data (string-append*
               (block-prev-block block)
               (list
                (block-data block)
                (number->string (block-timestamp block) 16)
                (number->string target-bits 16)
                (number->string (block-nonce block) 16)))]
        [data-hash (string->number
                    (bytes->hex-string
                     (sha256-bytes (open-input-string data))) 16)])
   (cond
     [(< data-hash target) #t]
     [else #f])))

(define (get-block data prev-block-hash)
  (let* ([now (current-seconds)]
         [new-block (block now data prev-block-hash "" 0)]
         [hashcode (proof-of-work new-block 0)])
    (block now data prev-block-hash (car hashcode) (cdr hashcode))))

(define (add-block bc data)
  (let* ([prev (last (blockchain-blocks bc))]
         [new-block (get-block data (block-hashcode prev))])
    (displayln (format "Mining the block ~s" data))
    (blockchain (cons new-block (blockchain-blocks bc)))))

(define (genesis-block)
  (get-block "Genesis block" ""))

;; Blockchain functions

(define (new-blockchain)
  (blockchain (list (genesis-block))))

(define (test-blockchain)
  (let* ([bc (new-blockchain)]
         [bc (add-block bc "Send 1 BTC to Casper")]
         [bc (add-block bc "Send 5 BTC to Melchior")]
         [bc (add-block bc "Send 2 BTC to Balthasar")])
    (for-each
     (λ (block)
       (displayln (format "Block: ~s" (block-data block)))
       (displayln (format "Hash: ~s" (block-hashcode block)))
       (displayln (format "Nonce: ~s" (block-nonce block)))
       (displayln (format "Valid: ~s" (validate-block block))))
     (reverse (blockchain-blocks bc)))))

;; HTTP REST APIs

(define bc (new-blockchain))

(define (blockchain-jsexpr bc)
  (bytes-append*
   #"{\"blocks\":"
   (list (jsexpr->bytes
          (map (λ (block)
                 (hasheq 'timestamp (block-timestamp block)
                         'data (block-data block)
                         'nonce (block-nonce block)
                         'valid (validate-block block)))
               (reverse (blockchain-blocks bc)))) #"}")))

(define (get-blockchain req)
  (response
   200 #"OK"
   (current-seconds) #"application/json"
   empty
   (λ (op)
     (write-bytes (blockchain-jsexpr bc) op))))

(define (post-blockchain req)
  (set! bc (add-block bc (bytes->string/utf-8 (request-post-data/raw req))))
  (response
   200 #"OK"
   (current-seconds) #"application/json"
   empty
   (λ (op)
     (write-bytes #"OK" op))))

(define-values (serve _)
  (dispatch-rules
   [("blockchain") #:method "get" get-blockchain]
   [("blockchain") #:method "post" post-blockchain]))

(serve/servlet serve #:port 6090 #:command-line? #t #:servlet-regexp #rx"")
