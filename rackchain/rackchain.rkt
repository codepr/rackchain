#lang racket

(require file/sha1)
(require web-server/dispatch
         web-server/http
         web-server/servlet-env)
(require json)

(define target-bits 20)
(define target (arithmetic-shift 1 (- 256 target-bits)))

(struct block (timestamp data prev-block hashcode nonce) #:transparent)
(struct blockchain (blocks) #:transparent)

(define (hash-block prev-hash data timestamp)
  (bytes->hex-string
   (sha256-bytes
    (open-input-string
     (string-append* prev-hash data (list (number->string timestamp)))))))

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
    (blockchain (reverse (cons new-block (blockchain-blocks bc))))))

(define (genesis-block)
  (get-block "Genesis block" ""))

(define (new-blockchain)
  (blockchain (list (genesis-block))))

(define (test-blockchain)
  (let ([bc (add-block
             (add-block
              (add-block (new-blockchain) "Send 1 BTC to Casper")
              "Send 5 BTC to Melchior")
             "Send 2 BTC to Balthasar")])
    (for-each
     (λ (block)
       (displayln (format "Block: ~s" (block-data block)))
       (displayln (format "Hash: ~s" (block-hashcode block)))
       (displayln (format "Nonce: ~s" (block-nonce block)))
       (displayln (format "Valid: ~s" (validate-block block))))
     (blockchain-blocks bc))))

(define bc (add-block
            (add-block
             (add-block (new-blockchain) "Send 1 BTC to Casper")
             "Send 5 BTC to Melchior")
            "Send 2 BTC to Balthasar"))

(define (blockchain-jsexpr bc)
  (bytes-append*
   #"{\"blocks\":"
   (list (jsexpr->bytes
          (map (λ (block)
                 (hasheq 'timestamp (block-timestamp block)
                         'data (block-data block)
                         'nonce (block-nonce block)
                         'valid (validate-block block)))
               (blockchain-blocks bc))) #"}")))

(define (get-blockchain req)
  (response
   200 #"OK"
   (current-seconds) #"application/json"
   empty
   (λ (op)
     (write-bytes (blockchain-jsexpr bc) op))))

(define-values (serve _)
  (dispatch-rules
   [("blockchain") #:method "get" get-blockchain]))

(serve/servlet serve #:port 6090 #:command-line? #t #:servlet-regexp #rx"")
