#lang racket

(require file/sha1)

(struct block (timestamp data prev-block hashcode) #:transparent)
(struct blockchain (blocks))

(define (hash-block prev-hash data timestamp)
  (bytes->hex-string
   (sha256-bytes
    (open-input-string
     (string-append prev-hash (string-append data (number->string timestamp)))))))

(define (get-block data prev-block-hash)
  (let* ([now (current-seconds)]
         [hashcode (hash-block prev-block-hash data now)])
    (block now data prev-block-hash hashcode)))

(define (add-block bc data)
  (let* ([prev (last (blockchain-blocks bc))]
         [new-block (get-block data (block-hashcode prev))])
    (blockchain (append (blockchain-blocks bc) (list new-block)))))

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
    (for-each displayln (blockchain-blocks bc))))
