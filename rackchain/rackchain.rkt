#lang racket

(require file/sha1)

(define TARGET-BITS 16)
(define TARGET (arithmetic-shift 1 (- 256 TARGET-BITS)))

(struct block (timestamp data prev-block hashcode nonce) #:transparent)
(struct blockchain (blocks))

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
                 (number->string (block-timestamp block))
                 (number->string TARGET-BITS)
                 (number->string nonce)))]
         [data-hash (string->number
                     (bytes->hex-string
                      (sha256-bytes (open-input-string data))) 16)])
    (cond
      [(< data-hash TARGET) (cons (number->string data-hash 16) nonce)]
      [else (proof-of-work block (+ 1 nonce))])))

(define (validate-block block)
 (let* ([data (string-append*
               (block-prev-block block)
               (list
                (block-data block)
                (number->string (block-timestamp block))
                (number->string TARGET-BITS)
                (number->string (block-nonce block))))]
        [data-hash (string->number
                    (bytes->hex-string
                     (sha256-bytes (open-input-string data))) 16)])
   (cond
     [(< data-hash TARGET) #t]
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
    (for-each
     (λ (block)
       (displayln (format "Block: ~s" (block-data block)))
       (displayln (format "Hash: ~s" (block-hashcode block)))
       (displayln (format "Valid: ~s" (validate-block block))))
     (blockchain-blocks bc))))
