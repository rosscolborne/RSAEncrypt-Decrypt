;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname RSA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require math/number-theory)

;; (encrypt string) takes a string, and encrypts it using RSA, with two random
;;   primes. It produces a list containing the encrypted string and its unique
;;   private key.
;; encrypt: Str --> (list of (list of Nat) (list of Nat))
(define (encrypt string)
  (local [(define (make-M string)
            (map char->integer (string->list string)))
          (define p (nth-prime (random 100)))
          (define q (nth-prime (+ 100 (random 100))))
          (define n (* p q))
          (define x (* (sub1 p) (sub1 q)))
          (define (make-e r)
            (cond [(= (gcd r x) 1)
                   r]
                  [else (make-e (add1 r))]))
          (define e-const (make-e (random (sub1 x))))
          (define d (modular-inverse e-const x))
          (define public-key (list e-const n))
          (define private-key (list d n))
          (define M (make-M string))]
    
    (list (map (lambda (i) (modulo (expt i e-const) n)) M)
          private-key)))

;; (decrypt cipher) takes a list of Nats as cyphertext, and a private key,
;;   and decrypts the ciphertext back into a String.
;; decrypt: (list of Nat)(list of Nat) --> Str
(define (decrypt cipher private-key)
  (local [(define d (first private-key))
          (define n (second private-key))
          (define R (map (lambda (i) (modulo (expt i d) n)) cipher))]
   (list->string (map integer->char R))))