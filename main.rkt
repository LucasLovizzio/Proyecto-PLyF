#lang racket
;; Ejecutar:
;; racket main.rkt --products products.json --cart cart.json --discounts discounts.json [--coupon SAVE10]

(require json)

(struct product (id name category brand price) #:transparent)
(struct line (pid qty) #:transparent)
(struct cart (lines) #:transparent)

;; ------------------- CLI -------------------
(define products-file (make-parameter "products.json"))
(define cart-file      (make-parameter "cart.json"))
(define discounts-file (make-parameter "discounts.json"))
(define coupon-code    (make-parameter #f))

(command-line
 #:once-each
 [("--products") p "Archivo de productos" (products-file p)]
 [("--cart")      c "Archivo de carrito"   (cart-file c)]
 [("--discounts") d "Archivo de descuentos" (discounts-file d)]
 [("--coupon")    k "Cupón (opcional)"     (coupon-code k)])

;; ------------------- Carga JSON -------------------
(define (read-json-file p)
  (call-with-input-file p read-json))

(define (load-products path)
  (for/hash ([d (in-list (read-json-file path))])
    (define id   (string->symbol (hash-ref d 'id)))
    (define name (hash-ref d 'name))
    (define cat  (string->symbol (hash-ref d 'category)))
    (define br   (string->symbol (hash-ref d 'brand)))
    (define pr   (hash-ref d 'price))
    (values id (product id name cat br pr))))

(define (load-cart path)
  (cart
   (for/list ([d (in-list (read-json-file path))])
     (line (string->symbol (hash-ref d 'product_id))
           (hash-ref d 'qty)))))

(define (load-discounts path)
  (define d (read-json-file path))
  (values (hash-ref d 'line_discounts)
          (hash-ref d 'cart_discounts)))

(define products #f)
(define cartA #f)
(define line-rules '())
(define cart-rules '())

(set! products (load-products (products-file)))
(set! cartA     (load-cart (cart-file)))
(let-values ([(lr cr) (load-discounts (discounts-file))])
  (set! line-rules lr) (set! cart-rules cr))

;; ------------------- Helpers -------------------
(define (line-subtotal ln)
  (define p (hash-ref products (line-pid ln)))
  (* (product-price p) (line-qty ln)))

(define (cart-subtotal c)
  (for/sum ([ln (in-list (cart-lines c))]) (line-subtotal ln)))

(define (product-of pid) (hash-ref products pid))

;; ------------------- Descuentos de línea -------------------
(define (line-rule-amount rule c)
  (define typ (hash-ref rule 'type))
  (cond
    [(string=? typ "percent_category")
     (define cat (string->symbol (hash-ref rule 'category)))
     (define perc (hash-ref rule 'percent))
     (for/sum ([ln (in-list (cart-lines c))])
       (define p (product-of (line-pid ln)))
       (if (eq? (product-category p) cat)
           (inexact->exact (round (* (line-subtotal ln) perc 1/100)))
           0))]
    [(string=? typ "percent_brand")
     (define br (string->symbol (hash-ref rule 'brand)))
     (define perc (hash-ref rule 'percent))
     (for/sum ([ln (in-list (cart-lines c))])
       (define p (product-of (line-pid ln)))
       (if (eq? (product-brand p) br)
           (inexact->exact (round (* (line-subtotal ln) perc 1/100)))
           0))]
    [(string=? typ "bogo_category")
     (define cat (string->symbol (hash-ref rule 'category)))
     (define buy (hash-ref rule 'buy))
     (define pay (hash-ref rule 'pay))
     (define free-each (- buy pay))
     (for/sum ([ln (in-list (cart-lines c))])
       (define p (product-of (line-pid ln)))
       (if (eq? (product-category p) cat)
           (* (quotient (line-qty ln) buy) free-each (product-price p))
           0))]
    [else 0]))

(define (applied-line-discounts c)
  (define apps
    (for/list ([r (in-list line-rules)])
      (define amt (line-rule-amount r c))
      (define label (hash-ref r 'label (hash-ref r 'type)))
      (list label amt)))
  (define filtered (filter (lambda (x) (> (second x) 0)) apps))
  (values filtered (for/sum ([x (in-list filtered)]) (second x))))

;; ------------------- Descuentos de carrito (único) -------------------
(define (eligible-cart-rule? r subtotal0)
  (define typ (hash-ref r 'type))
  (cond
    [(string=? typ "money_off")
     (>= subtotal0 (hash-ref r 'min_total))]
    [(string=? typ "percent_cart")
     (and (>= subtotal0 (hash-ref r 'min_total))
          (let ([code (hash-ref r 'code #f)]
                [user (coupon-code)])
            (or (not code) (and user (string=? code user)))))]
    [(string=? typ "tiered_cart")
     (for/or ([t (in-list (hash-ref r 'tiers))])
       (>= subtotal0 (hash-ref t 'threshold)))]
    [else #f]))

(define (cart-rule-amount r subtotal0 interim1)
  (define typ (hash-ref r 'type))
  (cond
    [(string=? typ "money_off") (hash-ref r 'amount)]
    [(string=? typ "percent_cart")
     (inexact->exact (round (* interim1 (hash-ref r 'percent) 1/100)))]
    [(string=? typ "tiered_cart")
     (define tiers (hash-ref r 'tiers))
     (define bestp
       (for/fold ([bp -1]) ([t (in-list tiers)])
         (define thr (hash-ref t 'threshold))
         (define p   (hash-ref t 'percent))
         (if (and (>= subtotal0 thr) (> p bp)) p bp)))
     (if (< bestp 0) 0 (inexact->exact (round (* interim1 bestp 1/100))))]
    [else 0]))

(define (choose-best-cart subtotal0 interim1)
  (define cands
    (for/list ([r (in-list cart-rules)])
      (if (eligible-cart-rule? r subtotal0)
          (let* ([amt   (cart-rule-amount r subtotal0 interim1)]
                 [label (hash-ref r 'label (hash-ref r 'type))]
                 [prio  (hash-ref r 'priority 1000)])
            (list label amt prio))
          (list #f 0 1000))))
  (define filtered (filter (lambda (x) (and (first x) (> (second x) 0))) cands))
  (if (null? filtered)
      (values #f 0)                                  ; <-- 2 valores
      (let* ([max-amt (apply max (map second filtered))]
             [ties    (filter (lambda (x) (= (second x) max-amt)) filtered)]
             [best    (argmin (lambda (x) (third x)) ties)])
        (values (first best) (second best)))))        ; <-- 2 valores

;; Devuelve SOLO el elemento con menor f(x)
(define (argmin f xs)
  (let loop ([best (car xs)] [bestv (f (car xs))] [rest (cdr xs)])
    (if (null? rest)
        best
        (let* ([x (car rest)] [v (f x)])
          (if (< v bestv)
              (loop x v (cdr rest))
              (loop best bestv (cdr rest)))))))


;; ------------------- Impresión -------------------

;; Anchos de columnas
(define W-IDX+NAME 26)   ; "001" + nombre
(define W-QTY      27)   ; Cantidad
(define W-PRICE    35)   ; Precio Unitario
(define TOTAL-COL  70)   ; Columna de alineación derecha para totales

(define (truncate s max)
  (define str (format "~a" s))
  (if (> (string-length str) max)
      (string-append (substring str 0 (max 0 (- max 1))) "…")
      str))

(define (pad-right s w)
  (define str (format "~a" s))
  (define n   (string-length str))
  (if (>= n w) str (string-append str (make-string (- w n) #\space))))

;; ---> reemplaza "~3,'0d" con un zero-pad casero
(define (zero-pad n width)
  (define s (number->string n))
  (string-append (make-string (max 0 (- width (string-length s))) #\0) s))

(define (row-idx+name idx name)
  (pad-right (string-append (zero-pad idx 3)
                            (truncate name (- W-IDX+NAME 3)))
             W-IDX+NAME))

(define (row-qty qty)
  (pad-right (format "~a" qty) W-QTY))

(define (row-price n)
  (pad-right (format "ARS ~a" n) W-PRICE))

(define (sep) (printf "~a~n" (make-string 96 #\-)))

(define (print-header)
  (newline)
  (printf "Carrito:~n~n")
  (printf "~a~a~a~a~n"
          (pad-right "N°Producto"      W-IDX+NAME)
          (pad-right "Cantidad"        W-QTY)
          (pad-right "Precio Unitario" W-PRICE)
          "Subtotal")
  (sep))

(define (print-items c)
  (for ([ln (in-list (cart-lines c))]
        [idx (in-naturals 1)])
    (define p   (product-of (line-pid ln)))
    (define sub (line-subtotal ln))
    (printf "~a~a~aARS ~a~n"
            (row-idx+name idx (product-name p))
            (row-qty (line-qty ln))
            (row-price (product-price p))
            sub))
  (sep))

;; Alinear cosas como "Subtotal: .... 8400" a TOTAL-COL
(define (print-right label value)
  (define left (format "~a:" label))
  (printf "~a~a~a~n"
          left
          (make-string (max 1 (- TOTAL-COL (string-length left))) #\space)
          value))

(define (print-applied-line label amt)
  (define left (format "Aplicado (línea):   ~a" label))
  (printf "~a~a-~a~n"
          left
          (make-string (max 1 (- TOTAL-COL (string-length left))) #\space)
          amt))

(define (print-cart-discount label amt)
  (define left (format "Descuento especial: ~a" label))
  (printf "~a~a-~a~n"
          left
          (make-string (max 1 (- TOTAL-COL (string-length left))) #\space)
          amt))

(define (print-total label value)
  (sep)
  (print-right label value))

(define (main)
  (define s0 (cart-subtotal cartA))
  (define-values (line-apps dline) (applied-line-discounts cartA))
  (define s1 (- s0 dline))
  (define-values (cart-label dcart) (choose-best-cart s0 s1))
  (define sfinal (- s1 dcart))

  (print-header)
  (print-items cartA)

  (print-right "Subtotal" s0)
  (newline)

  (for ([x (in-list line-apps)])
    (print-applied-line (first x) (second x)))

  (newline)
  (print-right "Total intermedio" s1)
  (newline)

  (when cart-label
    (print-cart-discount cart-label dcart)
    (newline))

  (print-total "TOTAL" sfinal))

(main)

