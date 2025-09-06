#lang racket
;; Ejecutar:
;; racket main.rkt --products products.json --cart cart.json --discounts discounts.json [--coupon SAVE10]
;; (también acepta: --productos, --carrito, --descuentos, --cupon)

(require json)

;; ---- TIMER: tiempo de inicio del script (incluye carga de JSON, etc.) ----
(define *t0-ms* (current-inexact-milliseconds))

;; =================== Modelos ===================
(struct producto (id nombre categoria marca precio) #:transparent)
(struct linea (pid cant) #:transparent)
(struct carrito (lineas) #:transparent)

;; =================== CLI ===================
(define archivo-productos (make-parameter "products.json"))
(define archivo-carrito   (make-parameter "cart.json"))
(define archivo-desc      (make-parameter "discounts.json"))
(define codigo-cupon      (make-parameter #f))

(command-line
 #:once-each
 [("--products" "--productos") p "Archivo de productos"   (archivo-productos p)]
 [("--cart"     "--carrito")   c "Archivo de carrito"     (archivo-carrito c)]
 [("--discounts" "--descuentos") d "Archivo de descuentos" (archivo-desc d)]
 [("--coupon"   "--cupon")     k "Cupón (opcional)"       (codigo-cupon k)])

;; =================== Carga JSON ===================
(define (leer-json p)
  (call-with-input-file p read-json))

(define (cargar-productos ruta)
  (for/hash ([d (in-list (leer-json ruta))])
    (define id   (string->symbol (hash-ref d 'id)))
    (define nom  (hash-ref d 'name))
    (define cat  (string->symbol (hash-ref d 'category)))
    (define mar  (string->symbol (hash-ref d 'brand)))
    (define pre  (hash-ref d 'price))
    (values id (producto id nom cat mar pre))))

(define (cargar-carrito ruta)
  (carrito
   (for/list ([d (in-list (leer-json ruta))])
     (linea (string->symbol (hash-ref d 'product_id))
            (hash-ref d 'qty)))))

(define (cargar-descuentos ruta)
  (define d (leer-json ruta))
  (values (hash-ref d 'line_discounts)
          (hash-ref d 'cart_discounts)))

(define productos #f)
(define carritoA  #f)
(define reglas-linea   '())
(define reglas-carrito '())

(set! productos (cargar-productos (archivo-productos)))
(set! carritoA  (cargar-carrito (archivo-carrito)))
(let-values ([(rl rc) (cargar-descuentos (archivo-desc))])
  (set! reglas-linea rl) (set! reglas-carrito rc))

;; =================== Helpers ===================
(define (producto-de pid) (hash-ref productos pid))

(define (subtotal-linea ln)
  (define p (producto-de (linea-pid ln)))
  (* (producto-precio p) (linea-cant ln)))

(define (subtotal-carrito c)
  (for/sum ([ln (in-list (carrito-lineas c))]) (subtotal-linea ln)))

;; =================== Descuentos de línea ===================
(define (monto-regla-linea regla c)
  (define tipo (hash-ref regla 'type))
  (cond
    [(string=? tipo "percent_category")
     (define cat (string->symbol (hash-ref regla 'category)))
     (define perc (hash-ref regla 'percent))
     (for/sum ([ln (in-list (carrito-lineas c))])
       (define p (producto-de (linea-pid ln)))
       (if (eq? (producto-categoria p) cat)
           (inexact->exact (round (* (subtotal-linea ln) perc 1/100)))
           0))]
    [(string=? tipo "percent_brand")
     (define mar (string->symbol (hash-ref regla 'brand)))
     (define perc (hash-ref regla 'percent))
     (for/sum ([ln (in-list (carrito-lineas c))])
       (define p (producto-de (linea-pid ln)))
       (if (eq? (producto-marca p) mar)
           (inexact->exact (round (* (subtotal-linea ln) perc 1/100)))
           0))]
    [(string=? tipo "bogo_category")
     (define cat (string->symbol (hash-ref regla 'category)))
     (define buy (hash-ref regla 'buy))
     (define pay (hash-ref regla 'pay))
     (define free-each (- buy pay))
     (for/sum ([ln (in-list (carrito-lineas c))])
       (define p (producto-de (linea-pid ln)))
       (if (eq? (producto-categoria p) cat)
           (* (quotient (linea-cant ln) buy) free-each (producto-precio p))
           0))]
    [else 0]))

(define (descuentos-linea-aplicados c)
  (define apps
    (for/list ([r (in-list reglas-linea)])
      (define amt (monto-regla-linea r c))
      (define etiqueta (hash-ref r 'label (hash-ref r 'type)))
      (list etiqueta amt)))
  (define filtrados (filter (lambda (x) (> (second x) 0)) apps))
  (values filtrados (for/sum ([x (in-list filtrados)]) (second x))))

;; =================== Descuentos de carrito (único) ===================
(define (regla-carrito-elegible? r subtotal0)
  (define tipo (hash-ref r 'type))
  (cond
    [(string=? tipo "money_off")
     (>= subtotal0 (hash-ref r 'min_total))]
    [(string=? tipo "percent_cart")
     (and (>= subtotal0 (hash-ref r 'min_total))
          (let ([code (hash-ref r 'code #f)]
                [user (codigo-cupon)])
            (or (not code) (and user (string=? code user)))))]
    [(string=? tipo "tiered_cart")
     (for/or ([t (in-list (hash-ref r 'tiers))])
       (>= subtotal0 (hash-ref t 'threshold)))]
    [else #f]))

(define (monto-regla-carrito r subtotal0 interino1)
  (define tipo (hash-ref r 'type))
  (cond
    [(string=? tipo "money_off") (hash-ref r 'amount)]
    [(string=? tipo "percent_cart")
     (inexact->exact (round (* interino1 (hash-ref r 'percent) 1/100)))]
    [(string=? tipo "tiered_cart")
     (define tiers (hash-ref r 'tiers))
     (define mejor%
       (for/fold ([bp -1]) ([t (in-list tiers)])
         (define thr (hash-ref t 'threshold))
         (define p   (hash-ref t 'percent))
         (if (and (>= subtotal0 thr) (> p bp)) p bp)))
     (if (< mejor% 0) 0 (inexact->exact (round (* interino1 mejor% 1/100))))]
    [else 0]))

(define (elegir-mejor-carrito subtotal0 interino1)
  (define cands
    (for/list ([r (in-list reglas-carrito)])
      (if (regla-carrito-elegible? r subtotal0)
          (let* ([amt   (monto-regla-carrito r subtotal0 interino1)]
                 [etq   (hash-ref r 'label (hash-ref r 'type))]
                 [prio  (hash-ref r 'priority 1000)])
            (list etq amt prio))
          (list #f 0 1000))))
  (define filtrados (filter (lambda (x) (and (first x) (> (second x) 0))) cands))
  (if (null? filtrados)
      (values #f 0)
      (let* ([max-amt (apply max (map second filtrados))]
             [empates (filter (lambda (x) (= (second x) max-amt)) filtrados)]
             [mejor   (argmin (lambda (x) (third x)) empates)])
        (values (first mejor) (second mejor)))))

;; Devuelve SOLO el elemento con menor f(x)
(define (argmin f xs)
  (let loop ([best (car xs)] [bestv (f (car xs))] [rest (cdr xs)])
    (if (null? rest)
        best
        (let* ([x (car rest)] [v (f x)])
          (if (< v bestv)
              (loop x v (cdr rest))
              (loop best bestv (cdr rest)))))))

;; =================== Impresión ===================
;; Anchos de columnas
(define W-IDX+NOMBRE 26)  ; "001" + nombre
(define W-CANT       27)  ; Cantidad
(define W-PRECIO     35)  ; Precio Unitario
(define COL-TOTAL    70)  ; Columna para alinear totales a la derecha

(define (truncar s max)
  (define str (format "~a" s))
  (if (> (string-length str) max)
      (string-append (substring str 0 (max 0 (- max 1))) "…")
      str))

(define (pad-der s w)
  (define str (format "~a" s))
  (define n   (string-length str))
  (if (>= n w) str (string-append str (make-string (- w n) #\space))))

(define (cero-pad n w)
  (define s (number->string n))
  (string-append (make-string (max 0 (- w (string-length s))) #\0) s))

(define (col-idx+nombre idx nombre)
  (pad-der (string-append (cero-pad idx 3)
                          (truncar nombre (- W-IDX+NOMBRE 3)))
           W-IDX+NOMBRE))

(define (col-cant c)   (pad-der (format "~a" c) W-CANT))
(define (col-precio n) (pad-der (format "ARS ~a" n) W-PRECIO))

(define (sep) (printf "~a~n" (make-string 96 #\-)))

(define (imprimir-encabezado)
  (newline)
  (printf "Carrito:~n~n")
  (printf "~a~a~a~a~n"
          (pad-der "N°Producto"      W-IDX+NOMBRE)
          (pad-der "Cantidad"        W-CANT)
          (pad-der "Precio Unitario" W-PRECIO)
          "Subtotal")
  (sep))

(define (imprimir-items c)
  (for ([ln (in-list (carrito-lineas c))]
        [idx (in-naturals 1)])
    (define p   (producto-de (linea-pid ln)))
    (define sub (subtotal-linea ln))
    (printf "~a~a~aARS ~a~n"
            (col-idx+nombre idx (producto-nombre p))
            (col-cant (linea-cant ln))
            (col-precio (producto-precio p))
            sub))
  (sep))

;; Alinea "Etiqueta: .... valor" en la columna COL-TOTAL
(define (imprimir-derecha etiqueta valor)
  (define izq (format "~a:" etiqueta))
  (printf "~a~a~a~n"
          izq
          (make-string (max 1 (- COL-TOTAL (string-length izq))) #\space)
          valor))

(define (imprimir-desc-linea etiqueta monto)
  (define izq (format "Aplicado (línea):   ~a" etiqueta))
  (printf "~a~a-~a~n"
          izq
          (make-string (max 1 (- COL-TOTAL (string-length izq))) #\space)
          monto))

(define (imprimir-desc-carrito etiqueta monto)
  (define izq (format "Descuento especial: ~a" etiqueta))
  (printf "~a~a-~a~n"
          izq
          (make-string (max 1 (- COL-TOTAL (string-length izq))) #\space)
          monto))

(define (imprimir-total etiqueta valor)
  (sep)
  (imprimir-derecha etiqueta valor))

;; =================== Programa principal ===================
(define (main)
  (define s0 (subtotal-carrito carritoA))
  (define-values (aplicadas dlinea) (descuentos-linea-aplicados carritoA))
  (define s1 (- s0 dlinea))
  (define-values (etq-carrito dcarrito) (elegir-mejor-carrito s0 s1))
  (define sfinal (- s1 dcarrito))

  (imprimir-encabezado)
  (imprimir-items carritoA)

  (imprimir-derecha "Subtotal" s0)
  (newline)

  (for ([x (in-list aplicadas)])
    (imprimir-desc-linea (first x) (second x)))

  (newline)
  (imprimir-derecha "Total intermedio" s1)
  (newline)

  (when etq-carrito
    (imprimir-desc-carrito etq-carrito dcarrito)
    (newline))

  (imprimir-total "TOTAL" sfinal)

  ;; ---- TIMER: imprimir duración total del script ----
  (newline)
  (define t1 (current-inexact-milliseconds))
  (define dt (- t1 *t0-ms*))
  (printf "Tiempo total: ~a ms (~a s)~n"
          (inexact->exact (round dt))
          (/ dt 1000.0)))

(main)