#lang racket
; =========================================================
; Mapa.rkt - Lógica del Buscaminas
; =========================================================
; Representación de cada celda:
;   (list bomba numero)
;   - bomba: 1 si la celda contiene una bomba, 0 si no.
;   - numero: cantidad de bombas en las 8 casillas vecinas.
;
; Flujo del algoritmo:
; 1. mapping: genera un tablero vacío con todas las celdas en (0 0).
; 2. colocar-bombas: selecciona posiciones aleatorias y coloca bombas (1 0).
; 3. contar-bombas-alrededor: para cada celda vacía, recorre sus vecinos y cuenta bombas.
; 4. calcular-numeros: llena cada celda vacía con el número de bombas cercanas.
; 5. generar-mapa: integra todo → devuelve un tablero con bombas (*) y números (1–8).
;
; Nota: las celdas con 0 bombas alrededor se muestran como vacías (no como "0").

(require racket/list)

(provide inicializar-reveladas
         inicializar-flags
         contar-banderas
         celda-revelada?
         marcar-revelada
         revelar-celda
         revelar-cascada
         victoria?
         revelar-todas-las-bombas
         generar-mapa
         level
         mapping)

;; ================= MAPA VACÍO =================
(define (mapping i j)
  (build-list i
    (lambda (_)
      (build-list j (lambda (_) (list 0 0))))))

;; ================= DIFICULTAD =================
(define (level n)
  (cond [(equal? n 1) (+ 1 (random 10))]
        [(equal? n 2) (+ 10 (random 20))]
        [(equal? n 3) (+ 20 (random 30))]
        [else 0]))

;; ================= UTILIDADES =================
(define (en-rango? r c filas cols)
  (and (>= r 0) (< r filas)
       (>= c 0) (< c cols)))

(define (contar-bombas-alrededor tablero r c)
  (for*/sum ([dr '(-1 0 1)]
             [dc '(-1 0 1)]
             #:when (not (and (= dr 0) (= dc 0))))
    (if (and (en-rango? (+ r dr) (+ c dc) (length tablero) (length (first tablero)))
             (= 1 (car (list-ref (list-ref tablero (+ r dr)) (+ c dc)))))
        1
        0)))

;; ================= COLOCAR BOMBAS =================
(define (colocar-bombas tablero num-bombas)
  (for/list ([i (in-range (length tablero))])
    (for/list ([j (in-range (length (first tablero)))])
      (if (member (list i j)
                  (take (shuffle
                         (for*/list ([r (in-range (length tablero))]
                                     [c (in-range (length (first tablero)))])
                           (list r c)))
                        num-bombas))
          (list 1 0)
          (list 0 0)))))


;; ================= NÚMEROS =================
(define (calcular-numeros tablero)
  (for/list ([i (in-range (length tablero))])
    (for/list ([j (in-range (length (first tablero)))])
      (if (= (car (list-ref (list-ref tablero i) j)) 1)
          (list 1 0)
          (list 0 (contar-bombas-alrededor tablero i j))))))

;; ================= MAPA COMPLETO =================
(define (generar-mapa filas cols num-bombas)
  (define tablero-final
    (calcular-numeros (colocar-bombas (mapping filas cols) num-bombas)))
  (imprimir-mapa tablero-final)
  tablero-final)

(define (imprimir-mapa tablero)
  (for ([fila tablero])
    (for ([celda fila])
      (display
       (cond [(= (car celda) 1) "* "]
             [(zero? (cadr celda)) "0"]
             [else (format "~a " (cadr celda))])))
    (newline)))

;; ================= MATRICES BOOLEANAS =================
(define (inicializar-reveladas filas cols)
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)]) #f)))

(define (inicializar-flags filas cols)
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)]) #f)))

(define (contar-banderas flags)
  (for/sum ([fila flags])
    (for/sum ([v fila]) (if v 1 0))))

(define (celda-revelada? reveladas r c)
  (and (< r (length reveladas))
       (< c (length (first reveladas)))
       (list-ref (list-ref reveladas r) c)))

(define (marcar-revelada reveladas r c)
  (for/list ([i (in-range (length reveladas))])
    (for/list ([j (in-range (length (first reveladas)))])
      (if (and (= i r) (= j c))
          #t
          (list-ref (list-ref reveladas i) j)))))

(define (revelar-celda reveladas r c)
  (marcar-revelada reveladas r c))


;; =============== FLood fill ===============
;; Flood fill (revelado en cascada para celdas con número 0)
(define (revelar-cascada mapa reveladas flags r0 c0)
  (define filas (length mapa))
  (define cols  (length (first mapa)))
  (let loop ([pend (list (list r0 c0))]
             [R reveladas]) ; R es la matriz de reveladas
    (cond
      [(null? pend) R]
      [else
       (define r (caar pend))
       (define c (cadar pend))
       (define rest (cdr pend))
       (cond
         [(not (en-rango? r c filas cols))
          (loop rest R)]
         [(or (celda-revelada? R r c)
              (list-ref (list-ref flags r) c)) ; no abrir si hay bandera
          (loop rest R)] ; Si ya esta revelada o hay bandera se la salta
         [else
          (define R1 (marcar-revelada R r c))
          (define celda (list-ref (list-ref mapa r) c))
          (define es-bomba (= 1 (car celda)))
          (define num (cadr celda))
          (if (and (not es-bomba) (= num 0)) ; Si es 0 se sigue expandiendo
              (let ([vecinos
                     (list (list (- r 1) (- c 1)) (list (- r 1) c) (list (- r 1) (+ c 1))
                           (list r (- c 1))                       (list r (+ c 1))
                           (list (+ r 1) (- c 1)) (list (+ r 1) c) (list (+ r 1) (+ c 1)))])
                (loop (append vecinos rest) R1)) ; Si no es 0 solo se abre una celda
              (loop rest R1))])])))

;; Victoria: todas las NO-bomba están reveladas
(define (victoria? mapa reveladas)
  (define filas (length mapa))
  (define cols  (length (first mapa)))
  (for*/and ([i (in-range filas)] [j (in-range cols)])
    (define celda (list-ref (list-ref mapa i) j))
    (define es-bomba (= 1 (car celda)))
    (define rev? (celda-revelada? reveladas i j))
    (or es-bomba rev?)))

;; Revelar todas las bombas (para game over)
(define (revelar-todas-las-bombas mapa reveladas)
  (define filas (length mapa))
  (define cols  (length (first mapa)))
  ; Recolectar coordenadas de bombas
  (define bombas
    (for*/list ([i (in-range filas)]
                [j (in-range cols)]
                #:when (= 1 (car (list-ref (list-ref mapa i) j))))
      (list i j)))
  ; Marcar todas como reveladas
  (for/fold ([R reveladas]) ([p bombas])
    (marcar-revelada R (first p) (second p))))
