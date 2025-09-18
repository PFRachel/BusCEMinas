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
;Ademas esta logica fue implementada requirio de ayuda de ChatGPT para calcular los numeros del tablero.


; =============== FUNCIONES EXPORTADAS ====================
(provide mapping)
(provide level)
(provide generar-mapa)
(require racket/list)

; crear una matriz que contiene una tupla
; (es bomba(1/0) y cant bombas alrededor)
(define (mapping i j ) ;toma parámetros de tamaño del mapa
  (build-list i ;longitud de filas
    (lambda (_)
      (build-list j (lambda (_) (list 0 0)))))) ;[MODIFICAR] llenar cada celda con los elementos

;toma n(nivel de dificultad)y realiza las validaciones
(define (level n)
  (cond [(equal? n 1) 10]  ; fácil: 10 minas
    [(equal? n 2) 20]  ; medio: 20 minas
    [(equal? n 3) 30]  ; difícil: 30 minas
    [else 0]))
  
;; Verificar si coordenada está en rango
(define (en-rango? r c filas cols)
  (and (>= r 0) (< r filas)
       (>= c 0) (< c cols)))

;; Contar bombas en vecinos (3x3 alrededor)
(define (contar-bombas-alrededor tablero r c)
  (define filas (length tablero))
  (define cols (length (first tablero)))
  (for*/sum ([dr '(-1 0 1)]     ; usar for*/sum en vez de for/sum
             [dc '(-1 0 1)]
             #:when (not (and (= dr 0) (= dc 0)))) ; excluir la celda misma
    (let ([nr (+ r dr)]
          [nc (+ c dc)])
      (if (and (en-rango? nr nc filas cols)
               (= 1 (car (list-ref (list-ref tablero nr) nc))))
          1
          0))))

;; Colocar bombas en posiciones aleatorias
(define (colocar-bombas tablero num-bombas)
  (define filas (length tablero))
  (define cols (length (first tablero)))
  (define coords
    (for*/list ([r (in-range filas)]
                [c (in-range cols)])
      (list r c)))
  (define minas (take (shuffle coords) num-bombas))
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)])
      (if (member (list i j) minas)
          (list 1 0) ; bomba
          (list 0 0)))))

;; Calcular números del tablero
(define (calcular-numeros tablero)
  (define filas (length tablero))
  (define cols (length (first tablero)))
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)])
      (let ([celda (list-ref (list-ref tablero i) j)])
        (if (= (car celda) 1) ; es bomba
            celda
            (list 0 (contar-bombas-alrededor tablero i j)))))))

;; Generar tablero completo con bombas y números
(define (generar-mapa filas cols num-bombas)
  (define tablero-con-bombas (colocar-bombas (mapping filas cols) num-bombas))
  (define tablero-final
    (for/list ([i (in-range filas)])
      (for/list ([j (in-range cols)])
        (let ([celda (list-ref (list-ref tablero-con-bombas i) j)])
          (if (= (car celda) 1) ; si es bomba, la dejamos igual
              celda
              (list 0 (contar-bombas-alrededor tablero-con-bombas i j)))))))
  ;; imprimir en consola el tablero
  (imprimir-mapa tablero-final)
  tablero-final)

;; Imprimir el tablero en consola
(define (imprimir-mapa tablero)
  (for ([fila tablero])
    (for ([celda fila])
      (display
       (cond
         [(= (car celda) 1) "* "]                   ; bomba
         [(zero? (cadr celda)) "0"]                ; sin bombas alrededor → vacío
         [else (format "~a " (cadr celda))])))      ; 1–8 bombas alrededor
    (newline)))
