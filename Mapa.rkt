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

;; Matrices booleanas (reveladas / flags)
(define (inicializar-reveladas filas cols)
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)]) #f))) ; Al inicio ninguna celda esta revelada

(define (inicializar-flags filas cols)
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)]) #f))); Al inicio no hay banderas colocadas por player

(define (contar-banderas flags)
  (for/sum ([fila flags])
    (for/sum ([v fila]) (if v 1 0)))) ; Cuantas banderas hay en el tablero

;; Lectura/escritura en matriz booleana
(define (celda-revelada? reveladas r c)
  (and (< r (length reveladas))
       (< c (length (first reveladas)))
       (list-ref (list-ref reveladas r) c))) ; Consulta el estdo de revelado

(define (marcar-revelada reveladas r c) ; Devuelve matriz de reveladas con las celdas marcadas
  (for/list ([i (in-range (length reveladas))])
    (for/list ([j (in-range (length (first reveladas)))])
      (if (and (= i r) (= j c))
          #t
          (list-ref (list-ref reveladas i) j)))))

;; Revelar UNA celda 
(define (revelar-celda reveladas r c)
  (marcar-revelada reveladas r c))

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
          (loop rest R)] ; Si ya esta rvelada o hay bandera se la salta
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
