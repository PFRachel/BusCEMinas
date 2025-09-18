#lang scheme
; =========================================================
; Mapa.rkt 
; Lógica del mapa de juego
; =========================================================
; =============== FUNCIONES EXPORTADAS ====================
(provide mapping)
(provide level)

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
  