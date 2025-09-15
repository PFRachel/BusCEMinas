#lang racket
; =========================================================
; Mapa.rkt 
; Lógica del mapa de juego
; =========================================================
; =============== FUNCIONES EXPORTADAS ====================
(provide mapping)

; crear una matriz que contiene una tupla
; (es bomba(1/0) y cant bombas alrededor)
(define (mapping i j ) ;toma parámetros de tamaño del mapa
  (build-list i ;longitud de filas
    (lambda (_)
      (build-list j (lambda (_) (list 0 0)))))) ;[MODIFICAR] llenar cada celda con los elementos
  
  