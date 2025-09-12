#lang scheme
; =========================================================
; Interfaz.rkt 
; Interfaz de usuario
; =========================================================
; ============= ARCHIVOS IMPORTADOS =======================
(require "Controlador.rkt")
(require 2htdp/image);libreria para importar imagen
(require (lib "graphics.ss" "graphics"));libreria de graficos
(open-graphics);abrir libreria

(define ventana (open-viewport "BUSCA-MINAS" 800 660));abrir ventana

;============== VARIABLES ===================
(define h 0) ;posicion en eje y
(define v 0) ;posicion en eje x
(define cellsize 80) ;tamaño en cada celda

;dibujar recursivamente las líneas del mapa
(for ([ h (in-range 11 660 80)]) ;variable en eje y
  ((draw-line ventana) (make-posn h 10) (make-posn h 650)"black")
  )
(for ([ v (in-range 11 660 80)]) ;variable en eje x
  ((draw-line ventana) (make-posn 10 v) (make-posn 650 v)"black")
  )

;proyectar números dentro de cada celda
(define (draw-numbers ventana matriz)
  (for ([fila matriz]
        [i (in-naturals 0)])
    (for ([celda fila]
          [j (in-naturals 0)])
      ((draw-viewport ventana)
       (format "~a" celda)
       (+ 25 (* j cellsize)) ;posicion x
       (+ 30 (* i cellsize)) ;posicion y
       "black"))))
