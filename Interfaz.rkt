 #lang racket
; =========================================================
; Interfaz.rkt 
; Interfaz de usuario
; =========================================================
; ============= ARCHIVOS IMPORTADOS =======================
(require "Controlador.rkt")
(require racket/gui) ;biblioteca para interfaz

;============== VARIABLES ===================
(define Wancho 700) ;ancho de la ventana
(define Walto 660) ;alto de la ventana
(define cellsize 70) ;tamaño en cada celda

;============== VENTANA DE JUEGO ===========
(define frame (new frame%
                   [label "BUSCA-MINAS"]
                   [width Wancho]
                   [height Walto]
                   [alignment '(center center)]))
(send frame show #t) ;mostrar interfaz

;canva(espacio) para dibujar el mapa
(define canvas
  (new canvas%
       [parent frame]
       [min-width Wancho]
       [min-height (- Walto 60)]
       [paint-callback (lambda (canvas dc)
                         (dibujar-tablero dc generar_mapa))]))
;dibujar el mapa
(define (dibujar-tablero dc matriz)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "white" 'transparent)
  (send dc set-font (make-font #:size 16 #:family 'modern))

  (send dc set-background "white")
  (send dc clear)

  ;dibujar líneas de la cuadrícula
  (for ([i (in-range 10 (+ 10 (* 9 cellsize)) cellsize)])
    (send dc draw-line i 10 i 570)   ; vertical
    (send dc draw-line 10 i 570 i))  ; horizontal

  ;dibujar contenido de las celdas
  (for ([fila matriz]
        [i (in-naturals)])
    (for ([celda fila]
          [j (in-naturals)])
      (let* ([contenido (cadr celda)]
             [x (+ 25 (* j cellsize))]
             [y (+ 25 (* i cellsize))])
        (send dc draw-text (format "~a" contenido) x y)))))
