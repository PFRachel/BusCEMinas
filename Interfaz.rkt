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
  (send dc set-font (make-font #:size 20 #:family 'modern))

  (send dc set-background "white")
  (send dc clear)

  (define filas (length matriz))
  (define cols (length (first matriz)))

  (for ([i (in-range filas)])
    (for ([j (in-range cols)])
      (let* ([celda (list-ref (list-ref matriz i) j)]
              [contenido (cond
                          [(= (car celda) 1) "*"]          ; bomba
                          [(zero? (cadr celda)) ""]        ; vacío (antes era 0)
                          [else (number->string (cadr celda))])]
             [x0 (* j cellsize)]
             [y0 (* i cellsize)]
             [w cellsize]
             [h cellsize])
        ;; aquí corregimos
        (define-values (tw th descent ascent)
          (send dc get-text-extent contenido))
        (define tx (+ x0 (/ (- w tw) 2)))
        (define ty (+ y0 (/ (- h th) 2)))
        (send dc draw-rectangle x0 y0 w h)
        (send dc draw-text contenido tx ty)))))

