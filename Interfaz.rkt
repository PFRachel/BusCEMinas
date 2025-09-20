#lang racket
; =========================================================
; Interfaz.rkt
; Interfaz de usuario del Buscaminas
; =========================================================
(require "Controlador.rkt")
(require racket/gui)

(define Wancho 700)
(define Walto 700)
(define cellsize 60)
(define fila 8)
(define column 8)
(define mapa '())
(define bomb-image (read-bitmap "C:/Users/Rachel/Desktop/BusCEMinas/bomba.png"))
(define label-bombas #f)
(define total-bombas 0)

; ========================================
; Dibujar tablero
; ========================================
(define (dibujar-tablero dc matriz)
  (define filas (length matriz))
  (define cols (length (first matriz)))
  (for ([i (in-range filas)])
    (for ([j (in-range cols)])
      (let* ([celda (list-ref (list-ref matriz i) j)]
             [x0 (* j cellsize)]
             [y0 (* i cellsize)])
        ;; dibujar celda
        (send dc set-pen "black" 1 'solid)
        (send dc set-brush "white" 'solid)
        (send dc draw-rectangle x0 y0 cellsize cellsize)

        ;; dibujar contenido
        (cond
          [(= (car celda) 1) ; bomba
           (define w (/ cellsize 2))
           (define h (/ cellsize 2))
           (define x (+ x0 (/ (- cellsize w) 2)))
           (define y (+ y0 (/ (- cellsize h) 2)))
           (send dc draw-bitmap bomb-image x y)] ; bitmap original, no escalamos
          [(positive? (cadr celda))
           (define txt (number->string (cadr celda)))
           (define-values (tw th descent ascent)
             (send dc get-text-extent txt))
           (define tx (+ x0 (/ (- cellsize tw) 2)))
           (define ty (+ y0 (/ (- cellsize th) 2)))
           (send dc draw-text txt tx ty)])))))

; ========================================
; Ventanas
; ========================================
(define frame (new frame% [label "Buscaminas"] [width Wancho] [height Walto]))
(define Menuframe (new frame% [label "MENU - BUSCAMINAS"] [width 400] [height 300]))

; Panel de menu
(define menu-panel (new vertical-panel% [parent Menuframe]))

(new message% [parent menu-panel] [label "\nBUSCAMINAS"] [font (make-font #:size 20 #:weight 'bold)])
(new message% [parent menu-panel] [label "Seleccione tamaño y dificultad"] [font (make-font #:size 12)])

; Choice para filas y columnas
(define menu-filas
  (new choice%
       [parent menu-panel]
       [label "Filas: "]
       [choices '("8" "9" "10" "12" "15")]
       [callback (lambda (c e) (set! fila (string->number (send c get-string (send c get-selection)))))]))

(define menu-columnas
  (new choice%
       [parent menu-panel]
       [label "Columnas: "]
       [choices '("8" "9" "10" "12" "15")]
       [callback (lambda (c e) (set! column (string->number (send c get-string (send c get-selection)))))]))

; Panel de dificultad
(new message% [parent menu-panel] [label "\nDificultad"] [font (make-font #:size 12 #:weight 'bold)])
(define botones-panel (new horizontal-panel% [parent menu-panel]))

; Iniciar juego
(define (iniciar-juego nivel)
  (set! total-bombas (que_nivel nivel))
  (set! mapa (generar_mapa fila column total-bombas))
  (send label-bombas set-label (format "Bombas: ~a" total-bombas))
  (send Menuframe show #f)
  (send frame show #t)
  (send canvas refresh))

(new button% [parent botones-panel] [label "Fácil"] [callback (lambda (b e) (iniciar-juego 1))])
(new button% [parent botones-panel] [label "Medio"] [callback (lambda (b e) (iniciar-juego 2))])
(new button% [parent botones-panel] [label "Difícil"] [callback (lambda (b e) (iniciar-juego 3))])

; Panel superior del juego
(define top-panel (new horizontal-panel% [parent frame] [min-height 40]))
(set! label-bombas (new message% [parent top-panel] [label "Bombas: 0"] [font (make-font #:size 14 #:weight 'bold)]))
(new button% [parent top-panel] [label "Reiniciar"] [callback (lambda (b e)
                                                               (send frame show #f)
                                                               (send Menuframe show #t))])

; Canvas del tablero
(define canvas
  (new canvas%
       [parent frame]
       [min-width Wancho]
       [min-height (- Walto 60)]
       [paint-callback
        (lambda (c dc)
          ;; fondo gris
          (send dc set-brush "lightgray" 'solid)
          (send dc draw-rectangle 0 0 Wancho (- Walto 60))
          ;; dibujar tablero
          (when (not (null? mapa))
            (dibujar-tablero dc mapa)))]))

(send Menuframe show #t)
(send frame show #f)
