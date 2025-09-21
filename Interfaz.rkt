#lang racket
; =========================================================
; Interfaz.rkt
; Interfaz de usuario del Buscaminas
; =========================================================
(require "Controlador.rkt")
(require racket/gui)

(define Wancho 700)
(define Walto 700)
(define cellsize 65)
(define fila 8)
(define column 8)
(define mapa '())
(define celdas-reveladas '()) ; matriz para controlar qué celdas están reveladas
(define bomb-image (read-bitmap "C:/Users/Rachel/Desktop/BusCEMinas/bomba.png"))
(define label-bombas #f)
(define total-bombas 0)
(define canvas #f)

; ========================================
; Inicializar matriz de celdas reveladas
; ========================================
(define (inicializar-reveladas filas cols)
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)])
      #f)))

; ========================================
; Revelar celda
; ========================================
(define (revelar-celda fila-idx col-idx)
  (when (and (< fila-idx (length celdas-reveladas))
             (< col-idx (length (first celdas-reveladas))))
    (set! celdas-reveladas
          (for/list ([i (in-range (length celdas-reveladas))])
            (for/list ([j (in-range (length (first celdas-reveladas)))])
              (if (and (= i fila-idx) (= j col-idx))
                  #t
                  (list-ref (list-ref celdas-reveladas i) j))))))
  (when canvas (send canvas refresh)))

; ========================================
; Verificar si una celda está revelada
; ========================================
(define (celda-revelada? fila-idx col-idx)
  (and (< fila-idx (length celdas-reveladas))
       (< col-idx (length (first celdas-reveladas)))
       (list-ref (list-ref celdas-reveladas fila-idx) col-idx)))

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
             [y0 (* i cellsize)]
             [revelada? (celda-revelada? i j)])
        
        ;; fondo y borde
        (send dc set-brush (if revelada? "white" "lightgray") 'solid)
        (send dc set-pen "black" 1 'solid)
        (send dc draw-rectangle x0 y0 cellsize cellsize)

        (if revelada?
            ;; Celda revelada
            (cond
              [(= (car celda) 1) ; bomba
               (define txt "💣") ; icono de bomba
               (send dc set-font (make-font #:size (round (* 0.6 cellsize)) #:weight 'bold))
               (send dc set-text-foreground "black") ; bomba negra
               (define-values (tw th descent ascent)
                 (send dc get-text-extent txt))
               (define tx (+ x0 (/ (- cellsize tw) 2)))
               (define ty (+ y0 (/ (- cellsize th) 2)))
               (send dc draw-text txt tx ty)]
              [else ; mostrar número, incluso 0
               (define num (cadr celda))
               (define txt (number->string num))
               (send dc set-font (make-font #:size (round (* 0.5 cellsize)) #:weight 'bold))
               (define-values (tw th descent ascent)
                 (send dc get-text-extent txt))
               (define tx (+ x0 (/ (- cellsize tw) 2)))
               (define ty (+ y0 (/ (- cellsize th) 2)))
               (send dc set-text-foreground
                     (case num
                       [(0) "black"] [(1) "blue"] [(2) "green"] [(3) "red"]
                       [(4) "purple"] [(5) "maroon"] [(6) "turquoise"]
                       [(7) "black"] [(8) "gray"] [else "black"]))
               (send dc draw-text txt tx ty)])
            ;; Celda no revelada: efecto 3D
            (begin
              (send dc set-pen "white" 2 'solid)
              (send dc draw-line x0 y0 (+ x0 cellsize) y0)
              (send dc draw-line x0 y0 x0 (+ y0 cellsize))
              (send dc set-pen "darkgray" 2 'solid)
              (send dc draw-line (+ x0 cellsize) y0 (+ x0 cellsize) (+ y0 cellsize))
              (send dc draw-line x0 (+ y0 cellsize) (+ x0 cellsize) (+ y0 cellsize))))))))

; ========================================
; Ventanas
; ========================================
(define frame (new frame% [label "Buscaminas"] [width Wancho] [height Walto]))
(define Menuframe (new frame% [label "MENU - BUSCAMINAS"] [width 450] [height 350]))

; Paneles y botones del menú
(define menu-panel (new vertical-panel% [parent Menuframe] [alignment '(center center)]))
(define titulo-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))
(new message% [parent titulo-panel] [label "BUSCAMINAS"] [font (make-font #:size 28 #:weight 'bold)])
(define subtitulo-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))
(new message% [parent subtitulo-panel] [label "Seleccione tamaño y dificultad"] [font (make-font #:size 14)])
(new message% [parent menu-panel] [label ""])
(define size-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))
(define menu-filas
  (new choice% [parent size-panel] [label "Filas: "] [choices '("8" "9" "10" "12" "15")]
       [font (make-font #:size 14 #:weight 'bold)]
       [callback (lambda (c e) (set! fila (string->number (send c get-string (send c get-selection)))))]))
(define menu-columnas
  (new choice% [parent size-panel] [label "Columnas: "] [choices '("8" "9" "10" "12" "15")]
       [font (make-font #:size 14 #:weight 'bold)]
       [callback (lambda (c e) (set! column (string->number (send c get-string (send c get-selection)))))]))
(new message% [parent menu-panel] [label ""])
(define dificultad-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))
(new message% [parent dificultad-panel] [label "Dificultad"] [font (make-font #:size 16 #:weight 'bold)])
(define botones-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))

; Función iniciar juego
(define (iniciar-juego nivel)
  (set! total-bombas (que_nivel nivel))
  (set! mapa (generar_mapa fila column total-bombas))
  (set! celdas-reveladas (inicializar-reveladas fila column))
  (send label-bombas set-label (format "Bombas: ~a" total-bombas))
  (send Menuframe show #f)
  (send frame show #t)
  (send canvas refresh))

(new button% [parent botones-panel] [label "Fácil"] [min-width 100]
     [font (make-font #:size 12 #:weight 'bold)] [callback (lambda (b e) (iniciar-juego 1))])
(new button% [parent botones-panel] [label "Medio"] [min-width 100]
     [font (make-font #:size 12 #:weight 'bold)] [callback (lambda (b e) (iniciar-juego 2))])
(new button% [parent botones-panel] [label "Difícil"] [min-width 100]
     [font (make-font #:size 12 #:weight 'bold)] [callback (lambda (b e) (iniciar-juego 3))])

; Panel superior del juego
(define top-panel (new horizontal-panel% [parent frame] [min-height 40]))
(set! label-bombas (new message% [parent top-panel] [label "Bombas: 0"] [font (make-font #:size 14 #:weight 'bold)]))
(new button% [parent top-panel] [label "Reiniciar"] [callback (lambda (b e)
                                                               (send frame show #f)
                                                               (send Menuframe show #t))])

; ========================================
; Canvas con clics funcionales
; ========================================
(define my-canvas%
  (class canvas%
    (inherit get-dc)
    (define/override (on-event event)
      (when (send event button-down?)
        (define x (send event get-x))
        (define y (send event get-y))
        (define col (quotient x cellsize))
        (define fila (quotient y cellsize))
        (when (and (< fila (length mapa))
                   (< col (length (first mapa))))
          (revelar-celda fila col)))
      (super on-event event))
    (super-new)))

(set! canvas
      (new my-canvas%
           [parent frame]
           [min-width Wancho]
           [min-height (- Walto 60)]
           [paint-callback
            (lambda (c dc)
              (send dc set-brush "lightgray" 'solid)
              (send dc draw-rectangle 0 0 Wancho (- Walto 60))
              (when (not (null? mapa))
                (dibujar-tablero dc mapa)))]))

(send Menuframe show #t)
(send frame show #f)
