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
(define column 8)
(define fila 8)
(define mapa '()) ; almacena la matriz actual

;============== VENTANA DE JUEGO ===========
(define frame (new frame%
                   [label "busCEMinas"]
                   [width Wancho]
                   [height Walto]
                   [alignment '(center center)]))
(send frame show #t) ;mostrar interfaz

;============== VENTANA DE MENU ===========
(define Menuframe (new frame%
                       [label "MENU-BUSCEMINAS"]
                       [width 889]
                       [height 550]
                       [alignment '(center center)]))
(send Menuframe show #t) ;mostrar interfaz

;panel para ordenar todos los elementos en la ventana
(define menu-panel
  (new group-box-panel% [label ""]
       [parent Menuframe]
       [min-width 300]
       [min-height 300]))

;labels
(define titulo
  (new message%
       [parent menu-panel]       
       [label "
BUSCEMINAS"]
       [font (make-font #:size 20)])) 

(define instruc1
  (new message%
       [parent menu-panel]  
       [label "INSERTE TAMAÑO DEL TABLERO
"]
       [min-width 300]   
       [min-height 100]
       [font (make-font #:size 14)]))

;choice para filas
(define menu-filas
  (new choice%
       [label "Filas: "]
       [parent menu-panel]
       [choices '("8" "9" "10" "11" "12" "13" "14" "15")]
       [callback (lambda (choice event)
                   (define idx (send choice get-selection))
                   (define str (send choice get-string idx))
                   (set! fila (string->number str)))]
       [font (make-font #:size 15)]))

;choice para columnas
(define menu-columnas
  (new choice%
       [label "Columnas: "]
       [choices '("8" "9" "10" "11" "12" "13" "14" "15")]
       [callback (lambda (choice event)
                   (define idx (send choice get-selection))
                   (define str (send choice get-string idx))
                   (set! column (string->number str)))]
       [parent menu-panel]
       [font (make-font #:size 15)]))

;label
(define level
  (new message%
       [parent menu-panel]    
       [label "
SELECCIONE EL NIVEL DE DIFICULTAD"]
       [min-width 400]   
       [min-height 100] 
       [font (make-font #:size 14)]))

;botones
(define botones-panel
  (new horizontal-panel%
       [parent menu-panel]
       [alignment '(center center)]))

(define btn-facil
  (new button%
       [label "Fácil"]
       [parent botones-panel]
       [callback (lambda (button event)
                   (set! mapa (generar_mapa fila column 10))
                   (que_nivel 1)
                   (send canvas refresh))]
       [min-width 100]
       [min-height 50]
       [horiz-margin 10]
       [font (make-font #:size 15)]))

(define btn-medio
  (new button%
       [label "Medio"]
       [parent botones-panel]
       [callback (lambda (button event)
                   (set! mapa (generar_mapa fila column 10))
                   (que_nivel 2)
                   (send canvas refresh))]
       [min-width 100]
       [min-height 50]
       [horiz-margin 10]
       [font (make-font #:size 15)]))

(define btn-dificil
  (new button%
       [label "Dificil"]
       [parent botones-panel]
       [callback (lambda (button event)
                   (set! mapa (generar_mapa fila column 10))
                   (que_nivel 3)
                   (send canvas refresh))]
       [min-width 100]
       [min-height 50]
       [horiz-margin 10]
       [font (make-font #:size 15)]))

;canva(espacio) para dibujar el mapa
(define canvas
  (new canvas%
       [parent frame]
       [min-width Wancho]
       [min-height (- Walto 60)]
       [paint-callback
        (lambda (canvas dc)
          (when (not (null? mapa))
            (dibujar-tablero dc mapa)))]))

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