#lang racket
; =========================================================
; Interfaz.rkt
; Interfaz de usuario del Buscaminas
; =========================================================
(require "Controlador.rkt")
(require racket/gui)

(define Wancho 700)
(define Walto 700)
(define cellsize 44)
(define fila 8)
(define column 8)
(define mapa '())
(define celdas-reveladas '()) ; #t si está revelada
(define celdas-flags '())     ; #t si tiene bandera 🚩
(define juego-terminado? #f)
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

(define (inicializar-flags filas cols)
  (for/list ([i (in-range filas)])
    (for/list ([j (in-range cols)]) #f)))

; ========================================
; Contador de banderas y actualización de label
; ========================================
(define (contar-banderas)
  (for/sum ([fila celdas-flags])
    (for/sum ([v fila]) (if v 1 0))))

(define (actualizar-indicadores!)
  (define k (contar-banderas))
  (send label-bombas set-label (format "Bombas: ~a   |   🚩: ~a" total-bombas k)))

; ========================================
; Revelar / consultar estado
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
; Game Over / Victoria
; ========================================
(define (revelar-todas-las-bombas!)
  (define filas (length mapa))
  (define cols  (length (first mapa)))
  (for* ([i (in-range filas)] [j (in-range cols)])
    (when (= 1 (car (list-ref (list-ref mapa i) j)))
      (revelar-celda i j))))

;; Ganas si todas las NO-bomba están reveladas
(define (victoria?)
  (define filas (length mapa))
  (define cols  (length (first mapa)))
  (for*/and ([i (in-range filas)] [j (in-range cols)])
    (define celda (list-ref (list-ref mapa i) j))
    (define es-bomba (= 1 (car celda)))
    (define rev? (celda-revelada? i j))
    (or es-bomba rev?)))

; ========================================
; Flood fill (revelado en cascada para celdas con 0)
; ========================================
(define (marcar-revelada! r c)
  (set! celdas-reveladas
        (for/list ([i (in-range (length celdas-reveladas))])
          (for/list ([j (in-range (length (first celdas-reveladas)))])
            (if (and (= i r) (= j c))
                #t
                (list-ref (list-ref celdas-reveladas i) j))))))

(define (revelar-cascada fila-idx col-idx)
  (define filas (length mapa))
  (define cols  (length (first mapa)))
  (define pendientes (list (list fila-idx col-idx))) ; cola LIFO simple
  (let loop ()
    (when (not (null? pendientes))
      (define actual (car pendientes))
      (set! pendientes (cdr pendientes))
      (define r (car actual))
      (define c (cadr actual))
      (when (and (>= r 0) (< r filas) (>= c 0) (< c cols))
        (unless (or (celda-revelada? r c)
                    (list-ref (list-ref celdas-flags r) c)) ; no abrir si hay bandera
          (marcar-revelada! r c)
          (define celda (list-ref (list-ref mapa r) c))
          (when (and (= 0 (car celda)) (= 0 (cadr celda))) ; celda segura con número 0
            (define vecinos
              (list (list (- r 1) (- c 1)) (list (- r 1) c) (list (- r 1) (+ c 1))
                    (list r (- c 1))                       (list r (+ c 1))
                    (list (+ r 1) (- c 1)) (list (+ r 1) c) (list (+ r 1) (+ c 1))))
            (for ([p vecinos])
              (define vr (car p))
              (define vc (cadr p))
              (when (and (>= vr 0) (< vr filas) (>= vc 0) (< vc cols)
                         (not (celda-revelada? vr vc)))
                (set! pendientes (cons (list vr vc) pendientes)))))))
      (loop)))
  (void))

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
              (send dc draw-line x0 (+ y0 cellsize) (+ x0 cellsize) (+ y0 cellsize))
              ; bandera si existe
              (when (list-ref (list-ref celdas-flags i) j)
                (define txt "🚩")
                (send dc set-font (make-font #:size (round (* 0.6 cellsize)) #:weight 'bold))
                (define-values (tw th _d _a) (send dc get-text-extent txt))
                (define tx (+ x0 (/ (- cellsize tw) 2)))
                (define ty (+ y0 (/ (- cellsize th) 2)))
                (send dc set-text-foreground "red")
                (send dc draw-text txt tx ty))))))))

; ========================================
; Ventanas y menú
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
  (new choice% [parent size-panel] [label "Filas: "] [choices '("8" "9" "10" "11" "12" "13" "14" "15")]
       [font (make-font #:size 14 #:weight 'bold)]
       [callback (lambda (c e)
                   (set! fila (string->number (send c get-string (send c get-selection)))))]))

(define menu-columnas
  (new choice% [parent size-panel] [label "Columnas: "]
       [choices '("8" "9" "10" "11" "12" "13" "14" "15")]
       [font (make-font #:size 14 #:weight 'bold)]
       [callback (lambda (c e)
                   (set! column (string->number (send c get-string (send c get-selection)))))]))

(new message% [parent menu-panel] [label ""])
(define dificultad-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))
(new message% [parent dificultad-panel] [label "Dificultad"] [font (make-font #:size 16 #:weight 'bold)])
(define botones-panel (new horizontal-panel% [parent menu-panel] [alignment '(center center)]))

; ========================================
; Iniciar juego
; ========================================
(define (iniciar-juego nivel)
  (set! total-bombas (que_nivel nivel))
  (set! mapa (generar_mapa fila column total-bombas))
  (set! celdas-reveladas (inicializar-reveladas fila column))
  (set! celdas-flags (inicializar-flags fila column))
  (set! juego-terminado? #f)
  (actualizar-indicadores!)
  ; ajustar tamaño dinámico
  (define new-width (* column cellsize))
  (define new-height (+ 60 (* fila cellsize))) 
  (send frame resize new-width new-height)
  (send canvas min-width new-width)
  (send canvas min-height (* fila cellsize))
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
(set! label-bombas (new message% [parent top-panel] [label "Bombas: 0   |   🚩: 0"]
                        [font (make-font #:size 14 #:weight 'bold)]))
(new button% [parent top-panel] [label "Reiniciar"]
     [callback (lambda (b e)
                 (set! mapa '())
                 (set! celdas-reveladas '())
                 (set! celdas-flags '())
                 (set! juego-terminado? #f)
                 (set! total-bombas 0)
                 (send frame show #f)
                 (send Menuframe show #t)
                 (actualizar-indicadores!))])

; ========================================
; Canvas con clics funcionales
; ========================================
(define my-canvas%
  (class canvas%
    (inherit get-dc)
    (define/override (on-event event)
      (define tipo (send event get-event-type)) ; 'left-down o 'right-down
      (when (and (not juego-terminado?)
                 (or (eq? tipo 'left-down) (eq? tipo 'right-down)))
        (define x (send event get-x))
        (define y (send event get-y))
        (define col-idx (quotient x cellsize))
        (define row-idx (quotient y cellsize))
        (when (and (< row-idx (length mapa))
                   (< col-idx (length (first mapa))))
          (cond
            [(eq? tipo 'right-down) ; alternar bandera
             (unless (celda-revelada? row-idx col-idx)
               (set! celdas-flags
                     (for/list ([i (in-range (length celdas-flags))])
                       (for/list ([j (in-range (length (first celdas-flags)))])
                         (if (and (= i row-idx) (= j col-idx))
                             (not (list-ref (list-ref celdas-flags i) j))
                             (list-ref (list-ref celdas-flags i) j))))))
             (actualizar-indicadores!)
             (send canvas refresh)]
            [else ; left-down → revelar (con flood fill si es 0)
             (unless (list-ref (list-ref celdas-flags row-idx) col-idx)
               (define celda (list-ref (list-ref mapa row-idx) col-idx))
               (cond
                 [(= 1 (car celda))
                  (set! juego-terminado? #t)
                  (revelar-todas-las-bombas!)
                  (send canvas refresh)
                  (message-box "Fin del juego" "¡Boom! Pisaste una bomba.")]
                 [else
                  (if (= 0 (cadr celda))
                      (revelar-cascada row-idx col-idx) ; ← FLOOD FILL
                      (revelar-celda row-idx col-idx))
                  (send canvas refresh)
                  (when (victoria?)
                    (set! juego-terminado? #t)
                    (message-box "¡Ganaste!" "Has despejado todas las celdas. 🎉"))]))])))
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
