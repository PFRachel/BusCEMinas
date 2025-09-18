#lang racket
; =========================================================
; Controlador.rkt 
; Intermedio entre la lógica y la interfaz
; =========================================================
; ============== ARCHIVOS IMPORTADOS ======================
(require "Mapa.rkt")
; =============== FUNCIONES EXPORTADAS ====================
(provide generar_mapa)
(provide que_nivel)

(define (generar_mapa  f c b)
  (generar-mapa f c b))

(define (que_nivel n)
  (level n))