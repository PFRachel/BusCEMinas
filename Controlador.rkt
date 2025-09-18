#lang scheme
; =========================================================
; Controlador.rkt 
; Intermedio entre la lógica y la interfaz
; =========================================================
; ============== ARCHIVOS IMPORTADOS ======================
(require "Mapa.rkt")
; =============== FUNCIONES EXPORTADAS ====================
(provide generar_mapa)
(provide que_nivel)

(define (generar_mapa n m)
  (mapping n m))

(define (que_nivel n)
  (level n))