#lang scheme
; =========================================================
; Controlador.rkt 
; Intermedio entre la lógica y la interfaz
; =========================================================
; ============== ARCHIVOS IMPORTADOS ======================
(require "Mapa.rkt")
; =============== FUNCIONES EXPORTADAS ====================
(provide generar_mapa)

(define generar_mapa (mapping 8 8))
