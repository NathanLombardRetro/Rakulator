#lang racket/gui

; Create the main frame
(define frame (new frame% [label "Calculator"] [width 300] [height 400]))

; Define a text field for displaying the input/output
(define display (new text-field%
                     [parent frame]
                     [enabled #f]
                     [label ""]))

; Define a variable to store the current calculation string
(define calc-str (make-parameter ""))

; Function to update the display
(define (update-display)
  (send display set-value (calc-str)))

; Function to handle button clicks
(define (button-click value)
  (cond
    [(equal? value "=")
     (with-handlers ([exn:fail? (λ (e) (calc-str "Error"))]) ; Catch errors
       (calc-str (number->string (eval (read (open-input-string (calc-str)))))))
     (update-display)]
    [(equal? value "C")
     (calc-str "")
     (update-display)]
    [else
     (calc-str (string-append (calc-str) value))
     (update-display)]))

; Helper to create buttons
(define (make-button parent label)
  (new button% [parent parent] [label label]
       [callback (λ (button event) (button-click label))]))

; Create a grid for buttons
(define button-panel (new horizontal-panel% [parent frame]))
(for ([row '(["7" "8" "9" "+"]
             ["4" "5" "6" "-"]
             ["1" "2" "3" "*"]
             ["C" "0" "=" "/"])])
  (define row-panel (new horizontal-panel% [parent button-panel]))
  (for ([label row])
    (make-button row-panel label)))

; Show the frame
(send frame show #t)