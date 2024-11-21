#lang racket/gui

; Create the main frame
(define frame 
  (new frame% 
       [label "Calculator"] 
       [width 300] 
       [height 400]))

; Define a text field for displaying the input/output
(define display 
  (new text-field% 
       [parent frame]
       [enabled #f]
       [label ""]))

; Define a variable to store the current calculation string
(define calc-str (make-parameter ""))

; Function to update the display
(define (update-display)
  (send display set-value (calc-str)))

; Function to manually parse and calculate basic arithmetic expressions
; Function to manually parse and calculate basic arithmetic expressions
; Function to manually parse and calculate basic arithmetic expressions
(define (calculate expr)
  (define operators '(+ - * /))

  ; Helper function to extract tokens from the expression
  (define (extract-tokens expr)
    (define (helper expr tokens current)
      (cond
        [(empty? expr) (if (empty? current) tokens (append tokens (list current)))] ; End of string
        [(char-numeric? (car expr)) ; If the current char is a number
         (helper (cdr expr) tokens (string-append current (string (car expr))))] ; Append to current number
        [(member (car expr) operators) ; If the current char is an operator
         (helper (cdr expr) (append tokens (list current (string (car expr)))) "")] ; Add the number and operator to tokens
        [else (helper (cdr expr) tokens current)])) ; Otherwise continue

    (helper (string->list expr) '() "")) ; Start from the beginning of the expression

  ; Split the expression into tokens (numbers and operators)
  (define tokens (extract-tokens expr))

  ; Debugging: Show tokens
  (displayln (string-append "Tokens: " (format "~a" tokens)))

  ; Function to apply an operator
  (define (apply-op num1 op num2)
    (cond
      [(equal? op '+) (+ num1 num2)]
      [(equal? op '-) (- num1 num2)]
      [(equal? op '*) (* num1 num2)]
      [(equal? op '/) (if (= num2 0) 'Error (/ num1 num2))])) ; Prevent division by zero

  ; Process the tokens
  (define (process-tokens tokens result)
    (if (null? tokens)
        result
        (let* ([num1 (string->number (car tokens))]
               [op (cadr tokens)]
               [num2 (string->number (caddr tokens))])
          (let* ([new-result (apply-op num1 op num2)]
                 [remaining-tokens (cdddr tokens)])
            (process-tokens remaining-tokens new-result)))))

  ; If there are no tokens, return 0
  (if (null? tokens)
      0
      (process-tokens tokens 0)))

; Function to handle button clicks
(define (button-click value)
  (displayln (string-append "Button clicked: " value)) ; Debugging: Show clicked button value
  (cond
    [(equal? value "=")
     (with-handlers ([exn:fail? (λ (e) (calc-str "Error"))]) ; Catch errors
       (let ([expr (calc-str)])  ; Get the current calculation expression
         (displayln (string-append "Evaluating: " expr)) ; Debugging: Print expression
         (if (string=? expr "")
             (calc-str "")  ; If empty expression, clear the display
             (let ([result (calculate expr)])  ; Perform the calculation
               (displayln (string-append "Result: " (number->string result))) ; Debugging: Print result
               (calc-str (number->string result))))))] ; Set result to display

    [(equal? value "C")
     (calc-str "") ; Clear the expression
     (update-display)] ; Update the display
    [else
     (calc-str (string-append (calc-str) value)) ; Append clicked value to expression
     (update-display)])) ; Update the display


; Helper to create buttons
(define (make-button parent label)
  (new button% 
       [parent parent] 
       [label (if (number? label) (number->string label) (symbol->string label))] ; Convert numbers and symbols to strings
       [callback (λ (button event) 
                   (button-click (if (number? label) 
                                    (number->string label) 
                                    (symbol->string label))))])) 

; Create a vertical panel to hold rows
(define button-panel
  (new vertical-panel% 
       [parent frame])) 

; Define button labels for the grid
(define button-labels
  '((7 8 9 +)
    (4 5 6 -)
    (1 2 3 *)
    (C 0 = /)))

; Create rows of buttons and add them to the vertical panel
(for-each
 (λ (row)
   (define row-panel 
     (new horizontal-panel% 
          [parent button-panel]))
   (for-each 
    (λ (label) 
      (make-button row-panel label)) 
    row))
 button-labels)

; Show the frame
(send frame show #t)