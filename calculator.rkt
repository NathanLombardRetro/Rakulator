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

; Define variables to store the current calculation state
(define current-num (make-parameter ""))
(define previous-num (make-parameter ""))
(define current-operator (make-parameter '()))

; Function to update the display
(define (update-display)
  (send display set-value (current-num)))

; Functions for each operation
(define (add num1 num2) (+ num1 num2))
(define (subtract num1 num2) (- num1 num2))
(define (multiply num1 num2) (* num1 num2))
(define (divide num1 num2) 
  (if (= num2 0) 'Error (/ num1 num2)))

; Function to perform the operation based on the current operator
(define (perform-operation)
  (let* ([num1 (string->number (previous-num))]
         [num2 (string->number (current-num))]
         [op (current-operator)])
    (cond
      [(not num1) 'Error] ; If previous-num is invalid, return error
      [(not num2) 'Error] ; If current-num is invalid, return error
      [(equal? op '+) (add num1 num2)]
      [(equal? op '-) (subtract num1 num2)]
      [(equal? op '*) (multiply num1 num2)]
      [(equal? op '/) (divide num1 num2)]
      [else 'Error])))

; Function to handle button clicks
(define (button-click value)
  (displayln (string-append "Button clicked: " value))) ; Debugging: Show clicked button value
  (cond
    [(equal? value "=") ; If "=" is clicked
     (with-handlers ([exn:fail? (λ (e) (current-num "Error"))]) ; Catch errors
       (let ([result (perform-operation)])  ; Perform the calculation
         (current-num (if (eq? result 'Error) 
                          "Error" 
                          (number->string result)))))]
     
     (set! previous-num (current-num)) ; Store result as previous-num for next operation
     )]

    [(equal? value "C") ; If "C" is clicked (clear)
     (current-num "")
     (previous-num "")
     (current-operator '())
     (update-display)] ; Clear everything and update the display

    [(member value '(+ - * /))  ; If the value is an operator
     (current-operator (string->symbol value))
     (previous-num (current-num)) ; Save the current number
     (current-num "")] ; Clear the current number for the next input

    [else ; For numbers and other values
     (current-num (string-append (current-num) value))] ; Append clicked value to current number
  ) ; Closing the cond expression
  (update-display)) ; Update the display

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