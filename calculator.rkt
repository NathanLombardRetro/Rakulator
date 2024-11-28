#lang racket
(require racket/gui)

;; Create the main frame (window) for the calculator
(define frame (new frame% [label "Calculator"] [width 300] [height 400]))

;; Create a vertical panel to hold the display and the buttons
(define panel (new vertical-panel% [parent frame]))

;; Create a text field for the display (non-editable)
(define display (new text-field% [parent panel] [label ""] [min-width 280] [min-height 40]))

;; Function to update the display with the current expression
(define (update-display text)
  (send display set-value text))

;; Define the current expression as an empty string
(define current-expression "")

;; Function to append to the current expression
(define (append-to-expression text)
  (set! current-expression (string-append current-expression text))
  (update-display current-expression))

;; Function to clear the current expression
(define (clear-expression)
  (set! current-expression "")
  (update-display current-expression))

;; Function to evaluate the current expression manually
(define (evaluate-expression)
  (define result
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (set! current-expression (format "Error: ~a" (exn-message e))))]) ;; Display full error message
                   (begin
                     ;; Parse and transform the expression
                     (define parsed-expression (parse-expression current-expression)) ;; Parse expression
                     
                     ;; Manually evaluate the expression
                     (define eval-result (manual-evaluate parsed-expression))

                     ;; Check if the result is a number
                     (if (number? eval-result)
                         (set! current-expression (number->string eval-result)) ;; Show result
                         (set! current-expression "Error"))))) ;; Show "Error" if it's not a number
  ;; Update display to show the result or error
  (update-display current-expression))

;; Helper function to parse the expression into a valid Racket list
(define (parse-expression expr)
  ;; Remove spaces from the expression
  (define cleaned-expr (string-replace expr " " "")) 
  (define match (regexp-match #px"^([0-9]+)([+\\-*/])([0-9]+)$" cleaned-expr))
  (if match
      ;; If match is successful, return the parsed expression in list form
      (list (string->symbol (second match))  ; operator
            (string->number (first match))   ; first operand
            (string->number (third match)))  ; second operand
      ;; If no match, return an error expression
      (error "Invalid Expression")))

;; Function to manually evaluate an expression
(define (manual-evaluate expr)
  (define operator (first expr))
  (define operand1 (second expr))
  (define operand2 (third expr))

  (cond
    [(eq? operator '+) (+ operand1 operand2)]
    [(eq? operator '-) (- operand1 operand2)]
    [(eq? operator '*) (* operand1 operand2)]
    [(eq? operator '/) (if (= operand2 0)
                           (error "Division by zero")
                           (/ operand1 operand2))]
    [(eq? operator '=) operand1]  ;; Return the first operand as the result
    [else (error "Unknown operator")])) ;; Handle unknown operators

;; Function to create a button with a label and callback
(define (create-button label callback)
  (new button% [parent panel] [label label] [callback callback]))

;; Function to handle button presses
(define (button-callback label event)
  (cond
    [(string=? label "C") (clear-expression)]         ;; Clear the display
    [(string=? label "=") (evaluate-expression)]      ;; Evaluate the expression
    [else (append-to-expression label)]))             ;; Append the label to the expression

;; Define the button labels in rows
(define buttons '(("7" "8" "9" "+")
                  ("4" "5" "6" "-")
                  ("1" "2" "3" "*")
                  ("0" "C" "=" "/")))

;; Create a panel for buttons (vertical layout)
(define button-panel (new vertical-panel% [parent panel]))

;; Create buttons for each row
(for-each (lambda (row)
            (define row-panel (new horizontal-panel% [parent button-panel])) ;; Create a horizontal panel for each row
            (for-each (lambda (label)
                        (create-button label (lambda (button event) (button-callback label event))))
                      row))
          buttons)

;; Show the frame (window) after all elements are created
(send frame show #t)