; LIBRARIES
(require "asdf")
(asdf:load-system :uiop)

;;; APP
; JSON ELEMENTS
(defun is-json-primitive (token)
  (or (typep token 'string)
      (typep token 'number) 
      (eql token 'true)
      (eql token 'false)
      (eql token 'null)) )


(defun jsonparse-array (input)
  (let ((agent (first input)))
    (cond ((null agent) (values NIL "ERROR: jsonparse-array" 'error))
          ((eql agent 'SQUARE-OPEN) 
           (if (eql (second input) 'SQUARE-CLOSE)
               (values (cons 'JSONARRAY NIL) (cdr (cdr input)) 'noerror )
               (multiple-value-bind
                   (array-values chewable result-type)
                   (jsonparse-values (rest input))
                   (if (eql result-type 'noerror)
                       (values (cons 'JSONARRAY array-values) chewable result-type)
                       (values array-values chewable result-type) ) ) )))))

(defun jsonparse-values (input) 
  (let ((element (first input)))
    (multiple-value-bind
        (digested-single-value chewable result-type)
        (jsonparse-single-value input)
        (if (eql result-type 'noerror)
            (cond ((eql (first chewable) 'SQUARE-CLOSE)
                   (values (cons digested-single-value NIL) (rest chewable) result-type))
                  ((eql (first chewable) 'COMMA)
                   (multiple-value-bind
                       (digested-values remaining-unparsed recursive-result-type)
                       (jsonparse-values (rest chewable))
                       (values (cons digested-single-value digested-values) remaining-unparsed result-type) ))
                  ( T (values NIL "ERROR: jsonparse-values" 'error)) )
            (values digested-single-value chewable result-type) ))))

(defun jsonparse-single-value (input)
  (let ((element (first input)))
    (if (is-json-primitive element) 
        (values element (rest input) 'noerror)
        (jsonparse-recognizer input) )))

(defun jsonparse-object (input)
  (let ((agent (first input)))
    (cond ((null agent) (values NIL "ERROR: jsonparse-object" 'error))
          ((eql agent 'BRACE-OPEN)
           (if (eql (second input) 'BRACE-CLOSE)
               (values (cons 'JSONOBJ NIL) (cdr (cdr input)) 'noerror)
               (multiple-value-bind
                   (members chewable result-type)
                   (jsonparse-members (rest input))
                   (if (eql result-type 'noerror)
                       (values (cons 'JSONOBJ members) chewable result-type)
                       (values members chewable result-type) ))) )) )) ; forwarding errors

(defun jsonparse-members (input)
  (multiple-value-bind 
      (digested-pair chewable result-type)
      (jsonparse-pair input)
      (if (eql result-type 'noerror)
          (cond ((eql (first chewable) 'COMMA)
                 (multiple-value-bind 
                     (digested-members remaining-unparsed recursive-result-type)
                     (jsonparse-members (rest chewable))
                     (values (cons digested-pair digested-members) remaining-unparsed recursive-result-type) ))
                ((eql (first chewable) 'BRACE-CLOSE) (values (cons digested-pair NIL) (rest chewable) 'noerror))
                (T (values NIL (concatenate 'string "ERROR: jsonparse-members, input:"  (string (first chewable))) 'error)))
          (values digested-pair chewable result-type))))

(defun jsonparse-pair (input) 
  (let ((key (first input)))
       (if (and (typep key 'string) 
                (eql (second input) 'COLON )
                (not (null (third input))))
           (if (is-json-primitive (third input)) 
               (values (list key (third input)) (cdr (cdr (cdr input))) 'noerror )
               (multiple-value-bind
                   (recognized chewable result-type) 
                   (json-recognizer (cdr (cdr input)))
                   (if (eql result-type 'noerror)
                       (values (list key recognized) chewable result-type )
                       (values recognized chewable result-type) ) ))
           (values NIL "ERROR: jsonparse-pair" 'error) )))


; JSONPARSE
(defun json-recognizer (input)
  (let ((token (first input)))
    (cond 
     ((eql token 'BRACE-OPEN) (jsonparse-object input))
      ((eql token 'SQUARE-OPEN) (jsonparse-array input) )
      ( T "ERROR: Syntax Error") )))

; JSONREAD
(defun jsonread (filename)
  (let (
        (unparsed (uiop:read-file-string filename))
        )
        unparsed
))




; UTILS
(defun tokenify-string (string &optional (start 0)) 
  (multiple-value-bind
      (token nextstart)
      (read-from-string string NIL NIL :start start)
    (if (null token)
        NIL
      (cons token (tokenify-string string nextstart  )))) )

(defun replace-all-in-string (targets replacers string) 
  (coerce 
   ( replace-all-in-charlist 
     targets
     replacers
     (coerce string 'list) )
   'string ))

(defun replace-all-in-charlist (targets replacers charlist)
  (let ((target-index (position (first charlist) targets)))
    (cond ((null charlist) NIL)
          ((null target-index)
           (cons
            (first charlist)
            (replace-all-in-charlist targets replacers (rest charlist)) ))
          (T (concatenate 'list (nth target-index replacers) (replace-all-in-charlist targets replacers (rest charlist)))) ) ))


(defun json-normalize-string (toparse) 
   (tokenify-string 
    (replace-all-in-string (list #\, #\: #\{ #\} #\[ #\]) (list " COMMA " " COLON " " BRACE-OPEN " " BRACE-CLOSE " " SQUARE-OPEN " " SQUARE-CLOSE " ) toparse )))


; PARAMETERS - TEST
(defparameter test-path-1 "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/input2.json")
(defparameter test-path-2 "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/real1.json")

(defparameter test1 (json-normalize-string (jsonread test-path-1)) )
(defparameter test2 (json-normalize-string (jsonread test-path-2)) )
