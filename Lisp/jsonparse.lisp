; LIBRARIES
(require "asdf")
(asdf:load-system :uiop)


(defvar macro-characters (list #\, #\: #\{ #\} #\[ #\]))
(defvar custom-delimiters 
            (list 
             " COMMA " " COLON " " BRACE-OPEN " " BRACE-CLOSE " 
             " SQUARE-OPEN " " SQUARE-CLOSE " ) )
(defvar blank-spaces 
  (list #\Space #\Newline #\Backspace #\Tab 
        #\Linefeed #\Page #\Return #\Rubout))

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
    (cond ((null agent) (values NIL 
                                "ERROR: Syntax Error - End of String reached" 
                                'error))
          ((eql agent 'SQUARE-OPEN) 
           (if (eql (second input) 'SQUARE-CLOSE)
               (values (cons 'JSONARRAY NIL) (cdr (cdr input)) 'noerror )
               (multiple-value-bind
                   (array-values chewable result-type)
                   (jsonparse-values (rest input))
                   (if (eql result-type 'noerror)
                       (values 
                        (cons 'JSONARRAY array-values) 
                        chewable 
                        result-type)
                       (values array-values chewable result-type) ) ) )))))

(defun jsonparse-values (input) 
  (let ((element (first input)))
    (multiple-value-bind
        (digested-single-value chewable result-type)
        (jsonparse-single-value input)
        (if (eql result-type 'noerror)
            (cond ((eql (first chewable) 'SQUARE-CLOSE)
                   (values 
                    (cons digested-single-value NIL) 
                    (rest chewable) 
                    result-type))
                  ((eql (first chewable) 'COMMA)
                   (multiple-value-bind
                       (digested-values 
                        remaining-unparsed 
                        recursive-result-type)
                       (jsonparse-values (rest chewable))
                       (values 
                        (cons digested-single-value digested-values) 
                        remaining-unparsed 
                        result-type) ))
                  ( T (values NIL "ERROR: jsonparse-values" 'error)) )
            (values digested-single-value chewable result-type) ))))

(defun jsonparse-single-value (input)
  (let ((element (first input)))
    (if (is-json-primitive element) 
        (values element (rest input) 'noerror)
        (json-recognizer input) )))

(defun jsonparse-object (input)
  (let ((agent (first input)))
    (cond ((null agent) (values NIL 
                                "ERROR: Syntax Error - End of String reached" 
                                'error))
          ((eql agent 'BRACE-OPEN)
           (if (eql (second input) 'BRACE-CLOSE)
               (values (cons 'JSONOBJ NIL) (cdr (cdr input)) 'noerror)
               (multiple-value-bind
                   (members chewable result-type)
                   (jsonparse-members (rest input))
                   (if (eql result-type 'noerror)
                       (values (cons 'JSONOBJ members) chewable result-type)
                       (values members chewable result-type) ))) )) ))

(defun jsonparse-members (input)
  (multiple-value-bind 
      (digested-pair chewable result-type)
      (jsonparse-pair input)
      (if (eql result-type 'noerror)
          (cond ((eql (first chewable) 'COMMA)
                 (multiple-value-bind 
                     (digested-members 
                      remaining-unparsed 
                      recursive-result-type)
                     (jsonparse-members (rest chewable))
                     (values 
                      (cons digested-pair digested-members) 
                      remaining-unparsed 
                      recursive-result-type) ))
                ((eql (first chewable) 'BRACE-CLOSE) 
                 (values (cons digested-pair NIL) 
                 (rest chewable) 
                 'noerror))
                (T (values 
                    NIL 
                    "ERROR: Syntax Error -  Missing closing brace or comma" 
                    'error)))
          (values digested-pair chewable result-type))))

(defun jsonparse-pair (input) 
  (let ((key (first input)))
       (if (and (typep key 'string) 
                (eql (second input) 'COLON )
                (not (null (third input))))
           (if (is-json-primitive (third input)) 
               (values 
                (list key (third input)) 
                (cdr (cdr (cdr input))) 'noerror )
               (multiple-value-bind
                   (recognized chewable result-type) 
                   (json-recognizer (cdr (cdr input)))
                   (if (eql result-type 'noerror)
                       (values (list key recognized) chewable result-type )
                       (values recognized chewable result-type) ) ))
           (values NIL 
                   (concatenate 'string
                                "ERROR: Syntax Error - "
                                (cond ((not (typep key 'string)) 
                                       "Pair-key is not a string")
                                      ((not (eql (second input) 'COLON)) 
                                       (format NIL "Missing colon after < ~d >" key))
                                      ( T "Expected value after \:" ) )) 
                   'error) )))


; JSONPARSE
(defun jsonparse (json) 
  (multiple-value-bind
      (digest error-string result-type)
      (json-recognizer json)
      (cond ((eql result-type 'noerror) digest )
            ((eql result-type 'error) error-string)
            ( T error-string)) ))

(defun json-recognizer (input)
  (let ((token (first input)))
    (cond 
     ((eql token 'BRACE-OPEN) (jsonparse-object input))
      ((eql token 'SQUARE-OPEN) (jsonparse-array input) )
      ( T (values input 
                  (format NIL "ERROR: Syntax Error - Unexpected token < ~d >" token ) 
                  'error) ) )))

; JSONREAD
(defun jsonread (filename)
  (let ((unparsed (uiop:read-file-string filename)))
    (jsonparse (json-normalize-string unparsed))
))




; UTILS
(defun json-normalize-string (toparse) 
   (tokenify-string 
    (replace-all-in-string macro-characters custom-delimiters toparse )))


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

(defun replace-all-in-charlist 
  (targets replacers charlist &optional &key (enable-replace T) (escaped NIL) )
  (let* ((suspect (first charlist)) 
         (target-index (position suspect targets))
         (next-escaped (eql suspect #\\))
         (toggle-enable-replace 
          (if (and (eql suspect #\") (not escaped)) (not enable-replace ) enable-replace)))
    (cond ((null charlist) NIL)
          ((or (null target-index) (not enable-replace))
           (cons
            (first charlist)
            (replace-all-in-charlist 
             targets 
             replacers 
             (rest charlist)
             :enable-replace toggle-enable-replace
             :escaped next-escaped) ))
          (enable-replace (concatenate 
                           'list 
                           (nth target-index replacers) 
                           (replace-all-in-charlist 
                            targets 
                            replacers 
                            (rest charlist) 
                            :enable-replace toggle-enable-replace
                            :escaped next-escaped))) ) ))

; PARAMETERS - TEST
(defparameter test-path-1 "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/input2.json")
(defparameter test-path-2 "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/real1.json")

;(defparameter test1 (json-normalize-string (jsonread test-path-1)) )
;(defparameter test2 (json-normalize-string (jsonread test-path-2)) )
