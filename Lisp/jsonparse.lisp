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
    (cond ((null agent) (error "ERROR: Syntax Error - End of String reached"))
          ((eql agent 'SQUARE-OPEN) 
           (if (eql (second input) 'SQUARE-CLOSE)
               (values (cons 'JSONARRAY NIL) (cdr (cdr input)))
               (multiple-value-bind
                   (array-values chewable)
                   (jsonparse-values (rest input))
                   (values (cons 'JSONARRAY array-values) chewable)) )))))

(defun jsonparse-values (input)
  (let ((element (first input)))
    (multiple-value-bind
        (digested-single-value chewable)
        (jsonparse-single-value input)
        (cond ((eql (first chewable) 'SQUARE-CLOSE)
               (values 
                (cons digested-single-value NIL)
                (rest chewable)))
              ((eql (first chewable) 'COMMA)
               (multiple-value-bind
                   (digested-values 
                    remaining-unparsed)
                   (jsonparse-values (rest chewable))
                 (values 
                  (cons digested-single-value digested-values)
                  remaining-unparsed) ))
              ( T (error "ERROR: Syntax Error - Missing closing square bracket or comma"))))))

(defun jsonparse-single-value (input)
  (let ((element (first input)))
    (if (is-json-primitive element) 
        (values element (rest input))
        (json-recognizer input) )))

(defun jsonparse-object (input)
  (let ((agent (first input)))
    (cond ((null agent) (error "ERROR: Syntax Error - End of String reached"))
          ((eql agent 'BRACE-OPEN)
           (if (eql (second input) 'BRACE-CLOSE)
               (values (cons 'JSONOBJ NIL) (cdr (cdr input)))
               (multiple-value-bind
                   (members chewable)
                   (jsonparse-members (rest input))
                 (values (cons 'JSONOBJ members) chewable)))))))

(defun jsonparse-members (input)
  (multiple-value-bind 
      (digested-pair chewable)
      (jsonparse-pair input)
    (cond ((eql (first chewable) 'COMMA)
           (multiple-value-bind 
               (digested-members 
                remaining-unparsed)
               (jsonparse-members (rest chewable))
             (values 
              (cons digested-pair digested-members) 
              remaining-unparsed) ))
          ((eql (first chewable) 'BRACE-CLOSE) 
           (values (cons digested-pair NIL) 
                   (rest chewable)))
          (T (error "ERROR: Syntax Error -  Missing closing brace or comma")))))

(defun jsonparse-pair (input) 
  (let ((key (first input)))
       (if (and (typep key 'string) 
                (eql (second input) 'COLON )
                (not (null (third input))))
           (if (is-json-primitive (third input)) 
               (values 
                (list key (third input)) 
                (cdr (cdr (cdr input))))
               (multiple-value-bind
                   (recognized chewable) 
                   (json-recognizer (cdr (cdr input)))
                 (values (list key recognized) chewable)))
           (error (concatenate 'string
                               "ERROR: Syntax Error - "
                               (cond ((not (typep key 'string)) 
                                      "Pair-key is not a string")
                                     ((not (eql (second input) 'COLON)) 
                                      (format NIL 
                                              "Missing colon after < ~d >" 
                                              key))
                                     ( T "Expected value after \:" )))))))


; JSONPARSE
(defun jsonparse (json) 
  (multiple-value-bind
      (digest empty-buffer)
      (json-recognizer (json-normalize-string json))
    (if (null empty-buffer)
        digest
      (error "ERROR: Syntax Error"))))

(defun json-recognizer (input)
  (let ((token (first input)))
    (cond 
     ((eql token 'BRACE-OPEN) (jsonparse-object input))
      ((eql token 'SQUARE-OPEN) (jsonparse-array input) )
      ( T  (error (format NIL 
                          "ERROR: Syntax Error - Unexpected token < ~d >" 
                          token))))))

;JSONACCESS
(defun jsonaccess (json &rest fields)
  (let ((key (first fields))
        (toptype (if (typep json 'cons) (first json) )))
    (cond ((null key) json)
          ((null toptype) "ERROR: Element cannot be accessed")
          ((and (eql toptype 'JSONOBJ) (typep key 'string)) 
           (let ((nexthop (jsonobj-get json key)))
             (if (null nexthop)
                 (format NIL "ERROR: Cannot access to attribute < ~d >" key)
                 (apply 'jsonaccess (cons nexthop (rest fields)))) ))
          ( (and (eql toptype 'JSONARRAY) (typep key 'number))
           (let ((nexthop (nth key (rest json)))) 
             (if (null nexthop)
                 (format NIL "ERROR: Index ~d out of bounds." key)
                 (apply 'jsonaccess (cons nexthop (rest fields))) )))
          ( T (format NIL "ERROR: < ~d > is not a valid identifier" key)) )))
; JSONREAD
(defun jsonread (filename)
  (let ((unparsed (uiop:read-file-string filename)))
    (jsonparse unparsed) ))

; JSONDUMP
(defun jsondump (json filename)
  (with-open-file (out filename 
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~d" (string-dumping-json json))) 
  filename)

(defun string-dumping-json (json)
  (let ((toptype (if (typep json 'cons) (first json)))
        (dump-members (lambda (member)
                        (cond ((and (is-json-primitive (second member))
                                    (not (typep (second member) 'symbol)))
                               (format nil "~s:~s" (first member) (second member)))
                              ; i symbols sono formattati in uppercase
                              ; nativamente in LISP
                              ((typep (second member) 'symbol) 
                               (format nil "~s:~d"
                                       (first member)
                                       (string-downcase 
                                        (string 
                                         (second member)))))
                              ( T (format nil "~s:~d" (first member) 
                                          (string-dumping-json 
                                           (second member)))) )))
        (dump-values (lambda (value)
                       (cond ((and (is-json-primitive value)
                                   (not (typep value 'symbol)))
                              (format nil "~s" value))
                             ((typep value 'symbol) 
                              (format nil "~d" (string-downcase (string value)) ))
                             ( T (format nil "~d" (string-dumping-json value)))))))
    (cond ((null toptype) "ERROR")
          ((eql toptype 'JSONOBJ) 
           (concatenate 'string 
                        "{" 
                        (join (mapcar dump-members (rest json)) ",")
                        "}")) 
          ((eql toptype 'JSONARRAY)
           (concatenate 'string 
                        "[" 
                        (join (mapcar dump-values (rest json)) ",")
                        "]"))) ))

;;; UTILS
; Returns the value identified by <key> in <jsonobj>, NIL otherwise. 
; Assuming that <jsonobj> is a list of key-pairs 
; always headed by CL 'JSONOBJ Symbol
(defun jsonobj-get (jsonobj key) 
  (if (eql (first jsonobj) 'JSONOBJ)
      (let* ((body (rest jsonobj)) (head (first body)))
        (cond ((null body) NIL)
              ((string= (first head) key) (second head) )
              ( T (jsonobj-get (cons 'JSONOBJ (rest body)) key)) ))))

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
  (targets replacers charlist &optional &key (enable-replace T) (escaped NIL))
  (let* ((suspect (first charlist)) 
         (target-index (position suspect targets))
         (next-escaped (eql suspect #\\))
         (toggle-enable-replace 
          (if (and (eql suspect #\") 
                   (not escaped)) 
              (not enable-replace ) 
              enable-replace)))
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

(defun join (list delimiter)
  (if list
      (format nil "~d~d" 
              (first list)
              (if (null (rest list)) ""
                  (format nil "~d~d" 
                          delimiter
                          (join (rest list) delimiter))))))

; PARAMETERS - TEST
(defparameter tp1 "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/input1.json")
(defparameter tp2 "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/real1.json")
(defparameter out-stream "C:/users/Asus/desktop/uni/2° anno/linguaggi di programmazione/progetto/lisp/testing/output.json")
;(defparameter test1 (jsonread test-path-1) )
;(defparameter test2 (json-normalize-string (jsonread test-path-2)) )
