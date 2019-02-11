;;;; -*- Mode: Lisp -*-
;;;; LITTLE MAN COMPUTER
;;; by 829474 Sgro Mattia (no group)
	
(defun is-istr (string)
  (cond ((equal "add" string) )
        ((equal "sub" string) )
        ((equal "lda" string) )
        ((equal "sta" string) )
        ((equal "bra" string) )
        ((equal "brz" string) )
        ((equal "brp" string) )
        ((equal "inp" string) )
        ((equal "out" string) )
        ((equal "hlt" string) )
        ((equal "dat" string) )))

(defun is-label (string)
  (cond ((and (not(is-istr string))
              (not(digit-char-p (schar string 0) 10))))))

(defun is-digit (string)
  (let ((number (values (parse-integer string))))
    (cond ((and (< number 1000)
              (> number -1)
              )))))
(defun is-legal (string)
  (let ((number (values (parse-integer string))))
    (cond ((and (< number 100)
                (> number -1))))))

(defun replace-in-list (list newItem index)
  (fill list newItem :start index :end (+ index 1)))

(defun parse-comments (string)
  (string-trim " " (subseq (string-downcase string) 0 (search "//" string))))

(defun fill-no-op (lista)
  (let ((one (first lista)))
    (cond ((equal one "inp") '("inp" "0"))
          ((equal one "out") '("out" "0"))
          ((equal one "hlt") '("hlt" "0"))
          ((and (equal one "dat") (null (second lista)))'("dat" "0"))
          (T lista))))

(defun no-space (stringa)
  (split-sequence " " stringa :coalesce-separators :true))
;;iterators
(defun listing-istr (lista)
  (if (null lista)
      NIL
    (cons (no-space (car lista)) (listing-istr (cdr lista)))))

(defun list-parse-comments (lista)
  (if (null lista)
      NIL
    (cons (parse-comments (car lista)) (list-parse-comments (cdr lista)))))

(defun list-fill-no-op (lista)
  (if (null lista)
      NIL
    (cons (fill-no-op (car lista)) (list-fill-no-op (cdr lista)))))
;; creates a list of labels to jump to 
(defun labeler (lista)
  (let ((label (first lista)))
    (cond ((is-label label) (first lista) )
          ((is-istr label) () ))))
;; removes labels in front of istructions
(defun elider (lista)
  (let ((label (first lista)))
    (cond ((is-label label) (cdr lista))
          ( T lista))))
;;links everything to a list of labels, justify the code
(defun linker (lista labels)
  (let ((label (second lista)  ))
    (cond ((null label) lista)
          ((is-label label)
            (list
              (first lista)
              (write-to-string (search
                (list label)
                labels :test #'string-equal)) ))
          ((is-legal label) (list (first lista) label ))
          ((and
            (equal "dat" (first lista))
            (is-digit label))
            (list (first lista) label)))))
;;iterators
(defun list-labeler (lista)
  (if (null lista)
      NIL
    (cons (labeler (car lista)) (list-labeler (cdr lista)))))
(defun list-elider (lista)
  (if (null lista)
      NIL
    (cons (elider (car lista)) (list-elider (cdr lista)))))
(defun list-linker (lista labels)
  (if (null lista)
      NIL
    (cons (linker (car lista) labels) (list-linker (cdr lista) labels))))

;;calls parse-integer to parse everything into MACHINE CODES
(defun assembler (lista)
  (let ((istr (car lista)))
    (cond ((equal istr "add") (+ 100 (values (parse-integer (second lista)))))
          ((equal istr "sub") (+ 200 (values (parse-integer (second lista)))))
          ((equal istr "sta") (+ 300 (values (parse-integer (second lista)))))
          ((equal istr "lda") (+ 500 (values (parse-integer (second lista)))))
          ((equal istr "bra") (+ 600 (values (parse-integer (second lista)))))
          ((equal istr "brz") (+ 700 (values (parse-integer (second lista)))))
          ((equal istr "brp") (+ 800 (values (parse-integer (second lista)))))
          ((equal istr "inp") (+ 901 0))
          ((equal istr "out") (+ 902 0))
          ((equal istr "hlt") (+ 0 0))
          ((equal istr "dat") (+ 0   (values (parse-integer (second lista))))))
          ))
;;iterates assembler
(defun list-assembler (lista)
  (if (null lista)
      NIL
    (cons (assembler (car lista)) (list-assembler (cdr lista)))))
	
(defun lmc-run (filename in)
  (execution-loop(list 'STATE ':acc '0 ':pc '0 ':mem (lmc-load filename)
    ':in in ':out '() ':flag 'NOFLAG)))

(defun lmc-load (filename)
  (with-open-file (in filename :direction :input :if-does-not-exist :error)
    (parserino(loadfile in))))

(defun loadfile (stream)
  (let ((linea (read-line stream nil nil)))
    (if (null linea)
        NIL
    (cons linea (loadfile stream )))))
;; parses an asm file
(defun parserino (lista)
  (fill-memory
 (list-assembler 
   (list-fill-no-op (list-linker
                      (list-elider
                       (listing-istr
                        (remove "" (list-parse-comments lista)
                          :test #'string=)))
                      (list-labeler
                       (listing-istr
                        (remove "" (list-parse-comments lista)
                          :test #'string=))))))))
;;checks mem length and if state is halted or not						
(defun check-mem (stat)
  (cond ((and (equal (first stat) 'STATE)
	(= 100 (list-length (seventh stat))) ) T)))
;;used to fill memory with zeros if needed	
(defun fill-memory (lista)
  (cond ( (= (list-length lista) 100) lista)
        ( (> (list-length lista) 100) NIL)
        (T (append lista (make-list (- 100 (list-length lista))
          :initial-element 0)))))

(defun execution-loop (stat)
  (cond ((equal (nth 0 stat) 'HALTED-STATE) (nth 10 stat))
        ((check-mem stat) (execution-loop (one-instruction stat)))
        (T (cons 'F (nth 10 stat)))))
		
(defun one-instruction (stat)
  (let ((xx (nth (nth 4 stat) (nth 6 stat)))
        (acc (nth 2 stat))
        (pc (nth 4 stat))
        (mem (nth 6 stat))
        (in (nth 8 stat))
        (out (nth 10 stat))
        (flag (nth 12 stat)))
    (cond
     ;add 
     ((and (< xx 200) (> xx 99) ( > 1000 (+ (nth (- xx 100) mem) acc))) 
      (list
       'state :acc (+ (nth (- xx 100) mem) acc)
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG ))
     ;add MODULE
     ((and (< xx 200) (> xx 99) ( < 999 (+ (nth (- xx 100) mem) acc)))
      (list
       'state :acc (mod (+ (nth (- xx 100) mem) acc) 1000)
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out out
              :flag 'FLAG ))
     ;sub
     ((and (< xx 300) (> xx 199) ( > (- acc (nth (- xx 200) mem)) -1))
      (list
       'state :acc (- acc ( nth (- xx 200) mem))
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG ))
     ;sub module
     ((and (< xx 300) (> xx 199) ( < (- acc (nth (- xx 200) mem)) 0))
      (list
       'state :acc (mod (- acc (nth (- xx 200) mem)) 1000)
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG ))
     ; STA
     ((and (< xx 400) (> xx 299))
      (list
       'state :acc acc
              :pc (mod (+ pc 1) 100)
              :mem (replace-in-list mem acc (- xx 300))
              :in in
              :out out
              :flag 'NOFLAG ))
     ; LDA
     ((and (< xx 600) (> xx 499))
      (list
       'state :acc (nth (- xx 500) mem)
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG))
     ; BRA
     ((and (< xx 700) (> xx 599))
      (list
       'state :acc acc
              :pc (- xx 600)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG))
     ; BRZ
     ((and (< xx 800) (> xx 699) (equal flag 'NOFLAG) (= acc 0))
      (list 
       'state :acc acc
              :pc (- xx 700)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG))
     ; hlt
     ((and (< xx 100) (> xx -1))
      (list
       'halted-state :acc acc
       :pc pc
       :mem mem
       :in in
       :out out
       :flag flag))
     ; BRZ fail
     ((and (< xx 800) (> xx 699) (not (and (equal flag 'NOFLAG) (= acc 0)))) 
      (list 'state :acc acc
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG
              ))
     ; BRP
     ((and (< xx 900) (> xx 799) (equal flag 'NOFLAG))
      (list
       'state :acc acc
              :pc (- xx 800)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG))
     ; BRP fail
     ((and (< xx 900) (> xx 799) (equal flag 'FLAG))
      (list
       'state :acc acc
              :pc (mod (+ 1 pc) 100)
              :mem mem
              :in in
              :out out
              :flag 'NOFLAG))
     ; in fail
     ((and (= xx 901) (null in)) 'NIL-STATE)
     ; in 
     ((and (= xx 901) (not(equal nil (first in))))
      (list
       'state :acc (first in)
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in (cdr in)
              :out out
              :flag 'NOFLAG))
     ; out
     ((and (= xx 902))
      (list
       'state :acc acc
              :pc (mod (+ pc 1) 100)
              :mem mem
              :in in
              :out (append (list acc) out)
              :flag 'NOFLAG))
     ((and (< xx 100) (> xx -1))
      (list
       'halted-state :acc acc
       :pc pc
       :mem mem
       :in in
       :out out
       :flag flag))
     ((equal 'HALTED-STATE (first stat)) 'NIL)
     (T 'NIL))))