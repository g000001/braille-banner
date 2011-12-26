;;;; braille-banner.lisp

(cl:in-package :braille-banner.internal)

(def-suite braille-banner)

(in-suite braille-banner)

(defvar *k14*
  (kl:read-file-to-strings #p"k14.bdf"))

(defun flatten (list)
  (etypecase list
    (null '() )
    (cons (let ((head (car list))
                (tail (flatten (cdr list))) )
            (etypecase head
              (atom (cons head tail))
              (cons (append (flatten head) tail)) )))))

;;; dots
(defun dotsp (list)
  (and (consp list)
       (every (lambda (x)
                (typep x '(and (integer 0 8))))
              list)))

(deftype dots ()
  '(satisfies dotsp))

(defun bits-to-dots (bits)
  (let ((i 0)
        (ans '() ) )
    (dolist (e bits)
      (PUSH
       (case i
         (3 (case e
              (0 0)
              (1 8)
              (2 7)
              (3 (list 7 8)) ))
         (otherwise
          (ecase e
            (0 0)
            (1 (+ 3 (1+ i)))
            (2 (1+ i))
            (3 (list
                (+ 3 (1+ i))
                (1+ i) )))))
       ANS )
      (incf i) )
    (flatten ans)))

(test bits-to-dots
  (is (equal (bits-to-dots '(0 3 2 1))
             '(8 3 5 2 0))))

(defun list-to-braille-name (list)
  (let ((ans (map 'string
                  (lambda (e)
                    (character (princ-to-string e)))
                  (sort (remove 0 list) #'<))))
    (if (string= "" ans)
        #\BRAILLE_PATTERN_BLANK
        (name-char
         (format nil "BRAILLE_PATTERN_DOTS-~A" ans)))))

(test list-to-braille-name
  (is (equal (mapcar #'list-to-braille-name
                     '((0 0 0 0) (0 0 0 0) (7 6 5 4 1) (0 0 5 2 0)
               (8 3 5 2 0) (7 0 5 2 0) (0 6 5 2 0) (0 0 0 0)))
             '(#\BRAILLE_PATTERN_BLANK #\BRAILLE_PATTERN_BLANK
               #\BRAILLE_PATTERN_DOTS-14567 #\BRAILLE_PATTERN_DOTS-25
               #\BRAILLE_PATTERN_DOTS-2358 #\BRAILLE_PATTERN_DOTS-257
               #\BRAILLE_PATTERN_DOTS-256 #\BRAILLE_PATTERN_BLANK))))

(defun bits-to-braille-name (bits)
  (mapcar (lambda (x)
            (list-to-braille-name
             (bits-to-dots x)))
          bits))

(test bits-to-braille-name
  (is (equal (mapcar #'bits-to-braille-name
                     '(((0 0 0 0) (0 0 0 0) (3 1 1 2) (0 3 0 0)
                        (0 3 2 1) (0 3 0 2) (0 3 1 0) (0 0 0 0))
                       ((0 0 0 0) (3 2 0 2) (3 0 1 0) (3 2 1 3)
                        (3 1 0 3) (3 1 1 0) (3 2 1 1) (0 0 1 0))
                       ((0 0 0 0) (2 0 0 0) (0 2 0 0) (0 3 1 2)
                        (2 3 2 1) (0 1 2 0) (2 0 3 0) (0 0 0 0))
                       ((0 0) (0 3) (2 1) (3 0)
                        (2 0) (1 2) (0 3) (0 1))))
             '((#\BRAILLE_PATTERN_BLANK #\BRAILLE_PATTERN_BLANK
                #\BRAILLE_PATTERN_DOTS-14567 #\BRAILLE_PATTERN_DOTS-25
                #\BRAILLE_PATTERN_DOTS-2358 #\BRAILLE_PATTERN_DOTS-257
                #\BRAILLE_PATTERN_DOTS-256 #\BRAILLE_PATTERN_BLANK)
               (#\BRAILLE_PATTERN_BLANK #\BRAILLE_PATTERN_DOTS-1247
                #\BRAILLE_PATTERN_DOTS-146 #\BRAILLE_PATTERN_DOTS-124678
                #\BRAILLE_PATTERN_DOTS-14578 #\BRAILLE_PATTERN_DOTS-1456
                #\BRAILLE_PATTERN_DOTS-12468 #\BRAILLE_PATTERN_DOTS-6)
               (#\BRAILLE_PATTERN_BLANK #\BRAILLE_PATTERN_DOTS-1
                #\BRAILLE_PATTERN_DOTS-2 #\BRAILLE_PATTERN_DOTS-2567
                #\BRAILLE_PATTERN_DOTS-12358 #\BRAILLE_PATTERN_DOTS-35
                #\BRAILLE_PATTERN_DOTS-136 #\BRAILLE_PATTERN_BLANK)
               (#\BRAILLE_PATTERN_BLANK #\BRAILLE_PATTERN_DOTS-25
                #\BRAILLE_PATTERN_DOTS-15 #\BRAILLE_PATTERN_DOTS-14
                #\BRAILLE_PATTERN_DOTS-1 #\BRAILLE_PATTERN_DOTS-24
                #\BRAILLE_PATTERN_DOTS-25 #\BRAILLE_PATTERN_DOTS-5))
             )))

(defun hexs-to-bit-list (hexs)
  (let ((bs hexs)
        (ans '() ))
    (dolist (b bs)
      (let ((tem '() ))
        (dotimes (i 8)
          (push (ldb (byte 2 (* 2 i)) b)
                tem))
        (push (reverse tem)
              ans)))
    (apply #'mapcar #'list
           (nreverse ans))))

(test hexs-to-bit-list
  (is (equal (mapcar #'hexs-to-bit-list '((48 16336 4624 2336)
                                          (16380 9608 21584 5064)
                                          (8712 2016 14912 384)
                                          (1760 30748) ))
             '(((0 0 0 0) (0 0 0 0) (3 1 1 2) (0 3 0 0)
                (0 3 2 1) (0 3 0 2) (0 3 1 0) (0 0 0 0))
               ((0 0 0 0) (3 2 0 2) (3 0 1 0) (3 2 1 3)
                (3 1 0 3) (3 1 1 0) (3 2 1 1) (0 0 1 0))
               ((0 0 0 0) (2 0 0 0) (0 2 0 0) (0 3 1 2)
                (2 3 2 1) (0 1 2 0) (2 0 3 0) (0 0 0 0))
               ((0 0) (0 3) (2 1) (3 0)
                (2 0) (1 2) (0 3) (0 1))))))

(defun hexs-to-braille-string (hexs)
  (COERCE (reverse
           (bits-to-braille-name
            (hexs-to-bit-list hexs)))
          'STRING))

(test hexs-to-braille-string
  (is (equal (mapcar #'hexs-to-braille-string
                     '((48 16336 4624 2336)
                       (16380 9608 21584 5064)
                       (8712 2016 14912 384)
                       (1760 30748)))
             '("⠀⠲⡒⢖⠒⡹⠀⠀" "⠠⢫⠹⣙⣫⠩⡋⠀" "⠀⠥⠔⢗⡲⠂⠁⠀" "⠐⠒⠊⠁⠉⠑⠒⠀"))))

(defun euc-octets-to-jis-code (octets)
  ;; http://d.hatena.ne.jp/snaka72/20100710/SUMMARY_ABOUT_JAPANESE_CHARACTER_CODE
  (let ((ku (aref octets 0))
        (ten (aref octets 1)))
    (+ (* #x100 (+ (- #xa0) #x20 ku))
       (+ (- #xa0) #x20 ten)) ))

(test euc-octets-to-jis-code
  (is (= (euc-octets-to-jis-code #(176 166))
         12326)))

(defun char-to-jis-code (char)
  (euc-octets-to-jis-code
   (babel:string-to-octets (string char) :encoding :eucjp)))

(test char-to-jis-code
  (is (= (char-to-jis-code #\愛)
         12326)))

(defun char-to-font (char &aux (k14 *k14*))
  (let ((code (char-to-jis-code char)))
    (subseq (member (format nil
                            "ENCODING ~A"
                            code)
                    k14 :test #'string=)
            5 19)))

(test char-to-font
  (is (equal (char-to-font #\愛)
             '("0030" "3FD0" "1210" "0920" "3FFC" "2588" "5450"
               "13C8" "2208" "07E0" "3A40" "0180" "06E0" "781C"))))

(defun char-to-braille (char)
  (mapcar #'hexs-to-braille-string
          (sl:group (mapcar (lambda (x)
                              (parse-integer x :radix 16))
                            (char-to-font char))
                    4) ))

(test char-to-braille
  (is (equal (char-to-braille #\愛)
             '("⠀⠲⡒⢖⠒⡹⠀⠀" "⠠⢫⠹⣙⣫⠩⡋⠀" "⠀⠥⠔⢗⡲⠂⠁⠀" "⠐⠒⠊⠁⠉⠑⠒⠀"))))

(defun string-to-braille (str)
  (fare-utils:join-strings
   (apply #'mapcar (lambda (&rest e)
                     (apply #'concatenate 'string e))
          (map 'list #'char-to-braille str))
   :separator #\Newline))

(test string-to-braille
  (is (string= (string-to-braille "龍龍龍龍")
           "⠠⡤⠧⡤⣗⣒⡂⠀⠠⡤⠧⡤⣗⣒⡂⠀⠠⡤⠧⡤⣗⣒⡂⠀⠠⡤⠧⡤⣗⣒⡂⠀
⠐⣚⣚⡒⣖⣒⡃⠀⠐⣚⣚⡒⣖⣒⡃⠀⠐⣚⣚⡒⣖⣒⡃⠀⠐⣚⣚⡒⣖⣒⡃⠀
⠀⣗⣒⡇⣗⣒⡂⠀⠀⣗⣒⡇⣗⣒⡂⠀⠀⣗⣒⡇⣗⣒⡂⠀⠀⣗⣒⡇⣗⣒⡂⠀
⠀⠃⠐⠃⠑⠒⠚⠀⠀⠃⠐⠃⠑⠒⠚⠀⠀⠃⠐⠃⠑⠒⠚⠀⠀⠃⠐⠃⠑⠒⠚⠀")))
;;; eof
