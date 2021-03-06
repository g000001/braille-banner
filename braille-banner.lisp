;;;; braille-banner.lisp

(cl:in-package :braille-banner.internal)

(def-suite braille-banner)

(in-suite braille-banner)

(defvar *k14*
  (kl:read-file-to-strings (asdf:system-relative-pathname :braille-banner "k14.bdf")))

;;; borrowed from "on lisp"
(defun group (source n)
  "groups every n elements together into new sublists.
   e.g. (group '(1 a 2 b) 2) -> ((1 a) (2 b))"
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil)
	nil)))


(test group
  (is (equal '((1 2) (3 4)) 
             (group '(1 2 3 4) 2)))
  (signals (error) (group '(8 8 8 8) 0)))


(defun bitsp (list)
  (and (listp list)
       ;; (= 4 (length list))
       (every (lambda (x)
                (typep x '(integer 0 3)))
              list)))

;;; bits
(deftype bits ()
  '(satisfies bitsp))

(test bits
  (is-true (typep '() 'bits))
  (is-false (typep '(a) 'bits))
  (is-true (typep '(0 0 0 0) 'bits))
  (is-true (typep '(0 0 0 0 0) 'bits))
  (is-true (typep '(3 3 3 3) 'bits))
  (is-false (typep '(4 4 4 4) 'bits))
  (is-false (typep 'a 'bits)))

;;; dots
(defun dotsp (list)
  (and (listp list)
       (every (lambda (x)
                (typep x '(and (integer 0 8))))
              list)))

(deftype dots ()
  '(satisfies dotsp))

(defun bits-to-dots (bits)
  (declare (type bits bits)
           (values dots))
  (let ((i -1))
    (mapcan (lambda (e)
              (case (incf i)
                (3 (case e
                     (0 (list 0))
                     (1 (list 8))
                     (2 (list 7))
                     (3 (list 7 8)) ))
                (otherwise
                 (ecase e
                   (0 (list 0))
                   (1 (list (+ 3 (1+ i))))
                   (2 (list (1+ i)))
                   (3 (list (+ 3 (1+ i))
                            (1+ i) ))))))
            bits)))

(test bits-to-dots
  (is (null (set-difference (bits-to-dots '(0 3 2 1))
                            '(8 3 5 2 0)))))

(declaim (ftype (function (dots) (values character &optional))
                dots-to-braille-char))

(defun dots-to-braille-char (dots)
  (let ((ans (map 'string
                  (lambda (e)
                    (character (princ-to-string e)))
                  (sort (remove 0 dots) #'<))))
    (if (string= "" ans)
        (character-named "BRAILLE_PATTERN_BLANK")
        (character-named
         (format nil "BRAILLE_PATTERN_DOTS-~A" ans)))))

(test dots-to-braille-char
  (is (equal (mapcar #'dots-to-braille-char
                     '((0 0 0 0) (0 0 0 0) (7 6 5 4 1) (0 0 5 2 0)
               (8 3 5 2 0) (7 0 5 2 0) (0 6 5 2 0) (0 0 0 0)))
             (mapcar #'character-named 
                     '("BRAILLE_PATTERN_BLANK" "BRAILLE_PATTERN_BLANK"
                       "BRAILLE_PATTERN_DOTS-14567" "BRAILLE_PATTERN_DOTS-25"
                       "BRAILLE_PATTERN_DOTS-2358" "BRAILLE_PATTERN_DOTS-257"
                       "BRAILLE_PATTERN_DOTS-256" "BRAILLE_PATTERN_BLANK")))))

(declaim (ftype (function (bits) character)
                bits-to-braille-char))

(defun bits-to-braille-char (bits)
  (dots-to-braille-char (bits-to-dots bits)))

(defun font-line-p (font-line)
  (and (listp font-line)
       (every (lambda (x) (typep x 'bits))
              font-line)))

(deftype font-line ()
  `(satisfies font-line-p))

(declaim (ftype (function (font-line) list)
                font-line-to-letter-line-chars))

(defun font-line-to-letter-line-chars (font-line)
  (mapcar #'bits-to-braille-char font-line))

(test font-line-to-letter-line-chars
  (is (equal (mapcar #'font-line-to-letter-line-chars
                     '(((0 0 0 0) (0 0 0 0) (3 1 1 2) (0 3 0 0)
                        (0 3 2 1) (0 3 0 2) (0 3 1 0) (0 0 0 0))
                       ((0 0 0 0) (3 2 0 2) (3 0 1 0) (3 2 1 3)
                        (3 1 0 3) (3 1 1 0) (3 2 1 1) (0 0 1 0))
                       ((0 0 0 0) (2 0 0 0) (0 2 0 0) (0 3 1 2)
                        (2 3 2 1) (0 1 2 0) (2 0 3 0) (0 0 0 0))
                       ((0 0) (0 3) (2 1) (3 0)
                        (2 0) (1 2) (0 3) (0 1))))
             (fare-utils:cons-tree-map #'character-named
                          '(("BRAILLE_PATTERN_BLANK"
 "BRAILLE_PATTERN_BLANK" "BRAILLE_PATTERN_DOTS-14567"
                             "BRAILLE_PATTERN_DOTS-25" "BRAILLE_PATTERN_DOTS-2358"
                             "BRAILLE_PATTERN_DOTS-257" "BRAILLE_PATTERN_DOTS-256"
                             "BRAILLE_PATTERN_BLANK")
                            ("BRAILLE_PATTERN_BLANK"
                             "BRAILLE_PATTERN_DOTS-1247"
                             "BRAILLE_PATTERN_DOTS-146" "BRAILLE_PATTERN_DOTS-124678"
                             "BRAILLE_PATTERN_DOTS-14578" "BRAILLE_PATTERN_DOTS-1456"
                             "BRAILLE_PATTERN_DOTS-12468" "BRAILLE_PATTERN_DOTS-6")
                            ("BRAILLE_PATTERN_BLANK"
                             "BRAILLE_PATTERN_DOTS-1" "BRAILLE_PATTERN_DOTS-2"
                             "BRAILLE_PATTERN_DOTS-2567" "BRAILLE_PATTERN_DOTS-12358"
                             "BRAILLE_PATTERN_DOTS-35" "BRAILLE_PATTERN_DOTS-136"
                             "BRAILLE_PATTERN_BLANK")
                            ("BRAILLE_PATTERN_BLANK"
                             "BRAILLE_PATTERN_DOTS-25" "BRAILLE_PATTERN_DOTS-15"
                             "BRAILLE_PATTERN_DOTS-14" "BRAILLE_PATTERN_DOTS-1" "BRAILLE_PATTERN_DOTS-24"
                             "BRAILLE_PATTERN_DOTS-25" "BRAILLE_PATTERN_DOTS-5"))))))


(defun font-data-to-font-lines (font-data)
  (group font-data 4))

(defun font-line-to-letter-line (font-line)
  (let ((ans '() ) )
    (dolist (b font-line)
      (let ((tem '() ))
        (dotimes (i 8)
          (push (ldb (byte 2 (* 2 i)) b)
                tem ))
        (push (reverse tem)
              ans )))
    (apply #'mapcar #'list
           (nreverse ans) )))

(test font-data-to-letter-bits-line
  (is (equal (font-line-to-letter-line '(48 16336 4624 2336))
             '((0 0 0 0) (0 0 0 0) (3 1 1 2) (0 3 0 0)
               (0 3 2 1) (0 3 0 2) (0 3 1 0) (0 0 0 0)))))

(defun font-data-to-letter-line (font-data)
  (mapcar #'font-line-to-letter-line
          (font-data-to-font-lines font-data)))

(test font-data-to-letter-line
  (is (equal (font-data-to-letter-line
              '(48 16336 4624 2336 16380 9608 21584 5064
                8712 2016 14912 384 1760 30748))
             '(((0 0 0 0) (0 0 0 0) (3 1 1 2) (0 3 0 0)
                (0 3 2 1) (0 3 0 2) (0 3 1 0) (0 0 0 0))
               ((0 0 0 0) (3 2 0 2) (3 0 1 0) (3 2 1 3)
                (3 1 0 3) (3 1 1 0) (3 2 1 1) (0 0 1 0))
               ((0 0 0 0) (2 0 0 0) (0 2 0 0) (0 3 1 2)
                (2 3 2 1) (0 1 2 0) (2 0 3 0) (0 0 0 0))
               ((0 0) (0 3) (2 1) (3 0)
                (2 0) (1 2) (0 3) (0 1))))))

(defun font-line-to-braille-letter-line (font-line)
  (COERCE (reverse
           (font-line-to-letter-line-chars
            (font-line-to-letter-line font-line)))
          'STRING))

(test font-line-to-braille-letter-line
  (is (equal (mapcar #'font-line-to-braille-letter-line
                     (font-data-to-font-lines
                      '(48 16336 4624 2336 16380 9608 21584 5064
                        8712 2016 14912 384 1760 30748)))
             '("⠀⠲⡒⢖⠒⡹⠀⠀" "⠠⢫⠹⣙⣫⠩⡋⠀" "⠀⠥⠔⢗⡲⠂⠁⠀" "⠐⠒⠊⠁⠉⠑⠒⠀"))))

(defun baikaku-num (num)
  (values
   (parse-integer
    (format nil
            "~{~A~:*~A~}"
            (coerce (format nil "~B" num) 'list))
    :radix 2)))

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

(defun char-to-font-data (char &aux (k14 *k14*))
  (let ((data (subseq (member (format nil
                                      "ENCODING ~A"
                                      (char-to-jis-code char))
                              k14 :test #'string=)
                      5 19)))
    (mapcar (lambda (x)
              (parse-integer x :radix 16))
            data)))

(test char-to-font-data
  (is (equal (char-to-font-data #\愛)
             '(48 16336 4624 2336 16380 9608 21584 5064
               8712 2016 14912 384 1760 30748))))

(defun char-to-braille-letter (char &optional (filter #'values))
  (mapcar #'font-line-to-braille-letter-line
          (font-data-to-font-lines
           (funcall filter
                    (char-to-font-data char)))))

(test double-width
  (is (equal (mapcar #'font-line-to-braille-letter-line
                     (font-data-to-font-lines
                      '(0 4095 780 195 4095 3123 13104 783
                        3084 63 4044 3 60 16320)))
             '("⠀⠀⠒⠶⣒⠒⠶⣒" "⠀⠤⠛⣭⠉⠿⣉⣛" "⠀⠀⠭⠤⠤⠒⠿⣒" "⠀⠒⠒⠒⠒⠉⠉⠀")))
  (is (mapcar (lambda (x)
          (list (baikaku-num (ldb (byte 8 0) x))
                (baikaku-num (ldb (byte 8 8) x))) )
              '(48 16336 4624 2336 16380 9608 21584 5064
                8712 2016 14912 384 1760 30748))
      '((3840 0) (62208 4095) (768 780) (3072 195) (65520 4095) (49344 3123)
        (13056 13104) (61632 783) (192 3084) (64512 63) (12288 4044) (49152 3)
        (64512 60) (1008 16320))))


(defun char-to-braille-double-width-letter (char &optional (filter #'values))
  (let ((dw (mapcar (lambda (x)
                      (mapcar (lambda (x)
                                (list (baikaku-num (ldb (byte 8 0) x))
                                      (baikaku-num (ldb (byte 8 8) x))))
                              x))
                    (font-data-to-font-lines
                     (funcall filter
                              (char-to-font-data char))))))
    (mapcar (lambda (e)
              (concatenate 'string
                           (font-line-to-braille-letter-line
                            (mapcar #'second e))
                           (font-line-to-braille-letter-line
                            (mapcar #'first e))))
            dw)))

(test char-to-braille-double-width-letter
  (is (equal (char-to-braille-double-width-letter #\あ)
             '("⠀⠀⠀⣀⣀⣉⣶⣀⠤⠤⠀⠀⠀⠀⠀⠀"
               "⠀⠀⠀⠀⣀⣿⠤⣤⠿⠤⠤⣀⠀⠀⠀⠀"
               "⠀⠀⣶⠉⣀⠭⣶⠉⠀⠀⠀⣀⠿⠀⠀⠀"
               "⠀⠀⠀⠉⠀⠀⠀⠀⠀⠉⠉⠀⠀⠀⠀⠀"))))

(test char-to-braille-letter
  (is (equal (char-to-braille-letter #\愛)
             '("⠀⠲⡒⢖⠒⡹⠀⠀" "⠠⢫⠹⣙⣫⠩⡋⠀" "⠀⠥⠔⢗⡲⠂⠁⠀" "⠐⠒⠊⠁⠉⠑⠒⠀"))))


(defun rotate-180 (font-data)
  (mapcar (lambda (x)
            (parse-integer
             (reverse (format nil "~16,'0,B" x))
             :radix 2))
          (reverse font-data)))

(defun string-to-braille (str &optional (filter #'values))
  (fare-utils:join-strings
   (apply #'mapcar (lambda (&rest e)
                     (apply #'concatenate 'string e))
          (map 'list (lambda (c)
                       (char-to-braille-letter c filter))
               str))
   :separator #\Newline))

(test string-to-braille
  (is (string= (string-to-braille "龍龍龍龍")
           "⠠⡤⠧⡤⣗⣒⡂⠀⠠⡤⠧⡤⣗⣒⡂⠀⠠⡤⠧⡤⣗⣒⡂⠀⠠⡤⠧⡤⣗⣒⡂⠀
⠐⣚⣚⡒⣖⣒⡃⠀⠐⣚⣚⡒⣖⣒⡃⠀⠐⣚⣚⡒⣖⣒⡃⠀⠐⣚⣚⡒⣖⣒⡃⠀
⠀⣗⣒⡇⣗⣒⡂⠀⠀⣗⣒⡇⣗⣒⡂⠀⠀⣗⣒⡇⣗⣒⡂⠀⠀⣗⣒⡇⣗⣒⡂⠀
⠀⠃⠐⠃⠑⠒⠚⠀⠀⠃⠐⠃⠑⠒⠚⠀⠀⠃⠐⠃⠑⠒⠚⠀⠀⠃⠐⠃⠑⠒⠚⠀")))

(defun string-to-double-width-braille (str &optional (filter #'values))
  (fare-utils:join-strings
   (apply #'mapcar (lambda (&rest e)
                     (apply #'concatenate 'string e))
          (map 'list
               (lambda (c)
                 (char-to-braille-double-width-letter c filter)) str))
   :separator #\Newline))


;;; eof
