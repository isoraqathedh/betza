(defpackage :info.isoraqathedh.betza.board
  (:use #:cl #:betza-lexer))

(in-package :info.isoraqathedh.betza.board)

;;; Classes:
;;; destination class
;;; See class documentation for usage.

(defclass destination ()
  ((x :accessor destination-x
      :initarg :x
      :initform 0
      :documentation "x-coordinate (left-right) of the destination. Right is positive.")
   (y :accessor destination-y
      :initarg :y
      :initform 0
      :documentation "y-coordinate (up-down) of the destination. Up is positive.")
   (signature :accessor signature
              :initarg :signature
              :initform nil
              :documentation "list of objects that represents what the piece can do at that square."))
  (:documentation "A representation of what a piece can do at a square."))

(defmethod print-object ((object destination) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((x destination-x) (y destination-y) (signature signature)) object
      (format stream "(~a, ~a) ~s" x y signature))))

(defparameter *primitives*
  (loop with ht = (make-hash-table)
        for (landmark . coords) in '((#\W 1 0) (#\F 1 1)
                                     (#\D 2 0) (#\N 2 1) (#\A 2 2)
                                     (#\H 3 0) (#\L 3 1) (#\J 3 2) (#\G 3 3))
        do (setf (gethash landmark ht) coords)
        finally (return ht))
  "Where each of the primitives point to, for instance \"W\" points to (1, 0).")

(defun riderp (power)
  "Detects if a move has a range modifier. Returns T/NIL as well as length"
  (when (or (limit power)
            (find (landmark power) '("R" "B" "Q") :test #'string=)
            (= (length (landmark power)) 2))
    (if (limit power) (parse-integer (limit power)) :infinite)))

(defun make-destination-object (x y &optional signature)
  "Helper function: makes a DESTINATION"
  (make-instance 'destination :x x :y y :signature signature))

(defun capturingp (modifiers)
  "Detects if a piece can capture or not"
  (or (find #\c modifiers)
      (not (or (find #\m modifiers)
               (find #\c modifiers)))))

(defun movingp (modifiers)
  "Detects if a piece can move or not"
  (or (find #\m modifiers)
      (not (or (find #\m modifiers)
               (find #\c modifiers)))))

(defun generate-directions (x y &optional sig)
  "Generates a list of destinations radially symmetric with the original (x, y) pair."
  (let ((max (max x y))
        (-max (- (max x y)))
        (min (min x y))
        (-min (- (min x y))))
    (mapcar #'make-destination-object
            (list max  max min  min -max -max -min -min)
            (list min -min max -max  min -min  max -max)
            (list sig  sig sig  sig  sig  sig  sig  sig))))

(defun force-exist-symbol (name package)
  (or (find-symbol (string-upcase name)
                   (string-upcase package))
      (intern (string-upcase name)
              (string-upcase package))))

(defun detect-direction (destination)
  "Finds a correct description of the exact location of the destination using brfl(vs)-style descriptions."
  (let ((fstr (make-array '(3) :element-type 'base-char :fill-pointer 0)) ; Only a maximum of 3 characters will be written into the string
        (x (destination-x destination))
        (y (destination-y destination)))
    (with-output-to-string (str fstr)
      (format str "~[b~;~;f~]" (1+ (signum y)))
      (when (/= 0 (abs x) (abs y)) ;; hippogonal
        (if (< (abs y) (abs x))
            (format str "s")
            (format str "~:[b~;f~]" (plusp y))))
      (format str "~[l~;~;r~]" (1+ (signum x))))
    (unless (string-equal "" fstr) ; catch (0, 0) and return nil for it.
      (force-exist-symbol fstr "keyword"))))

(defmacro case-using-equal (keyform &body clauses)
  ;; A lot of the things we do here could have used (case) but unfortunately case only uses eql as its comparison.
  ;; so here's a variant that uses #'equal.
  (let ((kf-sym (gensym)))
    `(let ((,kf-sym ,keyform))
       (cond ,@(loop for (cases . then) in clauses
                     if (listp cases)
                       append (loop for case in cases
                                    collect `((equal ,kf-sym ,case) ,@then))
                     else if (eql cases t)
                            collect `(t ,@then)
                     else
                       collect `((equal ,kf-sym ,cases) ,@then))))))

(defun parse-range-modifiers (power)
  (when (riderp power)
    (list :rider
          (list :limit (riderp power)
                :iteration-style (cond ((find #\z (modifiers power)) :zigzag)
                                       ((find #\q (modifiers power)) :circular)
                                       ((find #\g (modifiers power)) :grasshopper)
                                       ((find #\p (modifiers power)) :cannon)
                                       (t :line))))))

(defun parse-movement-modifiers (power)
  (list :move (movingp (modifiers power))
        :capture (capturingp (modifiers power))))

(defun parse-jumping-modifiers (power)
  (when (or (riderp power)
            (not (or (equal (landmark power) "W")
                     (equal (landmark power) "F"))))
    (list :jumping (cond ((find #\n (modifiers power)) nil)
                         ((find #\j (modifiers power)) t)
                         (t :default)))))

(defun decode-directional-modifiers (modifier-string &optional (context-landmark "W"))
  "Takes the modifier string of directions and transform them into a list of direction selectors."
  ;; Annoyingly, the semantics of each modifier is different depending on whether or not the landmark is orthogonal, diagonal or hippogonal.
  ;; fbN is not the same as fNbN, but fBbB is the same as fbB and the same goes with R;
  ;; but even worse is that fBbB = B while the same is not with fbR!
  ;; Things get even worse with the two hybrid landmarks Q and K,
  ;; where bK is bWbF, brlK is brlFbK, etc etc etc.
  ;; As such it is best to consider each case separate.
  (ecase (aref context-landmark 0) ; the first letter (the second letter is always the same) is...
    ((#\W #\R #\D #\H)             ; orthogonal
     (if (equal "" (remove-if-not #'(lambda (p) (find p "brflvs")) modifier-string))
         (list :f :b :r :l)
         (remove-duplicates
          (loop for i across modifier-string
                append (case i
                         (#\s (list :r :l))
                         (#\v (list :f :b))
                         ((#\b #\r #\f #\l) (list (force-exist-symbol i :keyword))))))))
    ((#\F #\B #\A #\G)                  ; diagonal
     (let ((effective-directions (remove-if-not #'(lambda (p) (find p "brfl")) modifier-string)))
       (case-using-equal effective-directions
         ("" (list :br :bl :fr :fl))
         ("b" (list :br :bl))
         ("r" (list :fr :br))
         ("f" (list :fr :fl))
         ("l" (list :fl :bl))
         (("br" "bl" "fr" "fl") (list (force-exist-symbol effective-directions :keyword)))
         (t (error (format nil "No match for directional modifier ~a found." effective-directions))))))
    ((#\N #\L #\J)                      ; hippogonal
     (let ((effective-directions (remove-if-not #'(lambda (p) (find p "brflvsh")) modifier-string)))
       (case-using-equal effective-directions
         ("" (list :ffl :ffr :bbl :bbr :fsl :fsr :bsl :bsr)) 
         (("b" "bh") (list :bbr :bsr :bbl :bsl))
         (("r" "rh") (list :bbr :bsr :ffr :fsr))
         (("f" "fh") (list :ffr :fsr :ffl :fsl))
         (("l" "lh") (list :ffl :fsl :bsl :bbl))
         (("s" "rl") (list :fsr :fsl :bsr :bsl))
         (("v" "fb") (list :ffl :ffr :bbl :bbr))
         ("ff" (list :ffl :ffr))
         ("bb" (list :bbl :bbr))
         ("fs" (list :fsl :fsr))
         ("bs" (list :bsl :bsr))
         ("ll" (list :fsl :bsl))
         ("lv" (list :ffl :bbl))
         ("rr" (list :fsr :bsr))
         ("rv" (list :ffr :ffl))
         (("ffr" "ffl" "fsr" "fsl" "bbr" "bbl" "bsr" "bsl")
          (list (force-exist-symbol effective-directions :keyword)))
         (t (error (format nil "No match for directional modifier ~a found." effective-directions))))))
    ((#\Q #\K)                          ; hybrid directions
     (if (equal "" (remove-if-not #'(lambda (p) (find p "brflvs")) modifier-string))
         (list :bl :b :br :fl :f :fr :r :l)
         (remove-duplicates
          (loop for i across modifier-string
                append (case i
                         (#\b (list :bl :b :br))
                         (#\r (list :fr :r :br))
                         (#\f (list :fl :f :fr))
                         (#\l (list :fl :l :bl))
                         (#\v (list :fl :f :fr :bl :b :br))
                         (#\s (list :fl :l :bl :fr :r :br)))))))))

(defun find-destination-roots (power)
  (let ((signature
          (append
           (parse-movement-modifiers power)
           (parse-jumping-modifiers power)
           (parse-range-modifiers power))))
    (mapcar #'(lambda (p)
                (find p (case-using-equal (landmark power)
                          (("Q" "K") (append (generate-directions 1 0 signature)
                                             (generate-directions 1 1 signature)))
                          (("R" "WW") (generate-directions 1 0 signature))
                          (("B" "FF") (generate-directions 1 1 signature))
                          (t (generate-directions
                              (first (gethash (aref (landmark power) 0) *primitives*))
                              (second (gethash (aref (landmark power) 0) *primitives*))
                              signature)))
                      :key #'detect-direction))
            (decode-directional-modifiers (modifiers power) (landmark power)))))
