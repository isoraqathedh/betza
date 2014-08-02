;;;; Betza Notation board builder

(defparameter *all-dirs*
  '(:f   :b   :r   :l                        ; orthogonals
    :fr  :fl  :br  :bl                       ; diagonals
    :ffr :fsr :bsr :bbr :bbl :bsl :fsl :ffl) ; hippogonals
  "The list of all available directions.")

(defparameter *special-symbols*
  '(((:move :capture) . "o")
    ((:move) . "m")
    ((:capture) . "c")
    ((:move :capture :jump) . "o")
    ((:move :jump) . "m")
    ((:capture :jump) . "c")
    ((:move :capture :range) . "x")
    ((:move :range) . "v")
    ((:capture :range) . "^")
    ((:origin) . "-"))
  "Special keys for descriptive mode.")

;;; Board class and attendent methods
;;; =================================
(defclass board ()
  ((upper-left-quadrant  :documentation "A #2A() holding the upper-left quadrant." )
   (upper-right-quadrant :documentation "A #2A() holding the upper-right quadrant.")
   (lower-left-quadrant  :documentation "A #2A() holding the lower-left quadrant.")
   (lower-right-quadrant :documentation "A #2A() holding the lower-right quadrant.")
   (plus-x-axis :documentation "A vector holding the positive x-axis.")
   (plus-y-axis :documentation "A vector holding the negative x-axis.")
   (minus-x-axis :documentation "A vector holding the positive y-axis.")
   (minus-y-axis :documentation "A vector holding the negative y-axis.")
   ;; Remember, use (get-cell-at) to access the above slots!
   (origin :accessor origin 
	   :initform '(:origin)
           :documentation "This slot holds the data for the one cell that doesn't fit in the above eight.")
   (lower-left-corner :accessor lower-left-corner
		      :initarg :lower-left
                      :documentation "Creation parameter: defines the lower-left corner of the board.")
   (upper-right-corner :accessor upper-right-corner
		       :initarg :upper-right
                       :documentation "Creation parameter: defines the upper-right corner of the board."))
  (:documentation "Defines a board. Board is cut up into 9 segments because using 1 array is hard."))

(defmethod initialize-instance :after ((instance board) &key)
  (with-slots (upper-left-quadrant upper-right-quadrant
               lower-left-quadrant lower-right-quadrant
               lower-left-corner   upper-right-corner
               plus-x-axis minus-x-axis
               plus-y-axis minus-y-axis origin) instance
    (let ((xmin (- (aref lower-left-corner  0)))
	  (xmax (aref upper-right-corner 0))
	  (ymin (- (aref lower-left-corner  1)))
	  (ymax (aref upper-right-corner 1)))
      (setf plus-x-axis          (make-array xmax :initial-element nil)
	    plus-y-axis          (make-array ymax :initial-element nil)
	    minus-x-axis         (make-array xmin :initial-element nil)
	    minus-y-axis         (make-array ymin :initial-element nil)
	    upper-left-quadrant  (make-array (list ymax xmin) :initial-element nil)
	    upper-right-quadrant (make-array (list ymax xmax) :initial-element nil)
	    lower-left-quadrant  (make-array (list ymin xmin) :initial-element nil)
	    lower-right-quadrant (make-array (list ymin xmax) :initial-element nil)))))

(defmethod initialize-instance :after ((instance board) &key)
  (with-slots (upper-left-quadrant upper-right-quadrant
               lower-left-quadrant lower-right-quadrant
               lower-left-corner   upper-right-corner
               plus-x-axis minus-x-axis
               plus-y-axis minus-y-axis origin) instance
    (let ((xmin (- (aref lower-left-corner  0)))
	  (xmax (aref upper-right-corner 0))
	  (ymin (- (aref lower-left-corner  1)))
	  (ymax (aref upper-right-corner 1)))
      (setf plus-x-axis          (make-array xmax :initial-element nil)
	    plus-y-axis          (make-array ymax :initial-element nil)
	    minus-x-axis         (make-array xmin :initial-element nil)
	    minus-y-axis         (make-array ymin :initial-element nil)
	    upper-left-quadrant  (make-array (list ymax xmin) :initial-element nil)
	    upper-right-quadrant (make-array (list ymax xmin))))))

(defmethod print-object ((object board) stream)
  (with-accessors ((upper-right-corner upper-right-corner)
                   (lower-left-corner lower-left-corner)) object
    (print-unreadable-object (object stream :type t)
      (format stream "(~{~a~^, ~}) ~~ (~{~a~^, ~})"
              (coerce (lower-left-corner object) 'list) (coerce (upper-right-corner object) 'list)))))
              

;;; Get and set cell values
;;; -----------------------

;;; Generics
(defgeneric get-cell-at (board x y)
  (:documentation "Retrieves the value of the cell at (x, y)."))

(defgeneric (setf get-cell-at) (value board x y)
  (:documentation "allows setting values into a board."))

;;; Case origin
(defmethod get-cell-at ((board board) (x (eql 0)) (y (eql 0)))
  (origin board))

(defmethod (setf get-cell-at) (value (board board) (x (eql 0)) (y (eql 0)))
  (setf (origin board) value))

;;; Case x-axis
(defmethod get-cell-at ((board board) (x number) (y (eql 0)))
  (with-slots (plus-x-axis minus-x-axis) board
    (if (plusp x)
        (aref plus-x-axis (1- x))
        (aref minus-x-axis (1- (- x))))))

(defmethod (setf get-cell-at) (value (board board) (x number) (y (eql 0)))
  (with-slots (plus-x-axis minus-x-axis) board
    (if (plusp x)
        (setf (aref plus-x-axis (1- x)) value)
        (setf (aref minus-x-axis (1- (- x))) value))))

;;; Case y-axis
(defmethod get-cell-at ((board board) (x (eql 0)) (y number))
  (with-slots (plus-y-axis minus-y-axis) board
    (if (plusp y)
        (aref plus-y-axis (1- y))
        (aref minus-y-axis (1- (- y))))))

(defmethod (setf get-cell-at) (value (board board) (x (eql 0)) (y number))
  (with-slots (plus-y-axis minus-y-axis) board
    (if (plusp y)
        (setf (aref plus-y-axis (1- y)) value)
        (setf (aref minus-y-axis (1- (- y))) value))))

;;; Else
(defmethod get-cell-at ((board board) (x number) (y number))
  (with-slots (upper-left-quadrant upper-right-quadrant
               lower-left-quadrant lower-right-quadrant) board
    (aref
     (cond ((and (plusp x)  (plusp y))  upper-right-quadrant)
           ((and (plusp x)  (minusp y)) lower-right-quadrant)
           ((and (minusp x) (plusp y))  upper-left-quadrant)
           (t lower-left-quadrant))
     (1- (abs y)) (1- (abs x)))))

(defmethod (setf get-cell-at) (value (board board) (x number) (y number))
  (with-slots (upper-left-quadrant upper-right-quadrant
               lower-left-quadrant lower-right-quadrant) board
    (setf
     (aref
      (cond ((and (plusp x)  (plusp y))  upper-right-quadrant)
            ((and (plusp x)  (minusp y)) lower-right-quadrant)
            ((and (minusp x) (plusp y))  upper-left-quadrant)
            (t lower-left-quadrant))
      (1- (abs y)) (1- (abs x))) value)))

;;; Print board
;;; -----------
  
(defgeneric print-board (board &optional verbose)
  (:documentation "Stitches together and prints the board in a human-readable fashion."))

(defmethod print-board ((board board) &optional verbose)
  ;; To do: make it print letters for certain signatures
  (format t "~&+")
  (loop for j
        from (aref (lower-left-corner board) 0)
          to (aref (upper-right-corner board) 0)
        do (format  t "---+"))
  (loop for i 
	  downfrom (aref (upper-right-corner board) 1)
	    to  (aref (lower-left-corner board) 1)
	do (format t "~&|")
	   (loop for j 
		   from (aref (lower-left-corner board) 0)
		     to (aref (upper-right-corner board) 0)
		 do (if verbose
                        (format t " ~a |" (get-cell-at board j i))
                        (format t " ~a |" (case (length (get-cell-at board j i))
					    (0 ".")
					    ((1 2 3 4 5 6 7 8 9) (length (get-cell-at board j i)))
					    (t "X")))))
           (format t "~&+")
           (loop for j
                 from (aref (lower-left-corner board) 0)
                   to (aref (upper-right-corner board) 0)
                 do (format  t "---+"))))

;;; Destination class
;;; =================

(defclass destination ()
  ((x :initarg :x
      :initform 0
      :accessor destination-x
      :documentation "The x-coordinate of the destination.")
   (y :initarg :y
      :initform 0
      :accessor destination-y
      :documentation "The y-coordinate of the destination.")
   (signature :initarg :signature
              :initform nil
              :accessor signature
              :documentation "The signature that describes what the piece can do at the coordinate."))
  (:documentation "Describes what a piece can do at a given destination."))

(defmethod print-object ((object destination) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((x destination-x) (y destination-y) (signature signature)) object
      (format stream "(~a, ~a) ~s" x y signature))))

(defgeneric push-to-board (destination board)
  (:documentation "Writes the destination into a board."))

(defmethod push-to-board ((destination destination) (board board))
  (with-accessors ((x destination-x) (y destination-y) (signatures signature)) destination
    (dolist (signature signatures)
      (push signature (get-cell-at board x y)))))

;;; Pieces
;;; ======

(defun make-piece (x y dirs signatures)
  "The basic method to create a piece."
  (when (or (null dirs) (eql dirs :all)) ;; default value
    (setf dirs *all-dirs*))
  (let (final-list)
    (destructuring-bind (&key f b r l fl bl br fr ffr ffl fsr fsl bsr bsl bbr bbl)
        (loop for i in dirs collect i collect t)
                                        ; This converts the directions list into a plist
                                        ; so I can refer to them individually below
                                        ; It also intentionally errors out
                                        ; if anything that shouldn't be there is.
      (macrolet ((push-destination-when (direction x y)
                   `(when ,direction
                      (push (make-instance 'destination
                                           :x ,x :y ,y
                                           :signature (or signatures (list :move :capture))) final-list))))
        (cond ((= 0 x y) (push-destination-when t 0 0)) ;; Zero case
              ((and (or  (zerop x) (zerop y))
                    (not (and (zerop x) (zerop y))))
               ;; Orthogonal case
               (let ((length (max x y)))
                 (push-destination-when f length 0)
                 (push-destination-when r 0 length)
                 (push-destination-when b 0 (- length))
                 (push-destination-when l (- length) 0)))
              ((and (= x y) (not (zerop x)) (not (zerop y)))
               ;; Diagonal case
               (push-destination-when fr x x)
               (push-destination-when br x (- x))
               (push-destination-when bl (- x) (- x))
               (push-destination-when fl (- x) x))
              (t
               ;; Hippogonal case
               (let ((max (max x y)) (min (min x y))
                     (-max (- (max x y))) (-min (- (min x y))))
                 (push-destination-when ffr min max)
                 (push-destination-when fsr max min)
                 (push-destination-when bsr max -min)
                 (push-destination-when bbr min -max)
                 (push-destination-when bbl -min -max)
                 (push-destination-when bsl -max -min)
                 (push-destination-when fsl -max min)
                 (push-destination-when ffl -min max))))))
    final-list))

(defun make-and-print-board (destination-list &key
                                                (at-least-as-left-as -1)
                                                (at-least-as-right-as 1)
                                                (at-least-as-low-as -1)
                                                (at-least-as-high-as 1))
    (let ((board (make-instance 'board
                                :lower-left
                                (vector
                                 (reduce #'min destination-list
                                         :key #'destination-x
                                         :initial-value at-least-as-left-as)
                                 (reduce #'min destination-list
                                         :key #'destination-y 
                                         :initial-value at-least-as-low-as))
                                :upper-right
                                (vector
                                 (reduce #'max destination-list
                                         :key #'destination-x
                                         :initial-value at-least-as-right-as)
                                 (reduce #'max destination-list
                                         :key #'destination-y
                                         :initial-value at-least-as-high-as))))
                                        ; may decide to parse the destination list here as well,
                                        ; but since that function doesn't exist yet none will exist.
          )
      (dolist (i destination-list)
        (push-to-board i board))
      (print-board board)
      board))

(defclass range (destination)
  ((range :initarg :range
          :initform nil
          :documentation "The maximum range that the piece can travel. NIL means \"infinity\"."))
  (:documentation "Pieces with range, i.e. can repeat it in the same direction for as long as directed."))

;; (defgeneric destination->range (range distance)
;;   (:method ((range range) (distance (eql nil)))
;;     (make-instance 'range

(defmethod range->destination-list ((range range) &optional board)
  (when (numberp (range-length range))
    (loop with dx = (destination-x range)
          with dy = (destination-y range)
          for current-cell-x = dx then (+ current-cell-x dx)
          for current-cell-y = dy then (+ current-cell-y dy)
          for i from 1 to 20000 ; safety valve
          until (or (if (range-length range)
                        (> i (range-length range)))
                    (not (<= lim-llx current-cell-x lim-urx))
                    (not (<= lim-lly current-cell-y lim-ury)))
          collect (make-instance 'destination
                                 :x current-cell-x
                                 :y current-cell-y
                                 :signature (cons :range (signature range))))))

(defmethod range->destination-list ((range list) &optional board)
  (loop for i in range
        append (range->destination-list i board)))

(defun range (name length &rest dirs-and-signatures)
  (cond ((member (type-of name) '(cons destination))
         (destination->range name length))
        ((or (numberp name) (member (type-of name) '(keyword)))
         (destination->range (apply #'piece (cons name dirs-and-signatures)) length))))

(defun inf-range (name &rest dirs-and-signatures)
  (apply #'range (append (list name nil) dirs-and-signatures)))


