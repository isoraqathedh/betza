;;;; Betza notation marker
;;;; Take 5 (but labeled 1 because whatever)

;;; Globals

(defvar *location-descriptors*
  (loop with hash-table = (make-hash-table)
        for (coords . names)
          in '(((0 . 0) :zero)
               ((1 . 0) :wazir :w)
               ((1 . 1) :ferz :advisor :f)
               ((2 . 0) :dabbabah :war-machine :d)
               ((2 . 1) :knight :horse :n)
               ((2 . 2) :alfil :elephant :a)
               ((3 . 0) :threeleaper :g)
               ((3 . 1) :camel :j)
               ((3 . 2) :zebra :l)
               ((3 . 3) :tripper :h)
               ((4 . 0) :fourleaper)
               ((4 . 1) :giraffe)
               ((4 . 2) :double-knight)
               ((4 . 3) :antelope))
        do (loop for i in names do (setf (gethash i hash-table) coords))
        finally (return hash-table))
  "The exact location where some names belong to (a displacement of (2, 1) is a knight-distance away.")
        
;;; Classes:
;;; Movement-component class
;;; See class documentation for usage.

(defclass movement-component ()
  ((x :accessor movement-component-x
      :initarg :x
      :initform 0
      :documentation "x-coordinate (left-right) of the movement-component. Right is positive.")
   (y :accessor movement-component-y
      :initarg :y
      :initform 0
      :documentation "y-coordinate (up-down) of the movement-component. Up is positive.")
   (signature :accessor signature
              :initarg :signature
              :initform nil
              :documentation "list of objects that represents what the piece can do at that square."))
  (:documentation "A representation of one component in Betza (for instance, rR or mQ)"))

(defmethod print-object ((object movement-component) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~a, ~a): ~s"
            (movement-component-x object)
            (movement-component-y object)
            (signature object))))

;;; Base-modifiers class (use or not?)

(defclass modifiers ()
  ((can-land :accessor can-land
             :initarg :can-land
             :initform '(:empty :enemy)
             :documentation "Describes which kinds of squares the space can land on: empty, contains enemy, or contains friendly."))
  (:documentation "Catch-all class for methods that specialize on all modifiers instead of specific ones."))

(defclass directions (modifiers)
  ((effective-directions
    :accessor directions
    :initarg :directions
    :initform '(:f :b :r :l :fl :bl :fr :br :ffl :fsl :bsl :bbl :bbr :bsr :fsr :ffr)
    :documentation "Directions in the movement-component. Must contain subsets of the default."))
  (:documentation "Direction specifier."))

(defclass range (modifiers)
  ((distance :accessor distance
             :initarg :distance
             :initform nil
             :documentation "The maximum distance the range is effective for. Nil means infinite."))
  (:documentation
   "Range: a step that can be repeated for as long as the range continues or an edge is reached"))



;;; Transform betza-string into #<movement-component>s.
(defun betza-string->movement-component (betza-string)
  "Turns a single letter (such as F, D, N, A, G) into a movement-component."
  ;; To do: support modifiers and riders (turn R into WW, support things like fN, etc.)
  (let ((landmark-letter (string-upcase
                          (elt
                           (reverse betza-string) 0))))
    (make-instance
     'movement-component
     :x (car (gethash (or (find-symbol landmark-letter "KEYWORD")
                          (intern landmark-letter "KEYWORD"))
                      *location-descriptors*))
     :y (cdr (gethash (or (find-symbol landmark-letter "KEYWORD")
                          (intern landmark-letter "KEYWORD"))
                      *location-descriptors*)))))
