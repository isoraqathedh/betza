;;;; Betza notation marker
;;;; Take 5 (but labeled 1 because whatever)

;;; Globals

(defvar *location-descriptors*
  (loop with hash-table = (make-hash-table)
        for (coords . names)
          in '(((0 . 0) :zero)
               ((1 . 0) :wazir)
               ((1 . 1) :ferz :advisor)
               ((2 . 0) :dabbabah :war-machine)
               ((2 . 1) :knight :horse)
               ((2 . 2) :alfil :elephant)
               ((3 . 0) :threeleaper)
               ((3 . 1) :camel)
               ((3 . 2) :zebra)
               ((3 . 3) :tripper)
               ((4 . 0) :fourleaper)
               ((4 . 1) :giraffe)
               ((4 . 2) :double-knight)
               ((4 . 3) :antelope))
        do (loop for i in names do (setf (gethash i hash-table) coords))
        finally (return hash-table)))
        
;;; Classes:
;; Destination class
;; See class documentation for usage.

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
  (:documentation "A description of how the chess piece interacts with a certain square."))

(defmethod print-object ((object destination) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~a, ~a): ~s" (destination-x object) (destination-y object) (signature object))))
