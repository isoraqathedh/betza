;;;;move-diagram.lisp

;;; This file handles making a move diagram and drawing one to stdout.
;;; or a PNG.
;;; maybe.

(in-package #:betza-board)

;;; The board
(defclass chess-board ()
  ((board-size :initarg :size
               :initform 3
               :reader board-size)
   (board :reader board)
   (features :initform nil
             :accessor features)))

(defmethod initialize-instance :after ((instance chess-board) &key)
  (with-slots (board board-size) instance
    (setf board (make-array (list (1+ (* 2 board-size))
                                  (1+ (* 2 board-size)))
                            :initial-element nil)
          (aref board board-size board-size) (list :piece))))

(defgeneric board-coords (board coordinate-x coordinate-y)
  (:documentation "Get the feature in the coordinates of the board.")
  (:method ((board-instance chess-board)
            (coordinate-x integer) (coordinate-y integer))
    (with-slots (board board-size) board-instance
      (aref board
            (- board-size coordinate-y)
            (+ board-size coordinate-x)))))

(defgeneric (setf board-coords) (val board coordinate-x coordinate-y)
  (:documentation "Set the feature in the coordinates of the board.")
  (:method (val (board-instance chess-board)
            (coordinate-x integer) (coordinate-y integer))
    (with-slots (board board-size) board-instance
      (setf (aref board
                  (- board-size coordinate-y)
                  (+ board-size coordinate-x))
            val))))

;;; The painters
;; Basic painter class
(defclass painter ()
  ())

;; Extended classes
;; (defclass text-painter (painter)
;;   (stream))

;; (defclass image-painter (painter)
;;   (file))

;; (defgeneric draw-board (board painter)
;;   (:documentation "Draw the board.")
;;   (:method ((board chess-board) (painter painter))
;;     (draw-header board)
;;     (iter rows
;;       (for u from 0 below (array-dimension (board board) 0))
;;       (draw-row-separator board u)
;;       (iter cols
;;         (for v from 0 below (array-dimension (board board) 1))
;;         (draw-cell board u v)))
;;     (draw-footer board)))
