(in-package #:info.isoraqathedh.betza.generators)

(defun make-square (x y)
  "Return the square (x, y) in a square grid."
  (cons x y))

(defmacro with-parsed-square (square x-name y-name &body body)
  (let ((square^ (gensym "SQUARE")))
    `(let ((,square^ ,square))
       (when ,square^
         (destructuring-bind (,x-name . ,y-name) ,square^
           ,@body)))))

(defun return-once (value)
  "Create a function that returns VALUE once, then return NIL."
  (let ((returnp t))
    (lambda ()
      (prog1 (when returnp value)
        (setf returnp nil)))))

(defun line (x y &optional (start-x 0) (start-y 0))
  "Create a generator that lists squares that draw a line."
  (lambda ()
    (make-square (incf start-x x) (incf start-y y))))

(defun fan-directions (large small)
  "Create a lookup table for large and small. Each one is unique.

Both arguments must be non-negative integers, and SMALL should be smaller than LARGE."
  (when (< large small)
    (rotatef small large))
  (remove-duplicates
   (mapcar (lambda (a b c d)
             (make-square (* a c) (* b d)))
           (list     1    -1     1    -1     1    -1     1    -1)
           (list     1     1    -1    -1     1     1    -1    -1)
           (list large large large large small small small small)
           (list small small small small large large large large))
   :test #'equal))

(defun leaves (x y &optional directions)
  "Create a generator that do something.."
  (let ()
   (lambda ()
     )))

(defun interleave (closures)
  "For a list of closures in CLOSURES,
create another closure that funcall each of them once per call."
  (lambda ()
    (mapcar #'funcall closures)))

(defun interleave* (closures)
  "For a list of closures in CLOSURES,
create another closure that funcall each of them *in turn*, once per call.
Additionally, return as a secondary value which of the values are returned."
  (let ((nth-function 0)
        (closure-count (length closures)))
    (lambda ()
      (values-list
       (prog1 (let ((function-called (mod nth-function closure-count)))
                (list (funcall (nth function-called closures))
                      function-called))
         (incf nth-function))))))

(defun collect-repeatedly (interleaved-closures &optional (limit 100))
  "Repeatedly call INTERLEAVED-CLOSURES, stopping "
  (loop for i = (funcall interleaved-closures)
        for j from 1 to limit
        while (notevery #'null i)
        append i))
