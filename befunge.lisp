(uiop/package:define-package #:befunge/all
    (:nicknames #:befunge)
  (:import-from #:uiop #:quit)
  (:import-from #:resettable-class #:resettable-class)
  (:use-reexport #:cl))
(in-package #:befunge)

(defclass state ()
  ((direction
    :initarg :direction
    :initform :right
    :accessor direction)
   (grid
    :initarg :grid
    :accessor grid
    :initform (make-array '(80 25)
                          :element-type 'character
                          :initial-element #\Space))
   (instruction-pointer
    :initform '(0 . 0)
    :initarg :instruction-pointer
    :accessor instruction-pointer)
   (stack
    :initarg :stack
    ;; TODO: figure out a good initial stack size
    :initform (make-array 50 :fill-pointer 0)
    :accessor stack))
  (:metaclass resettable-class))

(defparameter *state* (make-instance 'state))

(defun random-direction ()
  (case (random 4)
    (0 :up)
    (1 :down)
    (2 :left)
    (3 :right)))

(defun stack-pop (state)
  (declare (type state state))
  (let ((stack (stack state)))
    (or (ignore-errors (vector-pop stack))
        (and (vector-push 0 stack) nil))))

(defun stack-push (state n)
  (declare (type state state)
           (type number n))
  (let ((stack (stack state)))
    (vector-push n stack)))

(defmacro define-instructions (name pairs)
  `(prog1 (defparameter ,name (make-hash-table :size (length ',pairs)))
          (setf ,@(loop for (char . body) in pairs
                        append `((gethash ,char ,name) (lambda () ,@body))))))

(define-instructions *instructions*
  ((#\v (setf (direction *state*) :down))
   (#\^ (setf (direction *state*) :up))
   (#\< (setf (direction *state*) :left))
   (#\> (setf (direction *state*) :right))
   (#\? (setf (direction *state*) (random-direction)))
   (#\. (let ((n (stack-pop *state*)))
          (and n (princ n))))
   (#\& (let* ((line (read-line))
               (input (parse-integer line :junk-allowed t)))
          (when input
            (princ input)
            (stack-push *state* input))))
   (#\@
    ;; Silently reset the state if running in Sly/SLIME:
    ;; TODO: use conditions to signal this to the looping code below
    #+(or slynk swank)
    (reinitialize-instance *state*)
    ;; Otherwise quit:
    #-(or slynk swank)
    (quit))))

(defun populate-grid (stream)
  (declare (type stream stream))
  (loop for line = (read-line stream nil)
        for num-lines from 1
        while line
        do  (when (>= num-lines 24) (error "Max of 25 lines"))
            (when (>= (length line) 80) (error "Lines should be at most 80 chars long"))
            (loop for char across line
                  for i from 0 below 80
                  with grid = (grid *state*)
                  do (setf (row-major-aref grid i) char))))

;; TODO: figure out if we need flexi-streams or not.
(defun interpret-from-stream (stream)
  (declare (type stream stream))
  (populate-grid stream)
  (let ((grid (grid *state*))
        (stack (stack *state*)))
    (destructuring-bind (n m) (array-dimensions grid)
      (loop for i from 0 below n do
        (loop for j from 0 below m
              for c = (aref grid i j)
              for ascii-code = (char-code c) do
                (block continue
                  (when (char= c #\Space)
                    (return-from continue))
                  (if (<= 48 ascii-code 57)
                      (vector-push (digit-char-p c) stack)
                      (funcall (or (gethash c *instructions*)
                                   (error "No such instruction: ~C~&On line ~S, character ~S" c (1+ i) (1+ j)))))))))))

(defun interpret-from-file (pathspec)
  (declare (type (or string pathname) pathspec))
  (with-open-file (stream pathspec)
    (interpret-from-stream stream)))
