;; TODO: make a curses-based debugger/editor
(defpackage befunge
  (:import-from #:uiop #:quit)
  (:import-from #:alexandria #:with-gensyms #:if-let)
  (:import-from #:resettable-class #:resettable-class)
  (:export #:*state* #:state #:interpret-from-stream #:interpret-from-string #:interpret-from-file)
  (:use #:cl))
(in-package #:befunge)

(defconstant +x-limit+ 80)
(defconstant +y-limit+ 25)

(defstruct pair
  (x (error "must provide x parameter") :type number)
  (y (error "must provide y parameter") :type number))

(defstruct (coords (:include pair
                    (x (error "must provide x coordinate") :type (unsigned-byte 79))
                    (y (error "must-provide y coordinate") :type (unsigned-byte 24)))))

(defun pair (x y)
  (make-pair :x x :y y))

(defclass state ()
  ((direction
    :initarg :direction
    :initform :right
    :accessor direction)
   (grid
    :initarg :grid
    :accessor grid
    :initform (make-array (list +x-limit+ +y-limit+)
                          :initial-element (char-code #\Space)
                          :adjustable t))
   (instruction-pointer
    :initform (make-coords :x 0 :y 0)
    :type coords
    :initarg :instruction-pointer
    :accessor instruction-pointer)
   (stack
    :initarg :stack
    ;; TODO: figure out a good initial stack size
    :initform (make-array 50 :fill-pointer 0)
    :accessor stack)
   (string-mode-p
    :initarg :string-mode-p
    :initform nil
    :accessor string-mode-p))
  (:metaclass resettable-class))

(defparameter *state* (make-instance 'state))

(defun falsep (n)
  (zerop n))

(defun truep (n)
  (not (falsep n)))

(defun random-direction ()
  (case (random 4)
    (0 :up)
    (1 :down)
    (2 :left)
    (3 :right)))

(defun wrap-around-+ (limit n m)
  (mod (+ n m) limit))

(defun move-by-direction (state)
  (let ((grid (grid state)))
    (flet ((pair+ (c1 c2)
             (destructuring-bind (x-limit y-limit) (array-dimensions grid)
               (make-pair :x (wrap-around-+ x-limit (pair-x c1) (pair-x c2))
                          :y (wrap-around-+ y-limit (pair-y c1) (pair-y c2))))))

      (let* ((velocity (case (direction state)
                         (:up (pair 0 -1))
                         (:down (pair 0 1))
                         (:left (pair -1 0))
                         (:right (pair 1 0))))
             (ip (instruction-pointer state))
             (result (pair+ ip velocity)))
        (setf (instruction-pointer state)
              (make-coords :x (pair-x result)
                           :y (pair-y result)))
        state))))

(defun stack-pop (state)
  (declare (type state state))
  (or (vector-pop (stack state))
      (and (vector-push 0 (stack state)) nil)))

(defun stack-push (state n)
  (declare (type state state)
           (type number n))
  (vector-push n (stack state)))

(defmacro define-instructions (name &body pairs)
  (let ((length (length pairs)))
    `(prog1 (defparameter ,name (make-hash-table :size ,length))
       (setf ,@(loop for (char . body) in pairs
                     append `((gethash ,char ,name) (lambda () ,@body)))))))

(define-instructions *instructions*
  ;; Arithmetic
  (#\+ (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push *state* (+ n m))))
  (#\- (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push *state* (- n m))))
  (#\* (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push *state* (* n m))))
  (#\/ (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push *state* (floor n m))))
  (#\% (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push *state* (mod n m))))
  (#\! (let ((n (stack-pop *state*)))
         (stack-push
          *state*
          (if (falsep n)
              1
              0))))
  (#\` (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push
          *state*
          (if (> n m)
              1
              0))))
  ;; PC manipulation
  (#\v (setf (direction *state*) :down))
  (#\^ (setf (direction *state*) :up))
  (#\< (setf (direction *state*) :left))
  (#\> (setf (direction *state*) :right))
  (#\? (setf (direction *state*) (random-direction)))
  (#\_ (let ((n (stack-pop *state*)))
         (setf (direction *state*)
               (if (truep n)
                   :left
                   :right))))
  (#\| (let ((n (stack-pop *state*)))
         (setf (direction *state*)
               (if (truep n)
                   :up
                   :down))))
  (#\# (move-by-direction *state*))
  ;; I/O
  ;; TODO: figure out if we want to call `finish-output' after printing.
  (#\. (let ((n (stack-pop *state*)))
         (when n
           (format t "~D" n))))
  (#\, (let ((n (stack-pop *state*)))
         (when n
           (format t "~C" (code-char n)))))
  (#\& (loop for line = (read-line)
             for digit = (some #'digit-char-p line)
             until digit
             finally (stack-push *state* digit)))
  (#\~ (let* ((line (read-line))
              (first-char (or (ignore-errors (char line 0)) #\Newline))
              (ascii-code (char-code first-char)))
         (stack-push *state* ascii-code)))
  ;; Stack manipulation
  (#\: (let ((n (stack-pop *state*)))
         (stack-push *state* n)
         (stack-push *state* n)))
  (#\\ (let* ((n (stack-pop *state*))
              (m (stack-pop *state*)))
         (stack-push *state* n)
         (stack-push *state* m)))
  (#\$ (stack-pop *state*)
       nil)
  ;; Grid manipulation
  (#\g (let* ((grid (grid *state*))
              (y (stack-pop *state*))
              (x (stack-pop *state*))
              (ascii-code (aref grid x y)))
         (stack-push *state* ascii-code)))
  (#\p (let ((grid (grid *state*))
             (y (stack-pop *state*))
             (x (stack-pop *state*))
             (value (stack-pop *state*)))
         (setf (aref grid x y) value)))
  ;; Misc.
  (#\@
   ;; Signal a condition if running inside Emacs with Sly/SLIME:
   #+(or slynk swank)
   (error "Exited program")
   ;; Otherwise quit:
   #-(or slynk swank)
   (quit))
  (#\" (setf (string-mode-p *state*)
             (not (string-mode-p *state*)))) ; String mode
  (#\Space nil))

(defun populate-grid (stream &key (state *state*))
  (declare (type stream stream)
           (type state state))
  (loop with grid = (grid state)
        with lines = 0
        with chars = 0
        with line-length = 0
        for char = (read-char stream nil)
        for next-char = (peek-char nil stream nil)
        while char
        if (>= lines 25)
          do (cerror "Retry" "Max of 25 lines")
        if (>= chars 80)
          do (cerror "Retry" "Lines should be at most 80 chars long")
        if (char= char #\Newline) do
          (unless (zerop chars)
            (decf chars))
          (when (zerop line-length)
            (setf line-length chars))
          (when (and next-char (char/= next-char #\Newline) (/= line-length chars))
            (cerror "Retry" "Invalid structure, please make sure your befunge code is rectangular."))
          (incf lines)
          (setf chars 0)
        else do
          (setf (aref grid chars lines) (char-code char))
          (incf chars)
        finally (adjust-array grid (list (1+ line-length) (1+ lines)))))

(defun interpret-number-or-instruction (x y c state)
  (if-let ((digit-char (digit-char-p c)))
    (stack-push state digit-char)
    (funcall (or (gethash c *instructions*)
                 (cerror "Retry"
                         "(~S,~S): no such instruction ~C~&" x y c)))))

(defun state-step (&key (state *state*) ignore-whitespace)
  (declare (type state state))
  (let* ((grid (grid state))
         (ip (instruction-pointer state))
         (x (coords-x ip))
         (y (coords-y ip))
         (c (code-char (aref grid x y))))
    (cond ((string-mode-p state)
           (if (char= c #\")
               (setf (string-mode-p state) nil)
               (stack-push state (char-code c))))
          ((and ignore-whitespace (char= c #\Space))
           (loop for ip = (instruction-pointer state)
                 for x = (coords-x ip)
                 for y = (coords-y ip)
                 for c = (code-char (aref grid x y))
                 while (char= c #\Space)
                 do (move-by-direction state)
                 finally (interpret-number-or-instruction x y c state)))
          (t (interpret-number-or-instruction x y c state))))
  (move-by-direction state))

;; TODO: make a nicer "iterate n times or infinitely" construct using macrology.
;; TODO: add the option to ignore whitespace characters completely
(defun interpret-from-stream (stream &key (state *state*) iterations ignore-whitespace)
  (declare (type stream stream)
           (type state state))
  (reinitialize-instance state)
  (populate-grid stream :state state)
  ;; (when ignore-whitespace
  ;;   (delete 32 (grid state)))
  (if iterations
      (dotimes (_ iterations)
        (state-step :state state :ignore-whitespace ignore-whitespace))
      (loop (state-step :state state :ignore-whitespace ignore-whitespace))))

(defmacro interpret-from-file (pathspec &rest args)
  (declare (type (or string pathname) pathspec))
  (with-gensyms ((stream-sym "stream"))
    `(with-open-file (,stream-sym ,pathspec)
       (interpret-from-stream ,stream-sym ,@args))))

(defmacro interpret-from-string (string &rest args)
  (declare (type string string))
  (with-gensyms ((stream-sym "stream"))
    `(with-input-from-string (,stream-sym ,string)
       (interpret-from-stream ,stream-sym ,@args))))
