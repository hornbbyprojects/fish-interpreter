;;;; fish-interpreter.lisp

(in-package #:fish-interpreter)

(defstruct fish-frame
  (register nil)
  (stack nil))

(defstruct fish-state
  (frames (list (make-fish-frame)))
  (direction (list 1 0))
  (position (list 0 0))
  (text-mode nil)
  (instruction-grid (list)))

(defun fish-state-height (fish-state)
  (length (fish-state-instruction-grid fish-state)))
(defun fish-state-width (fish-state)
  (if (null (fish-state-instruction-grid fish-state))
      0
      (length (car (fish-state-instruction-grid fish-state)))))

(defun pop-stack (fish-state)
  (let ((ret (pop (fish-frame-stack (car (fish-state-frames fish-state))))))
    (if (null ret)
        (error "Popped off an empty stack!")
        ret)))

(defun push-stack (fish-state value)
  (push value (fish-frame-stack (car (fish-state-frames fish-state)))))

(defstruct op
  discriminator
  text-char
  (specific-op nil))

(defun perform-op-swap (fish-state)
  (let ((a (pop-stack fish-state))
        (b (pop-stack fish-state)))
    (push-stack fish-state a)
    (push-stack fish-state b)))
(defun perform-op-direction-up (fish-state)
  (setf (fish-state-direction fish-state) '(0 -1)))

(defun perform-op-direction-down (fish-state)
  (setf (fish-state-direction fish-state) '(0 1)))

(defun perform-op-direction-left (fish-state)
  (setf (fish-state-direction fish-state) '(-1 0)))

(defun perform-op-direction-right (fish-state)
  (setf (fish-state-direction fish-state) '(1 0)))

(defun perform-op-vertical-mirror (fish-state)
  (setf (fish-state-direction fish-state)
        (list (- (car (fish-state-direction fish-state)))
              (cadr (fish-state-direction fish-state)))))

(defun perform-op-horizontal-mirror (fish-state)
  (setf (fish-state-direction fish-state)
        (list (car (fish-state-direction fish-state))
              (- (cadr (fish-state-direction fish-state))))))

(defun perform-op-forward-slash-mirror (fish-state)
  (setf (fish-state-direction fish-state)
        (list (- (cadr (fish-state-direction fish-state)))
              (- (car (fish-state-direction fish-state))))))

(defun perform-op-backward-slash-mirror (fish-state)
  (setf (fish-state-direction fish-state)
        (list (cadr (fish-state-direction fish-state))
              (car (fish-state-direction fish-state)))))



(defun perform-op-create-stack (fish-state)
  (let* ((num-preserved (pop-stack fish-state))
         (new-stack (loop for i from 1 to num-preserved
                          collect (pop-stack fish-state)))
         (new-frame (make-fish-frame :stack new-stack)))
    (push new-frame (fish-state-frames fish-state))))

(defun perform-op-remove-stack (fish-state)
  (if (null (cdr (fish-state-frames fish-state)))
      (setf (car (fish-state-frames fish-state)) (make-fish-frame))
      (let ((old-stack (fish-frame-stack (pop (fish-state-frames fish-state)))))
        (loop for value in (reverse old-stack)
              do (push value (fish-frame-stack (car (fish-state-frames fish-state))))))))

(defun perform-op-reverse-stack (fish-state)
  (macrolet ((first-frame () `(car (fish-state-frames fish-state))))
    (macrolet ((first-stack () `(fish-frame-stack (first-frame))))
      (setf (first-stack) (reverse (first-stack))))))

(defun perform-op-print-number (fish-state)
  (let ((to-print (pop-stack fish-state)))
    (format t "~a" to-print)))

(defun perform-op-print-char (fish-state)
  (let ((to-print (pop-stack fish-state)))
    (format t "~a" (code-char to-print))))


(defun wrap-cursor (fish-state)
  (let ((position (fish-state-position fish-state)))
    (setf (car position) (mod (car position) (fish-state-width fish-state)))
    (setf (cadr position) (mod (cadr position) (fish-state-height fish-state)))
    (setf (fish-state-position fish-state) position)))

(defun move-cursor (fish-state)
  (incf (car (fish-state-position fish-state)) (car (fish-state-direction fish-state)))
  (incf (cadr (fish-state-position fish-state)) (cadr (fish-state-direction fish-state)))
  (wrap-cursor fish-state))

(defun perform-op-push-stack (fish-state value)
  (declare (type integer value))
  (push-stack fish-state value))

(defun perform-op-toggle-text-mode (fish-state)
  (setf (fish-state-text-mode fish-state) (not (fish-state-text-mode fish-state))))

(defun perform-op-trampoline (fish-state)
  (move-cursor fish-state))

(defun perform-op-conditional-trampoline (fish-state)
  (let ((next-value (pop-stack fish-state)))
    (when (= 0 next-value)
      (move-cursor fish-state))))

(defun perform-op-duplicate (fish-state)
  (let ((top-value (car (fish-frame-stack (car (fish-state-frames fish-state))))))
    (push-stack fish-state top-value)))

(defun perform-op-register (fish-state)
  (declare (type fish-state fish-state))
  (let ((first-frame (car (fish-state-frames fish-state))))
    (if (null (fish-frame-register first-frame))
        (setf (fish-frame-register first-frame) (pop (fish-frame-stack first-frame)))
        (progn
          (push (fish-frame-register first-frame) (fish-frame-stack first-frame))
          (setf (fish-frame-register first-frame) nil)))))

(defmacro perform-binary-op (fish-state a b &body body)
  (let ((result (gensym)))
    `(let* ((,a (pop-stack ,fish-state))
            (,b (pop-stack ,fish-state))
            (,result (progn ,@body)))
       (push-stack ,fish-state ,result))))

(defun perform-op-jump (fish-state)
  (let ((y (pop-stack fish-state))
        (x (pop-stack fish-state)))
    (setf (fish-state-position fish-state) (list x y))))

(defun perform-op (fish-state op)
  (ecase (op-discriminator op)
    (:op-nop)
    (:op-data-only (error (format nil"Attempted to perform an unrecognised instruction char: ~a" (code-char (op-text-char op)))))
    (:op-direction-up (perform-op-direction-up fish-state))
    (:op-direction-down (perform-op-direction-down fish-state))
    (:op-direction-left (perform-op-direction-left fish-state))
    (:op-direction-right (perform-op-direction-right fish-state))
    (:op-horizontal-mirror (perform-op-horizontal-mirror fish-state))
    (:op-vertical-mirror (perform-op-vertical-mirror fish-state))
    (:op-forward-slash-mirror (perform-op-forward-slash-mirror fish-state))
    (:op-backward-slash-mirror (perform-op-backward-slash-mirror fish-state))
    (:op-print-number (perform-op-print-number fish-state))
    (:op-print-char (perform-op-print-char fish-state))
    (:op-create-stack (perform-op-create-stack fish-state))
    (:op-remove-stack (perform-op-remove-stack fish-state))
    (:op-reverse-stack (perform-op-reverse-stack fish-state))
    (:op-push-stack (perform-op-push-stack fish-state (op-specific-op op)))
    (:op-toggle-text-mode (perform-op-toggle-text-mode fish-state))
    (:op-duplicate (perform-op-duplicate fish-state))
    (:op-trampoline (perform-op-trampoline fish-state))
    (:op-conditional-trampoline (perform-op-conditional-trampoline fish-state))
    (:op-quit (return-from perform-op t))
    (:op-register (perform-op-register fish-state))
    (:op-add (perform-binary-op fish-state a b (+ a b)))
    (:op-sub (perform-binary-op fish-state a b (- a b)))
    (:op-mul (perform-binary-op fish-state a b (* a b)))
    (:op-div (perform-binary-op fish-state a b (/ a b)))
    (:op-mod (perform-binary-op fish-state a b (mod a b)))
    (:op-equals (perform-binary-op fish-state a b (if (= a b) 1 0)))
    (:op-lt (perform-binary-op fish-state a b (if (< a b) 1 0)))
    (:op-gt (perform-binary-op fish-state a b (if (> a b) 1 0)))
    (:op-swap (perform-op-swap fish-state))
    (:op-jump (perform-op-jump fish-state))
    (:op-print-position (format t "~a~%" (fish-state-position fish-state))))
  nil)


(defun whitespace-char-p (c)
  (case c
    (#\space t)
    (#\newline t)
    (#\tab t)))

(defun parse-op (c)
  (declare (type character c))
  (let ((ret
          (cond
            ((and (<= (char-code c) (char-code #\9)) (>= (char-code c) (char-code #\0))) (make-op :discriminator :op-push-stack :specific-op (- (char-code c) (char-code #\0))))
            ((and (<= (char-code c) (char-code #\f)) (>= (char-code c) (char-code #\a))) (make-op :discriminator :op-push-stack :specific-op (+ 10 (- (char-code c) (char-code #\a)))))
            ((equal c #\>) (make-op :discriminator :op-direction-right))
            ((equal c #\^) (make-op :discriminator :op-direction-up))
            ((equal c #\<) (make-op :discriminator :op-direction-left))
            ((equal c #\v) (make-op :discriminator :op-direction-down))
            ((equal c #\n) (make-op :discriminator :op-print-number))
            ((equal c #\o) (make-op :discriminator :op-print-char))
            ((equal c #\|) (make-op :discriminator :op-vertical-mirror))
            ((equal c #\-) (make-op :discriminator :op-horizontal-mirror))
            ((equal c #\/) (make-op :discriminator :op-forward-slash-mirror))
            ((equal c #\\) (make-op :discriminator :op-backward-slash-mirror))
            ((equal c #\[) (make-op :discriminator :op-create-stack))
            ((equal c #\]) (make-op :discriminator :op-remove-stack))
            ((equal c #\r) (make-op :discriminator :op-reverse-stack))
            ((equal c #\space) (make-op :discriminator :op-nop))
            ((equal c #\;) (make-op :discriminator :op-quit))
            ((or (equal c #\") (equal c #\')) (make-op :discriminator :op-toggle-text-mode))
            ((equal c #\:) (make-op :discriminator :op-duplicate))
            ((equal c #\&) (make-op :discriminator :op-register))
            ((equal c #\!) (make-op :discriminator :op-trampoline))
            ((equal c #\?) (make-op :discriminator :op-conditional-trampoline))
            ((equal c #\+) (make-op :discriminator :op-add))
            ((equal c #\-) (make-op :discriminator :op-sub))
            ((equal c #\*) (make-op :discriminator :op-mul))
            ((equal c #\,) (make-op :discriminator :op-div))
            ((equal c #\%) (make-op :discriminator :op-mod))
            ((equal c #\=) (make-op :discriminator :op-equals))
            ((equal c #\() (make-op :discriminator :op-lt))
            ((equal c #\)) (make-op :discriminator :op-gt))
            ((equal c #\$) (make-op :discriminator :op-swap))
            ((equal c #\.) (make-op :discriminator :op-jump))
            ((whitespace-char-p c) nil)
            (t (make-op :discriminator :op-data-only))
            )))
    (setf (op-text-char ret) (char-code c))
    ret))

(defun parse-program-single-line (program-string)
  (loop for c across program-string
        for op = (parse-op c)
        when (not (null op))
          collect op))

(defun parse-program (in-stream)
  (loop
    collect (handler-case (parse-program-single-line (read-line in-stream))
              (end-of-file () (loop-finish)))))

(defun run-program (program &key (slow-mode nil) (debug-instructions nil))
  (let ((fish-state (make-fish-state :instruction-grid program)))
    (loop
      for (x y) = (fish-state-position fish-state)
      for op = (nth x (nth y (fish-state-instruction-grid fish-state)))
      when debug-instructions do

         (format t "Processing ~a (~a: ~a).~%" (list x y) (code-char (op-text-char op)) (op-discriminator op))
      when slow-mode do
        (sleep slow-mode)
      do
         (if (fish-state-text-mode fish-state)
             (if (equal (op-discriminator op) :op-toggle-text-mode)
                 (perform-op fish-state op)
                 (perform-op-push-stack fish-state (op-text-char op)))
             (when (perform-op fish-state op)
               (loop-finish)))
         (move-cursor fish-state))))

(defun run-file (filename &rest args)
  (with-open-file (in-stream filename)
    (let ((program (parse-program in-stream)))
      (apply #'run-program program args))))
