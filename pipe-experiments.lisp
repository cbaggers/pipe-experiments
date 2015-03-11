;;;; pipe-experiments.lisp

(in-package #:pipe-experiments)

(defun symb (&rest args) (values (intern (format nil "~{~a~}" args))))

;;----------------------------------------------------------------------

(defmacro => (&body forms)
  (labels ((^>expander (forms index)
             (if forms
                 (let ((stage-name (symb '^ index)))
                   `(let* ((^ ,(first forms))
                           (,stage-name ^))
                      (declare (ignorable ^ ,stage-name))
                      ,(^>expander (rest forms) (1+ index))))
                 '^)))
    (^>expander forms 0)))

(print (=> (* 1 2)
           (* ^ ^ ^)
           (/ ^ ^0)))

;;----------------------------------------------------------------------

(defmacro dmap (bindings expression &body body)
  "(d-map ((x) y z) '((1 2 3) (4 5 6) (7 8 9))
    (print x)
    (print y)
    (print z))"
  (let* ((appends (mapcar #'listp bindings))
         (bindings (mapcar (lambda (b) (if (listp b) (first b) b))
                           (break "" bindings)))
         (syms (mapcar (lambda (x) (gensym (symbol-name x))) bindings)))
    `(destructuring-bind ,bindings
         (loop :for ,syms :in ,expression
            ,@(mapcan (lambda (b s a) `(,(if a :append :collect) ,s :into ,b))
                      bindings syms appends)
            :finally (return ,(cons 'list bindings)))
       ,@body)))

;;----------------------------------------------------------------------

(defmacro /> (&body forms)
  (labels ((_ (list)
             (when list
               (if (listp (first list))
                   (list (append (first list) (_ (rest list))))
                   (if (rest list)
                       (error "invalid form")
                       (list (first list)))))))
    `(labels ((_ (arg) ,(first (_ (reverse (cons 'arg (rest forms)))))))
       (_ ,(first forms)))))


(/> 1
    (* 2)
    (print))

;;----------------------------------------------------------------------

(defmacro pipe-> (args &body stages)
  "\(pipe-> \(1 2 3\) #'a #'b #'c #'d\)
   Calls first function with args provided and uses result as
   arguments for next function. Uses multiple-value-call so you
   can use (values) to specify complex lambda-args."
  (let ((stages (reverse stages)))
    (when stages
      (let ((stage (first stages)))
        (if (eq 'function (first stage))
            `(multiple-value-call ,stage
               ,(if (rest stages)
                    `(pipe-> ,args ,@(reverse (rest stages)))
                    (if (listp args)
                        `(values ,@args)
                        `(values-list ,args))))
            (destructuring-bind (check-func &rest steps) stage
              `(let ((rest (multiple-value-list
                            ,(if (rest stages)
                                 `(pipe-> ,args ,@(reverse (rest stages)))
                                 (if (listp args)
                                     `(values ,@args)
                                     `(values-list ,args))))))
                 (let ((args rest))
                   (let ((passes nil))
                     (loop :do (let ((results (multiple-value-list
                                               (pipe-> ,@(cons 'args steps)))))
                                 (setf args results)
                                 (push results passes))
                        :until (,check-func (first passes) (second passes))))
                   (values-list args)))))))))

(pipe-> (in-args uniforms context body env)
  #'split-input-into-env
  #'process-context
  #'wrap-in-main-function
  #'add-context-glsl-vars
  (equalp #'symbol-macroexpand-pass
          #'macroexpand-pass
          #'compiler-macroexpand-pass)
  #'compile-pass
  #'filter-used-items
  #'check-stemcells
  #'gen-in-arg-strings)

;;----------------------------------------------------------------------

;;----------------------------------------------------------------------

(defmacro -> (args &body stages)
  (let ((stages (reverse stages)))
    (when stages
      (let ((stage (first stages)))
        (if (eq 'function (first stage))
            `(multiple-value-call ,stage
               ,(if (rest stages)
                    `(pipe-> ,args ,@(reverse (rest stages)))
                    (if (listp args)
                        `(values ,@args)
                        `(values-list ,args))))
            (destructuring-bind (check-func &rest steps) stage
              `(let ((rest (multiple-value-list
                            ,(if (rest stages)
                                 `(pipe-> ,args ,@(reverse (rest stages)))
                                 (if (listp args)
                                     `(values ,@args)
                                     `(values-list ,args))))))
                 (let ((args rest))
                   (let ((passes nil))
                     (loop :do (let ((results (multiple-value-list
                                               (pipe-> ,@(cons 'args steps)))))
                                 (setf args results)
                                 (push results passes))
                        :until (,check-func (first passes) (second passes))))
                   (values-list args)))))))))

(pipe-> (in-args uniforms context body env)
  #'split-input-into-env
  #'process-context
  #'process-in-args
  #'process-uniforms
  #'wrap-in-main-function
  #'add-context-glsl-vars
  (equalp #'symbol-macroexpand-pass
          #'macroexpand-pass
          #'compiler-macroexpand-pass)
  #'compile-pass
  #'filter-used-items
  #'check-stemcells
  #'gen-in-arg-strings
  #'gen-out-var-strings
  #'final-uniform-strings
  #'dedup-strings
  #'final-string-compose
  #'code-obj->result-object)

(symbol-macrolet ((a 1))
  (list a a
        (symbol-macrolet ((a 2))
          a 3)))

(defun make-call (form)
  (if (eq (first form) 'function)
      `(apply ,form ^)
      form))

(defun stuff (forms &optional (depth 0) previous)
  (let ((sym (gensym "last")))
    `(symbol-macrolet ,(loop :for p :in previous :for i :from 1 :collect
                          `(,(if (= 0 (- i 1)) '^ (symb '^ (- i 1))) ,p))
       ,(if forms
            `(let ((,sym (multiple-value-list ,(make-call (first forms)))))
               ,(stuff (rest forms) (1+ depth) (cons sym previous)))
            '^))))

(macroexpand-dammit
 (stuff '(#'split-input-into-env
          #'process-context
          #'process-in-args)))
