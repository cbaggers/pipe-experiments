;;;; pipe-experiments.lisp

(in-package #:pipe-experiments)

;;----------------------------------------------------------------------

(defmacro => (&body forms)
  (labels ((=>expander (forms index)  
             (if forms
                 (let ((stage-name (symb '^ index)))
                   `(let* ((^ ,(first forms))
                           (,stage-name ^))
                      (declare (ignorable ^ ,stage-name))
                      ,(^>expander (rest forms) (1+ index))))
                 '^)))
    (=>expander forms 0)))

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

;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
