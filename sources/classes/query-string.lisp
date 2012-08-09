;;; Copyright © 2012, Mathieu Lemoine <mlemoine@mentel.com>, Mentel Inc.
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of "Mentel Inc." nor the names of its contributors may be
;;;       used to endorse or promote products derived from this software without
;;;       specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(cl:in-package :cl-cheshire-cat)

(defclass query-string-update ()
  ()
  (:documentation "Root class for Query String operations."))

(defun class<-qsu-operation (operation)
  "Returns the class corresponding to this query-string operation."
  (let ((qsu-class-name (ensure-symbol (string-upcase (concatenate 'string "qs-" (symbol-name operation)))
                                       :cl-cheshire-cat)))
    (find-class qsu-class-name)))

(defun qsu-operation<-class (class)
  "Returns the query-string operation corresponding to this class."
  (make-keyword (subseq (symbol-name (class-name class)) 3)))

(defmethod qsu-operation (update)
  "Returns the operation of this update."
  (qsu-operation<-class (class-of update)))

(defun make-query-string-update (operation &rest initargs)
  "Generic generation of a query string update. <pre>operation</pre> must be one
  of :clear, :add, :rename or :update and initargs is the list of initargs for
  the qs-{operation} class."
  (apply #'make-instance (class<-qsu-operation operation) initargs))

(defgeneric qsu-key (update)
  (:documentation "Return the information required to identify this
  update. Usually a list of the form (operation &optional name match)")
  (:method (update)
    "Default method returns (operation name)."
    (list (qsu-operation update) (qsu-name update))))

(defparameter *qsu-generic-readers* '()
  "List of query string update generic readers")

(defmacro define-generic-qsu-reader (name &optional default-value)
  "Create a generic function and an associated method for
  query-string-update. This allow every reader to be used with any instance of
  query-string-update."
  (let ((reader-name (symbolicate 'qsu- name)))
    `(progn
       (defgeneric ,reader-name (update)
         (:documentation ,(format nil "Generic query string update reader for ~A." name))
         (:method ((update query-string-update))
           ,(format nil "Default value for ~A reader." name)
           ,default-value))
       (push (cons ,(make-keyword name) (fdefinition ',reader-name)) *qsu-generic-readers*))))

(define-generic-qsu-reader name)
(define-generic-qsu-reader new-name)
(define-generic-qsu-reader value)
(define-generic-qsu-reader match)
(define-generic-qsu-reader replacement)

(defgeneric update-query-string (update query-string domain-name path)
  (:documentation "This function (destructively) updates query-string to apply
  the specified query-string update. query-string is expected to be an alist of
  get parameters."))

(defgeneric qsu-summary (update)
  (:documentation "This method converts the redirection rule to the
  alist ((:operation . operation) params)")
  (:method (update)
    `((:operation . ,(qsu-operation update))
      ,@(mapcan (lambda (reader-spec)
                  `((,(car reader-spec) . ,(funcall (cdr reader-spec) update))))
                *qsu-generic-readers*))))

(defmethod print-object ((object query-string-update) stream)
  "Print a meaningful summarized representation of redirection
rules. Redirection rules are not readably printable."
  (print-unreadable-object (object stream :type t)
    (write (qsu-summary object) :stream stream)))

(defun unsummarize-query-string-update% (summary)
  "Return the list of arguments to give to make-instance to recreate a similar
query-string-update."
  `(,(class<-qsu-operation (cdr (assoc :operation summary)))
     ,@(mapcan (lambda (reader-spec)
                 (let ((kw (car reader-spec)))
                   (when-let (value (cdr (assoc kw summary)))
                     `((,kw . ,value)))))
               *qsu-generic-readers*)))

(defparameter +qsu-code+ (register-code 210 'query-string-update)
  "cl-store type-code for query string updates.")

(defstore-cl-store (rule query-string-update stream)
  (let ((*check-for-circs* nil))
    (output-type-code +qsu-code+ stream)
    (store-object (qsu-summary rule) stream)))

(defrestore-cl-store (query-string-update stream)
  (apply #'make-query-string-update
         (apply #'unsummarize-query-string-update% (restore-object stream))))

(defclass qs-clear (query-string-update)
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class)
  (:documentation "Clear the query string. Since this class has no state nor
  parameters, it's a singleton."))

(defmethod update-query-string ((update qs-clear) query-string domain-name path)
  "This method return an empty new alist."
  (declare (ignore update query-string domain-name path))
  '())

(defclass qs-add (query-string-update)
  ((name  :reader qsu-name  :initarg :name
          :type string
          :documentation "Name of the parameter to create.")
   (value :reader qsu-value :initarg :value
          :type (or (member :path :domain) string)
          :documentation "Value of the parameter to create. If the value
  is :path or :domain, the corresponding part of the URL will be used as the
  value."))
  (:documentation "Add a new parameter at the beginning the query string."))

(defmethod update-query-string ((update qs-add) query-string domain-name path)
  "This method adds a new parameter at the beginning of the query string"
  (let* ((value (qsu-value update))
         (value (case value
                  (:path   path)
                  (:domain domain-name)
                  (t       value))))
    (acons (qsu-name update) value query-string)))

(defmethod qsu-key ((update qs-clear))
  "This method always returns '(:clear)."
  '(:clear))

(defclass qs-rename (query-string-update)
  ((name     :reader qsu-name     :initarg :name
             :type string
             :documentation "Name of the parameter to rename (old name).")
   (new-name :reader qsu-new-name :initarg :new-name
             :type string
             :documentation "New name for the parameter to rename."))
  (:documentation "Rename the parameter in the query string."))

(defmethod update-query-string ((update qs-rename) query-string domain-name path)
  "This method renames a query string parameter. If there are several query
  string parameters with this name, only the first one is renamed. If there is
  no such parameter, do nothing."
  (declare (ignore domain-name path))
  (when-let (param (assoc (qsu-name update) query-string :test #'string=))
    (rplaca param (qsu-new-name update)))
  query-string)

(defclass qs-update (query-string-update)
  ((name        :reader qsu-name        :initarg :name
                :type string
                :documentation "Name of the parameter to update.")
   (match       :reader qsu-match       :initarg :match
                :type string
                :documentation "Regex to match the old value of the parameter")
   (replacement :reader qsu-replacement :initarg :replacement
                :type string
                :documentation "Replacement string to generate the new value of the parameter")
   (matcher     :reader qsu-matcher
                :type t
                :documentation "Compiled regex matcher"))
  (:documentation "Use a regex to modify the value of a parameter in the query string."))

(defmethod initialize-instance :after ((instance qs-update) &rest initargs &key &allow-other-keys)
  "Creates the regex matcher."
  (declare (ignore initargs))
  (set (slot-value instance 'matcher)
       (create-scanner (qsu-match instance))))

(defmethod update-query-string ((update qs-update) query-string domain-name path)
  "This method updates the value of a parameter using a regex replacement. If
  there are several query string parameters with this name, only the first one
  is renamed. If there is no such parameter, do nothing."
  (declare (ignore domain-name path))
  (when-let (param (assoc (qsu-name update) query-string :test #'string=))
    (rplacd param (regex-replace (qsu-matcher update) (cdr param) (qsu-replacement update)))))

(defmethod qsu-key ((update qs-update))
  "This methods returns (:update name match)."
  (list :update (qsu-name update) (qsu-match update)))

(defclass qs-delete (query-string-update)
  ((name :reader qsu-name :initarg :name
         :type string
         :documentation "Name of the parameter to delete."))
  (:documentation "Remove a parameter from the query string."))

(defmethod update-query-string ((update qs-delete) query-string domain-name path)
  "This method deletes a query string parameter. If there are several query
  string parameters with this name, only the first one is deleted. If there is
  no such parameter, do nothing."
  (declare (ignore domain-name path))
  (delete-if (curry #'string= (qsu-name update))
             query-string
             :key #'car :count 1))
