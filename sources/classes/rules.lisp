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

(defclass redirection-rule ()
  ((kind                 :accessor rr-kind        :initarg :kind
                         :type keyword
                         :documentation "A selector for the matching alogrithm
                         used for this rule.")
   (match                :accessor rr-match       :initarg :match
                         :type (or string list)
                         :documentation "The matching pattern used for this
                         rule.")
   (matcher              :reader   rr-matcher     :initform (lambda (str)
                                                              (declare (ignore str))
                                                              (error "Not Implemented."))
                         :type (function (string) (values t string))
                         :documentation "The matching closure implementing this
                         rule matching algorithm and performing the
                         replacement.")
   (replacement          :accessor rr-replacement :initarg :replacement :initform nil
                         :type (or string null)
                         :documentation "The replacement specification for this
                         string. If it's null, this rule does not modify its
                         target-string.")
   (http-code            :accessor rr-http-code   :initarg :http-code :initform nil
                         :type (or null (integer 300 399))
                         :documentation "The HTTP Status Code sent when this
                         rule is applied. Can be overwritten by a subsequent
                         rule."  )
   (protocol             :accessor rr-protocol    :initarg protocol :initform :http
                         :type (member :http :https)
                         :documentation "The protocol to use after
                         redirection (HTTP or HTTPS)")
   (port                 :accessor rr-port        :initarg port :initform nil
                         :type (or null unsigned-byte)
                         :documentation "Port to use for the
                         redirection. Default depends on the protocol")
   (query-string-updates :accessor rr-qs-updates  :initform '()
                         :type list
                         :documentation "List of query string update performed
                         when this rule match."))
  (:documentation "Root class for redirection rules."))

(defgeneric resolve-rr-matcher (rule)
  (:documentation "This function update the <pre>matcher</pre> slot of <pre>rule</pre> so
<pre>rr-matching-p</pre> can be used. It is the only function allowed to modify
the <pre>matcher</pre> slot"))

(defmethod initialize-instance :after ((instance redirection-rule)
                                       &rest initargs &key &allow-other-keys)
  "Ensures the initialization of the <pre>matcher</pre> slot and the type of the
http-code slot."
  (declare (ignore initargs))
  (check-type (rr-http-code instance) (or null (integer 300 399)))
  (resolve-rr-matcher instance))

(defmethod (setf rr-kind) :after (new-kind (rule redirection-rule))
  "Ensures that every modification of the <pre>kind</pre> slot is updating the
<pre>matcher</pre> slot."
  (declare (ignore new-kind))
  (resolve-rr-matcher rule))

(defmethod (setf rr-match) :after (new-match (rule redirection-rule))
  "Ensures that every modification of the <pre>match</pre> slot is updating the
<pre>matcher</pre> slot."
  (declare (ignore new-match))
  (resolve-rr-matcher rule))

(defmethod (setf rr-replacement) :after (new-replacement (rule redirection-rule))
  "Ensures that every modification of the <pre>replacement</pre> slot is
updating the <pre>matcher</pre> slot."
  (declare (ignore new-replacement))
  (resolve-rr-matcher rule))

(defmethod (setf rr-http-code) :before (new-http-code (rule redirection-rule))
  "Ensures the type of the http-code slot."
  (declare (ignore rule))
  (check-type new-http-code (or null (integer 300 399))))

(defun rr-key (rule)
  "This function returns the pair (kind match) for this rule."
  (list (rr-kind rule) (rr-match rule)))

(defgeneric rr-summary (rule &key serialize)
  (:documentation "This method converts the redirection rule to the list (kind
  match replacement . options)")
  (:method (rule &key serialize)
    "This method converts the redirection rule to the list (kind match
replacement http-code)"
    (list* (rr-kind        rule)
           (rr-match       rule)
           (rr-replacement rule)
           (rr-http-code   rule)
           (rr-protocol    rule)
           (rr-port        rule)
           (when serialize
             (list (rr-qs-updates rule))))))

(defmethod print-object ((object redirection-rule) stream)
  "Print a meaningful summarized representation of redirection
rules. Redirection rules are not readably printable."
  (print-unreadable-object (object stream :type t)
    (write (rr-summary object) :stream stream)))

(defun redirection-rule= (rule1 rule2 &rest rules)
  "This function checks whether two redirection rules are equals."
  (and (eq      (rr-kind rule1)  (rr-kind rule2))
       (string= (rr-match rule1) (rr-match rule2))
       (or (emptyp rules)
           (apply #'redirection-rule= rule2 rules))))

(defun rr-matching-p (rule target-string)
  "This function checks whether a redirection rule is matching for this haystack."
  (funcall (rr-matcher rule) target-string))

(defun create-regex-matcher (regex replacement)
  "This function creates the matcher for a rule based on a regex."
  (flet ((regex-replace* (regex target-string replacement)
           "Just like cl-ppcre:regex-replace, but reverse the order of the
returned values."
           (multiple-value-bind (string matchp)
               (regex-replace regex target-string replacement)
             (values matchp string))))
    (if replacement
        (lambda (target-string)
          (regex-replace* regex target-string replacement))
        (lambda (target-string)
          (values (scan regex target-string) target-string)))))

(defclass uri-redirection-rule (redirection-rule) ()
  (:documentation "Class for redirection rules applied to URIs (path part of the
  URL, no query-string)."))

(defmethod resolve-rr-matcher ((rule uri-redirection-rule))
  "This function updates the <pre>matcher</pre> slot for a URI redirection rule."
  (with-slots (match replacement) rule
    (setf (slot-value rule 'matcher)
          (ecase (rr-kind rule)

            (:exact  (if replacement
                         (lambda (target-string)
                           (values (string= match target-string) replacement))
                         (lambda (target-string)
                           (values (string= match target-string) target-string))))

            (:prefix (create-regex-matcher
                      (create-scanner `(:sequence :modeless-start-anchor ,match)
                                      :single-line-mode t)
                      replacement))
            
            (:regex  (create-regex-matcher
                      (create-scanner match :single-line-mode t)
                      replacement))))))

(defparameter +uri-redirection-rule-code+ (register-code 201 'uri-redirection-rule)
  "cl-store type-code for URI redirection rules.")

(defstore-cl-store (rule uri-redirection-rule stream)
  (let ((*check-for-circs* nil))
    (output-type-code +uri-redirection-rule-code+ stream)
    (store-object (rr-summary rule :serialize t) stream)))

(defrestore-cl-store (uri-redirection-rule stream)
  (destructuring-bind (kind match replacement http-code protocol port qsus)
      (restore-object stream)
    (let* ((rule (make-instance 'uri-redirection-rule
                                :kind kind :match match :replacement replacement
                                :http-code http-code
                                :protocol protocol :port port)))
      (setf (rr-qs-updates rule) qsus)
      rule)))

(defclass domain-redirection-rule (redirection-rule)
  ((uri-rules :accessor drr-uri-rules :initform nil
              :type list
              :documentation "List of URI redirection rules for the domain names
              matching this redirection rule."))
  (:documentation "Class for redirection rules applied to domain names."))

(defmethod rr-summary ((rule domain-redirection-rule) &key serialize)
  "This method adds the list of uri rules in the case of serialization."
  (nconc (call-next-method)
         (when serialize
           (list (drr-uri-rules rule)))))

(defmethod resolve-rr-matcher ((rule domain-redirection-rule))
  "This function updates the <pre>matcher</pre> slot for a domain name redirection rule."
  (with-slots (match replacement) rule
    (setf (slot-value rule 'matcher)
          (ecase (rr-kind rule)

            (:exact  (if replacement
                         (lambda (target-string)
                           (values (string-equal match target-string) replacement))
                         (lambda (target-string)
                           (values (string-equal match target-string) target-string))))
            
            (:suffix (create-regex-matcher
                      (create-scanner `(:sequence ,match :modeless-end-anchor-no-newline)
                                      :single-line-mode t :case-insensitive-mode t)
                      replacement))

            (:regex  (create-regex-matcher
                      (create-scanner match :single-line-mode t :case-insensitive-mode t)
                      replacement))))))

(defparameter +domain-redirection-rule-code+ (register-code 200 'domain-redirection-rule)
  "cl-store type-code for domain name redirection rules.")

(defstore-cl-store (rule domain-redirection-rule stream)
  (let ((*check-for-circs* nil))
    (output-type-code +domain-redirection-rule-code+ stream)
    (store-object (rr-summary rule :serialize t) stream)))

(defrestore-cl-store (domain-redirection-rule stream)
  (destructuring-bind (kind match replacement http-code protocol port qsus uri-rules)
      (restore-object stream)
    (let* ((rule (make-instance 'domain-redirection-rule
                                :kind kind :match match :replacement replacement
                                :http-code http-code
                                :protocol protocol :port port)))
      (setf (rr-qs-updates rule) qsus
            (drr-uri-rules rule) uri-rules)
      rule)))

(defun drr-is-loop-p (drr &key error-p)
  "Check if the domain name rule effect is empty, thus resulting in a loop."
  (or (rr-replacement drr)
      (drr-uri-rules drr)
      (when error-p
        (error 'rs-loop-detected))))
