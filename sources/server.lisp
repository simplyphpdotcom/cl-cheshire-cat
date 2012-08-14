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

(defclass redirection-acceptor (two-steps-start-acceptor)
  ((admin-host      :accessor redirection-acceptor-admin-host
                    :initarg :admin-host :initform "management.invalid"
                    :type string
                    :documentation "The domain name used to manage this
                    redirection acceptor.")
   (admin-allowed   :accessor redirection-acceptor-admin-allowed
                    :initarg :admin-allowed :initform '(("127.0.0.1"))
                    :type list
                    :documentation "A list of CIDR block specifications. Each item
                    of this list is a pair (IP prefix-length). IP is recommended
                    to a string using the decimal dotted notation but could also
                    be an host order 32 bytes integer or an host order byte
                    vector.")
   (rules           :accessor redirection-acceptor-rules
                    :initform '()
                    :type list
                    :documentation "The list of redirection rules used by this
                    acceptor.")
   (rules-directory :accessor redirection-acceptor-rules-directory
                    :type pathname
                    :initarg :rule-directory
                    :documentation "The directory in which rules will be
                    stored. Only the default rule file can be out of this
                    directory." )
   (rules-file      :reader redirection-acceptor-rules-file
                    :type string
                    :documentation "The default file used to store the
                    rules. This is set when loading the rules from the file."))
  (:documentation "Custom hunchentoot:acceptor implementing the behavior of the
  redirection server."))

(defmethod initialize-instance :after ((instance redirection-acceptor) &rest initargs &key &allow-other-keys)
  "Ensures there is no error template directory."
  (declare (ignore initargs))
  (setf (acceptor-error-template-directory instance) nil
        (acceptor-access-log-destination instance) *standard-output*))

(defmethod (setf redirection-acceptor-rules-directory) :around (new-directory (acceptor redirection-acceptor))
  "Ensures the directory is a directory with no name and the crr type."
  (call-next-method (make-pathname :name nil :type "crr"
                                   :defaults new-directory)
                    acceptor))

(defmethod acceptor-dispatch-request ((acceptor redirection-acceptor) request)
  "This request dispatcher processes each HTTP request and handle adequatly the
request."
  (if (string-equal (redirection-acceptor-admin-host acceptor)
                    (host *request*))
      (admin-handler acceptor)
      (handler-case
          (destructuring-bind (domain-name path &optional (http-status-code +http-moved-permanently+) protocol port qs-updates)
              (compute-redirection (redirection-acceptor-rules acceptor)
                                   (or (host *request*) "") (script-name* *request*))
            (let ((query-string (copy-alist (get-parameters* *request*))))
              (dolist (update qs-updates)
                (setf query-string (update-query-string update query-string (host *request*) (script-name* *request*))))
              (redirect (compute-uri path query-string) :host domain-name
                        :code (or http-status-code +http-moved-permanently+)
                        :protocol (or protocol :http)
                        :port port)))
        (rs-loop-detected ()
          (setf (return-code* *reply*) +http-not-found+)))))

(defmethod acceptor-log-access ((acceptor redirection-acceptor) &key return-code)
  "Adapted syntax for access log, just like the default, but include the host to."
  (with-log-stream (stream (acceptor-access-log-destination acceptor) *access-log-lock*)
    (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" (Host: ~A) ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization)
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            (host)
            return-code
            (content-length*)
            (referer)
            (user-agent))))

(defun load-rules (redirection-acceptor file)
  "This function restore the list of rules from file and set them as the list of
  rules for this acceptor. It's also registering the rule-file for future
  references."
  (setf (redirection-acceptor-rules redirection-acceptor) (when (file-exists-p file)
                                                            (restore file))
        (slot-value redirection-acceptor 'rules-file)      file)
  (when (not (slot-boundp redirection-acceptor 'rules-directory))
    (setf (redirection-acceptor-rules-directory redirection-acceptor) file)))

(defun save-rules (redirection-acceptor file)
  (store (redirection-acceptor-rules redirection-acceptor)
         (if file
             (merge-pathnames (pathname-name file)
                              (redirection-acceptor-rules-directory redirection-acceptor))
             (redirection-acceptor-rules-file redirection-acceptor))))
