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

(define-condition rs-error (error) ()
  (:documentation "Root condition for redirection server errors."))

(define-condition rs-loop-detected (rs-error) ()
  (:documentation "Error raised if a loop is detected by the redirection
    engine. If not caught, this loop will make the server reply with a \"404 Not
    Found\"."))

(define-condition rs-no-such-qs-update (rs-error)
  ((operation :reader qsu-operation :initarg :operation)
   (name      :reader qsu-name      :initarg :name :initform nil)
   (match     :reader qsu-match     :initarg :match :initform nil))
  (:documentation "Error raised if a query string update cannot be found."))

(define-condition rs-no-such-rule (rs-error)
  ((kind  :reader rr-kind  :initarg :kind)
   (match :reader rr-match :initarg :match))
  (:documentation "Error raised if a rule cannot be found."))

(define-condition rs-no-such-domain-rule (rs-no-such-rule) ()
  (:documentation "Error raised if a domain name rule cannot be found."))

(define-condition rs-no-such-uri-rule (rs-no-such-rule)
  ((domain-name-rule :accessor urr-domain-name-rule :initarg :domain-name-rule))
  (:documentation "Error raised if an URI rule cannot be found."))
