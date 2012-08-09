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

(defun parse-cidr-list (list)
  "Parse a comma-separated list of IPv4 CIDR specifications (such as
\"a.b.c.d/n, e.f.g.h/m\"). The CIDR prefix may be omited. Return a list of CIDR
specification, each being of the form (IP prefix-length)."
  (mapcar (lambda (item)
            (split-sequence #\/
                            (string-trim '(#\Space) item)))
          (split-sequence #\, list)))

(defun ip-cidr-match-p (ip1 ip2 &optional (prefix-length 32))
  "Check if both IPs are in the same network with respect to this CIDR prefix-length.

Each IPs can be either:
 * 32-bit positive integer (host order),
 * a four element integer list representing IPv4 address, i.e. #(127 0 0 1),
 * a string containing an IP addres in dotted notation.

For example, to check A.B.C.D against a.b.c.d/n, this function could be invoked
as (ip-cidr-match-p #(A B C D) #(a b c d) n)."
  (let ((mask (ash -1 prefix-length)))
    (= (logand (host-byte-order ip1) mask)
       (logand (host-byte-order ip2) mask))))

(defun send-bad-request (message &optional (http-code +http-bad-request+))
  "Error handling, abort the URL processing to signal an error to the client."
  (setf (return-code* *reply*) http-code)
  (abort-request-handler message))

(defun parse-integer-or-nil (string &rest keys)
  "Just like parse-integer, but if string is nil, returns (values nil nil) instead of throwing an error."
  (and string
       (apply #'parse-integer string keys)))

(defun compute-uri (path query-string)
  "Append each parameter in the query string to the path. path is expected not
to have any query-string."
  (let ((query-string (when query-string
                        (format nil "?~{~{~A~^=~}~^&~}"
                                (mapcar (lambda (arg)
                                          (list (url-encode (car arg))
                                                (url-encode (cdr arg))))
                                        query-string)))))
    (concatenate 'string path query-string)))
