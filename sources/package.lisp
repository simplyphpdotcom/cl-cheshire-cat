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

(cl:in-package :cl-user)

(defpackage cl-cheshire-cat
  (:nicknames cheshire cheshire-cat cl-cc)
  (:documentation "Cheshire Cat (HTTP Redirection Server) main package")
  (:use #:cl #:split-sequence)
  (:import-from #:alexandria
                #:emptyp #:deletef
                #:starts-with-subseq #:ends-with-subseq
                #:curry #:compose
                #:symbolicate #:ensure-symbol #:make-keyword
                #:once-only #:when-let #:if-let
                )
  (:import-from #:cl-store
                #:defstore-cl-store #:defrestore-cl-store
                #:store-object #:restore-object
                #:register-code #:output-type-code
                #:*check-for-circs*
                #:store #:restore
                )
  (:import-from #:cl-ppcre #:create-scanner #:regex-replace #:scan)
  (:import-from #:cl-fad #:file-exists-p)
  (:import-from #:usocket #:host-byte-order)
  (:import-from #:hunchentoot
                #:acceptor
                #:acceptor-dispatch-request #:acceptor-error-template-directory

                #:start #:start-listening #:acceptor-taskmaster
                #:taskmaster-acceptor #:execute-acceptor #:acceptor-shutdown-p

                #:*request* #:host #:script-name* #:remote-addr*
                #:post-parameter #:get-parameter #:get-parameters* #:post-parameters*

                #:content-type* #:return-code* #:*reply*

                #:url-encode #:redirect #:abort-request-handler

                #:+http-bad-request+ #:+http-not-found+
                #:+http-forbidden+ #:+http-moved-permanently+
                )
  (:export #:redirection-acceptor #:load-rules #:redirection-acceptor-rule-directory))
