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

(defun add-qs-update (rule update &key position)
  "This function add a new query string update operation to a redirection
  rule. <pre>position</pre> is the position at which the rule will be inserted
  in <pre>rules</pre>. If <pre>position</pre> is nil or unspecified,
  <pre>rule</pre> will be inserted at the begining of <pre>rules</pre>."
  (if position
      (push update (cdr (nthcdr position (rr-qs-updates rule))))
      (push update (rr-qs-updates rule))))

(defun remove-qs-update (rule operation name match)
  "This function removes the query string update designated by operation name
and match."
  (let ((key (remove nil (list operation name match))))
    (deletef (rr-qs-updates rule) key
             :key #'qsu-key :test #'equal :count 1)))

(defun find-qs-update (rule operation name match &key error-p)
  "This function returns the query string update designated by operation name
and match."
  (let* ((key   (remove nil (list operation name match)))
         (found (member key (rr-qs-updates rule)
                        :key #'qsu-key :test #'equal)))
    (if found
        (car found)
        (and error-p
             (error 'rs-no-such-qs-update :operation operation :name name :match match)))))

(defmacro add-domain-name-rule (rules rule &key position)
  "This macro adds <pre>rule</pre> (applying to domaine names) in
<pre>rules</pre>. <pre>position</pre> is the position at which the rule will be
inserted in <pre>rules</pre>. If <pre>position</pre> is nil or unspecified,
<pre>rule</pre> will be inserted at the begining of <pre>rules</pre>."
  (once-only (position)
    `(if ,position
         (push ,rule (cdr (nthcdr ,position ,rules)))
         (push ,rule ,rules))))

(defmacro remove-domain-name-rule (rules kind match)
  "This macro removes the rule from <pre>rule</pre> with matching
<pre>kind</pre> and <pre>match</pre>. <pre>rule</pre> must match exactly. If no
such rule is found, returns silently."
  `(deletef ,rules (list ,kind ,match)
            :key #'rr-key :test #'equal :count 1))

(defun find-domain-name-rule (rules kind match &key error-p)
  "This function returns the domain name rule with matching <pre>kind</pre> and
<pre>match</pre> in <pre>rules</pre> or nil if none was found. if
<pre>error-p</pre> is true, an error of type <pre>rs-no-such-domain-rule</pre>
is raised instead."
  (let ((found (member (list kind match) rules
                       :key #'rr-key :test #'equal)))
    (if found
        (car found)
        (and error-p
             (error 'rs-no-such-domain-rule :kind kind :match match)))))


(defun add-uri-rule (domain-redirection-rule uri-rule &key position)
  "This function adds <pre>rule</pre> (applying to URIs) to
<pre>domain-redirection-rule</pre>. <pre>position</pre> is the position at which
the rule will be inserted in <pre>rules</pre>. If <pre>position</pre> is nil or
unspecified, <pre>rule</pre> will be inserted at the begining of
<pre>rules</pre>."
  (if position
      (push uri-rule (cdr (nthcdr position (drr-uri-rules domain-redirection-rule))))
      (push uri-rule (drr-uri-rules domain-redirection-rule))))

(defun remove-uri-rule (domain-redirection-rule kind match)
  "This macro removes the rule from <pre>rule</pre> with matching
<pre>kind</pre> and <pre>match</pre>. <pre>rule</pre> must match exactly. If no
such rule is found, returns silently."
  (deletef (drr-uri-rules domain-redirection-rule) (list kind match)
           :key #'rr-key :test #'equal :count 1))

(defun find-uri-rule (domain-redirection-rule kind match &key error-p)
  "This function returns the URI rule with matching <pre>kind</pre> and
<pre>match</pre> in <pre>domain-redirection-rule</pre> or nil if none was
found. if <pre>error-p</pre> is true, an error of type
<pre>rs-no-such-uri-rule</pre> is raised instead."
  (let ((rule (member (list kind match) (drr-uri-rules domain-redirection-rule)
                      :key #'rr-key :test #'equal)))
    (if rule
        (car rule)
        (and error-p
             (error 'rs-no-such-uri-rule :kind kind :match match)))))

(defparameter +default-domain-name-rule+ (make-instance 'domain-redirection-rule
                                                        :kind :regex
                                                        :match '(:sequence :modeless-start-anchor (:negative-lookahead "www."))
                                                        :replacement "www."
                                                        :http-code +http-moved-permanently+)
  "Domain name rule with default behaviour. This should be treated as a
  constant, If the object, value or binding is modified behaviour is
  undefined.")

(defun apply-default-domain-name-rule (domain-name)
  "This function applies the default domain name redirection: if the domain-name
  does not start with a \"www.\",returns a permanent redirect to the same URL
  prepended with \"www.\", raise a rs-loop-detected condition otherwise."
  (multiple-value-bind (match-p new-domain)
      (rr-matching-p +default-domain-name-rule+ domain-name)
    (if match-p
        (values new-domain
                (rr-http-code +default-domain-name-rule+)
                (rr-protocol  +default-domain-name-rule+)
                (rr-port      +default-domain-name-rule+))
        (error 'rs-loop-detected))))

(defun apply-rules (target-string rules)
  "This function looks through the list of rules to find a matching rule for
<pre>target-string</pre>. The three returns values are:

 * Whether a rule was find or not,
 * The rewritten target string,
 * The matching rule.

If no such rule is found, all three values are nil."
  (dolist (rule rules ())
    (multiple-value-bind (match-p string)
        (rr-matching-p rule target-string)
      (when match-p
        (return (values match-p string rule))))))

(defun merge-redirection-parameters-list (rules parameter-readers)
  "This function merges a list of parameters from a list of rules.
For each element of parameter-readers, a parameter is extracted from each rule
and the last one non-null is returned."
  (mapcar (lambda (parameter-reader)
            (apply #'merge-redirection-parameter (mapcar parameter-reader rules)))
          parameter-readers))

(defun merge-redirection-parameter (&rest lists)
  "This function merges a set parameters (return the last non-null one)."
  (some #'identity (reverse lists)))

(defun compute-redirection (rules domain-name uri)
  "This function computes the actual value of the redirection for this
domain-name and URI, following this set of rules. It return the
list (new-domain-name new-uri . redirection-parameters)"
  (multiple-value-bind (domain-match-p new-domain domain-redirection-rule)
      (apply-rules domain-name rules)
    (if domain-match-p
        (if (and (null (rr-match domain-redirection-rule))
                 (null (drr-uri-rules domain-redirection-rule)))
            (error 'rs-loop-detected)
            (multiple-value-bind (uri-match-p new-uri uri-rule)
                (apply-rules uri (drr-uri-rules domain-redirection-rule))
              (if uri-match-p
                  (list new-domain
                        new-uri
                        (merge-redirection-parameter (rr-http-code domain-redirection-rule)
                                                     (rr-http-code uri-rule))
                        (merge-redirection-parameter (rr-protocol domain-redirection-rule)
                                                     (rr-protocol uri-rule))
                        (merge-redirection-parameter (rr-port domain-redirection-rule)
                                                     (rr-port uri-rule))
                        (append (rr-qs-updates domain-redirection-rule)
                                (rr-qs-updates uri-rule)))
                  (list new-domain
                        uri
                        (rr-http-code domain-redirection-rule)
                        (rr-protocol  domain-redirection-rule)
                        (rr-port      domain-redirection-rule)
                        (rr-qs-updates domain-redirection-rule)))))
        (multiple-value-bind (new-domain http-code protocol port)
            (apply-default-domain-name-rule domain-name)
          (list new-domain uri http-code protocol port)))))
