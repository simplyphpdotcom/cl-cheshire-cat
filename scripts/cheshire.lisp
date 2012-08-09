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

;; Load the config parser and the config
(asdf:load-system :py-configparser)
(use-package :py-configparser)

(defparameter *cheshire-config* (make-config)
  "Cheshire configuration holder")

(read-files *cheshire-config* `(,(or (nth 1 sb-ext:*posix-argv*)
                                     "/etc/cheshire.conf")))

(defun get-cheshire-config (option-name &key (section-name "Cheshire") default-value (type nil) (config *cheshire-config*))
  "Try to find the option in this section for this config. Type may be one
of :boolean, :number or :string. If type is not specified (or is nil), the
default type is return from py-configparser. This default is somewhat similar to
a string, but not exactly (e.g. sb-posix will reject it). Two values are
returned: The value of the option (or the default value if the option was not
found) and whether the option was found or not."
  (if (and (has-section-p config section-name)
           (has-option-p config section-name option-name))
      (values
       (let* ((pycp-type (if (eq type :string)
                             nil
                             type))
              (value (get-option config section-name option-name :type pycp-type :expand nil)))
         (if (eq type :string)
             (concatenate 'string value)
             value))
       t)
      default-value))

;; Load and start Cheshire
(asdf:load-system :cl-cheshire-cat)
(use-package :cheshire)

(defparameter *cheshire*
  (make-instance 'redirection-acceptor
                 :port           (get-cheshire-config "port"          :default-value 80   :type :number)
                 :address        (get-cheshire-config "address"       :default-value "0.0.0.0")
                 :admin-allowed (cheshire::parse-cidr-list
                                 (get-cheshire-config "admin_allowed" :default-value "127.0.0.1"))
                 :admin-host     (get-cheshire-config "admin_host"    :default-value "management.invalid"))
  "Cheshire cat acceptor")

;; debugging bookeeping
(setq hunchentoot:*show-lisp-errors-p* t)

(defparameter *cheshire-debugp* (get-cheshire-config "debug" :type :boolean))
(if *cheshire-debugp*
    (setq hunchentoot:*show-lisp-backtraces-p* t
          hunchentoot:*catch-errors-p* nil)
    (setq hunchentoot:*show-lisp-backtraces-p* nil
          hunchentoot:*catch-errors-p* t))

(hunchentoot:start *cheshire*)

;; Daemonize Cheshire
#+sbcl (asdf:load-system :sb-daemon)
(when (get-cheshire-config "daemonize" :section-name "daemon" :type :boolean)
  #+sbcl
  (sb-daemon:daemonize :exit-parent t
                       :pidfile (get-cheshire-config "pid_file"  :section-name "daemon" :default-value "/var/run/cheshire.pid")
                       :output  (get-cheshire-config "log"       :section-name "daemon")
                       :error   (get-cheshire-config "error_log" :section-name "daemon")
                       :user    (get-cheshire-config "user"      :section-name "daemon" :type :string)
                       :group   (get-cheshire-config "group"     :section-name "daemon" :type :string)
                       :disable-debugger (not *cheshire-debugp*))
  #-sbcl
  (error "Daemonize facility is supported only using SBCL and sb-daemon. Any compatibility improvment patch is welcome."))

;; Start swank server if requested
(when (get-cheshire-config "enable" :section-name "swank" :type :boolean)
  (asdf:load-system :swank)
  (defparameter *swank-server*
    (funcall (alexandria:ensure-symbol :create-server :swank)
             :port (get-cheshire-config "port" :section-name "swank" :type :number)
             :coding-system "utf-8-unix"
             :dont-clost t)))

;; Load redirection rules
(let ((rules-file (get-cheshire-config "rules_file")))
  (when rules-file
    (load-rules *cheshire* rules-file)))

;; Sleeping loop
(loop (sleep 10))
