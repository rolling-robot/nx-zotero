;;;; nx-zotero-mode.lisp
;; Copyright 2023 Petr Mukhachev
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;    http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


(in-package #:nx-zotero-mode)

(define-mode zotero-mode ()
  "A mode for extracting metadata from a web page and sending it to zotero."
  ((zotero-host :type string
                :initform "localhost:23119"
                :documentation "Host and port number to connect to Zotero instance.")
   (ping-timeout :type float
                 :initform 0.1
                 :documentation "Ping timeout in seconds")
   (keyscheme-map
    (define-keyscheme-map "zotero-mode" ()
      nyxt/keyscheme:emacs
      (list "C-c z" 'zotero-save-current)))
   ))

(defun api-save-page (host url html cookie translator-id)
  "Send a save page request to zotero connector api. Returns the result decoded from json to lisp structures."
  (multiple-value-bind (body status)
      (drakma:http-request (str:concat "http://" host "/connector/savePage")
                           :method :post
                           :content-type "application/json"
                           :content (njson:encode (list
                                                   (cons 'html html)
                                                   (cons 'uri (quri:render-uri url))
                                                   (cons 'cookie cookie)
                                                   (cons 'translator-+id+ translator-id))))
    (values (njson:decode (flexi-streams:octets-to-string body)) status)))

(defun api-select-items (host selected-items instance-id)
  "Send a list of selected items to zotero connector api in order to save them. Returns status of the request only."
  (let ((content (njson:encode (list
                                (cons 'selected-items (loop for item in selected-items
                                                            collect (cons (url item) (name item))))
                                (cons 'instance-+ID+ instance-id)))))
    (echo (format nil "Sending ~a" content))
    (multiple-value-bind (body status)
        (drakma:http-request (str:concat "http://" host "/connector/selectItems")
                             :method :post
                             :content-type "application/json"
                             :content content)
      (declare (ignore body))
      status)))

(defun api-ping (host timeout)
  "Ping zotero api. Returns status returned by api or throws a condition usocket:timeout-error"
  (multiple-value-bind (ans status)
      (drakma:http-request (str:concat "http://" host "/connector/ping")
                           :method :get
                           :connection-timeout timeout)
    (declare (ignore ans))
    status))

(defun current-zotero-mode ()
  "Return `zotero-mode' if it's active in the current buffer."
  (alex:when-let ((mode (sym:resolve-symbol :zotero-mode :mode '(:nx-zotero-mode))))
    (find-submode mode)))

(defun ping-p ()
  (with-slots (zotero-host ping-timeout) (current-zotero-mode)
    (if (handler-case (api-ping zotero-host ping-timeout)
          (usocket:timeout-error ()
                                 nil))
        t
      nil)))

(defun api-detect (host url html cookie)
  "\"Detect\" endpoint detects whether there is an available translator
to handle a given page. Returns a list of available translators as a list."
  (multiple-value-bind (body status)
      (drakma:http-request (str:concat "http://" host "/connector/detect")
                           :method :post
                           :content-type "application/json"
                           :content (njson:encode (list
                                                   (cons 'html html)
                                                   (cons 'uri (quri:render-uri url))
                                                   (cons 'cookie cookie)))
                           )
    (declare (ignore status))
    (njson:decode (flexi-streams:octets-to-string body))))

(defun detect (buffer)
  "Returns a list of available translators for a page in a given buffer. Not yet used."
  (let ((page-source (nyxt/mode/document:get-url-source (nyxt:url buffer))))
    (api-detect (zotero-host (current-zotero-mode))
                (nyxt:url buffer) page-source nil)))

(defun zotero-send-buffer (buffer)
  "Sends buffer source to zotero api for saving and returns results"
  (let ((page-source (nyxt/mode/document:get-url-source (nyxt:url buffer))))
    (api-save-page (zotero-host (current-zotero-mode))
                   (nyxt:url buffer) page-source nil nil)))

(define-class zotero-option ()
  ((name :initarg :name :accessor name)
   (url :initarg :url :accessor url)))

(defmethod prompter:object-attributes ((option zotero-option) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(name option))))

(define-class zotero-selection-source (prompter:source)
  ((prompter:name "Detected items")))

(defun select-one (options)
  (prompt :prompt "Select which item to save"
          :sources (make-instance 'zotero-selection-source
                                  :name (format nil "Detected items at ~a" (njson:jget "uri" options))
                                  :constructor (let ((items (njson:jget "selectItems" options)))
                                                 (loop for url in (njson:jkeys items)
                                                       collect (make-instance 'zotero-option :name (njson:jget url items) :url url))))))

(define-command zotero-save-current ()
  "Sends current buffer to zotero api and handles the result: in case
there are multiple papers to save, provides an interface to select one of them"
  (if (ping-p)
      (multiple-value-bind (ans status) (zotero-send-buffer (nyxt:current-buffer))
        (let ((result
               (case status
                 (300 (let ((selected (select-one ans)))  ;; Several items available. Select one.
                        (api-select-items (zotero-host (current-zotero-mode))
                                          selected (njson:jget "instanceID" ans))))
                 (201 201)
                 ((t) status))))
          (case result
            (201 (echo (format nil "Success ~a" result)))
            (500 (echo (format nil "Error ~a" result)))
            )
          result))
    (progn
      (echo "Ping timeout. Check that Zotero is running.")
      nil)
    ))
