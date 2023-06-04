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
  ((zotero-port :type integer
                :initform 23119
                :documentation "Port number to connect to Zotero instance.")
   (keyscheme-map
    (define-keyscheme-map "zotero-mode" ()
      nyxt/keyscheme:emacs
      (list "C-c z" 'save-current)))
   ))

(defun api-save-page (url html cookie translator-id)
  (multiple-value-bind (body status)
      (drakma:http-request "http://localhost:23119/connector/savePage"
                           :method :post
                           :content-type "application/json"
                           :content (njson:encode (list
                                                   (cons 'html html)
                                                   (cons 'uri (quri:render-uri url))
                                                   (cons 'cookie cookie)
                                                   (cons 'translator-+id+ translator-id))))
;    (echo (format nil "Received: ~a" (flexi-streams:octets-to-string body)))
    (values (njson:decode (flexi-streams:octets-to-string body)) status)))

(defun api-select-items (selected-items instance-id)
  (let ((content (njson:encode (list
                                (cons 'selected-items (loop for item in selected-items
                                                            collect (cons (url item) (name item))))
                                (cons 'instance-+ID+ instance-id)))))
;    (echo (format nil "Sending ~a" content))
    (multiple-value-bind (body status)
        (drakma:http-request "http://localhost:23119/connector/selectItems"
                             :method :post
                             :content-type "application/json"
                             :content content)
      status)))

(defun api-ping ()
    (drakma:http-request "http://localhost:23119/connector/ping"
                         :method :get))

(defun api-detect (url html cookie)
  (multiple-value-bind (body status)
      (drakma:http-request "http://localhost:23119/connector/detect"
                           :method :post
                           :content-type "application/json"
                           :content (njson:encode (list
                                                   (cons 'html html)
                                                   (cons 'uri (quri:render-uri url))
                                                   (cons 'cookie cookie)))
                           )
    (njson:decode (flexi-streams:octets-to-string body))))

(defun detect (buffer)
  (let ((page-source (nyxt/mode/document:get-url-source (nyxt:url buffer))))
    (api-detect (nyxt:url buffer) page-source nil)))

(defun select-items (items)
  (format t "~a" items))

(defun zotero-send-buffer (buffer)
  (let ((page-source (nyxt/mode/document:get-url-source (nyxt:url buffer))))
    (api-save-page (nyxt:url buffer) page-source nil nil)))

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

(define-command save-current ()
  (multiple-value-bind (ans status) (zotero-send-buffer (nyxt:current-buffer))
    (let ((result
            (case status
              (300 (let ((selected (select-one ans)))
;                     (echo (format nil "Selected ~a" ans))
                     (api-select-items selected (njson:jget "instanceID" ans))))
              (201 201)
              ((t) status))))
      (case result
        (201 (echo (format nil "Success ~a" result)))
        (500 (echo (format nil "Error ~a" result)))
      )
      result)))
