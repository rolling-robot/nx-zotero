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
                           :content (json:encode-json-to-string (list
                                                                 (cons 'html html)
                                                                 (cons 'uri (quri:render-uri url))
                                                                 (cons 'cookie cookie)
                                                                 (cons 'translator-id translator-id))))
    (json:decode-json-from-string (flexi-streams:octets-to-string body))))

(defun api-ping ()
    (drakma:http-request "http://localhost:23119/connector/ping"
                         :method :get))

(defun api-detect (url html cookie)
  (multiple-value-bind (body status)
      (drakma:http-request "http://localhost:23119/connector/detect"
                           :method :post
                           :content-type "application/json"
                           :content (json:encode-json-to-string (list
                                                                 (cons 'html html)
                                                                 (cons 'uri (quri:render-uri url))
                                                                 (cons 'cookie cookie)))
                           )
    (json:decode-json-from-string (flexi-streams:octets-to-string body))))

(defun detect (buffer)
  (let ((page-source (nyxt/mode/document:get-url-source (nyxt:url buffer))))
    (api-detect (nyxt:url buffer) page-source nil)))

(defun save-buffer (buffer)
  (let ((page-source (nyxt/mode/document:get-url-source (nyxt:url buffer))))
    (api-save-page (nyxt:url buffer) page-source nil nil)))

(define-command save-current ()
  (save-buffer (nyxt:current-buffer)))
