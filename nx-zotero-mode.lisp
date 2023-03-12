;;;; nx-zotero-mode.lisp

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
  (let ((page-source (nyxt/document-mode:get-url-source (nyxt:url buffer))))
    (api-detect (nyxt:url buffer) page-source nil)))

(defun save-buffer (buffer)
  (let ((page-source (nyxt/document-mode:get-url-source (nyxt:url buffer))))
    (api-save-page (nyxt:url buffer) page-source nil nil)))

(define-command save-current ()
  (save-buffer (nyxt:current-buffer)))
