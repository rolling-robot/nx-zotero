;;;; package.lisp

(nyxt:define-package #:nx-zotero-mode
  (:use #:cl)
  (:import-from #:nyxt
                #:current-buffer
                #:define-class
                #:define-mode
                #:define-command
                #:search-engine
                )
  (:export #:save-current)
  (:documentation "A mode for extracting metadata from a web page and sending it to zotero."))
