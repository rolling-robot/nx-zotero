;;;; nx-zotero-mode.asd

(asdf:defsystem #:nx-zotero-mode
  :description "A mode for extracting metadata from a web page and sending it to zotero."
  :author "Petr Mukhachev"
  :license  "GPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "nx-zotero-mode")))
