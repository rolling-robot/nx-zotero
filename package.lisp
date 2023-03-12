;;;; package.lisp
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
