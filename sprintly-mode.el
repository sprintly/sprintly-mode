;;; sprintly-mode.el --- Major mode for dealing with sprint.ly

;; Copyright (c) 2012, Justin Lilly
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the
;; distribution.  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS
;; AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
;; OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;; DAMAGE.

;; Author: Justin Lilly <justin@justinlilly.com>
;; URL: https://github.com/justinlilly/sprintly-mode
;; Version: 0.0.1
;; Package-Requires: ((furl "0.0.2"))

;;; Commentary:

;;; Code:
(require 'furl)
(require 'json)

(defvar sprintly-mode-hook nil)
(defvar sprintly-email nil)
(defvar sprintly-api-key nil)
(defvar sprintly-product-id nil)
(defvar sprintly-user-id nil)
(defvar sprintly-buffer-name "*sprintly*")

;;; @@@ What is the difference between make-sparse-keymap and
;;; make-keymap?
(defvar sprintly-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [\r] 'sprintly-open-item)
    (define-key map (kbd "C-c g")  'sprintly-list-items)
    map)
  "Keymap for `sprintly-mode'.")

(defvar sprintly-font-lock-keywords
  '(("#\\d+" (1 font-lock-keyword-face)) ;;; tickets
    (":\\sd+:" (1 font-lock-keyword-face))) ;;; status / types
  "Keyword highlighting specification for `sprintly-mode'.")

;;;###autoload
(define-derived-mode sprintly-mode fundamental-mode "Sprint.ly"
  "A major mode for interacting with sprint.ly."
  (set (make-local-variable 'font-lock-defaults)
	'(sprintly-font-lock-keywords))
  (kill-all-local-variables)
  (setq major-mode 'sprintly-mode)
  (setq mode-name "sprintly")
  
  ;; (set-syntax-table sprintly-mode-syntax-table)
  (use-local-map sprintly-mode-map)
  ;; create a sprintly buffer
  (get-buffer-create sprintly-buffer-name)
  (run-hooks 'sprintly-mode-hook))

(defun sprintly-open-item () 
  nil)

(defun render-item (item)
  (let* ((type (cdr (assoc 'type item)))
	 (who (cdr (assoc 'who item)))
	 (what (cdr (assoc 'what item)))
	 (why (cdr (assoc 'why item)))
	 (title (cdr (assoc 'title item)))
	 ;; (desc (if (equal type "story")
	 ;; 	   (format "As a %s I want to %s so that %s" who what why)
	 ;; 	 (replace-regexp-in-string "" " " (cdr (assoc 'description item)))))
	 (number (cdr (assoc 'number item)))
	 (status (cdr (assoc 'status item)))
	 )
    (insert (format "#%-7s %-7s %-11s %s\n" number type status title))))

(defun sprintly-show-item-list (response-string)
  (with-current-buffer (get-buffer-create sprintly-buffer-name)
    
    (erase-buffer)
    (let ((result (json-read-from-string response-string)))
      (mapcar 'render-item result))))

(defun sprintly-list-items ()
  (interactive)
  (furl-with-header "Authorization" (concat "Basic " (base64-encode-string (concat sprintly-email ":" sprintly-api-key)))
      (furl-retrieve (format "https://sprint.ly/api/products/%s/items.json?assigned_to=%s"
			     sprintly-product-id
			     sprintly-user-id
			     )
		     'sprintly-show-item-list)
    ))

(defun sprintly ()
  (interactive)
  (with-current-buffer (get-buffer-create sprintly-buffer-name)
    (sprintly-mode)
    (sprintly-list-items)
    (switch-to-buffer (current-buffer))))

(provide 'sprintly-mode)
;;; sprintly.el ends here
