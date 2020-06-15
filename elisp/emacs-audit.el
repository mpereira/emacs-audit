;;; emacs-audit.el --- TODO: description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Murilo Pereira

;; Author: Murilo Pereira <murilo@murilopereira.com>
;; URL: https://github.com/mpereira/emacs-audit.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.2") (json) (elx) (package))
;; Keywords: maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO: commentary.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'emacs-audit)

;;;; Usage

;; Run one of these commands:

;; `emacs-audit-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `emacs-audit' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'dash)
(require 'elx)
(require 'json)
(require 'package)
(require 's)
(require 'tablist)

(defvar package-alist)

(defvar emacs-audit--package-index)

(defun emacs-audit--package-index-default-get-directory ()
  "TODO: docstring."
  (let ((directory (concat (getenv "HOME") "/.cache/emacs_audit")))
    (unless (file-exists-p directory)
      (make-directory directory t))
    directory))

(defun emacs-audit--parse-name-and-email-address (part-alist)
  "TODO: PART-ALIST docstring."
  (let ((part-list (list (car part-alist)
                         (when-let (email-address (cdr part-alist))
                           (format "<%s>" email-address)))))
    (s-join " " (seq-filter 'stringp part-list))))

(defun emacs-audit--handle-optional (value)
  "TODO: VALUE docstring."
  (if (eq value :null) "" value))

(defun emacs-audit--handle-optional-number (n)
  "TODO: N docstring."
  (if (eq n :null) "" (number-to-string n)))

(defmacro emacs-audit--tabulated-list-number-sort-fn (column-index)
  "TODO: COLUMN-INDEX docstring."
  `(lambda (a b)
     (< (string-to-number (elt (cadr a) ,column-index))
        (string-to-number (elt (cadr b) ,column-index)))))

;;;; Options

(defgroup emacs-audit nil
  "Settings for `emacs-audit'."
  :link '(url-link "https://github.com/mpereira/emacs-audit")
  :group 'tools)

(defcustom emacs-audit-special-package-file-names
  '((org-plus-contrib . "org.el")
    (modus-themes . "modus-operandi-theme.el"))
  "TODO: docstring."
  :group 'emacs-audit
  :type 'alist)

(defcustom emacs-audit-package-index-raw-file-name "raw_package_index.json"
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

(defcustom emacs-audit-package-index-enriched-file-name "enriched_package_index.json"
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

(defcustom emacs-audit-package-list-buffer-name "*emacs-audit: package-list*"
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

(defcustom emacs-audit-package-index-get-directory-fn
  'emacs-audit--package-index-default-get-directory
  "TODO: docstring."
  :group 'emacs-audit
  :type 'function)

;;;; Commands

(defun emacs-audit-dump-package-index (&optional path)
  "TODO: PATH docstring."
  (interactive)
  (or path
      (setq path (concat (funcall emacs-audit-package-index-get-directory-fn)
                         "/"
                         emacs-audit-package-index-raw-file-name)))
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file path
      (insert
       (json-encode
        (mapcar
         (lambda (package)
           (let ((emacs-audit (car package))
                 (package-desc (cadr package)))
             (cons
              emacs-audit
              (let* ((extras (package-desc-extras package-desc))
                     (dir (package-desc-dir package-desc))
                     (file-name (or (alist-get
                                     emacs-audit
                                     emacs-audit-special-package-file-names)
                                    (format "%s.el" emacs-audit)))
                     (file-path (format "%s/%s" dir file-name)))
                `((name . ,emacs-audit)
                  (summary . ,(package-desc-summary package-desc))
                  (version . ,(package-desc-version package-desc))
                  (license . ,(elx-license file-path))
                  (kind . ,(package-desc-kind package-desc))
                  (signed . ,(package-desc-signed package-desc))
                  (archive . ,(package-desc-archive package-desc))
                  (dir . ,(package-desc-dir package-desc))
                  (maintainer . ,(emacs-audit--parse-name-and-email-address
                                  (alist-get :maintainer extras)))
                  (keywords . ,(alist-get :keywords extras))
                  (url . ,(alist-get :url extras))
                  (authors . ,(mapcar 'emacs-audit--parse-name-and-email-address
                                      (alist-get :authors extras))))))))
         package-alist)))
      (json-pretty-print-buffer)
      (message "Wrote %s" path))))

(defun emacs-audit-list-packages (&optional path)
  "TODO: PATH docstring."
  (interactive)
  (or path
      (setq path (concat (funcall emacs-audit-package-index-get-directory-fn)
                         "/"
                         emacs-audit-package-index-enriched-file-name)))
  (with-temp-buffer
    (insert-file-contents path)
    (setq emacs-audit--package-index (json-parse-buffer)))
  (with-current-buffer (get-buffer-create emacs-audit-package-list-buffer-name)
    (tablist-mode)
    (setq-local tabulated-list-format
                `[("Name" 20 t)
                  ("Version" 15 t)
                  ("License" 15 t)
                  ("GH license" 40 t)
                  ("Melpa downloads" 15 ,(emacs-audit--tabulated-list-number-sort-fn 4))
                  ("GH Stars" 8 ,(emacs-audit--tabulated-list-number-sort-fn 5))
                  ("GH Forks" 8 ,(emacs-audit--tabulated-list-number-sort-fn 6))
                  ("Summary" 20 t)])
    (setq-local tabulated-list-padding 2)
    (tabulated-list-init-header)
    (setq-local emacs-audit--rows '())
    (maphash (lambda (package-name package-fields)
               (add-to-list 'emacs-audit--rows
                            (vector
                             package-name
                             (if-let ((version (gethash "version" package-fields)))
                                 (mapconcat 'number-to-string version ".")
                               "")
                             (emacs-audit--handle-optional
                              (gethash "license" package-fields))
                             (emacs-audit--handle-optional
                              (gethash "github_license" package-fields))
                             (emacs-audit--handle-optional-number
                              (gethash "melpa_downloads_count" package-fields))
                             (emacs-audit--handle-optional-number
                              (gethash "github_stars_count" package-fields))
                             (emacs-audit--handle-optional-number
                              (gethash "github_forks_count" package-fields))
                             (gethash "summary" package-fields))))
             emacs-audit--package-index)
    (setq-local tabulated-list-entries
                (-zip-with (lambda (i x) (list i x))
                           (-iterate '1+ 0 (length emacs-audit--rows))
                           emacs-audit--rows))
    (tabulated-list-print t)
    (display-buffer (current-buffer))))

;;;; Footer

(provide 'emacs-audit)

;;; emacs-audit.el ends here
