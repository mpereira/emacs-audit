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

(declare-function org-table-convert-region
                  "ext:org-table.el"
                  (beg0 end0 &optional separator))

(declare-function org-html-export-to-html
                  "ext:ox-html.el"
                  (&optional async subtreep visible-only body-only ext-plist))

(defvar package-alist nil
  "TODO: docstring.")

(defvar package-selected-packages nil
  "TODO: docstring.")

;;;; Variables

(defvar emacs-audit-version 'undefined
  "TODO: docstring.")

(defvar emacs-audit--program-name (file-name-sans-extension
                                   (file-name-base buffer-file-name))
  "TODO: docstring.")

(defvar emacs-audit--package-index nil
  "TODO: docstring.")

(defvar emacs-audit--repository-directory (directory-file-name
                                           (file-name-directory
                                            (directory-file-name
                                             (file-name-directory
                                              (buffer-file-name)))))
  "TODO: docstring.")

(defvar emacs-audit--enrich-package-index-process nil
  "TODO: docstring.")

;;;; Options

(defun emacs-audit--package-index-default-get-directory ()
  "TODO: docstring."
  (let ((directory (concat
                    (getenv "HOME") "/.cache/" emacs-audit--program-name)))
    (unless (file-exists-p directory)
      (make-directory directory t))
    directory))

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

(defcustom emacs-audit-github-token nil
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

(defcustom emacs-audit-executable "cargo"
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

(defcustom emacs-audit-args
  `("run" "--manifest-path" ,(format "%s/Cargo.toml" emacs-audit--repository-directory)
    "--"
    "enrich-package-index")
  "TODO: docstring."
  :group 'emacs-audit
  :type 'list)

;;;; Helper functions and macros

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

(defun emacs-audit--emacs-audit-package-index-raw-file-path ()
  "TODO: docstring."
  (concat (funcall emacs-audit-package-index-get-directory-fn)
          "/"
          emacs-audit-package-index-raw-file-name))

(defun emacs-audit--emacs-audit-package-index-enriched-file-path ()
  "TODO: docstring."
  (concat (funcall emacs-audit-package-index-get-directory-fn)
          "/"
          emacs-audit-package-index-enriched-file-name))

(defun emacs-audit--enrich-package-index-command ()
  "TODO: docstring."
  (append (cons emacs-audit-executable emacs-audit-args)
          (list (emacs-audit--emacs-audit-package-index-raw-file-path))
          (apply 'list (when emacs-audit-github-token
                         (list "--token" emacs-audit-github-token)))))

(defun emacs-audit--enrich-package-index-process-sentinel
    (process event &optional list-packages)
  "TODO: PROCESS EVENT LIST-PACKAGES docstring."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message (concat (process-name process) " finished")))
    (when (eq process emacs-audit--enrich-package-index-process)
      (setq emacs-audit--enrich-package-index-process nil))
    (let ((buffer (process-get process 'enriched-package-index-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-min))
          (setq emacs-audit--package-index (json-parse-buffer)))
        (when list-packages
          (emacs-audit-list-packages nil))
        (kill-buffer buffer)))))

(defun emacs-audit--package-index-filter-selected (package-index)
  "TODO: PACKAGE-INDEX."
  (let ((package-index-ht (make-hash-table))
        (package-selected-packages-ht (make-hash-table)))
    (maphash (lambda (package-name package-fields)
               (let ((package-name-sym (intern package-name)))
                 (when (-contains? package-selected-packages package-name-sym)
                   (puthash package-name-sym package-fields package-index-ht))))
             package-index)
    (-each package-selected-packages
      (lambda (package-name)
        (when-let ((package-fields (gethash package-name package-index-ht)))
          (puthash (symbol-name package-name)
                   package-fields
                   package-selected-packages-ht))))
    package-selected-packages-ht))

;;;; Commands

(defun emacs-audit-dump-package-index (&optional path)
  "TODO: PATH docstring."
  (interactive)
  (or path (setq path (emacs-audit--emacs-audit-package-index-raw-file-path)))
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

(defun emacs-audit-list-packages-clear ()
  "TODO: docstring."
  (interactive)
  (setq emacs-audit--package-index nil))

(defun emacs-audit-list-packages-refresh ()
  "TODO: docstring."
  (interactive)
  (let ((process-buffer (generate-new-buffer "*temp*"))
        (list-packages (not (called-interactively-p 'any))))
    (message "Fetching package data asynchronously%s"
             (if list-packages
                 ", package list will open soon"
               ""))
    (with-current-buffer process-buffer
      (let* ((name "emacs-audit-enrich-package-index")
             (process (make-process
                       :name name
                       :buffer process-buffer
                       :command (emacs-audit--enrich-package-index-command)
                       :stderr (format "*%s stderr*" name)
                       :sentinel
                       #'(lambda (process event)
                           (emacs-audit--enrich-package-index-process-sentinel
                            process event (when list-packages
                                            'list-packages))))))
        (process-put process 'enriched-package-index-buffer process-buffer)
        (setq emacs-audit--enrich-package-index-process process)))))

(defun emacs-audit-list-packages (arg)
  "TODO: ARG docstring."
  (interactive "P")
  (if (not (and emacs-audit--package-index
                (hash-table-p emacs-audit--package-index)))
      (progn
        (emacs-audit-list-packages-refresh))
    (with-current-buffer (get-buffer-create emacs-audit-package-list-buffer-name)
      (tablist-mode)
      (setq-local tabulated-list-format
                  `[("Name" 20 t)
                    ("Version" 15 t)
                    ("License" 15 t)
                    ("GH license" 40 t)
                    ("Melpa downloads" 17 ,(emacs-audit--tabulated-list-number-sort-fn 4))
                    ("GH Stars" 10 ,(emacs-audit--tabulated-list-number-sort-fn 5))
                    ("GH Forks" 10 ,(emacs-audit--tabulated-list-number-sort-fn 6))
                    ("GH Issues" 11 ,(emacs-audit--tabulated-list-number-sort-fn 7))
                    ("GH PRs" 8 ,(emacs-audit--tabulated-list-number-sort-fn 8))
                    ("GH Releases" 13 ,(emacs-audit--tabulated-list-number-sort-fn 9))
                    ("GH Vulns" 10 ,(emacs-audit--tabulated-list-number-sort-fn 10))
                    ("Created at" 12 t)
                    ("Last commit" 12 t)
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
                               (emacs-audit--handle-optional-number
                                (gethash "github_issues_count" package-fields))
                               (emacs-audit--handle-optional-number
                                (gethash "github_pull_requests_count" package-fields))
                               (emacs-audit--handle-optional-number
                                (gethash "github_releases_count" package-fields))
                               (emacs-audit--handle-optional-number
                                (gethash "github_vulnerability_alerts_count" package-fields))
                               (emacs-audit--handle-optional
                                (gethash "github_created_at" package-fields))
                               (emacs-audit--handle-optional
                                (gethash "github_pushed_at" package-fields))
                               (gethash "summary" package-fields))))
               ;; Show only packages in `package-install-selected-packages' if
               ;; called with prefix argument, all packages otherwise.
               (if (and arg (equal '(4) arg))
                   (emacs-audit--package-index-filter-selected emacs-audit--package-index)
                 emacs-audit--package-index))
      (setq-local tabulated-list-entries
                  (-zip-with (lambda (i x) (list i x))
                             (-iterate '1+ 0 (length emacs-audit--rows))
                             emacs-audit--rows))
      (tabulated-list-print t)
      (display-buffer (current-buffer)))))

(defun emacs-audit-list-packages-export-html ()
  "TODO: docstring."
  (interactive)
  (with-current-buffer (get-buffer-create emacs-audit-package-list-buffer-name)
    (let ((buffer (generate-new-buffer "*temp*")))
      (tablist-export-csv nil nil nil buffer)
      (with-current-buffer buffer
        (org-table-convert-region (point-min) (point-max) ";")
        (browse-url (org-html-export-to-html))))))

;;;; Footer

(provide 'emacs-audit)

;;; emacs-audit.el ends here
