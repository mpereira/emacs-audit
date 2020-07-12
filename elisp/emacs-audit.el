;;; emacs-audit.el --- TODO: description -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Murilo Pereira

;; Author: Murilo Pereira <murilo@murilopereira.com>
;; Maintainer: Murilo Pereira <murilo@murilopereira.com>
;; URL: https://github.com/mpereira/emacs-audit
;; Package-Requires: ((emacs "25.2") (dash) (elx) (json) (package) (s) (tablist))
;; Keywords: maint
;; SPDX-License-Identifier: GPL-3+

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

(defun plist-each (function plist)
  "Iterate FUNCTION (a two-argument function) over PLIST."
  (when plist
    (funcall function (car plist) (cadr plist))
    (plist-each function (cddr plist))))

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

(defvar emacs-audit-version "1.0.0-snapshot"
  "TODO: docstring.")

(defvar emacs-audit--repository-url "https://github.com/mpereira/emacs-audit"
  "TODO: docstring.")

(defvar emacs-audit--program-name (file-name-sans-extension
                                   (file-name-base
                                    (or load-file-name buffer-file-name)))
  "TODO: docstring.")

(defvar emacs-audit--package-index nil
  "TODO: docstring.")

(defvar emacs-audit--extracted-archive-directory nil
  "TODO: docstring.")

(defvar emacs-audit--enrich-package-index-process nil
  
  "TODO: docstring.")

(defvar emacs-audit--supported-platforms '("x86_64-apple-darwin"
                                           "x86_64-unknown-linux-gnu")
  "TODO: docstring.")

(defvar emacs-audit--directories '(:cache ".cache"
                                   :configuration ".config"
                                   :user-data ".local/share")
  "TODO: docstring.")

(defun emacs-audit--get-repository-directory ()
  "TODO: docstring."
  (when buffer-file-name
    (-> buffer-file-name
        (file-name-directory)
        (directory-file-name)
        (file-name-directory)
        (directory-file-name))))

(defvar emacs-audit--development-command
  (when-let ((repository-directory (emacs-audit--get-repository-directory)))
    `("cargo"
      "run"
      "--manifest-path" ,(format "%s/Cargo.toml" repository-directory)
      "--"))
  "TODO: docstring.")

(defvar emacs-audit--development-mode nil
  "TODO: docstring.")

;;;; Options

(defgroup emacs-audit nil
  "Settings for `emacs-audit'."
  :link `(url-link ,emacs-audit--repository-url)
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

(defcustom emacs-audit-package-list-buffer-name "*emacs-audit: package-list*"
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

(defcustom emacs-audit-github-token nil
  "TODO: docstring."
  :group 'emacs-audit
  :type 'string)

;;;; Helper functions and macros

(defun emacs-audit--get-platform ()
  "TODO: docstring."
  (let ((matches (s-match-strings-all
                  (s-join "\\|" emacs-audit--supported-platforms)
                  system-configuration)))
    (when matches
      (if (= 1 (length matches))
          (caar matches)
        (error "Unexpected multiple platform match for %s"
               system-configuration)))))

(defmacro emacs-audit--with-platform (platform &rest body)
  "TODO: PLATFORM BODY docstring."
  `(if-let ((,platform (emacs-audit--get-platform)))
       (progn
         ,@body)
     (progn
       (message (concat "%s is not a supported platform. "
                        "Check `emacs-audit--supported-platforms'")
                system-configuration)
       nil)))

(defun emacs-audit--directory (directory-type)
  "TODO: DIRECTORY-TYPE docstring."
  (let* ((directory (plist-get emacs-audit--directories directory-type))
         (_ (or directory (error "No directory type %s in %s"
                                 directory-type
                                 emacs-audit--directories)))
         (directory
          (concat
           (getenv "HOME") "/" directory "/" emacs-audit--program-name)))
    (unless (file-exists-p directory)
      (make-directory directory t))
    directory))

(defun emacs-audit--archive-file-name (platform)
  "TODO: PLATFORM docstring."
  (format "emacs-audit-%s-%s.zip" emacs-audit-version platform))

(defun emacs-audit--archive-path (platform)
  "TODO: PLATFORM docstring."
  (concat (emacs-audit--directory :user-data)
          "/"
          (emacs-audit--archive-file-name platform)))

(defun emacs-audit--archive-url (platform)
  "TODO: VERSION PLATFORM docstring."
  (format "%s/releases/download/%s/%s"
          emacs-audit--repository-url
          emacs-audit-version
          (emacs-audit--archive-file-name platform)))

(defun emacs-audit--archive-download ()
  "TODO: docstring."
  (interactive)
  (emacs-audit--with-platform
   platform
   (url-copy-file (emacs-audit--archive-url platform)
                  (emacs-audit--archive-path platform))))

(defun emacs-audit--archive-extract ()
  "TODO: docstring."
  (interactive)
  (emacs-audit--with-platform
   platform
   (let ((archive (emacs-audit--archive-path platform)))
     (if-let ((uncompressed-directory (dired-compress-file archive)))
         (setq emacs-audit--extracted-archive-directory uncompressed-directory)
       (message "Failed to extract %s" archive)))))

(defun emacs-audit--executable-path (platform)
  "TODO: PLATFORM docstring."
  (concat (emacs-audit--directory :user-data)
          "/"
          (file-name-sans-extension (emacs-audit--archive-file-name platform))
          "/"
          emacs-audit--program-name))

(defmacro emacs-audit--with-executable (executable &rest body)
  "TODO: EXECUTABLE BODY docstring."
  `(emacs-audit--with-platform
    platform
    (let ((,executable (emacs-audit--executable-path platform)))
      (if (file-exists-p ,executable)
          (progn
            ,@body)
        (progn
          (message (concat "%s executable is not available. "
                           "Run `emacs-audit-setup'")
                   emacs-audit--program-name)
          nil)))))

(defun emacs-audit--development-mode-toggle ()
  "TODO: docstring."
  (interactive)
  (setq emacs-audit--development-mode (if emacs-audit--development-mode
                                          nil
                                        t)))

(defun emacs-audit--command ()
  "TODO: docstring."
  (if emacs-audit--development-mode
      (or emacs-audit--development-command
          (message (concat "In development mode but "
                           "`emacs-audit--development-command' "
                           "isn't available. Development mode only works when "
                           "inside an emacs-audit git repository")))
    (emacs-audit--with-executable executable executable)))

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

(defun emacs-audit--package-index-raw-file-path ()
  "TODO: docstring."
  (concat (emacs-audit--directory :cache)
          "/"
          emacs-audit-package-index-raw-file-name))

(defun emacs-audit--async-process (name command sentinel)
  "TODO: NAME COMMAND SENTINEL THEN docstring."
  (let ((process-buffer (generate-new-buffer "*temp*")))
    (with-current-buffer process-buffer
      (let* ((process (make-process
                       :name name
                       :buffer process-buffer
                       :command command
                       :stderr (format "*%s stderr*" name)
                       :sentinel sentinel)))
        process))))

(defmacro emacs-audit--async-process-on-finished (process event &optional body)
  "TODO: PROCESS EVENT BODY docstring."
  `(when (memq (process-status process) '(exit signal))
     (setq ,event (substring ,event 0 -1))
     (when (string-match "^finished" ,event)
       (message (concat (process-name process) " finished"))
       (let ((process ,process))
         ,body))))

(defun emacs-audit--package-index-enrich-command (command)
  "TODO: COMMAND docstring."
  (append command
          '("enrich-package-index")
          (list (emacs-audit--package-index-raw-file-path))
          (apply 'list (when emacs-audit-github-token
                         (list "--token" emacs-audit-github-token)))))

(defun emacs-audit--package-index-enrich-on-finished
    (process &optional list-packages)
  "TODO: PROCESS LIST-PACKAGES docstring."
  (when-let ((process-buffer (process-buffer process)))
    (when (buffer-live-p process-buffer)
      (with-current-buffer process-buffer
        (goto-char (point-min))
        (setq emacs-audit--package-index (json-parse-buffer)))
      (when list-packages
        (emacs-audit-list-packages nil)))))

(defun emacs-audit--package-index-enrich-process-sentinel
    (process event &optional list-packages)
  "TODO: PROCESS EVENT LIST-PACKAGES docstring."
  (emacs-audit--async-process-on-finished
   process event
   (when (eq process emacs-audit--enrich-package-index-process)
     (setq emacs-audit--enrich-package-index-process nil)
     (emacs-audit--package-index-enrich-on-finished process list-packages)
     (kill-buffer (process-buffer process)))))

(defun emacs-audit--package-index-enrich (base-command)
  "TODO: BASE-COMMAND docstring."
  (interactive)
  (let* ((list-packages (not (called-interactively-p 'any)))
         (sentinel #'(lambda (process event)
                       (emacs-audit--package-index-enrich-process-sentinel
                        process event (when list-packages
                                        'list-packages)))))
    (message "Fetching additional package data asynchronously%s"
             (if list-packages
                 ", package list will open soon"
               ""))
    (let ((process (emacs-audit--async-process
                    "emacs-audit-enrich-package-index"
                    (emacs-audit--package-index-enrich-command base-command)
                    sentinel)))
      (process-put process
                   'process-buffer
                   (process-buffer process))
      (setq emacs-audit--enrich-package-index-process process)
      process)))

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

(defun emacs-audit--package-index-dump (&optional path)
  "TODO: PATH docstring."
  (interactive)
  (or path (setq path (emacs-audit--package-index-raw-file-path)))
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file path
      (insert
       (json-encode
        (mapcar
         (lambda (package)
           (let ((package-name (car package))
                 (package-desc (cadr package)))
             (cons
              package-name
              (let* ((extras (package-desc-extras package-desc))
                     (dir (package-desc-dir package-desc))
                     (file-name (or (alist-get
                                     package-name
                                     emacs-audit-special-package-file-names)
                                    (format "%s.el" package-name)))
                     (file-path (format "%s/%s" dir file-name)))
                `((name . ,package-name)
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

;;;; Commands

(defun emacs-audit-setup ()
  "TODO: docstring."
  (interactive)
  (emacs-audit--archive-download)
  (emacs-audit--archive-extract))

(defun emacs-audit-clean ()
  "TODO: docstring."
  (interactive)
  (plist-each (lambda (directory-type _directory-name)
                (delete-directory (emacs-audit--directory directory-type)
                                  'recursive))
              emacs-audit--directories)
  (-each '(emacs-audit--extracted-archive-directory
           emacs-audit--package-index)
    (lambda (var)
      (set var nil))))

(defun emacs-audit-list-packages-refresh ()
  "TODO: docstring."
  (interactive)
  (let ((command (emacs-audit--command)))
    (if (file-exists-p (emacs-audit--package-index-raw-file-path))
        (emacs-audit--package-index-enrich command)
      (progn
        (message "Writing local package index to disk...")
        (emacs-audit--package-index-dump)
        (emacs-audit--package-index-enrich command)))))

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
