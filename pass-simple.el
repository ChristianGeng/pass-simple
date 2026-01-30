;;; pass-simple.el --- Simple password store utilities -*- lexical-binding: t; -*-

;; Author: cgeng
;; URL: https://github.com/cgeng/pass-simple
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, password, pass

;;; Commentary:
;; Simple utilities for managing passwords with pass (password-store).
;; Provides functions for idempotent password insertion, bulk operations,
;; and environment variable initialization from secrets.
;;
;; Features:
;; - List, insert, remove, and upsert pass entries
;; - Bulk import from elisp files
;; - Set environment variables from pass or auth-source
;; - Optional integration with consult for fuzzy completion
;;
;; Usage:
;;   (require 'pass-simple)
;;   (pass-simple-list)              ; List all entries
;;   (pass-simple-upsert "path" "secret")  ; Idempotent insert
;;   (pass-simple-set-env "OPENAI_API_KEY" "code/openai" "openai.com")

;;; Code:

(require 'cl-lib)
(require 'ansi-color)

;; Optional deps: prefer Emacs password-store and consult if available.
;; We'll gracefully fall back to the pass CLI and completing-read.
(eval-and-compile
  (require 'password-store nil t)
  (require 'consult nil t)
  (require 'auth-source nil t))

;;; Variables

(defvar pass-simple--path-history nil
  "History list for pass entry prompts.")

(defvar pass-simple-secret-specs nil
  "Alist mapping secret names to plists with :pass and :env keys.
Example:
  \\='((openai :pass \"code/openai_api_key\" :env \"OPENAI_API_KEY\")
    (anthropic :pass \"code/anthropic_key\" :env (\"ANTHROPIC_API_KEY\" \"CLAUDE_KEY\")))")

;;; Internal helpers

(defun pass-simple--list-entries ()
  "Return a list of pass entry names as strings.
Prefers `password-store-list' if available; otherwise uses the pass CLI."
  (cond
   ((fboundp 'password-store-list)
    (password-store-list))
   ((executable-find "pass")
    (with-temp-buffer
      (let ((status (call-process "pass" nil t nil "list" "--flat")))
        (when (and (integerp status) (= status 0))
          (goto-char (point-min))
          (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
                 (clean (ansi-color-filter-apply raw)))
            (split-string clean "[\r\n]+" t))))))
   (t nil)))

(defun pass-simple--read-path (prompt &optional require-match)
  "Read a pass entry path with fuzzy completion via Consult when available.
PROMPT is the minibuffer prompt.
When REQUIRE-MATCH is non-nil, restrict to existing entries."
  (let ((cands (pass-simple--list-entries)))
    (cond
     ((featurep 'consult)
      (consult--read cands
                     :prompt prompt
                     :require-match require-match
                     :history 'pass-simple--path-history
                     :category 'pass-entry
                     :sort nil))
     (t
      (completing-read prompt cands nil require-match nil 'pass-simple--path-history)))))

(defun pass-simple--ensure ()
  "Ensure pass is available, signaling an error if not."
  (or (executable-find "pass")
      (user-error "pass(1) not found. Install and initialize pass + GPG")))

(defun pass-simple--existing-first-line (path)
  "Return first line of existing pass entry PATH, or nil if missing/error."
  (let (out)
    (with-temp-buffer
      (let ((status (call-process "pass" nil t nil "show" path)))
        (when (and (integerp status) (= status 0))
          (goto-char (point-min))
          (when (re-search-forward "\\`\\([^\n\r]+\\)" nil t)
            (setq out (match-string 1))))))
    out))

(defun pass-simple--read-first-line (path)
  "Read first line from pass entry at PATH, or nil if unavailable."
  (ignore-errors
    (with-temp-buffer
      (let ((status (call-process "pass" nil t nil "show" path)))
        (when (and (integerp status) (= status 0))
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))))))

;;; Core password store operations

;;;###autoload
(defun pass-simple-insert (path secret &optional force)
  "Insert SECRET at PATH via pass.  If FORCE, overwrite.
Interactively, prompt for PATH and SECRET.  With prefix arg, set FORCE."
  (interactive
   (list (pass-simple--read-path "pass path: ")
         (read-passwd "Secret: ")
         current-prefix-arg))
  (let ((pass (pass-simple--ensure)))
    (with-temp-buffer
      (insert secret "\n")
      (let* ((args (append '("insert" "-m") (when force '("-f")) (list path)))
             (status (apply #'call-process-region (point-min) (point-max)
                            pass nil nil nil args)))
        (unless (and (integerp status) (= status 0))
          (user-error "pass insert failed (status %S) for %s" status path))))))

;;;###autoload
(defun pass-simple-remove (path &optional force)
  "Remove PATH from the password store using pass.
If FORCE is non-nil, do not prompt before removal.
Interactively, prompt for PATH; with prefix arg, set FORCE."
  (interactive
   (list (pass-simple--read-path "pass path: " t)
         current-prefix-arg))
  (let ((pass (pass-simple--ensure)))
    (when (or force (y-or-n-p (format "pass: remove %s? " path)))
      (let* ((args (append '("rm") (when force '("-f")) (list path)))
             (status (apply #'call-process pass nil nil nil args)))
        (if (and (integerp status) (= status 0))
            (message "pass: removed %s" path)
          (user-error "pass rm failed (status %S) for %s" status path))))))

;;;###autoload
(defun pass-simple-list ()
  "List all entries in the password store.
Display the output in a buffer named *pass-list*."
  (interactive)
  (let ((pass (pass-simple--ensure)))
    (with-current-buffer (get-buffer-create "*pass-list*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((status (call-process pass nil t nil "list")))
          (if (and (integerp status) (= status 0))
              (progn
                (goto-char (point-min))
                (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
                       (rendered (ansi-color-filter-apply raw)))
                  (erase-buffer)
                  (insert rendered))
                (goto-char (point-min))
                (special-mode)
                (pop-to-buffer (current-buffer)))
            (user-error "pass list failed (status %S)" status)))))))

;;;###autoload
(defun pass-simple-upsert (path secret &optional force)
  "Idempotent insert: if PATH exists and equals SECRET, do nothing.
If different, overwrite when FORCE is non-nil; otherwise prompt.
Interactively, prompt for PATH and SECRET; prefix arg sets FORCE."
  (interactive
   (list (pass-simple--read-path "pass path: ")
         (read-passwd "Secret: ")
         current-prefix-arg))
  (let ((existing (pass-simple--existing-first-line path)))
    (cond
     ((and existing (string= existing secret))
      (message "pass: %s already set; skipping" path))
     ((and existing (not force))
      (when (y-or-n-p (format "pass: %s exists and differs. Overwrite? " path))
        (pass-simple-insert path secret t)
        (message "pass: %s updated" path)))
     (t
      (pass-simple-insert path secret force)
      (message "pass: %s inserted" path)))))

;;; Bulk operations

;;;###autoload
(defun pass-simple-bulk-insert-from-file (file &optional force symbol)
  "Load FILE (e.g. ~/.config/doom/my-secrets.el.gpg) and upsert all entries.
FILE must define an alist variable.  SYMBOL (default: pass-simple-secrets)
is the variable name to read.  With FORCE, overwrite without prompting."
  (interactive
   (list (read-file-name "Secrets file: " "~/.config/doom/" nil t nil
                         (lambda (f) (string-match-p "\\.el\\(\\.gpg\\)?\\'" f)))
         current-prefix-arg
         (intern (completing-read "Var symbol: "
                                  '(pass-simple-secrets api-keys)
                                  nil t nil nil "pass-simple-secrets"))))
  (let ((sym (or symbol 'pass-simple-secrets)))
    (unless (file-readable-p file)
      (user-error "Secrets file not readable: %s" file))
    (load file nil t)
    (unless (boundp sym)
      (user-error "Variable %s not defined in %s" sym file))
    (pass-simple-bulk-insert-from-var (symbol-value sym) force)))

;;;###autoload
(defun pass-simple-bulk-insert-from-var (alist &optional force)
  "Upsert all (PATH . SECRET) pairs from ALIST into pass.
With FORCE, overwrite differing entries without prompting."
  (interactive
   (list (let* ((sym (intern (completing-read "Var symbol: "
                                              obarray
                                              (lambda (s)
                                                (and (boundp s)
                                                     (listp (symbol-value s))))
                                              t nil nil "api-keys"))))
           (symbol-value sym))
         current-prefix-arg))
  (unless (and (listp alist)
               (cl-every (lambda (x)
                           (and (consp x)
                                (stringp (car x))
                                (stringp (cdr x))))
                         alist))
    (user-error "Expected an alist of (PATH . SECRET) strings"))
  (dolist (cell alist)
    (pass-simple-upsert (car cell) (cdr cell) force)))

;;; Secret retrieval and environment variables

;;;###autoload
(defun pass-simple-get-secret (path)
  "Return first line of pass entry at PATH, or nil if unavailable.
Prefers `password-store-get' if available; otherwise uses pass CLI."
  (cond
   ((and path (fboundp 'password-store-get))
    (ignore-errors (password-store-get path)))
   (path
    (pass-simple--read-first-line path))))

;;;###autoload
(defun pass-simple-get-secret-auth (host)
  "Return secret from auth-source for HOST, or nil if unavailable."
  (when (and host (fboundp 'auth-source-pick-first-password))
    (ignore-errors (auth-source-pick-first-password :host host))))

;;;###autoload
(defun pass-simple-set-env (env-name pass-path auth-host)
  "Set ENV-NAME from pass PASS-PATH or auth-source AUTH-HOST if found.
Falls back to existing ENV-NAME value.  Returns the value set (or nil)."
  (let* ((val (or (pass-simple-get-secret pass-path)
                  (pass-simple-get-secret-auth auth-host)
                  (getenv env-name))))
    (when (and val (> (length val) 0))
      (setenv env-name val))
    (getenv env-name)))

;;;###autoload
(defun pass-simple-init-api-key (env-name pass-path auth-host)
  "Initialize ENV-NAME using PASS-PATH or AUTH-HOST.
Compatibility wrapper for `pass-simple-set-env'."
  (pass-simple-set-env env-name pass-path auth-host))

;;;###autoload
(defun pass-simple-export-env (&optional only-missing)
  "Set env vars in Emacs from pass using `pass-simple-secret-specs'.
With ONLY-MISSING (prefix arg), don't overwrite vars already set."
  (interactive "P")
  (unless (bound-and-true-p pass-simple-secret-specs)
    (user-error "`pass-simple-secret-specs' is not defined"))
  (dolist (cell pass-simple-secret-specs)
    (let* ((spec  (cdr cell))
           (path  (plist-get spec :pass))
           (envs  (let ((e (plist-get spec :env))) (if (listp e) e (list e))))
           (value (pass-simple--read-first-line path)))
      (when (and value (not (string-empty-p value)))
        (dolist (name envs)
          (when (or (not only-missing) (null (getenv name)))
            (setenv name value)))))))

(provide 'pass-simple)
;;; pass-simple.el ends here
