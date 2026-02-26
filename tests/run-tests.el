;;; run-tests.el --- Batch test runner for pass-simple -*- lexical-binding: t; -*-

(require 'ert)
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))

(require 'pass-simple-test)

(ert-run-tests-batch-and-exit t)

;;; run-tests.el ends here
