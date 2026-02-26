;;; pass-simple-test.el --- Tests for pass-simple -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(require 'pass-simple)

(ert-deftest pass-simple-set-env-prefers-pass-value ()
  "Use pass value when available."
  (let ((old (getenv "PASS_SIMPLE_TEST_ENV")))
    (unwind-protect
        (progn
          (setenv "PASS_SIMPLE_TEST_ENV" "old")
          (cl-letf (((symbol-function 'pass-simple-get-secret)
                     (lambda (&rest _) "from-pass"))
                    ((symbol-function 'pass-simple-get-secret-auth)
                     (lambda (&rest _) "from-auth")))
            (should (equal (pass-simple-set-env "PASS_SIMPLE_TEST_ENV" "path" "host")
                           "from-pass"))
            (should (equal (getenv "PASS_SIMPLE_TEST_ENV") "from-pass"))))
      (setenv "PASS_SIMPLE_TEST_ENV" old))))

(ert-deftest pass-simple-set-env-falls-back-to-auth-source ()
  "Use auth-source value when pass does not return one."
  (let ((old (getenv "PASS_SIMPLE_TEST_ENV")))
    (unwind-protect
        (progn
          (setenv "PASS_SIMPLE_TEST_ENV" nil)
          (cl-letf (((symbol-function 'pass-simple-get-secret)
                     (lambda (&rest _) nil))
                    ((symbol-function 'pass-simple-get-secret-auth)
                     (lambda (&rest _) "from-auth")))
            (should (equal (pass-simple-set-env "PASS_SIMPLE_TEST_ENV" "path" "host")
                           "from-auth"))
            (should (equal (getenv "PASS_SIMPLE_TEST_ENV") "from-auth"))))
      (setenv "PASS_SIMPLE_TEST_ENV" old))))

(ert-deftest pass-simple-upsert-skips-identical-secret ()
  "Do not insert when existing secret is unchanged."
  (let (insert-called)
    (cl-letf (((symbol-function 'pass-simple--existing-first-line)
               (lambda (&rest _) "same"))
              ((symbol-function 'pass-simple-insert)
               (lambda (&rest _) (setq insert-called t)))
              ((symbol-function 'message) #'ignore))
      (pass-simple-upsert "entry/path" "same")
      (should-not insert-called))))

(ert-deftest pass-simple-upsert-prompts-and-overwrites-when-confirmed ()
  "Overwrite differing secret when user confirms."
  (let (insert-args)
    (cl-letf (((symbol-function 'pass-simple--existing-first-line)
               (lambda (&rest _) "old"))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) t))
              ((symbol-function 'pass-simple-insert)
               (lambda (&rest args) (setq insert-args args)))
              ((symbol-function 'message) #'ignore))
      (pass-simple-upsert "entry/path" "new")
      (should (equal insert-args '("entry/path" "new" t))))))

(ert-deftest pass-simple-export-env-populates-all-targets ()
  "Export values from `pass-simple-secret-specs' into one or many env vars."
  (let ((pass-simple-secret-specs
         '((one :pass "path/one" :env "PASS_SIMPLE_ONE")
           (two :pass "path/two" :env ("PASS_SIMPLE_TWO_A" "PASS_SIMPLE_TWO_B"))))
        (old-one (getenv "PASS_SIMPLE_ONE"))
        (old-two-a (getenv "PASS_SIMPLE_TWO_A"))
        (old-two-b (getenv "PASS_SIMPLE_TWO_B")))
    (unwind-protect
        (progn
          (setenv "PASS_SIMPLE_ONE" nil)
          (setenv "PASS_SIMPLE_TWO_A" nil)
          (setenv "PASS_SIMPLE_TWO_B" nil)
          (cl-letf (((symbol-function 'pass-simple--read-first-line)
                     (lambda (path)
                       (cond
                        ((equal path "path/one") "one-secret")
                        ((equal path "path/two") "two-secret")
                        (t nil)))))
            (pass-simple-export-env)
            (should (equal (getenv "PASS_SIMPLE_ONE") "one-secret"))
            (should (equal (getenv "PASS_SIMPLE_TWO_A") "two-secret"))
            (should (equal (getenv "PASS_SIMPLE_TWO_B") "two-secret"))))
      (setenv "PASS_SIMPLE_ONE" old-one)
      (setenv "PASS_SIMPLE_TWO_A" old-two-a)
      (setenv "PASS_SIMPLE_TWO_B" old-two-b))))

(provide 'pass-simple-test)

;;; pass-simple-test.el ends here
