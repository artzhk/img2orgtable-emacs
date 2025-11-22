;;; -*- lexical-binding: t; -*-
(require 'cl-lib)              ; needed for cl-letf, cl-destructuring-bind, etc.
(require 'img2orgtable)

(ert-deftest img2orgtable-backend-detect ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "XDG_SESSION_TYPE" "wayland")
    (should (eq 'wayland (img2orgtable--env-backend)))
    (setenv "XDG_SESSION_TYPE" "x11")
    (should (eq 'x11 (img2orgtable--env-backend)))))

(ert-deftest img2orgtable-cmd-selection ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "XDG_SESSION_TYPE" "x11")
    (cl-destructuring-bind (prog . args)
        (img2orgtable--cmd-screenshot "/tmp/out.png")
      (should (equal prog "maim"))
      (should (member "-s" args)))
    (setenv "XDG_SESSION_TYPE" "wayland")
    (cl-destructuring-bind (prog . args)
        (img2orgtable--cmd-screenshot "/tmp/out.png")
      (should (equal prog "sh"))
      (let ((cmd (mapconcat #'identity args " ")))
        (should (string-match-p "\\bgrim\\b" cmd))
        (should (string-match-p "\\bslurp\\b" cmd))))))

(ert-deftest img2orgtable-end-to-end-stubbed ()
  ;; End-to-end test of img2orgtable-screenshot-to-org with all external calls stubbed.
  (cl-letf* (((symbol-function 'img2orgtable--cmd-screenshot)
              (lambda (_outfile)
                (cons "stub-screenshot" '())))
             ((symbol-function 'img2orgtable--call)
              ;; Replace all external calls with pure stubs.
              (lambda (prog _args)
                (cond
                 ((string= prog "stub-screenshot") (cons 0 ""))
                 (t (cons 0 "| col1 | col2 |\n|----+-----|\n| x   | y   |\n"))))))
    (with-temp-buffer
      ;; Optional: mock buffer-file-name for img2orgtable--call default-directory logic
      (setq buffer-file-name (expand-file-name "img2orgtable.el" "tests"))
      (img2orgtable-screenshot-to-org)
      (let ((out (buffer-substring-no-properties (point-min) (point-max))))
        (should (and (string-match-p "| col1" out)
                     (string-match-p "|----" out)))))))

(ert-deftest img2orgtable-extractor-error-propagates ()
  ;; Extraction error should propagate.
  (cl-letf* (((symbol-function 'img2orgtable--cmd-screenshot)
              (lambda (_outfile)
                (cons "stub-screenshot" '())))
             ((symbol-function 'img2orgtable--call)
              (lambda (prog _args)
                (if (string= prog "stub-screenshot")
                    ;; screenshot OK
                    (cons 0 "")
                  ;; extractor fails
                  (cons 2 "fail")))))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "img2orgtable.el" "tests"))
      (let* ((err (should-error (img2orgtable-screenshot-to-org) :type 'user-error))
             (msg (error-message-string err)))
        (should (string-match-p "fail" msg)))))

  ;; Test that missing python3 is detected and reported.
  (cl-letf* (((symbol-function 'img2orgtable--cmd-screenshot)
              (lambda (_outfile)
                (cons "stub-screenshot" '())))
             ((symbol-function 'img2orgtable--call)
              (lambda (prog _args)
                ;; screenshot succeeds; extractor never called in this branch
                (when (string= prog "stub-screenshot")
                  (cons 0 ""))))
             ((symbol-function 'img2orgtable-auto-python)
              (lambda ()
                (user-error "python3 not found"))))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "img2orgtable.el" "tests"))
      (setq img2orgtable-python nil)
      (let* ((err (should-error (img2orgtable-screenshot-to-org) :type 'user-error))
             (msg (error-message-string err)))
        (should (string-match-p "python3 not found" msg))))))

