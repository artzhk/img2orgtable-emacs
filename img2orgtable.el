;;; img2orgtable.el --- Image -> Org table via external screenshot + Python -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "24.3"))
;; Version: 0.1
;; Keywords: multimedia, tools, image-to-table, org
;; URL: https://github.com/artzhk/img2orgtable

(defgroup img2orgtable nil
  "Convert screenshots of tables into Org tables."
  :group 'convenience)

(defun img2orgtable--repo-root ()
  "Return the img2orgtable repo root directory."
  (file-name-directory (or load-file-name (locate-library "img2orgtable"))))

(defcustom img2orgtable-python nil
  "Python interpreter used by img2orgtable.
If nil, img2orgtable will auto-detect.

Auto-detection order:
  1. .venv/bin/python3 under the repo root, if executable.
  2. python3 in PATH.
  3. python in PATH."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file  :tag "Python executable"))
  :group 'img2orgtable)

(defcustom img2orgtable-python-script "img2orgtable.py"
  "Path to img2orgtable.py, relative to the img2orgtable.el file if not absolute."
  :type 'file
  :group 'img2orgtable)

;; Important to stay
(defcustom img2orgtable--screenshot-cmd nil
  "Override the default screenshot command.

If nil, a platform/desktop specific command is used.
If non-nil, must be a shell command string.  It may contain a
\"%s\" placeholder, which will be replaced with the shell-quoted
output filename.  If it does not, the filename is appended."
  :type '(choice (const :tag "Default per platform" nil)
                 (string :tag "Shell command"))
  :group 'img2orgtable)

(defcustom img2orgtable-timeout 20
  "Seconds to wait for external commands."
  :type 'integer
  :group 'img2orgtable)

;; Must be derived from repo root if repo root is specified
(defun img2orgtable-auto-python ()
  "Resolve Python interpreter for img2orgtable.

Respects `img2orgtable-python' when non-nil, otherwise auto-detect."
  (or img2orgtable-python
      (let* ((root (img2orgtable--repo-root))
             (venv-py3 (expand-file-name ".venv/bin/python3" root)))
        (cond
         ((file-executable-p venv-py3) venv-py3)
         ((executable-find "python3"))
         ((executable-find "python"))
         (t (user-error "No suitable python found for img2orgtable"))))))

(defun img2orgtable--script-path ()
  "Return absolute path to the img2orgtable Python script."
  (let* ((lib (or load-file-name (locate-library "img2orgtable")))
         (base (file-name-directory lib))
         (script img2orgtable-python-script))
    (if (file-name-absolute-p script)
        script
      (expand-file-name script base))))

(defun img2orgtable--tmpfile (&optional suffix)
  (make-temp-file "img2orgtable-" nil (or suffix ".png")))

(defun img2orgtable--env-backend ()
  ;; Wayland/X11 detection per XDG_SESSION_TYPE; fall back to heuristics.
  ;; Returns symbol: 'wayland, 'x11, or nil on unknown.
  (pcase (downcase (or (getenv "XDG_SESSION_TYPE") ""))
    ("wayland" 'wayland)
    ("x11" 'x11)
    (_ nil)))

(defun img2orgtable--system ()
  (cond
   ((eq system-type 'windows-nt) 'windows)
   ((memq system-type '(gnu gnu/linux gnu/kfreebsd)) 'linux)
   (t nil)))

(defun img2orgtable--cmd-screenshot (outfile)
  "Return (PROGRAM . ARGS) for region screenshot to OUTFILE.

Respects `img2orgtable--screenshot-cmd' when non-nil."
  (unless (stringp outfile)
    (error "OUTFILE must be a string"))
  (if img2orgtable--screenshot-cmd
      (let* ((cmd-template img2orgtable--screenshot-cmd)
             (quoted (shell-quote-argument outfile))
             (cmd (if (string-match-p "%s" cmd-template)
                      (format cmd-template quoted)
                    (format "%s %s" cmd-template quoted))))
        (cons "sh" (list "-c" cmd)))
    (let ((sys (img2orgtable--system))
          (be  (img2orgtable--env-backend)))
      (cond
       ;; Linux Wayland: grim + slurp
       ;; grim -g \"$(slurp)\" out.png
       ((and (eq sys 'linux) (eq be 'wayland))
        (cons "sh"
              (list "-c"
                    (format "grim -g \"$(slurp)\" %s"
                            (shell-quote-argument outfile)))))

       ;; Linux X11: maim -s out.png
       ((and (eq sys 'linux) (eq be 'x11))
        (cons "maim" (list "-s" outfile)))

       ;; Windows: PowerShell .NET capture entire desktop.
       ((eq sys 'windows)
        (let* ((out (replace-regexp-in-string "'" "''" outfile))
               (ps (concat
                    "$s=[System.Windows.Forms.SystemInformation]::VirtualScreen;"
                    "$b=New-Object Drawing.Bitmap $s.Width,$s.Height;"
                    "$g=[Drawing.Graphics]::FromImage($b);"
                    "$g.CopyFromScreen($s.Left,$s.Top,[Drawing.Point]::new(0,0),$b.Size);"
                    "[void]$b.Save('" out "',[Imaging.ImageFormat]::Png)")))
          (cons "powershell"
                (list "-NoProfile" "-NonInteractive" "-Command"
                      "Add-Type -AssemblyName System.Windows.Forms; Add-Type -AssemblyName System.Drawing; "
                      (concat "& {" ps "}")))))

       (t (error "Unsupported platform/backend: %S / %S" sys be))))))

(defun img2orgtable--call (prog args)
  "Call external PROG with ARGS, returning (STATUS . OUTPUT)."
  (let ((buf (generate-new-buffer " *img2orgtable-cmd*")))
    (unwind-protect
        (let* ((default-directory
                 ;; Prefer library dir if available, fall back to current
                 (or (when load-file-name
                       (file-name-directory load-file-name))
                     (when buffer-file-name
                       (file-name-directory buffer-file-name))
                     default-directory))
               (status
                (with-current-buffer buf
                  (let ((process-connection-type nil))
                    (apply #'call-process prog nil t nil args)))))
          (cons status (with-current-buffer buf (buffer-string))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun img2orgtable-screenshot-to-org ()
  "Take a region screenshot, run extractor, and insert an Org table."
  (interactive)
  (let ((png (img2orgtable--tmpfile)))
    (pcase (img2orgtable--cmd-screenshot png)
      (`(,prog . ,args)
       (let ((status+out (img2orgtable--call prog args)))
         (unless (and (integerp (car status+out)) (= 0 (car status+out)))
           (user-error "Screenshot failed: %s" (cdr status+out)))))
      (_ (user-error "No screenshot command")))
    (unwind-protect
        (let* ((py (img2orgtable-auto-python))
               (script (img2orgtable--script-path))
               (status+out (img2orgtable--call py (list script png))))
          (if (and (integerp (car status+out)) (= 0 (car status+out)))
              (insert (cdr status+out))
            (user-error "Extraction failed: %s" (cdr status+out))))
      (when (file-exists-p png)
        (ignore-errors (delete-file png))))))

(provide 'img2orgtable)
;;; img2orgtable.el ends here

