;;; claude-code-ide-helpers.el --- Window management and status for claude-code-ide -*- lexical-binding: t; -*-

;; Author: Par Bohrarper <par.bohrarper@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/parbo/claude-code-ide-helpers
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Extensions for claude-code-ide that help manage multiple Claude instances.
;;
;; Features:
;;   - Window layout with one main buffer and smaller stacked side buffers
;;   - Modeline indicator showing [Ready] or [Working...] status
;;   - Commands to arrange and cycle through Claude buffers
;;   - Simple diff-mode viewer as a lightweight alternative to ediff
;;
;; Usage:
;;   (require 'claude-code-ide-helpers)
;;   (add-hook 'vterm-mode-hook #'claude-code-ide-helpers-enable-status-mode)
;;
;; To use the simple diff viewer instead of ediff:
;;   (claude-code-ide-helpers-simple-diff-mode 1)
;;
;; Keybindings (suggested):
;;   C-c C-l  - claude-code-ide-helpers-arrange-windows
;;   C-c C-n  - claude-code-ide-helpers-cycle-main

;;; Code:

(require 'cl-lib)
(require 'diff)

(defgroup claude-code-ide-helpers nil
  "Helpers for claude-code-ide."
  :group 'tools
  :prefix "claude-code-ide-helpers-")

(defun claude-code-ide-helpers-get-buffers ()
  "Get all Claude Code IDE buffers."
  (seq-filter (lambda (buf)
                (string-match-p "\\*claude-code\\[" (buffer-name buf)))
              (buffer-list)))

;; Modeline indicator for Claude buffer status
(defvar-local claude-code-ide-helpers--status 'unknown
  "Status of Claude in this buffer: `ready', `working', or `unknown'.")

(defvar-local claude-code-ide-helpers--last-output-time nil
  "Time of last output in this Claude buffer.")

(defvar-local claude-code-ide-helpers--last-point-max nil
  "Last known point-max, used to detect new output.")

(defcustom claude-code-ide-helpers-idle-threshold 5.0
  "Seconds of no output before considering Claude ready."
  :type 'number
  :group 'claude-code-ide-helpers)

(defcustom claude-code-ide-helpers-side-window-width 80
  "Width of the right-hand side window in the Claude layout.
If an integer, specifies the width in characters.
If a float between 0 and 1, specifies the fraction of frame width."
  :type '(choice (integer :tag "Width in characters")
                 (float :tag "Fraction of frame width (0.0-1.0)"))
  :group 'claude-code-ide-helpers)

(defun claude-code-ide-helpers--modeline-indicator ()
  "Return a modeline string indicating Claude status."
  (pcase claude-code-ide-helpers--status
    ('ready (propertize " ✓" 'face '(:foreground "green")))
    ('working (propertize " ✱" 'face '(:foreground "orange")))
    (_ "")))

(defun claude-code-ide-helpers--check-status ()
  "Check and update Claude buffer status based on output activity."
  (when (and (bound-and-true-p claude-code-ide-helpers-status-mode)
             (string-match-p "\\*claude-code\\[" (buffer-name)))
    (let ((current-max (point-max)))
      (if (not (equal current-max claude-code-ide-helpers--last-point-max))
          ;; New output detected
          (progn
            (setq claude-code-ide-helpers--last-point-max current-max)
            (setq claude-code-ide-helpers--last-output-time (current-time))
            (setq claude-code-ide-helpers--status 'working))
        ;; No new output - check if idle long enough
        (when (and claude-code-ide-helpers--last-output-time
                   (> (float-time (time-subtract (current-time)
                                                 claude-code-ide-helpers--last-output-time))
                      claude-code-ide-helpers-idle-threshold))
          (setq claude-code-ide-helpers--status 'ready)))
      (force-mode-line-update))))

(defvar claude-code-ide-helpers--status-timer nil
  "Timer for checking Claude buffer status.")

(defun claude-code-ide-helpers--update-all-status ()
  "Update status for all Claude buffers."
  (dolist (buf (claude-code-ide-helpers-get-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-code-ide-helpers--check-status)))))

(define-minor-mode claude-code-ide-helpers-status-mode
  "Minor mode to show Claude status in modeline."
  :lighter nil
  (if claude-code-ide-helpers-status-mode
      (progn
        (setq claude-code-ide-helpers--last-point-max (point-max))
        (setq claude-code-ide-helpers--last-output-time (current-time))
        (setq claude-code-ide-helpers--status 'working)
        ;; Start global timer if not running
        (unless claude-code-ide-helpers--status-timer
          (setq claude-code-ide-helpers--status-timer
                (run-with-timer 1 1 #'claude-code-ide-helpers--update-all-status))))
    ;; Cleanup when disabled
    (when (and claude-code-ide-helpers--status-timer
               (null (seq-filter (lambda (buf)
                                   (buffer-local-value 'claude-code-ide-helpers-status-mode buf))
                                 (buffer-list))))
      (cancel-timer claude-code-ide-helpers--status-timer)
      (setq claude-code-ide-helpers--status-timer nil))))

(defun claude-code-ide-helpers-enable-status-mode ()
  "Enable status mode for Claude buffers."
  (when (string-match-p "\\*claude-code\\[" (buffer-name))
    (claude-code-ide-helpers-status-mode 1)))

(defun claude-code-ide-helpers-enable-status-mode-all ()
  "Enable status mode for all existing Claude buffers."
  (interactive)
  (dolist (buf (claude-code-ide-helpers-get-buffers))
    (with-current-buffer buf
      (unless claude-code-ide-helpers-status-mode
        (claude-code-ide-helpers-status-mode 1)))))

(defun claude-code-ide-helpers--buffer-waiting-p (buf)
  "Check if Claude buffer BUF appears to be waiting for input."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (forward-line -3)
      (let ((end-text (buffer-substring-no-properties (point) (point-max))))
        (or (string-match-p ">" end-text)
            (string-match-p "waiting" end-text)
            (string-match-p "ready" end-text)
            (and (derived-mode-p 'vterm-mode)
                 (fboundp 'vterm--get-cursor-point)
                 (= (point-max) (vterm--get-cursor-point))))))))

;;;###autoload
(defun claude-code-ide-helpers-arrange-windows ()
  "Arrange Claude buffers with main window and stacked side windows.
Layout:
---------------------------
|                  |      |
|                  |------|
|      Main        |      |
|                  |------|
|                  |      |
---------------------------"
  (interactive)
  (let ((claude-buffers (claude-code-ide-helpers-get-buffers)))
    (if (null claude-buffers)
        (message "No Claude Code buffers found")
      ;; Sort buffers - put waiting ones first
      (setq claude-buffers
            (sort claude-buffers
                  (lambda (a b)
                    (and (claude-code-ide-helpers--buffer-waiting-p a)
                         (not (claude-code-ide-helpers--buffer-waiting-p b))))))
      (claude-code-ide-helpers--apply-layout (car claude-buffers) (cdr claude-buffers)))))

(defvar claude-code-ide-helpers--main-buffer nil
  "The current main Claude buffer for cycling.")

(defun claude-code-ide-helpers--calculate-main-width ()
  "Calculate the main window width based on side window configuration."
  (let ((side-width claude-code-ide-helpers-side-window-width)
        (frame-w (frame-width)))
    (if (floatp side-width)
        ;; Percentage: side-width is fraction for the side, main gets the rest
        (floor (* (- 1.0 side-width) frame-w))
      ;; Character width: subtract from frame width
      (max 20 (- frame-w side-width)))))

(defun claude-code-ide-helpers--apply-layout (main-buf side-bufs)
  "Apply Claude window layout with MAIN-BUF as main and SIDE-BUFS on right."
  (setq claude-code-ide-helpers--main-buffer main-buf)
  (delete-other-windows)
  (switch-to-buffer main-buf)
  (when side-bufs
    (let ((side-window (split-window-right
                        (claude-code-ide-helpers--calculate-main-width))))
      (select-window side-window)
      (switch-to-buffer (car side-bufs))
      (dolist (buf (cdr side-bufs))
        (let ((new-win (split-window-below)))
          (select-window new-win)
          (switch-to-buffer buf)))
      (balance-windows side-window)))
  (select-window (get-buffer-window main-buf))
  (message "Main: %s" (buffer-name main-buf)))

(defun claude-code-ide-helpers--buffer-project-name (buf)
  "Extract project name from Claude buffer BUF name."
  (let ((name (buffer-name buf)))
    (if (string-match "\\*claude-code\\[\\([^]]+\\)\\]\\*" name)
        (match-string 1 name)
      name)))

;;;###autoload
(defun claude-code-ide-helpers-switch-main ()
  "Switch main Claude buffer using completing-read.
Integrates with vertico, ivy, etc. for fuzzy matching."
  (interactive)
  (let* ((claude-buffers (claude-code-ide-helpers-get-buffers))
         (buf-alist (mapcar (lambda (buf)
                              (cons (claude-code-ide-helpers--buffer-project-name buf) buf))
                            claude-buffers)))
    (if (null claude-buffers)
        (message "No Claude Code buffers found")
      (let* ((choice (completing-read "Claude project: " buf-alist nil t))
             (selected-buf (cdr (assoc choice buf-alist))))
        (when selected-buf
          (claude-code-ide-helpers--apply-layout
           selected-buf
           (cl-remove selected-buf claude-buffers)))))))

;;;###autoload
(defun claude-code-ide-helpers-cycle-main ()
  "Cycle which Claude buffer is the main (large) window."
  (interactive)
  (let* ((claude-buffers (claude-code-ide-helpers-get-buffers))
         (num-buffers (length claude-buffers)))
    (when (> num-buffers 1)
      ;; Find current main in the list and get next one
      (let* ((current-pos (or (cl-position claude-code-ide-helpers--main-buffer
                                           claude-buffers)
                              0))
             (next-pos (mod (1+ current-pos) num-buffers))
             (new-main (nth next-pos claude-buffers)))
        (claude-code-ide-helpers--apply-layout
         new-main
         (cl-remove new-main claude-buffers))))))

;;; Session persistence

(defcustom claude-code-ide-helpers-session-file
  (expand-file-name "claude-sessions" user-emacs-directory)
  "File to store active Claude session directories."
  :type 'file
  :group 'claude-code-ide-helpers)

(defun claude-code-ide-helpers--get-session-directories ()
  "Get list of directories for all active Claude sessions."
  (cl-remove-duplicates
   (mapcar (lambda (buf)
             (with-current-buffer buf
               (expand-file-name default-directory)))
           (claude-code-ide-helpers-get-buffers))
   :test #'string=))

(defun claude-code-ide-helpers-save-sessions ()
  "Save active Claude session directories to file."
  (let ((dirs (claude-code-ide-helpers--get-session-directories)))
    (when dirs
      (with-temp-file claude-code-ide-helpers-session-file
        (prin1 dirs (current-buffer))))))

(defun claude-code-ide-helpers--load-saved-sessions ()
  "Load saved session directories from file."
  (when (file-exists-p claude-code-ide-helpers-session-file)
    (with-temp-buffer
      (insert-file-contents claude-code-ide-helpers-session-file)
      (goto-char (point-min))
      (ignore-errors (read (current-buffer))))))

;;;###autoload
(defun claude-code-ide-helpers-restore-sessions ()
  "Restore Claude sessions from last Emacs session.
Shows a selection UI to choose which sessions to restore."
  (interactive)
  (let ((saved-dirs (claude-code-ide-helpers--load-saved-sessions)))
    (if (null saved-dirs)
        (message "No saved Claude sessions found")
      ;; Filter to only existing directories
      (setq saved-dirs (cl-remove-if-not #'file-directory-p saved-dirs))
      (if (null saved-dirs)
          (message "No valid session directories found")
        ;; Use completing-read-multiple for selection
        (let* ((options (cons "[All]" saved-dirs))
               (choices (completing-read-multiple
                         "Restore sessions (comma-separated): "
                         options nil t))
               (to-restore (if (member "[All]" choices)
                               saved-dirs
                             choices)))
          (if (null to-restore)
              (message "No sessions selected")
            (dolist (dir to-restore)
              (let ((default-directory dir))
                (when (fboundp 'claude-code-ide-resume)
                  (claude-code-ide-resume))))
            (message "Restored %d session(s)" (length to-restore))))))))

;; Save sessions on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-ide-helpers-save-sessions)

;;; Simple diff viewer (alternative to ediff)

(declare-function claude-code-ide-mcp-complete-deferred "claude-code-ide-mcp"
  (session method result &optional unique-key))
(declare-function claude-code-ide-mcp--find-session-for-file "claude-code-ide-mcp-handlers"
  (file-path))
(declare-function claude-code-ide-mcp--get-current-session "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp--get-active-diffs "claude-code-ide-mcp-handlers"
  (&optional session))
(declare-function claude-code-ide-mcp--cleanup-diff "claude-code-ide-mcp-handlers"
  (tab-name &optional session))
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide"
  (buffer))
(declare-function claude-code-ide--get-buffer-name "claude-code-ide"
  (&optional directory))
(defvar claude-code-ide-mcp--sessions)

(defvar-local claude-code-ide-helpers--diff-tab-name nil)
(defvar-local claude-code-ide-helpers--diff-session nil)
(defvar-local claude-code-ide-helpers--diff-new-contents nil)
(defvar-local claude-code-ide-helpers--diff-old-file nil)

(defvar claude-code-ide-helpers-simple-diff-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-ide-helpers-diff-accept)
    (define-key map (kbd "C-c C-k") #'claude-code-ide-helpers-diff-reject)
    (define-key map (kbd "q") #'claude-code-ide-helpers-diff-reject)
    map))

(defun claude-code-ide-helpers--generate-unified-diff (old-file new-contents)
  "Generate a unified diff between OLD-FILE and NEW-CONTENTS."
  (let ((new-file (make-temp-file "claude-diff-new")))
    (unwind-protect
        (progn
          (with-temp-file new-file
            (insert new-contents))
          (let ((old (if (file-exists-p old-file) old-file "/dev/null")))
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (file-name-nondirectory old-file)
                            "--label" (file-name-nondirectory old-file)
                            old new-file)
              (buffer-string))))
      (delete-file new-file))))

(defun claude-code-ide-helpers--show-simple-diff (arguments)
  "Show diff in a `diff-mode' buffer instead of ediff.
ARGUMENTS is the openDiff MCP tool arguments alist."
  (let* ((old-file-path (alist-get 'old_file_path arguments))
         (new-file-contents (alist-get 'new_file_contents arguments))
         (tab-name (alist-get 'tab_name arguments))
         (session (or (claude-code-ide-mcp--find-session-for-file old-file-path)
                      (claude-code-ide-mcp--get-current-session))))
    (unless (and old-file-path new-file-contents tab-name)
      (signal 'mcp-error '("Missing required parameters for openDiff")))
    (unless session
      (signal 'mcp-error '("No active MCP session found")))
    ;; Clean up any existing diff with this tab name
    (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
      (when (gethash tab-name active-diffs)
        (claude-code-ide-mcp--cleanup-diff tab-name session)))
    (let* ((diff-output (claude-code-ide-helpers--generate-unified-diff
                         old-file-path new-file-contents))
           (buf-name (format "*Diff: %s*" (file-name-nondirectory old-file-path)))
           (diff-buf (get-buffer-create buf-name)))
      (with-current-buffer diff-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert diff-output)
          (if (string-empty-p (string-trim diff-output))
              (insert "(no changes)\n")
            (diff-mode)))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (use-local-map (make-composed-keymap
                        claude-code-ide-helpers-simple-diff-map
                        (current-local-map)))
        (setq-local claude-code-ide-helpers--diff-tab-name tab-name)
        (setq-local claude-code-ide-helpers--diff-session session)
        (setq-local claude-code-ide-helpers--diff-new-contents new-file-contents)
        (setq-local claude-code-ide-helpers--diff-old-file old-file-path)
        (setq-local header-line-format
                     (substitute-command-keys
                      "Diff: \\[claude-code-ide-helpers-diff-accept] accept  \
\\[claude-code-ide-helpers-diff-reject] reject")))
      ;; Store in active-diffs so close_tab can find it
      (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
        (puthash tab-name
                 `((diff-buffer . ,diff-buf)
                   (old-file-path . ,old-file-path)
                   (session . ,session)
                   (created-at . ,(current-time)))
                 active-diffs))
      (display-buffer diff-buf '(display-buffer-use-some-window))
      `((deferred . t)
        (unique-key . ,tab-name)
        (session . ,session)))))

(defun claude-code-ide-helpers-diff-accept ()
  "Accept the proposed changes."
  (interactive)
  (let ((tab-name claude-code-ide-helpers--diff-tab-name)
        (session claude-code-ide-helpers--diff-session)
        (new-contents claude-code-ide-helpers--diff-new-contents)
        (buf (current-buffer)))
    (claude-code-ide-mcp-complete-deferred
     session "openDiff"
     (list `((type . "text") (text . "FILE_SAVED"))
           `((type . "text") (text . ,new-contents)))
     tab-name)
    (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
      (when active-diffs
        (remhash tab-name active-diffs)))
    (quit-window t (get-buffer-window buf))))

(defun claude-code-ide-helpers-diff-reject ()
  "Reject the proposed changes."
  (interactive)
  (let ((tab-name claude-code-ide-helpers--diff-tab-name)
        (session claude-code-ide-helpers--diff-session)
        (buf (current-buffer)))
    (claude-code-ide-mcp-complete-deferred
     session "openDiff"
     (list `((type . "text") (text . "DIFF_REJECTED"))
           `((type . "text") (text . ,tab-name)))
     tab-name)
    (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
      (when active-diffs
        (remhash tab-name active-diffs)))
    (quit-window t (get-buffer-window buf))))

;;;###autoload
(define-minor-mode claude-code-ide-helpers-simple-diff-mode
  "Use a simple diff-mode buffer instead of ediff for Claude diffs.
When enabled, overrides the openDiff MCP handler to show a unified
diff in a regular buffer with accept/reject keybindings."
  :global t
  :group 'claude-code-ide-helpers
  (if claude-code-ide-helpers-simple-diff-mode
      (advice-add 'claude-code-ide-mcp-handle-open-diff :override
                  #'claude-code-ide-helpers--show-simple-diff)
    (advice-remove 'claude-code-ide-mcp-handle-open-diff
                   #'claude-code-ide-helpers--show-simple-diff)))

(provide 'claude-code-ide-helpers)
;;; claude-code-ide-helpers.el ends here
