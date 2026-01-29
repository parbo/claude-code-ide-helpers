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
;;
;; Usage:
;;   (require 'claude-code-ide-helpers)
;;   (add-hook 'vterm-mode-hook #'claude-code-ide-helpers-enable-status-mode)
;;
;; Keybindings (suggested):
;;   C-c C-l  - claude-code-ide-helpers-arrange-windows
;;   C-c C-n  - claude-code-ide-helpers-cycle-main

;;; Code:

(require 'cl-lib)

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

(defun claude-code-ide-helpers--apply-layout (main-buf side-bufs)
  "Apply Claude window layout with MAIN-BUF as main and SIDE-BUFS on right."
  (setq claude-code-ide-helpers--main-buffer main-buf)
  (delete-other-windows)
  (switch-to-buffer main-buf)
  (when side-bufs
    (let ((side-window (split-window-right (floor (* 0.75 (frame-width))))))
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

(provide 'claude-code-ide-helpers)
;;; claude-code-ide-helpers.el ends here
