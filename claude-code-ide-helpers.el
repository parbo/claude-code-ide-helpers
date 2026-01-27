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

(defgroup claude-code-ide-helpers nil
  "Helpers for claude-code-ide."
  :group 'tools
  :prefix "claude-code-ide-helpers-")

(defun claude-code-ide-helpers-get-buffers ()
  "Get all Claude Code IDE buffers."
  (seq-filter (lambda (buf)
                (string-match-p "\\*claude-code:" (buffer-name buf)))
              (buffer-list)))

;; Modeline indicator for Claude buffer status
(defvar-local claude-code-ide-helpers--status 'unknown
  "Status of Claude in this buffer: `ready', `working', or `unknown'.")

(defvar-local claude-code-ide-helpers--last-output-time nil
  "Time of last output in this Claude buffer.")

(defvar-local claude-code-ide-helpers--last-point-max nil
  "Last known point-max, used to detect new output.")

(defcustom claude-code-ide-helpers-idle-threshold 2.0
  "Seconds of no output before considering Claude ready."
  :type 'number
  :group 'claude-code-ide-helpers)

(defun claude-code-ide-helpers--modeline-indicator ()
  "Return a modeline string indicating Claude status."
  (pcase claude-code-ide-helpers--status
    ('ready (propertize " [Ready]" 'face '(:foreground "green" :weight bold)))
    ('working (propertize " [Working...]" 'face '(:foreground "orange")))
    (_ "")))

(defun claude-code-ide-helpers--check-status ()
  "Check and update Claude buffer status based on output activity."
  (when (and (bound-and-true-p claude-code-ide-helpers-status-mode)
             (string-match-p "\\*claude-code:" (buffer-name)))
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
  :lighter (:eval (claude-code-ide-helpers--modeline-indicator))
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
  (when (string-match-p "\\*claude-code:" (buffer-name))
    (claude-code-ide-helpers-status-mode 1)))

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
  "Arrange Claude buffers with one main window and smaller ones stacked on the right.
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
      (delete-other-windows)
      (let* ((main-buf (car claude-buffers))
             (side-bufs (cdr claude-buffers)))
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
        (message "Arranged %d Claude buffer(s). Main: %s"
                 (length claude-buffers) (buffer-name main-buf))))))

;;;###autoload
(defun claude-code-ide-helpers-cycle-main ()
  "Cycle which Claude buffer is the main (large) window."
  (interactive)
  (let ((claude-buffers (claude-code-ide-helpers-get-buffers)))
    (when (> (length claude-buffers) 1)
      (let ((current-main (car claude-buffers)))
        (setq claude-buffers (append (cdr claude-buffers) (list current-main))))
      (delete-other-windows)
      (switch-to-buffer (car claude-buffers))
      (when (cdr claude-buffers)
        (let ((side-window (split-window-right (floor (* 0.75 (frame-width))))))
          (select-window side-window)
          (switch-to-buffer (cadr claude-buffers))
          (dolist (buf (cddr claude-buffers))
            (let ((new-win (split-window-below)))
              (select-window new-win)
              (switch-to-buffer buf)))
          (balance-windows side-window)))
      (select-window (frame-first-window)))))

(provide 'claude-code-ide-helpers)
;;; claude-code-ide-helpers.el ends here
