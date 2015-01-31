;;; elscreen-around-desktop.el --- store and restore elscreen tabs synchronously with desktop.el

;; Copyright (C) 2015 momomo5717

;; Keywords: elscreen desktop frameset
;; Version: 0.1.4
;; Package-Requires: ((cl-lib "1.0") (frameset "1") (desktop "206") (elscreen "20140421.414"))
;; URL: https://github.com/momomo5717/elscreen-around-desktop

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

;;; Commentary:
;; Tested on Emacs 24.4.1 (emacs-mac-app 5.2_0)
;;
;; Usage
;;   (elscreen-start)
;;   (desktop-save-mode t)
;;   (require 'elscreen-around-desktop)
;;   (elscreen-around-desktop)
;;
;;   You can use M-x elscreen-around-desktop-mode-off, if you want to off the mode.
;;
;;; Code:

(require 'cl-lib)
(require 'frameset)
(require 'desktop)
(require 'elscreen)

(defgroup elscreen-around-desktop nil
  "ElScreen around desktop -- Store and Restore ElScreen Tab"
  :tag "ElScreen around desktop"
  :group 'elscreen)

(defconst elscreen-around-desktop-version "0.1.5")

(defcustom elsc-desk:filename
  (convert-standard-filename ".elscreen-around-desktop")
  "Basic file name"
  :type 'file
  :group 'elscreen-around-desktop)

(defvar elsc-desk:tmp-stored-frame-id-configs nil
  "Saved frame-id-configs")

(defvar elsc-desk:wrote-message-silence t
  "Flag to run message or not when wrote file")

(define-minor-mode elscreen-around-desktop-mode
  "Toggle elscreen-around-desktop-mode."
  :global t
  :group 'elscreen-around-desktop
  (if elscreen-around-desktop-mode
      (elsc-desk:enable-around-desktop)
    (elsc-desk:disable-around-desktop)))

;; Data Structure :
;;   frame-id-configs  = (list frame-id-config ...)
;;   frame-id-config   = (list frame-id frame-params screen-configs)
;;   screen-configs = (list screen-config ...) in reverse order of screen-history
;;   screen-config  = (list screen-num window-state nickname)
;;   frame-id       = string
;;   screen-num     = 0 | 1 | ... | n
;;   frame-params   = frame-parameters
;;   window-state   = writable windwo-state from window-state-get
;;   nickname       = string | nil

;; Functions to store

(defun elsc-desk:filtered-frame-params (frame)
  "Return filltered frame-parameters to reset frame-parameters after frame-notice-user-settings
between emacs-startup-hook window-setup-hook in normal-top-level."
  (let ((frameset--target-display nil))
    (frameset-filter-params (frame-parameters frame) frameset-filter-alist t)))

(defun elsc-desk:screen-configs (frame)
  (let ((now-fr (selected-frame)))
    (select-frame frame)
    (elscreen-set-window-configuration
     (elscreen-get-current-screen)
     (elscreen-current-window-configuration))
    (cl-loop for s-num in (reverse (elscreen-get-conf-list 'screen-history)) do
             (elscreen-apply-window-configuration
              (elscreen-get-window-configuration s-num))
             collect (list s-num
                           (window-state-get (frame-root-window frame) t)
                           (elscreen-get-screen-nickname s-num))
             finally (select-frame now-fr))))

(defun elsc-desk:frame-id-configs ()
  "Return frame-confs"
  (let* ((now-fr (selected-frame))
         (registered-fr-ls (mapcar #'car elscreen-frame-confs))
         (fr-ls
          (cons (cl-find-if (lambda (fr)(eq fr now-fr)) registered-fr-ls)
                (cl-remove-if (lambda (fr)(eq fr now-fr)) registered-fr-ls))))
    (mapcar (lambda(frame)
              (frameset--set-id frame)
              (list (frameset-frame-id frame)
                    (elsc-desk:filtered-frame-params frame)
                    (elsc-desk:screen-configs frame)))
            fr-ls)))

(defun elsc-desk:write-frame-id-configs (file)
  "Write frame-id-configs to file"
  (let ((fr-id-configs (elsc-desk:frame-id-configs)))
    (with-temp-file file
      (insert (prin1-to-string `(setq elsc-desk:tmp-stored-frame-id-configs
                                      ',fr-id-configs))))
    (unless elsc-desk:wrote-message-silence
     (message (format "Wrote elsc-desk:frame-id-configs :%s" file)))))

;; Functions to restore

(defun elsc-desk:restore-screen-configs (screen-configs &optional frame)
  "Restore from (list (list screen-num window-state nickname) ...)"
  (let ((fr (if (framep frame) frame (selected-frame)))
        (ls screen-configs) res-ls current-conf)
    (select-frame fr)
    (setq current-conf (elscreen-current-window-configuration))
    (cl-loop when (null ls) return nil
             with createdp = t
             for s-config = (car ls) for s-num = (car s-config) do
             (cond
              ((or (not (integerp s-num)) (< s-num 0)) (pop ls))
              ((elscreen-screen-live-p s-num)
               (elscreen-goto s-num)
               (window-state-put
                (cl-second s-config) (frame-root-window fr) 'safe)
               (when (stringp (cl-third s-config))
                 (elscreen-screen-nickname (cl-third s-config)))
               (pop ls) (push s-num res-ls))
              ((null createdp) (pop ls))
              (t (setq createdp (elscreen-create-internal)))))
    (cl-mapc #'elscreen-kill-internal
             (cl-set-difference (elscreen-get-screen-list) res-ls))
    (unless (elscreen-get-screen-list)
      (elscreen-delete-frame-confs fr)
      (elscreen-make-frame-confs fr)
      (elscreen-apply-window-configuration current-conf))
    (elscreen-notify-screen-modification 'force-immediately)))

(defun elsc-desk:restore-frame-id-config (frame-id-config)
  "Restore from (list frame-id frame-params screen-configs)"
  (let ((fr (frameset-frame-with-id (car frame-id-config))))
    (when fr
      (select-frame fr)
      (modify-frame-parameters fr (cl-second frame-id-config))
      (elsc-desk:restore-screen-configs (cl-third frame-id-config) fr))))

(defun elsc-desk:restore-frame-id-configs (frame-id-configs)
  "Restore from frame-id-configs"
  (let ((focus-frame (frameset-frame-with-id (caar frame-id-configs))))
    (cl-mapc #'elsc-desk:restore-frame-id-config frame-id-configs)
    (when (framep focus-frame) (select-frame-set-input-focus focus-frame))))

(defun elsc-desk:restore-frame-id-configs-file (file)
  "Restore from a config file."
  (if (not (file-exists-p file))
      (message (format "File not found : %s" file))
    (load file)
    (elsc-desk:restore-frame-id-configs elsc-desk:tmp-stored-frame-id-configs)
    (setq elsc-desk:tmp-stored-frame-id-configs nil)
    (message "Done elsc-desk:restore-frame-id-configs-file")))

;; Restore at the start of the session

(defvar elsc-desk:done-read-desktop-start-session-p nil)

(defun elsc-desk:set-done-read-desktop-start-session-p ()
  (setq elsc-desk:done-read-desktop-start-session-p t))

(add-hook 'desktop-after-read-hook 'elsc-desk:set-done-read-desktop-start-session-p)

(defun elsc-desk:restore-start-session ()
  "Restore at the start of the session."
  (when (and elscreen-around-desktop-mode
             elsc-desk:done-read-desktop-start-session-p)
    ;; Temporarily disable the autosave like desktop-read
    (desktop-auto-save-disable)
    (elsc-desk:restore-frame-id-configs-file
     (expand-file-name elsc-desk:filename  desktop-dirname))
    (desktop-auto-save-enable))
  (remove-hook 'desktop-after-read-hook 'elsc-desk:set-done-read-desktop-start-session-p))

(add-hook 'window-setup-hook 'elsc-desk:restore-start-session)

;; Store at the end of the session

(defun elsc-desk:advice-desktop-kill (elsc-desk:origin-fun &rest elsc-desk:args)
  "Store synchronously with desktop-kill"
  (let ((prev-dirname desktop-dirname)
        (prev-modtime (nth 5 (file-attributes (desktop-full-file-name))))
        origin-return-obj)

    (setq origin-return-obj (apply elsc-desk:origin-fun elsc-desk:args))

    (when (and elscreen-around-desktop-mode
               (or (not (equal prev-dirname desktop-dirname))
                   (> (float-time (nth 5 (file-attributes (desktop-full-file-name))))
                      (float-time prev-modtime))))
      (elsc-desk:write-frame-id-configs
       (expand-file-name elsc-desk:filename desktop-dirname)))
    origin-return-obj))

;; Emulate auto save written in desktop.el

(defvar elsc-desk:auto-store-timer nil)

(defvar elsc-desk:auto-store-activep t)

(defun elsc-desk:advice-desktop-auto-save-enable (&rest _ignore)
  (when (member 'desktop-auto-save-set-timer
                (default-value 'window-configuration-change-hook))
    (advice-add 'desktop-auto-save-set-timer :after #'elsc-desk:advice-auto-save-set-timer)))

(advice-add 'desktop-auto-save-enable :after #'elsc-desk:advice-desktop-auto-save-enable)

(defun elsc-desk:advice-desktop-auto-save-disable (&rest _ignore)
  (advice-remove 'desktop-auto-save-set-timer  #'elsc-desk:advice-auto-save-set-timer)
  (elsc-desk:cancel-auto-store-timer))

(advice-add 'desktop-auto-save-disable :before #'elsc-desk:advice-desktop-auto-save-disable)

(defmacro elsc-desk:progn-under-silence-wcc-hook (&rest body)
  "Not to run functions in window-configuration-change-hook while evaluating."
  (let ((tmp-local-wcc-hook-alist (cl-gensym "tmp-local"))
        (tmp-global-wcc-hook (cl-gensym "tmp-global")))
    `(let ((,tmp-local-wcc-hook-alist
            (save-excursion
              (cl-loop for buf in (buffer-list) with ls do
                    (set-buffer buf)
                    when (local-variable-p 'window-configuration-change-hook)
                      do (push (cons buf window-configuration-change-hook) ls)
                    finally return ls)))
           (,tmp-global-wcc-hook (default-value 'window-configuration-change-hook)))
       (unwind-protect
           (progn
             (save-excursion
               (dolist (buf-wcc ,tmp-local-wcc-hook-alist)
                 (set-buffer (car buf-wcc))
                 (setq window-configuration-change-hook nil)))
             (setq-default window-configuration-change-hook nil)
             ,@body)
         (save-excursion
           (dolist (buf-wcc ,tmp-local-wcc-hook-alist)
             (set-buffer (car buf-wcc))
             (setq window-configuration-change-hook (cdr buf-wcc))))
         (setq-default window-configuration-change-hook ,tmp-global-wcc-hook)))))

(defun elsc-desk:auto-store ()
  "Store synchronously with desktop-auto-save."
  (when (and desktop-save-mode
             elscreen-around-desktop-mode
             elsc-desk:auto-store-activep
             (integerp desktop-auto-save-timeout)
             (> desktop-auto-save-timeout 0)
             (not desktop-lazy-timer)
             (eq (emacs-pid) (desktop-owner))
             desktop-dirname)
    (elsc-desk:progn-under-silence-wcc-hook
     (elsc-desk:write-frame-id-configs
      (expand-file-name elsc-desk:filename desktop-dirname))
     (elsc-desk:cancel-auto-store-timer))))

(defun elsc-desk:auto-store-set-timer ()
  (elsc-desk:cancel-auto-store-timer)
  (when (and (integerp desktop-auto-save-timeout)
             (> desktop-auto-save-timeout 0))
    (setq elsc-desk:auto-store-timer
          (run-with-idle-timer desktop-auto-save-timeout nil 'elsc-desk:auto-store))))

(defun elsc-desk:advice-auto-save-set-timer (&rest _ignore)
  (elsc-desk:auto-store-set-timer))

(defun elsc-desk:cancel-auto-store-timer ()
  (when elsc-desk:auto-store-timer
    (cancel-timer elsc-desk:auto-store-timer)
    (setq elsc-desk:auto-store-timer nil)))

;; Emulate interactive functions of desktop.el

;;;###autoload
(defun elscreen-desktop-clear ()
  "Emulate desktop-clear."
  (interactive)
  (when (called-interactively-p)
    (call-interactively 'desktop-clear)
    (desktop-clear))
  (let ((now-fr (selected-frame)))
   (dolist (fr (frame-list))
     (select-frame fr)
     (cl-mapc #'elscreen-kill-internal (elscreen-get-screen-list))
     (elscreen-delete-frame-confs fr)
     (elscreen-make-frame-confs fr 'keep))
   (select-frame now-fr)))

;;;###autoload
(defun elscreen-desktop-save (&optional dirname release only-if-changed)
  "Save elscreen configrations with desktop-save."
  (interactive)
  (cond
   ((interactive-p)
    (call-interactively 'desktop-save)
    (elsc-desk:write-frame-id-configs
     (expand-file-name elsc-desk:filename desktop-dirname)))
   ((and (stringp dirname) (file-exists-p dirname))
    (desktop-save dirname release only-if-changed)
    (elsc-desk:write-frame-id-configs
     (expand-file-name elsc-desk:filename dirname)))
   (t (message (format "File not found : %s" dirname)))))

(defun elsc-desk:restore-after-desktop-read (&rest _ignore)
  "Restore elscreen configurations in desktop-after-read-hook"
  (elsc-desk:restore-frame-id-configs-file
   (expand-file-name elsc-desk:filename desktop-dirname)))

;;;###autoload
(defun elscreen-desktop-read (&optional dirname)
  "Emulate desktop-read."
  (interactive)
  (unwind-protect
      (progn
        (add-hook 'desktop-after-read-hook 'elsc-desk:restore-after-desktop-read)
        (desktop-read dirname))
    (remove-hook 'desktop-after-read-hook 'elsc-desk:restore-after-desktop-read)))

;; Functions for minor mode
(defun elsc-desk:enable-around-desktop ()
  (advice-add 'desktop-kill :around #'elsc-desk:advice-desktop-kill)
  (add-hook 'desktop-after-read-hook 'elsc-desk:restore-after-desktop-read))

(defun elsc-desk:disable-around-desktop ()
  (advice-remove 'desktop-kill #'elsc-desk:advice-desktop-kill)
  (remove-hook 'desktop-after-read-hook 'elsc-desk:restore-after-desktop-read))

;;;###autoload
(defun elscreen-around-desktop-mode-off ()
  "Turn off elscreen-around-desktop-mode."
  (interactive)
  (elscreen-around-desktop-mode 0))

(defun elscreen-around-desktop-start ()
  "Start elscreen-around-desktop-mode."
  (elscreen-around-desktop-mode t)
  (when desktop-save-mode (elsc-desk:advice-desktop-auto-save-enable)))

(provide 'elscreen-around-desktop)
;;; elscreen-around-desktop.el ends here
