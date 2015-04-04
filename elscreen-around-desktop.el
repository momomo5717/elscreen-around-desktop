;;; elscreen-around-desktop.el --- save and restore elscreen tabs synchronously with desktop.el  -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: elscreen desktop frameset
;; Version: 0.1.7
;; Package-Requires: ((emacs "24.4") (elscreen "20140421.414"))
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
;;
;; Usage
;;   (elscreen-start)
;;   (require 'elscreen-around-desktop)
;;   (desktop-save-mode t)
;;   (elscreen-around-desktop-mode t)
;;
;;; Code:

(require 'cl-lib)
(require 'frameset)
(require 'desktop)
(require 'elscreen)

(defgroup elscreen-around-desktop nil
  "ElScreen around desktop -- Save and Restore ElScreen Tab"
  :tag "ElScreen around desktop"
  :group 'elscreen)

(defconst elscreen-around-desktop-version "0.1.7")

(defcustom elsc-desk-filename
  (convert-standard-filename ".elscreen-around-desktop")
  "Basic file name."
  :type 'file
  :group 'elscreen-around-desktop)

(defun elsc-desk-full-file-name (&optional dirname)
  "Return the full name of the elscreen-around-desktop file in DIRNAME.
DIRNAME omitted or nil means use `desktop-dirname'."
  (expand-file-name elsc-desk-filename (or dirname desktop-dirname)))

(defvar elsc-desk-saved-frame-id-configs nil
  "Saved frame-id-configs.")

(defvar elsc-desk--wrote-message-silence t
  "Flag to run message or not when wrote file.")

;;;###autoload
(define-minor-mode elscreen-around-desktop-mode
  "Toggle elscreen-around-desktop-mode."
  :global t
  :group 'elscreen-around-desktop
  (if elscreen-around-desktop-mode
      (elsc-desk--enable-around-desktop)
    (elsc-desk--disable-around-desktop)))

;;; Data Structure :
;;   frame-id-configs = (list frame-id-config ...)
;;   frame-id-config  = (list frame-id (frame-params frame-parameters)
;;                            (screen-property screen-configs) (type value) ...)
;;   screen-configs   = (list screen-config ...) in reverse order of screen-history
;;   screen-config    = (list screen-num (type value) ...)
;;   frame-id         = string
;;   screen-num       = 0 | 1 | ... | n

;;; Variables and functions for converters

(defun elsc-desk--elscreen-get-screen-conf-list (screen type)
  (assoc-default type (elscreen-get-screen-property screen)))

(defun elsc-desk--elscreen-set-screen-conf-list (screen type conf-list)
  ;; If type doesn't exist, the pair of type and conf-list is added.
  (let* ((screen-property (elscreen-get-screen-property screen))
         (assoc (assoc type screen-property)))
    (if assoc (setcdr assoc conf-list)
      (setcdr screen-property (cons (car screen-property) (cdr screen-property)))
      (setcar screen-property (cons type conf-list)))))

(defvar elsc-desk-screen-config-converters
  '((window-configuration
     (lambda (_screen value)
         (elscreen-apply-window-configuration value)
         (window-state-get (frame-root-window (selected-frame)) t))
     (lambda (_screen value)
         (window-state-put value (frame-root-window (selected-frame)) 'safe)
         (elscreen-current-window-configuration)))
    (nickname
     (lambda (_screen value) value)
     (lambda (_screen value) value)))
  "Alist of key and functions to convert a screen configuration.
\(list \(list key write-fun restore-fun\) ... \)
The functions take two args \(screen value\)")

;;;###autoload
(defun elsc-desk-add-to-screen-config-converters
    (key &optional write-fun restore-fun appendp)
  "Add \(KEY WRITE-FUN RESTORE-FUN\) to `elsc-desk-screen-config-converters'.
When APPENDP is non-nil, add to last."
  (let* ((assoc (assoc key elsc-desk-screen-config-converters))
         (write-fun (or write-fun (lambda (_screen v) v)))
         (restore-fun (or restore-fun (lambda (_screen v) v)))
         (converter (list key write-fun restore-fun)))
    (unless assoc
      (if appendp
          (setq elsc-desk-screen-config-converters
                (nconc elsc-desk-screen-config-converters (list converter)))
        (push converter elsc-desk-screen-config-converters)))))

;;;###autoload
(defun elsc-desk-reset-screen-config-converters
    (key &optional write-fun restore-fun)
  "RSET KEY's WRITE-FUN, RESTORE-FUN in `elsc-desk-screen-config-converters'."
  (let ((assoc (assoc key elsc-desk-screen-config-converters))
        (write-fun (or write-fun (lambda (_screen v) v)))
        (restore-fun (or restore-fun (lambda (_screen v) v))))
    (when assoc
      (setcdr assoc (list write-fun restore-fun)))))

;;;###autoload
(defun elsc-desk-delete-from-screen-config-converters (key)
  "Delete the converter of KEY in `elsc-desk-screen-config-converters'."
  (setq elsc-desk-screen-config-converters
        (cl-delete key elsc-desk-screen-config-converters
                   :key #'car)))

(defun elsc-desk-intern-saved-symbol (sym)
  "Intern SYM with elsc-desk-saved- prefix."
  (intern (concat "elsc-desk-saved-" (symbol-name sym))))

(defvar elsc-desk-variable-converters nil
  "Alist of a variable name and functions to save and restore the variable.
\(list \(list name write-fun restore-fun\) ... \)
The functions take no arg.")

;;;###autoload
(defun elsc-desk-add-to-variable-converters
    (name &optional write-fun restore-fun appendp)
    "Add \(NAME WRITE-FUN RESTORE-FUN\) to `elsc-desk-variable-converters'.
When APPENDP is non-nil, add to last."
  (let* ((assoc (assoc name elsc-desk-variable-converters))
         (saved-name (elsc-desk-intern-saved-symbol name))
         (write-fun (or write-fun `(lambda () ,name)))
         (restore-fun (or restore-fun `(lambda () (setq ,name  ,saved-name))))
         (converter (list name write-fun restore-fun)))
    (unless assoc
      (if appendp
          (setq elsc-desk-variable-converters
                (nconc elsc-desk-variable-converters (list converter)))
       (push converter elsc-desk-variable-converters)))))

;;;###autoload
(defun elsc-desk-reset-variable-converters
    (name &optional write-fun restore-fun)
  "RSET NAME's WRITE-FUN, RESTORE-FUN in `elsc-desk-variable-converters'."
  (let* ((assoc (assoc name elsc-desk-variable-converters))
         (saved-name (elsc-desk-intern-saved-symbol name))
         (write-fun (or write-fun `(lambda () ,name)))
         (restore-fun (or restore-fun `(lambda () (setq ,name ,saved-name)))))
    (when assoc
      (setcdr assoc (list write-fun restore-fun)))))

;;;###autoload
(defun elsc-desk-delete-from-variable-converters (name)
  "Delete the converter of NAME in `elsc-desk-variable-converters'."
  (setq elsc-desk-variable-converters
        (cl-delete name elsc-desk-variable-converters :key #'car)))

(defvar elsc-desk-save-before-hook nil
  "Run before getting frame-id-configs.")

(defvar elsc-desk-save-after-hook nil
  "Run after getting frame-id-configs.")

(defvar elsc-desk-restore-before-hook nil
  "Run before restoreing frame-id-configs.")

(defvar elsc-desk-restore-after-hook nil
  "Run after restoreing frame-id-configs.")

;; Functions to save

(defvar frameset--target-display)
(defvar elsc-desk--filtered-params)
(defun elsc-desk--filtered-frame-params (frame)
  "Return filltered `frame-parameters' of FRAME.
To reset `frame-parameters'
after running the function `frame-notice-user-settings'
in `normal-top-level'."
  (let* ((frameset--target-display nil)
         (elsc-desk--filtered-params
          (frameset-filter-params
           (frame-parameters frame) frameset-filter-alist t)))
    (cl-delete "Unprintable entity"
               (eval (read (with-temp-buffer
                             (desktop-outvar 'elsc-desk--filtered-params)
                             (buffer-string))))
               :key #'cdr :test #'equal)))

(defun elsc-desk--writable-screen-config-list (screen screen-property)
  "Convert SCREEN's SCREEN-PROPERTY to writable screen-config-list."
  (cl-loop with als = nil
           for (key write-fn) in elsc-desk-screen-config-converters do
           (push (cons key (funcall write-fn screen (assoc-default key screen-property)))
                 als)
           finally return (cons screen (nreverse als))))

(defun elsc-desk--screen-configs (frame)
  "Return screen-configs of FRAME."
  (let ((selected-fr (selected-frame)))
    (select-frame frame)
    (elscreen-set-window-configuration
     (elscreen-get-current-screen)
     (elscreen-current-window-configuration))
    (prog1
        (elscreen-save-screen-excursion
         (cl-loop for s-num in (reverse (elscreen-get-conf-list 'screen-history))
                  for s-property = (elscreen-get-screen-property s-num)
                  collect (elsc-desk--writable-screen-config-list s-num s-property)))
      (select-frame selected-fr))))

(defun elsc-desk--frame-id-config (frame)
  "Return frame-id-config of FRAME."
  (frameset--set-id frame)
  `(,(frameset-frame-id frame)
    (frame-params ,@(elsc-desk--filtered-frame-params frame))
    (screen-property ,@(elsc-desk--screen-configs frame))))

(defun elsc-desk--frame-id-configs ()
  "Return frame-id-confs."
  (let* ((selected-fr (selected-frame))
         (fr-ls (cons selected-fr
                      (cl-delete selected-fr (mapcar #'car elscreen-frame-confs)))))
    (run-hooks 'elsc-desk-save-before-hook)
    (prog1
        (mapcar (lambda(frame)
                  (elsc-desk--frame-id-config frame))
                fr-ls)
      (select-frame selected-fr)
      (run-hooks 'elsc-desk-save-after-hook))))

(defun elsc-desk--insert-save-values ()
  (insert "\n")
  (cl-loop for (name write-fun) in elsc-desk-variable-converters
           for saved-name = (elsc-desk-intern-saved-symbol name) do
           (set saved-name (funcall write-fun))
           (desktop-outvar saved-name)
           (set saved-name nil)))

(defun elsc-desk--write-frame-id-configs (file)
  "Write frame-id-configs to FILE."
  (let ((fr-id-configs (elsc-desk--frame-id-configs)))
    (with-temp-buffer
      (insert (prin1-to-string `(setq elsc-desk-saved-frame-id-configs
                                      ',fr-id-configs)))
      (elsc-desk--insert-save-values)
      (write-region (point-min) (point-max) file nil 'nomessage))
    (unless elsc-desk--wrote-message-silence
      (message "ElScreen: Wrote %s" (abbreviate-file-name file)))))

;;; Functions to restore

(defvar elsc-desk--restore-error-p)

(defun elsc-desk--set-restored-screen-conf-list (screen als)
  "Restore from SCREEN's ALS  and set screen-conf-list."
  (cl-loop for (key _ restore-fn)
           in elsc-desk-screen-config-converters do
           (condition-case err
            (elsc-desk--elscreen-set-screen-conf-list
             screen key (funcall restore-fn screen (assoc-default key als)))
            (error
             (message "ElScreen: Failed to restore %s: %s"
                      key (error-message-string err))
             (add-to-list 'elsc-desk--restore-error-p key)))))

(defun elsc-desk--restore-screen-configs (screen-configs &optional frame)
  "Restore from SCREEN-CONFIGS to FRAME."
  (let ((fr (if (framep frame) frame (selected-frame)))
        (ls screen-configs) res-ls current-win-conf)
    (select-frame fr)
    (setq current-win-conf (elscreen-current-window-configuration))
    (cl-loop while ls with createdp = t
             for (s-num . s-conf-ls) = (car ls) do
             (cond
              ((or (not (integerp s-num)) (< s-num 0)) (pop ls))
              ((elscreen-screen-live-p s-num)
               (elscreen-goto s-num)
               (elsc-desk--set-restored-screen-conf-list s-num s-conf-ls)
               (pop ls) (push s-num res-ls))
              ((null createdp) (pop ls))
              (t (setq createdp (elscreen-create-internal)))))
    (mapc #'elscreen-kill-internal
             (cl-set-difference (elscreen-get-screen-list) res-ls))
    (unless (elscreen-get-screen-list)
      (elscreen-delete-frame-confs fr)
      (elscreen-make-frame-confs fr)
      (elscreen-apply-window-configuration current-win-conf))))

(defun elsc-desk--restore-frame-id-config (frame-id-config &optional modify-frame-p)
  "Restore from FRAME-ID-CONFIG.
When MODIFY-FRAME-P is non-nil, reset `frame-parameters'."
  (let ((fr (frameset-frame-with-id (car frame-id-config))))
    (when fr
      (select-frame fr)
      (when modify-frame-p
        (modify-frame-parameters fr (assoc-default 'frame-params frame-id-config)))
      (elsc-desk--restore-screen-configs
       (assoc-default 'screen-property frame-id-config) fr))))

(defun elsc-desk--restore-saved-values ()
  (cl-loop for (name _write-fun restore-fun)
           in elsc-desk-variable-converters do
           (condition-case err
               (funcall restore-fun)
             (error
              (message "ElScreen: Failed to restore %s: %s"
                       name (error-message-string err))
              (add-to-list 'elsc-desk--restore-error-p name)))))

(defun elsc-desk--restore-frame-id-configs (frame-id-configs &optional modify-frame-p)
  "Restore from FRAME-ID-CONFIGS.
When MODIFY-FRAME-P is non-nil, reset `frame-parameters'."
  (let ((focus-frame (frameset-frame-with-id (caar frame-id-configs))))
    (run-hooks 'elsc-desk-restore-before-hook)
    (dolist (frame-id-config frame-id-configs)
      (elsc-desk--restore-frame-id-config frame-id-config modify-frame-p))
    (when (framep focus-frame) (select-frame-set-input-focus focus-frame))
    (run-hooks 'elsc-desk-restore-after-hook)
    (elscreen-notify-screen-modification 'force-immediately)))

(defun elsc-desk--set-saved-values-to-nil ()
  (cl-loop for (name) in elsc-desk-variable-converters do
           (set (elsc-desk-intern-saved-symbol name) nil)))

(defun elsc-desk--restore-frame-id-configs-file (file &optional modify-frame-p)
  "Restore from FILE.
When MODIFY-FRAME-P is non-nil, reset `frame-parameters'."
  (if (not (file-exists-p file))
      (message "File not found : %s" file)
    (load file t t t)
    (elsc-desk--restore-saved-values)
    (let (desktop-autosave-was-enabled
          elsc-desk--restore-error-p)
      ;; Temporarily disable the autosave while restoring.
      ;; Not to overwrite the desktop by autosave when restoring fails.
      (setq desktop-autosave-was-enabled
            (memq 'desktop-auto-save-set-timer
                  (default-value 'window-configuration-change-hook)))
      (desktop-auto-save-disable)
      (elsc-desk--restore-frame-id-configs
       elsc-desk-saved-frame-id-configs modify-frame-p)
      (when elsc-desk--restore-error-p
        (error "Elscreen: Failed to restore %s"
               elsc-desk--restore-error-p))
      (when desktop-autosave-was-enabled
        (desktop-auto-save-enable)))
    (setq elsc-desk-saved-frame-id-configs nil)
    (elsc-desk--set-saved-values-to-nil)
    (message "ElScreen: Restored")))

;; Restore at the start of the session

(defvar elsc-desk--done-read-desktop-start-session-p nil)

(defun elsc-desk--set-done-read-desktop-start-session-p ()
  (setq elsc-desk--done-read-desktop-start-session-p t))

(add-hook 'desktop-after-read-hook 'elsc-desk--set-done-read-desktop-start-session-p)

(defun elsc-desk--restore-start-session ()
  "Restore at the start of the session."
  (when (and elscreen-around-desktop-mode
             elsc-desk--done-read-desktop-start-session-p)
    (elsc-desk--restore-frame-id-configs-file (elsc-desk-full-file-name) t))
  (remove-hook 'desktop-after-read-hook 'elsc-desk--set-done-read-desktop-start-session-p))

(add-hook 'window-setup-hook 'elsc-desk--restore-start-session)

;;; Save at the end of the session

(defun elsc-desk--advice-desktop-kill (elsc-desk--origin-fun &rest elsc-desk--args)
  "Save synchronously with `desktop-kill'.
Around advice for `desktop-kill'."
  (let ((prev-dirname desktop-dirname)
        (prev-modtime (nth 5 (file-attributes (desktop-full-file-name))))
        origin-return-obj)

    (setq origin-return-obj (apply elsc-desk--origin-fun elsc-desk--args))

    (when (and elscreen-around-desktop-mode
               (or (not (equal prev-dirname desktop-dirname))
                   (> (float-time (nth 5 (file-attributes (desktop-full-file-name))))
                      (float-time prev-modtime))
                   (and (null prev-modtime) (file-exists-p (desktop-full-file-name)))))
      (elsc-desk--write-frame-id-configs (elsc-desk-full-file-name)))
    origin-return-obj))

;; Emulate auto save defined in desktop.el

(defvar elsc-desk--auto-save-timer nil)

(defvar elsc-desk-auto-save-active-p t)

(defun elsc-desk--advice-desktop-auto-save-enable (&rest _ignore)
  "After advice for `desktop-auto-save-enable'."
  (when (memq 'desktop-auto-save-set-timer
              (default-value 'window-configuration-change-hook))
    (advice-add 'desktop-auto-save-set-timer :after #'elsc-desk--advice-desktop-auto-save-set-timer)))

(advice-add 'desktop-auto-save-enable :after #'elsc-desk--advice-desktop-auto-save-enable)

(defun elsc-desk--advice-desktop-auto-save-disable (&rest _ignore)
  "Before advice for `desktop-auto-save-disable'."
  (advice-remove 'desktop-auto-save-set-timer  #'elsc-desk--advice-desktop-auto-save-set-timer)
  (elsc-desk--cancel-auto-save-timer))

(advice-add 'desktop-auto-save-disable :before #'elsc-desk--advice-desktop-auto-save-disable)

(defmacro elsc-desk--progn-under-silence-wcc-hook (&rest body)
  "Not to run functions in `window-configuration-change-hook' while evaluating BODY."
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

(defun elsc-desk--auto-save ()
  "Emulate `desktop-auto-save'."
  (when (and desktop-save-mode
             elscreen-around-desktop-mode
             elsc-desk-auto-save-active-p
             (integerp desktop-auto-save-timeout)
             (> desktop-auto-save-timeout 0)
             (not desktop-lazy-timer)
             (eq (emacs-pid) (desktop-owner))
             desktop-dirname)
    (elsc-desk--progn-under-silence-wcc-hook
     (elsc-desk--write-frame-id-configs (elsc-desk-full-file-name))
     (elsc-desk--cancel-auto-save-timer))))

(defun elsc-desk--auto-save-set-timer ()
  (elsc-desk--cancel-auto-save-timer)
  (when (and (integerp desktop-auto-save-timeout)
             (> desktop-auto-save-timeout 0))
    (setq elsc-desk--auto-save-timer
          (run-with-idle-timer desktop-auto-save-timeout nil 'elsc-desk--auto-save))))

(defun elsc-desk--advice-desktop-auto-save-set-timer (&rest _ignore)
  "After advice for `desktop-auto-save-set-timer'."
  (elsc-desk--auto-save-set-timer))

(defun elsc-desk--cancel-auto-save-timer ()
  (when elsc-desk--auto-save-timer
    (cancel-timer elsc-desk--auto-save-timer)
    (setq elsc-desk--auto-save-timer nil)))

;;; Emulate interactive functions defined desktop.el

;;;###autoload
(defun elscreen-desktop-clear ()
  "Emulate `desktop-clear'."
  (interactive)
  (desktop-lazy-abort)
  (dolist (fr (frame-list))
    (unless (frame-parameter fr 'desktop-dont-clear)
      (elscreen-delete-frame-confs fr)
      (elscreen-make-frame-confs fr)))
  (if (called-interactively-p 'interactive)
      (call-interactively 'desktop-clear)
    (desktop-clear)))

;;;###autoload
(defun elscreen-desktop-save (&optional dirname release only-if-changed)
  "Emulate `desktop-save'."
  (interactive)
  (let ((current-desktop-file-modtime desktop-file-modtime))
   (if (called-interactively-p 'interactive)
       (call-interactively 'desktop-save)
     (desktop-save dirname release only-if-changed))
   (unless (equal current-desktop-file-modtime desktop-file-modtime)
    (elsc-desk--write-frame-id-configs (elsc-desk-full-file-name dirname)))))

;;;###autoload
(defun elscreen-desktop-save-in-desktop-dir ()
  "Emulate `desktop-save-in-desktop-dir'."
  (interactive)
  (if desktop-dirname
      (elscreen-desktop-save desktop-dirname)
    (call-interactively 'elscreen-desktop-save))
  (message "Desktop saved in %s" (abbreviate-file-name desktop-dirname)))

(defun elsc-desk--restore-after-desktop-read (&rest _ignore)
  "Restore elscreen configurations in `desktop-after-read-hook'."
  (elsc-desk--restore-frame-id-configs-file (elsc-desk-full-file-name)))

;;;###autoload
(defun elscreen-desktop-read (&optional dirname)
  "Emulate `desktop-read'."
  (interactive)
  (unwind-protect
      (progn
        (add-hook 'desktop-after-read-hook 'elsc-desk--restore-after-desktop-read)
        (desktop-read dirname))
    (remove-hook 'desktop-after-read-hook 'elsc-desk--restore-after-desktop-read)))

;;;###autoload
(defun elscreen-desktop-change-dir (dirname)
  "Emulate `desktop-change-dir'."
  (interactive "DChange to directory: ")
  (setq dirname (file-name-as-directory (expand-file-name dirname desktop-dirname)))
  (unless (file-exists-p dirname)
    (error "No directory: %s" (abbreviate-file-name dirname)))
  (desktop-kill)
  (elscreen-desktop-clear)
  (elscreen-desktop-read dirname))

;;;###autoload
(defun elscreen-desktop-remove ()
  "Emulate `desktop-remove'."
  (interactive)
  (when desktop-dirname
    (let ((filename (elsc-desk-full-file-name)))
      (when (file-exists-p filename)
        (delete-file filename))))
  (desktop-remove))

;;;###autoload
(defun elscreen-desktop-revert ()
  "Emulate `desktop-revert'."
  (interactive)
  (unwind-protect
      (progn
        (add-hook 'desktop-after-read-hook 'elsc-desk--restore-after-desktop-read)
        (desktop-revert))
    (remove-hook 'desktop-after-read-hook 'elsc-desk--restore-after-desktop-read)))

;; Functions for minor mode
(defun elsc-desk--enable-around-desktop ()
  (advice-add 'desktop-kill :around #'elsc-desk--advice-desktop-kill))

(defun elsc-desk--disable-around-desktop ()
  (advice-remove 'desktop-kill #'elsc-desk--advice-desktop-kill))

(provide 'elscreen-around-desktop)
;;; elscreen-around-desktop.el ends here
