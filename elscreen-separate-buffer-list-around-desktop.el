;;; elscreen-separate-buffer-list-around-desktop.el --- An example of setting for elscreen-around-desktop.el -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

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
;;
;;   (elscreen-start)
;;   (require 'elscreen-separate-buffer-list)
;;   (require 'elscreen-around-desktop)
;;   (require 'elscreen-separate-buffer-list-around-desktop)
;;   (elscreen-separate-buffer-list-mode 1)
;;   (desktop-save-mode 1)
;;   (elscreen-around-desktop-mode 1)
;;
;;; Code:

(require 'elscreen-separate-buffer-list)
(require 'elscreen-around-desktop)

;; Functions to convert separate-buffer-list

(defun elsc-desk--writable-separate-buffer-list (_screen value)
  "Convert separate-buffer-list to be writable."
  (cl-loop for buf in value
           for buf-name = (buffer-name buf)
           when buf-name collect buf-name))

(defun elsc-desk--restore-separate-buffer-list (_screen value)
  "Restore separate-buffer-list from buffer name lis."
  (setq esbl-separate-buffer-list
        (cl-loop for buf-name in value with buf
                 for buf = (and (stringp buf-name) (get-buffer buf-name))
                 when buf collect buf)))

(elsc-desk-add-to-screen-config-converters
 'separate-buffer-list
 'elsc-desk--writable-separate-buffer-list
 'elsc-desk--restore-separate-buffer-list)

(add-hook 'elsc-desk-save-before-hook 'esbl-save-separate-buffer-list)

(add-hook 'elsc-desk-restore-after-hook 'esbl-restore-separate-buffer-list)

;; Functions to convert esbl-separate-buffer-count-list
(defun elsc-desk--writable-esbl-separate-buffer-count-list ()
  "Convert `esbl-separate-buffer-count-list' to be writable."
  (cl-loop for (buf . count) in esbl-separate-buffer-count-list
           when (buffer-live-p buf)
           collect (cons (buffer-name buf) count)))

(defun elsc-desk--restore-esbl-separate-buffer-count-list ()
  "Restore `esbl-separate-buffer-count-list' from `elsc-desk-saved-esbl-separate-buffer-count-list'."
  ;; The variable is saved with elsc-desk-saved- prefix.
  (let ((saved-symbol-name
         (elsc-desk-intern-saved-symbol 'esbl-separate-buffer-count-list)))
   (setq esbl-separate-buffer-count-list
         (cl-loop for (name . count) in (symbol-value saved-symbol-name)
                  for buf = (and (stringp name) (get-buffer name))
                  when buf collect (cons buf count)))))

(elsc-desk-add-to-variable-converters
 'esbl-separate-buffer-count-list
 'elsc-desk--writable-esbl-separate-buffer-count-list
 'elsc-desk--restore-esbl-separate-buffer-count-list)

;; Functions to convert separate-window-history

(defun elsc-desk--esbl-get-all-window-history-alist ()
  "Override `esbl-get-all-window-history-alist' to ensure the cyclic ordering of windows."
  (when (esbl-window-history-supported-p)
    (let ((all-window-history-alist))
      (walk-windows
       (lambda (window)
         (let ((prevs (window-prev-buffers window))
               (nexts (window-next-buffers window)))
           (push (cons window (cons prevs nexts))
                 all-window-history-alist)))
       nil (window-frame))
      (nreverse all-window-history-alist))))

(advice-add 'esbl-get-all-window-history-alist :override
            #'elsc-desk--esbl-get-all-window-history-alist)

(defun elsc-desk--writable-separate-window-history (_screen value)
  "Convert separate-window-history to be writable."
  (cl-labels
      ((writable-entrys (entrys)
          (cl-loop for (buf s p) in entrys
                   when (buffer-live-p buf)
                   collect (list (buffer-name buf)
                                 (marker-position s)
                                 (marker-position p)))))
    (cl-loop for (_win . (prevs . nexts)) in value
             collect (cons (writable-entrys prevs)
                           (writable-entrys nexts)))))

(defun elsc-desk--restore-separate-window-history (_screen value)
  "Restore separate-window-history."
  (cl-labels
      ((restore-entrys (saved-entrys)
          (cl-loop for (buf-name s p) in saved-entrys
                   for buf = (and (stringp buf-name)
                                  (integerp s)
                                  (integerp p)
                                  (get-buffer buf-name))
                   when buf collect (list buf
                                          (set-marker (make-marker) s buf)
                                          (set-marker (make-marker) p buf)))))
    (let ((saved-history-ls value)
          win-history-als histories prevs nexts)
      (walk-windows
       (lambda (win)
         (setq histories (pop saved-history-ls)
               prevs (restore-entrys (car histories))
               nexts (restore-entrys (cdr histories)))
         (push (cons win (cons prevs nexts)) win-history-als)
         (set-window-prev-buffers win prevs)
         (set-window-next-buffers win nexts))
       nil (window-frame))
      (nreverse win-history-als))))

(elsc-desk-add-to-screen-config-converters
 'separate-window-history
 'elsc-desk--writable-separate-window-history
 'elsc-desk--restore-separate-window-history
 ;; APPENDP is t because it uses window tree to restore.
 t)

(add-hook 'elsc-desk-save-before-hook 'esbl-save-separate-window-history)

(add-hook 'elsc-desk-restore-after-hook 'esbl-restore-separate-window-history)

(provide 'elscreen-separate-buffer-list-around-desktop)
;;; elscreen-separate-buffer-list-around-desktop.el ends here
