;;; helm.el --- Emacs incremental and narrowing framework -*- lexical-binding: t -*-

;; Copyright (C) 2016 Sameer Sahasrabuddhe <sameer@sbuddhe.net>

;; Author: Sameer Sahasrabuddhe <sameer@sbuddhe.net>
;; Created: 11 September 2016

;; Homepage: http://github.com/sameerds/helm-modeless

;; Version: 0.1

;; Keywords: convenience matching

;; Package-Requires: ((helm "2.1.0"))

;; This file is not part of GNU Emacs.

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

;; This package is an attempt at radically reinterpreting the
;; operation of `helm', the coolest Emacs framework for incremental
;; completion and narrowing selections. It allows a user to continue
;; accessing candidates from the last `helm' session that are still
;; cached in the `helm-buffer' created by that session.
;;
;; Example: Pressing "M-g n" after a successful `helm-grep' or
;; `helm-occur' session should take the user to the next candidate
;; with no other interaction.
;;
;; One can think of a typical `helm' interaction as centering around a
;; special buffer called the `helm-buffer', which contains the current
;; set of possible candidates. A typical `helm' session is modal in
;; nature --- the user's focus is trapped within the `helm' buffer while
;; they execute commands to modify the set of candidates or perform
;; persistent actions in them.
;;
;; There are two ways for a user to exit a `helm' session:
;; 1. Either quit the session and return to the `helm-source-buffer'
;; 2. Or choose a candidate and perform an action on it that exits the
;;    `helm' session.
;;
;; But the `helm-buffer' created for that session is still present,
;; and can be used to start a new `helm' session using the command
;; `helm-resume'. This package is an attempt to use that buffer to
;; interact with the listed candidates without starting a new
;; session. Hence the name `helm-modeless' --- the user's focus
;; remains in their own buffer, and is not captured by a `helm' session.
;;
;; As a simple example, the plain `grep' and `occur' commands are
;; modeless. When the user selects an occurence, the `*grep*' or
;; `*occur*' stays out of the way until it is recalled by
;; `compilation-minor-mode' commands `next-error' ("M-g n") or
;; `previous-error' ("M-g n"). `helm-modeless' enables the same
;; behaviour for `helm', by interacting with the last `helm-buffer'
;; without actually starting a `helm' session.

;;; Code:

;; FIXME: This should ideally be declared in `helm-grep.el'
(defvar helm-modeless-grep-after-init-hook)

;; FIXME: This will not be necessary if the `helm-grep' source was
;; initialized with an appropriate hook.
(defmethod helm-setup-user-source ((source helm-grep-class))
  (setf (slot-value source 'after-init-hook) helm-modeless-grep-after-init-hook))

(defun helm-modeless-mark-current-line ()
  "Like `helm-mark-current-line', but modeless!"
  (move-overlay helm-selection-overlay
                (point-at-bol) (1+ (point-at-eol)))
  (setq helm-selection-point (overlay-start helm-selection-overlay)))

(defun helm-modeless-grep-jump (arg rst)
  "Modelessly move the current selection in the `helm-buffer'.
Suitable for use as `next-error-function'"
  ;; TODO: Handle the `rst' argument.
  (interactive)
  (let ((orig-helm-buffer helm-buffer))
    (unwind-protect
        (let ((helm-buffer (current-buffer)))
          (funcall helm-display-function helm-buffer)
          ;; If window is not dedicated, it sticks around after a
          ;; successful exit from `helm-resume'.
          (set-window-dedicated-p (get-buffer-window helm-buffer) t)
          (forward-line arg)
          (helm-skip-noncandidate-line (cl-ecase arg
                                         (1 'next)
                                         (-1 'previous)))
          (helm-modeless-mark-current-line)
          (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
            (helm-grep-action candidate)))
      (setq helm-buffer orig-helm-buffer))))

(defun helm-modeless-enable-compilation-minor-mode ()
  (with-helm-buffer
    (setq next-error-function 'helm-modeless-grep-jump)))

(add-hook 'helm-modeless-grep-after-init-hook 'helm-modeless-enable-compilation-minor-mode)

(defun helm-modeless-set-compilation-last-buffer ()
  "Register `helm-buffer' as the last known compilation buffer.
But only if `helm-buffer' has suitable commands for `compilation-minor-mode'."
  (with-helm-buffer
    (when (compilation-buffer-p (current-buffer))
      (setq compilation-last-buffer (current-buffer)))))

(add-hook 'helm-after-action-hook 'helm-modeless-set-compilation-last-buffer)

(provide 'helm-modeless)

;;; helm-modeless ends here
