;;; helm-modeless.el --- Bind next-error and previous-error in helm

;; Copyright (C) 2016-2017 Sameer Sahasrabuddhe

;; Author: Sameer Sahasrabuddhe <sameer@sbuddhe.net>
;; Created: 11 September 2016
;; Version: 0.1
;; Keywords: convenience matching
;; Homepage: http://github.com/sameerds/helm-modeless
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


(defun helm-modeless-mark-current-line ()
  "Like `helm-mark-current-line', but modeless!"
  (move-overlay helm-selection-overlay
                (point-at-bol) (1+ (point-at-eol)))
  (setq helm-selection-point (overlay-start helm-selection-overlay)))

(defun helm-modeless-action (action arg rst)
  "Modelessly move the current selection in a `helm' buffer and perform action."
  ;; TODO: Handle the `rst' argument.
  (interactive)
  (setq helm-buffer (current-buffer))
  (with-selected-window (display-buffer (current-buffer))
    (forward-line arg)
    (helm-skip-noncandidate-line (cl-ecase arg
                                   (1 'next)
                                   (-1 'previous)))
    (move-overlay helm-selection-overlay
                  (point-at-bol) (1+ (point-at-eol)) (current-buffer))
    (setq helm-selection-point (overlay-start helm-selection-overlay))
    (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
      (funcall action candidate))))

(defun helm-modeless-set-compilation-last-buffer ()
  "Register `helm-buffer' as the last known compilation buffer.
But only if `helm-buffer' has suitable commands for `compilation-minor-mode'."
  (with-helm-buffer
    (when (next-error-buffer-p (current-buffer))
      (display-buffer (current-buffer))
      (setq next-error-last-buffer (current-buffer)))))

(add-hook 'helm-after-action-hook 'helm-modeless-set-compilation-last-buffer)


;; Modeless actions for helm-grep
;;
;;

;; FIXME: This will not be necessary if the `helm-grep' source was
;; initialized with an appropriate hook.
(defmethod helm-setup-user-source ((source helm-grep-class))
  (setf (slot-value source 'after-init-hook)
        (lambda ()
          (with-helm-buffer
            (setq next-error-function 'helm-modeless-grep-jump)))))

(defun helm-modeless-grep-jump (arg rst)
  "Modelessly perform grep action in the `*helm\ grep*' buffer.
Suitable for use as `next-error-function'"
  (helm-modeless-action 'helm-grep-other-window arg rst))


;; Modeless actions for helm-occur
;;
;;

;; FIXME: This will not be necessary if the `helm-occur' source was
;; initialized with an appropriate hook.
(defmethod helm-setup-user-source ((source helm-source-multi-occur))
  (setf (slot-value source 'after-init-hook)
        (lambda ()
          (with-helm-buffer
            (setq next-error-function 'helm-modeless-occur-jump)))))

(defun helm-modeless-occur-jump (arg rst)
  "Modelessly perform grep action in the `*helm\ occur*' buffer.
Suitable for use as `next-error-function'"
  (helm-modeless-action 'helm-moccur-goto-line-ow arg rst))

(provide 'helm-modeless)

;;; helm-modeless.el ends here
