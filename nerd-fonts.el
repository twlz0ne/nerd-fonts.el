;;; nerd-fonts.el --- Nerd font utility -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/03/16
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/nerd-fonts.el
;; Keywords: utilities

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

;; Provides nerd-fonts (https://github.com/ryanoasis/nerd-fontss) utilities.

;;; Change Log:

;;  0.1.0  2019/03/16  Initial version.

;;; Code:

(require 'nerd-fonts-data)

(declare-function helm "helm")
(declare-function helm-build-sync-source "helm")
(declare-function ivy-read "ivy")

(defun nerd-fonts--propertize (glyph)
  (propertize glyph
              'face '(:family "Hack Nerd Font" :height 1.5)))

(defun nerd-fonts--construct-candidates ()
  (mapcar (lambda (nerd-fonts)
            (cons (concat (car nerd-fonts)
                          " -> "
                          (nerd-fonts--propertize
                           (cdr nerd-fonts)))
                  (cdr nerd-fonts)))
          nerd-fonts-alist))

(defun nerd-fonts--completing-read ()
  (let* ((comp-func (if ido-mode 'ido-completing-read 'completing-read))
         (selection (funcall comp-func "pattern: "
                            (nerd-fonts--construct-candidates) nil t)))
    (insert (replace-regexp-in-string "\\`[^>]*> " "" selection))))

(defmacro nerd-fonts--ivy-save-action (&rest body)
  "Save the `ivy--actions-list'; execute BODY; resotre the `ivy--actions-list'."
  (declare (indent defun) (debug t))
  `(let* ((old-actions ivy--actions-list)
          (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (setq ivy--actions-list old-actions))))

(defun nerd-fonts--helm-read ()
  (require 'helm)
  (helm :sources
    (helm-build-sync-source "Nerd Fonts"
      :candidates (nerd-fonts--construct-candidates)
      :action
      '(("Insert" . (lambda (candidate)
                      (insert (nerd-fonts--propertize candidate))))
        ("Copy"   . (lambda (candidate)
                      (let ((icon (nerd-fonts--propertize candidate)))
                        (kill-new (format "%s" icon))
                        (message "Copied: %s" icon)))))
      :candidate-number-limit 9999)))

(defun nerd-fonts--ivy-read ()
  (require 'ivy)
  (nerd-fonts--ivy-save-action
   (let* ((actions '(("i" (lambda (candidate)
                           (insert (cdr candidate))) "insert")
                     ("w" (lambda (candidate)
                            (let ((icon (cdr candidate)))
                              (kill-new (format "%s" icon))
                              (message "Copied: %s" icon))) "copy")))
          (ivy--actions-list (plist-put nil t `(,(car actions)))))
     (ivy-read "pattern> " (nerd-fonts--construct-candidates)
               :action `(1 ,@actions)))))

;;;###autoload
(defun nerd-fonts (icon-name)
  "Return code point of ICON-NAME.

Or insert it into buffer while called interactivelly."
  (interactive
   (list
    (cond ((and (featurep 'helm) helm-mode) (nerd-fonts--helm-read))
          ((and (featurep 'ivy) ivy-mode) (nerd-fonts--ivy-read))
          (t (nerd-fonts--completing-read)))))
  (unless (called-interactively-p 'any)
    (let ((reg (format "\\`\\(\\|.*\s\\)%s\\(\\|\s.*\\)\\'" icon-name)))
      (assoc-default reg nerd-fonts-alist
                     (lambda (str reg)
                       (string-match-p reg str))))))

(provide 'nerd-fonts)

;;; nerd-fonts.el ends here
