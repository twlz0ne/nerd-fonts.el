;;; nerd-fonts.el --- Test for Nerd font utility -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'nerd-fonts)

(defvar test-helm-nerd-fonts-insert nil)

(defun nerd-fonts--helm-read@ad-override ()
  (require 'helm)
  (let ((helm--maybe-use-default-as-input t))
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
        :candidate-number-limit 9999)
      :default test-helm-nerd-fonts-insert))
  (unload-feature 'helm))

(ert-deftest test-helm-nerd-fonts-default ()
  (should
   (equal
    "ﬦ"
    (with-temp-buffer
      (let ((test-helm-nerd-fonts-insert "mdi-lambda"))
        (test-with '(funcall 'nerd-fonts--helm-read@ad-override) "RET"))
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-helm-nerd-fonts-insert ()
  (should
   (equal
    "ﬦ"
    (with-temp-buffer
      (let ((test-helm-nerd-fonts-insert "mdi-lambda"))
        (test-with '(funcall 'nerd-fonts--helm-read@ad-override) "TAB RET"))
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-helm-nerd-fonts-copy ()
  (should
   (equal
    "ﬦ"
    (with-temp-buffer
      (let ((test-helm-nerd-fonts-insert "mdi-lambda"))
        (test-with '(funcall 'nerd-fonts--helm-read@ad-override) "TAB C-n RET"))
      (car kill-ring)))))

;;; nerd-fonts-helm-test.el ends here
