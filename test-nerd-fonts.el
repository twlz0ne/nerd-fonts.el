;;; nerd-fonts.el --- Nerd font utility -*- lexical-binding: t; -*-

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

(defvar test-expr nil
  "Holds a test expression to evaluate with `test-eval'.")

(defvar test-result nil
  "Holds the eval result of `test-expr' by `test-eval'.")

(defun test-eval ()
  "Evaluate `test-expr'."
  (interactive)
  (setq test-result (eval test-expr)))

(global-set-key (kbd "C-c e") 'test-eval)

(defun test-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((test-expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c e")
              (kbd keys)))
    test-result))

(ert-deftest test-nerd-fonts-0 ()
  (should (equal nil (nerd-fonts "lambda"))))

(ert-deftest test-nerd-fonts-1 ()
  (should (equal "ﬦ" (nerd-fonts "mdi-lambda"))))

(ert-deftest test-nerd-fonts-2 ()
  (should
   (equal
    "ﬦ"
    (with-temp-buffer
      (test-with '(call-interactively 'nerd-fonts 'any) "mdi-lambda RET")
      (buffer-substring-no-properties (point-min) (point-max))))))

;;; nerd-fonts.el ends here
