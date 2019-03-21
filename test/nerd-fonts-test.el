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

(ert-deftest test-nerd-fonts-miss-match ()
  (should (equal nil (nerd-fonts "m-i-s-s-m-a-t-c-h"))))

(ert-deftest test-nerd-fonts-match ()
  (should (equal "ﬦ" (nerd-fonts "mdi-lambda"))))

(ert-deftest test-nerd-fonts-insert ()
  (let ((test-helm-nerd-fonts-insert nil))
    (should
     (equal
      "ﬦ"
      (with-temp-buffer
        (test-with '(call-interactively 'nerd-fonts 'any) "mdi-lambda RET")
        (buffer-substring-no-properties (point-min) (point-max)))))))

;;; nerd-fonts.el ends here
