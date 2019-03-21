;;; test-helper.el --- Helpers for nerd-fonts-test.el

(require 'cl)

(let ((ver-list (version-to-list emacs-version)))
  (setq user-emacs-directory (expand-file-name "./"))
  (setq package-user-dir (expand-file-name (format "./.cask/%s.%s/elpa/" (car ver-list) (car (cdr ver-list)))))
  (package-initialize))

(defvar test-expr nil
  "Holds a test expression to evaluate with `test-eval'.")

(defvar test-result nil
  "Holds the eval result of `test-expr' by `test-eval'.")

(defun test-eval ()
  "Evaluate `test-expr'."
  (interactive)
  (setq test-result (eval test-expr)))

(global-set-key (kbd "C-c C-c e") 'test-eval)

(defun test-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (erase-buffer))))
  (let ((test-expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c C-c e")
              (kbd keys)))
    test-result))

;; @FIX-ERROR After 0 kbd macro iterations: Symbol’s function definition is void: ert--print-backtrace
;; @COPY-FROM .cask/${emacs_version}/elpa/ert-runner-20180831.1145/ert-compat.el
;; @RELATED-2 https://github.com/rejeep/ert-runner.el/issues/49
(defun ert--print-backtrace (backtrace)
  "Format the backtrace BACKTRACE to the current buffer."
  ;; This is essentially a reimplementation of Fbacktrace
  ;; (src/eval.c), but for a saved backtrace, not the current one.
  (let ((print-escape-newlines t)
        (print-level 8)
        (print-length 50))
    (dolist (frame backtrace)
      (ecase (first frame)
        ((nil)
         ;; Special operator.
         (destructuring-bind (special-operator &rest arg-forms)
             (cdr frame)
           (insert
            (format "  %S\n" (list* special-operator arg-forms)))))
        ((t)
         ;; Function call.
         (destructuring-bind (fn &rest args) (cdr frame)
           (insert (format "  %S(" fn))
           (loop for firstp = t then nil
                 for arg in args do
                 (unless firstp
                   (insert " "))
                 (insert (format "%S" arg)))
           (insert ")\n")))))))

;;; test-helper.el ends here
