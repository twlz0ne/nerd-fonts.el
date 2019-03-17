;; download sh scripts
(--map
 (->> (concat "https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/bin/scripts/lib/" it)
      (url-retrieve-synchronously)
      (funcall
       (lambda (buffer)
         (with-current-buffer buffer
           (goto-char (point-min))
           (set-mark (point))
           (search-forward-regexp "\n\n")
           (delete-region (region-beginning) (region-end))
           (message "==> %s downloaded" it)
           (write-file it)
           ))))
 '("i_dev.sh" "i_fa.sh" "i_fae.sh" "i_iec.sh" "i_linux.sh" "i_material.sh"
   "i_oct.sh" "i_ple.sh" "i_pom.sh" "i_seti.sh" "i_weather.sh"
   ))

;; generate el data
(let ((el-data))
  (-map (lambda (sh-file)
          (with-temp-buffer
            (insert-file sh-file)
            (goto-char (point-min))
            (when (re-search-forward "test -n" nil t)
              (forward-line)
              (set-mark (point))
              (if (re-search-forward "unset i" nil t)
                  (goto-char (point-at-bol))
                (goto-char (point-max)))
              (setq el-data
                    (->> (buffer-substring-no-properties (region-beginning) (region-end))
                         (replace-regexp-in-string "\n[\s\t]*" " ")
                         (s-split "i=")
                         (-filter (lambda (it) (not (string-empty-p it))))
                         (--map
                          (->> it
                               (replace-regexp-in-string "=\\$i[^\s]*\\(?:\s\\|\\'\\)" " ")
                               (funcall
                                (lambda (it_)
                                  (when (string-match "\\`'\\([^']+\\)'[\t\s]+i_\\(.*\\)\\'" it_)
                                    (cons (->> (match-string 2 it_)
                                               (replace-regexp-in-string " i_" " ")
                                               (replace-regexp-in-string "[\t\s]+\\'" "")
                                               (replace-regexp-in-string "_" "-"))
                                          (match-string 1 it_)))))))
                         (--filter (and it it))
                         (-concat el-data))))))
        '("i_dev.sh" "i_fa.sh" "i_fae.sh" "i_iec.sh" "i_linux.sh" "i_material.sh"
          "i_oct.sh" "i_ple.sh" "i_pom.sh" "i_seti.sh" "i_weather.sh"
          ))
  (with-temp-buffer
    (insert ";; This file is generated automatically. DO NOT change it !!\n")
    (insert (format "(defconst nerd-fonts-alist '%S)" el-data))
    (insert "\n(provide 'nerd-fonts-data)")
    (el-beautify (point-min) (point-max))
    (write-file (expand-file-name "../nerd-fonts-data.el"))
    ))
