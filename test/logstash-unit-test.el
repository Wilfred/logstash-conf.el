(require 'ert)
(require 'logstash-conf)

(ert-deftest logstash-indent ()
  (let ((src "input {
    file {
        path => \"/\"
    }
}"))
    (with-temp-buffer
      (logstash-conf-mode)
      (insert src)

      (indent-region (point-min) (point-max))
      (should (string= (buffer-string) src)))))

(ert-deftest logstash-highlight-single-quoted ()
  (let ((src "'foo'"))
    (with-temp-buffer
      (logstash-conf-mode)
      (insert src)

      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          (font-lock-fontify-buffer)))

      (goto-char (point-min))
      (should (eq (face-at-point) 'font-lock-string-face)))))
