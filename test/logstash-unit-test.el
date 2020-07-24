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
