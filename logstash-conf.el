;;; logstash-conf.el -- basic mode for editing logstash configuration

(require 'conf-mode)

(defvar logstash-indent 8)

(defun logstash--open-parens ()
  "Return the number of open brackets before point."
  (let ((open-paren-count 0)
        (open-parens '(?\{ ?\[)))
    (save-excursion
      (while (search-backward-regexp (rx (or "{" "}" "[" "]")) nil t)
        (if (memq (char-after) open-parens)
            (incf open-paren-count)
          (decf open-paren-count))))
    open-paren-count))

(defun logstash-indent-line ()
  (interactive)
  (let ((initial-column (current-column))
        initial-indentation
        correct-indentation-level)
    ;; Get the current indentation
    (back-to-indentation)
    (setq initial-indentation (current-column))

    ;; Remove it.
    (while (not (zerop (current-column)))
      (delete-char -1))

    ;; Step over trailing close curlies before counting.
    (save-excursion
      (while (looking-at "}")
        (forward-char 1))

      (setq correct-indentation-level (logstash--open-parens)))

    ;; Replace with the correct indentation.
    (dotimes (_ (* logstash-indent correct-indentation-level))
      (insert " "))

    ;; Restore point at the same offset on this line.
    (let ((point-offset (- initial-column initial-indentation)))
      (forward-char point-offset))))

(defun logstash-conf-mode ()
  (interactive)
  ;; It's a pain to use `define-derived-mode' with conf-mode, so just
  ;; call it directly instead.
  (conf-unix-mode)
  (setq indent-line-function 'logstash-indent-line)
  (setq mode-name "Logstash"))
