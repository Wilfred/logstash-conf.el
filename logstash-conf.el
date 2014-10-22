;;; logstash-conf.el -- basic mode for editing logstash configuration

;; Copyright (C) 2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 21 October 2014
;; Version: 0.1

;;; Commentary:
;; `conf-mode' offers adequate highlighting for Logstash configuration
;; files, but does not indent them correctly. This file defines a
;; simple `logstash-conf-mode' that both highlights and indents.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
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

(provide 'logstash-conf)
;;; logstash-conf.el ends here
