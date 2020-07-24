;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((logstash-conf-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path logstash-conf-dir))

(require 'undercover)
(undercover "logstash-conf.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
