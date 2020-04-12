;;; octo-mode.el --- A simple major mode providing syntax highlighting for octo-lang.
;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst octo-highlights
  '(("where" . font-lock-keyword-face)
    ("--.*\n" . font-lock-comment-face)
    ("\n\\([a-zA-Z]*\\)\\([a-zA-Z _]*\\)=" . (2 font-lock-variable-name-face))
    ("\n\\([a-zA-Z]*\\).*=" . (1 font-lock-function-name-face))))

(define-derived-mode octo-mode fundamental-mode "octo"
  "major mode for editing octo language code."
  (setq comment-add "-- "
        comment-style "-- "
        comment-styles "-- "
        comment-start "-- "
        comment-end ""
        comment-auto-fill-only-comments t
        font-lock-defaults '(octo-highlights)))

(provide 'octo-mode)
;;; octo-mode ends here
