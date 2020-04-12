;;; octo-mode.el --- A simple major mode providing syntax highlighting for octo-lang.
;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst octo-highlights
  '(("where" . font-lock-keyword-face)
    ("--.*\n" . font-lock-comment-face)
    (" \\([a-zA-Z]*\\)\\([a-zA-Z _]*\\)=" . (2 font-lock-variable-name-face))
    ("\\([a-zA-Z]*\\).*=" . (1 font-lock-function-name-face))))

(defun indent-line ()
  "Indent current line as octo code."
  (interactive)
  (let ((cur-indent 0) begin)
    (setq begin (point))
    (beginning-of-line)
    (if (looking-at "\\(\n\n[a-zA-Z]*\\).*=") ; If the line is a function declaration
        (indent-line-to 0)                ; to 0.
      (progn
        (forward-line -1)

        (if (and (looking-at "\\([a-zA-Z]*\\).*=") (not (looking-at "[ ]+")))
            (progn
              (forward-line 1)
              (indent-line-to 2))
          (let (b e)
            (setq b (point))
            (skip-chars-forward " ")
            (setq e (point))
            (if (looking-at ".*where\n") ; Add a level of indentation after the
                (progn                     ; keyword where.
                   (forward-line 1)
                   (indent-line-to (+ (- e b) 2)))
              (progn
                (forward-line 1)
                (indent-line-to (- e b) )))))))
    (if (> begin (point))
        (goto-char begin))
    ))
(define-derived-mode octo-mode fundamental-mode "octo"
  "major mode for editing octo language code."
  (setq comment-add "-- "
        comment-style "-- "
        comment-styles "-- "
        comment-start "-- "
        comment-end ""
        comment-auto-fill-only-comments t
        font-lock-defaults '(octo-highlights)
        )
  (set (make-local-variable 'indent-line-function) 'indent-line)
  )

(provide 'octo-mode)
;;; octo-mode ends here
