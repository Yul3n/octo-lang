;;; octo-mode.el --- A simple major mode providing syntax highlighting for octo-lang.
;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst octo-highlights
  '(("where\\|type\\|int" . font-lock-keyword-face)
    ("--.*\n" . font-lock-comment-face)
    ("\\([a-zA-Z]*\\)\\([a-zA-Z _]*\\)=" . (2 font-lock-variable-name-face))
    ("\\([a-zA-Z]*\\).*=" . (1 font-lock-function-name-face))))

(defun indent-line ()
  "Indent current line as octo code."
  (interactive)
  (let (begin curindent)
    (setq begin (point))
    (if (= 1 (line-number-at-pos))
        (setq curindent 0)
      (progn
        (beginning-of-line)
         (if (looking-at "\\(\n\n[a-zA-Z]*\\).*=") ; If the line is a function declaration
             (setq curindent 0)                    ; to 0.
           (progn
             (forward-line -1)
             (if (and (looking-at "\\([a-zA-Z]*\\).*=") (not (looking-at "[ ]+")))
                 (progn
                   (forward-line 1)
                   (setq curindent 2))
               (let (b e)
                 (setq b (point)) ; Get the indentation level of the previous line.
                 (skip-chars-forward " ")
                 (setq e (point))
                 (if (looking-at ".*where\n") ; Add a level of indentation after the
                     (progn                   ; keyword where.
                       (forward-line 1)
                       (setq curindent (+ (- e b) 2)))
                   (progn
                     (forward-line 1)
                     (setq curindent (- e b))))))))))
      (indent-line-to curindent)
      (if (/= (point) begin)
          (goto-char (+ curindent begin)))))
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
  (setq display-line-numbers t)
  )

(add-to-list 'auto-mode-alist
             '("\\.oc\\'" . octo-mode))
(provide 'octo-mode)
;;; octo-mode ends here
