;;; octo-mode.el --- A simple major mode providing syntax highlighting for octo-lang.
;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst octo-highlights
  '(("--.*$" . font-lock-comment-face)
    ("'[\\]?.'" . font-lock-string-face)
    (" where[ $\n]\\|type\\| float\\| char[ $\n]\\|case\\|of[ $\n]\\|open \\|when \\| option"
     . font-lock-keyword-face)
    ("map \\|error \\|and \\|or" . font-lock-function-name-face)
    ("\\([a-zA-Z_']*\\)\\([a-zA-Z_', ]*\\)[ \n]*=" . (2 font-lock-variable-name-face))
    ("[A-Z][a-zA-Z_']*". font-lock-constant-face)
    ("[0-9]*\\|@\\|::" . font-lock-constant-face)
    ("type \\([a-zA-Z_']*\\)". (1 font-lock-function-name-face))
    ("\\([a-zA-Z_']*\\).*=" . (1 font-lock-function-name-face))))

(defun indent-line ()
  "Indent current line as octo code."
  (interactive)
  (let (begin curindent b1 e1 actindent)
    (setq begin (point))
    (if (= 1 (line-number-at-pos)) ; The first line is never indented
        (setq curindent 0)
      (save-excursion
        (beginning-of-line)
        (setq b1 (point)) ; Get the indentation level of this line.
        (skip-chars-forward " ")
        (setq e1 (point))
        (beginning-of-line)
        (setq actindent (- e1 b1))
         (if (looking-at "\n\n[a-zA-Z]*.* =") ; If the line is a function declaration
             (setq curindent 0)               ; to 0.
           (progn
             (forward-line -1)
             ; Indent the line if the line before is a function declaration
             (if (and (looking-at "[a-zA-Z]*.*[ \n]*=[ \n]*") (not (looking-at "[ ]+")))
                 (progn
                   (setq curindent 2))
               (let (b e)
                 (setq b (point)) ; Get the indentation level of the previous line.
                 (skip-chars-forward " ")
                 (setq e (point))
                 (if (looking-at ".*where[ ]*$\\|.*case.*of[ ]*$\\|.*->[ ]*$")
                     (setq curindent (+ (- e b) 2))
                   (progn
                     (if (looking-at ".*->.*") ; Set the indentation according to the last line
                         (progn
                           (forward-line 1)
                           (if (looking-at ".*->.*")
                               (setq curindent (- e b))
                             (setq curindent (- (- e b) 2))))
                           (setq curindent (- e b)))))))))))
      (when (/= curindent actindent)
        (indent-line-to curindent)
        (goto-char (- (+ curindent begin) actindent)))
      (skip-chars-forward " ")))

(defun back-indent ()
  "Indent the line backward."
  (interactive)
  (let (e b)
    (save-excursion
      (beginning-of-line)
      (setq b (point)) ; Get the indentation level of this line.
      (skip-chars-forward " ")
      (setq e (point))
      (beginning-of-line)
      (indent-line-to (- (- e b) 2)))))

(define-derived-mode octo-mode fundamental-mode "octo"
  "major mode for editing octo language code."
  (setq comment-add    "-- " ; Set up the comment style
        comment-style  "-- "
        comment-styles "-- "
        comment-start  "-- "
        comment-end    ""
        comment-auto-fill-only-comments t
        font-lock-defaults '(octo-highlights)) ; Activate syntax highlighting
  (set (make-local-variable 'indent-line-function) 'indent-line)
  (setq display-line-numbers t))

(add-to-list 'auto-mode-alist
             '("\\.oc\\'" . octo-mode)) ; Open the mode on each ".oc" file

(global-set-key (kbd "<backtab>") 'back-indent)
(provide 'octo-mode)
;;; octo-mode ends here
