;;; poly-quarto.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Pierre-André Cornillon
;;
;; Author: Pierre-André Cornillon <pierre-andre.cornillon@univ-rennes2.fr>
;; Maintainer: Pierre-André Cornillon <pierre-andre.cornillon@univ-rennes2.fr>
;; Created: février 22, 2021
;; Modified: april 30, 2025
;; Version: 0.5.0
;; package-requires: ((emacs "25.1") (polymode "0.2.2") (markdown-mode "2.3"))
;; Keywords: languages, emacs, multi-modes, tex
;; Homepage: https://github.com/pac/toto
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Fresetqe Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;  initial code from Benjamin Philip (poly-markdown) and Carlos Scheidegger (quarto-emacs).
;;  Description
;;
;;; Code:

(require 'comint)
(require 'shell)
(require 'polymode)
;; (require 'polymode-debug.el)
(require 'poly-lock)
(require 'markdown-mode)
(declare-function pm-debug-fontify-current-span "pm-debug.el")

(define-obsolete-variable-alias 'pm-host/poly-quarto 'poly-quarto-hostmode "v0.2")
(define-obsolete-variable-alias 'pm-inner/poly-quarto-yaml-metadata 'poly-quarto-yaml-metadata-innermode "v0.2")
(define-obsolete-variable-alias 'pm-inner/poly-quarto-fenced-code 'poly-quarto-fenced-code-innermode "v0.2")
(define-obsolete-variable-alias 'pm-inner/poly-quarto-inline-code 'poly-quarto-inline-code-innermode "v0.2")
(define-obsolete-variable-alias 'pm-inner/poly-quarto-displayed-math 'poly-quarto-displayed-math-innermode "v0.2")
(define-obsolete-variable-alias 'pm-inner/poly-quarto-inline-math 'poly-quarto-inline-math-innermode "v0.2")
(define-obsolete-variable-alias 'pm-poly/poly-quarto 'poly-quarto-polymode "v0.2")

(defcustom poly-quarto-latex-mode 'LaTeX-mode
  "latex-mode or LaTeX-mode.
By default poly-quarto use LaTeX-mode from AUCTeX but if someone
wants to use 'latex-mode' included in emacs to work
with the lighter 'latex-mode' and without AUCTeX dependancy."
  :group 'poly-quarto)

(defcustom poly-quarto-enable-math 't
  "Enabling math rendering by default."
  :type 'boolean
  :group 'poly-quarto)

(defcustom poly-quarto-preview-display-buffer nil
  "When nil, `poly-quarto-preview' does not automatically display buffer.
This buffer shows the output of quarto command."
  :group 'poly-quarto
  :type 'boolean)

;; (defcustom poly-quarto-force-preview t
;;   "Preview with `poly-quarto-preview'.
;; When t, all markdown rendering commands go through  instead of producing
;; disk output."
;;   :group 'poly-quarto
;;   :type 'boolean)

(defcustom poly-quarto-command (let ((cmd (executable-find "quarto")))
			    (and cmd (file-name-nondirectory cmd)))
  "Command to run quarto."
  :group 'poly-quarto
  :type '(string :tag "Shell command"))

(defcustom poly-quarto-codelang "r"
  "Default language when inserting chunk/block of code."
  :group 'poly-quarto
  :type '(string :tag "Lang"))

(defcustom poly-quarto-watch-inputs t
  "Watch for file change."
  :group 'poly-quarto
  :type 'boolean)


(defcustom poly-quarto-nblocks-to-fontify 5
 "Number of blocks to fontify when fontifying around point.
Actually it fontifies `2*poly-quarto-nblocks-to-fontify` blocks/chunk."
  :group 'poly-quarto
  :type 'integer)

(defcustom poly-quarto-latex-delims '("eqnarray" "align" "equation")
 "LaTeX environment that can be alone (without $$ surrounding it) in quarto.
In quarto, LaTeX chunk can be defined as `\begin{eqnarray*} ...
\end{eqnarray*}` without surrounding `$$` whereas in
poly-quarto the LaTeX chunk is (for that example)
 `$$%\n\begin{eqnarray*} ... \end{eqnarray*}$$\n`.
The name of that kind of
environment is given here. The function
`poly-quarto-fix-alone-begin-end` will surround that kind of chunk with
`$$% ... $$`."
  :group 'poly-quarto
  :type '(restricted-sexp :tag "Vector"
              :match-alternatives
              (lambda (xs) (and (vectorp xs) (seq-every-p #'stringp xs)))))

(defcustom  poly-quarto-all-codelang '("julia" "r" "python")
  "List of all language available in quarto code."
    :type '(repeat string)
    :group 'poly-quarto)

(defvar poly-quarto-mode--latex-displayed-beg-bracket "^[ \t]*\\\\\\[")
(defvar poly-quarto-mode--latex-displayed-end-bracket "\\\\\\]")
(defvar poly-quarto-mode--latex-displayed-beg-dol "^[ \t]*\\([$][$]\\)%")
(defvar poly-quarto-mode--latex-displayed-end-dol "\\(\\$\\$\\)")
(defvar poly-quarto-mode--latex-inline-beg-paren "\\\\(")
(defvar poly-quarto-mode--latex-inline-end-paren "\\\\)")
(defvar poly-quarto-mode--latex-inline-beg-dol "[ \n\t]\\(\\$\\)[^$ .,;:!?~\n\t]")
(defvar poly-quarto-mode--latex-inline-end-dol "\\(\\$\\)[ .,;:!?~\n\t]")

(defvar poly-quarto-mode--pandocblock-beg "::: {")
(defvar poly-quarto-mode--pandocblock-end "}")

(defvar poly-quarto-mode--fenced-code-beg "^[ \t]*\\(```[ \t]*{?[[:alpha:].=].*\n\\)")
(defvar poly-quarto-mode--fenced-code-end "^[ \t]*\\(```\\)[ \t]*$")
;(defvar poly-quarto-mode--fenced-code-choose "```[ \t]*{\\([^ \t\n;=,}]+\\)")
(defvar poly-quarto-mode--fenced-code-choose "```[ \t]*{?[.=]?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)")
(defvar poly-quarto-mode--inline-code-beg "[^`]\\(`\\){?[[:alpha:]+-&({*[]")
(defvar poly-quarto-mode--inline-code-end "[^`]\\(`\\)[^`[:alpha:]+-&({*[]")
(defvar poly-quarto-mode--inline-code-choose "`[ \t]*{?\\([[:alpha:]+-]+\\)")

(defvar-local poly-quarto-mode--preview-process nil)
(defvar-local poly-quarto-mode--preview-url nil)

(defcustom poly-quarto-markdown-exporter
  (pm-shell-exporter :name "quarto"
		     :from
		     '(("quarto" "\\.qmd" "quarto Markdown"
			"quarto render --to=%t --output=%o"))
		     :to
		     '(("auto" . poly-quarto-pm--shell-auto-selector)
                       ("default" . poly-quarto-pm--shell-auto-selector)
		       ("html" "html" "html document" "html")
                       ("pdf" "pdf" "pdf document" "pdf")
                       ("word" "docx" "word document" "docx")
                       ("odt"  "odt" "open document" "odt")
		       ("revealjs" "html" "revealjs presentation" "revealjs"))) ;; TODO fill this out automatically
  "Quarto Markdown exporter.
Please note that with 'AUTO DETECT' export options, output file
names are inferred by quarto from the appropriate metadata.
That is, output file names don't comply with
`polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(define-hostmode poly-quarto-hostmode
  :mode 'markdown-mode)
;  :init-functions '(poly-quarto-remove-markdown-hooks))

(define-innermode poly-quarto-root-innermode
  :mode nil
  :fallback-mode 'host
  :head-mode 'host
  :tail-mode 'host)

(define-innermode poly-quarto-yaml-metadata-innermode poly-quarto-root-innermode
  :mode 'yaml-mode
  :head-matcher (pm-make-text-property-matcher 'markdown-yaml-metadata-begin :inc-end)
  :tail-matcher (pm-make-text-property-matcher 'markdown-yaml-metadata-end)
  :allow-nested nil)

;; allow extra . before language name https://github.com/polymode/polymode/issues/296
;; allow extra = before language name https://github.com/polymode/poly-markdown/issues/22
(define-auto-innermode poly-quarto-fenced-code-innermode poly-quarto-root-innermode
  :head-matcher (cons poly-quarto-mode--fenced-code-beg 1)
  :tail-matcher (cons poly-quarto-mode--fenced-code-end 1)
  :mode-matcher (cons poly-quarto-mode--fenced-code-choose 1)
  :allow-nested nil)

(define-auto-innermode poly-quarto-inline-code-innermode poly-quarto-root-innermode
  :head-matcher (cons poly-quarto-mode--inline-code-beg 1)
  :tail-matcher (cons poly-quarto-mode--inline-code-end 1)
  :mode-matcher (cons poly-quarto-mode--inline-code-choose 1)
  :allow-nested nil)

(defun poly-quarto-displayed-math-head-matcher (count)
  "Find the beginning of displayed math latex span.
COUNT have the same meaning as in `re-search-forward'.
Beginning is either `\\[` or `$$%`. Note that a `%` is added
to distinguish head from tail (and it does not change output)"
  (when poly-quarto-enable-math
    (when (re-search-forward
           (concat
            poly-quarto-mode--latex-displayed-beg-bracket
            "\\|"
             poly-quarto-mode--latex-displayed-beg-dol)
           nil t count)
      (if (match-beginning 1)
          (cons (match-beginning 1) (match-end 1))
        (cons (match-beginning 0) (match-end 0))))))

(defun poly-quarto-displayed-math-tail-matcher (_count)
  "Find the end of displayed math latex span."
  (when poly-quarto-enable-math
   (if (match-beginning 1)
       (when (re-search-forward
              poly-quarto-mode--latex-displayed-end-dol
              nil t)
         (cons (match-beginning 1) (match-end 1)))
     (when (re-search-forward
            poly-quarto-mode--latex-displayed-end-bracket
            nil t)
       (cons (match-beginning 0) (match-end 0))))))

(define-innermode poly-quarto-displayed-math-innermode poly-quarto-root-innermode
  "Displayed math $$..$$ innermode.
Tail must be flowed by a new line but head need not (a space or
comment character would do)."
  :mode poly-quarto-latex-mode
  :head-matcher #'poly-quarto-displayed-math-head-matcher
  :tail-matcher #'poly-quarto-displayed-math-tail-matcher
  :head-mode 'host
  :tail-mode 'host
  :allow-nested nil)

(defun poly-quarto-inline-math-head-matcher (count)
"Find the beginning of inline math latex span.
COUNT have the same meaning as in `re-search-forward'."
(when poly-quarto-enable-math
    (when (re-search-forward
           (concat poly-quarto-mode--latex-inline-beg-paren
                   "\\|"
                   poly-quarto-mode--latex-inline-beg-dol)
           nil t count)
      (if (match-beginning 1)
          (cons (match-beginning 1) (match-end 1))
        (cons (match-beginning 0) (match-end 0))))))

(defun poly-quarto-inline-math-tail-matcher (_count)
  "Find the end of inline math latex span."
  (when poly-quarto-enable-math
    (if (match-beginning 1)
        ;; head matched an $..$ block
        (when (re-search-forward
               poly-quarto-mode--latex-inline-end-dol
               nil t)
          (cons (match-beginning 1) (match-end 1)))
      ;; head matched an \(..\) block
      (when (re-search-forward
             poly-quarto-mode--latex-inline-end-paren
             nil t)
        (cons (match-beginning 0) (match-end 0))))))

(define-innermode poly-quarto-inline-math-innermode poly-quarto-root-innermode
  "Inline math $..$ block.
First $ must be preceded by a white-space character and followed
by a non-whitespace/digit character. The closing $ must be
preceded by a non-whitespace and not followed by an alphanumeric
character."
  :mode 'latex-mode
  :can-nest nil
  :head-matcher #'poly-quarto-inline-math-head-matcher
  :tail-matcher #'poly-quarto-inline-math-tail-matcher
  :allow-nested nil)

(define-innermode poly-quarto-pandocblock-innermode poly-quarto-root-innermode
  "Div line."
  :mode 'pandocblock-mode
  :head-matcher poly-quarto-mode--pandocblock-beg
  :tail-matcher poly-quarto-mode--pandocblock-end
  :head-mode 'host
  :tail-mode 'host
  :allow-nested nil)

;;;###autoload  (autoload 'poly-quarto-mode "poly-quarto")
(define-polymode poly-quarto-mode
  :hostmode 'poly-quarto-hostmode
  :innermodes '(poly-quarto-fenced-code-innermode
                poly-quarto-pandocblock-innermode
                poly-quarto-displayed-math-innermode
                poly-quarto-inline-code-innermode
                poly-quarto-inline-math-innermode
                poly-quarto-pandocblock-innermode
                poly-quarto-yaml-metadata-innermode))

(polymode-register-exporter poly-quarto-markdown-exporter
			    nil poly-quarto-polymode)

;;; ; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun poly-quarto-pm--output-file-sniffer ()
  "Detect the output format from a quarto run."
  (goto-char (point-min))
  (let (files)
    (while (re-search-forward "Output created: +\\(.*\\)" nil t)
      (push (expand-file-name (match-string 1)) files))
    (reverse (delete-dups files))))

(defun poly-quarto-pm--shell-auto-selector (action &rest _ignore)
  "Select the output format automatically from a run of quarto in a shell.
ACTION decides which action to take, `doc`, `command` or `output-file`."
  (cl-case action
    (doc "AUTO DETECT")
    (command "quarto render %i")
    (output-file #'poly-quarto-pm--output-file-sniffer)))

(defun poly-quarto-preview ()
  "Start/restart a quarto preview process to rerender documents.

`poly-quarto-preview` checks parent directories for a `_quarto.yml`
file.  If one is found, then `poly-quarto-preview` previews that entire
project, this is \"project mode\"..

If not, then `poly-quarto-preview` previews the file for the current
buffer, this is \"file mode\".

In project mode, project files aren't automatically watched in
the file system.

To control whether or not to show the display, customize
`poly-quarto-preview-display-buffer`."
  (interactive)
  (when poly-quarto-mode--preview-process
    (delete-process poly-quarto-mode--preview-process))
  (when (get-buffer "*quarto-preview*")
    (kill-buffer "*quarto-preview*"))

  (let*  ;; ((project-directory (poly-quarto-mode--buffer-in-quarto-project-p))
	 ;; (browser-path (cond
	 ;;        	(project-directory
	 ;;        	 (file-relative-name buffer-file-name project-directory))
	 ;;        	(t "")))
	 ((process
	  ;; (let ((process-environment (cons (concat "QUARTO_RENDER_TOKEN="
	  ;;       				   poly-quarto-mode--quarto-preview-uuid)
	  ;;       			   process-environment)))
	    (make-process :name (format "quarto-preview-%s" buffer-file-name)
			  :buffer "*quarto-preview*"
			  :command (if  poly-quarto-watch-inputs
                                       (list poly-quarto-command
					 "preview"
					 buffer-file-name)
                                     (list poly-quarto-command
					 "preview"
					 buffer-file-name
                                         "--no-watch-inputs")))))
    (setq poly-quarto-mode--preview-process process)
    (with-current-buffer (process-buffer process)
      (when poly-quarto-preview-display-buffer
	(display-buffer (current-buffer)))
      (shell-mode))))

(defun poly-quarto-next-chunk (&optional N)
  "Go N chunks forwards.
If N negative go backward
Return t if the line number is increased (or decreased if backward)."
  (interactive)
  (let ((thepoint (point))
        (back (if N (< N 0))))
    (pm-goto-span-of-type '(nil body) (or N 1))
    (if back
        (progn
          (when (looking-back "^\\s *" (max 1 (- (point) 90)))
            (forward-line -1))
          (< (point) thepoint))
      (progn
        (when (looking-at "\\s *$")
          (forward-line 1))
        (> (point) thepoint)))))

(defvar poly-quarto-block-beg-regexp
  "[[:blank:]]*::")
(defvar poly-quarto-block-end-regexp
  "}[ ]*$")
(defvar poly-quarto-block-regexp
  (concat  poly-quarto-block-beg-regexp "[:]+ {"))
(defvar poly-quarto-block-level-regexp
  (concat  poly-quarto-block-beg-regexp "\\([:]+\\) {"))

(defun poly-quarto-on-block-p (&optional invisible-ok)
  "Return t if point is on a (visible) pandoc/div block `::: {` heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (poly-quarto-block-invisible-p)))
	 (looking-at poly-quarto-block-regexp))))

(defsubst poly-quarto-block-invisible-p (&optional pos)
  "Non-nil if the character after POS has outline invisible property.
If POS is nil, use `point' instead."
  (eq (get-char-property (or pos (point)) 'invisible) 'outline))

(defun poly-quarto-block-end-of-heading ()
  "Move to one char before the next `poly-quarto-block-end-regexp'."
  (if (re-search-forward poly-quarto-block-end-regexp nil 'move)
      (forward-char -1)))

(defun poly-quarto-block-end-of-same-level ()
  "Move to one char before the same level `poly-quarto-block-end-regexp'."
  (when (poly-quarto-back-to-heading)
    (re-search-forward poly-quarto-block-level-regexp nil 'move)
    (let ((outfound 1)
          (regexptoreach
           (concat
            "\\(" poly-quarto-block-beg-regexp (match-string 1) " {\\)"
            "\\|"
            "\\(" poly-quarto-block-beg-regexp (match-string 1) "\\)")))
      (poly-quarto-block-end-of-heading)
      (while (> outfound 0)
        (re-search-forward regexptoreach  nil 'move)
        (if (match-string 1)
            (setq outfound (+ outfound 1))
          (setq outfound (- outfound 1)))))
  (point)))


(defun poly-quarto-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (poly-quarto-on-block-p invisible-ok)
      (let (found)
        (save-excursion
          (while (not found)
            (or (re-search-backward (concat "^\\(?:" poly-quarto-block-regexp "\\)") nil t)
                (signal 'outline-before-first-heading nil))
            (setq found (and (or invisible-ok (not (outline-invisible-p)))
                             (point)))))
        (goto-char found)
        found)))

(defun poly-quarto-hide-same-level ()
  "Hide the body of the div/pandoc block.
Search the end of the current block and hide it"
  (interactive)
  (save-excursion
    (poly-quarto-back-to-heading)
    (poly-quarto-block-end-of-heading)
       (poly-quarto-flag-region
        (point)
        (poly-quarto-block-end-of-same-level) t)))
(defun poly-quarto-show-same-level ()
  "Show the body of the div/pandoc block.
Search the end of the current block and hide it."
  (interactive)
  (save-excursion
    (poly-quarto-back-to-heading t)
    (poly-quarto-flag-region (1- (point))
                             (progn
                               (poly-quarto-block-end-of-same-level)
                               (if (= 1 (- (point-max) (point)))
                                   (point-max)
                                 (point)))
                             nil)))

(defun poly-quarto-toggle-block ()
  "Show or hide the current block depending on its current state."
  (interactive)
  (save-excursion
    (poly-quarto-back-to-heading)
    (if (not (poly-quarto-block-invisible-p (line-end-position)))
        (poly-quarto-hide-same-level)
      (poly-quarto-show-same-level))))

(defun poly-quarto-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden.
this is a simplified copy of `outline-flag-region`"
  (remove-overlays from to 'invisible 'outline)
  (when flag
    ;; We use `front-advance' here because the invisible text begins at the
    ;; very end of the heading, before the newline, so text inserted at FROM
    ;; belongs to the heading rather than to the entry.
    (let ((o (make-overlay from to nil 'front-advance)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'invisible 'outline)
      (overlay-put o 'isearch-open-invisible
		   (or outline-isearch-open-invisible-function
		       #'outline-isearch-open-invisible))))
  (run-hooks 'outline-view-change-hook))

(defun poly-quarto-cycle (orig-fun &rest arg)
  (interactive "P")
  (message "arg:%s" arg)
  (if (nth 0 arg)
   (apply orig-fun arg)
   (if (save-excursion (beginning-of-line 1) (poly-quarto-on-block-p))
       (poly-quarto-toggle-block)
     (apply orig-fun arg))))


(advice-add 'markdown-cycle :around #'poly-quarto-cycle)

(defun poly-quarto-fontify-current-buffer ()
  "Fontify current buffer.
Go from chunk to chunk and refontify"
  (interactive)
  (let ((poly-lock-allow-fontification t))
    (font-lock-unfontify-buffer)
    (poly-lock-flush (point-min) (point-max))
    (save-excursion
      (goto-char (point-min))
      (while (poly-quarto-next-chunk)
        (pm-debug-fontify-current-span)))))

(defun poly-quarto-fontify-around-point ()
  "Fontify around point.
Go up `poly-quarto-fontify-around-nblocks` and fontify
2*`poly-quarto-fontify-around-nblocks`+1 nblocks"
  (interactive)
  (let ((poly-lock-allow-fontification t))
    (font-lock-unfontify-buffer)
    (poly-lock-flush (point-min) (point-max))
    (save-excursion
      (poly-quarto-next-chunk (- poly-quarto-nblocks-to-fontify))
      (let ((niter 0))
        (while
            (and (poly-quarto-next-chunk)
                 (< niter (+ (* 2 poly-quarto-nblocks-to-fontify) 1)))
          (pm-debug-fontify-current-span)
          (setq niter (+ 1 niter)))))))

(defun poly-quarto-fix-only-dollar ()
  "Check and replace latex displayed and inline delimiters.
Block `$$ ... $$` is replaced by `$$%\n ... $$\n` and
block `$...$` is left with a preceeding blank,
ie if there is no `[ \n\t]` before,
and followed by `[.\n\t]`"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((entered))
      (while (re-search-forward
              (concat "\\([^\\][$]\\)" "\\|\\("
                      poly-quarto-mode--fenced-code-beg "\\)\\|\\("
                      poly-quarto-mode--inline-code-beg "\\)")
              nil 't)
        (if (not (match-end 1))
            ;; go to end of code
            (if (match-end 2)
                (re-search-forward poly-quarto-mode--fenced-code-end nil 't)
              (re-search-forward poly-quarto-mode--inline-code-end))
          ;; we have found a LaTeX $
          (let ((current-indent 0))
            (if entered
                ;; exiting displayed LaTeX
                (if (looking-at "[$]")
                    (unless (looking-at "[$]\n")
                      (forward-char)
                      (insert ?\n)
                      (save-excursion (beginning-of-line)
                                      (insert (make-string current-indent 32))))
                  ;; exiting inline LaTeX
                  (progn
                    (backward-char)
                    (poly-quarto-remove-whitespace-and-eol 't)
                    (forward-char)
                    (unless (looking-at "[ \n\t]")
                      (insert " "))))
              (if (looking-at "[$]")
                  ;; entering displayed LaTeX
                  ;; already done ?
                  (if (looking-at "[$]%\n")
                      (setq current-indent  (current-indentation))
                    (progn
                      (backward-char)
                      (poly-quarto-remove-whitespace-and-eol 't)
                      (insert ?\n)
                      (forward-char 2)
                      (unless (looking-at "%") (insert "%"))
                      (poly-quarto-remove-whitespace-and-eol)
                      (save-excursion
                        (beginning-of-line)
                        (indent--funcall-widened
                         (default-value 'indent-line-function))
                        (setq current-indent  (current-indentation)))
                      (insert ?\n)))
                ;; entering inline LaTeX
                (progn
                  (unless (looking-back "[ \t\n][$]" (- (point) 2))
                    (backward-char)
                    (insert " ")
                    (forward-char))
                  ;; clean after $
                  (poly-quarto-remove-whitespace-and-eol))))
            ;; update entered or not
            (setq entered (not entered))))))))

(defun poly-quarto-remove-whitespace-and-eol (&optional back)
  "Remove white spaces before or after point.
If BACK is non-nil remove before point. Return the number of
character deleted."
(let ((number-deleted 0))
  (save-excursion
    (if back (backward-char))
    (while (looking-at "[ \t\n]")
                (progn
                  (delete-char 1)
                  (setq number-deleted (+ number-deleted 1))
                  (if back (backward-char)))))
  number-deleted))

(defun poly-quarto-fix-only-alone-begin-end ()
  "Fix latex displayed delimiters.
It must begin by `$$` and ended by `$$` and not
by `\begin{align}` and `\end{align}` which must be inside the displayed block."
  (interactive)
  (let* ((regexp1 (concat "\\("
                          (mapconcat
                           (lambda(x) (concat "\\\\begin{" x "[*]?}"))
                           poly-quarto-latex-delims "\\|")
                          "\\)"))
         (regexp2 (concat "\\("
                          (mapconcat
                           (lambda(x) (concat "\\\\end{" x "[*]?}"))
                           poly-quarto-latex-delims "\\|")
                          "\\)"))
         (regexp (mapconcat #'identity (list regexp1 "\\|" regexp2)))
         (current-indent 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil 't)
        (if (match-beginning 1)
            ;; beginning
            (let ((mybeg (match-beginning 1))
                  (correction 0))
              ;; clean white space before \n$$
              (goto-char mybeg)
              (setq correction (poly-quarto-remove-whitespace-and-eol 't))
              (if (looking-back "[$][$]" (- (point) 2))
                  ;; case $$
                  (progn
                    (insert "%\n")
                    (save-excursion (beginning-of-line)
                     (indent--funcall-widened
                       (default-value 'indent-line-function))
                     (setq current-indent (current-indentation))))
                ;; other
                (if (looking-back "[$][$]%" (- (point) 3))
                    (progn
                      (setq current-indent (current-indentation))
                      (insert ?\n)
                      (insert (make-string current-indent 32)))
                      (progn
                        (insert "\n$$%\n")
                        (save-excursion
                         (forward-line -1)
                         (indent--funcall-widened
                   (default-value 'indent-line-function))
                         (setq current-indent  (current-indentation))
                         (forward-line 1)
                         (indent--funcall-widened
                   (default-value 'indent-line-function))))))
                    ;; return after roughly the beginning of match in regexp
              (forward-char 10))
                ;; ending
                (unless (looking-at "[ \t\n]*[$$]")
                  (insert "\n$$")
                  (save-excursion (beginning-of-line)
                    (insert (make-string current-indent 32)))))))))

(defun poly-quarto-fix-latex-delimiters ()
  "Fix latex delimiters to match rules for poly-quarto.
As polymode need by design disymmetric delimiters
`$$...$$` is replaced by `$$%...$$` and
`$...$` must be preceded by ` ` or `\t` or `\n`
and followed by `.` or ` ` or `\t` or `\n`."
  (interactive)
  (poly-quarto-fix-only-alone-begin-end)
  (poly-quarto-fix-only-dollar))

(defun poly-quarto-insert-align-block ()
  "Insert an align* displayed LaTeX block/chunk at point."
  (interactive)
  (let ((current-indent (current-indentation)))
  (unless (bolp) (insert "\n"))
  (insert (make-string current-indent 32))
  (insert "$$%\n")
  (insert (make-string current-indent 32))
  (insert "\\begin{align*}\n")
  (insert (make-string current-indent 32))
  (insert "\n")
  (insert (make-string current-indent 32))
  (insert "\\end{align*}\n")
  (insert (make-string current-indent 32))
  (insert "$$\n")
  (forward-line -3)
  (end-of-line)))
(defun poly-quarto-insert-latex-block ()
  "Insert a displayed chunk of LaTeX at point."
  (interactive)
  (let ((current-indent (current-indentation)))
  (unless (bolp) (insert "\n"))
  (insert (make-string current-indent 32))
  (insert "$$%\n")
  (insert (make-string current-indent 32))
  (insert "\n")
  (insert (make-string current-indent 32))
  (insert "$$\n")
  (forward-line -2)
  (end-of-line)))

(defun poly-quarto--codelang ()
  "Make 'poly-quarto-codelang' local variable."
  (make-local-variable  'poly-quarto-codelang))
(add-hook 'poly-quarto-mode-hook #'poly-quarto--codelang)

(defun poly-quarto-set-lang ()
  "Set/change 'poly-quarto-codelang' local variable."
  (interactive)
  (setq-local poly-quarto-codelang (completing-read "choose Lang: " poly-quarto-all-codelang nil t nil nil)))

(defun poly-quarto-insert-inline-code ()
  "Insert an inline code at point."
  (interactive)
  (insert "`{" poly-quarto-codelang "} `")
  (backward-char 1))

(defun poly-quarto-insert-codechunk ()
  "Insert chunk of prog."
  (interactive)
  (let ((current-indent (current-indentation)))
    (unless (bolp) (insert "\n"))
    (insert (make-string current-indent 32))
    (insert (concat "```{" poly-quarto-codelang "}"))
    (insert "\n")
    (insert (make-string current-indent 32))
    (insert "\n")
    (insert (make-string current-indent 32))
    (insert "```\n")
    (forward-line -2)
    (end-of-line)))

(defun poly-quarto--first-line-indent (span)
  "Return indentation of first line if not on a first line."
  (let ((pos (point)))
    (save-excursion
      (goto-char (nth 1 span))
      (when (not (bolp)) ; for spans which don't start at bol, first line is next line
        (forward-line 1))
      (skip-chars-forward " \t\n\r")
      (back-to-indentation)
      (- (point) (point-at-bol)))))
(defun poly-quarto--strip-indent (cmds indent)
  "Strip indentation INDENT in CMDS."
  (replace-regexp-in-string
   (concat "\n[\r]?" (make-string indent 32)) "\n"
   (string-remove-prefix (make-string indent 32) cmds)))

(defun poly-quarto-send-string-in-ess (string)
  "Set all ess variable and send string to process."
  (ess-force-buffer-current "Process to use: ")
   (unless ess-local-customize-alist
    (ess-setq-vars-local (symbol-value (ess-get-process-variable 'ess-local-customize-alist))))
     (ess-send-string (ess-get-process) string ess-eval-visibly "Eval region"))

(defun poly-quarto-send-chunk ()
  "Send chunk of code."
  (interactive)
  (let* ((span (pm-innermost-span))
         (indent (poly-quarto--first-line-indent span))
         (range (if (memq (car span) '(nil body))
                    (pm-span-to-range span)
                  (pm-chunk-range)))
         (beg (car range))
         (end (cdr range))
         (name (nth 1 (split-string (eieio-object-name-string (nth 3 span)) "::")))
         )
  (cond
   ;; R using ess
   ((string= "ess-r-mode" name)
    (if (and indent (> indent 0))
       (let
           ((cmds (poly-quarto--strip-indent
                   (buffer-substring-no-properties beg end) indent)))
         (poly-quarto-send-string-in-ess cmds))
      (ess-eval-region beg end nil)))
   ;; Python via python-mode
   ((string= "python-mode" name)
   (if (and indent (> indent 0))
       (let ((cmds (poly-quarto--strip-indent (buffer-substring-no-properties beg end) indent)))
             (message "Sent: %s..."
                      (substring cmds 0
                                 (min (string-match "\n" cmds) 40)))
             (python-shell-send-string cmds nil t))
     (python-shell-send-region beg end nil t))))))

(defun poly-quarto--ispell ()
  "Configure `ispell-skip-region-alist' for `poly-quarto-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--inline-code-beg poly-quarto-mode--inline-code-end))
   (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--fenced-code-beg poly-quarto-mode--fenced-code-end))
  (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--pandocblock-beg poly-quarto-mode--pandocblock-end))
  (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--latex-inline-beg-paren poly-quarto-mode--latex-inline-end-paren))
  (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--latex-inline-beg-dol poly-quarto-mode--latex-inline-end-dol))
  (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--latex-displayed-beg-bracket poly-quarto-mode--latex-displayed-end-bracket))
  (add-to-list 'ispell-skip-region-alist (cons poly-quarto-mode--latex-displayed-beg-dol poly-quarto-mode--latex-displayed-end-dol)))
(add-hook 'poly-quarto-mode-hook #'poly-quarto--ispell)

(easy-menu-define quarto-menu
  (list markdown-mode-map)
  "Menu for poly-quarto-mode."
  '("Quarto"
    ["Start Preview" poly-quarto-preview t]
    ["Fontify Buffer" poly-quarto-fontify-current-buffer]
    ["Fontify Around" poly-quarto-fontify-around-point]
    ["Fix LaTeX Delim"  poly-quarto-fix-latex-delimiters]))

;; (with-eval-after-load 'markdown-mode
     ;; (define-key markdown-mode-map "\C-c\C-l"   #'poly-quarto-insert-latex)
    (define-key markdown-mode-map (kbd "C-M-l")   #'poly-quarto-insert-latex-block)
    (define-key markdown-mode-map (kbd "C-M-a")   #'poly-quarto-insert-align-block)
    ;;  (define-key markdown-mode-map (kbd "\C-c\C-i") nil)
    ;; (define-key markdown-mode-map "\C-c\C-i"   #'poly-quarto-insert-chunk)
    (define-key markdown-mode-map (kbd "C-M-i")   #'poly-quarto-insert-codechunk)
    ;; (define-key markdown-mode-map "\C-c\C-c" #'poly-quarto-send-chunk)
;; (with-eval-after-load 'polymode
  (define-key polymode-mode-map "\C-c\C-c" #'poly-quarto-send-chunk)

;; div block mode
(defvar pandocblock-mode-font-lock-keyword-face
  (list
   (cons "[a-zA-Z0-9_-]+[ \t]*="
             font-lock-keyword-face)))
(defvar pandocblock-mode-font-classes-face
  (list
   (cons "[.][a-zA-Z0-9-_]+"
             font-lock-type-face)))
(defvar pandocblock-mode-font-id-face
  (list
   (cons "#[a-zA-Z0-9-_]+"
             font-lock-constant-face)))

(defvar pandocblock-mode-font-lock-defaults
  (append
   pandocblock-mode-font-lock-keyword-face
   pandocblock-mode-font-classes-face
   pandocblock-mode-font-id-face))

(define-derived-mode pandocblock-mode prog-mode "pandoc"
  "Major mode for div header block in poly-quarto."
  :group 'pandocblock-mode
   (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(pandocblock-mode-font-lock-defaults nil t)))
(add-to-list 'hs-special-modes-alist
             '(markdown-mode ":::" ":::" "<!--"))

(add-to-list 'polymode-mode-abbrev-aliases '(("ess-r" . "R") ("pandocblock" . "pandoc")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(provide 'poly-quarto)
;;; poly-quarto.el ends here
