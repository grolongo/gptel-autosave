;;; gptel-autosave.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2026 grolongo
;; Author: grolongo
;; Version: 1.0
;; Package-Requires: ((gptel "0.9.9.6))
;; URL: https://github.com/grolongo/gptel-autosave.el
;; Keywords: ai, gpt, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package automatically saves a conversation when using the package gptel.

;;; Code:

(require 'gptel)

(defgroup gptel-autosave nil
  "Manage how gptel-autosave saves files."
  :prefix "gptel-autosave-"
  :group 'convenience)

(defcustom gptel-autosave-directory
  (expand-file-name "gptel_conversations" user-emacs-directory)
  "Directory where gptel-autosave will save conversations."
  :group 'gptel-autosave
  :type 'directory)

(defcustom gptel-autosave-slug-length 6
  "Maximum number of words to use for the filename."
  :group 'gptel-autosave
  :type 'natnum)

(defcustom gptel-autosave-stopwords
  '(;; pronouns
    "i" "me" "my" "myself" "we" "our" "ours" "ourselves"
    "you" "you're" "you've" "you'll" "you'd" "your" "yours"
    "yourself" "yourselves" "he" "him" "his" "himself"
    "she" "she's" "her" "hers" "herself" "it" "it's" "its" "itself"
    "they" "them" "their" "theirs" "themselves"
    "what" "which" "who" "whom"

    ;; articles & determiners
    "this" "that" "that'll" "these" "those" "a" "an" "the"
    "all" "any" "both" "each" "few" "more" "most" "other"
    "some" "such" "no" "own" "same"

    ;; common verbs
    "am" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "having" "do" "does" "did" "doing"

    ;; prepositions
    "of" "at" "by" "for" "with" "about" "against" "between"
    "into" "through" "during" "before" "after" "above" "below"
    "to" "from" "up" "down" "in" "out" "on" "off" "over" "under"

    ;; conjunctions
    "and" "but" "if" "or" "because" "as" "until" "while"
    "nor" "so" "than"

    ;; modal / auxiliary verbs
    "can" "will" "should" "should've"
    "don" "don't" "aren" "aren't" "couldn" "couldn't"
    "didn" "didn't" "doesn" "doesn't" "hadn" "hadn't"
    "hasn" "hasn't" "haven" "haven't" "isn" "isn't"
    "mightn" "mightn't" "mustn" "mustn't" "needn" "needn't"
    "shan" "shan't" "shouldn" "shouldn't" "wasn" "wasn't"
    "weren" "weren't" "won" "won't" "wouldn" "wouldn't"

    ;; other: negation, adverbs, intensifiers, and leftover
    ;; contraction fragments (s/t/d/ll/m/o/re/ve/y come from
    ;; splitting words like "it's" -> "it" + "s")
    "again" "further" "then" "once" "here" "there" "when"
    "where" "why" "how" "not" "only" "too" "very" "just" "now"
    "s" "t" "d" "ll" "m" "o" "re" "ve" "y" "ain" "ma")
  "List of words to strip from the slug.
Defaults to the NLTK English stopwords corpus, grouped by
part of speech. See comments in the source for grouping."
  :group 'gptel-autosave
  :type '(repeat string))

(defun gptel-autosave--slugify (text)
  "Turn the first few words of TEXT into a filename-safe slug."
  (let* ((clean
          (replace-regexp-in-string
           "[^[:alnum:][:space:]-]" "" (downcase text)))
         (second-clean
          (replace-regexp-in-string
           (concat "\\b" (regexp-opt gptel-autosave-stopwords) "\\b") ""
           clean))
         (words (seq-take (split-string second-clean) gptel-autosave-slug-length))
         (slug (string-join words "-")))
    (if (string-empty-p slug)
        "untitled"
      slug)))

(defun gptel-autosave--extension ()
  "Retrieve the current major-mode of the gptel buffer to determine
the correct extension."
  (cond ((derived-mode-p 'org-mode) "org")
        ((derived-mode-p 'markdown-mode) "md")
        (t "txt")))

(defun gptel-autosave--unique-filename (dir slug extension)
  "Return an unused autosave filename for DIR, SLUG and EXTENSION."
  (let* ((filename (concat slug "." extension))
         (filepath (expand-file-name filename dir))
         (counter 2))
    (while (file-exists-p filepath)
      (setq filepath (expand-file-name
                      (format "%s-%d.%s" slug counter extension) dir))
      (setq counter (1+ counter)))
    filepath))

(defun gptel-autosave--save (beg _end)
  "Save the current gptel buffer using an unique filename."
  (when (bound-and-true-p gptel-mode)
    (unless (file-directory-p gptel-autosave-directory)
      (make-directory gptel-autosave-directory t))
    (if (buffer-file-name)
        (let ((inhibit-message t))
          (save-buffer))
      (let* ((first-question (buffer-substring-no-properties (point-min) beg))
             (slug (gptel-autosave--slugify first-question)))
        (let ((inhibit-message t))
          (write-file
           (gptel-autosave--unique-filename
            gptel-autosave-directory slug
            (gptel-autosave--extension))))))))

(defun gptel-autosave--mode-line-order ()
  "Place gptel-autosave lighter immediately after
gptel lighter in the mode line."
  (let ((gptel-entry (assq 'gptel-mode minor-mode-alist))
        (autosave-entry (assq 'gptel-autosave-mode minor-mode-alist)))
    (when (and gptel-entry autosave-entry)
      (setq minor-mode-alist
            (cons gptel-entry
                  (cons autosave-entry
                        (delq gptel-entry
                              (delq autosave-entry minor-mode-alist))))))))

(add-hook 'gptel-mode-hook #'gptel-autosave--mode-line-order)

(defun gptel-autosave--enable ()
  (add-hook 'gptel-post-response-functions #'gptel-autosave--save nil t))

(defun gptel-autosave--disable ()
  (remove-hook 'gptel-post-response-functions #'gptel-autosave--save t))

;;;###autoload

(define-minor-mode gptel-autosave-mode
  "Toggle automatic saving of conversations in gptel buffers."
  :group 'gptel-autosave
  :lighter " Autosave"
  (if gptel-autosave-mode
      (if (bound-and-true-p gptel-mode)
          (progn
            (gptel-autosave--enable)
            (message "gptel-autosave mode enabled in current buffer"))
        (setq gptel-autosave-mode nil)
        (user-error
         (format
          "`gptel-mode' and `gptel-autosave-mode' are not supported in `%s'"
          major-mode)))
    (gptel-autosave--disable)
    (message "gptel-autosave mode disabled in current buffer")))

(provide 'gptel-autosave)
;;; gptel-autosave.el ends here
