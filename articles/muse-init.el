(add-to-list 'load-path "~/.emacs.d/muse/lisp")
(require 'muse-mode)
(require 'muse-html)
(setq muse-xhtml-style-sheet "")
(setq muse-xhtml-header "\n<h1><lisp>(muse-publishing-directive \"title\")</lisp></h1>\n<h4><lisp>(muse-publishing-directive \"author\")</lisp></h4>\n")
(setq muse-xhtml-footer "")
(setq muse-publish-contents-depth 1)
; The following is currently broken for depth > 1
(defun muse-html-insert-contents (depth)
  "Scan the current document and generate a table of contents at point.
DEPTH indicates how many levels of headings to include.  The default is 2."
  (let ((max-depth (or depth 2))
        (index 1)
        base contents l end)
    (save-excursion
      (goto-char (point-min))
      (search-forward "Page published by Emacs Muse begins here" nil t)
      (catch 'done
        (while (re-search-forward "<h\\([0-9]+\\)>\\(.+?\\)</h\\1>$" nil t)
          (unless (and (get-text-property (point) 'read-only)
                       (not (get-text-property (match-beginning 0)
                                               'muse-contents)))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(muse-contents nil))
            (setq l (1- (string-to-number (match-string 1))))
            (if (null base)
                (setq base l)
              (if (< l base)
                  (throw 'done t)))
            (when (<= l max-depth)
              ;; escape specials now before copying the text, so that we
              ;; can deal sanely with both emphasis in titles and
              ;; special characters
              (goto-char (match-end 2))
              (setq end (point-marker))
              (muse-publish-escape-specials (match-beginning 2) end
                                            nil 'document)
              (muse-publish-mark-read-only (match-beginning 2) end)
              (setq contents (cons (cons l (buffer-substring-no-properties
                                            (match-beginning 2) end))
                                   contents))
              (set-marker end nil)
              (goto-char (match-beginning 2))
              (muse-html-insert-anchor (concat "sec" (int-to-string index)))
              (setq index (1+ index)))))))
    (setq index 1 contents (nreverse contents))
    (let ((depth 1) (sub-open 0) (p (point)))
      (muse-insert-markup "<div class=\"contents\">\n<h3>Contents</h3>\n<ol>\n")
      (while contents
        (muse-insert-markup "<li>\n"
                            "<a href=\"#sec" (int-to-string index) "\">"
                            (muse-html-strip-links (cdar contents))
                            "</a>\n"
                            "</li>\n")
        (setq index (1+ index)
              depth (caar contents)
              contents (cdr contents))
        (if contents
          (cond
           ((< (caar contents) depth)
            (let ((idx (caar contents)))
              (while (< idx depth)
                (muse-insert-markup "</ol>\n")
                (setq sub-open (1- sub-open)
                      idx (1+ idx)))))
           ((> (caar contents) depth) ; can't jump more than one ahead
            (muse-insert-markup "<ol>\n")
            (setq sub-open (1+ sub-open))))
          (muse-insert-markup "</li>\n")))
      (while (> sub-open 0)
        (muse-insert-markup "</ol>\n")
        (setq sub-open (1- sub-open)))
      (muse-insert-markup "</ol>\n</div>\n")
      (muse-publish-mark-read-only p (point)))))
