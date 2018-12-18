;;; clang-format.el --- Format code using clang-format  -*- lexical-binding: t; -*-

;; Keywords: tools, c
;; Package-Requires: ((cl-lib "0.3"))

;;; Commentary:

;; This package allows to filter code through clang-format to fix its formatting.
;; clang-format is a tool that formats C/C++/Obj-C code according to a set of
;; style options, see <http://clang.llvm.org/docs/ClangFormatStyleOptions.html>.
;; Note that clang-format 3.4 or newer is required.

;; clang-format.el is available via MELPA and can be installed via
;;
;;   M-x package-install clang-format
;;
;; when ("melpa" . "http://melpa.org/packages/") is included in
;; `package-archives'.  Alternatively, ensure the directory of this
;; file is in your `load-path' and add
;;
;;   (require 'clang-format)
;;
;; to your .emacs configuration.

;; You may also want to bind `clang-format-region' to a key:
;;
;;   (global-set-key [C-M-tab] 'clang-format-region)

;;; Code:

(require 'cl-lib)
(require 'xml)

(defgroup clang-format nil
  "Format code using clang-format."
  :group 'tools)

(defcustom clang-format-executable
  (or (executable-find "clang-format")
      "clang-format")
  "Location of the clang-format executable.

A string containing the name or the full path of the executable."
  :group 'clang-format
  :type '(file :must-match t)
  :risky t)

(defcustom clang-format-style "file"
  "Style argument to pass to clang-format.

By default clang-format will load the style configuration from
a file named .clang-format located in one of the parent directories
of the buffer."
  :group 'clang-format
  :type 'string
  :safe #'stringp)
(make-variable-buffer-local 'clang-format-style)

(defconst clang-format--git-diff-summary-re "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?")

(defun clang-format--parse-git-diff-to-changed-regions (diff-result-lines)
  (let ((ranges nil))
    (dolist (line diff-result-lines)
      (when (string-match clang-format--git-diff-summary-re line)
        (setq ranges (add-to-list 'ranges
                                  (let ((start (string-to-int (match-string 1 line)))
                                        (length (string-to-int (if-let ((len (match-string 3 line))) len "1"))))
                                    ;; clang-format -liens start:end, the end is inclusive
                                    `(,start . ,(+ start (- length 1))))
                                  t))))
    ranges))

(defun clang-format--git-diff-changed-regions (file-path)
  (let ((diff-result (shell-command-to-string
                      (concat "cd " (file-name-directory file-path)
                              " && git diff HEAD -U0 -- "
                              (file-name-nondirectory file-path)))))
    (clang-format--parse-git-diff-to-changed-regions (split-string diff-result "\n"))))

(defun clang-format--git-diff-buffer-changed-regions (buffer-name)
  (interactive "b")
  (clang-format--git-diff-changed-regions (buffer-file-name (get-buffer buffer-name))))

(defun clang-format--extract (xml-node)
  "Extract replacements and cursor information from XML-NODE."
  (unless (and (listp xml-node) (eq (xml-node-name xml-node) 'replacements))
    (error "Expected <replacements> node"))
  (let ((nodes (xml-node-children xml-node))
        (incomplete-format (xml-get-attribute xml-node 'incomplete_format))
        replacements
        cursor)
    (dolist (node nodes)
      (when (listp node)
        (let* ((children (xml-node-children node))
               (text (car children)))
          (cl-case (xml-node-name node)
            ('replacement
             (let* ((offset (xml-get-attribute-or-nil node 'offset))
                    (length (xml-get-attribute-or-nil node 'length)))
               (when (or (null offset) (null length))
                 (error "<replacement> node does not have offset and length attributes"))
               (when (cdr children)
                 (error "More than one child node in <replacement> node"))

               (setq offset (string-to-number offset))
               (setq length (string-to-number length))
               (push (list offset length text) replacements)))
            ('cursor
             (setq cursor (string-to-number text)))))))

    ;; Sort by decreasing offset, length.
    (setq replacements (sort (delq nil replacements)
                             (lambda (a b)
                               (or (> (car a) (car b))
                                   (and (= (car a) (car b))
                                        (> (cadr a) (cadr b)))))))

    (list replacements cursor (string= incomplete-format "true"))))

(defun clang-format--replace (offset length &optional text)
  "Replace the region defined by OFFSET and LENGTH with TEXT.
OFFSET and LENGTH are measured in bytes, not characters.  OFFSET
is a zero-based file offset, assuming ‘utf-8-unix’ coding."
  (let ((start (clang-format--filepos-to-bufferpos offset 'exact 'utf-8-unix))
        (end (clang-format--filepos-to-bufferpos (+ offset length) 'exact
                                                 'utf-8-unix)))
    (goto-char start)
    (delete-region start end)
    (when text
      (insert text))))

;; ‘bufferpos-to-filepos’ and ‘filepos-to-bufferpos’ are new in Emacs 25.1.
;; Provide fallbacks for older versions.
(defalias 'clang-format--bufferpos-to-filepos
  (if (fboundp 'bufferpos-to-filepos)
      'bufferpos-to-filepos
    (lambda (position &optional _quality _coding-system)
      (1- (position-bytes position)))))

(defalias 'clang-format--filepos-to-bufferpos
  (if (fboundp 'filepos-to-bufferpos)
      'filepos-to-bufferpos
    (lambda (byte &optional _quality _coding-system)
      (byte-to-position (1+ byte)))))

;;;###autoload
(defun clang-format-region (regions count-by-lines &optional style assume-file-name)
  "Use clang-format to format the code in REGIONS according to STYLE.
If called interactively uses the region or the current statement if there is no
no active region. If no STYLE is given uses `clang-format-style'. Use
ASSUME-FILE-NAME to locate a style config file, if no ASSUME-FILE-NAME is given
uses the function `buffer-file-name'."
  (interactive
   (if (use-region-p)
       (list (list `(,(clang-format--bufferpos-to-filepos (region-beginning) 'approximate 'utf-8-unix) .
                     ,(clang-format--bufferpos-to-filepos (region-end) 'approximate 'utf-8-unix)))
             nil)
     (list (list `(,(clang-format--bufferpos-to-filepos (point) 'approximate 'utf-8-unix) .
                   ,(clang-format--bufferpos-to-filepos (point) 'approximate 'utf-8-unix)))
           nil)))

  (unless regions (return))

  (unless style
    (setq style clang-format-style))

  (unless assume-file-name
    (setq assume-file-name buffer-file-name))

  (let ((cursor (clang-format--bufferpos-to-filepos (point) 'exact 'utf-8-unix))
        (temp-buffer (generate-new-buffer " *clang-format-temp*"))
        (temp-file (make-temp-file "clang-format"))
        ;; Output is XML, which is always UTF-8.  Input encoding should match
        ;; the encoding used to convert between buffer and file positions,
        ;; otherwise the offsets calculated above are off.  For simplicity, we
        ;; always use ‘utf-8-unix’ and ignore the buffer coding system.
        (default-process-coding-system '(utf-8-unix . utf-8-unix)))
    (unwind-protect
        (let* ((clang-format-region-options (mapcan (lambda (pair)
                                                      (let ((start (car pair)) (end (cdr pair)))
                                                        (if count-by-lines
                                                            (list "-lines" (concat (number-to-string start) ":" (number-to-string end)))
                                                          (list "-offset" (number-to-string start)
                                                                "-length" (number-to-string (- end start))))))
                                                    regions))
               (status (apply #'call-process-region
                              nil nil clang-format-executable
                              nil `(,temp-buffer ,temp-file) nil
                              (append
                               `("-output-replacements-xml"
                                 ;; Guard against a nil assume-file-name.
                                 ;; If the clang-format option -assume-filename
                                 ;; is given a blank string it will crash as per
                                 ;; the following bug report
                                 ;; https://bugs.llvm.org/show_bug.cgi?id=34667
                                 ,@(and assume-file-name
                                        (list "-assume-filename" assume-file-name))
                                 "-style"  ,style
                                 "-cursor" ,(number-to-string cursor))
                               clang-format-region-options)))
               (stderr (with-temp-buffer
                         (unless (zerop (cadr (insert-file-contents temp-file)))
                           (insert ": "))
                         (buffer-substring-no-properties
                          (point-min) (line-end-position)))))
          (cond
           ((stringp status)
            (error "(clang-format killed by signal %s%s)" status stderr))
           ((not (zerop status))
            (error "(clang-format failed with code %d%s)" status stderr)))

          (cl-destructuring-bind (replacements cursor incomplete-format)
              (with-current-buffer temp-buffer
                (clang-format--extract (car (xml-parse-region))))
            (save-excursion
              (dolist (rpl replacements)
                (apply #'clang-format--replace rpl)))
            (when cursor
              (goto-char (clang-format--filepos-to-bufferpos cursor 'exact
                                                             'utf-8-unix)))
            (if incomplete-format
                (message "(clang-format: incomplete (syntax errors)%s)" stderr)
              (message "(clang-format: success%s)" stderr))))
      (delete-file temp-file)
      (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))

;;;###autoload
(defun clang-format-buffer (&optional style assume-file-name)
  "Use clang-format to format the current buffer according to STYLE.
If no STYLE is given uses `clang-format-style'. Use ASSUME-FILE-NAME
to locate a style config file. If no ASSUME-FILE-NAME is given uses
the function `buffer-file-name'."
  (interactive)
  (clang-format-region (list `(,(clang-format--bufferpos-to-filepos (point-min) 'approximate 'utf-8-unix) .
                               ,(clang-format--bufferpos-to-filepos (point-max) 'approximate 'utf-8-unix)))
                       nil style assume-file-name))

;;;###autoload
(defun git-clang-format-buffer (&optional style assume-file-name)
  "Use clang-format to format the changed part of current buffer according to STYLE.
If no STYLE is given uses `clang-format-style'. Use ASSUME-FILE-NAME
to locate a style config file. If no ASSUME-FILE-NAME is given uses
the function `buffer-file-name'."
  (interactive)
  (clang-format-region (clang-format--git-diff-buffer-changed-regions (buffer-name)) t style assume-file-name))

;;;###autoload
(defalias 'clang-format 'clang-format-region)

(provide 'clang-format)
;;; clang-format.el ends here
