;;; vue-ts-mode.el --- Major mode for editing Vue templates -*- lexical-binding: t; -*-

;; Copyright (C) 2023 8uff3r
;; Copyright (C) 2026 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: 8uff3r <8uff3r@gmail.com>
;; Maintainer: Karim Aziiev <karim.aziiev@gmail.com>
;; Homepage: https://github.com/KarimAziev/vue-ts-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "30"))
;; Keywords: languages
;; URL: https://github.com/KarimAziev/vue-ts-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode with syntax highlighting for Vue
;; templates. It leverages Emacs' built-in tree-sitter support, as well as
;; ikatyang's tree-sitter grammar for Vue.

;; More info:
;; README: https://github.com/KarimAziev/vue-ts-mode
;; tree-sitter-vue: https://github.com/ikatyang/tree-sitter-vue
;; Vue: https://vuejs.org//

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)
(require 'typescript-ts-mode)
(require 'css-mode)
(require 'transient)


;; Indent Rules
(defcustom vue-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `vue-ts-mode'."
  :type 'integer
  :group 'vue
  :package-version '(vue-ts-mode . "1.0.0"))



(eval-and-compile
  (defun vue-ts-mode--expand (init-fn)
    "If INIT-FN is a non-quoted symbol, add a sharp quote.
Otherwise, return it as is."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defmacro vue-ts-mode--rpartial (fn &rest args)
  "Return a partial application of a function FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (let ((pre-args (make-symbol "pre-args")))
    `(lambda (&rest ,pre-args)
       ,(car (list
              `(apply ,@(vue-ts-mode--expand fn)
                (append ,pre-args (list ,@args))))))))


(defvar vue-ts-mode--indent-rules
  `((vue
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "text") grand-parent vue-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol 0)
     ((parent-is "style_element") parent-bol 0)
     ((parent-is "template_element") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol vue-ts-mode-indent-offset))
    (css . ,(append (alist-get 'css css--treesit-indent-rules)
             '(((parent-is "stylesheet") parent-bol 0))))
    (typescript . ,(alist-get 'typescript (typescript-ts-mode--indent-rules
                                           'typescript))))
  "Tree-sitter indentation rules for `vue-ts-mode'.")

;; font-lock rules
(defface vue-ts-mode-template-tag-bracket-face
  '((t :foreground "#86e1fc"))
  "Face for html tags angle brackets (<, > and />)."
  :group 'vue-ts-mode-faces)

(defconst vue-ts-mode--html-entity-regexp
  "&\\(?:[[:alpha:]][[:alnum:]]*\\|#[[:digit:]]+\\|#[xX][[:xdigit:]]+\\);"
  "Regexp matching HTML named and numeric character references.")

(defun vue-ts-mode--match-html-entity (limit)
  "Search for an HTML entity before LIMIT in Vue template code."
  (catch 'match
    (while (re-search-forward vue-ts-mode--html-entity-regexp limit t)
      (when (eq (vue-ts-mode--treesit-language-at-point (match-beginning 0))
                'vue)
        (throw 'match t)))
    nil))


(defvar vue-ts-mode--font-lock-keywords
  '((vue-ts-mode--match-html-entity . font-lock-constant-face))
  "Additional non-tree-sitter font-lock keywords for `vue-ts-mode'.")

(defun vue-ts-mode--fontify-directive-attribute (node override start end &rest _)
  "Fontify Vue directive attribute NODE.

Arguments OVERRIDE, START and END have the same meaning as in
`treesit-fontify-with-override'.

Remaining arguments _ are ignored."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (name-end nil)
         (arg-start nil)
         (shorthand-bind nil)
         (bind-directive nil)
         (value-start nil))
    (save-excursion
      (goto-char node-start)
      (when (looking-at "\\(?:v-[^<>'\"=/[:space:].:]+\\|[:@#]\\)")
        (setq name-end (match-end 0))
        (setq shorthand-bind (string= (match-string 0) ":"))
        (setq bind-directive (or shorthand-bind
                                 (string= (match-string 0) "v-bind")))
        (treesit-fontify-with-override
         (match-beginning 0) name-end
         'font-lock-keyword-face override start end)
        (setq arg-start
              (cond
               ((memq (char-after node-start) '(?: ?@ ?#))
                (1+ node-start))
               ((and (< name-end node-end) (eq (char-after name-end) ?:))
                (1+ name-end))))
        (when arg-start
          (goto-char arg-start)
          (if (eq (char-after) ?\[)
              (when-let* ((close-bracket
                           (save-excursion
                             (search-forward "]" node-end t))))
                (treesit-fontify-with-override
                 arg-start (1+ arg-start)
                 'font-lock-bracket-face override start end)
                (treesit-fontify-with-override
                 (1+ arg-start)
                 (1- close-bracket)
                 (if bind-directive
                     'font-lock-builtin-face
                   'font-lock-variable-name-face)
                 override start end)
                (treesit-fontify-with-override
                 (1- close-bracket) close-bracket
                 'font-lock-bracket-face override start end))
            (when (looking-at "[^<>'\"=/[:space:].]+")
              (treesit-fontify-with-override
               (match-beginning 0)
               (match-end 0)
               (if bind-directive
                   'font-lock-builtin-face
                 'font-lock-property-name-face)
               override start end))))
        (when (setq value-start
                    (save-excursion
                      (search-forward "=" node-end t)))
          (unless shorthand-bind
            (treesit-fontify-with-override
             (1- value-start) value-start
             'font-lock-keyword-face override start end)))
        (goto-char (or arg-start name-end))
        (while (re-search-forward
                "\\.\\([^<>'\"=/[:space:].]+\\)"
                (or (and value-start (1- value-start)) node-end)
                t)
          (treesit-fontify-with-override
           (match-beginning 1)
           (match-end 1)
           'font-lock-constant-face override start end))))))

(defun vue-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (let ((copy (copy-sequence setting)))
              (setcar (nthcdr 2 copy)
                      (intern (format "%s-%s" prefix (nth 2 setting))))
              copy))
          settings))


(defun vue-ts-mode--font-lock-settings ()
  "Return font-lock settings for `vue-ts-mode'."
  (append
   (vue-ts-mode--prefix-font-lock-features
    "typescript"
    (typescript-ts-mode--font-lock-settings 'typescript))

   (vue-ts-mode--prefix-font-lock-features
    "css" css--treesit-settings)

   (treesit-font-lock-rules
    :language 'vue
    :override t
    :feature 'vue-ref
    '((element (_ (attribute
                   (attribute_name)
                   @font-lock-type-face
                   (:equal @font-lock-type-face "ref")
                   (quoted_attribute_value
                    (attribute_value)
                    @font-lock-variable-name-face)))))

    :language 'vue
    :override t
    :feature 'vue-sp-dir
    '((_ (_ (directive_attribute
             (directive_name)
             @font-lock-type-face
             (:match "\\`\\(v-if\\|v-for\\|v-model\\|v-else\\|v-else-if\\)\\'"
                     @font-lock-type-face)))))

    :language 'vue
    :feature 'vue-attr
    '((attribute_name) @font-lock-property-name-face)

    :language 'vue
    :override t
    :feature 'vue-definition
    '((tag_name) @font-lock-function-name-face)

    :language 'vue
    :override t
    :feature 'vue-directive
    '((directive_attribute) @vue-ts-mode--fontify-directive-attribute)

    :language 'vue
    :feature 'vue-directive-value
    '((directive_attribute
       [(attribute_value)
        (quoted_attribute_value (attribute_value))]
       @font-lock-variable-name-face))

    :language 'vue
    :override t
    :feature 'vue-bracket
    '(([ "{{" "}}" "<" ">" "</" "/>"]) @vue-ts-mode-template-tag-bracket-face)

    :language 'vue
    :feature 'vue-string
    '((attribute (quoted_attribute_value) @font-lock-string-face))

    :language 'typescript
    :override t
    :feature 'typescript-custom-property
    '(((property_identifier) @font-lock-property-name-face))

    :language 'typescript
    :override t
    :feature 'typescript-custom-variable
    '(((identifier) @font-lock-variable-name-face))

    :language 'typescript
    :override 't
    :feature 'typescript-custom-function
    '((call_expression
       function:
       [(identifier) @font-lock-function-call-face
        (member_expression
         property: (property_identifier) @font-lock-function-call-face)])))))

(defvar vue-ts-mode--range-settings
  (treesit-range-rules

   :embed 'typescript
   :host 'vue
   '((script_element (raw_text) @capture))

   :embed 'typescript
   :host 'vue
   :local 't
   '((interpolation (raw_text) @capture))

   :embed 'typescript
   :host 'vue
   :local 't
   '((directive_attribute
      (quoted_attribute_value (attribute_value) @capture)))

   :embed 'css
   :host 'vue
   '((style_element (raw_text) @capture))))

(defun vue-ts-mode--advice-for-treesit-buffer-root-node (&optional lang)
  "Return the current ranges for the LANG parser in the current buffer.

If LANG is omitted, return ranges for the first language in the parser list.

If `major-mode' is currently `vue-ts-mode', or if LANG is vue, this function
instead always returns t."
  (if (or (eq lang 'vue) (not (eq major-mode 'vue-ts-mode)))
      t
    (treesit-parser-included-ranges
     (treesit-parser-create

      (or lang (treesit-parser-language (car (treesit-parser-list))))))))

(defun vue-ts-mode--advice-for-treesit--merge-ranges (_ new-ranges _ _)
  "Return truthy if `major-mode' is `vue-ts-mode', and if NEW-RANGES is non-nil."
  (and (eq major-mode 'vue-ts-mode) new-ranges))

(defun vue-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "tag_name")
    (treesit-node-text node t)))

(defun vue-ts-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((range nil)
         (language-in-range
          (cl-loop
           for parser in (treesit-parser-list)
           do (setq range
                    (cl-loop
                     for range in (treesit-parser-included-ranges parser)
                     if (and (>= point (car range)) (<= point (cdr range)))
                     return parser))
           if range
           return (treesit-parser-language parser))))
    (or language-in-range 'vue)))

(defun vue-ts-mode--comment-for-language-at-point ()
  "Return the comment syntax for language at point."
  (let ((lang (vue-ts-mode--treesit-language-at-point (point))))
    (cond ((equal lang 'vue) `(:start "<!-- "
                               :start-skip "<!--[ \t]*"
                               :end " -->"
                               :end-skip "[ \t]*--[ \t\n]*>"))
          ((equal lang 'typescript) `(:start "// "
                                      :start-skip "\\(?://+\\|/\\*+\\)\\s-*"
                                      :end ""
                                      :end-skip "\\s-*\\(\\s>\\|\\*+/\\)"))
          ((equal lang 'css) `(:start "/*"
                               :start-skip "/\\*+[ \t]*"
                               :end "*/"
                               :end-skip "[ \t]*\\*+/")))))

(defun vue-ts-mode--advice-for-comment-fns (fn &rest args)
  "Apply FN with comment syntax for the language at point.

Argument FN is the function to call with comment syntax bindings.

Remaining arguments ARGS are the arguments passed to FN."
  (if (equal major-mode 'vue-ts-mode)
      (let* ((comment-vars (vue-ts-mode--comment-for-language-at-point))
             (comment-start (plist-get comment-vars :start))
             (comment-start-skip (plist-get comment-vars :start-skip))
             (comment-end (plist-get comment-vars :end))
             (comment-end-skip (plist-get comment-vars :end-skip)))
        (apply fn args))
    (apply fn args)))

;;; Element navigation

(defconst vue-ts-mode--element-node-types
  '("element" "template_element" "script_element" "style_element")
  "Tree-sitter node types that represent Vue elements.")

(defconst vue-ts-mode--element-node-type-regexp
  (regexp-opt vue-ts-mode--element-node-types))

(defconst vue-ts-mode--element-content-node-types
  '("text" "interpolation" "raw_text")
  "Tree-sitter node types that are useful child navigation targets.")

(defconst vue-ts-mode--tag-node-types
  '("start_tag" "self_closing_tag")
  "Tree-sitter node types that can hold Vue attributes.")

(defconst vue-ts-mode--attribute-node-types
  '("attribute" "directive_attribute")
  "Tree-sitter node types that represent Vue attributes.")

(defconst vue-ts-mode--attribute-value-node-types
  '("attribute_value" "quoted_attribute_value")
  "Tree-sitter node types that represent Vue attribute values.")

(defun vue-ts-mode--treesit-node-type-p (node type)
  "Return non-nil if NODE's type is TYPE.
TYPE may be a string, a list of strings, or a regexp."
  (when node
    (let ((node-type (treesit-node-type node)))
      (cond
       ((stringp type) (string-match-p type node-type))
       ((listp type) (member node-type type))))))

(defun vue-ts-mode--element-node-p (node)
  "Return non-nil if NODE is a Vue element node."
  (vue-ts-mode--treesit-node-type-p node vue-ts-mode--element-node-types))

(defun vue-ts-mode--tag-node-p (node)
  "Return non-nil if NODE is a Vue start or self-closing tag node."
  (vue-ts-mode--treesit-node-type-p node vue-ts-mode--tag-node-types))

(defun vue-ts-mode--attribute-node-p (node)
  "Return non-nil if NODE is a Vue attribute node."
  (vue-ts-mode--treesit-node-type-p node vue-ts-mode--attribute-node-types))

(defun vue-ts-mode--attribute-value-node-p (node)
  "Return non-nil if NODE is a Vue attribute value node."
  (vue-ts-mode--treesit-node-type-p
   node vue-ts-mode--attribute-value-node-types))

(defun vue-ts-mode--treesit-parent-until (node predicate)
  "Return NODE or its nearest parent satisfying PREDICATE."
  (while (and node (not (funcall predicate node)))
    (setq node (treesit-node-parent node)))
  node)

(defun vue-ts-mode--node-at-pos (&optional pos)
  "Return the Vue node at POS.
When POS is nil, use point.  If POS is at the end of a node, also
try the preceding character."
  (let ((pos (or pos (point))))
    (or (treesit-node-at pos 'vue)
        (and (> pos (point-min))
             (treesit-node-at (1- pos) 'vue)))))

(defun vue-ts-mode--element-at-pos (&optional pos)
  "Return the nearest element node at POS.
When POS is nil, use point."
  (let ((node (vue-ts-mode--node-at-pos pos)))
    (vue-ts-mode--treesit-parent-until node #'vue-ts-mode--element-node-p)))

(defun vue-ts-mode--element-parent (node)
  "Return NODE's nearest parent element node."
  (vue-ts-mode--treesit-parent-until
   (treesit-node-parent node)
   #'vue-ts-mode--element-node-p))

(defun vue-ts-mode--node-contains-pos-p (node pos)
  "Return non-nil if NODE contains POS."
  (and node
       (<= (treesit-node-start node)
           pos)
       (< pos
          (treesit-node-end node))))

(defun vue-ts-mode--element-start-tag (element)
  "Return ELEMENT's start or self-closing tag node."
  (cl-find-if #'vue-ts-mode--tag-node-p
              (treesit-node-children element)))

(defun vue-ts-mode--element-end-tag (element)
  "Return ELEMENT's end tag node, if any."
  (cl-find-if
   (lambda (node)
     (string= (treesit-node-type node) "end_tag"))
   (treesit-node-children element)))

(defun vue-ts-mode--tag-at-pos (&optional pos)
  "Return the Vue tag node at POS.
When POS is nil, use point."
  (let ((node (vue-ts-mode--node-at-pos pos)))
    (vue-ts-mode--treesit-parent-until node #'vue-ts-mode--tag-node-p)))

(defun vue-ts-mode--attribute-value-at-pos (&optional pos)
  "Return the attribute value node at POS, if any."
  (vue-ts-mode--treesit-parent-until
   (vue-ts-mode--node-at-pos pos)
   #'vue-ts-mode--attribute-value-node-p))

(defun vue-ts-mode--element-sibling (node backward)
  "Return NODE's previous or next element sibling.
If BACKWARD is non-nil, return the previous sibling.  Otherwise,
return the next sibling."
  (let ((sibling-fn (if backward
                        #'treesit-node-prev-sibling
                      #'treesit-node-next-sibling))
        sibling)
    (while (and node
                (setq sibling (funcall sibling-fn node t))
                (not (vue-ts-mode--element-node-p sibling)))
      (setq node sibling))
    (and sibling
         (vue-ts-mode--element-node-p sibling)
         sibling)))

(defun vue-ts-mode--goto-node-start (node)
  "Move point to NODE's start and return point."
  (goto-char (treesit-node-start node)))

(defun vue-ts-mode--node-first-non-whitespace-pos (node)
  "Return the first non-whitespace position in NODE."
  (let ((text (treesit-node-text node t)))
    (when (string-match-p "[^[:space:]]" text)
      (+ (treesit-node-start node)
         (string-match "[^[:space:]]" text)))))

(defun vue-ts-mode--element-first-child-target (node)
  "Return the first useful navigation child of element NODE."
  (let ((children (treesit-node-children node)))
    (or (cl-find-if #'vue-ts-mode--element-node-p children)
        (cl-find-if
         (lambda (child)
           (and (vue-ts-mode--treesit-node-type-p
                 child vue-ts-mode--element-content-node-types)
                (vue-ts-mode--node-first-non-whitespace-pos child)))
         children))))

(defun vue-ts-mode--element-empty-content-pos (element)
  "Return the insertion position inside empty paired ELEMENT."
  (when-let* ((start-tag (vue-ts-mode--element-start-tag element))
              (end-tag (vue-ts-mode--element-end-tag element))
              (content-start (treesit-node-end start-tag))
              (content-end (treesit-node-start end-tag)))
    (when (save-excursion
            (goto-char content-start)
            (skip-chars-forward " \t\n" content-end)
            (= (point) content-end))
      content-start)))

(defun vue-ts-mode--element-content-contains-pos-p (element pos)
  "Return non-nil if ELEMENT's content range contains POS."
  (when-let* ((start-tag (vue-ts-mode--element-start-tag element))
              (end-tag (vue-ts-mode--element-end-tag element)))
    (<= (treesit-node-end start-tag)
        pos
        (treesit-node-start end-tag))))

(defun vue-ts-mode--content-element-at-pos (&optional pos)
  "Return the innermost element whose content contains POS."
  (let ((pos (or pos (point))))
    (car (last (seq-filter
                (lambda (element)
                  (vue-ts-mode--element-content-contains-pos-p element pos))
                (vue-ts-mode--get-all-elements))))))

(defun vue-ts-mode-element-next-sibling (&optional pos)
  "Move point to the next element sibling at POS.
When POS is nil, use point.  If there is no next sibling, do
nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos))
              (sibling (vue-ts-mode--element-sibling element nil)))
    (vue-ts-mode--goto-node-start sibling)))

(defun vue-ts-mode-element-previous-sibling (&optional pos)
  "Move point to the previous element sibling at POS.
When POS is nil, use point.  If there is no previous sibling, do
nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos))
              (sibling (vue-ts-mode--element-sibling element t)))
    (vue-ts-mode--goto-node-start sibling)))

(defun vue-ts-mode-mark-element (&optional pos)
  "Mark the whole element at POS, including its children.
When POS is nil, use point."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (push-mark (treesit-node-start element) nil t)
    (goto-char (treesit-node-end element))
    (activate-mark)))

(defalias 'vue-ts-mode-element-mark #'vue-ts-mode-mark-element)

(defun vue-ts-mode-element-start (&optional pos)
  "Move point to the start of the element at POS.
When POS is nil, use point."
  (interactive "d")
  (if (not (eq (vue-ts-mode--treesit-language-at-point pos) 'vue))
      (call-interactively #'beginning-of-defun)
    (when-let* ((element (vue-ts-mode--element-at-pos pos)))
      (vue-ts-mode--goto-node-start element))))

(defun vue-ts-mode-element-end (&optional pos)
  "Move point to the end of the element at POS.
When POS is nil, use point."
  (interactive "d")
  (if (not (eq (vue-ts-mode--treesit-language-at-point pos) 'vue))
      (call-interactively #'end-of-defun)
    (when-let* ((element (vue-ts-mode--element-at-pos pos)))
      (goto-char (treesit-node-end element)))))

(defalias 'vue-ts-mode-element-beginning #'vue-ts-mode-element-start)

(defun vue-ts-mode--get-all-elements ()
  "Return a list of all element nodes in the buffer."
  (flatten-tree (treesit-induce-sparse-tree
                 (treesit-buffer-root-node 'vue)
                 (vue-ts-mode--rpartial
                  vue-ts-mode--treesit-node-type-p
                  vue-ts-mode--element-node-type-regexp))))

(defun vue-ts-mode-prev-element-dwim (pos)
  "Move point to the beginning of the nearest HTML element after POS."
  (interactive "d")
  (if (vue-ts-mode--in-attr-value-or-not-vue-p)
      (call-interactively #'backward-list)
    (when-let* ((elements (vue-ts-mode--get-all-elements)))
      (cl-loop for el in elements
               for next in (cdr elements)
               if (and (> pos (treesit-node-start el))
                       (<= pos (treesit-node-start next)))
               return (goto-char (treesit-node-start el))))))

(defun vue-ts-mode-next-element-dwim (pos)
  "Move point to the beginning of the nearest HTML element at or before POS."
  (interactive "d")
  (if (vue-ts-mode--in-attr-value-or-not-vue-p)
      (call-interactively #'forward-list)
    (when-let* ((elements (vue-ts-mode--get-all-elements))
                (next-element (cl-find-if
                               (lambda (n)
                                 (and n (< pos (treesit-node-start n))))
                               elements)))
      (goto-char (treesit-node-start next-element)))))

(defun vue-ts-mode-element-parent (&optional pos)
  "Move point to the parent element of the element at POS.
When POS is nil, use point.  If there is no parent element, do
nothing."
  (interactive "d")
  (let* ((pos (or pos (point)))
         (node (treesit-node-at pos 'vue))
         (content-element (vue-ts-mode--content-element-at-pos pos))
         (element (or content-element
                      (vue-ts-mode--element-at-pos pos))))
    (cond
     (content-element
      (vue-ts-mode--goto-node-start content-element))
     ((and element
           (vue-ts-mode--treesit-node-type-p
            node vue-ts-mode--element-content-node-types))
      (vue-ts-mode--goto-node-start element))
     (t
      (when-let* ((parent (and element
                               (vue-ts-mode--element-parent element))))
        (vue-ts-mode--goto-node-start parent))))))


(defun vue-ts-mode--in-attr-value-or-not-vue-p (&optional pos)
  "Return non-nil if POS is in an attribute value or outside Vue.

Optional argument POS is a buffer position; it defaults to point."

  (let ((pos (or pos (point))))
    (or (vue-ts-mode--attribute-value-at-pos pos)
        (not (eq (vue-ts-mode--treesit-language-at-point pos) 'vue)))))

(defun vue-ts-mode-element-up (&optional pos)
  "Move up from POS.
Inside attribute values and embedded script/style code, call
`backward-up-list'.  Otherwise move to the parent Vue element."
  (interactive "d")
  (let ((pos (or pos (point))))
    (goto-char pos)
    (if (vue-ts-mode--in-attr-value-or-not-vue-p pos)
        (call-interactively #'backward-up-list)
      (vue-ts-mode-element-parent pos))))

(defun vue-ts-mode-element-down (&optional pos)
  "Move point down into a list or the element's first useful child.

Optional argument POS is a buffer position; it defaults to point."
  (interactive "d")
  (let ((pos (or pos (point))))
    (goto-char pos)
    (if (vue-ts-mode--in-attr-value-or-not-vue-p pos)
        (call-interactively #'down-list)
      (vue-ts-mode-element-child pos))))

(defun vue-ts-mode-element-child (&optional pos)
  "Move point to the first useful child of the element at POS.
Element children are preferred.  If there are no element children,
move to the first non-whitespace text/interpolation/raw-text child.
When POS is nil, use point.  If there is no useful child, do
nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (if-let* ((child (vue-ts-mode--element-first-child-target element)))
        (if (vue-ts-mode--element-node-p child)
            (vue-ts-mode--goto-node-start child)
          (goto-char (vue-ts-mode--node-first-non-whitespace-pos child)))
      (when-let* ((empty-pos
                   (vue-ts-mode--element-empty-content-pos element)))
        (goto-char empty-pos)))))

;;; Attribute navigation

(defun vue-ts-mode--attribute-tag-dwim (&optional pos)
  "Return the tag whose attributes should be navigated at POS.
Prefer a tag at POS.  Otherwise, use the start or self-closing tag
of the element at POS."
  (or (vue-ts-mode--tag-at-pos pos)
      (when-let* ((element (vue-ts-mode--element-at-pos pos)))
        (vue-ts-mode--element-start-tag element))))

(defun vue-ts-mode--tag-attributes (tag)
  "Return TAG's direct attribute nodes."
  (seq-filter #'vue-ts-mode--attribute-node-p
              (treesit-node-children tag)))

(defun vue-ts-mode--attribute-at-pos (attributes pos)
  "Return the attribute in ATTRIBUTES containing POS."
  (cl-find-if
   (lambda (attribute)
     (vue-ts-mode--node-contains-pos-p attribute pos))
   attributes))

(defun vue-ts-mode--attribute-dwim-target (pos backward)
  "Return the attribute navigation target from POS.
If BACKWARD is non-nil, return the previous attribute target.
Otherwise, return the next attribute target."
  (when-let* ((tag (vue-ts-mode--attribute-tag-dwim pos))
              (attributes (vue-ts-mode--tag-attributes tag)))
    (let* ((effective-pos
            (if (vue-ts-mode--node-contains-pos-p tag pos)
                pos
              (if backward
                  (treesit-node-end tag)
                (treesit-node-start tag))))
           (current (vue-ts-mode--attribute-at-pos attributes effective-pos))
           previous)
      (if backward
          (catch 'target
            (dolist (attribute attributes previous)
              (cond
               ((eq attribute current)
                (throw 'target previous))
               ((< (treesit-node-end attribute) effective-pos)
                (setq previous attribute)))))
        (if current
            (cadr (memq current attributes))
          (cl-find-if
           (lambda (attribute)
             (> (treesit-node-start attribute) effective-pos))
           attributes))))))

(defun vue-ts-mode-next-attribute-dwim (&optional pos)
  "Move to the next attribute, or the next element if none exists.

Optional argument POS is a buffer position; it defaults to point."
  (interactive "d")
  (unless pos (setq pos (point)))
  (if (not (eq (vue-ts-mode--treesit-language-at-point pos) 'vue))
      (call-interactively #'forward-to-indentation)
    (if-let* ((attribute (vue-ts-mode--attribute-dwim-target
                          pos nil)))
        (vue-ts-mode--goto-node-start attribute)
      (vue-ts-mode-next-element-dwim pos))))


(defun vue-ts-mode-previous-attribute-dwim (&optional pos)
  "Move to the previous Vue attribute, or previous element if none.

Optional argument POS is a buffer position; it defaults to point."
  (interactive "d")
  (unless pos (setq pos (point)))
  (if (not (eq (vue-ts-mode--treesit-language-at-point pos) 'vue))
      (call-interactively #'back-to-indentation)
    (if-let* ((attribute (vue-ts-mode--attribute-dwim-target
                          (or pos (point)) t)))
        (vue-ts-mode--goto-node-start attribute)
      (vue-ts-mode-prev-element-dwim pos))))

(defalias 'vue-ts-mode-attribute-next-dwim
  #'vue-ts-mode-next-attribute-dwim)
(defalias 'vue-ts-mode-attribute-previous-dwim
  #'vue-ts-mode-previous-attribute-dwim)

;;; Element movement

(defun vue-ts-mode--node-markers (node)
  "Return start and end markers for NODE."
  (cons (copy-marker (treesit-node-start node))
        (copy-marker (treesit-node-end node) t)))

(defun vue-ts-mode--node-line-range (node)
  "Return a cons cell for NODE's movable text range.
When NODE is the only non-whitespace text on its outer lines,
include the leading indentation and trailing newline.  Otherwise,
return NODE's exact range."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((range-start start)
              (range-end end))
          (goto-char start)
          (let ((bol (line-beginning-position)))
            (when (save-excursion
                    (goto-char bol)
                    (skip-chars-forward " \t" start)
                    (= (point) start))
              (setq range-start bol)))
          (goto-char end)
          (let ((eol (line-end-position)))
            (when (save-excursion
                    (skip-chars-forward " \t" eol)
                    (= (point) eol))
              (setq range-end
                    (if (< eol (point-max))
                        (1+ eol)
                      eol))))
          (cons range-start range-end))))))

(defun vue-ts-mode--swap-nodes (node-a node-b)
  "Swap NODE-A and NODE-B in the current buffer."
  (pcase-let ((`(,node-a-start . ,node-a-end)
               (vue-ts-mode--node-markers node-a))
              (`(,node-b-start . ,node-b-end)
               (vue-ts-mode--node-markers node-b)))
    (let ((node-a-text (treesit-node-text node-a t))
          (node-b-text (treesit-node-text node-b t)))
      (atomic-change-group
        (delete-region node-a-start node-a-end)
        (delete-region node-b-start node-b-end)
        (goto-char node-a-start)
        (insert node-b-text)
        (goto-char node-b-start)
        (insert node-a-text)
        (goto-char node-b-start)))
    (set-marker node-a-start nil)
    (set-marker node-a-end nil)
    (set-marker node-b-start nil)
    (set-marker node-b-end nil)))

(defun vue-ts-mode--move-element-to-sibling (element backward)
  "Swap ELEMENT with a sibling.
If BACKWARD is non-nil, swap with the previous element sibling.
Otherwise, swap with the next element sibling."
  (when-let* ((sibling (vue-ts-mode--element-sibling element backward)))
    (vue-ts-mode--swap-nodes element sibling)
    t))

(defun vue-ts-mode--move-element-out (element backward)
  "Move ELEMENT out of its parent.
If BACKWARD is non-nil, move ELEMENT before its parent.  Otherwise,
move it after its parent."
  (when-let* ((parent (vue-ts-mode--element-parent element))
              (_ (string= (treesit-node-type parent) "element")))
    (pcase-let* ((`(,element-start . ,element-end)
                  (vue-ts-mode--node-line-range element))
                 (`(,parent-start . ,parent-end)
                  (vue-ts-mode--node-line-range parent))
                 (insert-marker (copy-marker
                                 (if backward parent-start parent-end)
                                 (not backward)))
                 (text (buffer-substring-no-properties
                        element-start element-end)))
      (atomic-change-group
        (delete-region element-start element-end)
        (goto-char insert-marker)
        (let ((insert-start (point)))
          (insert text)
          (indent-region insert-start (point))
          (goto-char insert-start)
          (back-to-indentation)))
      (set-marker insert-marker nil)
      t)))

(defun vue-ts-mode-element-move-previous-sibling (&optional pos)
  "Move the element at POS before its previous element sibling.
When POS is nil, use point.  If there is no previous sibling, do
nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (vue-ts-mode--move-element-to-sibling element t)))

(defun vue-ts-mode-element-move-next-sibling (&optional pos)
  "Move the element at POS after its next element sibling.
When POS is nil, use point.  If there is no next sibling, do
nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (vue-ts-mode--move-element-to-sibling element nil)))

(defun vue-ts-mode-element-move-up (&optional pos)
  "Move the element at POS before its parent.
When POS is nil, use point.  If the element cannot move out of its
parent, do nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (vue-ts-mode--move-element-out element t)))

(defun vue-ts-mode-element-move-down (&optional pos)
  "Move the element at POS after its parent.
When POS is nil, use point.  If the element cannot move out of its
parent, do nothing."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (vue-ts-mode--move-element-out element nil)))

(defun vue-ts-mode-element-move-up-dwim (&optional pos)
  "Move the element at POS upward.
Prefer swapping with the previous element sibling.  If there is no
previous sibling, move the element before its parent."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (or (vue-ts-mode--move-element-to-sibling element t)
        (vue-ts-mode--move-element-out element t))))

(defun vue-ts-mode-element-move-down-dwim (&optional pos)
  "Move the element at POS downward.
Prefer swapping with the next element sibling.  If there is no next
sibling, move the element after its parent."
  (interactive "d")
  (when-let* ((element (vue-ts-mode--element-at-pos pos)))
    (or (vue-ts-mode--move-element-to-sibling element nil)
        (vue-ts-mode--move-element-out element nil))))

;;;###autoload
(define-derived-mode vue-ts-mode prog-mode "Vue-ts"
  "Major mode for editing Vue templates, powered by tree-sitter."
  :group 'vue
  ;; :syntax-table html-mode-syntax-table

  (unless (treesit-ready-p 'vue)
    (error "Tree-sitter grammar for Vue isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))

  (unless (treesit-ready-p 'typescript)
    (error "Tree-sitter grammar for Typescript/TYPESCRIPT isn't available"))

  (when (treesit-ready-p 'typescript)
    (treesit-parser-create 'vue)
    (treesit-parser-create 'typescript)
    (treesit-parser-create 'css)

    ;; Comments and text content
    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment" "text")))

    ;; Indentation rules
    (setq-local treesit-simple-indent-rules vue-ts-mode--indent-rules
                css-indent-offset vue-ts-mode-indent-offset)

    ;; Font locking
    (setq-local treesit-font-lock-settings (vue-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                '((vue-attr vue-definition css-selector
                   css-comment css-query css-keyword
                   typescript-comment typescript-declaration)
                  (vue-ref vue-string vue-directive css-property css-constant
                           css-string
                           typescript-keyword
                           typescript-string typescript-escape-sequence)
                  (vue-sp-dir vue-directive-value
                              css-error css-variable css-function
                              css-operator
                              typescript-constant
                              typescript-expression typescript-identifier
                              typescript-number typescript-pattern
                              typescript-operator
                              typescript-property)
                  (vue-bracket css-bracket
                               typescript-function
                               typescript-bracket
                               typescript-delimiter
                               typescript-custom-function
                               typescript-custom-variable
                               typescript-custom-property)))

    ;; Embedded languages
    (setq-local treesit-range-settings vue-ts-mode--range-settings)
    (setq-local treesit-language-at-point-function
                #'vue-ts-mode--treesit-language-at-point)

    (setq-local treesit-primary-parser (treesit-parser-create 'vue))

    (treesit-major-mode-setup)
    (font-lock-add-keywords nil vue-ts-mode--font-lock-keywords 'append)))

(if (treesit-ready-p 'vue)
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode)))

(advice-add
 #'treesit-buffer-root-node
 :before-while
 #'vue-ts-mode--advice-for-treesit-buffer-root-node)

(advice-add
 #'treesit--merge-ranges
 :before-while
 #'vue-ts-mode--advice-for-treesit--merge-ranges)

(advice-add
 #'comment-dwim
 :around
 #'vue-ts-mode--advice-for-comment-fns)

(defvar-keymap vue-ts-mode-repeat-map
  :doc
  "Keymap to repeat navigation commands."
  :repeat t
  "p" #'vue-ts-mode-prev-element-dwim
  "n" #'vue-ts-mode-next-element-dwim
  "N" #'vue-ts-mode-element-next-sibling
  "P" #'vue-ts-mode-element-previous-sibling
  "d" #'vue-ts-mode-element-down
  "u" #'vue-ts-mode-element-up
  "a" #'vue-ts-mode-element-start
  "e" #'vue-ts-mode-element-end)


;;;###autoload (autoload 'vue-ts-mode-menu "vue-ts-mode" nil t)
(transient-define-prefix vue-ts-mode-menu ()
  :transient-suffix     #'transient--do-call
  :transient-non-suffix #'transient--do-exit
  [["Elements Navigation"
    ("p" "Previous or up" vue-ts-mode-prev-element-dwim)
    ("n" "Next or down" vue-ts-mode-next-element-dwim)
    ""
    ("a" "Start of element" vue-ts-mode-element-start)
    ("e" "End of element" vue-ts-mode-element-end)
    ""
    ("N" "Next sibling" vue-ts-mode-element-next-sibling)
    ("P" "Prev sibling" vue-ts-mode-element-previous-sibling)
    ""
    ("u" "Up element" vue-ts-mode-element-up)
    ("d" "Down element" vue-ts-mode-element-down)]
   ["Attributes navigation"
    ("<tab>" "Next attribute" vue-ts-mode-next-attribute-dwim)
    ("S-<tab>" "Previous attribute" vue-ts-mode-previous-attribute-dwim)
    "Mark"
    ("m" "Mark element" vue-ts-mode-mark-element)]])

(provide 'vue-ts-mode)
;;; vue-ts-mode.el ends here
