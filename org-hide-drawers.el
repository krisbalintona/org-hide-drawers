;;; org-hide-drawers.el --- Hide drawers in Org using overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: tools, extensions

;; This program is free software; you can redistribute it and/or modify
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

;; Hide drawers in Org using overlays.  My improved version of org-tidy.

;;; Code:
(require 'org)
(require 'org-element)

;; TODO 2024-11-12: Ensure namespace is adhered to

;;; Options
(defgroup org-hide-drawers ()
  "Hide Org drawers using overlays."
  :group 'org-mode
  :prefix "org-hide-drawers-")

(defcustom org-hide-drawers-display-string (propertize " #" 'face 'shadow)
  "Display string used for overlays."
  :type 'string)

(defcustom org-hide-drawers-property-name-blacklist (list)
  "Regexps that prevent hiding drawers when matching a property name.
A list of regexps that match property names, where matches prevent
hiding drawer.  If a drawer contains a property that matches any of the
regexps in this list, that drawer will not be hidden.

See also `org-hide-drawers-property-name-blacklist-ignore-case-p' for
whether these regexps are case insensitive or not."
  :type '(repeat regexp))

(defcustom org-hide-drawers-property-name-blacklist-ignore-case-p t
  "Whether the regexps in `org-hide-drawers-property-name-blacklist' ignore case.
If non-nil, match property names as if `case-fold-search' were non-nil.
Likewise, if nil, match property names as if `case-fold-search' were
nil."
  :type 'boolean)

(defcustom org-hide-drawers-hide-top-level t
  "If nil, don't hide top-level property drawers."
  :type 'boolean)

;;; Variables
(defvar org-hide-drawers--category 'org-hide-drawers
  "Category of org-hide-drawers overlays.")

;;; Functions
(defun org-hide-drawers--get-properties (drawer)
  "Extract all properties from the given Org DRAWER element."
  (let ((contents (org-element-contents drawer))
        properties)
    (dolist (element contents)
      (when (and (eq (org-element-type element) 'node-property)
                 (org-element-property :key element))
        (push (cons (org-element-property :key element)
                    (org-element-property :value element))
              properties)))
    (nreverse properties)))             ; Return the list in original order

(defun org-hide-drawers--hide-drawer-p (drawer)
  "Predicate for whether DRAWER should be hidden.
DRAWER is an org-element.

Considers `org-hide-drawers-property-name-blacklist'."
  (let* ((properties (org-hide-drawers--get-properties drawer))
         (property-keys (mapcar #'car properties))
         ;; We check if DRAWER is a top-level drawer by checking if the
         ;; beginning of the drawer (an org element) is at the first point in
         ;; the buffer
         (top-level-p (= 1 (org-element-property :begin drawer))))
    (and
     ;; First adhere to the value of `org-hide-drawers-hide-top-level'
     (or org-hide-drawers-hide-top-level (not top-level-p))
     ;; Check against properties in the blacklist
     (not (cl-some (lambda (blacklist-prop-regexp)
                     (let ((case-fold-search org-hide-drawers-property-name-blacklist-ignore-case-p))
                       (cl-some (lambda (key) (string-match-p blacklist-prop-regexp key))
                                property-keys)))
                   org-hide-drawers-property-name-blacklist)))))

(defun org-hide-drawers-get-overlays (&optional buffer)
  "Return a list of all org-hide-drawers overlays.
If BUFFER is non-nil, return a list of org-hide-drawers overlays in that
buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (let ((all-overlays (overlays-in (point-min) (point-max)))
          our-overlays)
      (dolist (ov all-overlays)
        (when (eq (overlay-get ov 'category) org-hide-drawers--category)
          (push ov our-overlays)))
      our-overlays)))

(defun org-hide-drawer-hide-region (begin end)
  "Create an overlay to hide region from BEGIN to END."
  (let ((ov (make-overlay (1- begin)  ; Include preceding newline in overlay
                          end         ; Exclude proceeding whitespace in overlay
                          nil         ; Current buffer
                          t      ; Exclude text inserted at the start of overlay
                          nil))) ; Exclude text inserted at the end of overlay
    ;; Read (info "(elisp) Overlay Properties") for an explanation of overlay
    ;; properties
    (overlay-put ov 'category 'org-hide-drawers)
    (overlay-put ov 'display org-hide-drawers-display-string)
    (overlay-put ov 'modification-hooks '((lambda (overlay _after _beg _end)
                                            (delete-overlay overlay))))
    (overlay-put ov 'read-only t)
    (overlay-put ov 'evaporate t)
    ;; Read (info "(elisp) Invisible Text") for interactions with isearch
    (overlay-put ov 'invisible t)
    (overlay-put ov 'isearch-open-invisible (lambda (overlay) (delete-overlay overlay)))
    (overlay-put ov 'isearch-open-invisible-temporary
                 (lambda (overlay hidep)
                   (overlay-put overlay 'invisible hidep)
                   (overlay-put overlay 'display (when hidep org-hide-drawers-display-string))))))

;;; Commands
;; TODO 2024-10-23: Consider special behavior for top-level drawers.  See
;; `org-tidy-should-tidy'.
(defun org-hide-drawers-make-overlays ()
  "Conditionally hide org drawers in the current buffer using the org AST.
Hide every drawer in the current buffer if it satisfies
`org-hide-drawers--hide-drawer-p'."
  (interactive)
  (let ((ast (org-element-parse-buffer 'element nil)))
    (org-element-map ast '(drawer property-drawer)
      (lambda (drawer)
        (when (org-hide-drawers--hide-drawer-p drawer)
          (let* ((begin (org-element-property :begin drawer))
                 (end (save-excursion
                        (goto-char (org-element-property :end drawer))
                        (skip-chars-backward "\n\t ") ; Skip trailing whitespace
                        (point))))
            (org-hide-drawer-hide-region begin end)))))))

(defun org-hide-drawers-delete-overlays (&optional buffer)
  "Delete all drawer-hiding overlays in the current buffer.
If BUFFER is non-nil, delete overlays in that buffer instead."
  (interactive)
  (mapc #'delete-overlay (org-hide-drawers-get-overlays buffer)))

(defun org-hide-drawers-toggle ()
  "Toggle visibility of Org drawers in the current buffer."
  (interactive)
  (if (org-hide-drawers-get-overlays)
      (org-hide-drawers-delete-overlays)
    (org-hide-drawers-make-overlays)))

;;; Minor mode
;;;###autoload
(define-minor-mode org-hide-drawers-mode
  "Minor mode to hide Org drawers with a # symbol."
  :lighter " HideDrawers"
  (if org-hide-drawers-mode
      (progn
        (org-hide-drawers-make-overlays)
        (add-hook 'after-save-hook #'org-hide-drawers-make-overlays nil t))
    (org-hide-drawers-delete-overlays)
    (remove-hook 'after-save-hook #'org-hide-drawers-make-overlays t)))

;;; Provide
(provide 'org-hide-drawers)
;;; org-hide-drawers.el ends here
