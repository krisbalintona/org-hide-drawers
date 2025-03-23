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

(defcustom org-hide-drawers-string (propertize " #" 'face 'shadow)
  "Display string used for overlays."
  :type 'string)

(defcustom org-hide-drawers-blacklist (list)
  "A list of properties that prevent hiding drawer.
If any property in this option is present in a drawer, it will not be
hidden."
  :type '(repeat string))

(defcustom krisb-org-hide-top-level-drawers t
  "If nil, don't hide top-level property drawers."
  :type 'boolean)

;;; Variables
;; TODO 2025-03-22: There might be future bugs related to creating indirect
;; buffers of buffers with existent overlays.  The reason is that the overlays
;; stored in this variable for those indirect buffers will contain overlay
;; objects of the original object, not the indirect buffer's overlays.  However,
;; with the current implementation of commands, this only applies before
;; `org-hide-drawers-delete-overlays' then
;; `org-hide-drawers-create-overlays' is created.  After deletion and
;; creation, the overlay list is associated with the indirect buffer's objects
;; -- but there might be bugs because these two commands are needed to replace
;; this variable's value appropriately.  Can this be avoided?
(defvar-local org-hide-drawers-overlays nil
  "A list of overlays used to hide Org drawers in the current buffer.")

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

(defun org-hide-drawers--should-hide (drawer)
  "Predicate for whether DRAWER should be hidden.
DRAWER is an org-element.

Considers `org-hide-drawers-blacklist'."
  (let* ((properties (org-hide-drawers--get-properties drawer))
         (property-keys (mapcar #'car properties))
         ;; We check if DRAWER is a top-level drawer by checking if the
         ;; beginning of the drawer (an org element) is at the first point in
         ;; the buffer
         (top-level-p (= 1 (org-element-property :begin drawer))))
    (and
     ;; First adhere to the value of `krisb-org-hide-top-level-drawers'
     (or krisb-org-hide-top-level-drawers (not top-level-p))
     ;; Check against properties in the blacklist
     (not (cl-some (lambda (blacklist-prop)
                     (member blacklist-prop property-keys))
                   org-hide-drawers-blacklist)))))

;; TODO 2024-10-23: Consider special behavior for top-level drawers.  See
;; `org-tidy-should-tidy'.
(defun org-hide-drawers-create-overlays ()
  "Create overlays to hide Org drawers in the current buffer using the Org AST."
  (interactive)
  (let ((ast (org-element-parse-buffer 'element nil)))
    (org-element-map ast '(drawer property-drawer)
      (lambda (drawer)
        (when (org-hide-drawers--should-hide drawer)
          (let* ((begin (org-element-property :begin drawer))
                 (end (save-excursion
                        (goto-char (org-element-property :end drawer))
                        (skip-chars-backward "\n\t ") ; Skip trailing whitespace
                        (point)))
                 (ov (make-overlay (1- begin) ; Include preceding newline in overlay
                                   end ; Don't include proceeding whitespace in overlay
                                   nil ; Current buffer
                                   t ; Don't include text inserted at the start of overlay
                                   nil))) ; Don't include text inserted at the end of overlay

            ;; TODO 2024-10-23: Consider using the `insert-in-front-hooks'
            ;; special text property to notify the user of danger when adding
            ;; characters in front of hidden property drawer, since un-hiding it
            ;; then means that drawer is no longer recognized as such.

            (overlay-put ov 'display org-hide-drawers-string)
            (overlay-put ov 'modification-hooks
                         '((lambda (overlay after beg end)
                             (setq org-hide-drawers-overlays
                                   (remove overlay org-hide-drawers-overlays))
                             (delete-overlay overlay))))
            (overlay-put ov 'read-only t)
            (push ov org-hide-drawers-overlays)))))))

(defun org-hide-drawers-delete-overlays (&optional buffer)
  "Delete all drawer-hiding overlays in the current buffer.
If BUFFER is non-nil, delete overlays in that buffer instead."
  (interactive)
  ;; We use `delete-all-overlays' instead of `delete-overlay' on every overlay
  ;; stored in `org-hide-drawers-overlays' because the overlay objects in
  ;; this variable do not change when making an indirect buffer;
  ;; `delete-overlay' will delete overlays in the original buffer but
  ;; `delete-all-overlays' will not.
  (delete-all-overlays (or buffer (current-buffer)))
  (setq org-hide-drawers-overlays nil))

(defun org-hide-drawers-toggle ()
  "Toggle visibility of Org drawers in the current buffer."
  (interactive)
  (if org-hide-drawers-overlays
      (org-hide-drawers-delete-overlays)
    (org-hide-drawers-create-overlays)))

;;; Minor mode
;;;###autoload
(define-minor-mode org-hide-drawers-mode
  "Minor mode to hide Org drawers with a # symbol."
  :lighter " HideDrawers"
  (if org-hide-drawers-mode
      (progn
        (org-hide-drawers-create-overlays)
        (add-hook 'after-save-hook #'org-hide-drawers-create-overlays nil t))
    (org-hide-drawers-delete-overlays)
    (remove-hook 'after-save-hook #'org-hide-drawers-create-overlays t)))

;;; Provide
(provide 'org-hide-drawers)
;;; org-hide-drawers.el ends here
