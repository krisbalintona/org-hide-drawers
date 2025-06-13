;;; org-hide-drawers.el --- Hide drawers in Org using overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/org-hide-drawers.git
;; Keywords: tools, extensions
;; Version: 1.0.2
;; Package-Requires: ((emacs "26.1"))

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

;; Hide drawers in org-mode buffers using overlays.  These overlays
;; replace the visual display of drawers using their "display"
;; property.
;;
;; Test out the functionality easily:
;; 1. Open an org buffer.  (Indirect buffers are supported by
;;    org-hide-drawers.)
;; 2. Enable `org-hide-drawers-mode'.  Drawers become "hidden": their
;;    displayed text is changed to `org-hide-drawers-display-string'.
;;    The actual text remains unchanged.
;; 3. Un-hide drawers by calling `org-hide-drawers-toggle', or
;;    `org-hide-drawers-make-overlays’.
;; 4. Hide drawers by calling `org-hide-drawers-toggle' again, or
;;    `org-hide-drawers-delete-overlays'.
;;
;; Which drawers and property drawers are hidden and replaced by what
;; strings may be specified by the `org-hide-drawers-display-strings’
;; user option.  Org-hide-drawers supports matching against drawer
;; names, property drawer key names, and arbitrary user-defined
;; predicates.

;;; Code:
(require 'org-element)

;;; Variables
(defvar org-hide-drawers--category 'org-hide-drawers
  "Category of org-hide-drawers overlays.")

(defvar org-hide-drawers--display-strings-func nil
  "The function object that returns the display string for a given drawer.
The value of this variable is function that accepts one argument, an
org-element whose type is drawer or property drawer.  It returns a
string that will be the the value of the display property of the overlay
encompassing the region of that drawer.  These strings are determined by
the value of `org-hide-drawers-display-strings’.

See the docstring of `org-hide-drawers-display-strings’ for a
description of which drawers are replaced by which strings.  This
variable is set by `org-hide-drawers--set-display-strings-func’ when the
value of `org-hide-drawers-display-strings’ is customized or set with
`setopt’.")

;;; Options
(defgroup org-hide-drawers ()
  "Hide Org drawers using overlays."
  :group 'org-mode
  :prefix "org-hide-drawers-")

;; REVIEW 2025-06-13: Perhaps ‘property-drawer-regexp should be
;; renamed to something like ‘property-drawer-property-regexp to
;; suggest its actual function better.
;; TODO 2025-06-13: Is ‘property-drawer-regexp really sufficient for
;; all possible property drawers?  It seems strange that if one wanted
;; to match against all property drawers they would need to match
;; against any property drawer property key name.  Additionally, this
;; specification, because it checks property key names, will never
;; match a property drawer that is empty (has no property keys).
(defcustom org-hide-drawers-display-strings
  (list (list 'top-level-property-drawer (propertize "[Hidden...]" 'face 'shadow))
        (list 'drawer-regexp (propertize "[Hidden...]" 'face 'shadow) (rx (0+ anychar)))
        (list 'property-drawer-regexp (propertize " #" 'face 'shadow) (rx (0+ anychar))))
  "Display strings used to hide drawers.
This is a list of lists.  Each inner list is a specification that for
the kind of drawer or property drawer that should be matched against, a
matching condition for the drawer or property drawer, and the display
string used to hide the drawers or property drawers that satisfy the
matching condition.

The order of these specifications matter.  The first specification that
succeeds will be used.  This means that if a drawer or property drawer
succeeds in matching against the first specification, all other
specifications will be skipped over for this drawer.

For this option to take effect, it must be set by customizing it or
using `setopt’.  To verify that this option is in effect and to check
whether its behavior is as intended, see the value of
`org-hide-drawers--display-strings-func’ after setting it.

Each specification may have one of the following forms:

    (\\=‘top-level-property-drawer DISPLAY-STRING)
        Match against the top-level property drawer.  The top-level
        property drawer is the one at the very beginning of an org file.

    (\\=‘property drawer-regexp DISPLAY-STRING REGEXP)
    (\\=‘property drawer-regexp DISPLAY-STRING REGEXP CASE-FOLD)
        Use REGEXP to match against the key names of property drawers.
        This includes the top-level property drawer.  A key in a
        property drawer is the name of a property set in the drawer,
        something like \“ID\” or \“CUSTOM_ID\”.  CASE-FOLD is whether
        REGEXP is matched case-insensitively or not.  It is either nil
        or non-nil.  If it is not specified, then the value of
        `case-fold-search' will be used.

    (\\=‘drawer-regexp)
        Use REGEXP to match against the names of drawers.  CASE-FOLD is
        whether REGEXP is matched case-insensitively or not.  It is
        either nil or non-nil.  If it is not specified, then the value
        of `case-fold-search' will be used.

    (\\=‘pred FUNCTION-SYMBOL DISPLAY-STRING)
    (\\=‘pred FUNCTION-OBJECT DISPLAY-STRING)
        Specify a function (either by name or as a lambda) that accepts
        a single argument: an org-element drawer or property drawer.
        Match against drawers or property drawers where this function
        returns non-nil.

    (\\=‘all)
        Match against every drawer or property drawer.  This is useful
        as the final inner list to match against every drawer or
        property drawer not already matched against from other
        specifications.

Any instance of DISPLAY-STRING in the specifications above is the string
that will be the value of the display property of the overlay created to
hide drawers matched against.  DISPLAY-STRING may be a propertized
string if the user wishes to, for example, use strings with faces
applied.  DISPLAY-STRING may also be nil to indicate that the drawer or
property drawer matched against should not be hidden.

Below are several example elisp forms that evaluate to possible values
of this user option.  They illustrate the possible ways this user option
may be set to achieve various behaviors:

    (list (list \\=’top-level-property drawer nil))
        Do not hide the top-level property drawer.

    (list (list \\='property-drawer-regexp \" [Hidden...]\" \"ID\"))
        Hide property drawers with an \“ID\” property.  Display the
        string \" [Hidden...]\" instead.  The following drawer would be
        hidden:

            * Example headline
            :PROPERTIES:
            :ID: 20230824T194824.718246
            :END:

    (list (list \\='drawer-regexp (propertize \" #\" 'face 'shadow) \“^CONTENTS$\“))
        Hide drawers whose name matched \“CONTENTS\” exactly.  Display
        the string \" #\" with the shadow face applied instead.  The
        following drawer would be hidden:

            * Example headline
            Some text.

            :CONTENTS:
            In id erat non orci commodo lobortis.
            :END:

    (list (list \\='property-drawer-regexp nil \"ID\")
          (list \\=’top-level-property drawer (propertize \"[Hidden...]\" 'face 'shadow))
          (list 'drawer-regexp (propertize \"[Hidden...]\" 'face 'shadow) (rx (0+ anychar)))
          (list 'all (propertize \" #\" 'face 'shadow)))
        Hide the top-level property drawer unless it has the \“ID\”
        property set, in which case keep it shown.  Also don’t hide any
        other property drawer that sets the \“ID\” property.  Hide all
        regular drawers with a propertized \“[Hidden...]\”.  Then hide
        everything else with a propertized \“ #\”.

        (Note that if the first specification were swapped with the
        second specification, then the top-level property drawer would
        always be hidden.  This is because in that scenario the
        top-level property drawer would succeed in matching against the
        first specification, so it would not reach the second
        specification which would’ve kept it shown if the \“ID\”
        property were present.)

    (list (list \\='property-drawer-regexp \"\" (rx (0+ anychar)))
        Hide all property drawers, displaying an empty string instead.
        Effectively, these property drawers are made invisible."
  :type '(repeat (repeat sexp))
  :set 'org-hide-drawers--set-display-strings-func)

;;; Functions
(defun org-hide-drawers--get-properties (property-drawer)
  "Extract all properties from the given org drawer (PROPERTY-DRAWER)."
  (let ((contents (org-element-contents property-drawer))
        properties)
    (dolist (element contents)
      (when (and (eq (org-element-type element) 'node-property)
                 (org-element-property :key element))
        (push (cons (org-element-property :key element)
                    (org-element-property :value element))
              properties)))
    (nreverse properties)))             ; Return the list in original order

(defun org-hide-drawers--set-display-strings-func (_option value)
  "Set the value of `org-hide-drawers--display-strings-func’ based on VALUE.
This function is meant as the value of the :set property of
`org-hide-drawers-display-strings’.  It transforms the value of that
option into a cond form stored in
`org-hide-drawers--display-strings-func’.  See the docstring of
`org-hide-drawers--display-strings-func’ for a description of this cond
form and its purpose.

_OPTION is the symbol of a user option and VALUE is the value of that
user option."
  (setq org-hide-drawers--display-strings-func
        ;; Construct a lambda with a `cond’ whose conditions match
        ;; against a certain type of drawer and whose bodies are the
        ;; display string that should be displayed instead of the text
        ;; of that drawer
        `(lambda (drawer)
           (cond
            ,@(mapcar
               (lambda (spec)
                 (let ((condition-type (car spec))
                       (body (cdr spec)))
                   (cond
                    ;; Match on drawer names based on regexp
                    ((eq condition-type 'drawer-regexp)
                     (let ((display-string (car body))
                           (regexp (cadr body))
                           (case-fold-search (if (cddr body)
                                                 (caddr body)
                                               case-fold-search)))
                       (list `(let ((case-fold-search ,case-fold-search))
                                (and (eq 'drawer (org-element-type drawer))
                                     (string-match-p ,regexp (org-element-property :drawer-name drawer))))
                             display-string)))
                    ;; Match against the key names of property drawers
                    ;; (e.g., “ID” and “CUSTOM_ID”) based on regexp.
                    ;; We should take care to match against property
                    ;; drawers generally before matching against the
                    ;; top-level property drawer just in case the
                    ;; top-level property drawer satisfies a general
                    ;; property drawer specification.
                    ((eq condition-type 'property-drawer-regexp)
                     (let ((display-string (car body))
                           (regexp (cadr body))
                           (case-fold-search (if (cddr body)
                                                 (caddr body)
                                               case-fold-search)))
                       (list `(and (eq 'property-drawer (org-element-type drawer))
                                   (let* ((properties (org-hide-drawers--get-properties drawer))
                                          (property-keys (mapcar #'car properties))
                                          (case-fold-search ,case-fold-search))
                                     (seq-some (lambda (key) (string-match-p ,regexp key)) property-keys)))
                             display-string)))
                    ;; Match on the top-level property drawer (the one
                    ;; at the very beginning of an org file)
                    ((eq condition-type 'top-level-property-drawer)
                     (list `(and (eq 'property-drawer (org-element-type drawer))
                                 (= 1 (org-element-property :begin drawer)))
                           (car body)))
                    ;; Match against a user-provided predicate
                    ((eq condition-type 'pred)
                     (list `(save-excursion (funcall ,(car body) drawer)) (cadr body)))
                    ;; Match against every drawer and property drawer
                    ((eq condition-type 'all)
                     (list t (car body)))
                    ;; Error if the supplied org element is not a
                    ;; drawer or property drawer
                    (t (error "[org-hide-drawers--set-display-strings-func] Unknown condition type: %s" condition-type)))))
               value)))))

(defun org-hide-drawers--determine-display-string (drawer)
  "Return the string that should be used to hide DRAWER.
DRAWER is either a drawer or property drawer org element.

The string returned by this function is dependent on the value of
`org-hide-drawers-display-strings’.  See the docstring of
`org-hide-drawers-display-strings’ for a description of which display
strings match against which drawers.

If this function returns nil, DRAWER should not be hidden according to
the value of `org-hide-drawers-display-strings’.

If DRAWER is not an org drawer or property drawer, throw an error."
  (if (memq (org-element-type drawer) '(drawer property-drawer))
      (funcall org-hide-drawers--display-strings-func drawer)
    (error "`org-hide-drawers--determine-display-string' accepts only drawer and property drawer elements")))

(defun org-hide-drawers-get-overlays (&optional buffer)
  "Return a list of all org-hide-drawers overlays.
If BUFFER is non-nil, return a list of org-hide-drawers overlays in that
buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (let ((all-overlays (overlays-in (point-min) (point-max)))
          our-overlays)
      (dolist (ov all-overlays)
        (when (eq (overlay-get ov 'category) 'org-hide-drawers)
          (push ov our-overlays)))
      our-overlays)))

(defun org-hide-drawers-hide-region (begin end display-string)
  "Create an overlay from BEGIN to END, displaying DISPLAY-STRING instead.
An overlay from BEGIN to END is created.  The display property over the
overlay is set to DISPLAY-STRING.  (See (info \"(elisp) Overlay
Properties\") for more information on the display property of overkays.)"
  (let ((ov (make-overlay begin end
                          nil           ; Current buffer
                          t ; Exclude text inserted at the start of overlay
                          nil))) ; Exclude text inserted at the end of overlay
    ;; Read (info "(elisp) Overlay Properties") for an explanation of
    ;; overlay properties
    (overlay-put ov 'category 'org-hide-drawers)
    (overlay-put ov 'display display-string)
    (overlay-put ov 'modification-hooks ; Delete overlay on modifying its text
                 (list (lambda (overlay _after _beg _end) (delete-overlay overlay))))
    (overlay-put ov 'evaporate t)   ; Delete overlay if it has no text
    ;; Read (info "(elisp) Invisible Text") for interactions with
    ;; isearch
    (overlay-put ov 'invisible t)
    (overlay-put ov 'isearch-open-invisible (lambda (overlay) (delete-overlay overlay)))
    (overlay-put ov 'isearch-open-invisible-temporary
                 (lambda (overlay hidep)
                   (overlay-put overlay 'invisible hidep)
                   ;; Set the display property to nil (therefore
                   ;; removing it) if hidep is nil
                   (overlay-put overlay 'display (when hidep display-string))))))

(defun org-hide-drawers-hide-drawer (drawer)
  "Hide DRAWER.
DRAWER is an org element drawer (e.g., a drawer or property drawer org
element type object)."
  (when-let* ((display-string (org-hide-drawers--determine-display-string drawer))
              (begin (save-excursion
                       (goto-char (org-element-property :begin drawer))
                       ;; Include the preceding whitespace in overlay
                       ;; if DRAWER is a property drawer.  This is
                       ;; because users tend to expect that their
                       ;; display strings replace the drawer and this
                       ;; preceding newline.  But only for property
                       ;; drawers; this is undesirable for regular
                       ;; drawers
                       (when (eq 'property-drawer (org-element-type drawer))
                         (skip-chars-backward "\n\t "))
                       (point)))
              (end (save-excursion
                     (goto-char (org-element-property :end drawer))
                     ;; The end of org-element drawers goes all the
                     ;; way until the beginning of the next element.
                     ;; Therefore, we should avoid hiding whitespace
                     ;; proceeding a drawer
                     (skip-chars-backward "\n\t ")
                     (point))))
    (org-hide-drawers-hide-region begin end display-string)))

;;; Commands
;;;###autoload
(defun org-hide-drawers-make-overlays ()
  "Conditionally hide org drawers in the current buffer using the org AST.
Hide every drawer in the current buffer if
`org-hide-drawers--determine-display-string' returns non-nil when the
drawer is passed to it."
  (interactive)
  (let ((ast (org-element-parse-buffer 'element nil)))
    (org-element-map ast '(drawer property-drawer) #'org-hide-drawers-hide-drawer)))

;;;###autoload
(defun org-hide-drawers-delete-overlays (&optional buffer)
  "Delete all drawer-hiding overlays in the current buffer.
If BUFFER is non-nil, delete overlays in that buffer instead."
  (interactive)
  (mapc #'delete-overlay (org-hide-drawers-get-overlays buffer)))

;;;###autoload
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
