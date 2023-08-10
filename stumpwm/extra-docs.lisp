(in-package stumpwm)

;; NOTE: Fill column for Docstrings should be 60! fill column for code is 80
;;
;; use C-x f to set the fill column

(setf (documentation 'add-group 'function) (format nil "~
Create a new group in SCREEN with the supplied name.

group names starting with a . are considered hidden groups.
Hidden groups are skipped by gprev and gnext and do not show
up in the group listings (unless *list-hidden-groups* is T).
They also use negative numbers.

--EXTRA DOCS--
The TYPE parameter may be used to specify a dynamic group,
by setting it to 'dynamic-group"))
