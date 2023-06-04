;;; org-publish-local.el -*- lexical-binding: t; -*-

(add-hook
 'org-mode-hook
 (lambda ()
   (evil-define-key '(normal visual) org-mode-map (kbd "SPC m P l") #'org-publish-local-project)))

;;; NOTE: every file in a project is loaded into a buffer, and made current in
;;; the org-publish-to function. That function tries to use existing buffers if
;;; available, otherwise it will just create a new one, temporarily.
;;;
;;; If a buffer already exists, but its path is screwed up, that will screw up
;;; the path of its includes.

;; state variable for local publish function
(setq org-publish-local-root nil)
(defun org-publish-local-project (project-root)
  "command to publish projects not listed in ~org-publish-project-alist~.

This requrires the project alist to be defined as
org-publish-local-alist in a file called export-config.el in the
project root directory. When this command is called for the first
time during a session, the user will be prompted to select the
root directory for their project. This directory is remembered
between calls"
  (interactive (list (if org-publish-local-root org-publish-local-root
                       (setq org-publish-local-root
                             (read-directory-name "Select project root: " )))))

  (let ((config-file (concat project-root "export-config.el")))
    (when (file-exists-p config-file)
      ;; file exists, so get the code from it
      (require 'ox-publish)
      (load config-file nil nil t)
      (let (tmp-alist
            (org-publish-use-timestamps-flag nil)
            (buffer-directory (file-name-directory (buffer-file-name)))) ; force all files to be published

        (setq org-publish-local-alist
              (map 'list
                   (lambda (project)
                     (cons (car project)
                           (publish-local-fix-base-dir org-publish-local-root
                                                           (cdr project))))
                   org-publish-local-alist))

        ;; the org-publish-expand-project function requires that
        ;; org-publish-project-alist contain all projects. save the current
        ;; value of org-publish-project-alist, and restore it after this
        ;; function is ran.
        (cl-rotatef org-publish-local-alist org-publish-project-alist)

        ;; publish all files in the project. NOTE: this might not actually work.
        ;; (cd org-publish-local-root)
        (org-publish-projects org-publish-local-alist)
        ;; (cd buffer-directory)

        ;; restore project-alist variable
        (cl-rotatef org-publish-local-alist org-publish-project-alist)))))

(defun publish-local-fix-base-dir (root-dir plist)
  "updates :base-directory in project plist to be a subdirectory of root-dir.

if :base-directory is absolute, then then this function simply
returns a copy of project. when :base-directory is relative, that
key is updated in a copy of project, which is returned.

root-dir is the directory where export-config.el is located. It
should be in the form of /foo/bar/

project is the project plist"
;;;
;;; org-publish-fix-base-dir
;;;

  (let ((new-plist (copy-tree plist))
        relative-dir)
    (dolist (key '(:base-directory :publishing-directory) new-plist)
      (setf relative-dir (plist-get plist key))
      (cond ((f-relative-p root-dir)
             (error "root-dir must be absolute: %S" root-dir))
            ;; don't return error if base-directory is nil
            ((and relative-dir
                  (not (directory-name-p relative-dir)))
             (error "%S must be a directory. (did you mean  %S?)"
                    relative-dir
                    (concat relative-dir "/"))))

      ;; update key with either expanded or replaced filename
      (setf new-plist
            (if (and relative-dir
                     (f-relative-p relative-dir))
                (plist-put (copy-tree new-plist) key
                           (expand-file-name relative-dir root-dir))
              (copy-tree new-plist))))))

(setq my-proj (copy-tree org-publish-local-alist))

(publish-local-fix-base-dir "~/Documents/personal_webpage/" (cdr (car my-proj)))
my-proj

;; this function is no longer used in this file, but I like it so much, I can't
;; bring myself to delete it.
(defun alist-update-key (key value alist)
  "returns a new alist with the value at key updated.

The whole structure of the alist is copied over (ie copy-tree vs
copy-list). the result is an alist with key removed, and a new
element with key pushed to the front of the alist. "
;;;
;;; alist-update-key
;;;
  (cons (list key value)
        (assq-delete-all key (copy-tree alist))))
