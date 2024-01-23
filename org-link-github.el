;; Allow org to understand github links
;;
;; Examples:
;;
;;  gh:scikit-image/scikit-image#1000
;;
;; By customizing org-gh-repo-shortcuts, you can have even shorter URLs:
;;
;;  gh:skimage#1000
;;
;; It should be an association list of shortcut-repo:
;;
;; (setq org-gh-repo-shortcuts
;;      '(("skimage" . "scikit-image/scikit-image")
;;        ("numpy" . "numpy/numpy")))

(org-link-set-parameters "gh"
                         :follow #'org-gh-open
                         :export #'org-gh-export)

(defcustom org-gh-repo-shortcuts nil
  "Shortcuts to org/repo recognized by org gh links."
  :group 'org-link
  :type '(cons string string))

(defun org-gh-url-from-repo-issue (repo_issue)
  (if (string-match-p (rx "/") repo_issue)
      (let*
          ((re (rx (group (one-or-more any)) "/" (group (one-or-more any))
                   "#" (group (one-or-more digit))))
           (_ (string-match re repo_issue))
           (org (match-string 1 repo_issue))
           (repo (match-string 2 repo_issue))
           (nr (match-string 3 repo_issue)))
        (format "https://github.com/%s/%s/pull/%s" org repo nr))
    (let*
        ((re (rx (group (one-or-more any)) "#" (group (one-or-more digit))))
         (_ (string-match re repo_issue))
         (repo-shortcut (match-string 1 repo_issue))
         (nr (match-string 2 repo_issue))
         (org-repo (or (cdr (assoc repo-shortcut org-gh-repo-shortcuts))
                   (error "Could not find repo %s in org-gh-repo-shortcuts" repo-shortcut)))
         (org-and-repo (split-string org-repo "/"))
         (org (nth 0 org-and-repo))
         (repo (nth 1 org-and-repo)))
      (format "https://github.com/%s/%s/pull/%s" org repo nr))))

(defun org-gh-open (repo_issue _)
  "Open GitHub link to PR or issue.
REPO_ISSUE is of the format org/repo#issue-or-pr"
    (browse-url (org-gh-url-from-repo-issue repo_issue)))

(defun org-gh-export (link description format _)
  "Export a GitHub link from org files."
  (let ((path (org-gh-url-from-repo-issue link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (t path))))

(provide 'org-gh-links)
