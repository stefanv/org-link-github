(require 'map)
(require 'ol)

(defgroup org-link-github nil
  "Easy links to GitHub repositories."
  :group 'ol)

(defcustom org-link-github-shortcuts nil
  "Shortcuts to org/repo recognized by org gh links."
  :group 'org-link
  :type '(alist :key-type (string :tag "Short form")
                :value-type (string :tag "Full form" :doc "e.g. USERNAME/REPOSITORY")))

(defun org-link-github-expand-target (target)
  "Return full URL to issue/PR on GitHub based on TARGET.
TARGET is a shortcut found in `org-link-github-shortcuts'
followed by a \"#\" and an issue or PR number."
  (if (string-match (rx (group (one-or-more any)) "/" (group (one-or-more any))
                        "#" (group (one-or-more digit)))
                    target)
      ;; Target has both org and repo.
      (let ((org (match-string 1 target))
            (repo (match-string 2 target))
            (nr (match-string 3 target)))
        (format "https://github.com/%s/%s/pull/%s" org repo nr))
    ;; Target uses a shortcut.
    (unless (string-match (rx (group (one-or-more any)) "#" (group (one-or-more digit))) target)
      (error "Invalid target format"))
    (pcase-let* ((repo-shortcut (match-string 1 target))
                 (nr (match-string 2 target))
                 (org-repo (or (map-elt org-link-github-shortcuts repo-shortcut)
                               (error "Could not find repo %s in org-link-github-shortcuts" repo-shortcut)))
                 (`(,org ,repo) (split-string org-repo "/")))
      (format "https://github.com/%s/%s/pull/%s" org repo nr))))

(defun org-link-github-alias (target)
  "For the given target, return `org/repo#issue-or-pr'."
  (string-replace "https://github.com/" ""
                  (string-replace "/pull/" "#"
                                  (org-link-github-expand-target link))))

(defun org-link-github-open (target _)
  "Open GitHub link to PR or issue.
TARGET is of the format org/repo#issue-or-pr"
  (browse-url (org-link-github-expand-target target)))

(defun org-link-github-export (link description format _)
  "Return a GitHub link for exporting according to LINK, DESCRIPTION, and FORMAT."
  (let* ((path (org-link-github-expand-target link))
         (desc (or description (org-link-github-alias link))))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`md (format "[%s](%s)" desc path))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(org-link-set-parameters "gh"
                         :follow #'org-link-github-open
                         :export #'org-link-github-export)

(provide 'org-link-github)
