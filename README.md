# org-link-github

Add GitHub links to org-mode (see [External links](https://orgmode.org/guide/Hyperlinks.html#External-Links-1)).

See also [Descriptive links](#descriptive-links) below.

## Examples

```
gh:scikit-image/scikit-image#1000
```

## Configuration

By customizing `org-link-github-shortcuts`, you can have even shorter URLs:

```
gh:skimage#1000
```

`org-link-github-shortcuts` should be an association list of shortcut-repo:

```lisp
(setq org-link-github-shortcuts
     '(("skimage" . "scikit-image/scikit-image")
       ("numpy" . "numpy/numpy")))
```

## Installation

```lisp
(use-package org-link-github
  :vc (:fetcher github :repo stefanv/org-link-github)
  :commands (org-link-github-open org-link-github-export)
  :custom (org-link-github-shortcuts '(("skimage" . "scikit-image/scikit-image")
                                       ("numpy" . "numpy/numpy")))
  :init (org-link-set-parameters "gh"
          :follow #'org-link-github-open
          :export #'org-link-github-export))
```

### Emacs <29

Either place `org-link-github.el` on your `load-path`, or add
`vc-use-package` and follow the installation instructions above:

```
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
```

## Advanced usage

I also provide `org-link-github-contract-url` to convert GitHub URLs
to the short org form. You can paste URLs to `gh:...` links as follows:

```lisp
(defun stefanv/gh-paste ()
  (interactive)
  (insert
   (format "[[gh:%s]]"
           (string-trim (org-link-github-contract-url
                         (substring-no-properties
                          (gui-get-selection 'CLIPBOARD)))))))
```

With, e.g., a key binding:

```lisp
(use-package org-link-github
    :commands (org-link-github-contract-url ...)

    ...

    :init
    (define-key org-mode-map (kbd "C-x C-g") 'stefanv/gh-paste))
```

## Descriptive Links

Nowadays, I find myself preferring links that contain the title of the PR / issue. I.e., a link that looks like:

```
reponame#123 · PR title goes here
```

For that, I use:

```lisp
  (defun stefanv/gh-paste-link-with-description ()
    "Insert Org link and reformat to 'repo#prnum · title'."
    (interactive)
    (org-web-tools-insert-link-for-url (org-web-tools--get-first-url))
    (save-excursion
      (let* ((link-data (org-element-context))
             (raw-link (org-element-property :raw-link link-data))
             (old-desc (org-element-property :contents-begin link-data)))
        (when old-desc
          (let* ((desc (buffer-substring-no-properties
                        (org-element-property :contents-begin link-data)
                        (org-element-property :contents-end link-data)))
                 (new-desc (replace-regexp-in-string
                             "^\\(.*?\\) · .*?/\\(.*\\)$" "\\2 · \\1"  ; Swap desc and user/repo#nr, remove user/
                             (replace-regexp-in-string
                               " by [^·]+ · " " · "  ; Remove "by username" part
                               (replace-regexp-in-string
                                 "\\(?:Pull Request\\|Issue\\) #\\([0-9]+\\) · \\(.*?\\) · GitHub"
                                 "\\2#\\1" desc)))))
            (when (string-match-p "\\(?:Pull Request\\|Issue\\) #" desc)
              (delete-region (org-element-property :begin link-data)
                             (org-element-property :end link-data))
              (insert (format "[[%s][%s]]" raw-link new-desc))))))))
```

Note that this requires the `org-web-tools` package:

```lisp
(use-package org-web-tools
  :commands org-web-tools-insert-link-for-url
  :after org)
```
