# org-link-github

Add GitHub links to org-mode (see [External links](https://orgmode.org/guide/Hyperlinks.html#External-Links-1)).

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
