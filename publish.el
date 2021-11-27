(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				    'silent
				    'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defalias 'sup 'straight-use-package)

(sup 'org)
(sup 'htmlize)

(require 'ox-publish)

(defun expand-relative-path (path)
  "Expand relative PATH from current buffer or file to a full path"
  (concat
   (if load-file-name (file-name-directory load-file-name)
     default-directory)
   path))

(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      `(("pages"
         :base-directory ,(expand-relative-path "org/")
         :base-extension "org"
         :recursive nil
         :with-toc nil
         :with-properties nil
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-scripts nil
         :html-head-include-default-style t
         :html-validation-link nil
         :publishing-directory ,(expand-relative-path "publish/")
         :publish-function org-html-publish-to-html
         )
        ("posts"
         :base-directory ,(expand-relative-path "org/posts/")
         :base-extension "org"
         :exclude ".draft.org"
         :recursive t
         :with-toc nil
         :with-properties nil
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-scripts nil
         :html-head-include-default-style t
         :html-validation-link nil
         :auto-sitemap t
         :sitemap-title "archive"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :publishing-directory ,(expand-relative-path "publish/posts/")
         :publish-function org-html-publish-to-html)
        ("static"
         :base-directory ,(expand-relative-path "org/")
         :base-extension "css\\|txt\\|jpg\\|gif\\|png"
         :recursive t
         :publishing-directory ,(expand-relative-path "publish/")
         :publishing-function org-publish-attachment)
        ("www.kamelasa.dev" :components ("pages" "posts" "static"))))