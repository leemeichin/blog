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
(sup 'org-contrib)
(sup 'htmlize)

;; Add colours for languages used in codeblocks
(sup 'ruby-mode)
(sup 'smalltalk-mode)
(sup 'yaml-mode)
(sup 'racket-mode)
(sup 'pollen-mode)
(sup 'prolog-mode)

(require 'ox-publish)
(require 'ox-rss)

(defun expand-relative-path (path)
  "Expand relative PATH from current buffer or file to a full path"
  (concat
   (if load-file-name (file-name-directory load-file-name)
     default-directory)
   path))

(defun read-template (template-name)
  "Read HTML template file TEMPLATE-NAME into string"
  (with-temp-buffer
    (insert-file-contents (expand-relative-path (concat "tpl/" template-name ".html")))
    (buffer-string)))

(defun kamelasa/sitemap-function-table (title files)
  "Format FILES as a an org-table with TITLE as the page title."
  (concat "#+TITLE: " title "\n\n"
          "| permissions | user | group | name |\n"
          "|--+--+--+--+--|\n"
          (org-list-to-generic
           files
           '(:raw t
             :splice t
             :isep "\n"
             :istart "| "
             :iend " |"
             :ifmt (lambda (type file)
                     (concat "-rw-r--r-- | lee | www | " file))))))

(defun kamelasa/sitemap-function-rss (title files)
  "Format FILES as a list of top level headers, with TITLE as the title."
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree files 1 '(:icount "" :istart ""))))

(defun kamelasa/sitemap-format-entry-rss (entry style project)
"Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat "posts/" (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" file title))
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "RSS_TITLE" title)
             (org-set-property "PUBDATE" date)
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun kamelasa/publish-rss (plist filename pub-dir)
   "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed.
This is to avoid republishing all other individual org-files."
   (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(setq org-html-htmlize-output-type 'css)
(setq org-rss-use-entry-url-as-guid t)

(setq org-publish-project-alist
      `(("posts"
         :base-directory ,(expand-relative-path "org/posts/")
         :base-extension "org"
         :exclude ,(regexp-opt '("rss.org" "index.org" ".draft.org"))
         :recursive t
         :with-toc nil
         :with-properties nil
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-validation-link nil
         :auto-sitemap t
         :sitemap-title "posts"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-function kamelasa/sitemap-function-table
         :html-head ,(read-template "head")
         :html-preamble ,(read-template "preamble")
         :html-postamble ,(read-template "postamble")
         :publishing-directory ,(expand-relative-path "publish/posts/")
         :publish-function org-html-publish-to-html)
        ("pages"
         :base-directory ,(expand-relative-path "org/")
         :base-extension "org"
         :recursive nil
         :with-toc nil
         :with-properties nil
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-validation-link nil
         :html-head ,(read-template "head")
         :html-preamble ,(read-template "preamble")
         :html-postamble ,(read-template "postamble")
         :publishing-directory ,(expand-relative-path "publish/")
         :publish-function org-html-publish-to-html)
        ("rss"
         :base-directory ,(expand-relative-path "org/posts/")
         :base-extension "org"
         :exclude ,(regexp-opt '("rss.org" "index.org" ".draft.org"))
         :recursive nil
         :rss-extension "xml"
         :html-link-home "https://www.leemeichin.com"
         :html-link-use-abs-url t
         :auto-sitemap t
         :sitemap-title "leemeichin.com"
         :sitemap-filename "rss.org"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-function kamelasa/sitemap-function-rss
         :sitemap-format-entry kamelasa/sitemap-format-entry-rss
         :publishing-directory ,(expand-relative-path "publish/")
         :publishing-function kamelasa/publish-rss)
        ("assets"
         :base-directory ,(expand-relative-path "assets/")
         :base-extension ".*"
         :recursive t
         :publishing-directory ,(expand-relative-path "publish/")
         :publishing-function org-publish-attachment)
        ("www.kamelasa.dev" :components ("posts" "pages" "assets" "rss"))))
