(require 'url)
(require 'cl)

(defun percona-launchpad-user ()
  "sergei.glushchenko")

(defun percona-top-dir-get ()
  "~/percona/repo")

(defun launchpad-path-pam-p (path)
  (string= "percona-pam-for-mysql" (cadr (split-string path "/"))))

(defun launchpad-path-xtrabackup-p (path)
  (string= "percona-xtrabackup" (cadr (split-string path "/"))))

(defun launchpad-path-server-p (path)
  (string= "percona-server" (cadr (split-string path "/"))))

(defun launchpad-path-project-get (path)
  (cond
   ((launchpad-path-xtrabackup-p path) "xb-")
   ((launchpad-path-server-p path) "ps-")
   ((launchpad-path-pam-p path) "pam-")))

(defun launchpad-path-bug-p (path)
  (string= "+bug" (caddr (split-string path "/"))))

(defun launchpad-path-type-get (path)
  (cond
   ((string= "+bug" (caddr (split-string path "/"))) "bug")
   (t "blueprint-")))

(defun launchpad-path-id-get (path)
  (file-name-base path))

(defun launchpad-url-path-get (launchpad-url)
  (url-filename (url-generic-parse-url launchpad-url)))

(defun launchpad-url-ticket-id-get (launchpad-url)
  (launchpad-path-id-get (url-filename)))

(defun percona-project-name-gen ()
  (let
      ((path (launchpad-url-path-get (percona-launchpad-url-get))))
    (concat (launchpad-path-project-get path)
            (launchpad-path-type-get path)
            (launchpad-path-id-get path))))

(defun percona-launchpad-branch-name-gen ()
  (format
   "lp:~%s/%s/%s"
   (percona-launchpad-user)
   (percona-launchpad-project-get)
   (percona-project-dir-get)))

(defun run-it (command)
  (async-shell-command command "*checkout*")
  (pop-to-buffer "*checkout*")
  (buffer-disable-undo))

(defun eventum-path-id-get (eventum-path)
  (cadr (split-string eventum-path "=")))

(defun eventum-url ()
  (org-entry-get nil "issue" 'inherit))

(defun percona-project-prefix-get ()
  (let ((eventum-url (eventum-url)))
    (cond
     (eventum-url
      (let
          ((issue-type-prefix (if (member "bt" (org-get-tags-at nil)) "BT" "ST"))
           (path (url-filename (url-generic-parse-url eventum-url))))
        (concat issue-type-prefix (eventum-path-id-get path) "-"))))))

(defun percona-xtrabackup-setup ()
  (let
      ((project-root
        (concat
         (percona-project-prefix-get)
         (percona-project-name-gen)))
       (top-dir (percona-top-dir-get)))
    (org-entry-put nil "project-root" project-root)
    (run-it
     (concat
      top-dir
      "/percona_create_branches_wrapper.sh xb "
      project-root))))

(defun percona-server-setup ()
  (let
      ((project-root
        (concat
         (percona-project-prefix-get)
         (percona-project-name-gen)))
       (top-dir (percona-top-dir-get)))
    (percona-project-root-put project-root)
    (run-it
     (concat
      top-dir
      "/percona_create_branches_wrapper.sh server "
      project-root))))

(defun percona-pam-setup ()
  (let
      ((project-root (percona-project-name-gen))
       (top-dir (percona-top-dir-get)))
    (percona-project-root-put project-root)
    (run-it
     (concat
      top-dir
      "/percona_create_branches_wrapper.sh pam "
      project-root))))

(defun percona-project-setup ()
  "setup project associated with current heading"
  (interactive)
  (cond
   ((percona-pam-p) (percona-pam-setup))
   ((percona-xtrabackup-p) (percona-xtrabackup-setup))
   ((percona-server-p) (percona-server-setup))
   (t (error "unknown project"))))

(defun percona-project-goto ()
  "reveal project associated with current heading in iTerm2"
  (interactive)
  (let
      ((project-root (percona-project-root-get))
       (top-dir (percona-top-dir-get)))
    (shell-command
     (concat
      (file-name-as-directory top-dir)
      "reveal_terminal_at.sh "
      project-root))))


(defun percona-project-path-kill ()
  "kill the full path to project"
  (interactive)
  (let
      ((project-root (percona-project-root-get))
       (top-dir (percona-top-dir-get)))
    (message "project name killed")
    (kill-new
     (concat
      (file-name-as-directory top-dir)
      project-root))))


(defun percona-project-path-eshell ()
  "Open shell for current project"
  (interactive)
  (let*
      ((dir (percona-project-dir-full-get)))
    (eshell)
    (with-current-buffer "*eshell*"
      (insert
       (concat
        "cd "
        dir))
      (eshell-send-input))))

(defun percona-project-root-get ()
  (org-entry-get nil "project-root" 'inherit))

(defun percona-project-root-full-get ()
  (concat
   (file-name-as-directory (percona-top-dir-get)) (percona-project-root-get)))

(defun percona-project-version-get ()
  (org-entry-get nil "version" 'inherit))

(defun percona-project-dir-full-get ()
  (concat
   (file-name-as-directory (percona-project-root-full-get))
   (percona-project-dir-get)))

(defun percona-project-dir-get ()
  (cond
   ((percona-pam-p) "")
   (t (concat (percona-project-version-get) "-" (percona-project-root-get)))))

(defun percona-project-root-put (project-root)
  (org-entry-put nil "project-root" project-root))

(defun percona-launchpad-url-get ()
  (org-entry-get nil "URL" 'inherit))

(defun percona-bug-p ()
  (launchpad-path-bug-p (launchpad-url-path-get (percona-launchpad-url-get))))

(defun percona-launchpad-id-get ()
  (launchpad-path-id-get (launchpad-url-path-get (percona-launchpad-url-get))))

(defun percona-launchpad-project-get ()
  (cadr
   (split-string (launchpad-url-path-get (percona-launchpad-url-get)) "/")))

(defun percona-pam-p ()
  (launchpad-path-pam-p (launchpad-url-path-get (percona-launchpad-url-get))))

(defun percona-xtrabackup-p ()
  (launchpad-path-xtrabackup-p
   (launchpad-url-path-get (percona-launchpad-url-get))))

(defun percona-server-p ()
  (launchpad-path-server-p
   (launchpad-url-path-get (percona-launchpad-url-get))))

(defun trim (line)
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" line)))

(defun trim-multi-line (text)
  (let ((result ""))
    (progn
      (dolist (line (split-string text "\n" t))
        (setq result (concat result (trim line) "\n"))))
    result))

(defun outline-write-body-to-file (f)
  (let ((body (outline-get-body)))
    (with-temp-file f
      (insert body))))

(defun outline-get-body ()
  (trim-multi-line
   (save-excursion
     (buffer-substring
      (progn
        (outline-back-to-heading)
        (outline-end-of-heading)
        (point))
      (progn
        (outline-next-preface)
        (point))))))

(defun percona-branch-commit ()
  (interactive)
  (let ((f (make-temp-file "commit-message")))
    (progn
      (outline-write-body-to-file f)
      (let
          ((bug-id (if (percona-bug-p) (percona-launchpad-id-get) "")))
        (run-it
         (format
          "%s/percona_commit.sh %s %s %s"
          (percona-top-dir-get)
          (percona-project-dir-full-get)
          f
          bug-id))))))

(defun percona-branch-push ()
  (interactive)
  (run-it
   (format
    "%s/percona_push.sh %s %s"
    (percona-top-dir-get)
    (percona-project-dir-full-get)
    (percona-launchpad-branch-name-gen))))

(defun todo-bugfix-create (title url)
  (outline-insert-heading)
  (insert "TODO Bug " (launchpad-path-id-get (launchpad-url-path-get url)) ": " title)
  (org-entry-put nil "URL" url))

(defun percona-bug-fix ()
  (interactive
   (let
       ((url (read-string "Bug URL: " nil 'my-history)))
     (cond
      ((launchpad-path-bug-p (launchpad-url-path-get url))
       (let
           ((title (trim
                    (shell-command-to-string
                     (concat "LC_ALL=en_US.UTF-8 /usr/bin/env python ~/scripts/lp.py bugtitle "
                             (launchpad-path-id-get (launchpad-url-path-get url)))))))
         (todo-bugfix-create title url)))
      (t (error "Unknown URL"))))))

(provide 'percona)
