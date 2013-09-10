
(let ((base "~/.emacs.d/selwyn")) "~/.emacs.d/selwyn"
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(setq use-cua t)
;;rebind rectangle mark as I want to use C-return for R/ESS and
;;sending commands to the shell
(setq cua-rectangle-mark-key "")
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(cua-mode 1)


(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;   "A list of packages to ensure are installed at launch."            
(defvar my-packages '(
                      scala-mode2
                      fuzzy
                      zenburn-theme
                      autopair
                      auto-complete
                      yasnippet
                      starter-kit
                      starter-kit-js
;;              starter-kit-bindings -- something in here inteferes
              ;;              with shift-select take just those
              ;;              bindings of interest
		       ))


(dolist (p my-packages)
   (when (not (package-installed-p p))
     (package-install p)))

;; autopair and yas in all modes
(autopair-global-mode)
(yas-global-mode 1)

(setq-default default-tab-width 4)

(load-theme 'zenburn t)

;;(add-to-list 'load-path "~/.emacs.d/pig-mode/")
(require 'pig-mode)

(require 'scala-mode2)

;; load the ensime lisp code...
(add-to-list 'load-path "~/.emacs.d/selwyn/ensime_2.9.2-0.9.8.9/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq x-select-enable-clipboard t)

(setq my-font "Droid Sans Mono-10")     

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;;R stuff
(require 'ess-site)
;;(load "~/.emacs.d/selwyn/ESS/lisp/ess-site")
(ess-toggle-underscore nil)
(define-key ess-mode-map "\C-l" 'ess-eval-line-and-step)
(define-key ess-mode-map [(control return)] 'ess-eval-line-and-step)
;;(global-set-key [(control return)] 'ess-eval-line-and-step)

;;(global-set-key [(alt f1)] 'cua-rectangle-mark-key)
;;(define-key ess-mode-map  (kbd "C-<return>") 'ess-eval-line-and-step)

(define-key ess-mode-map "\C-p" 'ess-eval-function-or-paragraph-and-step)
(define-key ess-mode-map "\C-r" 'send-region-to-R)
(setq ess-ask-for-ess-directory nil)

;; polymode so can combine R code with markdown in Rmd files
(setq load-path
      (append '("/home/selwyn/.emacs.d/selwyn/polymode/"  "/home/selwyn/.emacs.d/selwyn/polymode/modes")
              load-path))

(require 'poly-R)
(require 'poly-markdown)

;;               
;; from: http://rstudio-pubs-static.s3.amazonaws.com/2246_f78ac20295054967916c7855d3610317.html
(defun ess-knit2html ()
  "Run knit2html R function on the current .Rmd file"
  (interactive)
  (ess-swv-run-in-R "require(knitr) ; require(markdown) ; knit2html")
)              
              
(defun ess-knit2pdf ()
  "Run knit2pdf R function on the current .Rmd file"
  (interactive)
  (ess-swv-run-in-R "require(knitr) ; require(markdown) ; knit2pdf")
)              


;;Mac key bindings - http://stackoverflow.com/questi3ons/7231651/emacs-on-mac-os-x-lion-forward-delete 
(global-set-key '[(kp-delete)] 'delete-char)
(global-set-key '[(delete)] 'delete-char)
(global-set-key '[(meta delete)] 'kill-word)
(global-set-key '[(meta kp-delete)] 'kill-word)
(global-set-key '[(control delete)] 'kill-word)
(global-set-key '[(control kp-delete)] 'kill-word)
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(setq mac-command-modifier 'control)
              
;;http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
(defun sh-send-line-or-region (&optional step)
  (interactive ())
;;  (let ((proc (get-process "shell"))
  (let ((proc (get-process "*SQL*"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        ;;(setq proc (get-process "shell"))
        (setq proc (get-process "*SQL*"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step 
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(global-set-key [(control return)] 'sh-send-line-or-region-and-step)
;;(define-key sh-mode-map [(control ?j)] 
;;(define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)
(setq comint-scroll-to-bottom-on-output t)

(global-set-key  [f2]   'shell)

(require 'essh)                                                    ;;
(defun essh-sh-hook ()                                             ;;
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)        ;;
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)        ;;
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)          ;;
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step) ;;
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)      ;;
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)) ;;
(add-hook 'sh-mode-hook 'essh-sh-hook)                             ;;

;; starter-kit bindings of interest that do not interfere with
;; shift-selection

  ;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

 ;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;;Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;;SQL stuff
(eval-after-load "sql"
      (load-library "sql-indent"))

;;make long lines play nice - you can navigte  
(defalias 'tt 'toggle-truncate-lines) 


;; redshift connection via M-x sql-postgres
;; (defun sql-get-login (&rest what) 
;;   (setq sql-server "warehouse.ceh4g9e4mhbh.eu-west-1.redshift.amazonaws.com"
;;         sql-user "smccracken"
;;         sql-password "GxKsBVMLMmtqht3U"
;;         sql-database "data"
;;         sql-port 5439
;;         ))

;; connect to multiple databases in sql-mode
;; http://speeves.erikin.com/2011/10/emacs-sql-mode-connect-to-multiple.html
;; define a set of database conections
;; not currently working as expected...
;; (setq sql-connection-alist
;;       '(
;;         (pool-redshift
;;          (sql-name "redshift")
;;          (sql-product 'postgres)
;;          (sql-server "warehouse.ceh4g9e4mhbh.eu-west-1.redshift.amazonaws.com")
;;          (sql-user "smccracken")
;;          (sql-password "GxKsBVMLMmtqht3U")
;;          (sql-database "data")
;;          (sql-port 5439)
;;          )
;;         (pool-pglocal
;;          (sql-name "pglocal")
;;          (sql-product 'postgres)
;;          (sql-server "localhost")
;;          (sql-user "selwyn")
;;          (sql-password "")
;;          (sql-database "selwyn")
;;          (sql-port 5432)
;;          )
  
;;         )
;;       )

;; (defun sql-pool-redshift ()
;;   (interactive)
;;   (sql-connect-preset 'redshift)
;; )

;; (defun sql-pool-pglocal ()
;;   (interactive)
;;   (sql-connect-preset 'pglocal)
;; )

;; ;; this makes all it all happen via M-x sql-pool-host1_db1, etc.
;; (defun sql-connect-preset (name)
;;   "Connect to a predefined SQL connection listed in `sql-connection-alist'"
;;   (eval `(let ,(cdr (assoc name sql-connection-alist))
;;            (flet ((sql-get-login (&rest what)))
;;              (sql-product-interactive sql-product)))))

;; ;; names the buffer *SQL: <host>_<db>, which is easier to 
;; ;; find when you M-x list-buffers, or C-x C-b
;; (defun sql-make-smart-buffer-name ()
;;   "Return a string that can be used to rename a SQLi buffer.
;;   This is used to set `sql-alternate-buffer-name' within
;;   `sql-interactive-mode'."
;;   (or (and (boundp 'sql-name) sql-name)
;;       (concat (if (not(string= "" sql-server))
;;                   (concat
;;                    (or (and (string-match "[0-9.]+" sql-server) sql-server)
;;                        (car (split-string sql-server "\\.")))
;;                    "/"))
;;               sql-database)))

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
;;             (sql-rename-buffer)))


;;inspired by http://www.emacswiki.org/emacs/AlexSchroederConfigWindows
(add-hook 'sql-mode-hook
	  (lambda ()

        (local-set-key (kbd "<C-return>") 'sql-send-region)

        (local-set-key (kbd "<f5>") 'sql-send-region)
        (local-set-key (kbd "<f6>") 'sql-send-buffer)
        (local-set-key (kbd "<f4>") 'sql-send-paragraph)

))





;;markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;(require 'realtime-markdown-viewer)

;; connect to a specified db
;; http://curiousprogrammer.wordpress.com/2009/04/16/dangerous-elisp/
(defun pglocal ()
  (interactive)
  (let (
        (sql-name "pglocal")
        (sql-product 'postgres)
        (sql-server "localhost")
        (sql-user "selwyn")
        (sql-password "")
        (sql-database "selwyn")
        (sql-port 5432)
        )
    (delete-other-windows)
    (sql-postgres)
    (other-window -1)
    (switch-to-buffer "*sql*" t)
    (sql-mode)
    )
  )


(defun redshift ()
  (interactive)
  (let (      
        (sql-name "redshift")
        (sql-product 'postgres)
        (sql-server "warehouse.ceh4g9e4mhbh.eu-west-1.redshift.amazonaws.com")
        (sql-user "smccracken")
        (sql-password "GxKsBVMLMmtqht3U")
        (sql-database "data")
        (sql-port 5439)     
        )
    (delete-other-windows)
    (sql-postgres)
    (other-window -1)
    (switch-to-buffer "*sql*" t)
    (sql-mode)
    )
  )
