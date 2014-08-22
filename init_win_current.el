(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/monokai-theme-20140814.556")
(load-theme 'monokai t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "<f10>") 'menu-bar-mode)

(setq use-cua t)

;;http://news.slashdot.org/story/08/11/07/0533222/stupid-useful-emacs-tricks
(fset 'yes-or-no-p 'y-or-n-p) ; stop forcing me to spell out "yes"
(setq inhibit-startup-message t)
(setq backup-directory-alist '(("." . "~/.emacs-backups"))) ; stop leaving backup~ turds scattered everywhere


;;rebind rectangle mark as I want to use C-return for R/ESS and
(setq cua-rectangle-mark-key  [M-return])
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(cua-mode 1)


(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(setq my-font "Droid Sans Mono-10")     

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; ;;R stuff
(require 'ess-site)
;;(load "~/.emacs.d/selwyn/ESS/lisp/ess-site")
(ess-toggle-underscore nil)
(define-key ess-mode-map "\C-l" 'ess-eval-line-and-step)
(define-key ess-mode-map [(control return)] 'ess-eval-line-and-step)


;; connect to a specified db
;; http://curiousprogrammer.wordpress.com/2009/04/16/dangerous-elisp/
(defun pglocal ()
  (interactive)
  (let (
        (sql-name "pglocal")
        (sql-product 'postgres)
        (sql-server "localhost")
        (sql-user "mccracsb")
        (sql-password "")
        (sql-database "testy_poo")
        (sql-port 5432)
        )
    (delete-other-windows)
    (sql-postgres)
    (other-window -1)
    (switch-to-buffer "*sql*" t)
    (sql-mode)
    )
  )


  ;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)
 
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
