
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
(load "~/.emacs.d/selwyn/ESS/lisp/ess-site")
(ess-toggle-underscore nil)
(define-key ess-mode-map "\C-l" 'ess-eval-line-and-step)
(define-key ess-mode-map [(control return)] 'ess-eval-line-and-step)
;;(global-set-key [(control return)] 'ess-eval-line-and-step)

;;(global-set-key [(alt f1)] 'cua-rectangle-mark-key)
;;(define-key ess-mode-map  (kbd "C-<return>") 'ess-eval-line-and-step)

(define-key ess-mode-map "\C-p" 'ess-eval-function-or-paragraph-and-step)
(define-key ess-mode-map "\C-r" 'send-region-to-R)
(setq ess-ask-for-ess-directory nil)

;;http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
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
