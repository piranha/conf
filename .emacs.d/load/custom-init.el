(put 'test-case-name 'safe-local-variable '(lambda (x) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-clojure-cli-global-aliases ":user")
 '(package-selected-packages
   '(aidermacs auto-dark back-button benchmark-init buffer-name-relative cider
               claude-code-ide clj-refactor consult corfu-prescient ctrlf
               deadgrep deft dockerfile-mode dumb-jump dune eat eca ef-themes
               exec-path-from-shell expand-region feebleline flimenu
               flycheck-biomejs flycheck-clj-kondo flycheck-janet flycheck-joker
               flycheck-popup-tip flycheck-pos-tip flycheck-posframe fzf
               git-commit git-link go-mode goto-last-change gptel
               graphviz-dot-mode highlight-indent-guides highlight-parentheses
               hl-todo ialign iedit imenu-anywhere isearch-light janet-mode
               jinja2-mode json-mode kotlin-mode lua-mode magit markdown-mode
               mermaid-mode mini-frame mini-modeline minibuffer-line minions
               moody nav-flash openai pcache pkl-mode po-mode poetry point-stack
               popwin projectile python-pytest pyvenv rainbow-mode restclient
               sideline-flycheck significant-other significant-others sql-indent
               string-edit string-inflection swift-mode terraform-mode
               treesit-langs typescript-mode undo-tree utop vcl-mode
               vertico-prescient visible-mark visual-fill-column web-mode
               whole-line-or-region yaml-mode yaml-pro yasnippet-snippets
               zig-mode))
 '(package-vc-selected-packages
   '((significant-other :url "https://github.com/ovistoica/significant-other.el")
     (flycheck-biomejs :url "https://github.com/craneduck/flycheck-biomejs")
     (isearch-light :url "https://github.com/thierryvolpiatto/isearch-light")
     (claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")
     (relysium :url "https://github.com/bluzky/relysium")
     (aidermacs :url "https://github.com/MatthewZMD/aidermacs")))
 '(safe-local-variable-values
   '((engine . go) (python-shell-interpreter . "poetry run python")
     (python-pytest-executable . "poetry run pytest")
     (eval put 'api/defendpoint-async 'clojure-doc-string-elt 3)
     (eval put 'api/defendpoint 'clojure-doc-string-elt 3)
     (eval put 'api/defendpoint-schema 'clojure-doc-string-elt 3)
     (eval put 'defendpoint-async 'clojure-doc-string-elt 3)
     (eval put 'defendpoint 'clojure-doc-string-elt 3)
     (eval put 'defendpoint-schema 'clojure-doc-string-elt 3)
     (python-shell-interpreter-args . "run ipython3 --simple-prompt")
     (python-shell-interpreter . "uv")
     (eval define-clojure-indent (l/matcha '(1 (:defn))) (l/matche '(1 (:defn)))
           (p.types/def-abstract-type '(1 (:defn)))
           (p.types/defprotocol+ '(1 (:defn)))
           (p.types/defrecord+ '(2 nil nil (:defn)))
           (p.types/deftype+ '(2 nil nil (:defn)))
           (p/def-map-type '(2 nil nil (:defn))) (p/defprotocol+ '(1 (:defn)))
           (p/defrecord+ '(2 nil nil (:defn))) (p/deftype+ '(2 nil nil (:defn)))
           (tools.macro/macrolet '(1 ((:defn)) :form)))
     (eval put 'mu/defn 'clojure-doc-string-elt 2)
     (eval put 'mr/def 'clojure-doc-string-elt 2)
     (eval put 'mi/define-batched-hydration-method 'clojure-doc-string-elt 3)
     (eval put 'mi/define-simple-hydration-method 'clojure-doc-string-elt 3)
     (eval put 'methodical/defmulti 'clojure-doc-string-elt 2)
     (eval put 'methodical/defmethod 'clojure-doc-string-elt 3)
     (eval put 'p.types/defprotocol+ 'clojure-doc-string-elt 2)
     (eval put 's/defn 'clojure-doc-string-elt 2)
     (eval put 'setting/defsetting 'clojure-doc-string-elt 2)
     (eval put 'defsetting 'clojure-doc-string-elt 2)
     (eval put 'api.macros/defendpoint 'clojure-doc-string-elt 3)
     (eval put 'define-premium-feature 'clojure-doc-string-elt 2)
     (ftf-project-finders ftf-get-top-git-dir) (encoding . utf-8)
     (prompt-to-byte-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
