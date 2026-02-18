;; -*- lexical-binding: t; -*-
(put 'test-case-name 'safe-local-variable '(lambda (x) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(back-button benchmark-init buttercup circe claude-repl clj-refactor
                 clojure-ts-mode consult consult-projectile corfu-prescient
                 deadgrep deft dockerfile-mode dumb-jump easy-kill eat eca
                 exec-path-from-shell expand-region flimenu flycheck-biomejs
                 flycheck-clj-kondo flymake-ruff fzf general git-link go-mode
                 goto-last-change gptel graphviz-dot-mode
                 highlight-indent-guides highlight-parentheses hl-todo ialign
                 iedit jinja2-mode json-mode magit mini-frame minions moody
                 orderless pi-coding-agent po-mode point-stack python-pytest
                 pyvenv qml-mode restclient reverse-im sideline-flycheck
                 significant-other sql-indent string-inflection swift-mode
                 treesit-langs typescript-mode valign vcl-mode vertico-prescient
                 web-mode web-server websocket whole-line-or-region yaml-mode
                 yasnippet-snippets zig-mode))
 '(package-vc-selected-packages
   '((claude-repl :url "https://github.com/edpaget/edmacs" :branch "main" :lisp-dir
                  "modules/claude-repl")))
 '(safe-local-variable-values
   '((engine . go)
     (eval progn (put 'describe 'lisp-indent-function 1)
           (put 'context 'lisp-indent-function 1)
           (put 'it 'lisp-indent-function 1)
           (put 'before-each 'lisp-indent-function 0)
           (put 'after-each 'lisp-indent-function 0)
           (put 'before-all 'lisp-indent-function 0)
           (put 'after-all 'lisp-indent-function 0)
           (put 'spy-on 'lisp-indent-function 1))
     (python-pytest-executable . "poetry run pytest")
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
