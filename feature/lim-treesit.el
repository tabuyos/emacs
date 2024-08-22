;;; lim-treesit.el --- Enhanced treesit -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Enhanced treesit.

;;; Code:

(require 'treesit)

(defvar lim-treesit-recipe-alist nil "Tree sitter language source list")

;;;###autoload
(defun lim-treesit-registry-recipe (lang &rest keys)
    "Registry recipe to `lim-treesit-recipe-alist'.

support keywords:

`:ts-mode' `:remap' `:url' `:revision'
`:source-dir' `:cc' `:c++' `:ext'."
  (unless (null keys)
    (when (plistp keys)
      (unless (alist-get lang lim-treesit-recipe-alist)
        (push (cons lang keys) lim-treesit-recipe-alist)))))

;;;###autoload
(defun lim-treesit-modify-recipe (lang &rest keys)
  "Modify `lang' if existed in `lim-treesit-recipe-alist'."
  (unless (null keys)
    (when (plistp keys)
      (when-let (recipe (alist-get lang lim-treesit-recipe-alist))
        (while keys
          (plist-put recipe (pop keys) (pop keys)))
        (setf (alist-get lang lim-treesit-recipe-alist) recipe)))))

(defun lim-treesit--enable (lang)
  "Enable treesit support for `lang'."
  (when-let (recipe (alist-get lang lim-treesit-recipe-alist))
    (let ((ts-mode (plist-get recipe :ts-mode))
          (remap (plist-get recipe :remap))
          (url (plist-get recipe :url))
          (revision (plist-get recipe :revision))
          (source-dir (plist-get recipe :source-dir))
          (cc (plist-get recipe :cc))
          (c++ (plist-get recipe :c++))
          (ext (plist-get recipe :ext)))

      (unless (assoc lang treesit-language-source-alist)
        (push
         (cons lang (list url revision source-dir cc c++))
         treesit-language-source-alist))

      (when (and ts-mode remap)
        (if (listp remap)
            (dolist (r remap)
              (setf (alist-get r major-mode-remap-alist) ts-mode))
          (setf (alist-get remap major-mode-remap-alist) ts-mode)))

      (when (and ts-mode ext)
        (unless (assoc ext auto-mode-alist)
          (push (cons ext ts-mode) auto-mode-alist))))))

;;;###autoload
(defun lim-treesit-enable (&rest langs)
  "Enable treesit support for `langs'."
  (dolist (lang langs)
    (lim-treesit--enable lang)))

(lim-treesit-registry-recipe
  'awk
  :ts-mode 'awk-ts-mode
  :remap 'awk-mode
  :url "https://github.com/Beaglefoot/tree-sitter-awk"
  :ext "\\.awk\\'")

(lim-treesit-registry-recipe
  'bash
  :ts-mode 'bash-ts-mode
  :remap 'sh-mode
  :url "https://github.com/tree-sitter/tree-sitter-bash"
  :ext "\\.sh\\'")

(lim-treesit-registry-recipe
  'bibtex
  :ts-mode 'bibtex-ts-mode
  :remap 'bibtex-mode
  :url "https://github.com/latex-lsp/tree-sitter-bibtex"
  :ext "\\.bib\\'")

(lim-treesit-registry-recipe
  'blueprint
  :ts-mode 'blueprint-ts-mode
  :remap 'blueprint-mode
  :url "https://github.com/huanie/tree-sitter-blueprint"
  :ext "\\.blp\\'")

(lim-treesit-registry-recipe
  'c
  :ts-mode 'c-ts-mode
  :remap 'c-mode
  :url "https://github.com/tree-sitter/tree-sitter-c"
  :ext "\\.c\\'")

(lim-treesit-registry-recipe
  'c-sharp
  :ts-mode 'csharp-ts-mode
  :remap 'csharp-mode
  :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
  :ext "\\.cs\\'")

(lim-treesit-registry-recipe
  'clojure
  :ts-mode 'clojure-ts-mode
  :remap '(clojure-mode clojurescript-mode clojurec-mode)
  :url "https://github.com/sogaiu/tree-sitter-clojure"
  :ext "\\.cljc?s?d?\\'")

(lim-treesit-registry-recipe
  'cmake
  :ts-mode 'cmake-ts-mode
  :remap 'cmake-mode
  :url "https://github.com/uyha/tree-sitter-cmake"
  :ext "\\.cmake\\'")

(lim-treesit-registry-recipe
  'commonlisp
  :ts-mode 'commonlisp-ts-mode
  :remap 'common-lisp-mode
  :url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"
  :ext "\\.cl\\'")

(lim-treesit-registry-recipe
  'cpp
  :ts-mode 'c++-ts-mode
  :remap 'c++-mode
  :url "https://github.com/tree-sitter/tree-sitter-cpp"
  :ext "\\.cpp\\'")

(lim-treesit-registry-recipe
  'css
  :ts-mode 'css-ts-mode
  :remap 'css-mode
  :url "https://github.com/tree-sitter/tree-sitter-css"
  :ext "\\.css\\'")

(lim-treesit-registry-recipe
  'dart
  :ts-mode 'dart-ts-mode
  :remap 'dart-mode
  :url "https://github.com/ast-grep/tree-sitter-dart"
  :ext "\\.dart\\'")

(lim-treesit-registry-recipe
  'dockerfile
  :ts-mode 'dockerfile-ts-mode
  :remap 'dockerfile-mode
  :url "https://github.com/camdencheek/tree-sitter-dockerfile"
  :ext "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'")

(lim-treesit-registry-recipe
  'elixir
  :ts-mode 'elixir-ts-mode
  :remap 'elixir-mode
  :url "https://github.com/elixir-lang/tree-sitter-elixir"
  :ext "\\.ex\\'")

(lim-treesit-registry-recipe
  'elisp
  :url "https://github.com/Wilfred/tree-sitter-elisp")

(lim-treesit-registry-recipe
  'glsl
  :ts-mode 'glsl-ts-mode
  :remap 'glsl-mode
  :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl")

(lim-treesit-registry-recipe
  'go
  :ts-mode 'go-ts-mode
  :remap 'go-mode
  :url "https://github.com/tree-sitter/tree-sitter-go"
  :ext "\\.go\\'")

(lim-treesit-registry-recipe
  'gomod
  :ts-mode 'go-mod-ts-mode
  :remap 'go-mod-mode
  :url "https://github.com/camdencheek/tree-sitter-go-mod"
  :ext "go\\.mod\\'")

(lim-treesit-registry-recipe
  'heex
  :ts-mode 'heex-ts-mode
  :remap 'heex-mode
  :url "https://github.com/phoenixframework/tree-sitter-heex"
  :ext "\\.heex\\'")

(lim-treesit-registry-recipe
  'html
  :ts-mode 'html-ts-mode
  :remap '(mhtml-mode sgml-mode)
  :url "https://github.com/tree-sitter/tree-sitter-html"
  :ext "\\.html\\'")

(lim-treesit-registry-recipe
  'janet
  :ts-mode 'janet-ts-mode
  :remap 'janet-mode
  :url "https://github.com/sogaiu/tree-sitter-janet-simple"
  :ext "\\.janet\\'")

(lim-treesit-registry-recipe
  'java
  :ts-mode 'java-ts-mode
  :remap 'java-mode
  :url "https://github.com/tree-sitter/tree-sitter-java"
  :ext "\\.java\\'")

(lim-treesit-registry-recipe
  'javascript
  :ts-mode 'js-ts-mode
  :remap '(js-mode javascript-mode js2-mode)
  :url "https://github.com/tree-sitter/tree-sitter-javascript"
  :revision "master"
  :source-dir "src"
  :ext "\\.js\\'")

(lim-treesit-registry-recipe
  'json
  :ts-mode 'json-ts-mode
  :remap '(js-json-mode json-mode jsonc-mode)
  :url "https://github.com/tree-sitter/tree-sitter-json"
  :ext "\\.json\\'")

(lim-treesit-registry-recipe
  'julia
  :ts-mode 'julia-ts-mode
  :remap 'julia-mode
  :url "https://github.com/tree-sitter/tree-sitter-julia"
  :ext "\\.jl\\'")

(lim-treesit-registry-recipe
  'kotlin
  :ts-mode 'kotlin-ts-mode
  :remap 'kotlin-mode
  :url "https://github.com/fwcd/tree-sitter-kotlin"
  :ext "\\.kts?\\'")

(lim-treesit-registry-recipe
  'latex
  :ts-mode 'latex-ts-mode
  :remap 'latex-mode
  :url "https://github.com/latex-lsp/tree-sitter-latex"
  :ext "\\.tex\\'")

(lim-treesit-registry-recipe
  'lua
  :ts-mode 'lua-ts-mode
  :remap 'lua-mode
  :url "https://github.com/tree-sitter-grammars/tree-sitter-lua"
  :ext "\\.lua\\'")

(lim-treesit-registry-recipe
  'magik
  :ts-mode 'magik-ts-mode
  :remap 'magik-mode
  :url "https://github.com/krn-robin/tree-sitter-magik"
  :ext "\\.magik\\'")

(lim-treesit-registry-recipe
  'make
  ;; :ts-mode 'makefile-ts-mode
  ;; :remap 'makefile-mode
  :url "https://github.com/tree-sitter-grammars/tree-sitter-make"
  :ext "\\([Mm]akefile\\|.*\\.\\(mk\\|make\\)\\)\\'")

(lim-treesit-registry-recipe
  'markdown
  :ts-mode 'markdown-ts-mode
  :remap '(poly-markdown-mode markdown-mode)
  :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
  :revision "split_parser"
  :source-dir "tree-sitter-markdown/src"
  :ext "\\.md\\'")

(lim-treesit-registry-recipe
  'markdown-inline
  :ts-mode 'markdown-ts-mode
  :remap '(poly-markdown-mode markdown-mode)
  :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
  :revision "split_parser"
  :source-dir "tree-sitter-markdown-inline/src"
  :ext "\\.md\\'")

(lim-treesit-registry-recipe
  'nix
  :ts-mode 'nix-ts-mode
  :remap 'nix-mode
  :url "https://github.com/nix-community/tree-sitter-nix"
  :ext "\\.nix\\'")

(lim-treesit-registry-recipe
  'nu
  :ts-mode 'nushell-ts-mode
  :remap 'nushell-mode
  :url "https://github.com/nushell/tree-sitter-nu"
  :ext "\\.nu\\'")

(lim-treesit-registry-recipe
  'ocaml
  :ts-mode 'ocaml-ts-mode
  :remap 'tuareg-mode
  :url "https://github.com/tree-sitter/tree-sitter-ocaml"
  :source-dir "grammars/ocaml/src"
  :ext "\\.ml[i]?\\'")

(lim-treesit-registry-recipe
  'org
  :ts-mode 'org-ts-mode
  :remap 'org-mode
  :url "https://github.com/milisims/tree-sitter-org"
  :ext "\\.org\\'")

(lim-treesit-registry-recipe
  'perl
  :ts-mode 'perl-ts-mode
  :remap 'perl-mode
  :url "https://github.com/ganezdragon/tree-sitter-perl"
  :ext "\\.pl\\'")

(lim-treesit-registry-recipe
  'php
  :ts-mode 'php-ts-mode
  :remap 'php-mode
  :url "https://github.com/tree-sitter/tree-sitter-php"
  :revision "master"
  :source-dir "php/src"
  :ext "\\.php[s345]?\\'")

(lim-treesit-registry-recipe
  'proto
  :ts-mode 'protobuf-ts-mode
  :remap 'protobuf-mode
  :url "https://github.com/mitchellh/tree-sitter-proto"
  :ext "\\.proto\\'")

(lim-treesit-registry-recipe
  'python
  :ts-mode 'python-ts-mode
  :remap 'python-mode
  :url "https://github.com/tree-sitter/tree-sitter-python"
  :ext "\\.py[iw]?\\'")

(lim-treesit-registry-recipe
  'r
  :ts-mode 'r-ts-mode
  :remap 'ess-mode
  :url "https://github.com/r-lib/tree-sitter-r"
  :ext "\\.r\\'")

(lim-treesit-registry-recipe
  'ruby
  :ts-mode 'ruby-ts-mode
  :remap 'ruby-mode
  :url "https://github.com/tree-sitter/tree-sitter-ruby"
  :ext "\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

(lim-treesit-registry-recipe
  'rust
  :ts-mode 'rust-ts-mode
  :remap '(rustic-mode rust-mode)
  :url "https://github.com/tree-sitter/tree-sitter-rust"
  :ext "\\.rs\\'")

(lim-treesit-registry-recipe
  'scala
  :ts-mode 'scala-ts-mode
  :remap 'scala-mode
  :url "https://github.com/tree-sitter/tree-sitter-scala"
  :ext "\\.\\(scala\\|sbt\\)\\'")

(lim-treesit-registry-recipe
  'sql
  ;; :ts-mode 'sql-ts-mode
  ;; :remap 'sql-mode
  :revision "gh-pages"
  :url "https://github.com/DerekStride/tree-sitter-sql"
  :ext "\\.sql\\'")

(lim-treesit-registry-recipe
  'surface
  :ts-mode 'surface-ts-mode
  :remap 'surface-mode
  :url "https://github.com/connorlay/tree-sitter-surface")

(lim-treesit-registry-recipe
  'toml
  :ts-mode 'toml-ts-mode
  :remap '(conf-toml-mode toml-mode)
  :url "https://github.com/tree-sitter/tree-sitter-toml"
  :ext "\\.toml\\'")

(lim-treesit-registry-recipe
  'tsx
  :ts-mode 'tsx-ts-mode
  :remap 'tsx-mode
  :url "https://github.com/tree-sitter/tree-sitter-typescript"
  :revision "master"
  :source-dir "tsx/src"
  :ext "\\.tsx\\'")

(lim-treesit-registry-recipe
  'typescript
  :ts-mode 'typescript-ts-mode
  :remap 'typescript-mode
  :url "https://github.com/tree-sitter/tree-sitter-typescript"
  :revision "master"
  :source-dir "typescript/src"
  :ext "\\.ts\\'")

(lim-treesit-registry-recipe
  'typst
  :ts-mode 'typst-ts-mode
  :remap 'typst-mode
  :url "https://github.com/uben0/tree-sitter-typst"
  :revision "master"
  :source-dir "src"
  :ext "\\.typ\\'")

(lim-treesit-registry-recipe
  'verilog
  :ts-mode 'verilog-ts-mode
  :remap 'verilog-mode
  :url "https://github.com/gmlarumbe/tree-sitter-verilog"
  :ext "\\.s?vh?\\'")

(lim-treesit-registry-recipe
  'vhdl
  :ts-mode 'vhdl-ts-mode
  :remap 'vhdl-mode
  :url "https://github.com/alemuller/tree-sitter-vhdl"
  :ext "\\.vhdl?\\'")

(lim-treesit-registry-recipe
  'vue
  :ts-mode 'vue-ts-mode
  :remap 'vue-mode
  :url "https://github.com/tree-sitter-grammars/tree-sitter-vue"
  :ext "\\.vue\\'")

(lim-treesit-registry-recipe
  'wast
  :ts-mode 'wat-ts-wast-mode
  :remap 'wat-mode
  :url "https://github.com/wasm-lsp/tree-sitter-wasm"
  :source-dir "wast/src"
  :ext "\\.wast\\'")

(lim-treesit-registry-recipe
  'wat
  :ts-mode 'wat-ts-mode
  :remap 'wat-mode
  :url "https://github.com/wasm-lsp/tree-sitter-wasm"
  :source-dir "wat/src"
  :ext "\\.wat\\'")

(lim-treesit-registry-recipe
  'wgsl
  :ts-mode 'wgsl-ts-mode
  :remap 'wgsl-mode
  :url "https://github.com/mehmetoguzderin/tree-sitter-wgsl"
  :ext "\\.wgsl\\'")

(lim-treesit-registry-recipe
  'yaml
  :ts-mode 'yaml-ts-mode
  :remap 'yaml-mode
  :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
  :ext "\\.ya?ml\\'")

(provide 'lim-treesit)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
