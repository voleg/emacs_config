(flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                              '("pyproject.toml" "ruff.toml" ".ruff.toml"))

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.

See URL `https://beta.ruff.rs/docs/'."
  :command ("ruff"
            "check"
            (config-file "--config" flycheck-python-ruff-config)
            "--output-format=text"
            "--stdin-filename" source-original
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

;; Python config: Use ruff + mypy.
(defun python-flycheck-setup ()
  (progn
    (flycheck-select-checker 'python-ruff)
    (flycheck-add-next-checker 'python-ruff)
;;    (flycheck-add-next-checker 'python-ruff 'python-mypy)
    ))


(add-to-list 'flycheck-checkers 'python-ruff)
(add-hook 'python-mode-local-vars-hook #'python-flycheck-setup 'append)
