;;; Sensitive, secret-containing stuff gitignored.

;; Setup agent shell for trial.
;; See https://github.com/xenodium/agent-shell for setup details
(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_AUTH_TOKEN" "sk-S7B--pWBjyfVieny4bUIVg"
         "ANTHROPIC_BASE_URL" "https://litellm2.hometogo.rocks"))
  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)))

(provide 'work-sensitive)
