;;; ../../development/dotfiles/.config/doom/llm.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq
   gptel-model "mistral:latest"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("mistral:latest"))))

(use-package! ellama
  :config
  ;; setup key bindings
  (setq! ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setq! ellama-language "English")
  ;;  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setq ellama-provider
        (make-llm-ollama
         ;; this model should be pulled to use it
         ;; value should be the same as you print in terminal during pull
         :chat-model "mistral:latest"
         :embedding-model "mistral:latest"
         ;; :default-chat-non-standard-params '(("num_ctx" . 8192))
         ))
  ;;  ;; Predefined llm providers for interactive switching.
  ;;  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;;  ;; without it. It is just example.
  (setq ellama-providers
        '(("zephyr" . (make-llm-ollama
                       :chat-model "zephyr:latest"
                       :embedding-model "zephyr:latest"))
          ("mistral" . (make-llm-ollama
                        :chat-model "mistral:latest"
                        :embedding-model "mistral:latest"))
          ("mixtral" . (make-llm-ollama
                        :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
                        :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setq ellama-naming-provider
        (make-llm-ollama
         :chat-model "zephyr:latest"
         :embedding-model "zephyr:latest"
         :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;;  ;; ;; Translation llm provider
  (setq ellama-translation-provider (make-llm-ollama
                                     :chat-model "phi3:14b-medium-128k-instruct-q6_K"
                                     :embedding-model "nomic-embed-text"))
  )
