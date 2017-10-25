;; customaisations to handle ibuffer and buffer switching

;; Bind keys:
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key [f12] 'electric-buffer-list)

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(global-set-key [kbd (apps)] 'ibuffer)

(provide 'de_buffer)