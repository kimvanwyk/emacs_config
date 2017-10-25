;; Music playing functionality
;; EMMS stuff
(if (require-if-exists 'emms-setup "emms-3.0/")
    (progn
      (add-to-list 'exec-path "C:/utils/MPlayer-1.0rc2/")
    (emms-standard)
    (global-set-key (kbd "C-' C-s") 'emms-show)
    (global-set-key (kbd "C-' C-n") 'emms-next)
    (global-set-key (kbd "C-' C-b") 'emms-previous)
    (global-set-key (kbd "C-' C-p") 'emms-pause)
    (global-set-key (kbd "C-' C-l") 'emms)
    (emms-default-players)))


(provide 'de_music)