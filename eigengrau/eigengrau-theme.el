(setq custom--inhibit-theme-enable nil)
(require 'eigengrau-definitions
         (locate-file "eigengrau-definitions.el" custom-theme-load-path
                      '("c" "")))

(create-eigengrau-theme)
