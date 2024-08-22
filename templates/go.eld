go-mode go-ts-mode

(header
 "// Copyright " (p (format-time-string "%Y")) " tabuyos. All rights reserved." n
 "//" n
 "// @author tabuyos" n
 "// @since " (p (format-time-string "%Y/%m/%d")) n
 "// @description " (p "description") n
 "package " (p (or (lim-simple-run-shell-command "go list -f '{{.Name}}'") (file-name-base (buffer-file-name)))) n n
 )

(app (i header) (i mn) n)

(lib (i header))
