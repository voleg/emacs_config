(defun helje/mac-clipboard-html2org-yank ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' 2>/dev/null | pandoc -f html -t json 2>/dev/null | pandoc -f json -t org 2>/dev/null | sed 's/ / /g' 2>/dev/null "))
  (yank))
