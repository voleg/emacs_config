(defvar video-keymap (make-sparse-keymap)
  "Keymap for video-related commands.")

(define-key global-map (kbd "C-c v") video-keymap)

(defun open-video-link-in-mpv ()
  "Open video link under point in MPV player."
  (interactive)
  (let ((url (thing-at-point 'url t)))
    (if (and url
             (string-match "\\(youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\)" url))
        (start-process "mpv" "*mpv*" "mpv" url)
      (message "No valid video link under point!"))))

(defvar mpv-video-default-directory "~/Downloads/"
  "Default directory to save videos using MPV.")

(defun save-video-with-mpv (&optional directory)
  "Save video while playing using MPV.
Prompts for a file name, appends .mkv if missing, and saves in the specified DIRECTORY.
If DIRECTORY is not provided, uses `save-video-default-directory`."
  (interactive)
  (let* ((directory (or directory mpv-video-default-directory))
         (url (thing-at-point 'url t)))
    (if (and url
             (string-match "\\(youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\)" url))
        (let* ((filename (read-string "Save video as (without extension): "))
               (formatted-filename (if (string-suffix-p ".mkv" filename)
                                       filename
                                     (concat filename ".mkv")))
               (full-path (expand-file-name formatted-filename directory))
               (default-directory (file-name-as-directory directory)))
          (start-process "mpv" "*mpv*" "mpv" (format "--stream-record=%s" full-path) url)
          (message "Saving video to %s" full-path))
      (message "No valid video link under point!"))))

(define-key video-keymap (kbd "o") 'open-video-link-in-mpv)
(define-key video-keymap (kbd "s") 'save-video-with-mpv)
