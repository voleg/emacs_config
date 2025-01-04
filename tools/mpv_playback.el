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

					;; Insert the link to the saved video file
          (save-excursion
						(end-of-line) ; Move to the end of the current line
            (newline)     ; Insert a newline for the new link
						(org-insert-link nil (format "%s" full-path) "saved"))
					
          (message "Saving video to %s" full-path))
      (message "No valid video link under point!"))))


(defun encode-to-mp4 ()
	(interactive)
  (let ((link (org-element-context))) ; Get the element at point
    (if (eq (org-element-type link) 'link) ; Ensure the cursor is on a link
        (let* ((input-path (expand-file-name (org-element-property :path link)))
               (output-path (concat (file-name-sans-extension input-path) ".mp4")))
					(message "Starting FFmpeg encoding: %s -> %s" input-path output-path)
          (start-process
           "ffmpeg" "*ffmpeg*"
           "ffmpeg"
					 "-y"
           "-i" (format "%s" input-path)
           "-c:v" "libx264"
           "-c:a" "aac"
					 "-strict" "experimental"
           (format "%s" output-path))

          ;; Insert the encoded video link after the original link
          (save-excursion
            (org-end-of-line)
            (insert "\n" (format "[[%s][encoded]]" output-path)))
          (message "FFmpeg encoding started: %s" output-path))
      (message "No valid link under point!"))))

(defun encode-to-smallest-mp4 ()
  "Run FFmpeg encoding on a video file linked in an Org file.
Insert a link to the encoded file below the original link.
The encoded video is saved in the same directory as the original file."
  (interactive)
  (let ((link (org-element-context))) ; Get the element at point
    (if (eq (org-element-type link) 'link) ; Ensure the cursor is on a link
        (let* ((input-path (expand-file-name (org-element-property :path link)))
               (output-path (concat (file-name-sans-extension input-path) ".mp4")))
					(message "Starting FFmpeg encoding: %s -> %s" input-path output-path)
          (start-process
           "ffmpeg" "*ffmpeg*"
           "ffmpeg"
					 "-y"
           "-i" (format "%s" input-path)
           "-c:v" "libx265"
					 "-preset" "slower"
					 "-crf" "30"
					 "-tag:v" "hvc1" ;; set a tag it's relevant for QuickTime player. 
           "-c:a" "aac"
					 "-b:a" "48k"
					 "-ac" "1"  ;; mono 48kb aac sounds ok for voice
					 "-movflags" "+faststart"
 					 "-strict" "experimental"
           (format "%s" output-path))

          ;; Insert the encoded video link after the original link
          (save-excursion
            (org-end-of-line)
            (insert "\n" (format "[[%s][encoded]]" output-path)))
          (message "FFmpeg encoding started: %s" output-path))
      (message "No valid link under point!"))))


(define-key video-keymap (kbd "o") 'open-video-link-in-mpv)
(define-key video-keymap (kbd "s") 'save-video-with-mpv)
(define-key video-keymap (kbd "e") 'encode-to-mp4)
(define-key video-keymap (kbd "a") 'encode-to-smallest-mp4)  ;; a - Archive supper slow and high compression (for screencasts)
