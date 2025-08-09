;;; my-images.el --- Image file trash management for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Provides functions to move image files to/from a .trash subdirectory
;; with convenient keybindings in image-mode.

;;; Code:

(require 'dired)
(require 'image-mode)

(with-eval-after-load 'image-file
  (add-to-list 'image-file-name-extensions "heic"))


(defun image-get-next-file ()
  "Return the filename of the next image file in the current directory, or nil.
Uses the current buffer's file as the starting point."
  (let ((current-file (buffer-file-name)))
    (when current-file
      (save-excursion
        (save-window-excursion
          (dired default-directory)
          (dired-goto-file current-file)
          (let ((found nil))
            (while (and (not found)
                        (= 0 (forward-line 1))
                        (not (eobp)))
              (let ((file (dired-get-filename nil t)))
                (when (and file
                           (not (file-directory-p file))
                           (string-match-p (image-file-name-regexp) file))
                  (setq found file))))
            (kill-buffer)
            found))))))

(defun trash-image-file ()
  "Move the current image file to a .trash subdirectory.
If successful, visits the next image file or the directory in dired mode."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (cond
     ((not current-file)
      (message "Buffer is not visiting a file"))
     
     ((string-match-p "/\\.trash/" current-file)
      (message "Cannot trash file already in .trash directory"))
     
     (t
      (condition-case err
          (let* ((dir (file-name-directory current-file))
                 (filename (file-name-nondirectory current-file))
                 (trash-dir (expand-file-name ".trash" dir))
                 (trash-file (expand-file-name filename trash-dir))
                 (next-file (image-get-next-file)))
            
            ;; Check if file already exists in trash
            (when (file-exists-p trash-file)
              (error "File already exists in .trash directory"))
            
            ;; Create .trash directory if needed
            (unless (file-exists-p trash-dir)
              (make-directory trash-dir))
            
            ;; Move the file
            (rename-file current-file trash-file)
            
            ;; Kill the current buffer
            (kill-buffer)
            
            ;; Display success message
            (message "Trashed %s into .trash" filename)
            
            ;; Visit next file or directory
            (if next-file
                (find-file next-file)
              (dired dir)
              (message "No more image files to visit")))
        
        (error
         (message "Error: %s" (error-message-string err))))))))

(defun untrash-image-file ()
  "Move the current image file from .trash subdirectory back to parent.
If successful, visits the next image file in .trash or the directory in dired mode."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (cond
     ((not current-file)
      (message "Buffer is not visiting a file"))
     
     ((not (string-match-p "/\\.trash/" current-file))
      (message "Not in trash directory"))
     
     (t
      (condition-case err
          (let* ((trash-dir (file-name-directory current-file))
                 (parent-dir (file-name-directory (directory-file-name trash-dir)))
                 (filename (file-name-nondirectory current-file))
                 (target-file (expand-file-name filename parent-dir))
                 (next-file (image-get-next-file)))
            
            ;; Check if file already exists in parent directory
            (when (file-exists-p target-file)
              (error "File already exists in parent directory"))
            
            ;; Move the file
            (rename-file current-file target-file)
            
            ;; Kill the current buffer
            (kill-buffer)
            
            ;; Display success message with relative paths
            (message "Untrashed %s back into %s" 
                     filename
                     (file-relative-name parent-dir default-directory))
            
            ;; Visit next file or directory
            (if next-file
                (find-file next-file)
              (dired trash-dir)
              (message "No more image files to visit")))
        
        (error
         (message "Error: %s" (error-message-string err))))))))

(defun my-images-setup-keybindings ()
  "Set up keybindings for trash/untrash in image-mode."
  (define-key image-mode-map "t" 'trash-image-file)
  (define-key image-mode-map "u" 'untrash-image-file))

(add-hook 'image-mode-hook 'my-images-setup-keybindings)


