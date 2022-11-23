(require 'cl)
(lambda (data)
  (labels
      ((drop-parent (data)
                    (cond ((listp data) (org-plist-delete data :parent))
                          (t data)))
       (extract (data)
                (cond
                 ((listp data)
                  (setq data (drop-parent data))
                  (mapcar (lambda (data) (extract data)) data))
                 ((stringp data)
                  (remove-text-properties 0 (length data) '(:parent) data)
                  data)
                 (t data))))
    (extract data)))

(progn
  (let ((elements nil))
    (dolist (buffer (org-buffer-list))
      (with-current-buffer buffer
        (push (cons (buffer-name) (org-element-parse-buffer)) elements)
        )
      )
    elements))

(progn
  (let ((elements nil))
    (dolist (buffer (org-buffer-list))
      (with-current-buffer buffer
        (let ((visit-time (time-convert (visited-file-modtime) 'integer))
              (mod-time (buffer-modified-tick)))
          (push (list (buffer-file-name) visit-time mod-time) elements))
        )
      )
    elements))

 
