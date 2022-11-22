((todo.org org-data nil
  (headline
   (:raw-value
    "+Convert+ epic /todo list/ ~to~ *pretty* _printed_ =form= for random_{things} or other^{things} and =save_epic= and \\Gamma is not awesome"
    :begin 1 :end 285 :pre-blank 0 :contents-begin 146 :contents-end 285
    :level 1 :priority nil :tags nil :todo-keyword #
    (TODO 0 4 (fontified t face (org-todo org-level-1))) :todo-type todo
    :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil
    :post-affiliated 1 :title
    ((strike-through
      (:begin 8 :end 18 :contents-begin 9 :contents-end 16 :post-blank 1
       :parent #2)
      # (Convert 0 7 (:parent #5)))
     # ("epic " 0 5 (:parent #2))
     (italic
      (:begin 23 :end 35 :contents-begin 24 :contents-end 33 :post-blank 1
       :parent #2)
      # ("todo list" 0 9 (:parent #5)))
     (code (:value to :begin 35 :end 40 :post-blank 1 :parent #2))
     (bold
      (:begin 40 :end 49 :contents-begin 41 :contents-end 47 :post-blank 1
       :parent #2)
      # (pretty 0 6 (:parent #5)))
     (underline
      (:begin 49 :end 59 :contents-begin 50 :contents-end 57 :post-blank 1
       :parent #2)
      # (printed 0 7 (:parent #5)))
     (verbatim (:value form :begin 59 :end 66 :post-blank 1 :parent #2)) #
     ("for random" 0 10 (:parent #2))
     (subscript
      (:begin 76 :end 86 :use-brackets-p 78 :contents-begin 78 :contents-end
       84 :post-blank 1 :parent #2)
      # (things 0 6 (:parent #5)))
     # ("or other" 0 8 (:parent #2))
     (superscript
      (:begin 94 :end 104 :use-brackets-p 96 :contents-begin 96 :contents-end
       102 :post-blank 1 :parent #2)
      # (things 0 6 (:parent #5)))
     # ("and " 0 4 (:parent #2))
     (verbatim
      (:value save_epic :begin 108 :end 120 :post-blank 1 :parent #2))
     # ("and " 0 4 (:parent #2))
     (entity
      (:name Gamma :latex "\\Gamma" :latex-math-p t :html "Γ" :ascii
       Gamma :latin1 Gamma :utf-8 "\206\147" :begin 124 :end 131
       :use-brackets-p nil :post-blank 1 :parent #2))
     # ("is not awesome" 0 14 (:parent #2)))
    :parent nil)
   (headline
    (:raw-value "Subtask number 1" :begin 146 :end 171 :pre-blank 0
     :contents-begin nil :contents-end nil :level 2 :priority nil :tags nil
     :todo-keyword #
     (TODO 0 4 (fontified t org-todo-head TODO face (org-todo org-level-2)))
     :todo-type todo :post-blank 0 :footnote-section-p nil :archivedp nil
     :commentedp nil :post-affiliated 146 :title
     (# ("Subtask number 1" 0 16 (:parent #3))) :parent #2))
   (headline
    (:raw-value "Task 2" :begin 171 :end 232 :pre-blank 0 :contents-begin 186
     :contents-end 232 :level 2 :priority nil :tags nil :todo-keyword #
     (TODO 0 4 (fontified t org-todo-head TODO face (org-todo org-level-2)))
     :todo-type todo :post-blank 0 :footnote-section-p nil :archivedp nil
     :commentedp nil :post-affiliated 171 :title
     (# ("Task 2" 0 6 (:parent #3))) :parent #2)
    (section
     (:begin 186 :end 232 :contents-begin 186 :contents-end 232 :post-blank 0
      :post-affiliated 186 :parent #3)
     (drawer
      (:begin 186 :end 232 :drawer-name LOGBOOK :contents-begin 196
       :contents-end 226 :post-blank 0 :post-affiliated 186 :parent #4)
      (clock
       (:status running :value
        (timestamp
         (:type inactive :raw-value "[2022-11-22 Tue 07:10]" :year-start 2022
          :month-start 11 :day-start 22 :hour-start 7 :minute-start 10
          :year-end 2022 :month-end 11 :day-end 22 :hour-end 7 :minute-end 10
          :begin 203 :end 225 :post-blank 0))
        :duration nil :begin 196 :end 226 :post-blank 0 :post-affiliated 196
        :parent #5)))))
   (headline
    (:raw-value "Task 3 src_sh[:exports code]{echo -e \"test\"}" :begin 232
     :end 285 :pre-blank 0 :contents-begin nil :contents-end nil :level 2
     :priority nil :tags nil :todo-keyword #
     (TODO 0 4 (fontified t org-todo-head TODO face (org-todo org-level-2)))
     :todo-type todo :post-blank 0 :footnote-section-p nil :archivedp nil
     :commentedp nil :post-affiliated 232 :title
     (# ("Task 3 " 0 7 (:parent #3))
      (inline-src-block
       (:language sh :value "echo -e \"test\"" :parameters ":exports code"
        :begin 247 :end 284 :post-blank 0 :parent #3)))
     :parent #2)))
  (headline
   (:raw-value "High priority task" :begin 285 :end 316 :pre-blank 0
    :contents-begin nil :contents-end nil :level 1 :priority 65 :tags nil
    :todo-keyword #
    (TODO 0 4 (fontified t org-todo-head TODO face (org-todo org-level-1)))
    :todo-type todo :post-blank 0 :footnote-section-p nil :archivedp nil
    :commentedp nil :post-affiliated 285 :title
    (# ("High priority task" 0 18 (:parent #2))) :parent nil))
  (headline
   (:raw-value "Low priority task" :begin 316 :end 346 :pre-blank 0
    :contents-begin nil :contents-end nil :level 1 :priority 67 :tags nil
    :todo-keyword #
    (TODO 0 4 (fontified t org-todo-head TODO face (org-todo org-level-1)))
    :todo-type todo :post-blank 0 :footnote-section-p nil :archivedp nil
    :commentedp nil :post-affiliated 316 :title
    (# ("Low priority task" 0 17 (:parent #2))) :parent nil))
  (headline
   (:raw-value "Non-todo element" :begin 346 :end 365 :pre-blank 0
    :contents-begin nil :contents-end nil :level 1 :priority nil :tags nil
    :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil
    :archivedp nil :commentedp nil :post-affiliated 346 :title
    (# ("Non-todo element" 0 16 (:parent #2))) :parent nil))
  (headline
   (:raw-value "Element with tags" :begin 365 :end 443 :pre-blank 0
    :contents-begin nil :contents-end nil :level 1 :priority nil :tags
    (#
     (tag1 0 4
      (keymap
       (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse)
        (mouse-2 . org-open-at-mouse))
       mouse-face highlight face (org-tag org-level-1) fontified t))
     #
     (tag2 0 4
      (keymap
       (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse)
        (mouse-2 . org-open-at-mouse))
       mouse-face highlight face (org-tag org-level-1) fontified t))
     #
     (tag3 0 4
      (keymap
       (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse)
        (mouse-2 . org-open-at-mouse))
       mouse-face highlight face (org-tag org-level-1) fontified t)))
    :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil
    :archivedp nil :commentedp nil :post-affiliated 365 :title
    (# ("Element with tags" 0 17 (:parent #2))) :parent nil))
  (headline
   (:raw-value "Element with a single tag" :begin 443 :end 526 :pre-blank 0
    :contents-begin nil :contents-end nil :level 1 :priority nil :tags
    (#
     (tagsingle 0 9
      (keymap
       (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse)
        (mouse-2 . org-open-at-mouse))
       mouse-face highlight face (org-tag org-level-1) fontified t)))
    :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil
    :archivedp nil :commentedp nil :post-affiliated 443 :title
    (# ("Element with a single tag" 0 25 (:parent #2))) :parent nil))
  (headline
   (:raw-value "Completed element" :begin 526 :end 582 :pre-blank 0
    :contents-begin 551 :contents-end 582 :level 1 :priority nil :tags nil
    :todo-keyword # (DONE 0 4 (fontified t face (org-done org-level-1)))
    :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil
    :commentedp nil :post-affiliated 526 :closed
    (timestamp
     (:type inactive :raw-value "[2022-11-22 Tue 06:58]" :year-start 2022
      :month-start 11 :day-start 22 :hour-start 6 :minute-start 58 :year-end
      2022 :month-end 11 :day-end 22 :hour-end 6 :minute-end 58 :begin 559
      :end 581 :post-blank 0))
    :title (# ("Completed element" 0 17 (:parent #2))) :parent nil)
   (section
    (:begin 551 :end 582 :contents-begin 551 :contents-end 582 :post-blank 0
     :post-affiliated 551 :parent #2)
    (planning
     (:closed
      (timestamp
       (:type inactive :raw-value "[2022-11-22 Tue 06:58]" :year-start 2022
        :month-start 11 :day-start 22 :hour-start 6 :minute-start 58
        :year-end 2022 :month-end 11 :day-end 22 :hour-end 6 :minute-end 58
        :begin 559 :end 581 :post-blank 0))
      :deadline nil :scheduled nil :begin 551 :end 582 :post-blank 0
      :post-affiliated 551 :parent #3))))))
