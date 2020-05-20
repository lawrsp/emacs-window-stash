;;; window-stash.el --- Stash Windows

(defun window-stash--display-buffer-at-direction (buffer direction window width height)
  ;; display current buffer in DIRECTION of WINDOW by WIDTH HEIGHT
  (display-buffer buffer 
                  `(display-buffer-in-direction . ((direction . ,direction)
                                                   (window . ,window)
                                                   (slot . 0)
                                                   (dedicated . t)
                                                   (window-width . ,width)
                                                   (window-height . ,height)
                                                   (window-parameters . ((no-delete-other-windows . t)))
                                                   ))))

(defun window-stash--next-stash-direction (current initial)
  "get the next stash postion `right or `below'"
  (if (eq current initial)
      'right
    'above))

(defun window-stash--find-window-direction-most (initial direction)
  "find the window at most of DIRECTION from an INITIAL window"
  (let ((win initial))
    ;; find direction most
    (while (-when-let (w (window-in-direction direction win))
             (setq win w)))
    win))

(defun window-stash--next-stash-window-refer (initial)
  "get the next right fix window reference. right most and then below most"
  (let ((win initial))
    ;; find right most
    (setq win (window-stash--find-window-direction-most win 'right))
    (when (not (eq win initial))
      ;; find below most
      (setq win (window-stash--find-window-direction-most win 'above))) 
    win))

(defun window-stash-stash ()
  "display current buffer to a dedicated window stashed at right of of main window"
  (interactive)
  ;; (with-selected-window (selected-window)
  (let* ((buffer (current-buffer))
         (initial (selected-window))
         (refer (window-stash--next-stash-window-refer initial))
         (direction (window-stash--next-stash-direction refer initial))
         (window-combination-limit nil)
         (window-combination-resize t))
    (window-stash--display-buffer-at-direction buffer direction refer 40 `balance-windows)))


(defun window-stash--window-list (initial)
  ;; get all the stashed window
  (let ((win (window-stash--next-stash-window-refer initial))
        list)
    (when (not (eq win initial) )
      (while win
        (when (window-dedicated-p win)
          (add-to-list 'list win))
        (setq win  (window-in-direction 'below win))
        ))
    list))

(defun window-stash--pop-it (selected window)
  ;; if SELECTED is not equal to WINDOW,
  ;; set SELECTED content to WINDOW's and delete WINDOW
  (when (not (equal selected window))
    (set-window-buffer selected (window-buffer window))
    (delete-window window)))

(defun window-stash--aw-select (start-window mode-line &optional action)
  "Return a selected other window.
Amend MODE-LINE to the mode line for the duration of the selection."
  (require 'ace-window)
  (let ((aw-action action)
        (wnd-list (window-stash--window-list start-window))
        window)
    (setq window
          (cond ((<= (length wnd-list) 1)
                 (when aw-dispatch-always
                   (setq aw-action
                         (unwind-protect
                             (catch 'done
                               (funcall aw-dispatch-function (read-char)))
                           (aw--done)))
                   (when (eq aw-action 'exit)
                     (setq aw-action nil)))
                 (or (car wnd-list) start-window))
                (t
                 (let ((candidate-list
                        (mapcar (lambda (wnd)
                                  (cons (aw-offset wnd) wnd))
                                wnd-list)))
                   (aw--make-backgrounds wnd-list)
                   (aw-set-mode-line mode-line)
                   ;; turn off helm transient map
                   (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
                   (unwind-protect
                       (let* ((avy-handler-function aw-dispatch-function)
                              (avy-translate-char-function aw-translate-char-function)
                              (transient-mark-mode nil)
                              (res (avy-read (avy-tree candidate-list aw-keys)
                                             (if (and ace-window-display-mode
                                                      (null aw-display-mode-overlay))
                                                 (lambda (_path _leaf))
                                               #'aw--lead-overlay)
                                             #'avy--remove-leading-chars)))
                         (if (eq res 'exit)
                             (setq aw-action nil)
                           (or (cdr res)
                               start-window)))
                     (aw--done))))))
    (if aw-action
        (funcall aw-action window)
      window)))

(defun window-stash-stash-pop ()
  "pop a buffer to current" 
  (interactive)
  (let ((selected (selected-window)))
    (window-stash--aw-select
     selected
     " Ace - Pop Stashed Window"
     (lambda (window)
       (with-selected-window window
         (window-stash--pop-it selected window)))
     )))

(provide 'window-stash)


;;; window-stash.el ends here
