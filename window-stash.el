;;; window-stash.el --- Quickly Stash Windows -*- lexical-binding: t -*-

; Copyright © 2020 lawrsp <lawrance.rsp@gmail.com>

;; Author: lawrsp <lawrance.rsp@gmail.com>
;; URL: https://github.com/lawrsp/eamcs-window-stash
;; Keywords: window, stash, convenience
;; Version: 0.1.3
;; Package-Requires: ((emacs "26.1") (avy "0.5.0") (ace-window "0.1.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library provides the window-stash functionality 

;;; Code:
(require 'avy)
(require 'ace-window)

(defvar window-stash-lay-side 'right
  ;; which side of the stashed windows layed
  )

(defvar window-stash-grow-direction 'above
  ;; which direction of the window stash grows
  )

(defvar window-stash-window-width 40
  ;; if <= 0 will balance the window
  ;; if integer will have columns
  ;; if float  will have percent of main window
  )

(defvar window-stash-window-height 0
 ;; if <= 0 will balance the window
  ;; if integer will have lines
  ;; if float  will have percent of main window
  )


(defun window-stash--display-buffer-at-direction (buffer direction window width height)
  ;; display current buffer in DIRECTION of WINDOW by WIDTH HEIGHT
  (display-buffer buffer 
                  `(display-buffer-in-direction . ((direction . ,direction)
                                                   (window . ,window)
                                                   (slot . 0)
                                                   (dedicated . t)
                                                   (window-width . ,width)
                                                   (window-height . ,height)
                                                   (window-parameters . ((no-delete-other-windows . t)))))))

(defun window-stash--get-window-width ()
  ;; get window windth
  (if (<= window-stash-window-width 0)
       `balance-windows
    window-stash-window-width))

(defun window-stash--get-window-height ()
  ;; get window height
  (if (<= window-stash-window-height 0)
      `balance-windows
    window-stash-window-height))

(defun window-stash--next-stash-direction (current initial)
  "get the next stash postion `window-stash-lay-side or `window-stash-grow-direction"
  (if (or (eq current initial)
          (not (window-dedicated-p current)))
      window-stash-lay-side
    window-stash-grow-direction))

(defun window-stash--find-window-direction-most (initial direction)
  "find the window at most of DIRECTION from an INITIAL window"
  (let ((w initial)
        win)
    (while w
      (setq win w)
      (setq w (window-in-direction direction win)))
    win))

(defun window-stash--next-stash-window-refer (initial)
  "get the next fix window reference"
  (let ((win initial))
    ;; find lay side most
    (setq win (window-stash--find-window-direction-most win window-stash-lay-side))
    (when (not (eq win initial))
      ;; find group direction most
      (setq win (window-stash--find-window-direction-most win window-stash-grow-direction))) 
    win))

(defun window-stash-stash ()
  "display current buffer to a dedicated window stashed"
  (interactive)
  ;; (with-selected-window (selected-window)
  (let* ((buffer (current-buffer))
         (initial (selected-window))
         (refer (window-stash--next-stash-window-refer initial))
         (direction (window-stash--next-stash-direction refer initial))
         (width (window-stash--get-window-width))
         (height (window-stash--get-window-height))
         (window-combination-limit nil)
         (window-combination-resize t))
    (window-stash--display-buffer-at-direction buffer direction refer width height)))


(defun window-stash--window-list (initial)
  ;; get all the stashed window
  (let ((win (window-stash--next-stash-window-refer initial))
        list)
    (when (not (eq win initial) )
      (while win
        (when (window-dedicated-p win)
          (push win list))
        (setq win  (window-in-direction 'below win))))
    list))

(defun window-stash--pop-it (selected window)
  ;; if SELECTED is not equal to WINDOW,
  ;; set SELECTED content to WINDOW's and delete WINDOW
  (when (not (equal selected window))
    (set-window-buffer selected (window-buffer window))
    (delete-window window)))

(defun window-stash--aw-select (start-window mode-line &optional action)
  "Return a selected other window.
Amend MODE-LINE to the mode line for the duration of the selection.
Make a small modification of the orgith aw-select function of ace-widnow"
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

(defun window-stash-pop ()
  "pop a buffer to current" 
  (interactive)
  (let ((selected (selected-window)))
    (window-stash--aw-select
     selected
     " Ace - Pop Stashed Window"
     (lambda (window)
       (with-selected-window window
         (window-stash--pop-it selected window))))))

(provide 'window-stash)


;;; window-stash.el ends here
