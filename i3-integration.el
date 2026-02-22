;;; i3-integration.el -- using i3 IPC to integrate Emacs with i3.  -*- lexical-binding: t; -*-

;; Copyright (c) 2012, Vadim Atlygin.
;;               2015, Jan Path
;; All rights reserved.

;; Author:  Vadim Atlygin <vadim.atlygin@gmail.com>
;; Version: 0.1
;; Package-Requires: (
;;     (emacs "24.3")
;;     (seq      "2.24"))

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies,
;; either expressed or implied, of the FreeBSD Project.

;;; Commentary:
;;
;; this is a set of advises that allows Emacs to play nicely and in
;; predictable fashion with i3. Some of them are quite subjective so
;; they are disabled by default.

;;; Code:

(require 'i3)
(require 'cl-lib)
;; For older Emacs releases we depend on an updated `seq' release from GNU
;; ELPA, for `seq-keep'.  Unfortunately something else may require `seq'
;; before `package' had a chance to put this version on the `load-path'.
(when (and (featurep 'seq)
           (not (fboundp 'seq-keep)))
  (unload-feature 'seq 'force))
(require 'seq)

(defcustom i3-collect-windows-function 'i3-collect-only-visible-windows
  "Function used to select windows when used in
one-window-per-frame mode. You can choose between
i3-collect-only-visible-windows which ignores windows hidden in
stacked or tabbed containers, or i3-collect-all-windows, which
will use all of them."
  :type 'function
  :group 'i3)

(defcustom i3-window-list-frame-visible-function 'i3-get-visible-windows-ids
  "Function to select the visible frame when listing windows.
Filters windows for use all functions that try to find visible windows."
  :type '(choice (function-item i3-collect-only-visible-windows)
                 (function-item i3-get-visible-workspace-window-ids)
                 (function :tag "Custom function"))
  :group 'i3)

(defun i3-one-window-per-frame-mode-on ()
  "Turns on one window per frame mode. After switching it on,
emacs will not split your frames, instead it will reuse them, in
more or less sensible manner. It will not reuse frames from
invisible workspaces either and will prefer to replace special
kind of buffers or least recently used ones. Works only in Emacs 24."
  (interactive)
  (i3-one-window-per-frame-mode t))

(defun i3-one-window-per-frame-mode-off ()
  "Turns off one window per frame mode. This is the default."
  (interactive)
  (i3-one-window-per-frame-mode nil))

;;; Internal functions

(defun i3-one-window-per-frame-mode (turn-on)
  (if turn-on
      (progn (advice-add 'select-frame :before #'i3-timestamp-frame-selection)
	     (advice-add 'pop-to-buffer-same-window :override #'i3-pop-to-buffer-same-window)
             (cl-pushnew 'i3-display-buffer-use-some-frame
                         (car display-buffer-overriding-action)))
    (advice-remove 'select-frame #'i3-timestamp-frame-selection)
    (advice-remove 'pop-to-buffer-same-window #'i3-pop-to-buffer-same-window)
    (cl-callf2 delq 'i3-display-buffer-use-some-frame
               (car display-buffer-overriding-action))))

;;; Advices
(defun i3-pop-to-buffer-same-window (buffer &optional norecord)
  (let ((display-buffer-overriding-action '(nil . nil)))
  (pop-to-buffer buffer display-buffer--same-window-action norecord)))

(defun i3-visible-frame-list-filter (frame-list)
  (i3-filter-visible-frame-list (i3-filter-other-display-frames frame-list)))

(defun i3-timestamp-frame-selection (frame &rest _)
  (set-frame-parameter frame 'i3-frame-selected-time (current-time)))

;;; i3 dependent pieces

(defun i3-filter-visible-frame-list (visible-frame-list)
  (condition-case nil
      (let ((visible-window-ids (i3-get-visible-windows-ids)))
        (seq-keep (lambda(f)
                             (when (member (string-to-number (frame-parameter f 'outer-window-id))
                                           visible-window-ids)
                               f))
                           visible-frame-list))
    (error visible-frame-list)))


;; To be less confusing these functions refer to i3 windows as frames
;; as they only handle i3 windows which are Emacs frames.

(defvar i3-filter--frames-visible nil
  "Internal variable to cache visible window-ids to avoid repeated calls.")

;;;###autoload
(defun i3-filter--frame-visible-set ()
  "Set list of frames considered visible.
Determined according to `i3-window-list-frame-visible-function'."
  (setq i3-filter--frames-visible (or (and i3-window-list-frame-visible-function
                                            (funcall i3-window-list-frame-visible-function))
                                       ;; Have a fallback, we don't want to break Emacs
                                       ;; when this is set wrong.
                                       (i3-get-visible-windows-ids))))

;;;###autoload
(defun i3-filter-frame-visible-p (frame)
  "Return t if FRAME is visible."
  (when-let* ((frame-outer-id (frame-parameter frame 'outer-window-id))
              (frame-outer-id (string-to-number frame-outer-id)))
    (if (memq frame-outer-id i3-filter--frames-visible) t)))

;;;###autoload
(defun i3-filter-window-visible-p (window)
  "Return non-nil if WINDOW is visible."
  (i3-filter-frame-visible-p (window-frame window)))

;;;###autoload
(defun i3-filter-window-list-1-filter-all-frames-visible (old-func &optional window minibuf all-frames)
  "Filter the visible WINDOW(S) returned by OLD-FUNC.

If ALL-FRAMES is either \='visible\=' or \='0\=' filter them.
Else just call the advised function regularly."
  (if (or (eq all-frames 'visible)
          (eq all-frames 0))
      (let* ((windows (funcall old-func window minibuf all-frames)))
        (seq-filter #'i3-filter-window-visible-p windows))
    (funcall old-func window minibuf all-frames)))

(defun i3-get-visible-workspace-names ()
  "Return any i3 workspace which is visible on any of the current screen(s)."
  (seq-keep (lambda(w) (when (i3-field-is 'visible #'eq t w)
                                  (i3-field 'name w)))
            (i3-get-workspaces)))

;;;###autoload
(defun i3-get-visible-windows ()
  "Return visible windows according to `i3-collect-windows-function'."
  (i3-filter--visible-frames i3-collect-windows-function))

;;;###autoload
(defun i3-filter--visible-frames (predicate)
  "Return visible window according to PREDICATE."
  (let ((visible-workspace-names (i3-get-visible-workspace-names)))
    (i3-flatten
    (mapcar predicate
              (i3-flatten (seq-keep (lambda(w)
                               (when (i3-field-is 'name #'member visible-workspace-names w)
                                 (append (i3-field 'nodes w) nil)));convert vector to list
                             (i3-collect-workspaces (i3-get-tree-layout))))))))

;;;###autoload
(defun i3-get-visible-windows-ids ()
  "Return window-id's of all currently visible windows."
  (mapcar (apply-partially #'i3-field 'window) (i3-get-visible-windows)))

;;;###autoload
(defun i3-get-visible-workspace-window-ids ()
  "Return window-id's of all windows on currently visible workspaces.
Also returns windows which are on top of other windows."
  (mapcar (apply-partially #'i3-field 'window) (i3-filter--visible-frames #'i3-collect-all-windows)))

(defun i3-collect-entities (checkp)
  (letrec ((collect (lambda (root)
                      (if (funcall checkp root)
                          (list root)
                        (i3-flatten (seq-keep collect
                                              (i3-field 'nodes root)))))))
    collect))

(defalias 'i3-collect-workspaces
  (i3-collect-entities (lambda (root)
                         (or (i3-field-is 'type #'equal 4 root)
                             (i3-field-is 'type #'equal "workspace" root)))))

(defalias 'i3-collect-all-windows
  (i3-collect-entities (apply-partially #'i3-field 'window)))

;;;###autoload
(defun i3-collect-only-visible-windows (root)
  "Return all windows on ROOT which are considered visible.

Visible means that they are on any of the layouts which are currently on top."
  (if (i3-field 'window root)
      (list root)
    (let* ((folded (i3-field-is 'layout #'member '("tabbed" "stacked") root))
           (id (when folded (elt (i3-field 'focus root) 0)))
           (children (if folded
                         (list (cl-find-if (apply-partially #'i3-field-is 'id #'eq id) (i3-field 'nodes root)))
                       (i3-field 'nodes root))))
      (i3-flatten (seq-keep #'i3-collect-only-visible-windows children)))))

;;; Helper functions

(defun i3-flatten (list-of-lists)
  (apply 'append list-of-lists))

; json objects get converted to alists
(defun i3-field (symbol alist)
  (cdr (assq symbol alist)))
(defun i3-field-is (symbol pred compare alist)
  (funcall pred (i3-field symbol alist) compare))

(defun i3-get-frame-buffer (frame)
  (car (frame-parameter frame 'buffer-list)))

(defun i3-get-frame-selected-time (frame)
  (float-time (frame-parameter frame 'i3-frame-selected-time)))

(defun i3-filter-all-but-special-buffer-frames (frames)
  (seq-keep (lambda (f) (when (not (buffer-file-name (i3-get-frame-buffer f)))
                                   f))
                     frames))

(defun i3-filter-frames-by-buffer (buffer frames)
  (seq-keep (lambda(f)
                       (when (memq buffer (frame-parameter f 'buffer-list))
                         f))
                     frames))

(defun i3-sort-frames-by-buffer (buffer frames)
  (sort frames
        (lambda(f1 f2) (< (cl-position buffer (frame-parameter f1 'buffer-list))
                          (cl-position buffer (frame-parameter f2 'buffer-list))))))

(defun i3-sort-frames-by-selected-time (frames)
  (sort frames (lambda (f1 f2) (< (i3-get-frame-selected-time f1) (i3-get-frame-selected-time f2)))))


(defun i3-get-frame-showing-buffer (buffer frames)
  (cl-find-if (lambda (f) (eq (car (frame-parameter f 'buffer-list)) buffer))
              frames))

(defun i3-get-frame-most-recently-displayed-buffer (buffer frames)
  (car (i3-sort-frames-by-buffer buffer (i3-filter-frames-by-buffer buffer frames))))

(defun i3-get-frame-least-recently-used (frames)
  (car (i3-sort-frames-by-selected-time frames)))


(defun i3-filter-other-display-frames (frames)
  (let ((selected-display (frame-parameter (selected-frame) 'display)))
    (seq-keep (lambda(f)
                         (when (and (not (frame-parameter f 'tty))
                                    (not (frame-parameter f 'i3-ignore-frame))
                                    (eq (frame-parameter f 'display) selected-display))
                           f))
                       frames)))

(defun i3-get-popup-frame-for-buffer (buffer)
  (let* ((frames (visible-frame-list))
         (frames-no-selected-frame (remove (selected-frame) frames))
         (special-frames (i3-filter-all-but-special-buffer-frames frames-no-selected-frame)))
    (or (i3-get-frame-showing-buffer buffer frames)
        (i3-get-frame-most-recently-displayed-buffer buffer special-frames)
        (i3-get-frame-least-recently-used special-frames)
        (i3-get-frame-most-recently-displayed-buffer buffer frames-no-selected-frame)
        (i3-get-frame-least-recently-used frames-no-selected-frame)
        (car frames))))

(defun i3-get-window-for-frame (frame)
  (let ((selected-window (frame-selected-window frame)))
    (if (window-minibuffer-p selected-window)
        (next-window selected-window)
      selected-window)))

(defun i3-display-buffer-use-some-frame (buffer alist)
  (ignore alist)
  (when (and (display-graphic-p)
             (not (or (member (buffer-name buffer)
                              ;; FIXME: Should have defcustom
                              '("*Completions*" " *undo-tree*"))
                      (string-match-p "\\`[*][Hh]elm.*[*]\\'"
                                      (buffer-name buffer)))))
    (let* ((frame (i3-get-popup-frame-for-buffer buffer))
           (window (i3-get-window-for-frame frame)))
      (window--display-buffer buffer window 'reuse))))

;;; Set defaults
;;;###autoload
(define-minor-mode i3-integration-mode
  "Global minor mode to make Emacs aware of i3's window visibility state.
Ensures `visible-frame-list' will only return visible windows.
Same for `display-buffer'."
  :global t :group 'i3
  :init-value t
  :initialize
  (lambda (symbol exp)
    (custom-initialize-default symbol exp)
    (when i3-integration-mode
      (advice-add #'visible-frame-list :filter-return #'i3-visible-frame-list-filter)
      (advice-add #'window-list-1 :around #'i3-filter-window-list-1-filter-all-frames-visible)
      (add-hook 'window-configuration-change-hook #'i3-filter--frame-visible-set)))
  (let ((enable (if (eq arg 'toggle)
                    (not i3-integration-mode)
                  (> (prefix-numeric-value arg) 0))))
    (if enable
        (progn
          (advice-add #'visible-frame-list :filter-return #'i3-visible-frame-list-filter)
          (advice-add #'window-list-1 :around #'i3-filter-window-list-1-filter-all-frames-visible)
          (add-hook 'window-configuration-change-hook #'i3-filter--frame-visible-set))
      (progn (advice-remove #'visible-frame-list #'i3-visible-frame-list-filter)
             (advice-remove #'window-list-1 #'i3-filter-window-list-1-filter-all-frames-visible)
             (remove-hook 'window-configuration-change-hook #'i3-filter--frame-visible-set)))))

(provide 'i3-integration)
