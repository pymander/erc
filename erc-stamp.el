;;; erc-stamp.el --- Timestamping for ERC messages

;; Copyright (C) 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, processes, timestamp
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcStamp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The code contained in this module is responsible for inserting
;; timestamps into ERC buffers.  In order to actually activate this,
;; you must call `erc-timestamp-mode'.

;; You can choose between two different ways of inserting timestamps.
;; Customize `erc-insert-timestamp-function' and
;; `erc-insert-away-timestamp-function'.

;;; Code:

(require 'erc)
(require 'erc-compat)

(defconst erc-stamp-version "$Revision: 1.54 $"
  "ERC stamp mode revision.")

(defgroup erc-stamp nil
  "For long conversation on IRC it is sometimes quite
useful to have individual messages timestamp.  This
group provides settings related to the format and display
of timestamp information in `erc-mode' buffer.

For timestamping to be activated, you just need to load `erc-stamp'
in your .emacs file or interactively using `load-library'."
  :group 'erc)

(defcustom erc-timestamp-format "[%H:%M]"
  "*If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%T\" and \"%H:%M\".

If nil, timestamping is turned off."
  :group 'erc-stamp
  :type '(choice (const nil)
		 (string)))

(defcustom erc-insert-timestamp-function 'erc-insert-timestamp-right
  "*Function to use to insert timestamps.

It takes a single argument STRING which is the final string
which all text-properties already appended.  This function only cares about
inserting this string at the right position.  Narrowing is in effect
while it is called, so (point-min) and (point-max) determine the region to
operate on."
  :group 'erc-stamp
  :type '(choice (const :tag "Right" erc-insert-timestamp-right)
		 (const :tag "Left" erc-insert-timestamp-left)
		 function))

(defcustom erc-away-timestamp-format "<%H:%M>"
  "*Timestamp format used when marked as being away.

If nil, timestamping is turned off when away unless `erc-timestamp-format'
is set.

If `erc-timestamp-format' is set, this will not be used."
  :group 'erc-stamp
  :type '(choice (const nil)
		 (string)))

(defcustom erc-insert-away-timestamp-function 'erc-insert-timestamp-right
  "*Function to use to insert the away timestamp.

See `erc-insert-timestamp-function' for details."
  :group 'erc-stamp
  :type '(choice (const :tag "Right" erc-insert-timestamp-right)
		 (const :tag "Left" erc-insert-timestamp-left)
		 function))

(defcustom erc-hide-timestamps nil
  "*If non-nil, timestamps will be invisible.

This is useful for logging, because, although timestamps will be
hidden, they will still be present in the logs."
  :group 'erc-stamp
  :type 'boolean)

(defcustom erc-echo-timestamps nil
  "*If non-nil, print timestamp in the minibuffer when point is moved.
Using this variable, you can turn off normal timestamping,
and simply move point to an irc message to see its timestamp
printed in the minibuffer."
  :group 'erc-stamp
  :type 'boolean)

(defcustom erc-echo-timestamp-format "Timestamped %A, %H:%M:%S"
  "*Format string to be used when `erc-echo-timestamps' is non-nil.
This string specifies the format of the timestamp being echoed in
the minibuffer."
  :group 'erc-stamp
  :type 'string)

(defcustom erc-timestamp-intangible t
  "*Whether the timestamps should be intangible, i.e. prevent the point
from entering them and instead jump over them."
  :group 'erc-stamp
  :type 'boolean)

(defface erc-timestamp-face '((t (:bold t :foreground "green")))
  "ERC timestamp face."
  :group 'erc-faces)

;;;###autoload (autoload 'erc-timestamp-mode "erc-stamp" nil t)
(define-erc-module stamp timestamp
  "This mode timestamps messages in the channel buffers."
  ((add-hook 'erc-mode-hook 'erc-munge-invisibility-spec)
   (add-hook 'erc-insert-modify-hook 'erc-add-timestamp t)
   (add-hook 'erc-send-modify-hook 'erc-add-timestamp t))
  ((remove-hook 'erc-mode-hook 'erc-munge-invisibility-spec)
   (remove-hook 'erc-insert-modify-hook 'erc-add-timestamp)
   (remove-hook 'erc-send-modify-hook 'erc-add-timestamp)))

(defun erc-add-timestamp ()
  "Add timestamp and text-properties to message.

This function is meant to be called from `erc-insert-modify-hook'
or `erc-send-modify-hook'."
  (unless (get-text-property (point) 'invisible)
    (let ((ct (current-time)))
      (if (fboundp erc-insert-timestamp-function)
	  (funcall erc-insert-timestamp-function
		   (erc-format-timestamp ct erc-timestamp-format))
	(error "Timestamp function unbound"))
      (when (and (fboundp erc-insert-away-timestamp-function)
		 erc-away-timestamp-format
		 (with-current-buffer (erc-server-buffer) erc-away)
		 (not erc-timestamp-format))
	(funcall erc-insert-away-timestamp-function
		 (erc-format-timestamp ct erc-away-timestamp-format)))
      (add-text-properties (point-min) (point-max)
			   (list 'timestamp ct))
      (add-text-properties (point-min) (point-max)
			   (list 'point-entered 'erc-echo-timestamp)))))

(defvar erc-timestamp-last-inserted nil
  "Last timestamp inserted into the buffer.")
(make-variable-buffer-local 'erc-timestamp-last-inserted)

(defcustom erc-timestamp-only-if-changed-flag t
  "*Insert timestamp only if its value changed since last insertion.
If `erc-insert-timestamp-function' is `erc-insert-timestamp-left', a
string of spaces which is the same size as the timestamp is added to
the beginning of the line in its place. If you use
`erc-insert-timestamp-right', nothing gets inserted in place of the
timestamp."
  :group 'erc-stamp
  :type 'boolean)

(defcustom erc-timestamp-right-column nil
  "*If non-nil, the column at which the timestamp is inserted,
if the timestamp is to be printed to the right.  If nil,
`erc-insert-timestamp-right' will use other means to determine
the correct column."
  :group 'erc-stamp
  :type '(choice
	  (integer :tag "Column number")
	  (const :tag "Unspecified" nil)))

(defcustom erc-timestamp-right-align-by-pixel nil
  "*If non-nil, insert the right timestamp based on a pixel value.
This is needed when variable-width text precedes a timestamp.
Unfortunately, it only works in Emacs 22 and when using the X
Window System."
  :group 'erc-stamp
  :type 'boolean)

(defun erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
			(string-equal string erc-timestamp-last-inserted)))
	 (len (length string))
	 (s (if ignore-p (make-string len ? ) string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (erc-put-text-property 0 len 'field 'erc-timestamp s)
    (insert s)))

(defun erc-insert-aligned (string pos)
  "Insert STRING based on a fraction of the width of the buffer.
Fraction is roughly (/ POS (window-width)).

If `erc-timestamp-right-align-by-pixel' is nil, insert STRING at the
POSth column, without using pixel coordinates."
  (if (not erc-timestamp-right-align-by-pixel)
      (indent-to pos)
    (insert " ")
    (let ((offset (floor (* (/ (1- pos) (window-width) 1.0)
			    (nth 2 (window-inside-pixel-edges))))))
      (put-text-property (1- (point)) (point) 'display
			 `(space :align-to (,offset)))))
  (insert string))

(defun erc-insert-timestamp-right (string)
  "Insert timestamp on the right side of the screen.
STRING is the timestamp to insert.  The function is a possible value
for `erc-insert-timestamp-function'.

If `erc-timestamp-only-if-changed-flag' is nil, a timestamp is always
printed.  If this variable is non-nil, a timestamp is only printed if
it is different from the last.

If `erc-timestamp-right-column' is set, its value will be used as the
column at which the timestamp is to be printed.  If it is nil, and
`erc-fill-mode' is active, then the timestamp will be printed just
before `erc-fill-column'.  Otherwise, if the current buffer is
shown in a window, that window's width is used.  If the buffer is
not shown, and `fill-column' is set, then the timestamp will be
printed just `fill-column'.  As a last resort, the timestamp will
be printed just before the window-width."
  (unless (and erc-timestamp-only-if-changed-flag
	       (string-equal string erc-timestamp-last-inserted))
    (setq erc-timestamp-last-inserted string)
    (goto-char (point-max))
    (forward-char -1);; before the last newline
    (let* ((current-window (get-buffer-window (current-buffer)))
	   (pos (cond
		 (erc-timestamp-right-column
		  (+ erc-timestamp-right-column (length string)))
		 ((and (boundp 'erc-fill-mode)
		       erc-fill-mode
		       (boundp 'erc-fill-column))
		  (1+ erc-fill-column))
		 (current-window
		  (- (window-width current-window)
		     1))
		 (fill-column
		  (1+ fill-column))
		 (t
		  (- (window-width)
		     1))))
	   (from (point))
	   (col (current-column))
	   indent)
      ;; deal with variable-width characters
      (setq pos (- pos (string-width string))
	    ;; The following is a kludge that works with most
	    ;; international input.  It is now only used to calculate
	    ;; whether to move to the next line before inserting a
	    ;; stamp.
	    col (+ col (ceiling (/ (- col (- (point) (point-at-bol))) 1.6))))
      (if (< col pos)
	  (erc-insert-aligned string pos)
	(newline)
	(indent-to pos)
	(setq from (point))
	(insert string))
      (erc-put-text-property from (1+ (point)) 'field 'erc-timestamp)
      (erc-put-text-property from (1+ (point)) 'rear-nonsticky t)
      (when erc-timestamp-intangible
	(erc-put-text-property from (1+ (point)) 'intangible t)))))

;; for testing: (setq erc-timestamp-only-if-changed-flag nil)

(defun erc-format-timestamp (time format)
  "Return TIME formatted as string according to FORMAT.
Return the empty string if FORMAT is nil."
  (if format
      (let ((ts (format-time-string format time)))
	(erc-put-text-property 0 (length ts) 'face 'erc-timestamp-face ts)
	(erc-put-text-property 0 (length ts) 'invisible 'timestamp ts)
	(erc-put-text-property 0 (length ts)
			       'isearch-open-invisible 'timestamp ts)
	;; N.B. Later use categories instead of this harmless, but
	;; inelegant, hack. -- BPT
	(when erc-timestamp-intangible
	  (erc-put-text-property 0 (length ts) 'intangible t ts))
	ts)
    ""))

;; This function is used to munge `buffer-invisibility-spec to an
;; appropriate value. Currently, it only handles timestamps, thus its
;; location.  If you add other features which affect invisibility,
;; please modify this function and move it to a more appropriate
;; location.
(defun erc-munge-invisibility-spec ()
  (if erc-hide-timestamps
      (setq buffer-invisibility-spec
	    (if (listp buffer-invisibility-spec)
		(cons 'timestamp buffer-invisibility-spec)
	      (list 't 'timestamp)))
    (setq buffer-invisibility-spec
	  (if (listp buffer-invisibility-spec)
	      (remove 'timestamp buffer-invisibility-spec)
	    (list 't)))))

(defun erc-hide-timestamps ()
  "Hide timestamp information from display."
  (interactive)
  (setq erc-hide-timestamps t)
  (erc-munge-invisibility-spec))

(defun erc-show-timestamps ()
  "Show timestamp information on display.
This function only works if `erc-timestamp-format' was previously
set, and timestamping is already active."
  (interactive)
  (setq erc-hide-timestamps nil)
  (erc-munge-invisibility-spec))

(defun erc-echo-timestamp (before now)
  "Print timestamp text-property of an IRC message.
Argument BEFORE is where point was before it got moved and
NOW is position of point currently."
  (when erc-echo-timestamps
    (let ((stamp (get-text-property now 'timestamp)))
      (when stamp
	(message (format-time-string erc-echo-timestamp-format
				     stamp))))))

(provide 'erc-stamp)

;;; erc-stamp.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: 57aefab4-63e0-4c48-91d5-6efa145487e0
