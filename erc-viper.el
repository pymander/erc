;;; erc-viper.el --- Viper compatibility hacks for ERC

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: emulation

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Viper is a VI emulation mode for Emacs. ERC and Viper don't quite get
;; along by default; the code in this file fixes that. A simple
;;   (require 'erc-viper)
;; in your ~/.ercrc.el should be all it takes for you to use ERC and
;; Viper together happily.

;;; Code:

(require 'viper)

;; Fix RET in ERC buffers, by telling Viper to pass RET through to the
;; normal keymap.

(defvar viper-erc-modifier-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'viper-exec-key-in-emacs)
    map)
  "Ensure that RET will always do the right thing in ERC buffers.")

(add-to-list 'viper-major-mode-modifier-list
             '(erc-mode insert-state viper-erc-modifier-map))
(add-to-list 'viper-major-mode-modifier-list
             '(erc-mode vi-state viper-erc-modifier-map))

(viper-apply-major-mode-modifiers)

;; Ensure that ERC buffers come up in insert state.
(add-to-list 'viper-insert-state-mode-list 'erc-mode)

;; Fix various local variables in Viper.
(add-hook 'erc-mode-hook 'viper-comint-mode-hook)

(provide 'erc-viper)
;;; erc-viper.el ends here
