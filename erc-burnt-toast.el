;;; erc-burnt-toast.el --- erc-match support for w32 notification center -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; URL: https://bru.st
;; Version: 0.3-pre
;; Package-Requires: ((emacs "26.0"))
;; Keywords: irc erc burnt-toast

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Provide Windows Notification Center to erc with burnt-toast and erc-match
;;
;; NOTICE: This is alpha software.  It working for me but not robustly tested.
;;         outside my actual usecase which is ERC + ercn.  Patches welcome.
;;
;; Basic setup:
;;   (eval-after-load 'erc-match
;;     (progn (require 'erc-burnt-toast)
;;   	   (erc-burnt-toast-mode 1)))
;;
;; Using `ercn':
;;   (eval-after-load 'ercn
;;     (progn
;;       (require 'erc-burnt-toast)
;;       (add-hook 'ercn-notify-hook
;; 	        (lambda (nickname message)
;; 	  	(erc-burnt-toast-command "ERC@ema.cs" nickname message)))))
;;
;; 0.2 - clean-input: remove text properties using `org-no-properties'
;;
;;;; Installation
;;
;;;;; Manual
;;
;; If you wish, install this optional package:
;; + ercn
;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'erc-burnt-toast)
;;
;;;;; MELPA
;; TODO If you installed from MELPA, you're done.  Feel free to open
;; an issue issue if you are using this and would like to see it on
;; MELPA.  I will create a release branch and submit MELPA PR when:
;;    (< 1 users)
;;    (> 1 issues)
;; Welcome to erc-burnt-toast!
;;
;; This program provides support for the Window 10 Notification center.
;;
;; You can download GNU Emacs for Windows 10 from GNU's website:
;;   https://www.gnu.org/software/emacs/
;; Or try-prelease binaries from here:
;;   https://alpha.gnu.org/gnu/emacs/pretest/windows/
;;
;; This program relies on the Burnt-Toast module for Windows
;; powershell which must be installed and working prior to use.
;;   https://github.com/Windos/BurntToast
;;
;; In addition to ERC support, this provides an interactive command to
;; create sholder-tap notifications and Notification Center toast in
;; Windows 10.
;;
;;   `erc-burnt-toast-command'
;;
;;  person: an email address.  If this does not matche a contact
;;          pinned to the task-bar you will only get toast
;;          without the "sholder tap" animation.
;;  title:  first line of the toast, shown in bold
;;  description: rest of the toast text
;;
;;;; Tips
;; + You can customize settings in the `erc-burnt-toast' group.
;; + Customize `erc-burnt-toast-image' to controls the animation
;;
;;;; Credits
;; In addition to Joshua King's Burnt-Toast module for PowerShell[1],
;; this program would not have been possible without the following
;; packages: emacs-package-dev-handbook[2], which showed the form
;; and Alex Murray's erc-desktop-notifications[3] from which I lifted
;; the vast majority of the implementation.  Thanks also to "wasamma"
;; from Freenode#ERC for the inspiration and, as always to Xah Lee for
;; his helpful website[4]
;;
;;  [1] https://github.com/Windos/BurntToast
;;  [2] https://github.com/alphapapa/emacs-package-dev-handbook#template
;;  [3] https://github.com/emacs-mirror/emacs/blob/master/lisp/erc/erc-desktop-notifications.el
;;  [4] http://xahlee.info/comp/unicode_index.html
;;
;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements
(eval-when-compile
  (require 'erc)
  (require 'erc-match)
  (require 'org-macs))

;;;; Customization

(defgroup erc-burnt-toast nil
  "Settings for `erc-burnt-toast'."
  :group 'erc
  :link '(url-link "http://example.com/erc-burnt-toast.el"))

(define-widget 'erc-burnt-toast-sound-options 'lazy
  "Sound options are based on those available from Notification Center."
  :tag "Sound"
  :type '(choice
	  (const :tag "Slient" nil)
	  (const "IM" )
	  (const "Alarm2")
	  (const "Default")
	  (const "IM")
	  (const "Mail")
	  (const "Reminder")
	  (const "SMS")
	  (const "Alarm")
	  (const "Alarm2")
	  (const "Alarm3")
	  (const "Alarm4")
	  (const "Alarm5")
	  (const "Alarm6")
	  (const "Alarm7")
	  (const "Alarm8")
	  (const "Alarm9")
	  (const "Alarm10")
	  (const "Call")
	  (const "Call2")
	  (const "Call3")
	  (const "Call4")
	  (const "Call5")
	  (const "Call6")
	  (const "Call7")
	  (const "Call8")
	  (const "Call9")
	  (const "Call10")))

(define-widget 'erc-burnt-toast-image-options 'lazy
  "TODO Sound options are based on those available from Notification Center."
  :tag "Image"
  :type '(choice (string :tag "URL" ) ;; TODO: :validate
		 (file   :tag "File"
			 :must-match t)))


;; (define-inline erc-burnt-toast--strip-string-props (string)
;;   "Remote text properties and return STRING."
;;   (when (and string (stringp string))
;;     (set-text-properties 0 (length string) nil string)
;;     string))

(defcustom erc-burnt-toast-person
  "ERC@ema.cs"
  "Microsoft People Contact Identifier, shape of: Email Address.
This `address` must appear People and must be pinned to the taskbar
in order for the pop-up image to be shown for Sholdertaps."
  :type 'string)
;;(setq erc-burnt-toast-person "ERC@ema.cs")

(defcustom erc-burnt-toast-image "https://i.imgur.com/WKiNp5o.gif"
  "Controls the pop-up image.
The image is shown only for ShoulderTap notifications and onlyl if the
`ERC@ema.cs' (or whatever you've customized `erc-burnt-toast-person' to
be) contact must be added to the task-bar favorited for Windows
Notification Center."
  :type 'erc-burnt-toast-image-options)

(defcustom erc-burnt-toast-logo
  "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcTqIcX9SEkNI-53SDYe5HLlfT2zTl2RhUCayMop97C1BqGkDo9J"
  "This setting controls the logo shown in Windows Notification Center."
  :type 'erc-burnt-toast-image-options)
;;(setq erc-burnt-toast-logo "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcTqIcX9SEkNI-53SDYe5HLlfT2zTl2RhUCayMop97C1BqGkDo9J")

(defcustom erc-burnt-toast-PRIVMSG-sound
  "IM"
  "Controls the sound played by Notification Center when a new PM arrives."
  :type 'erc-burnt-toast-sound-options)

(defcustom erc-burnt-toast-text-match-sound
  "Default"
  "Controls the sound played by Notification Center with match notifications."
  :type 'erc-burnt-toast-sound-options)

(defcustom erc-burnt-toast-max-body-text-size 10
  "Messages longer than this will be trunacted in Windows Notification Center."
  :type 'number)

(defcustom erc-burnt-toast-truncation-indicator "..."
  "Postfix indicating the message sent to Windows Notification Center was truncated."
  :type 'string)


;;;; Variables

(defvar erc-burnt-toast-buffer
    "*Messages*"
    "Buffer to receive powershell output.")

(defvar erc-burnt-toast-format
  "powershell.exe -Command \"$Logo = '%s'; $Image = '%s'; $Person = '%s'; $Text = '%s','%s'; New-BurntToastShoulderTap -AppLogo $Logo -Image $Image -Person $Person -Text $Text\""
  "This is the format string for the powershell command.")

(defvar erc-burnt-toast-query-on-unjoined-chan-privmsg
  nil
  "TODO Whether notifications are triggered by query from unjoined channels.")

;;;;; Keymaps

;;;; Functions
(defun erc-burnt-toast--clean-input (string)
  "Return a version of STRING sanitized for use as input to PowerShell.
<> are replaced with [], new-line is removed, and single-quotes are doubled."
  (org-no-properties
   (replace-regexp-in-string
    "\s+$" ""
    (replace-regexp-in-string
     "<" "["
     (replace-regexp-in-string
      ">" "]"
      (replace-regexp-in-string
       "[\t\n\r]+" ""
       (replace-regexp-in-string
	"'" "''"
	string)))))))
;;(message "fixed:%s" (erc-burnt-toast--clean-input "this i:sn't \na test\n"))

(defun erc-burnt-toast--clean-msg (long-message nick)
    (let*
	((short-message
	 ;;(replace-regexp-in-string "[\r\n]+" " " long-message))
	 (replace-regexp-in-string
	  (concat "\s?[@]?" (downcase nick) "\s?[:,]\s?") "" ;; nick is removed
	  (replace-regexp-in-string
	   (concat "^\\(\s?"
		   "[\\[<]?[0-9]\\{1,2\\}:[0-9]\\{2\\}[]>]?"
		   "\s?\\)\\|\\(\s?"
		   "[\\[<]?[0-9]\\{1,2\\}:[0-9]\\{2\\}[]>]?"
		   "\s?\\)$")  ""  ;; remove pre and post timestamp
		   (replace-regexp-in-string
		    "[\s\t]+" " " ;; colapse whitespace
		    (replace-regexp-in-string
		     "[\r\n]+" " " ;; replace line-breaks with space
		     long-message))))))
      (if (< erc-burnt-toast-max-body-text-size (length short-message))
	  (concat (substring short-message
			     0
			     (- erc-burnt-toast-max-body-text-size
				(length erc-burnt-toast-truncation-indicator)))
		  erc-burnt-toast-truncation-indicator)
	short-message)))
;;(setq erc-burnt-toast-max-body-text-size 17)
;;(setq erc-burnt-toast-truncation-indicator "...")
;;(erc-burnt-toast--clean-msg "[12:12> corwin:\n\n yore\tmess  age \n\n <2:12]"  "corwin")

(defun erc-burnt-toast-format-command (logo image person title description &optional no-clean)
  "Prepare powershell command line invoking Burnt-Toast.

LOGO IMAGE, PERSON, TITLE, and DESCRIPTION and NO-CLEAN."
  (if no-clean
      (format erc-burnt-toast-format logo image person title description)
    (format erc-burnt-toast-format
	    logo image
	    (erc-burnt-toast--clean-input person)
	    (erc-burnt-toast--clean-input title)
	    (erc-burnt-toast--clean-input description)
	    ;;"hi" "mom"
	    )))
;;(message "<[CMD:[%s]]>" (erc-burnt-toast-format-command "L" "I" "<p>" "t\nt" "d'd" t))
;;(message "<[CMD:[%s]]>" (erc-burnt-toast-format-command "L" "I" "<p>" "t\nt" "d'd"))

;;;;; Commands
;;;###autoload
(defun erc-burnt-toast-command (person title description)
  "Pop toast in the Windows Notification Center given PERSON, TITLE, AND DESCRIPTION."
  (interactive "sFrom:\nsSubject:\nsBody:")
  (let* ((command-text (erc-burnt-toast-format-command erc-burnt-toast-logo
						       erc-burnt-toast-image
						       person
						       title
						       description)))
    ;; (message "(%s) %s => %s " (equal command-text cleaned) command-text cleaned)
    ;; (message "Running command> %s" command-text)
    (start-process-shell-command "make-toast" erc-burnt-toast-buffer
				 command-text)))
;; (erc-burnt-toast-command "ERC@ema.cs" "teste" "still a test")

(defun erc-burnt-toast-PRIVMSG (_proc parsed)
  "Given PROC and PARSED, issue a Windows Notification Center alert.
PROC and PARSED represent a ERC process and a parsed private message."
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	 (target (car (erc-response.command-args parsed)))
	 (msg (erc-response.contents parsed))
	 ;; (query  (if (not erc-burnt-toast-query-on-unjoined-chan-privmsg)
	 ;; 	     nick
	 ;; 	   (if (erc-current-nick-p target)
	 ;; 	       nick
	 ;; 	     target)))
	 )
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (erc-burnt-toast-command erc-burnt-toast-person nick msg)))
  ;; Always return nil. This allows other handlers -if any- to act.
  nil)

;; erc-text-matched-hook documentation:
;; Hook run when text matches a given match-type.
;; Functions in this hook are passed as arguments:
;; (match-type nick!user@host message) where MATCH-TYPE is a symbol of:
;; current-nick, keyword, pal, dangerous-host, fool
(defun erc-burnt-toast-notify-on-match (match-type nickuserhost msg)
  "Conditionally send a notification given MATCH-TYPE NICKUSERHOST MSG."
  (message "type:%s nic:%s msg:%s" match-type nickuserhost msg)
  (let ((nick (nth 0 (erc-parse-user nickuserhost))))
    (when (eq match-type 'current-nick)
      (message "current-nick! type:%s nic:%s msg:%s" match-type nickuserhost msg)
      (unless (when (boundp 'erc-track-exclude)
                (member nick erc-track-exclude))
	  (message "NOT excluded!  type:%s nic:%s msg:%s" match-type nickuserhost msg)
	(erc-burnt-toast-command erc-burnt-toast-person
				 nick
				 (erc-burnt-toast--clean-msg msg nick))))))

;;;; real simple version for testing
;;;; from https://gist.github.com/jro/950202
;; (defun my/erc-global-notify (match-type nick msg)
;;   "Notify when a MSG of any MATCH-TYPE is recieved from NICK."
;;   (erc-burnt-toast-command "ERC@emacs" nick msg))
;; (add-hook 'erc-text-matched-hook 'my/erc-global-notify)

;;;###autoload(autoload 'erc-burnt-toast "erc-burnt-toast" "" t)
(eval-after-load 'erc
  `(define-erc-module burnt-toast nil
    "Send BURNT-TOAST notifications on private message and mentions."
    ;; Enable
    ((add-hook 'erc-server-PRIVMSG-functions #'erc-burnt-toast-PRIVMSG)
     (add-hook 'erc-text-matched-hook #'erc-burnt-toast-notify-on-match))
    ;; Disable
    ((remove-hook 'erc-server-PRIVMSG-functions #'erc-burnt-toast-PRIVMSG)
     (remove-hook 'erc-text-matched-hook #'erc-burnt-toast-notify-on-match))))

;;;; Footer

(provide 'erc-burnt-toast)
;;; erc-burnt-toast.el ends here
