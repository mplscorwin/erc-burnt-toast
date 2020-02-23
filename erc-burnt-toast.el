;;; erc-burnt-toast.el --- erc-match support for w32 notification center -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; URL: https://bru.st
;; Version: 0.2-pre
;; Package-Requires: ((emacs "26.0"))
;; Keywords: irc erc burnt-toast

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Provide Windows Notification Center to erc with burnt-toast and erc-match
;;
;; 0.2 - clean-input: remove text properties using `org-no-properties'

;;;; Installation
;;;;; MELPA
;; If you installed from MELPA, you're done.
;;;;; Manual
;; Install these required packages:
;; + ercn
;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'erc-burnt-toast)
;;;; Usage
;; Run one of these commands:
;; `erc-burnt-toast-command': Frobnicate the flange.
;;;; Tips
;; + You can customize settings in the `erc-burnt-toast' group.
;;;; Credits
;; This package would not have been possible without the following
;; packages: emacs-package-dev-handbook[1], which showed the form
;; and Alex Murray's erc-desktop-notifications from which I lifted
;; the vast majority of the implementation.
;;
;;  [1] https://github.com/alphapapa/emacs-package-dev-handbook#template
;;  [2] https://github.com/emacs-mirror/emacs/blob/master/lisp/erc/erc-desktop-notifications.el
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
(require 'erc)
(require 'erc-match)
(require 'org-macs)
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
  "Sound options are based on those available from Notification Center."
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
  "powershell.exe -Command \"\\$Logo = '%s'; \\$Image = '%s'; \\$Person = '%s'; \\$Text = '%s %s'; New-BurntToastShoulderTap -AppLogo \\$Logo -Image \\$Image -Person \\$Person -Text \\$Text\""
  "This is the format string for the powershell command.")

;; humurglesmurf
;;(setq erc-burnt-toast-format   "powershell.exe New-BurntToastShoulderTap -AppLogo '%s' -Image '%s' -Person '%s' -Text \"%s\", \"%s\"")
;(setq erc-burnt-toast-format "powershell.exe -Command \"Import-Module C:\\Program Files\\WindowsPowerShell\\Modules\\BurntToast\\0.7.0\\BurntToast; New-BurntToastShoulderTap -AppLogo '%s' -Image '%s' -Person '%s' -Text '%s','%s'\"")

;; this one worked.
;;(setq erc-burnt-toast-format "powershell.exe -Command \"\\$Logo = '%s'; \\$Image = '%s'; \\$Person = '%s'; \\$Text = '%s %s'; New-BurntToastShoulderTap -AppLogo \\$Logo -Image \\$Image -Person \\$Person -Text \\$Text\"")

(defvar erc-burnt-toast-query-on-unjoined-chan-privmsg
  nil
  "Whether notifications are triggered by query from unjoined channels.")

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

;;;;; Commands
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

(defun xah-asciify-text (&optional @begin @end)
  "Remove accents in some letters.

Change European language characters into equivalent ASCII ones,
e.g. “café” ⇒ “cafe”.  When called interactively, work on current
line or text selection, optionally between BEGIN and END,
otherwise convert the whole string.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun xah-asciify-string (@string)
  "Returns a new @STRING.

European language chars are changed ot ASCII ones e.g. “café” ⇒
“cafe”.  See `xah-asciify-text' Version 2015-06-08"
  (with-temp-buffer
      (insert @string)
      (xah-asciify-text (point-min) (point-max))
      (buffer-string)))

(defun erc-burnt-toast-command (person title description)
  "Pop toast in the Windows Notification Center given PERSON, TITLE, AND DESCRIPTION."
  (interactive "sFrom:\nsSubject:\nsBody:")
  (let* ((command-text (erc-burnt-toast-format-command erc-burnt-toast-logo
						       erc-burnt-toast-image
						       person
						       title
						       description))
	 (cleaned (xah-asciify-string command-text)))
    (message "(%s) %s => %s " (equal command-text cleaned) command-text cleaned)
    (message "Running command> %s" command-text)
    (start-process-shell-command "make-toast" erc-burnt-toast-buffer
				 command-text)))
;; (erc-burnt-toast-command "ERC@ema.cs" "teste" "still a test")

(defun erc-burnt-toast-2-PRIVMSG (_proc parsed)
  "Conditionally send a notification given _PROC and PARSED."
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (and (boundp 'erc-track-exclude)
                         (member nick erc-track-exclude)))
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (erc-burnt-toast-command erc-burnt-toast-person nick msg)))
  ;; Return nil to continue processing by ERC
  nil)

(defun erc-burnt-toast-PRIVMSG (proc parsed)
  "Give PROC and PARSED, issue a Windows Notification Center alert.
PROC and PARSED represent a ERC process and a parsed private message."
  (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
	 (target (car (erc-response.command-args parsed)))
	 (msg (erc-response.contents parsed))
	 (query  (if (not erc-burnt-toast-query-on-unjoined-chan-privmsg)
		     nick
		   (if (erc-current-nick-p target)
		       nick
		     target))))
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

;;;###autoload(autoload 'erc-burnt-toast "erc-desktop-notifications" "" t)
(define-erc-module notifications nil
  "Send NOTIFICATIONS on private message reception and mentions."
  ;; Enable
  ((add-hook 'erc-server-PRIVMSG-functions 'erc-burnt-toast-PRIVMSG)
   (add-hook 'erc-text-matched-hook 'erc-burnt-toast-notify-on-match))
  ;; Disable
  ((remove-hook 'erc-server-PRIVMSG-functions 'erc-burnt-toast-PRIVMSG)
   (remove-hook 'erc-text-matched-hook 'erc-burnt-toast-notify-on-match)))
;;;;; Support

(defun erc-burnt-toast--something (args)
  "This function helps frobnicate ARGS flange."
  (message "%s" args))

;;; Cruft

;;((nick (nth 0 (erc-parse-user nickuserhost)))
;; (nick (if (string-match-p "^Server:" orig-nick)
;; 	     ;;(or (replace-regexp-in-string "^\\s*<\\(\\S+\\)>" "\\1" msg)
;; 	     (if (string-match
;; 		  "\\([^\s><]+\\)>\\(\s+\\([^\s:\r\n]*\\)[\s\r\n:]*\\)" msg)
;; 		 (match-string 1 msg)
;; 	       orig-nick)
;; 	   orig-nick))
;;(short-msg (if (not (string= orig-nick nick))
;;	  (or (substring msg (match-end 2))
;;		      msg)
;;		msg)))


;; (let ((str "  [20:56 ] <corwin> corwin_: hi!")
;;       (data (match-data)))
;;   (unwind-protect ; Ok to change the original match data.
;;       (if (not (string-match erc-burnt-toast--timestamp-restr
;; 		;; (concat "^\\([\n\r\t\s]*"
;; 		;; 		     "\\[?\\(?:[\n\r\t\s]*"
;; 		;; 		     "[0-9]?[0-0]:[0-9][0-9]"
;; 		;; 		     "[\n\r\t\s]*\\)]?"
;; 		;; 		     "[\n\r\t\s]*\\)")
;; 			     str))
;; 	  (message "no match")
;; 	(message "match[1]%s" (match-string 1 str))
;; 	(message "match[1]%s" (match-string 1 str)))
;;     (set-match-data data)))


    ;; (message "processing match on current nick!")
    ;; (let* ((orig-nick (nth 0 (erc-parse-user nickuserhost)))
    ;; 	   (nick (if (string-match-p "^Server:" orig-nick)
    ;; 		     ;;(or (replace-regexp-in-string "^\\s*<\\(\\S+\\)>" "\\1" msg)
    ;; 		     (if (string-match
    ;; 			  "\\([^\s><]+\\)>\\(\s+\\([^\s:\r\n]*\\)[\s\r\n:]*\\)" msg)
    ;; 			 (match-string 1 msg)
    ;; 		       orig-nick)
    ;; 		   orig-nick))
    ;; 	   (short-msg (if (not (string= orig-nick nick))
    ;; 			  (or (substring msg (match-end 2))
    ;; 			      msg)
    ;; 			msg)))
    ;;   (message "nick:%s, msg:%s" nick short-msg)
    ;;   (unless (when (boundp 'erc-track-exclude)
    ;;             (member nick erc-track-exclude))
    ;;     (erc-burnt-toast-command nickuserhost nick short-msg)))))


;;(erc-burnt-toast-notify-on-match 'current-nick "foo!bar@baz" "test msg")
;(erc-burnt-toast-notify-on-match 'current-nick "Server:" "20:56 <corwin> corwin_: hi!")


;; (let ((str "20:56 <corwin> corwin_: hi!")
;;       (data (match-data)))
;;   (unwind-protect ; Ok to change the original match data.
;;       (if (not (string-match "\\([^\s><]+\\)>\s+\\([^\s:\r\n]*[\s\r\n:]*\\)" str))
;; 	  (message "no match")
;; 	(message "match[1]%s" (match-string 1 str))
;; 	(message "match[2]%s" (match-string 2 str)))
;;     (set-match-data data)))

;; (replace-regexp-in-string "^\\([\r\n\t\s]*[\\[<]?[0-9][0-9]:[0-9][0-9][]>]?[\r\n\t\s]*\\)\\|\\([\r\n\t\s]*[\\[<]?[0-9][0-9]:[0-9][0-9][]>]?[\r\n\t\s]*$\\)" ""  "[12:12>  nick:    \n\n    a long message         \n\n      <12:12]")


;;;; Footer

(provide 'erc-burnt-toast)
;;; erc-burnt-toast.el ends here
