;;; package --- display status information in bar or in modeline  -*- lexical-binding:t -*-
;;; Commentary:
;;; modes to display statuses in mode line.  requires pulseaudio and NetworkManager

;;; Code:

(require 'time)
(require 'battery)
(require 'cl-lib)
(require 'posframe)

(defun statusbar--get-buffer ()
  "Return statusbar buffer."
    (get-buffer-create " *statusbar-buffer*"))

(defun statusbar--line-length (buf)
  "Return current line length of the statusbar text.
BUF is the statusbar buffer."
  (with-current-buffer buf
    (point-max)))

(defun statusbar--position-handler (info)
  "Posframe position handler.
INFO is the childframe plist from `posframe'.
Position the statusbar in the bottom right over the minibuffer."
  (let* ((font-width (plist-get info :font-width))
         (buf (plist-get info :posframe-buffer))
         (buf-width (* font-width (statusbar--line-length buf)))
         (parent-frame (or (frame-parent (plist-get info :parent-frame)) (plist-get info :parent-frame)))
         (parent-frame-width (frame-pixel-width parent-frame))
         (x-offset (plist-get info :x-pixel-offset))
         (x-pos (- parent-frame-width buf-width x-offset))
         (y-pos 0))
    (cons x-pos y-pos)))

(defun symbol-concat (s1 s2)
  "Concatenate S1 and S2.
S1 can be a string."
  (if (symbolp s1)
      (concat (symbol-value s1) (symbol-value s2))
    (concat s1 (symbol-value s2))))

(defvar statusbar-strings (list 'display-externalcmd-string 'display-wifi-string 'display-volume-string 'display-brightness-string 'battery-statusbar-string 'statusbar-time-string)
  "List of strings for statusbar.")

(defvar statusbar-update-timer nil
  "Blah dont change.")

(defun statusbar-update ()
  "Brightness update string."
  (let ((buf (statusbar--get-buffer))
        (posframe-mouse-banish nil)
        (buffer-read-only nil)
        (inhibit-read-only t))
    (posframe-show buf
                   :string (cl-reduce #'symbol-concat statusbar-strings)
                   :poshandler #'statusbar--position-handler)))

(defun statusbar-update-handler ()
  "Handler for brightness update."
  (statusbar-update)
  (sit-for 0))

(define-minor-mode statusbar-mode
  "Displays the volume and whether or not its muted."
  :global t :group 'statusbar
  (if statusbar-mode
      (progn
        (setq statusbar-update-timer (run-at-time nil 1 #'statusbar-update-handler))
    (setq-default header-line-format "")
    (mapc (lambda (b) (with-current-buffer b
                        (or header-line-format
                            (setq-local header-line-format "")))) (buffer-list))
        (statusbar-update))
    (cancel-timer statusbar-update-timer)
    (setq statusbar-update-timer nil)
    (setq-default header-line-format nil)
    (mapc (lambda (b) (with-current-buffer b
                        (and (eq header-line-format "")
                             (setq-local header-line-format nil)))) (buffer-list))
    (posframe-delete-frame (statusbar--get-buffer))))

;; --------------------------------------------------------
;;                statusbar battery mode
;; --------------------------------------------------------


(defvar statusbar-battery-update-timer nil
  "Timer handle for battery timer.")

(defvar battery-statusbar-string nil
  "String for statusbar.")

(defun statusbar-battery-update ()
  "Update battery status information in the statusbar."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data)))))
         (state (cdr (assq ?b data)))
         (res (format "|  %s%s%% |" state percentage))
         (len (length res)))
    (unless (zerop len)
      (cond ((not (numberp percentage)))
            ((< percentage battery-load-critical)
             (add-face-text-property 0 len 'battery-load-critical t res))
            ((< percentage battery-load-low)
             (add-face-text-property 0 len 'battery-load-low t res)))
      (put-text-property 0 len 'help-echo "Battery status information" res))
    (setq battery-statusbar-string (or res ""))))

(defun statusbar-battery-update-handler ()
  "Run update for statusbar battery."
  (statusbar-battery-update)
  (sit-for 0))

(define-minor-mode statusbar-battery-mode
  "Toggle battery status display in mode line (Display Battery mode).

The text displayed in the mode line is controlled by
`battery-mode-line-format' and `battery-status-function'.
The mode line is be updated every `battery-update-interval'
seconds."
  :global t :group 'statusbar-battery
  (setq battery-statusbar-string "")
  (and statusbar-battery-update-timer (cancel-timer statusbar-battery-update-timer))
  (battery--upower-unsubscribe)
  (if battery-status-function
      (if (not statusbar-battery-mode)
          (setq battery-statusbar-string "")
        (and (eq battery-status-function #'battery-upower)
             battery-upower-subscribe
             (battery--upower-subscribe))
        (setq statusbar-battery-update-timer (run-at-time nil 1
                                                          #'statusbar-battery-update-handler))
	(statusbar-battery-update))
    (message "Battery status not available")
    (setq statusbar-battery-mode nil)))

;; --------------------------------------------------------
;;              statusbar time mode
;; --------------------------------------------------------

(defvar statusbar-time-string nil
  "String for statusbar time.")

(defvar statusbar-time-timer nil
  "Timer handle for time statusbar.")

(with-no-warnings
  ;; Warnings are suppressed to avoid "global/dynamic var `X' lacks a prefix".
  (defvar now)
  (defvar time)
  (defvar load)
  (defvar mail)
  (defvar 24-hours)
  (defvar hour)
  (defvar 12-hours)
  (defvar am-pm)
  (defvar minutes)
  (defvar seconds)
  (defvar time-zone)
  (defvar day)
  (defvar year)
  (defvar monthname)
  (defvar month)
  (defvar dayname))

(defun statusbar-time-update ()
  "Update the `display-time' info for the mode line.
However, don't redisplay right now.

This is used for things like Rmail `g' that want to force an
update which can wait for the next redisplay."
  (let* ((now (current-time))
         (time (current-time-string now))
         (load (display-time-update--load))
         (mail (display-time-update--mail))
         (24-hours (substring time 11 13))
         (hour (string-to-number 24-hours))
         (12-hours (int-to-string (1+ (% (+ hour 11) 12))))
         (am-pm (if (>= hour 12) "pm" "am"))
         (minutes (substring time 14 16))
         (seconds (substring time 17 19))
         (time-zone (car (cdr (current-time-zone now))))
         (day (substring time 8 10))
         (year (format-time-string "%Y" now))
         (monthname (substring time 4 7))
         (month
          (cdr
           (assoc
            monthname
            '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4")
              ("May" . "5") ("Jun" . "6") ("Jul" . "7") ("Aug" . "8")
              ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
         (dayname (substring time 0 3)))
    (setq statusbar-time-string
          (concat "|  " (mapconcat 'eval display-time-string-forms "") "|"))))


(defun statusbar-time-event-handler ()
  "Handler for statusbar display time."
  (statusbar-time-update)
  (sit-for 0))

(define-minor-mode statusbar-time-mode
  "Toggle display of time, load level, and mail flag in mode lines.

When Display Time mode is enabled, it updates every minute (you
can control the number of seconds between updates by customizing
`display-time-interval').  If `display-time-day-and-date' is
non-nil, the current day and date are displayed as well.  This
runs the normal hook `display-time-hook' after each update."
  :global t :group 'statusbar-time
  (and statusbar-time-timer (cancel-timer statusbar-time-timer))
  (setq statusbar-time-timer nil)
  (setq statusbar-time-string "")
  (if statusbar-time-mode
      (progn
        (setq statusbar-time-timer (run-at-time nil 1 'statusbar-time-event-handler))
	(statusbar-time-update))))

;; --------------------------------------------------------
;;                diplay brightness mode
;; --------------------------------------------------------

(defvar display-brightness-string nil
  "Volume displayed string.")

(defvar brightness-update-timer nil
  "Blah dont change.")

(defvar brightness-shellcommand "backlightctrl -get"
  "Change to get the brightness for your system.")

(defun brightness-update ()
  "Brightness update string."
  (let* ((percent (car (split-string (shell-command-to-string brightness-shellcommand) "\n" t)))
         (str (concat "|  " percent (if statusbar-mode "% |" "%% |")))
         (len (length str)))
    (put-text-property 0 len 'help-echo (format "Brightness: %s%%" percent) str)
    (setq display-brightness-string str)))

(defun brightness-update-handler ()
  "Handler for brightness update."
  (brightness-update)
  (sit-for 0))

(define-minor-mode display-brightness-mode
  "Displays the volume and whether or not its muted."
  :global t :group 'display-brightness
  (if display-brightness-mode
      (progn
        (or statusbar-mode
            (or (memq 'display-brightness-string global-mode-string)
            (add-to-list 'global-mode-string 'display-brightness-string t)))
        (setq brightness-update-timer (run-at-time nil 1 #'brightness-update-handler))
        (brightness-update))
    (setq global-mode-string (delq 'display-brightness-string global-mode-string))
    (cancel-timer brightness-update-timer)
    (setq brightness-update-timer nil)
    (setq display-brightness-string "")))

;; --------------------------------------------------------
;;                   diplay volume mode
;; --------------------------------------------------------

(defvar display-volume-string nil
  "Volume displayed string.")

(defvar display-volume-pa-sink "@DEFAULT_SINK@"
  "What sink to use for getting volume and muted.")

(defvar volume-update-timer nil
  "Blah dont change.")

(defun volume-update ()
  "Update volume string."
  (let* ((muted (string= "yes" (substring (car (split-string (shell-command-to-string (concat "pactl get-sink-mute " display-volume-pa-sink)) "\n" t)) 6)))
         (vol (format "%d" (let* ((cmd (split-string (shell-command-to-string (concat "pactl get-sink-volume " display-volume-pa-sink)) " " t))
                                  (left (string-to-number(car (split-string (nth 4 cmd) "%" t))))
                                  (right (string-to-number(car (split-string (nth 11 cmd) "%" t)))))
                             (/ (+ left right) 2))))

         (str (concat (if muted "|  " "|  ") vol (if statusbar-mode "%" "%%") " |"))
         (len (length str)))
    (put-text-property 0 len 'help-echo (format "Volume: %s%% Muted: %s" vol muted) str)
    (setq display-volume-string str)))

(defun volume-update-handler ()
  "Handler for volume update."
  (volume-update)
  (sit-for 0))

(define-minor-mode display-volume-mode
  "Displays the volume and whether or not its muted."
  :global t :group 'display-volume
  (if display-volume-mode
      (progn
        (or statusbar-mode
            (or (memq 'display-volume-string global-mode-string)
            (add-to-list 'global-mode-string 'display-volume-string t)))
        (setq volume-update-timer (run-at-time nil 1 #'volume-update-handler))
        (volume-update))
    (setq global-mode-string (delq 'display-volume-string global-mode-string))
    (cancel-timer volume-update-timer)
    (setq volume-update-timer nil)
    (setq display-volume-string "")))

;; --------------------------------------------------------
;;                   diplay wifi mode
;; --------------------------------------------------------

(defvar display-wifi-string nil
  "Volume displayed string.")

(defvar display-wifi-essid-command "iw dev $(ip addr | awk '/state UP/ {gsub(\":\",\"\"); print $2}') link | grep SSID: | cut -d':' -f2"
  "Should get the essid of wifi connected to.")

(defvar display-wifi-connection-command "iw dev $(ip addr | awk '/state UP/ {gsub(\":\",\"\"); print $2}') link | awk '/signal:/ {gsub(\"-\",\"\"); printf $2}'"
  "Should get the connection strength of wifi connected to.")

(defvar wifi-update-timer nil
  "Blah dont change.")

(defun wifi-update ()
  "Update wifi string."
  (let* ((essid (string-trim (shell-command-to-string display-wifi-essid-command)))
         (connection (string-trim (shell-command-to-string display-wifi-connection-command)))
         (str (if (string= essid "") "|  NO SIGNAL |" (concat (format "|  %s %s" essid connection) (if statusbar-mode "% |" "%% |"))))
         (len (length str)))
    (put-text-property 0 len 'help-echo (format "Wifi: essid: %s" essid) str)
    (setq display-wifi-string str)))

(defun wifi-update-handler ()
  "Handler for wifi update."
  (wifi-update)
  (sit-for 0))

(define-minor-mode display-wifi-mode
  "Displays the volume and whether or not its muted."
  :global t :group 'display-wifi
  (if display-wifi-mode
      (progn
        (or statusbar-mode
            (or (memq 'display-wifi-string global-mode-string)
            (add-to-list 'global-mode-string 'display-wifi-string t)))
        (setq wifi-update-timer (run-at-time nil 1 #'wifi-update-handler))
        (wifi-update))
    (setq global-mode-string (delq 'display-wifi-string global-mode-string))
    (cancel-timer wifi-update-timer)
    (setq wifi-update-timer nil)
    (setq display-wifi-string "")))

;; --------------------------------------------------------
;;                   diplay externalcmd mode
;; --------------------------------------------------------

(defvar display-externalcmd-string nil
  "Volume displayed string.")

(defvar externalcmd-update-timer nil
  "Blah dont change.")

(defvar externalcmd-shellcommand ""
  "Any command you want.")

(defun externalcmd-update ()
  "Externalcmd update string."
    (setq display-externalcmd-string (string-trim (shell-command-to-string externalcmd-shellcommand))))

(defun externalcmd-update-handler ()
  "Handler for externalcmd update."
  (externalcmd-update)
  (sit-for 0))

(define-minor-mode display-externalcmd-mode
  "Displays the volume and whether or not its muted."
  :global t :group 'display-externalcmd
  (if display-externalcmd-mode
      (progn
        (or statusbar-mode
            (or (memq 'display-externalcmd-string global-mode-string)
            (add-to-list 'global-mode-string 'display-externalcmd-string t)))
        (setq externalcmd-update-timer (run-at-time nil 1 #'externalcmd-update-handler))
        (externalcmd-update))
    (setq global-mode-string (delq 'display-externalcmd-string global-mode-string))
    (cancel-timer externalcmd-update-timer)
    (setq externalcmd-update-timer nil)
    (setq display-externalcmd-string "")))

(provide 'statusbar)
;;; statusbar.el ends here
