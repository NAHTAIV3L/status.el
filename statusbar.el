;;; package --- display status information in bar or in modeline  -*- lexical-binding:t -*-
;;; Commentary:
;;; modes to display statuses in mode line.  requires pulseaudio and NetworkManager

;;; Code:

(require 'time)
(require 'battery)
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
         (parent-frame (plist-get info :parent-frame))
         (parent-frame-width (frame-pixel-width parent-frame))
         (x-offset (plist-get info :x-pixel-offset))
         (x-pos (- parent-frame-width buf-width x-offset))
         (y-pos -1))
    (cons x-pos y-pos)))

(defun symbol-concat (s1 s2)
  "Concatenate S1 and S2.
S1 can be a string."
  (if (symbolp s1)
      (concat (symbol-value s1) (symbol-value s2))
    (concat s1 (symbol-value s2))))

(defvar statusbar-strings (list 'display-wifi-string 'display-volume-string 'display-brightness-string 'battery-statusbar-string 'statusbar-time-string)
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
                   :string (-reduce #'symbol-concat statusbar-strings)
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
        (statusbar-update))
    (cancel-timer statusbar-update-timer)
    (setq statusbar-update-timer nil)
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
         (state (cdr (assq ?b (funcall battery-status-function))))
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
  (let* ((current (current-time))
         (timer display-time-timer)
         ;; Compute the time when this timer will run again, next.
         (next-time (timer-relative-time
        	     (list (aref timer 1) (aref timer 2) (aref timer 3))
        	     (* 5 (aref timer 4)) 0)))
    ;; If the activation time is not in the future,
    ;; skip executions until we reach a time in the future.
    ;; This avoids a long pause if Emacs has been suspended for hours.
    (or (time-less-p current next-time)
        (progn
          (timer-set-time timer (timer-next-integral-multiple-of-time
        			 current display-time-interval)
        		  display-time-interval)
          (timer-activate timer)))))

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

(defun brightness-update ()
  "Brightness update string."
  (setq display-brightness-string
	(concat "|  " (car (split-string (shell-command-to-string "printf \"%.*f\n\" 0 $(xbacklight -get)") "\n" t)) (if statusbar-mode "% |" "%% |"))))

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
  (setq display-volume-string
	(concat
	 (if (string= "yes" (substring (car (split-string (shell-command-to-string (concat "pactl get-sink-mute " display-volume-pa-sink)) "\n" t)) 6))
	   "|  "
	   "|  ")
         (format "%d" (let* ((cmd (split-string (shell-command-to-string (concat "pactl get-sink-volume " display-volume-pa-sink)) " " t))
                             (left (string-to-number(car (split-string (nth 4 cmd) "%" t))))
                             (right (string-to-number(car (split-string (nth 11 cmd) "%" t)))))
                        (/ (+ left right) 2)))
	 (if statusbar-mode "%" "%%")
	 " |")))

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

(defvar wifi-update-timer nil
  "Blah dont change.")

(defun wifi-update ()
  "Update wifi string."
  (let ((essid
	 (car
	  (split-string
	   (or
	    (car
	    (split-string
	     (shell-command-to-string "nmcli -t -f name,device connection show --active | grep -v lo:lo")
	     "\n" t))
	    "")
	   ":" t)))
        (connection
	 (car
	  (split-string
	   (shell-command-to-string "awk 'NR==3 {print $3}' /proc/net/wireless | cut -d. -f1")
	   "\n" t))))
    (if (equal essid nil)
      (setq display-wifi-string "|  NO SIGNAL |")
      (setq display-wifi-string (concat (format "|  %s %s" essid connection) (if statusbar-mode "% |" "%% |"))))))

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

(provide 'statusbar)
;;; statusbar.el ends here
