(in-package :stumpwm)

(defparameter *playerctl-format* "%T - %A %p"
  "modeline format for playerctl output.

%T - Track Title
%A - Artist")

;; Here is how I got the special characters down there:
;;  1) I am using Nerd Fonts, so look up the symbol list online.
;;  2) set the unicode radix to 16: (setq read-quoted-char-radix 16)
;;  3) use C-q in emacs and type the unicode value you found
(defparameter *playerctl-format-alist*
  (list '(#\T track-title)
        '(#\A track-artist)
        (list '#\p (lambda () (play-status #\ #\ #\)))
        ))

(defun track-artist ()
  "return the track artist as reported by playerctl"
  (string-trim (string #\newline)
               (run-shell-command "playerctl metadata xesam:artist" t)))

(defun track-title ()
  "return the track title as reported by playerctl"
  (string-trim (string #\newline)
               (run-shell-command "playerctl metadata xesam:title" t)))

(defun play-status (&optional play pause stop)
  "returns the current state of the player.

If play, pause, or stop are set, then those characters will be returned intead
of the default output of playerctl"

  (let ((output (string-right-trim '(#\newline)
                                   (run-shell-command "playerctl status" t))))
    (cond ((equal output "Playing")
           (or play output)
           )
          ((equal output "Paused")
           (or pause output))
          (t
           (or stop output)))))

(add-screen-mode-line-formatter #\p 'playerctl-modeline)

(defun playerctl-toggle ()
  "toggles the current player state. ie, if music is playing stop, etc."
  (run-shell-command "playerctl play-pause"))


(defun playerctl-modeline (ml)
  (declare (ignore ml))
  "retuns the text for the mode line, based off of `*PLAYERCTL-FORMAT*'"
  (format-expand *playerctl-format-alist* *playerctl-format*))
