;;;; Programmer: Ethan Smith
;;;; Date: 19 June 2023
;;;; Description: StumpWM Config

;;;
;;; Debugging stuff
;;;

;; (setf *debug-level* 0)
;; (redirect-all-output (data-dir-file "my-debug" "txt"))
;; (setf *debug-expose-events* nil)

;;;
;;; Environment Setup
;;;

(in-package :stumpwm)                       ; don't type stumpwm so much
(require 'ql "~/quicklisp/setup.lisp")      ; load slynk to enable easy editing
(ql:quickload 'slynk)                       ; in emacs
(require :slynk)                            ; |

(defcommand slynk () ()                    ; setup the slynk server on port 1234
  (slynk:create-server :port 1234          ; |
                       :dont-close t))     ; |

;; load your extra documentation
;; (load "~/.stumpwm.d/extra-docs.lisp")

;; launch default programs

;;;
;;; Async program launching
;;;
(defparameter *async-shell* (uiop:launch-program "bash" :input :stream
                                                        :output :stream))

;; TODO finish making this work properly
(defun async-run (command)
  (write-line command (uiop:process-info-input *async-shell*))
  (force-output (uiop:process-info-input *async-shell*)))

(define-key *top-map* (kbd "s-SPC") "launch")

(defcommand launch (bin) ((:string "program: "))
  (uiop:launch-program bin :input :stream :output :stream))

(defcommand now-we-are-six (name age)
    ((:string "enter you name: ")
     (:number "enter your age: "))
  (message "~A, in six years you will be ~A" name (+ 6 age)))

(define-key *root-map* (kbd "L") "now-we-are-six Ethan")

;;;
;;; Window Movement
;;;
;; <super> c is convenient, and follows the spirit of emacs key conventions.
(set-prefix-key (kbd "s-c"))
(setf *mouse-focus-policy* :click)

;; for convenience, these movement keys aren't hidden behind a prefix layer
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-l") "move-focus right")

;; moving between hidden windows
(define-key *top-map* (kbd "s-TAB") "pull-hidden-next")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "pull-hidden-previous")

;; resize windows
(define-key *top-map* (kbd "s-C-h") "resize-direction left")
(define-key *top-map* (kbd "s-C-l") "resize-direction right")

;;;
;;; Groups
;;;
(setf *default-group-type* 'tile-group)
(define-key *groups-map* (kbd "d") "gnew-dynamic")

(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-6") "gselect 6")
(define-key *top-map* (kbd "s-7") "gselect 7")
(define-key *top-map* (kbd "s-8") "gselect 8")
(define-key *top-map* (kbd "s-9") "gselect 9")
(define-key *top-map* (kbd "s-0") "gselect 0")


;;;
;;; Programs
;;;
(define-key *root-map* (kbd "RET") "exec alacritty")
(define-key *root-map* (kbd "s-RET") "exec alacritty")

;;;
;;; MEDIA
;;;

(stumpwm:add-to-load-path "~/.stumpwm.d/modules/stumpwm-pamixer/")
(load-module "pamixer")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "pamixer-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "pamixer-volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "pamixer-toggle-mute")

(define-key *top-map* (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map* (kbd "XF86AudioPause") "exec playerctl play-pause")

(define-key *top-map* (kbd "XF86AudioNext") "exec playerctl next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec playerctl previous")

;; (define-key *top-map* (kbd "print") "launch flameshot-gui")

;;;
;;; Display Settings
;;;
;; (add-to-load-path "~/.stumpwm.d/modules/modeline/wifi")
;; (load-module "wifi")

(load "~/.stumpwm.d/playerctl.lisp")

;;;;;;;;;;;;;;;;;;;;
;; CLIM Mode Line ;;
;;;;;;;;;;;;;;;;;;;;

(load-module "clim-mode-line")
(clim-mode-line:app-main)
;;  for some reason, StumpWM detects a ton of bogus modelines. they can easily
;;  be removed by sorting out by remvoing the ones with nil heads
(setf *mode-lines* (remove-if-not #'mode-line-head *mode-lines*))

;; run redisplaay twice, because once doesn't do it correctly for some reason.
(clim-mode-line::redisp clim-mode-line::*stumpwm-modeline-frame*)
(clim-mode-line::redisp clim-mode-line::*stumpwm-modeline-frame*)

(defun format-windows (frame pane other-formaters)
  (let ((cur-win (current-window)))
    (clim-mode-line::do-list-with-interspersed-element
        (win (group-windows (current-group))
          (format pane "] ["))
      (clim-mode-line::with-stumpwm-highlighting (pane (eq cur-win win))
        (clim-mode-line::present win 'stumpwm::window :stream pane :single-box t)))))


(defun remove-some-threads ()
  "remove clim-mode-line threads, because they are causing problems for some reason"
  (mapc #'sb-thread:terminate-thread
        (remove-if-not
         (lambda (thr)
           (equal (slot-value thr 'sb-thread::name) "CLIM-MODE-LINE"))
         (sb-thread:list-all-threads))))

;; mode line configuration
(setf *window-format* "%m(%n%s%50c)")
(setf *time-modeline-string* "%e %b^2 %H%M^n")

(setf *screen-mode-line-format* "[^B%n^b] %W ^> %p %P [%I] [^B%d^b]")
;; (enable-mode-line (current-screen) (current-head) t)
;; (setf *mode-line-timeout* 1)
;; (enable-mode-line (current-screen) (current-head) t)

;; load nice fonts
(ql:quickload :clx-truetype)
(add-to-load-path "~/.stumpwm.d/modules/util/ttf-fonts")
(load-module "ttf-fonts")
(setq clx-truetype::*font-dirs*
      (append (list (namestring "~/.stumPwm.d/fonts/dovens/"))
              clx-truetype::*font-dirs*))
(clx-truetype:cache-fonts)

;; (set-font (make-instance 'xft:font :family "DOVENSPersonalUse"
;;                                    :subfamily "Regular"
;;                                    :size 10
;;                                    :antialias t))

;; (set-font (make-instance 'xft:font :family "BigBlueTerm437 Nerd Font Mono"
;;                                    :subfamily "Regular"
;;                                    :size 10.5
;;                                    :antialias nil))

;; (set-font (make-instance 'xft:font :family "ProggyCleanSZ Nerd Font Mono"
;; ;;                                    :subfamily "Regular"
;; ;;                                    :size 14
;; ;;                                    :antialias t))

(setf *mode-line-background-color* "grey20")
(setf *mode-line-foreground-color* "grey40")

;;;
;;; Focus Mode
;;;

;; when s-SPC is pressed, we want to
;;  - store the current layout of the group
;;  - delete all but the active frame
;;  - change layout state to focus

(defun layout-state (group)
  "describes the group state. possible options are :normal and :focus")

;;;
;;; Example Configuration
;;
;;; what follows is some of the default configuration file that StumpWM provides
;;;

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; Message window font
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")


;; Clear rules
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
(define-frame-preference "Default"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "Ardour"
  (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
  (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))
