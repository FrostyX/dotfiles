;; https://crontab.guru/

;; Great demo for transient
;; https://github.com/positron-solutions/transient-showcase/blob/master/transient-showcase.el
;;
;; (use-package transient-showcase
;;   :quelpa (transient-showcase
;;            :fetcher github
;;            :repo "positron-solutions/transient-showcase"))


;; TODO Theese need custom reader. It needs to allow *, -, /, and comma.

(transient-define-argument crontab:--minute ()
  :description "Minute"
  :class 'transient-option
  :key "-m"
  :argument "--minute="
  :reader #'transient-read-number-N0)

(transient-define-argument crontab:--hour ()
  :description "Hour"
  :class 'transient-option
  :key "-h"
  :argument "--hour="
  :reader #'transient-read-number-N0)

(transient-define-argument crontab:--day-of-month ()
  :description "Day of month"
  :class 'transient-option
  :key "-d"
  :argument "--day-of-month="
  :reader #'transient-read-number-N0)

(transient-define-argument crontab:--month ()
  :description "Month"
  :class 'transient-option
  :key "-M"
  :argument "--month="
  :reader #'transient-read-number-N0)

(transient-define-argument crontab:--day-of-week ()
  :description "Day of week"
  :class 'transient-option
  :key "-D"
  :argument "--day-of-week="
  :reader #'transient-read-number-N0)

(defun crontab-human-friendly ()
  ;; This should work
  ;; (print (transient-arg-value "--minute=" (transient-args 'crontab)))
  ;; (print (transient-args 'crontab))

  "At 04:05 on Sunday."

  ;; Proof that we can return dynamic value
  ;; and it will change in the transient window
  ;; (current-time-string)
  )

(transient-define-suffix crontab-foo (&optional args)
  (transient-arg-value "--minute=" (transient-args 'crontab)))

(transient-define-prefix crontab ()
  ;; This is not ideal because for changing the value, one must have to do
  ;; for example `-m' to reset the minute and then `-m' again to set its value
  ;; TODO Use :init-value instead
  ;; :value '("--minute=*"
  ;;          "--hour=*"
  ;;          "--day-of-month=*"
  ;;          "--month=*"
  ;;          "--day-of-week=*")

  [ :description (lambda () (format "Crontab: %s\n" (crontab-human-friendly)))
    ["Custom"
     (crontab:--minute)
     (crontab:--hour)
     (crontab:--day-of-month)
     (crontab:--month)
     (crontab:--day-of-week)]
    ["Preconfigured (non-standard)"
     ("@y" "Yearly" customize-group)
     ("@a" "Annually" customize-option)
     ("@m" "Monthly" customize-option)
     ("@w" "Weekly" customize-face)
     ("@d" "Daily" customize-face)
     ("@h" "Hourly" customize-face)
     ("@r" "Reboot" customize-face)

     ;; TODO Actions
     ;; - Generate random values
     ;; - Copy the string to clipboard
     ;; - Write to buffer
     ]])
