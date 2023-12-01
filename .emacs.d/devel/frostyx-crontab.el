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

;; (defun crontab--time (minute hour)
;;   (if (equal minute "*")
;;       "every minute"
;;     (format "minute %s" minute)))

(defun crontab--time (minute hour)
  (cond ((and (equal minute "*") (equal hour "*"))
         "every minute")

        ((equal hour "*")
         (format "minute %s" minute))

        (t
         (format
          "%s:%s"
          (format "%02d" (string-to-number hour))
          (format "%02d" (string-to-number minute))))))

;; (crontab--time "*" "*")
;; (crontab--time "1" "*")
;; (crontab--time "5" "4")

(defun crontab--day-of-week (value)
  (if (equal value "*")
      nil
    (calendar-day-name
     (if (equal value "7") 0 (string-to-number value)) nil t)))

;; (crontab--day-of-week "*")
;; (crontab--day-of-week "2")
;; (crontab--day-of-week "7")

(defun crontab--day-of-month (value)
  (if (equal value "*")
      nil
    (format "day-of-month %s" value)))

;; (crontab--day-of-month "*")
;; (crontab--day-of-month "5")

(defun crontab--month (value)
  (if (equal value "*")
      nil
    (calendar-month-name (string-to-number value))))

;; (crontab--month "*")
;; (crontab--month "8")

(defun crontab-human-friendly (entry)
  ;; This should work
  ;; (print (transient-arg-value "--minute=" (transient-args 'crontab)))
  ;; (print (transient-args 'crontab))

  (let* ((split (split-string entry))
         (minute       (elt split 0))
         (hour         (elt split 1))
         (day-of-month (elt split 2))
         (month        (elt split 3))
         (day-of-week  (elt split 4)))

    ;; WIP
    (let* ((time (crontab--time minute hour))
           (day-of-week (crontab--day-of-week day-of-week))
           (month (crontab--month month))
           (day-of-month (crontab--day-of-month day-of-month)))

      ;; (format "At %s on %s." time day-of-week)

      (let* ((result "")
             (result (string-join (list result "At " time)))

             (result
              (if day-of-week
                  (string-join (list result " on " day-of-week))
                result))

             (result
              (if month
                  (string-join (list result " in " month))
                result))


             (result (string-join (list result "."))))

        result

        )



      )

  ;; Proof that we can return dynamic value
  ;; and it will change in the transient window
  ;; (current-time-string)
  ))

(crontab-human-friendly "* * * * *")
(crontab-human-friendly "1 * * * *")
(crontab-human-friendly "5 4 * * 7")
(crontab-human-friendly "5 0 * 8 *")
(crontab-human-friendly "5 0 * 8 1")


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
