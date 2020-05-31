;;; common-allow-deny-rule.el --- a function to determine whether a member is
;;;     allowed or not by checking against the white and black list
;;
;; Filename: common-allow-deny-rule.el
;; Description:
;; Author: Myoungjin Jeon <jeongoon@g>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  Basic Usage:
;;  check hte the arg given is the member of white-list and
;;  exempt if it is also found in black list
;;   example)
;;   (setq some-allowed-mode '(prog-mode emacs-lisp-mode text-mode))
;;   (setq some-exempt-mode '(org-mode))
;;   (common-allow-deny-rule-apply major-mode some-allowed-mode some-exempt-mode)
;;
;;  return value will be one of the vaules below
;;   result   at stage
;;  ---------------------
;;  (denied  allow-stage)
;;  (denied  deny-stage)
;;  (allowed deny-stage)
;;
;; There are only two stage.
;;  - test against with white list at allow-stage
;;  - test against with black list at denied-stage
;;
;;; Code:

(defun common-allow-deny-rule-apply (candi white-list black-list &rest allow-stage-failed-hook-list)
  "check candidate against white list and black list and return a list of two elements"
  (let* ((dmsg_ nil) (final-status 'denied)
         (final-stage  'allow-stage))
    (if (or (eq white-list t) ;; if t then allow all but will check black list again
            (member candi white-list))
      ;; THEN
        (setq final-stage 'deny-stage) ;; ready to goto next stage
      ;; ELSE
      (progn
        (when dmsg_ (message "%s" allow-stage-failed-hook-list))
        (if (and allow-stage-failed-hook-list ;; why needed?
                 (listp allow-stage-failed-hook-list))
            (progn
              (when dmsg_ (message "common-allow-deny-rule-apply: go check further with allowed-stage-failed-hook-list: %s" allow-stage-failed-hook-list))
              (let (allowed? curr-hook hook-list res)
                (setq allowed? nil) ; not yet
                (setq hook-list allow-stage-failed-hook-list) ; copy the hook list?
                (if (dolist (allowed-elem white-list allowed?)
                      (while (and hook-list (not allowed?))
                        ;; FIXME: Better way?
                        (setq curr-hook (car hook-list))
                        (setq hook-list (cdr hook-list))
                        (when dmsg_ (message "common-allow-deny-rule-apply: (%s %s %s)" curr-hook candi allowed-elem))
                        (setq res (funcall curr-hook candi allowed-elem))
                        (when dmsg_ (message "common-allow-deny-rule-apply: result: %s" res))
                        (when res
                                        ; if curr-hook return t candi is allowed
                          (setq allowed? t))))
                                        ; allowed!
                    (setq final-stage 'deny-stage))))
                                        ; denied already without further investigation
          (setq final-status 'denied))))
    ; deny-stage
    (when (eq final-stage 'deny-stage)
      (when (not (member candi black-list))
        (setq final-status 'allowed)))
    (list final-status final-stage)))

(provide 'common-allow-deny-rule)
;;; common-allow-deny-rule.el ends here
