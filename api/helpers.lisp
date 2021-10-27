(in-package #:latter-day-paypal)

(defun %quick-hash (alist &rest rest &key &allow-other-keys)
  "Takes in an alist and quickly generators a hash"
  (let ((hashtable (apply #'make-hash-table rest)))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                alist
              (unless (eq b :ne)
                (setf (gethash a hashtable) b))))
          alist)
    hashtable))
