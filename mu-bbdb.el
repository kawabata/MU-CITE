;;; mu-bbdb.el --- registration feature of mu-cite using BBDB

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;   (modified for BBDB3 by KAWABATA Taichi <kawabata.taichi _at_ gmail.com )
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: BBDB, citation, mail, news

;; This file is part of MU (Message Utilities).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Codes are modified to work with BBDB3.

;;; Code:

(require 'mu-cite)
(require 'bbdb-com)

(defvar mu-bbdb-history nil)


;;; @ BBDB interface
;;;

(defun mu-bbdb-get-attr (address-structure)
  "Extract attribute information from BBDB."
  (let* ((mail (cadr address-structure))
         (record (car (bbdb-search (bbdb-records) nil nil mail))))
    (if record
	(bbdb-record-get-field record 'attribution))))

(defun mu-bbdb-set-attr (attr addr-structure)
  "Add attribute information to BBDB."
  (let* ((record (bbdb-annotate-message
		  addr-structure
		  t)))
    (if record
	(progn
	  (bbdb-record-set-field (car record) 'attribution attr)
	  (bbdb-change-record (car record) nil t)))))


;;; @ methods
;;;

;;;###autoload
(defun mu-bbdb-get-prefix-method ()
  "A mu-cite method to return a prefix from BBDB or \">\".
If an `attribution' value is found in BBDB, the value is returned.
Otherwise \">\" is returned.

Notice that please use (mu-cite-get-value 'bbdb-prefix)
instead of call the function directly."
  (or (mu-bbdb-get-attr (mu-cite-get-value 'address-structure))
      ">"))

;;;###autoload
(defun mu-bbdb-get-prefix-register-method ()
  "A mu-cite method to return a prefix from BBDB or register it.
If an `attribution' value is found in BBDB, the value is returned.
Otherwise the function requests a prefix from a user.  The prefix will
be registered to BBDB if the user wants it.

Notice that please use (mu-cite-get-value 'bbdb-prefix-register)
instead of call the function directly."
  (let ((addr (mu-cite-get-value 'address-structure)))
    (or (mu-bbdb-get-attr addr)
	(let* ((minibuffer-allow-text-properties nil)
	       (return
		(mu-cite-remove-text-properties
		 (read-string "Citation name? "
			      (or (mu-cite-get-value 'x-attribution)
				  (mu-cite-get-value 'x-cite-me)
				  (mu-cite-get-value 'full-name))
			      'mu-bbdb-history))))
	  (if (and (not (string-equal return ""))
		   (y-or-n-p (format "Register \"%s\"? " return)))
	      (mu-bbdb-set-attr return addr))
	  return))))

;;;###autoload
(defun mu-bbdb-get-prefix-register-verbose-method ()
  "A mu-cite method to return a prefix using BBDB.

In this method, a user must specify a prefix unconditionally.  If an
`attribution' value is found in BBDB, the value is used as a initial
value to input the prefix.  The prefix will be registered to BBDB if
the user wants it.

Notice that please use (mu-cite-get-value 'bbdb-prefix-register-verbose)
instead of call the function directly."
  (let* ((addr (mu-cite-get-value 'address-structure))
	 (attr (mu-bbdb-get-attr addr))
	 (minibuffer-allow-text-properties nil)
	 (return (mu-cite-remove-text-properties
		  (read-string "Citation name? "
			       (or attr
				   (mu-cite-get-value 'x-attribution)
				   (mu-cite-get-value 'x-cite-me)
				   (mu-cite-get-value 'full-name))
			       'mu-bbdb-history))))
    (if (and (not (string-equal return ""))
	     (not (string-equal return attr))
	     (y-or-n-p (format "Register \"%s\"? " return)))
	(mu-bbdb-set-attr return addr))
    return))


;;; @ end
;;;

(provide 'mu-bbdb)

(run-hooks 'mu-bbdb-load-hook)

;;; mu-bbdb.el ends here
