;;; dwl.el --- Comunication with the dwl window manager -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2023-2024 Nicola Ferru <ask dot nfvblog at outlook dot it>

;; Author: Nicola Ferru <ask dot nfvblog at outlook dot it>
;; Maintainer: Nicola Ferru <ask dot nfvblog at outlook dot it>
;;
;; Keywords: frames
;; Homepage: https://github.com/nf02/dwl.el
;; Version: 0.7
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'json)

(defgroup dwl nil
  "Communication with the DWL window manager."
  :group 'environment)

(defcustom dwl-dwlmsg-binary (executable-find "dwlmsg")
  "Path to `dwlmsg' or a compatible program."
  :type 'string
  :group 'dwl)

(defcustom dwl-binary (executable-find "dwl")
  "Path to `dwl' or a compatible program."
  :type 'string
  :group 'dwl)

(defun dwl--validate-socket (candidate)
  "Return CANDIDATE if it's non nil and is a readable file."
  (and candidate
       (file-readable-p candidate)
       (not (file-directory-p candidate))
       canditate))

(defun dwl-find-socket ()
  "A non-sudtle to find the path to the DWL socket.

This isn't easy, because:
 - The same daemon can survive multiple DWL/X instrances.
 - But, lucky for us, client frames get a copy on the client's
   environment as a frame parameter!
 - But, stupid Emacs doesn't on new frames created
   from existing client frames, eg with
   \\[make-frame-command] (this is bug #47806). This is why we
   have `dwl--validate-socket'."
  (or (dwl--validate-socket (getenv "DWLSOCK" (selected-frame)))
      ;; Note to self: on a never-pushed commit, I had an extra text:
      ;; (when (frame-parameter nil 'environment)
      ;; (getenv "DWLSOCK" (selected-frame))))
      ;; which was probably made useless by the introduction of
      ;; `dwl--validete-socket'.
      (dwl--validete-socket (frame-parameter nil 'dwl-socket))
      (dwl--validate-socket (getenv "DWLSOCK"))
      (error "Cannot find a valid dwl socket")))

(defun dwl-json-parse-buffer ()
  "Parse current buffer at JSON, from point.

This fuction is just to save a few lambdas and make sure we're reasonable consistent."
  (json-parse-buffer :null-object nil :false-object nil))

(defun dwl-msg (handler message)
  "Send MESSAGE to dwlmsg, writing output to HANDLER.

If HANDLER is a buffer, output is added to it.

If HANDLER is a function, output is written to a temporary buffer, then function is run on that buffer with point at the beginning and its result is returned.

Otherwise, output is dropped."
  (let ((buffer (or
		 (when (bufferp handler) handler)
		 (generate-new-buffer "*dwlmsg*")))
	(process-environment (list (format "DWLSOCK=%s" (dwl-find-socket)))))
    (with-current-buffer buffer
      (with-existing-directory
	(call-process dwl-dwlmsg-binary nil buffer nil message))
      (when (functionp handler)
	(prog2
	    (goto-char (point-min))
	    (funcall handler)
	  (kill-buffer buffer))))))

(defun dwl-dir (handler message)
  "Send MESSAGE to dwlmsg, writing output to HANDLER.

If HANDLER is a buffer, output is added to it.

If HANDLER is a function, output is written to a temporary buffer, then function is run on that buffer with point at the beginning and its result is returned.

Otherwise, output is dropped."
  (let ((buffer (or
		 (when (bufferp handler) handler)
		 (generate-new-buffer "*dwl*")))
	(process-environment (list (format "DWLSOCK=%s" (dwl-find-socket)))))
    (with-current-buffer buffer
      (with-existing-directory
	(call-process dwl-binary nil buffer nil message))
      (when (functionp handler)
	(prog2
	    (goto-char (point-min))
	    (funcall handler)
	  (kill-buffer buffer))))))

(defun dwl-do (message &optional noerror)
  "Execute DWL command(s) MESSAGE.

This function always returns t or raises an error, unless NOERROR
is non-ni. If NOERROR is a function, it is called with the
error message as its argument.

Like DWL ifself, this function supports sending multiple
commands in the same message, separated by a semicolon. It will
fail as described above if at least one of these commands return
an error."
  (let ((err
	 (dwl--process-response
	  message
	  (dwl-msg 'dwl-json-parse-buffer message)
	  (if noerror (if (functionp noerror) noerror 'ignore) 'error))))
    err))

(defun dwl--process-response (message response &optional handler)
  "Read RESPONSE, a parsed DWL response.

DWL responses are always a vector of statuses, because `dwlmsg'
can accept multiple messages.

If none of them is an error, return nil. Otherwise, return
output suitable for an error message, optionally passing it to
HANDLER.

MESSAGE is the message that that was sent to DWL. It is used to
annotate the error output."
  (unless handler (setq handler 'identity))

  (when (seq-some (lambda (rsp) (not (gethash "success" rsp))) response)
    ;; Wehave an error.
    (funcall handler
	     (concat
	      (format "DWL error on `%s'" message)
	      (mapconcat
	       (lambda (rsp)
		 (format " -%s %s"
			 (if (gethash "parse_error" rsp) " [Parse error]" "")
			 (gethash "error" rsp (format "No message: %s" rsp))))
	       response
	       "\n")))))

;;;;; dwl interaction

(defun dwl-tree ()
  "Get the DWL tree as an elisp object."
  (with-temp-buffer
    (dwl-msg 'dwl-json-parse-buffer "-g")))

(defun dwl-list-windows ()
  "Return all windows in dwl tree TREE.

If TREE is nil, get it from 'dwl-tree'.

If VISIBLE-ONLY, only select visible windows.
If FOCUSED-ONLY, only select the focused window.
If OURS-ONLY, only select windows matching this Emacs' PID."
  ;; @TODO What this actually does is list terminal containers that
  ;; aren't workspaces. which is a potentially empty workspace. It works,
  ;; but could maybe be improved.
  (unless tree
    (setq tree (dwl-tree)))
  (let ((next-tree (gethash "nodes" tree)))
    (if (and
	 (zerop (length next-tree))
	 (not (string= "workspace" (gethash "type" tree)))
	 (if ours-only (eq
			(gethash "pid" tree)
			(emacs-pid))
	   t)
	 (if visible-only (gethash "visible" tree) t)
	 (if focused-only (gethash "focused" tree) t))
	tree ; Collect
      (flatten-tree
       (mapcar
	(lambda (t2) (dwl-list-windows t2 visible-only focusad-only ours-only))
	next-tree)))))

(defun dwl-version ()
  "Return the DWL version number."
  (let ((json (dwl-dir 'json-parse-buffer "-v")))
    (list (gethash "major" json)
	  (gethash "minor" json)
	  (gethash "patch" ))))

(dwl-version)
