;;; emacsvox-indentation-tests.el --- Indentation menu tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Emacsvox contributors
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:
;; Exercise scope, previews, persistence, and real Transient interaction.

;;; Code:
(require 'ert)
(require 'emacsvox-indentation)

(defmacro emacsvox-test--with-indentation-menu (&rest body)
  "Run BODY with isolated defaults and an indentation source buffer."
  (declare (indent 0) (debug t))
  `(let* ((symbols '(emacsvox-audio-indentation emacsvox-indentation-presentation
                    emacsvox-indentation-tone-timing tts-main-tone-volume))
          (defaults (mapcar (lambda (symbol) (cons symbol (default-value symbol))) symbols))
          (properties (mapcar (lambda (symbol) (cons symbol (copy-tree (symbol-plist symbol)))) symbols))
          (native-comp-enable-subr-trampolines nil))
     (unwind-protect
         (progn
           (set-default 'emacsvox-audio-indentation nil)
           (set-default 'emacsvox-indentation-presentation 'spoken)
           (set-default 'emacsvox-indentation-tone-timing 'overlay)
           (set-default 'tts-main-tone-volume 50)
           (with-temp-buffer
             (setq-local emacsvox-indentation-presentation 'spoken)
             (setq-local emacsvox-indentation-tone-timing 'overlay)
             (let* ((source (current-buffer))
                    (transient--prefix
                     (transient-prefix :command 'emacsvox-indentation
                                       :scope (cons source nil))))
               (cl-letf (((symbol-function 'emacsvox-aural-submit) #'ignore)
                         ((symbol-function 'emacsvox-aural-submit-actions) #'ignore)
                         ((symbol-function 'tts-stop) #'ignore)
                         ((symbol-function 'tts--omnivox-program-p) (lambda () nil)))
                 ,@body))))
       (dolist (entry defaults) (set-default (car entry) (cdr entry)))
       (dolist (entry properties) (setplist (car entry) (cdr entry))))))

(ert-deftest emacsvox-indentation-menu-targets-source-buffer ()
  "Actions in another buffer still change only the captured source."
  (emacsvox-test--with-indentation-menu
    (with-temp-buffer
      (emacsvox-indentation--set 'emacsvox-indentation-presentation 'pitch-tone "Pitch tone")
      (should (eq emacsvox-indentation-presentation 'spoken))
      (should (eq (emacsvox-indentation--value 'emacsvox-indentation-presentation) 'pitch-tone)))
    (should (eq emacsvox-indentation-presentation 'pitch-tone))
    (should (eq (default-value 'emacsvox-indentation-presentation) 'spoken))
    (emacsvox-indentation--toggle)
    (should emacsvox-audio-indentation)
    (should-not (default-value 'emacsvox-audio-indentation))))

(ert-deftest emacsvox-indentation-menu-global-preserves-local-choices ()
  "Global edits affect inheriting buffers and preserve explicit local choices."
  (emacsvox-test--with-indentation-menu
    (emacsvox-indentation--scope)
    (emacsvox-indentation--set 'emacsvox-indentation-presentation 'duration-tone "Duration tone")
    (emacsvox-indentation--set 'emacsvox-indentation-tone-timing 'insert "Before speech")
    (should (eq emacsvox-indentation-presentation 'spoken))
    (should (eq emacsvox-indentation-tone-timing 'overlay))
    (with-temp-buffer
      (should (eq emacsvox-indentation-presentation 'duration-tone))
      (should (eq emacsvox-indentation-tone-timing 'insert)))
    (should (emacsvox-indentation--tonal-p))
    (emacsvox-indentation--scope)
    (should-not (emacsvox-indentation--tonal-p))))

(ert-deftest emacsvox-indentation-menu-preview-uses-displayed-scope ()
  "Global previews use defaults without altering the local presentation."
  (emacsvox-test--with-indentation-menu
    (emacsvox-indentation--scope)
    (emacsvox-indentation--set 'emacsvox-indentation-presentation 'pitch-tone "Pitch tone")
    (emacsvox-indentation--set 'emacsvox-indentation-tone-timing 'insert "Before speech")
    (let (text)
      (cl-letf (((symbol-function 'tts-speak) (lambda (value) (setq text value))))
        (with-temp-buffer (emacsvox-indentation--preview)))
      (let ((facts (get-text-property 4 emacsvox-aural-facts-property text)))
        (should (eq (plist-get facts :indentation-presentation) 'pitch-tone))
        (should (eq (plist-get facts :indentation-tone-timing) 'insert))))
    (should-not emacsvox-audio-indentation)
    (should (eq emacsvox-indentation-presentation 'spoken))
    (should (eq emacsvox-indentation-tone-timing 'overlay))))

(ert-deftest emacsvox-indentation-menu-volume-is-global-and-bounded ()
  "Volume changes use Customize in either scope and stay in the valid range."
  (emacsvox-test--with-indentation-menu
    (emacsvox-indentation--volume -5)
    (should (= (default-value 'tts-main-tone-volume) 45))
    (emacsvox-indentation--scope)
    (emacsvox-indentation--volume 5)
    (should (= (default-value 'tts-main-tone-volume) 50))
    (emacsvox-indentation--volume -100)
    (should (= (default-value 'tts-main-tone-volume) 0))
    (emacsvox-indentation--volume 200)
    (should (= (default-value 'tts-main-tone-volume) 100))
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 101)))
      (should-error (emacsvox-indentation--volume) :type 'user-error))
    (should (= (default-value 'tts-main-tone-volume) 100))))

(ert-deftest emacsvox-indentation-menu-save-does-not-promote-local-settings ()
  "Saving persists global values, never the source buffer's local overrides."
  (emacsvox-test--with-indentation-menu
    (setq-local emacsvox-indentation-presentation 'pitch-tone)
    (let (saved)
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (symbol value &rest _) (push (cons symbol value) saved))))
        (should-error (emacsvox-indentation--save) :type 'user-error)
        (should-not saved)
        (emacsvox-indentation--scope)
        (emacsvox-indentation--save))
      (should (equal (nreverse saved)
                     '((emacsvox-audio-indentation)
                       (emacsvox-indentation-presentation . spoken)
                       (emacsvox-indentation-tone-timing . overlay)
                       (tts-main-tone-volume . 50)))))
    (should (eq emacsvox-indentation-presentation 'pitch-tone))))

(ert-deftest emacsvox-indentation-menu-saved-defaults-reload ()
  "Saved settings restore through Customize without persisting local choices."
  (emacsvox-test--with-indentation-menu
    (let* ((custom-file (make-temp-file "emacsvox-indentation-custom-" nil ".el"))
           (user-init-file custom-file))
      (unwind-protect
          (progn
            (emacsvox-indentation--scope)
            (emacsvox-indentation--set 'emacsvox-indentation-presentation 'duration-tone "Duration tone")
            (emacsvox-indentation--set 'emacsvox-indentation-tone-timing 'insert "Before speech")
            (emacsvox-indentation--toggle)
            (emacsvox-indentation--volume -5)
            (emacsvox-indentation--save)
            (set-default 'emacsvox-audio-indentation nil)
            (set-default 'emacsvox-indentation-presentation 'spoken)
            (set-default 'emacsvox-indentation-tone-timing 'overlay)
            (set-default 'tts-main-tone-volume 100)
            (load custom-file nil t)
            (should (default-value 'emacsvox-audio-indentation))
            (should (eq (default-value 'emacsvox-indentation-presentation) 'duration-tone))
            (should (eq (default-value 'emacsvox-indentation-tone-timing) 'insert))
            (should (= (default-value 'tts-main-tone-volume) 45))
            (should (eq emacsvox-indentation-presentation 'spoken)))
        (delete-file custom-file)))))

(ert-deftest emacsvox-indentation-menu-closed-source-refuses-mutations ()
  "A menu whose source has died cannot change global settings."
  (emacsvox-test--with-indentation-menu
    (emacsvox-indentation--scope)
    (let ((dead (generate-new-buffer " *closed indentation source*")))
      (kill-buffer dead)
      (setcar (oref transient--prefix scope) dead))
    (should-error (emacsvox-indentation--set 'emacsvox-indentation-presentation 'pitch-tone "Pitch tone") :type 'user-error)
    (should-error (emacsvox-indentation--volume 5) :type 'user-error)
    (should-error (emacsvox-indentation--save) :type 'user-error)
    (should (eq (default-value 'emacsvox-indentation-presentation) 'spoken))
    (should (= (default-value 'tts-main-tone-volume) 50))))

(ert-deftest emacsvox-indentation-menu-graphical-shortcuts-and-scope ()
  "Real menu keys retain the source, refresh choices, and honour C-u."
  (skip-unless (display-graphic-p))
  (save-window-excursion
    (emacsvox-test--with-indentation-menu
      (switch-to-buffer source)
      (insert "source text")
      (goto-char 4)
      (local-set-key (kbd "<f8>") #'emacsvox-indentation)
      (let ((transient--prefix nil)
            (transient-show-menu 0)
            (transient-values nil)
            (transient-history nil)
            (transient-history-file nil)
            (transient-save-history nil)
            preview)
        (cl-letf (((symbol-function 'tts-speak) (lambda (text) (setq preview text))))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "<f8>"))
                (transient--show)
                (should-not (cdr (emacsvox-indentation--state)))
                (execute-kbd-macro (kbd "p b - SPC"))
                (should transient--prefix)
                (should (eq (buffer-local-value 'emacsvox-indentation-presentation source) 'pitch-tone))
                (should (eq (buffer-local-value 'emacsvox-indentation-tone-timing source) 'insert))
                (should (= (default-value 'tts-main-tone-volume) 45))
                (should preview)
                (with-current-buffer (window-buffer transient--window)
                  (should (string-match-p "Pitch tone (selected)" (buffer-string)))
                  (should (string-match-p "Before speech (selected)" (buffer-string))))
                (execute-kbd-macro (kbd "s"))
                (should (oref (seq-find (lambda (suffix) (equal (oref suffix key) "b")) transient--suffixes) inapt))
                (execute-kbd-macro (kbd "<down> <down> <up> q"))
                (should-not transient--prefix)
                (should (eq (current-buffer) source))
                (should (= (point) 4))
                (execute-kbd-macro (kbd "C-u <f8>"))
                (should (cdr (emacsvox-indentation--state)))
                (execute-kbd-macro (kbd "d a q"))
                (should (eq (default-value 'emacsvox-indentation-presentation) 'duration-tone))
                (should (eq (buffer-local-value 'emacsvox-indentation-presentation source) 'spoken))
                (should (= (point) 4)))
            (when transient--prefix (execute-kbd-macro (kbd "C-g")))))))))

(provide 'emacsvox-indentation-tests)
;;; emacsvox-indentation-tests.el ends here
