;;; zulip-transient.el --- Transient menus for emacs-zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; Discoverable, message-at-point actions for Zulip feeds.  Timeline
;; single-key bindings remain available for frequently used navigation, while
;; this menu gathers less frequent state-changing operations in one place.

;;; Code:

(require 'transient)
(require 'zulip-feed)
(require 'zulip-state)

(defvar-local zulip-transient--unsubscribe nil
  "Cleanup function for this feed's active message-menu subscription.")

(defun zulip-transient--watch-view (prefix surface)
  "Refresh PREFIX after SURFACE is projected, while this menu owns the view."
  (when zulip-transient--unsubscribe
    (funcall zulip-transient--unsubscribe))
  (let ((buffer (current-buffer))
        (active t)
        cleanup check update)
    (setq cleanup
          (lambda ()
            (setq active nil)
            (remove-hook 'transient-exit-hook cleanup)
            (remove-hook 'post-command-hook check)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (remove-hook 'zulip-feed-view-change-hook update t)
                (remove-hook 'kill-buffer-hook cleanup t)
                (remove-hook 'change-major-mode-hook cleanup t)
                (when (eq zulip-transient--unsubscribe cleanup)
                  (setq zulip-transient--unsubscribe nil))))))
    (setq check
          (lambda ()
            (when active
              (unless (and (eq (transient-active-prefix
                                'zulip-transient-msg-operate)
                               prefix)
                           (eq (window-buffer (selected-window)) buffer)
                           (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (zulip-feed--captured-view-current-p surface)))
                (funcall cleanup)))))
    (setq update
          (lambda ()
            (funcall check)
            (when active
              ;; Rebuild both the native keymap and display before the next
              ;; key is read.  `transient-update' alone only takes effect in
              ;; the command loop, too late for a formerly inapt first key.
              (zulip-transient--setup-message-menu surface))))
    (setq zulip-transient--unsubscribe cleanup)
    (add-hook 'zulip-feed-view-change-hook update nil t)
    (add-hook 'kill-buffer-hook cleanup nil t)
    (add-hook 'change-major-mode-hook cleanup nil t)
    (add-hook 'transient-exit-hook cleanup)
    (add-hook 'post-command-hook check 90)))

(defun zulip-transient--setup-message-menu (surface)
  "Activate a message menu and its refresh subscription for exact SURFACE."
  (transient-setup 'zulip-transient-msg-operate nil nil :scope surface)
  ;; Only explicit activation and a still-owned projection may subscribe.
  ;; An environment function also runs on redisplay, which could otherwise
  ;; resurrect a subscription revoked after leaving the source buffer.
  (when (zulip-feed--captured-view-current-p surface)
    (zulip-transient--watch-view (transient-prefix-object) surface)))

(defun zulip-transient--message-at-point ()
  "Return the canonical Zulip message at point without signaling."
  (ignore-errors (zulip-feed-message-at-point)))

(defun zulip-transient--content-inapt-reason ()
  "Return why local-content message actions are unavailable at point."
  (let ((message (zulip-transient--message-at-point)))
    (cond
     ((null message) "No Zulip message at point")
     ((not (or (stringp (zulip-feed--field message 'local-content))
               (stringp (zulip-feed--field message 'rendered-content))
               (stringp (zulip-feed--field message 'content))))
      "Message has no text content"))))

(defun zulip-transient--context-inapt-reason ()
  "Return why the current message has no openable conversation."
  (let ((message (zulip-transient--message-at-point)))
    (cond
     ((null message) "No Zulip message at point")
     ((eq (zulip-feed--message-kind message) 'channel) nil)
     ((eq (zulip-feed--message-kind message) 'direct)
      (unless (zulip-feed--message-direct-recipients message)
        "Direct-message participants are unavailable"))
     (t "Message has no openable Zulip context"))))

(defun zulip-transient--server-message-inapt-reason ()
  "Return why server-backed message actions are unavailable at point.

The return value is nil for an authoritative server message and a human
readable reason otherwise.  In particular, optimistic `local-*' rows must not
escape into APIs that require a server message ID."
  (let ((message (zulip-transient--message-at-point)))
    (cond
     ((null message) "No Zulip message at point")
     ((not (zulip-state-server-message-id-p
            (ignore-errors (zulip-state-message-id message))))
      "Message is still local; wait for server acknowledgement")
     (t nil))))

(defun zulip-transient--retry-inapt-reason ()
  "Return why retrying a failed local message is unavailable at point."
  (let ((message (zulip-transient--message-at-point)))
    (cond
     ((null message) "No Zulip message at point")
     ((zulip-state-server-message-id-p
       (ignore-errors (zulip-state-message-id message)))
      "Server messages do not need send retry")
     ((not (zulip-state-object-get message 'failed))
      "Local message has not failed")
     (t nil))))

;; Magit-style autoload: a bare `;;;###autoload' above a
;; `transient-define-prefix' form would copy the whole form into loaddefs,
;; before `transient' itself is loaded.
;;;###autoload(autoload 'zulip-transient-msg-operate "zulip" nil t)
(transient-define-prefix zulip-transient-msg-operate ()
  "Message actions for the Zulip feed message at point."
  :refresh-suffixes t
  [["Message"
    ("o" "Open context" zulip-feed-open-message-context
     :inapt-if zulip-transient--context-inapt-reason)
    ("T" "Open topic" zulip-feed-open-topic)
    ("t" "Translate" zulip-feed-translate-message
     :inapt-if zulip-transient--content-inapt-reason)
    ("c" "Copy text" zulip-feed-copy-message
     :inapt-if zulip-transient--content-inapt-reason)]
   ["Status"
    ("r" "Mark read" zulip-feed-mark-read
     :inapt-if zulip-transient--server-message-inapt-reason)
    ("u" "Mark unread" zulip-feed-mark-unread
     :inapt-if zulip-transient--server-message-inapt-reason)
    ("s" "Toggle starred" zulip-feed-toggle-star
     :inapt-if zulip-transient--server-message-inapt-reason)]
   ["Modify"
    ("R" "Retry failed send" zulip-feed-retry-send
     :inapt-if zulip-transient--retry-inapt-reason)
    ("e" "Edit" zulip-feed-edit-message
     :inapt-if zulip-transient--server-message-inapt-reason)
    ("d" "Delete" zulip-feed-delete-message
     :inapt-if zulip-transient--server-message-inapt-reason)
    ("!" "Toggle reaction" zulip-feed-toggle-reaction
     :inapt-if zulip-transient--server-message-inapt-reason)]]
  (interactive)
  (zulip-feed--assert-live)
  (zulip-transient--setup-message-menu (appkit-current-surface)))

(provide 'zulip-transient)

;;; zulip-transient.el ends here
