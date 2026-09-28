;;; zulip-transient-test.el --- Tests for Zulip transient menus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-translate)
(require 'zulip-feed-test)
(require 'zulip-transient)

(defmacro zulip-transient-test--with-feed (feed-narrow &rest body)
  "Run BODY in a real FEED-NARROW with captured HTTP transport requests."
  (declare (indent 1) (debug (form body)))
  `(save-window-excursion
     (let ((transient-save-history nil)
           (transient-history nil)
           requests processes)
       (unwind-protect
           (zulip-feed-test--with-account account
             (cl-letf (((symbol-function 'plz)
                        (lambda (method url &rest arguments)
                          (let ((process
                                 (make-pipe-process
                                  :name "zulip-transient-test" :noquery t)))
                            (push process processes)
                            (push (list :method method :url url
                                        :arguments arguments :process process)
                                  requests)
                            process))))
               (let* ((narrow ,feed-narrow)
                      (buffer (zulip-feed--open-buffer account narrow)))
                 (unwind-protect
                     (with-current-buffer buffer
                       (switch-to-buffer buffer)
                       (appkit-chat-history-window-establish-empty)
                       (zulip-feed-render)
                       ,@body)
                   (when (transient-active-prefix)
                     (execute-kbd-macro (kbd "C-q")))))))
         (dolist (process processes)
           (when (process-live-p process) (delete-process process)))))))

(defun zulip-transient-test--send (account content)
  "Send CONTENT from this feed and return the projected pending ID."
  (appkit-chatbuf-input-set-text content)
  (let ((id (zulip-feed-send-message)))
    (zulip-runtime-test--drain account)
    (goto-char (appkit-chat-timeline-key-position id))
    id))

(defun zulip-transient-test--respond (request body)
  "Deliver JSON BODY through REQUEST's real HTTP completion boundary."
  (let ((process (plist-get request :process)))
    (when (process-live-p process) (delete-process process)))
  (funcall (plist-get (plist-get request :arguments) :then)
           (make-plz-response
            :version 2 :status 200
            :headers '((content-type . "application/json"))
            :body body)))

(defun zulip-transient-test--request-body (request)
  "Return REQUEST's decoded HTTP form body."
  (url-unhex-string (plist-get (plist-get request :arguments) :body)))

(ert-deftest zulip-transient-local-content-works-without-server-actions ()
  (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
    (let* ((local-id (zulip-transient-test--send account "Unsent local text"))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           sent-text
           (backend
            (list :id 'local-menu-test :label "Local menu test"
                  :start (lambda (source _language resolve _reject)
                           (setq sent-text (plist-get source :text))
                           (funcall resolve "Translated local text")
                           nil))))
      (appkit-chatbuf-input-set-text "Next unsent draft")
      (goto-char (appkit-chat-timeline-key-position local-id))
      (zulip-transient-msg-operate)
      ;; Inapt keys must stay in the menu, without entering prompts or APIs.
      (execute-kbd-macro (kbd "r u s e d !"))
      (should (transient-active-prefix 'zulip-transient-msg-operate))
      (should (= (length requests) 1))
      (execute-kbd-macro (kbd "c"))
      (should (equal (current-kill 0 t) "Unsent local text"))
      (should-not (transient-active-prefix))
      (should-not zulip-transient--unsubscribe)
      (zulip-transient-msg-operate)
      (cl-letf (((symbol-function 'appkit-translate-respond-backend)
                 (lambda () backend)))
        (execute-kbd-macro (kbd "t"))
        (zulip-runtime-test--drain account))
      (should (equal sent-text "Unsent local text"))
      (should (save-excursion
                (goto-char (point-min))
                (search-forward "Translated local text" nil t)))
      (should (equal (appkit-chatbuf-input-string) "Next unsent draft"))
      (should (equal (zulip-feed--field
                      (zulip-state-message (zulip-account-state account) local-id)
                      'local-content)
                     "Unsent local text"))
      (should (= (length requests) 1))
      (should-not zulip-transient--unsubscribe))))

(ert-deftest zulip-transient-first-retry-key-after-failure-and-server-promotion ()
  (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
    (let* ((local-id (zulip-transient-test--send account "Retry this message"))
           (original-request (car requests))
           (server-id "90071992547409931234"))
      (zulip-transient-msg-operate)
      ;; No command runs between the pending menu and the projected failure.
      (zulip-transient-test--respond
       original-request "{\"result\":\"error\",\"msg\":\"temporary failure\"}")
      (zulip-runtime-test--drain account)
      (should (save-excursion
                (goto-char (point-min))
                (search-forward "Send failed: temporary failure" nil t)))
      (execute-kbd-macro (kbd "R"))
      (zulip-runtime-test--drain account)
      (should (= (length requests) 2))
      (should (equal (zulip-transient-test--request-body (car requests))
                     (zulip-transient-test--request-body original-request)))
      (should (string-match-p
               (regexp-quote (concat "local_id=" local-id))
               (zulip-transient-test--request-body (car requests))))
      (let ((row (zulip-state-message (zulip-account-state account) local-id)))
        (should (zulip-feed--field row 'pending))
        (should-not (zulip-feed--field row 'failed)))
      (should-not (transient-active-prefix))
      (should-not zulip-transient--unsubscribe)
      ;; A menu opened during retry must follow the canonical row's rekey.
      (goto-char (appkit-chat-timeline-key-position local-id))
      (zulip-transient-msg-operate)
      (zulip-transient-test--respond
       (car requests)
       (format "{\"result\":\"success\",\"id\":%s}" server-id))
      (zulip-runtime-test--drain account)
      (should-not (zulip-state-message (zulip-account-state account) local-id))
      (should (equal (appkit-chat-timeline-keys) (list server-id)))
      (should (equal (zulip-feed-message-id-at-point) server-id))
      (execute-kbd-macro (kbd "s"))
      (zulip-runtime-test--drain account)
      (should (= (length requests) 3))
      (should (eq (plist-get (car requests) :method) 'post))
      (should (string-suffix-p "/api/v1/messages/flags"
                               (plist-get (car requests) :url)))
      (should (equal (zulip-transient-test--request-body (car requests))
                     (concat "messages=[" server-id "]&op=add&flag=starred")))
      (should-not (transient-active-prefix))
      (should-not zulip-transient--unsubscribe)
      (should-not zulip-feed-view-change-hook))))

(ert-deftest zulip-transient-exit-releases-refresh-before-late-failure ()
  (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
    (let ((local-id (zulip-transient-test--send account "Exit before response")))
      (zulip-transient-msg-operate)
      (execute-kbd-macro (kbd "C-q"))
      (should-not zulip-transient--unsubscribe)
      (should-not zulip-feed-view-change-hook)
      (zulip-transient-test--respond
       (car requests) "{\"result\":\"error\",\"msg\":\"late failure\"}")
      (zulip-runtime-test--drain account)
      (should (equal (zulip-feed--field
                      (zulip-state-message (zulip-account-state account) local-id)
                      'failed)
                     "late failure"))
      (should-not (transient-active-prefix))
      (should-not zulip-transient--unsubscribe))))

(ert-deftest zulip-transient-retired-source-cannot-reacquire-refresh ()
  (dolist (retirement '(surface account buffer))
    (ert-info ((format "Retirement: %S" retirement))
      (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
        (let* ((local-id (zulip-transient-test--send account "Retired send"))
               (surface (appkit-current-surface))
               prefix)
          (zulip-transient-msg-operate)
          (setq prefix (transient-active-prefix))
          (pcase retirement
            ('surface
             (appkit-surface-stop surface)
             (should-not zulip-transient--unsubscribe)
             ;; Same buffer and narrow, but a different Surface authority.
             (should (eq buffer (zulip-feed--open-buffer account narrow)))
             (should-not (eq surface (appkit-current-surface)))
             (appkit-chat-history-window-set local-id nil)
             (zulip-feed-render))
            ('account (zulip-runtime-stop-account account))
            ('buffer (kill-buffer buffer)))
          (zulip-transient-test--respond
           (car requests) "{\"result\":\"error\",\"msg\":\"retired failure\"}")
          (zulip-runtime-test--drain account)
          (should-not (appkit-surface-live-p surface))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (should-not zulip-transient--unsubscribe)
              (should-not zulip-feed-view-change-hook)
              (when (eq retirement 'surface)
                (should (equal
                         (zulip-feed--field
                          (zulip-state-message
                           (zulip-account-state account) local-id)
                          'failed)
                         "retired failure")))))
          ;; A late projection may not replace the retired menu's activation.
          (when (transient-active-prefix)
            (should (eq (transient-active-prefix) prefix)))
          (should (= (length requests) 1)))))))

(transient-define-prefix zulip-transient-test--other-menu ()
  "Unrelated native menu used to exercise refresh ownership."
  [("x" "Finish" ignore)])

(ert-deftest zulip-transient-late-projection-leaves-other-prefix-alone ()
  (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
    (zulip-transient-test--send account "Other menu owns input")
    (zulip-transient-msg-operate)
    (zulip-transient-test--other-menu)
    (let ((prefix (transient-active-prefix)))
      (zulip-transient-test--respond
       (car requests) "{\"result\":\"error\",\"msg\":\"other menu failure\"}")
      (zulip-runtime-test--drain account)
      (should (eq (transient-active-prefix) prefix))
      (should-not zulip-transient--unsubscribe)
      (should-not zulip-feed-view-change-hook)
      (execute-kbd-macro (kbd "x"))
      (should-not (transient-active-prefix)))))

(ert-deftest zulip-transient-returning-to-source-does-not-resurrect-subscription ()
  (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
    (zulip-transient-test--send account "Leave this feed")
    (let ((elsewhere (generate-new-buffer " *zulip-menu-elsewhere*")))
      (unwind-protect
          (progn
            (zulip-transient-msg-operate)
            (switch-to-buffer elsewhere)
            (execute-kbd-macro (kbd "r"))
            (with-current-buffer buffer
              (should-not zulip-transient--unsubscribe))
            (switch-to-buffer buffer)
            ;; Native suffix refresh on return must not resubscribe.
            (execute-kbd-macro (kbd "r"))
            (should-not zulip-transient--unsubscribe)
            (let ((prefix (transient-active-prefix)))
              (zulip-transient-test--respond
               (car requests) "{\"result\":\"error\",\"msg\":\"returned failure\"}")
              (zulip-runtime-test--drain account)
              (should (eq (transient-active-prefix) prefix))
              (should-not zulip-feed-view-change-hook))
            (should (= (length requests) 1)))
        (kill-buffer elsewhere)))))

(ert-deftest zulip-transient-local-context-and-topic-follow-feed-semantics ()
  (zulip-transient-test--with-feed (zulip-narrow-topic 7 "client")
    (zulip-transient-test--send account "Local topic context")
    (zulip-transient-msg-operate)
    (execute-kbd-macro (kbd "o"))
    (should-not (transient-active-prefix))
    (set-buffer (window-buffer (selected-window)))
    (should (equal (format "%s" (zulip-narrow-channel-operand zulip-feed--narrow))
                   "7"))
    (should (equal (zulip-narrow-topic-name zulip-feed--narrow) "client"))
    ;; Topic opening is also valid from the composer, with no row at point.
    (goto-char (appkit-chatbuf-input-start-position))
    (zulip-transient-msg-operate)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional initial &rest _arguments)
                 (should (equal initial "client"))
                 "next topic")))
      (execute-kbd-macro (kbd "T")))
    (set-buffer (window-buffer (selected-window)))
    (should (equal (format "%s" (zulip-narrow-channel-operand zulip-feed--narrow))
                   "7"))
    (should (equal (zulip-narrow-topic-name zulip-feed--narrow) "next topic"))
    (should (= (length requests) 1))))

(ert-deftest zulip-transient-local-direct-context-preserves-participants ()
  (zulip-transient-test--with-feed (zulip-narrow-direct '(2 3))
    (zulip-transient-test--send account "Local group context")
    (zulip-transient-msg-operate)
    (execute-kbd-macro (kbd "o"))
    (should-not (transient-active-prefix))
    (with-current-buffer (window-buffer (selected-window))
      (should (equal (zulip-narrow-recipient-ids zulip-feed--narrow) '(2 3))))
    (should (= (length requests) 1))))

(ert-deftest zulip-transient-context-requires-kind-and-direct-participants ()
  (dolist (type '("private" "unknown"))
    (ert-info ((format "Incomplete local context: %s" type))
      (zulip-transient-test--with-feed (zulip-narrow-all)
        (zulip-runtime-publish-state
         account
         (zulip-state-upsert-message
          (zulip-account-state account)
          `((id . "local-context") (type . ,type)
            (content . "Local text without context") (pending . t))))
        (appkit-chat-history-window-set "local-context" nil)
        (zulip-feed-render)
        (goto-char (appkit-chat-timeline-key-position "local-context"))
        (zulip-transient-msg-operate)
        (execute-kbd-macro (kbd "o"))
        (should (transient-active-prefix 'zulip-transient-msg-operate))
        (should (eq (window-buffer (selected-window)) buffer))
        (should-not requests)))))

(provide 'zulip-transient-test)

;;; zulip-transient-test.el ends here
