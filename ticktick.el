(require 'oauth2)
(require 'json)
(require 'url)
(require 'seq)
(require 'tabulated-list)
(require 'org)

(defgroup ticktick nil
  "Interface with TickTick API."
  :prefix "ticktick-"
  :group 'applications)

(defcustom ticktick-client-id ""
  "TickTick client ID."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-client-secret ""
  "TickTick client secret."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-auth-scopes "tasks:write tasks:read"
  "Space-separated scopes for TickTick API access."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-dir
  (concat user-emacs-directory "ticktick/")
  "Folder in which to save token."
  :group 'ticktick
  :type 'string)

(defcustom ticktick-token-file
  (expand-file-name ".ticktick-token" ticktick-dir)
  "File in which to store TickTick OAuth token."
  :type 'file
  :group 'ticktick)

(defcustom ticktick-sync-file (expand-file-name "ticktick.org" ticktick-dir)
  "Path to the org file where all TickTick tasks will be synchronized."
  :type 'file
  :group 'ticktick)

(defcustom ticktick--autosync nil
  "If non-nil, automatically sync TickTick when switching buffers or Emacs loses focus."
  :type 'boolean
  :group 'ticktick)

(defvar ticktick-token nil
  "Access token for accessing the TickTick API.")

(defvar ticktick-redirect-uri "http://localhost"
  "The redirect URI registered with TickTick.")

;;; Authorization functions
;;;###autoload
(defun ticktick-authorize ()
  "Authorize tick.el with TickTick and obtain an access token."
  (interactive)
  (let* ((state (format "%06x" (random (expt 16 6))))
         (auth-url (concat "https://ticktick.com/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,ticktick-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,ticktick-redirect-uri)
                              ("scope" ,ticktick-auth-scopes)
                              ("state" ,state)))))
         (authorization (concat "Basic "
                                (base64-encode-string
                                 (concat ticktick-client-id ":" ticktick-client-secret)
                                 t)))
         code token-response)
    (browse-url auth-url)
    (message "Please authorize the application in your browser and enter the authorization code (after '?code=').")
    (setq code (read-string "Enter authorization code: "))
    (setq token-response
          (ticktick--exchange-code-for-token code authorization))
    (if token-response
        (progn
          (setq ticktick-token token-response)
          (message "Authorization successful!"))
      (message "Failed to obtain access token."))))

(defun ticktick--exchange-code-for-token (code authorization)
  "Exchange the authorization code for an access token using AUTHORIZATION header."
  (let* ((token-url "https://ticktick.com/oauth/token")
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,authorization)
                                      ("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (mapconcat
                            (lambda (kv)
                              (concat (url-hexify-string (car kv)) "=" (url-hexify-string (cdr kv))))
                            `(("grant_type" . "authorization_code")
                              ("code" . ,code)
                              ("redirect_uri" . ,ticktick-redirect-uri)
                              ("scope" . ,ticktick-auth-scopes))
                            "&"))
         response token-data)
    (with-current-buffer (url-retrieve-synchronously token-url t t)
      (goto-char url-http-end-of-headers)
      (setq response (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    (let ((json-object-type 'plist))
      (setq token-data (json-read-from-string response)))
    (if (plist-get token-data :access_token)
        (progn
          (plist-put token-data :created_at (float-time))
          token-data)
      nil)))

;;;###autoload
(defun ticktick-refresh-token ()
  "Refresh the OAuth2 token."
  (interactive)
  (when ticktick-token
    (let* ((token-url "https://ticktick.com/oauth/token")
           (authorization (concat "Basic "
                                  (base64-encode-string
                                   (concat ticktick-client-id ":" ticktick-client-secret)
                                   t)))
           (url-request-method "POST")
           (url-request-extra-headers `(("Authorization" . ,authorization)
                                        ("Content-Type" . "application/x-www-form-urlencoded")))
           (refresh-token (plist-get ticktick-token :refresh_token))
           (url-request-data (mapconcat
                              (lambda (kv)
                                (concat (url-hexify-string (car kv)) "=" (url-hexify-string (cdr kv))))
                              `(("grant_type" . "refresh_token")
                                ("refresh_token" . ,refresh-token)
                                ("redirect_uri" . ,ticktick-redirect-uri)
                                ("scope" . ,ticktick-auth-scopes))
                              "&"))
           response token-data)
      (with-current-buffer (url-retrieve-synchronously token-url t t)
        (goto-char url-http-end-of-headers)
        (setq response (buffer-substring-no-properties (point) (point-max)))
        (kill-buffer (current-buffer)))
      (let ((json-object-type 'plist))
        (setq token-data (json-read-from-string response)))
      (if (plist-get token-data :access_token)
          (progn
            (plist-put token-data :created_at (float-time))
            (setq ticktick-token token-data)
            (message "Token refreshed!"))
        (message "Failed to refresh token.")))))

(defun ticktick-ensure-token ()
  "Ensure we have a valid access token."
  (unless (and ticktick-token
               (plist-get ticktick-token :access_token)
               (not (ticktick-token-expired-p ticktick-token)))
    (ticktick-refresh-token)))

(defun ticktick-token-expired-p (token)
  "Check if token has expired."
  (let ((expires-in (plist-get token :expires_in))
        (created-at (plist-get token :created_at)))
    (if (and expires-in created-at)
        (> (float-time) (+ created-at expires-in -30))
      nil)))

(defun ticktick-request (method endpoint &optional data)
  "Send a request to the TickTick API.
METHOD is the HTTP method as a string.
ENDPOINT is the API endpoint.
DATA is an alist of data to send with the request."
  (ticktick-ensure-token)
  (condition-case err
      (let* ((url-request-method method)
             (url (concat "https://api.ticktick.com" endpoint))
             (access-token (plist-get ticktick-token :access_token))
             (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " access-token))
                                           ("Content-Type" . "application/json")))
             (url-request-data (and data (encode-coding-string (json-encode data) 'utf-8)))
             response status)
        (with-current-buffer (url-retrieve-synchronously url t t)
          (goto-char url-http-end-of-headers)
          (setq response (buffer-substring-no-properties (point) (point-max)))
          (setq status url-http-response-status)
          (kill-buffer (current-buffer)))
        (cond
         ((and (>= status 200) (< status 300))
          (if (string-blank-p response)
              nil
            (let ((json-object-type 'plist)
                  (json-array-type 'list))
              (json-read-from-string response))))
         ((= status 401)
          (ticktick-refresh-token)
          (ticktick-request method endpoint data))
         (t
          (error "HTTP Error %s: %s" status response))))
    (error
     (message "Request failed: %s" (error-message-string err))
     nil)))

(defun ticktick--task-to-heading (task)
  "Convert TASK to org heading."
  (let ((id (plist-get task :id))
        (title (plist-get task :title))
        (status (plist-get task :status))
        (priority (plist-get task :priority))
        (due (plist-get task :dueDate))
        (etag (plist-get task :etag))
        (content (plist-get task :content)))
    (string-join
     (delq nil
           (list
            (format "** %s%s %s"
                    (if (= status 2) "DONE" "TODO")
                    (pcase priority (5 " [#A]") (3 " [#B]") (1 " [#C]") (_ ""))
                    title)
            (when due
              (format "DEADLINE: <%s>" (format-time-string "%Y-%m-%d %a" (date-to-time due))))
            ":PROPERTIES:"
            (format ":TICKTICK_ID: %s" id)
            (format ":TICKTICK_ETAG: %s" (or etag ""))
            ":END:"
            (when content (string-trim content))
            ))
     "\n")))

(defun ticktick--heading-to-task ()
  "Convert org heading at point to a TickTick task plist."
  (let* ((el (org-element-at-point))
         (title (org-element-property :title el))
         (todo (org-element-property :todo-type el))
         (priority (org-element-property :priority el))
         (deadline (org-element-property :deadline el))
         (id (org-entry-get nil "TICKTICK_ID"))
         (content
          (save-excursion
            (save-restriction
              (org-narrow-to-subtree)
              (goto-char (point-min))
              ;; Skip heading
              (forward-line)
              ;; Skip planning lines (e.g. DEADLINE)
              (while (looking-at org-planning-line-re)
                (forward-line))
              ;; Skip properties drawer
              (when (looking-at ":PROPERTIES:")
                (re-search-forward "^:END:" nil t)
                (forward-line))
              ;; Capture content
              (string-trim (buffer-substring-no-properties (point) (point-max)))))))
    `(("id" . ,id)
      ("title" . ,title)
      ("status" . ,(if (eq todo 'done) 2 0))
      ("priority" . ,(pcase priority (?A 5) (?B 3) (?C 1) (_ 0)))
      ("dueDate" . ,(when deadline
                      (format-time-string "%Y-%m-%dT%H:%M:%S+0000"
                                          (org-timestamp-to-time deadline))))
      ("content" . ,content))))


(defun ticktick--should-sync-p ()
  "Should we sync task?"
  (let* ((etag (org-entry-get nil "TICKTICK_ETAG"))
         (cached (org-entry-get nil "SYNC_CACHE"))
         (body (buffer-substring-no-properties
                (org-entry-beginning-position)
                (org-entry-end-position)))
         (changed (not (string= cached (secure-hash 'sha1 body)))))
    (cond
     ((not etag) t)
     ((and changed) t)
     (t nil))))

(defun ticktick--update-sync-meta ()
  "Set sync hash and time."
  (let ((body (buffer-substring-no-properties (org-entry-beginning-position)
                                              (org-entry-end-position))))
    (org-set-property "LAST_SYNCED" (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
    (org-set-property "SYNC_CACHE" (secure-hash 'sha1 body))))

;;; Sync functions
(defun ticktick--find-task-under-project (project-heading id)
  "Return position of task heading with ID under PROJECT-HEADING."
  (save-excursion
    (goto-char project-heading)
    (catch 'found
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get nil "TICKTICK_ID") id)
           (throw 'found (point))))
       nil 'tree)
      nil)))

(defun ticktick-fetch-to-org ()
  "Fetch all tasks from TickTick and update org file without duplicating."
  (interactive)
  (let ((projects (ticktick-request "GET" "/open/v1/project")))
    (with-current-buffer (find-file-noselect ticktick-sync-file)
      (org-with-wide-buffer
       (dolist (project projects)
         (let* ((project-id (plist-get project :id))
                (project-name (plist-get project :name))
                (project-heading-re (format "^\\* %s$" (regexp-quote project-name)))
                (project-pos (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward project-heading-re nil t)
                                 (match-beginning 0)))))
           ;; Create project section if not found
           (unless project-pos
             (goto-char (point-max))
             (insert (format "* %s\n:PROPERTIES:\n:TICKTICK_PROJECT_ID: %s\n:END:\n" project-name project-id))
             (setq project-pos (point-at-bol)))
           (goto-char project-pos)
           (outline-show-subtree)
           (let* ((project-data (ticktick-request "GET" (format "/open/v1/project/%s/data" project-id)))
                  (tasks (plist-get project-data :tasks)))
             (dolist (task tasks)
               (let* ((id (plist-get task :id))
                      (etag (plist-get task :etag))
                      (existing-pos (ticktick--find-task-under-project project-pos id)))
                 (if existing-pos
                     ;; Update existing task if etag differs
                     (save-excursion
                       (goto-char existing-pos)
                       (let ((existing-etag (org-entry-get nil "TICKTICK_ETAG")))
                         (unless (string= existing-etag etag)
                           (delete-region (org-entry-beginning-position)
                                          (org-entry-end-position))
                           (insert (ticktick--task-to-heading task))
                           (ticktick--update-sync-meta))))
                   ;; Only insert if it doesn't exist
                   (save-excursion
                     (goto-char project-pos)
                     (outline-next-heading)
                     (insert (ticktick--task-to-heading task) "\n")
                     (ticktick--update-sync-meta))))))))
       (save-buffer)))))

(defun ticktick-push-from-org ()
  "Push all updated org tasks back to TickTick."
  (interactive)
  (with-current-buffer (find-file-noselect ticktick-sync-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (outline-next-heading)
       (when (and (= (org-current-level) 2)
                  (not (org-entry-get nil "TICKTICK_PROJECT_ID")))
         (when (ticktick--should-sync-p)
           (let* ((task (ticktick--heading-to-task))
                  (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))
                  (id (alist-get "id" task)))
             (if (and id (not (string-empty-p id)))
                 (progn
                   (ticktick-request "POST" (format "/open/v1/task/%s" id)
                                     (append task `(("projectId" . ,project-id))))
                   (ticktick--update-sync-meta)
                   (message "Updated: %s" (alist-get "title" task)))
               (let ((resp (ticktick-request "POST" "/open/v1/task"
                                             (append task `(("projectId" . ,project-id))))))
                 (when resp
                   (org-set-property "TICKTICK_ID" (plist-get resp :id))
                   (org-set-property "TICKTICK_ETAG" (plist-get resp :etag))
                   (ticktick--update-sync-meta)
                   (message "Created: %s" (plist-get resp :title)))))))))))

(defun ticktick-sync-two-way ()
  "Two-way sync: fetch and push."
  (interactive)
  (ticktick-fetch-to-org)
  (ticktick-push-from-org))

(defun ticktick--autosync ()
  "Auto sync if enabled."
  (when ticktick--autosync
    (when (file-exists-p ticktick-sync-file)
      (ignore-errors (ticktick-sync-two-way)))))

;;; Task creation
(defun ticktick-create-task ()
  "Create new task from heading."
  (interactive)
  (let* ((project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))
         (task (ticktick--heading-to-task))
         (resp (ticktick-request "POST" "/open/v1/task"
                                 (append task `(("projectId" . ,project-id))))))
    (when resp
      (org-set-property "TICKTICK_ID" (plist-get resp :id))
      (org-set-property "TICKTICK_ETAG" (plist-get resp :etag))
      (message "Created task: %s" (plist-get resp :title)))))

(add-hook 'focus-out-hook #'ticktick--autosync)
(add-hook 'window-buffer-change-functions (lambda (&rest _) (ticktick--autosync)))

(provide 'ticktick)
;;; ticktick.el ends here
