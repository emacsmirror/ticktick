(require 'oauth2)
(require 'json)
(require 'url)
(require 'seq)
(require 'tabulated-list)
(require 'org)

(defgroup tickel nil
  "Interface with TickTick API."
  :prefix "tickel-"
  :group 'applications)

(defcustom tickel-client-id "uxXCDqEv3nV3C2M1hn"
  "TickTick client ID."
  :type 'string
  :group 'tickel)

(defcustom tickel-client-secret "6eh+gE#66+3lKHJv56d)EU8&eru_k$*8"
  "TickTick client secret."
  :type 'string
  :group 'ticktick)

(defcustom tickel-auth-scopes "tasks:write tasks:read"
  "Space-separated scopes for TickTick API access."
  :type 'string
  :group 'ticktick)

(defvar tickel-redirect-uri "http://localhost"
  "The redirect URI registered with TickTick.")

(defvar tickel-token nil
  "Access token for accessing the TickTick API.
This is a plist containing token information.")

(defcustom tickel-sync-file "~/org/ticktick.org"
  "File path where all TickTick tasks will be synced."
  :type 'file
  :group 'ticktick)

(defcustom tickel-sync-interval 600
  "Interval in seconds between automatic syncs with TickTick (default: 10 minutes)."
  :type 'integer
  :group 'ticktick)

(defvar tickel-sync-timer nil
  "Timer object for automatic syncing.")

;;; Authorization functions (unchanged)
;;;###autoload
(defun tickel-authorize ()
  "Authorize tick.el with TickTick and obtain an access token."
  (interactive)
  (let* ((state (format "%06x" (random (expt 16 6))))
         (auth-url (concat "https://ticktick.com/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,tickel-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,tickel-redirect-uri)
                              ("scope" ,tickel-auth-scopes)
                              ("state" ,state)))))
         (authorization (concat "Basic "
                                (base64-encode-string
                                 (concat tickel-client-id ":" tickel-client-secret)
                                 t)))
         code token-response)
    (browse-url auth-url)
    (message "Please authorize the application in your browser and enter the authorization code (after '?code=').")
    (setq code (read-string "Enter authorization code: "))
    (setq token-response
          (tickel--exchange-code-for-token code authorization))
    (if token-response
        (progn
          (setq tickel-token token-response)
          (message "Authorization successful!"))
      (message "Failed to obtain access token."))))

(defun tickel--exchange-code-for-token (code authorization)
  "Exchange the authorization CODE for an access token using AUTHORIZATION header."
  (let* ((token-url "https://ticktick.com/oauth/token")
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,authorization)
                                      ("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (mapconcat
                            (lambda (kv)
                              (concat (url-hexify-string (car kv)) "=" (url-hexify-string (cdr kv))))
                            `(("grant_type" . "authorization_code")
                              ("code" . ,code)
                              ("redirect_uri" . ,tickel-redirect-uri)
                              ("scope" . ,tickel-auth-scopes))
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
(defun tickel-refresh-token ()
  "Refresh the OAuth2 token."
  (interactive)
  (when tickel-token
    (let* ((token-url "https://ticktick.com/oauth/token")
           (authorization (concat "Basic "
                                  (base64-encode-string
                                   (concat tickel-client-id ":" tickel-client-secret)
                                   t)))
           (url-request-method "POST")
           (url-request-extra-headers `(("Authorization" . ,authorization)
                                        ("Content-Type" . "application/x-www-form-urlencoded")))
           (refresh-token (plist-get tickel-token :refresh_token))
           (url-request-data (mapconcat
                              (lambda (kv)
                                (concat (url-hexify-string (car kv)) "=" (url-hexify-string (cdr kv))))
                              `(("grant_type" . "refresh_token")
                                ("refresh_token" . ,refresh-token)
                                ("redirect_uri" . ,tickel-redirect-uri)
                                ("scope" . ,tickel-auth-scopes))
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
            (setq tickel-token token-data)
            (message "Token refreshed!"))
        (message "Failed to refresh token.")))))

(defun tickel--ensure-token ()
  "Ensure we have a valid access token."
  (unless (and tickel-token
               (plist-get tickel-token :access_token)
               (not (tickel--token-expired-p tickel-token)))
    (tickel-refresh-token)))

(defun tickel--token-expired-p (token)
  "Check if TOKEN has expired."
  (let ((expires-in (plist-get token :expires_in))
        (created-at (plist-get token :created_at)))
    (if (and expires-in created-at)
        (> (float-time) (+ created-at expires-in -30))
      nil)))

(defun tickel-request (method endpoint &optional data)
  "Send a request to the TickTick API.
METHOD is the HTTP method as a string.
ENDPOINT is the API endpoint.
DATA is an alist of data to send with the request."
  (tickel--ensure-token)
  (condition-case err
      (let* ((url-request-method method)
             (url (concat "https://api.ticktick.com" endpoint))
             (access-token (plist-get tickel-token :access_token))
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
          (tickel-refresh-token)
          (tickel-request method endpoint data))
         (t
          (error "HTTP Error %s: %s" status response))))
    (error
     (message "Request failed: %s" (error-message-string err))
     nil)))

(defun tickel--task-to-heading (task)
  "Convert a TickTick TASK to an org heading."
  (let* ((id (plist-get task :id))
         (title (plist-get task :title))
         (content (plist-get task :content))
         (status (plist-get task :status))
         (due-date (plist-get task :dueDate))
         (priority (plist-get task :priority)))
    (concat "** " (if (= status 2) "DONE" "TODO")
            (pcase priority
              (5 " [#A]")
              (3 " [#B]")
              (1 " [#C]")
              (_ ""))
            " " title
            (when due-date
              (concat "\nDEADLINE: "
                     (format-time-string "<%Y-%m-%d %a>"
                                       (date-to-time due-date))))
            "\n:PROPERTIES:\n:TICKTICK_ID: " id "\n:END:\n"
            (when content
              (concat (replace-regexp-in-string "\\*" "\\\\*" content) "\n")))))

(defun tickel--heading-to-task ()
  "Convert org heading at point to a TickTick task."
  (let* ((element (org-element-at-point))
         (title (org-element-property :title element))
         (todo-type (org-element-property :todo-type element))
         (priority (org-element-property :priority element))
         (deadline (org-element-property :deadline element))
         (id (org-entry-get nil "TICKTICK_ID"))
         (content (org-element-property :contents-begin element)))
    `(("id" . ,id)
      ("title" . ,title)
      ("status" . ,(if (eq todo-type 'done) 2 0))
      ("priority" . ,(pcase priority
                      (?A 5)
                      (?B 3)
                      (?C 1)
                      (_ 0)))
      ("dueDate" . ,(when deadline
                      (format-time-string "%Y-%m-%dT%H:%M:%S+0000"
                                        (org-timestamp-to-time deadline))))
      ("content" . ,(when content
                     (string-trim (buffer-substring-no-properties
                                 content
                                 (org-element-property :contents-end element))))))))

;;;###autoload
(defun tickel-fetch ()
  "Fetch all tasks from TickTick and update the sync file."
  (interactive)
  (let ((projects (tickel-request "GET" "/open/v1/project")))
    (with-current-buffer (find-file-noselect tickel-sync-file)
      (org-with-wide-buffer
       (erase-buffer)  ; Start fresh
       (dolist (project projects)
         (let* ((project-id (plist-get project :id))
                (project-name (plist-get project :name))
                (project-data (tickel-request "GET" (format "/open/v1/project/%s/data" project-id)))
                (tasks (plist-get project-data :tasks)))
           ;; Insert project heading
           (insert (format "* %s\n:PROPERTIES:\n:TICKTICK_PROJECT_ID: %s\n:END:\n\n" 
                         project-name project-id))
           ;; Insert all tasks under this project
           (dolist (task tasks)
             (insert (tickel--task-to-heading task)))))
       (save-buffer)))))

;;;###autoload
(defun tickel-sync ()
  "Sync tasks between local file and TickTick server."
  (interactive)
  (tickel-fetch)
  (with-current-buffer (find-file-noselect tickel-sync-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (outline-next-heading)
       (unless (org-entry-get nil "TICKTICK_PROJECT_ID")  ; Skip project headings
         (when-let ((id (org-entry-get nil "TICKTICK_ID")))
           (let* ((task (tickel--heading-to-task))
                  (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t)))  ; Get inherited property
             (tickel-request "POST" (format "/open/v1/task/%s" id) 
                           (append task `(("projectId" . ,project-id)))))))))))

;;;###autoload
(defun tickel-create-task ()
  "Create a new TickTick task from the org heading at point."
  (interactive)
  (let* ((project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))  ; Get inherited property
         (task (tickel--heading-to-task))
         (response (tickel-request "POST" "/open/v1/task" 
                                 (append task `(("projectId" . ,project-id))))))
    (when response
      (org-set-property "TICKTICK_ID" (plist-get response :id))
      (message "Task created successfully!"))))

(provide 'tickel)
;;; ticktick.el ends here
