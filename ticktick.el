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

(defcustom ticktick-auth-scope "tasks:write tasks:read"
  "Space-separated scopes for TickTick API access."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-token-file (concat user-emacs-directory "org-gcal/")
  "Location of file storing token"
  :type 'file
  :group 'ticktick)

(defcustom ticktick-auto-fetch-new-tasks-from-server nil
  "Whether ticktick.el should automatically fetch new
objects from the server."
  :type 'boolean
  :group 'ticktick)

(defcustom ticktick-sync-mode "down"
  :type 'string
  :group 'ticktick)

(defcustom ticktick-sync-files ("~/org/ticktick.org")
   "Association list of file path(s) where tasks under a TickTick project should be
imported '(project-id file)."
  :type 'file
  :group 'ticktick)

(defcustom ticktick-)

(defcustom ticktick-files-alist nil
  "Association list of  '(project-id file). For each project-id, ‘ticktick-fetch’
and ‘ticktick-sync’ will retrieve new tasks and events and insert them into the
file."
  :group 'ticktick
  :type '(alist :key-type (string :tag "project-id") :value-type (file :tag "Org file")))

(defvar ticktick-token nil
  "Access token for accessing the TickTick API."
  :type 'string
  :group 'ticktick)

(defvar ticktick-redirect-uri "http://localhost"
  "The redirect URI registered with TickTick."
  :type 'string
  :group 'ticktick)

(defvar ticktick-sync-timer nil
  "Timer object for automatic syncing.")

;;; Authorization functions (unchanged)
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

(defun ticktick--ensure-token ()
  "Ensure we have a valid access token."
  (unless (and ticktick-token
               (plist-get ticktick-token :access_token)
               (not (ticktick--token-expired-p ticktick-token)))
    (ticktick-refresh-token)))

(defun ticktick--token-expired-p (token)
  "Check if TOKEN has expired."
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
  (ticktick--ensure-token)
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

(defun ticktick--heading-to-task ()
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
(defun ticktick-fetch ()
  "Fetch all tasks from TickTick and update the sync file."
  (interactive)
  (let ((projects (ticktick-request "GET" "/open/v1/project")))
    (with-current-buffer (find-file-noselect ticktick-sync-file)
      (org-with-wide-buffer
       (erase-buffer)  ; Start fresh
       (dolist (project projects)
         (let* ((project-id (plist-get project :id))
                (project-name (plist-get project :name))
                (project-data (ticktick-request "GET" (format "/open/v1/project/%s/data" project-id)))
                (tasks (plist-get project-data :tasks)))
           ;; Insert project heading
           (insert (format "* %s\n:PROPERTIES:\n:TICKTICK_PROJECT_ID: %s\n:END:\n\n" 
                         project-name project-id))
           ;; Insert all tasks under this project
           (dolist (task tasks)
             (insert (ticktick--task-to-heading task)))))
       (save-buffer)))))

;;;###autoload
(defun ticktick-sync ()
  "Sync tasks between local file and TickTick server."
  (interactive)
  (ticktick-fetch)
  (with-current-buffer (find-file-noselect ticktick-sync-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (outline-next-heading)
       (unless (org-entry-get nil "TICKTICK_PROJECT_ID")  ; Skip project headings
         (when-let ((id (org-entry-get nil "TICKTICK_ID")))
           (let* ((task (ticktick--heading-to-task))
                  (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t)))  ; Get inherited property
             (ticktick-request "POST" (format "/open/v1/task/%s" id) 
                           (append task `(("projectId" . ,project-id)))))))))))

;;;###autoload
(defun ticktick-create-task ()
  "Create a new TickTick task from the org heading at point."
  (interactive)
  (let* ((project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))  ; Get inherited property
         (task (ticktick--heading-to-task))
         (response (ticktick-request "POST" "/open/v1/task" 
                                 (append task `(("projectId" . ,project-id))))))
    (when response
      (org-set-property "TICKTICK_ID" (plist-get response :id))
      (message "Task created successfully!"))))

(provide 'ticktick)
;;; ticktick.el ends here
