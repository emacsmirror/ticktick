(require 'oauth2)

(defgroup ticktick nil
  "Settings for TickTick API"
  :prefix "ticktick-"
  :group 'applications)

(defcustom ticktick-client-id ""
  "OAuth2 client ID for TickTick"
  :type 'string
  :group 'ticktick)

(defcustom ticktick-client-secret ""
  "OAuth2 client secret for TickTick"
  :type 'string
  :safe #'stringp
  :group 'ticktick)

(defcustom ticktick-redirect-uri "http://localhost"
  "OAuth2 redirect URI for TickTick."
  :type 'string
  :group 'ticktick)

(defvar oauth2-ticktick-token nil)

(defun ticktick-authenticate ()
  "Authenticate tick.el via OAuth2 with TickTick API."
  (setq oauth2-ticktick-token
        (oauth2-auth-and-store 
         "https://ticktick.com/oauth/authorize"
         "https://ticktick.com/oauth/token"
         "tasks:read tasks:write"
         ticktick-client-id
         ticktick-client-secret
         ticktick-redirect-uri)))

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
  :group 'tickel)

(defcustom tickel-auth-scopes "tasks:write tasks:read"
  "Space-separated scopes for TickTick API access."
  :type 'string
  :group 'tickel)

(defvar tickel-redirect-uri "http://localhost"
  "The redirect URI registered with TickTick.")

(defvar tickel-token nil
  "Access token for accessing the TickTick API.
This is a plist containing token information.")

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
    ;; Open the authorization URL in the browser
    (browse-url auth-url)
    (message "Please authorize the application in your browser and enter the authorization code from the redirected URL (which comes after '?code=').")
    ;; Prompt the user to enter the authorization code
    (setq code (read-string "Enter authorization code: "))
    ;; Exchange the authorization code for access token
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
        token-data
      nil)))

(defun tickel-refresh-token ()
  "Refresh the OAuth2 token."
  (interactive)
  (when tickel-token
    ;; Prepare request to refresh the token
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
            (setq tickel-token token-data)
            (message "Token refreshed!"))
        (message "Failed to refresh token.")))))

(defun tickel-request (method endpoint &optional data)
  "Send a request to the TickTick API.
METHOD is the HTTP method as a string (e.g., \"GET\", \"POST\").
ENDPOINT is the API endpoint (e.g., \"/open/v1/project\").
DATA is an alist of data to send with the request."
  (tickel--ensure-token)
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
      ;; Success: parse the JSON response
      (if (string-blank-p response)
          nil
        (let ((json-object-type 'plist))
          (json-read-from-string response))))
     ((= status 401)
      ;; Unauthorized: Try refreshing the token
      (tickel-refresh-token)
      ;; Retry the request once
      (tickel-request method endpoint data))
     (t
      ;; Other errors
      (message "HTTP Error %s: %s" status response)
      nil))))

(defun tickel-get-projects ()
  "Retrieve the list of projects from TickTick."
  (interactive)
  (let ((projects (tickel-request "GET" "/open/v1/project")))
    (if projects
        (progn
          (message "Projects retrieved successfully.")
          projects)
      (message "Failed to retrieve projects.")
      nil)))

(defun tickel-get-tasks (project-id)
  "Retrieve the list of tasks for PROJECT-ID from TickTick."
  (interactive "sProject ID: ")
  (let* ((endpoint (format "/open/v1/project/%s/data" project-id))
         (data (tickel-request "GET" endpoint)))
    (if data
        (let ((tasks (plist-get data :tasks)))
          (message "Tasks retrieved successfully.")
          tasks)
      (message "Failed to retrieve tasks.")
      nil)))

(defun tickel-create-task (project-id title &optional content)
  "Create a new task with TITLE in the project with PROJECT-ID.
Optional argument CONTENT is the task content."
  (interactive "sProject ID: \nsTask Title: \nsTask Content (optional): ")
  (let ((data `(("title" . ,title)
                ("projectId" . ,project-id)
                ,@(when content `(("content" . ,content))))))
    (let ((result (tickel-request "POST" "/open/v1/task" data)))
      (if result
          (message "Task '%s' created in project '%s'." title project-id)
        (message "Failed to create task.")))))

(defun tickel-update-task (task-id project-id updates)
  "Update the task with TASK-ID in PROJECT-ID using UPDATES alist.
UPDATES should be an alist of fields to update."
  (let ((endpoint (format "/open/v1/task/%s" task-id))
        (data (append `(("id" . ,task-id)
                        ("projectId" . ,project-id))
                      updates)))
    (let ((result (tickel-request "POST" endpoint data)))
      (if result
          (message "Task '%s' updated." task-id)
        (message "Failed to update task.")))))

(defun tickel-delete-task (project-id task-id)
  "Delete the task with TASK-ID from PROJECT-ID."
  (interactive "sProject ID: \nsTask ID: ")
  (let ((endpoint (format "/open/v1/project/%s/task/%s" project-id task-id)))
    (let ((result (tickel-request "DELETE" endpoint)))
      (if result
          (message "Task '%s' deleted from project '%s'." task-id project-id)
        (message "Failed to delete task.")))))
