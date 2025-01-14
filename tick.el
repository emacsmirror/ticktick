(require 'oauth2)
(require 'json)
(require 'url)
(require 'seq)
(require 'tabulated-list)

(defgroup ticktick nil
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

(defcustom tickel-task-file-mapping nil
  "An alist mapping TickTick task IDs to file paths and optional Org headings for syncing tasks.
Each entry is a cons cell where the car is the TickTick task ID (string),
and the cdr is a cons cell of the file path (string) and optional Org heading (string)."
  :type '(alist :key-type (string :tag "Task ID")
                :value-type (cons (file :tag "File Path")
                                  (choice (const :tag "No specific heading" nil)
                                          (string :tag "Org Heading"))))
  :group 'tickel)

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
        (progn
          (plist-put token-data :created_at (float-time))
          token-data)
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
        (> (float-time) (+ created-at expires-in -30)) ; refresh 30 seconds before expiry
      nil)))

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
        (let ((json-object-type 'plist)
              (json-array-type 'list))
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
  ;; Note: Some projects may not have tasks; handle accordingly
  (let* ((endpoint (format "/open/v1/project/%s/data" project-id))
         (data (tickel-request "GET" endpoint)))
    (if data
        (let ((tasks (plist-get data :tasks)))
          tasks)
      nil)))

(defun tickel-format-org-deadline (ticktick-date)
  "Convert a TickTick date to Org mode format.
Input date format: YYYY-MM-DDTHH:MM:SS.sss+ZZZZ
Output Org format: <YYYY-MM-DD>"
  (when (and ticktick-date (string-match "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" ticktick-date))
    (format "<%s>" (match-string 1 ticktick-date))))

;; Variables to keep track of original buffer and tasks
(defvar tickel-original-buffer nil
  "The buffer from which `tickel-sync-project-tasks' was called.")

(defvar tickel-projects nil
  "List of projects.")

(defvar tickel-tasks-data nil
  "List of tasks data retrieved from TickTick, with project information.")

;; Define tickel-sync-mode
(define-derived-mode tickel-sync-mode tabulated-list-mode "Tickel-Sync"
  "Major mode for displaying TickTick tasks and selecting them for import."
  (setq tabulated-list-format [("Mark" 5 t)
                               ("Title" 30 t)
                               ("Project" 20 t)
                               ("ID" 24 t)
                               ("Status" 6 t)
                               ("Due Date" 12 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries nil)
  (setq tabulated-list--tag-column 0)
  (tabulated-list-init-header)
  ;; Define keybindings
  (define-key tickel-sync-mode-map (kbd "m") 'tickel-mark)
  (define-key tickel-sync-mode-map (kbd "u") 'tickel-unmark)
  (define-key tickel-sync-mode-map (kbd "x") 'tickel-import-marked-tasks)
  (define-key tickel-sync-mode-map (kbd "q") 'quit-window)
  (setq-local revert-buffer-function 'tickel--refresh-task-buffer))

(defun tickel--make-tabulated-list-entries (tasks)
  "Create tabulated list entries from TASKS."
  (mapcar (lambda (task)
            (let* ((id (plist-get task :id))
                   (title (plist-get task :title))
                   (project-name (plist-get task :projectName))
                   (status (number-to-string (plist-get task :status)))
                   (due-date (or (tickel-format-org-deadline (plist-get task :dueDate)) ""))
                   (cols (vector " " title project-name id status due-date))) ; Mark column starts as " "
              (list id cols)))
          tasks))

(defun tickel--refresh-task-buffer (&rest _args)
  "Refresh the task list."
  (let ((tasks (tickel--fetch-all-tasks))
        (pos (point)))
    (setq tickel-tasks-data tasks)
    (setq tabulated-list-entries (tickel--make-tabulated-list-entries tasks))
    (tabulated-list-print t)
    (goto-char pos)))

(defun tickel--fetch-all-tasks ()
  "Fetch tasks from all projects and combine them with project information."
  (setq tickel-projects (tickel-get-projects))
  (let (all-tasks)
    (dolist (project tickel-projects)
      (let* ((project-id (plist-get project :id))
             (project-name (plist-get project :name))
             (tasks (tickel-get-tasks project-id)))
        (dolist (task tasks)
          ;; Add project name to each task
          (plist-put task :projectName project-name)
          ;; Add project ID to each task (already present)
          (push task all-tasks))))
    ;; Return the combined list of tasks
    (nreverse all-tasks)))

(defun tickel-sync-project-tasks ()
  "Display tasks from all TickTick projects, allowing selection for import."
  (interactive)
  (setq tickel-original-buffer (current-buffer))
  (let* ((tasks (tickel--fetch-all-tasks))
         (buf (get-buffer-create "*Tickel Tasks*")))
    (setq tickel-tasks-data tasks)
    (with-current-buffer buf
      (tickel-sync-mode)
      (setq tabulated-list-entries (tickel--make-tabulated-list-entries tasks))
      ;; Sort entries by project name
      (setq tabulated-list-sort-key (cons "Project" nil))
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun tickel-mark ()
  "Mark the task at point for import."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (tabulated-list-get-id))          ; Get the task ID
         (entry (assq id tabulated-list-entries))) ; Find the corresponding entry
    (when entry
      ;; Set the mark in the entry vector itself
      (aset (cadr entry) 0 "*")
      ;; Refresh the display for the current line
      (tabulated-list-set-col 0 "*"))
    (forward-line 1)))

(defun tickel-unmark ()
  "Unmark the task at point."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (tabulated-list-get-id))          ; Get the task ID
         (entry (assq id tabulated-list-entries))) ; Find the corresponding entry
    (when entry
      ;; Remove the mark in the entry vector itself
      (aset (cadr entry) 0 " ")
      ;; Refresh the display for the current line
      (tabulated-list-set-col 0 " "))
    (forward-line 1)))

(defun tickel--get-marked-tasks ()
  "Return list of IDs of marked tasks."
  (let (marked-tasks)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((entry (tabulated-list-get-entry)))
          ;; Check if the mark is set
          (when (and entry (string= (aref entry 0) "*"))
            (push (tabulated-list-get-id) marked-tasks)))
        (forward-line 1)))
    marked-tasks))

(defun tickel-import-marked-tasks ()
  "Import the marked tasks into the original buffer."
  (interactive)
  (let ((marked-task-ids (tickel--get-marked-tasks)))
    (if (null marked-task-ids)
        (message "No tasks marked.")
      (let ((tasks-to-insert (seq-filter (lambda (task)
                                           (member (plist-get task :id) marked-task-ids))
                                         tickel-tasks-data)))
        (with-current-buffer tickel-original-buffer
          (save-excursion
            (dolist (task tasks-to-insert)
              (tickel--insert-task-into-buffer task))))
        ;; Update tickel-task-file-mapping
        (dolist (task tasks-to-insert)
          (tickel--update-task-file-mapping task (buffer-file-name tickel-original-buffer)))
        (message "Imported %d tasks." (length tasks-to-insert))
        ;; Close the tasks buffer
        (quit-window)))))

(defun tickel--insert-task-into-buffer (task)
  "Insert TASK into current buffer in Org format."
  (let* ((task-title (plist-get task :title))
         (task-id (plist-get task :id))
         (task-content (plist-get task :content))
         (task-project-id (plist-get task :projectId))
         (task-project-name (plist-get task :projectName))
         (task-status (plist-get task :status))
         (task-due-date (tickel-format-org-deadline (plist-get task :dueDate)))
         (task-is-completed (eq (plist-get task :status) 2)) ; Assuming status 2 is completed
         (todo-state (if task-is-completed "DONE" "TODO")))
    (org-back-to-heading t)
    (org-end-of-subtree t t)  ; Move to the end of the current subtree
    (org-insert-heading-respect-content)
    (insert (format "%s %s\n" todo-state task-title))
    (when task-due-date
      (insert (format "DEADLINE: %s\n" task-due-date)))
    (insert ":PROPERTIES:\n")
    (insert (format ":ticktick-task-id: %s\n" task-id))
    (when task-project-id
      (insert (format ":ticktick-project-id: %s\n" task-project-id)))
    (when task-project-name
      (insert (format ":ticktick-project-name: %s\n" task-project-name)))
    (when task-status
      (insert (format ":ticktick-status: %s\n" task-status)))
    (insert ":END:\n")
    (when task-content
      (insert (format "%s\n" task-content)))))

(defun tickel--update-task-file-mapping (task file &optional heading)
  "Update `tickel-task-file-mapping' with TASK and FILE, and optional HEADING."
  (let ((task-id (plist-get task :id)))
    ;; Remove any existing mapping for this task ID
    (setq tickel-task-file-mapping (assq-delete-all task-id tickel-task-file-mapping))
    ;; Add the new mapping
    (add-to-list 'tickel-task-file-mapping
                 (cons task-id
                       (cons file heading)))))
