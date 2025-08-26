;;; ticktick.el --- Sync tasks between TickTick and Emacs / Org Mode -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Package-Requires: ((emacs "26.1") (request "0.3.0") (simple-httpd "1.5.0"))
;; Keywords: tools, ticktick, org, tasks, todo
;; URL: https://github.com/polhuang/ticktick.el

;;; Commentary:
;; ticktick.el provides two-way sync between TickTick and Emacs / Org-mode.

;;; Code:

(require 'request)
(require 'json)
(require 'url)
(require 'seq)
(require 'tabulated-list)
(require 'org)
(require 'subr-x)       ;; for string-blank-p, etc.
(require 'simple-httpd) ;; tiny local HTTP server
(require 'cl-lib)       ;; for cl-find

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

(defcustom ticktick-httpd-port 8080
  "Local port for the OAuth callback server."
  :type 'integer
  :group 'ticktick)

(defcustom ticktick-redirect-uri "http://localhost:8080/ticktick-callback"
  "Redirect URI registered with TickTick. Must match OAuth app settings."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-snapshots-file
  (expand-file-name ".ticktick-snapshots" ticktick-dir)
  "File in which to store sync snapshots for incremental sync."
  :type 'file
  :group 'ticktick)

(defcustom ticktick-org-file "ticktick.org"
  "Org file to sync with TickTick. If nil, uses current buffer when syncing."
  :type '(choice (const :tag "Current buffer" nil)
                 (file :tag "Org file"))
  :group 'ticktick)

(defvar ticktick-token nil
  "Access token plist for accessing the TickTick API.")

(defvar ticktick-oauth-state nil
  "CSRF-prevention state for the OAuth flow.")

(defvar ticktick-snapshots nil
  "Cached snapshots data for incremental sync.")

;; --- Token persistence helpers (optional but handy) --------------------------
(defun ticktick--ensure-dir ()
  (unless (file-directory-p ticktick-dir)
    (make-directory ticktick-dir t)))

(defun ticktick--save-token ()
  "Persist `ticktick-token` to `ticktick-token-file`."
  (when ticktick-token
    (ticktick--ensure-dir)
    (with-temp-file ticktick-token-file
      (insert (prin1-to-string ticktick-token)))))

(defun ticktick--load-token ()
  "Load token from `ticktick-token-file` into `ticktick-token`."
  (when (file-exists-p ticktick-token-file)
    (with-temp-buffer
      (insert-file-contents ticktick-token-file)
      (goto-char (point-min))
      (setq ticktick-token (read (current-buffer))))))

;; Load any existing token at startup
(ticktick--load-token)

;;; Snapshot management --------------------------------------------------------

(defun ticktick--save-snapshots ()
  "Persist `ticktick-snapshots` to `ticktick-snapshots-file`."
  (when ticktick-snapshots
    (ticktick--ensure-dir)
    (with-temp-file ticktick-snapshots-file
      (insert (prin1-to-string ticktick-snapshots)))))

(defun ticktick--load-snapshots ()
  "Load snapshots from `ticktick-snapshots-file` into `ticktick-snapshots`."
  (when (file-exists-p ticktick-snapshots-file)
    (with-temp-buffer
      (insert-file-contents ticktick-snapshots-file)
      (goto-char (point-min))
      (setq ticktick-snapshots (read (current-buffer))))))

(defun ticktick--get-org-file-path ()
  "Get the org file path to sync. Uses `ticktick-org-file` or current buffer."
  (or ticktick-org-file
      (and (derived-mode-p 'org-mode) (buffer-file-name))
      (user-error "No org file specified and current buffer is not an org file")))

(defun ticktick--create-org-snapshot (org-file-path)
  "Create a snapshot of the org file at ORG-FILE-PATH."
  (with-temp-buffer
    (insert-file-contents org-file-path)
    (let ((tasks (ticktick--parse-org-tasks)))
      (list :file org-file-path
            :timestamp (float-time)
            :tasks tasks
            :content-hash (secure-hash 'md5 (current-buffer))))))

(defun ticktick--create-ticktick-snapshot (tasks)
  "Create a snapshot of TickTick TASKS."
  (list :timestamp (float-time)
        :tasks tasks
        :tasks-hash (secure-hash 'md5 (prin1-to-string tasks))))

(defun ticktick--get-current-snapshots ()
  "Get current snapshots, loading from file if not in memory."
  (unless ticktick-snapshots
    (ticktick--load-snapshots))
  (or ticktick-snapshots
      (list :org nil :ticktick nil :sync-timestamp nil)))

(defun ticktick--update-snapshots (org-snapshot ticktick-snapshot)
  "Update snapshots with new ORG-SNAPSHOT and TICKTICK-SNAPSHOT."
  (setq ticktick-snapshots
        (list :org org-snapshot
              :ticktick ticktick-snapshot
              :sync-timestamp (float-time)))
  (ticktick--save-snapshots))

;;; Org file parsing and diffing -----------------------------------------------

(defun ticktick--parse-org-tasks ()
  "Parse org tasks from current buffer and return a list of task plists."
  (let ((tasks '()))
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))
              (todo-state (org-get-todo-state))
              (tags (org-get-tags))
              (scheduled (org-get-scheduled-time (point)))
              (deadline (org-get-deadline-time (point)))
              (id (org-id-get-create))
              (priority (org-get-priority))
              (content (save-excursion
                        (org-back-to-heading t)
                        (let ((start (point)))
                          (org-end-of-subtree)
                          (buffer-substring-no-properties start (point))))))
         (push (list :id id
                     :heading heading
                     :todo-state todo-state
                     :tags tags
                     :scheduled scheduled
                     :deadline deadline
                     :priority priority
                     :content content
                     :content-hash (secure-hash 'md5 content))
               tasks)))
     t 'file)
    (nreverse tasks)))

(defun ticktick--normalize-task (task)
  "Normalize a TASK for comparison by removing position-dependent fields."
  (let ((normalized (copy-sequence task)))
    (plist-put normalized :content nil)
    normalized))

(defun ticktick--find-task-by-id (task-id tasks)
  "Find a task with TASK-ID in TASKS list."
  (cl-find-if (lambda (task) (string= (plist-get task :id) task-id)) tasks))

(defun ticktick--task-changed-p (old-task new-task)
  "Check if NEW-TASK has changed compared to OLD-TASK."
  (not (equal (ticktick--normalize-task old-task)
              (ticktick--normalize-task new-task))))

(defun ticktick--diff-org-tasks (old-tasks new-tasks)
  "Compare OLD-TASKS with NEW-TASKS and return changes.
Returns a plist with :added, :modified, :deleted lists."
  (let ((added '())
        (modified '())
        (deleted '()))
    
    (dolist (new-task new-tasks)
      (let* ((task-id (plist-get new-task :id))
             (old-task (ticktick--find-task-by-id task-id old-tasks)))
        (if old-task
            (when (ticktick--task-changed-p old-task new-task)
              (push (list :old old-task :new new-task) modified))
          (push new-task added))))
    
    (dolist (old-task old-tasks)
      (let ((task-id (plist-get old-task :id)))
        (unless (ticktick--find-task-by-id task-id new-tasks)
          (push old-task deleted))))
    
    (list :added (nreverse added)
          :modified (nreverse modified)
          :deleted (nreverse deleted))))

;;; Authorization functions ----------------------------------------------------

(defun ticktick--authorization-header ()
  (concat "Basic "
          (base64-encode-string
           (concat ticktick-client-id ":" ticktick-client-secret) t)))

(defun ticktick--exchange-code-for-token (code authorization)
  "Exchange the authorization CODE for an access token using AUTHORIZATION header."
  (let* ((form-data (mapconcat
                     (lambda (kv)
                       (concat (url-hexify-string (car kv)) "=" (url-hexify-string (cdr kv))))
                     `(("grant_type" . "authorization_code")
                       ("code" . ,code)
                       ("redirect_uri" . ,ticktick-redirect-uri)
                       ("scope" . ,ticktick-auth-scopes))
                     "&"))
         (response-data nil)
         (request-obj (request "https://ticktick.com/oauth/token"
                        :type "POST"
                        :headers `(("Authorization" . ,authorization)
                                   ("Content-Type" . "application/x-www-form-urlencoded"))
                        :data form-data
                        :parser (lambda ()
                                  (let ((json-object-type 'plist))
                                    (json-read)))
                        :sync t
                        :success (cl-function 
                                  (lambda (&key data response &allow-other-keys)
                                    (setq response-data data)))
                        :error (cl-function 
                                (lambda (&key response error-thrown &allow-other-keys)
                                  (message "Token exchange failed: %s (HTTP %s)" 
                                           (or error-thrown "unknown error")
                                           (and response (request-response-status-code response)))
                                  (setq response-data nil))))))
    (when (and response-data (plist-get response-data :access_token))
      (plist-put response-data :created_at (float-time))
      response-data)))

(defun ticktick--start-callback-server ()
  "Start the local OAuth callback server if not already running."
  (setq httpd-port ticktick-httpd-port)
  (unless (httpd-running-p)
    (httpd-start)))

;; The servlet name determines the path: /ticktick-callback
(defservlet ticktick-callback text/plain (path query)
  "Handle the TickTick OAuth redirect."
  (let ((code  (cadr (assoc "code" query)))
        (state (cadr (assoc "state" query))))
    (cond
     ((not (and code state))
      (insert "Authentication failed: missing code/state."))
     ((and ticktick-oauth-state (not (string= state ticktick-oauth-state)))
      (insert "Authentication failed: invalid state."))
     (t
      (let ((tok (ticktick--exchange-code-for-token code (ticktick--authorization-header))))
        (if tok
            (progn
              (setq ticktick-token tok)
              (ticktick--save-token)
              (insert "Authentication successful! You can close this window.")
              (message "TickTick: authenticated successfully."))
          (insert "Authentication failed while exchanging code.")
          (message "TickTick: token exchange failed.")))))))

(defun ticktick-debug-oauth ()
  "Debug OAuth configuration and connection."
  (interactive)
  (message "=== TickTick OAuth Debug Info ===")
  (message "Client ID: %s" (if (string-empty-p ticktick-client-id) "NOT SET" "SET"))
  (message "Client Secret: %s" (if (string-empty-p ticktick-client-secret) "NOT SET" "SET"))
  (message "Redirect URI: %s" ticktick-redirect-uri)
  (message "Auth Scopes: %s" ticktick-auth-scopes)
  (message "Token file: %s" ticktick-token-file)
  (message "Token exists: %s" (if (and ticktick-token (plist-get ticktick-token :access_token)) "YES" "NO"))
  (when ticktick-token
    (message "Token expires: %s" (if (ticktick-token-expired-p ticktick-token) "EXPIRED" "VALID")))
  (message "Server running: %s" (if (httpd-running-p) "YES" "NO"))
  (message "Server port: %d" ticktick-httpd-port))

;;;###autoload
(defun ticktick-authorize ()
  "Authorize ticktick.el with TickTick and obtain an access token via local callback.
Starts a local server, opens the browser for consent, then captures the redirect."
  (interactive)
  (unless (and (stringp ticktick-client-id) (not (string-empty-p ticktick-client-id))
               (stringp ticktick-client-secret) (not (string-empty-p ticktick-client-secret)))
    (user-error "ticktick-client-id and ticktick-client-secret must be set"))
  (ticktick--start-callback-server)
  (setq ticktick-oauth-state (format "%06x" (random (expt 16 6))))
  (let* ((auth-url (concat "https://ticktick.com/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,ticktick-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,ticktick-redirect-uri)
                              ("scope" ,ticktick-auth-scopes)
                              ("state" ,ticktick-oauth-state))))))
    (browse-url auth-url)
    (message "TickTick: opened browser for OAuth. Waiting for http://localhost:%d/ticktick-callback ..." ticktick-httpd-port)))

;;; Manual fallback (optional): if you ever want to paste a code directly
(defun ticktick-authorize-manual (code)
  "Manually exchange CODE for an access token (fallback)."
  (interactive "sPaste ?code= value from redirect URL: ")
  (let ((tok (ticktick--exchange-code-for-token code (ticktick--authorization-header))))
    (if tok
        (progn
          (setq ticktick-token tok)
          (ticktick--save-token)
          (message "Authorization successful!"))
      (user-error "Failed to obtain access token."))))

;;; Token maintenance ----------------------------------------------------------

;;;###autoload
(defun ticktick-refresh-token ()
  "Refresh the OAuth2 token."
  (interactive)
  (when ticktick-token
    (let* ((authorization (ticktick--authorization-header))
           (refresh-token (plist-get ticktick-token :refresh_token))
           (form-data (mapconcat
                       (lambda (kv)
                         (concat (url-hexify-string (car kv)) "=" (url-hexify-string (cdr kv))))
                       `(("grant_type" . "refresh_token")
                         ("refresh_token" . ,refresh-token)
                         ("redirect_uri" . ,ticktick-redirect-uri)
                         ("scope" . ,ticktick-auth-scopes))
                       "&"))
           (response-data nil)
           (request-obj (request "https://ticktick.com/oauth/token"
                          :type "POST"
                          :headers `(("Authorization" . ,authorization)
                                     ("Content-Type" . "application/x-www-form-urlencoded"))
                          :data form-data
                          :parser (lambda ()
                                    (let ((json-object-type 'plist))
                                      (json-read)))
                          :sync t
                          :success (cl-function 
                                    (lambda (&key data response &allow-other-keys)
                                      (setq response-data data)))
                          :error (cl-function 
                                  (lambda (&key response error-thrown &allow-other-keys)
                                    (message "Token refresh failed: %s (HTTP %s)" 
                                             (or error-thrown "unknown error")
                                             (and response (request-response-status-code response)))
                                    (setq response-data nil))))))
      (if (and response-data (plist-get response-data :access_token))
          (progn
            (plist-put response-data :created_at (float-time))
            (setq ticktick-token response-data)
            (ticktick--save-token)
            (message "Token refreshed!"))
        (message "Failed to refresh token.")))))

(defun ticktick-token-expired-p (token)
  "Check if TOKEN has expired."
  (let ((expires-in (plist-get token :expires_in))
        (created-at (plist-get token :created_at)))
    (if (and expires-in created-at)
        (> (float-time) (+ created-at expires-in -30))
      nil)))

(defun ticktick-ensure-token ()
  "Ensure we have a valid access token."
  (ticktick--load-token)
  (unless (and ticktick-token
               (plist-get ticktick-token :access_token)
               (not (ticktick-token-expired-p ticktick-token)))
    (ticktick-refresh-token)))

;;; TickTick task operations ---------------------------------------------------

(defun ticktick--get-all-tasks ()
  "Get all tasks from TickTick."
  (let ((tasks '())
        (projects (ticktick-request "GET" "/open/v1/project")))
    (dolist (project projects)
      (let* ((project-id (plist-get project :id))
             (project-tasks (ticktick-request "GET" (format "/open/v1/project/%s/task" project-id))))
        (dolist (task project-tasks)
          (plist-put task :project-id project-id)
          (push task tasks))))
    (nreverse tasks)))

(defun ticktick--create-task (task)
  "Create a new task in TickTick from org TASK."
  (let* ((project-id (or (plist-get task :project-id) "inbox"))
         (task-data (ticktick--org-task-to-ticktick task)))
    (ticktick-request "POST" (format "/open/v1/project/%s/task" project-id) task-data)))

(defun ticktick--update-task (task)
  "Update an existing task in TickTick from org TASK."
  (let* ((project-id (plist-get task :project-id))
         (task-id (plist-get task :ticktick-id))
         (task-data (ticktick--org-task-to-ticktick task)))
    (when (and project-id task-id)
      (ticktick-request "POST" (format "/open/v1/project/%s/task/%s" project-id task-id) task-data))))

(defun ticktick--delete-task (task)
  "Delete a task in TickTick."
  (let* ((project-id (plist-get task :project-id))
         (task-id (plist-get task :ticktick-id)))
    (when (and project-id task-id)
      (ticktick-request "DELETE" (format "/open/v1/project/%s/task/%s" project-id task-id)))))

(defun ticktick--org-task-to-ticktick (org-task)
  "Convert an org TASK to TickTick task format."
  (let ((due-date nil)
        (start-date nil))
    
    (when-let ((deadline (plist-get org-task :deadline)))
      (setq due-date (format-time-string "%Y-%m-%dT%H:%M:%S.000+0000" deadline t)))
    
    (when-let ((scheduled (plist-get org-task :scheduled)))
      (setq start-date (format-time-string "%Y-%m-%dT%H:%M:%S.000+0000" scheduled t)))
    
    (let ((task-data (list :title (plist-get org-task :heading)
                          :content (or (plist-get org-task :content) "")
                          :status (if (string= (plist-get org-task :todo-state) "DONE") 2 0))))
      
      (when due-date
        (plist-put task-data :dueDate due-date))
      
      (when start-date
        (plist-put task-data :startDate start-date))
      
      (when-let ((tags (plist-get org-task :tags)))
        (plist-put task-data :tags tags))
      
      (let ((priority (plist-get org-task :priority)))
        (when priority
          (plist-put task-data :priority (cond
                                         ((>= priority org-priority-highest) 3)
                                         ((>= priority org-priority-default) 1)
                                         (t 0)))))
      
      task-data)))

(defun ticktick--ticktick-task-to-org (ticktick-task)
  "Convert a TickTick TASK to org task format."
  (let* ((title (plist-get ticktick-task :title))
         (content (plist-get ticktick-task :content))
         (status (plist-get ticktick-task :status))
         (due-date (plist-get ticktick-task :dueDate))
         (start-date (plist-get ticktick-task :startDate))
         (tags (plist-get ticktick-task :tags))
         (priority (plist-get ticktick-task :priority))
         (todo-state (if (= status 2) "DONE" "TODO")))
    
    (list :ticktick-id (plist-get ticktick-task :id)
          :project-id (plist-get ticktick-task :projectId)
          :heading title
          :content content
          :todo-state todo-state
          :deadline (when due-date (date-to-time due-date))
          :scheduled (when start-date (date-to-time start-date))
          :tags tags
          :priority (cond
                    ((= priority 3) org-priority-highest)
                    ((= priority 1) org-priority-default)
                    (t org-priority-lowest)))))

(defun ticktick--find-ticktick-task-by-org-id (org-id ticktick-tasks)
  "Find TickTick task that corresponds to org task with ORG-ID."
  (cl-find-if (lambda (task)
                (string= (plist-get task :content) org-id))
              ticktick-tasks))

(defun ticktick--diff-ticktick-tasks (old-tasks new-tasks)
  "Compare old TickTick tasks with new tasks and return changes."
  (let ((added '())
        (modified '())
        (deleted '()))
    
    (dolist (new-task new-tasks)
      (let* ((task-id (plist-get new-task :id))
             (old-task (cl-find-if (lambda (task) 
                                   (string= (plist-get task :id) task-id)) 
                                 old-tasks)))
        (if old-task
            (unless (equal old-task new-task)
              (push (list :old old-task :new new-task) modified))
          (push new-task added))))
    
    (dolist (old-task old-tasks)
      (let ((task-id (plist-get old-task :id)))
        (unless (cl-find-if (lambda (task) 
                            (string= (plist-get task :id) task-id)) 
                          new-tasks)
          (push old-task deleted))))
    
    (list :added (nreverse added)
          :modified (nreverse modified)
          :deleted (nreverse deleted))))

;;; Core request ---------------------------------------------------------------

(defun ticktick-request (method endpoint &optional data)
  "Send a request to the TickTick API.
METHOD is the HTTP method as a string.
ENDPOINT is the API endpoint.
DATA is an alist of data to send with the request."
  (ticktick-ensure-token)
  (let* ((url (concat "https://api.ticktick.com" endpoint))
         (access-token (plist-get ticktick-token :access_token))
         (headers `(("Authorization" . ,(concat "Bearer " access-token))
                    ("Content-Type" . "application/json")))
         (json-data (and data (json-encode data)))
         (response-data nil)
         (request-obj (request url
                        :type method
                        :headers headers
                        :data json-data
                        :parser (lambda ()
                                  (let ((json-object-type 'plist)
                                        (json-array-type 'list))
                                    (json-read)))
                        :sync t
                        :success (cl-function 
                                  (lambda (&key data response &allow-other-keys)
                                    (let ((status-code (request-response-status-code response)))
                                      (cond
                                       ((and (>= status-code 200) (< status-code 300))
                                        (setq response-data data))
                                       ((= status-code 401)
                                        (ticktick-refresh-token)
                                        (setq response-data (ticktick-request method endpoint data)))
                                       (t
                                        (error "HTTP Error %s" status-code))))))
                        :error (cl-function 
                                (lambda (&key response error-thrown &allow-other-keys)
                                  (let ((status-code (and response (request-response-status-code response))))
                                    (cond
                                     ((and status-code (= status-code 401))
                                      (ticktick-refresh-token)
                                      (setq response-data (ticktick-request method endpoint data)))
                                     (t
                                      (message "Request failed: %s" 
                                               (or (and response (request-response-data response))
                                                   error-thrown))
                                      (setq response-data nil)))))))))
    response-data))


;;; Bidirectional sync with conflict resolution ----------------------------------

(defun ticktick--apply-org-changes-to-file (org-file-path changes)
  "Apply ORG CHANGES to the org file at ORG-FILE-PATH."
  (with-current-buffer (find-file-noselect org-file-path)
    (dolist (added-task (plist-get changes :added))
      (ticktick--insert-org-task added-task))
    
    (dolist (modification (plist-get changes :modified))
      (let ((new-task (plist-get modification :new)))
        (ticktick--update-org-task new-task)))
    
    (dolist (deleted-task (plist-get changes :deleted))
      (ticktick--delete-org-task deleted-task))
    
    (save-buffer)))

(defun ticktick--insert-org-task (task)
  "Insert a new org TASK into the current buffer."
  (goto-char (point-max))
  (insert "\n* " (plist-get task :todo-state) " " (plist-get task :heading))
  (when-let ((tags (plist-get task :tags)))
    (insert " :" (mapconcat 'identity tags ":") ":"))
  (insert "\n")
  
  (when-let ((scheduled (plist-get task :scheduled)))
    (insert "SCHEDULED: ")
    (insert (format-time-string "<%Y-%m-%d %a>" scheduled))
    (insert "\n"))
  
  (when-let ((deadline (plist-get task :deadline)))
    (insert "DEADLINE: ")
    (insert (format-time-string "<%Y-%m-%d %a>" deadline))
    (insert "\n"))
  
  (when-let ((content (plist-get task :content)))
    (unless (string-blank-p content)
      (insert content "\n")))
  
  (let ((id (plist-get task :id)))
    (unless id
      (setq id (org-id-new))
      (plist-put task :id id))
    (insert ":PROPERTIES:\n:ID: " id "\n:END:\n")))

(defun ticktick--update-org-task (task)
  "Update an existing org TASK in the current buffer."
  (let ((id (plist-get task :id)))
    (when id
      (org-id-goto id)
      (org-back-to-heading t)
      (let ((start (point)))
        (org-end-of-subtree)
        (delete-region start (point))
        (goto-char start)
        (ticktick--insert-org-task task)))))

(defun ticktick--delete-org-task (task)
  "Delete an org TASK from the current buffer."
  (let ((id (plist-get task :id)))
    (when id
      (org-id-goto id)
      (org-back-to-heading t)
      (let ((start (point)))
        (org-end-of-subtree)
        (delete-region start (point))))))

(defun ticktick--sync-org-changes-to-ticktick (org-changes)
  "Sync ORG CHANGES to TickTick."
  (dolist (added-task (plist-get org-changes :added))
    (let ((created-task (ticktick--create-task added-task)))
      (when created-task
        (plist-put added-task :ticktick-id (plist-get created-task :id)))))
  
  (dolist (modification (plist-get org-changes :modified))
    (let ((new-task (plist-get modification :new)))
      (ticktick--update-task new-task)))
  
  (dolist (deleted-task (plist-get org-changes :deleted))
    (ticktick--delete-task deleted-task)))

(defun ticktick--sync-ticktick-changes-to-org (org-file-path ticktick-changes)
  "Sync TickTick CHANGES to org file."
  (let ((org-changes (list :added '() :modified '() :deleted '())))
    
    (dolist (added-task (plist-get ticktick-changes :added))
      (let ((org-task (ticktick--ticktick-task-to-org added-task)))
        (push org-task (plist-get org-changes :added))))
    
    (dolist (modification (plist-get ticktick-changes :modified))
      (let* ((new-ticktick-task (plist-get modification :new))
             (org-task (ticktick--ticktick-task-to-org new-ticktick-task)))
        (push (list :old nil :new org-task) (plist-get org-changes :modified))))
    
    (dolist (deleted-task (plist-get ticktick-changes :deleted))
      (let ((org-task (ticktick--ticktick-task-to-org deleted-task)))
        (push org-task (plist-get org-changes :deleted))))
    
    (ticktick--apply-org-changes-to-file org-file-path org-changes)))

(defun ticktick--resolve-conflicts (org-changes ticktick-changes)
  "Resolve conflicts between org and TickTick changes.
Returns a plist with :org-wins and :ticktick-wins changes."
  (let ((org-wins (copy-sequence org-changes))
        (ticktick-wins (copy-sequence ticktick-changes))
        (conflicts '()))
    
    (dolist (org-mod (plist-get org-changes :modified))
      (let* ((org-task-id (plist-get (plist-get org-mod :new) :id))
             (conflicting-ticktick-mod 
              (cl-find-if (lambda (tt-mod)
                           (let ((tt-task (plist-get tt-mod :new)))
                             (string= (plist-get tt-task :content) org-task-id)))
                         (plist-get ticktick-changes :modified))))
        (when conflicting-ticktick-mod
          (push (list :org org-mod :ticktick conflicting-ticktick-mod) conflicts)
          (setf (plist-get org-wins :modified)
                (cl-remove org-mod (plist-get org-wins :modified)))
          (setf (plist-get ticktick-wins :modified)
                (cl-remove conflicting-ticktick-mod (plist-get ticktick-wins :modified))))))
    
    (when conflicts
      (message "Conflict resolution: defaulting to org changes for %d conflicts" (length conflicts))
      (dolist (conflict conflicts)
        (push (plist-get conflict :org) (plist-get org-wins :modified))))
    
    (list :org-wins org-wins :ticktick-wins ticktick-wins :conflicts conflicts)))

;;;###autoload
(defun ticktick-sync ()
  "Perform bidirectional incremental sync between org and TickTick."
  (interactive)
  (let* ((org-file-path (ticktick--get-org-file-path))
         (snapshots (ticktick--get-current-snapshots))
         (old-org-snapshot (plist-get snapshots :org))
         (old-ticktick-snapshot (plist-get snapshots :ticktick)))
    
    (message "TickTick: Starting bidirectional sync...")
    
    (let* ((current-org-snapshot (ticktick--create-org-snapshot org-file-path))
           (current-org-tasks (plist-get current-org-snapshot :tasks))
           (current-ticktick-tasks (ticktick--get-all-tasks))
           (current-ticktick-snapshot (ticktick--create-ticktick-snapshot current-ticktick-tasks)))
      
      (let* ((org-changes (if old-org-snapshot
                             (ticktick--diff-org-tasks 
                              (plist-get old-org-snapshot :tasks)
                              current-org-tasks)
                           (list :added current-org-tasks :modified '() :deleted '())))
             (ticktick-changes (if old-ticktick-snapshot
                                  (ticktick--diff-ticktick-tasks
                                   (plist-get old-ticktick-snapshot :tasks)
                                   current-ticktick-tasks)
                                (list :added current-ticktick-tasks :modified '() :deleted '()))))
        
        (if (and (not (plist-get org-changes :added))
                 (not (plist-get org-changes :modified)) 
                 (not (plist-get org-changes :deleted))
                 (not (plist-get ticktick-changes :added))
                 (not (plist-get ticktick-changes :modified))
                 (not (plist-get ticktick-changes :deleted)))
            (message "TickTick: No changes detected")
          
          (let ((resolution (ticktick--resolve-conflicts org-changes ticktick-changes)))
            (let ((org-wins (plist-get resolution :org-wins))
                  (ticktick-wins (plist-get resolution :ticktick-wins)))
              
              (when (or (plist-get org-wins :added)
                       (plist-get org-wins :modified)
                       (plist-get org-wins :deleted))
                (message "TickTick: Syncing org changes to TickTick...")
                (ticktick--sync-org-changes-to-ticktick org-wins))
              
              (when (or (plist-get ticktick-wins :added)
                       (plist-get ticktick-wins :modified)
                       (plist-get ticktick-wins :deleted))
                (message "TickTick: Syncing TickTick changes to org...")
                (ticktick--sync-ticktick-changes-to-org org-file-path ticktick-wins))
              
              (let ((final-org-snapshot (ticktick--create-org-snapshot org-file-path))
                    (final-ticktick-tasks (ticktick--get-all-tasks)))
                (ticktick--update-snapshots 
                 final-org-snapshot
                 (ticktick--create-ticktick-snapshot final-ticktick-tasks)))
              
              (message "TickTick: Sync completed successfully"))))))))

;;; Additional utilities -------------------------------------------------------

;;;###autoload
(defun ticktick-clear-snapshots ()
  "Clear all stored snapshots and force a full sync on next run."
  (interactive)
  (setq ticktick-snapshots nil)
  (when (file-exists-p ticktick-snapshots-file)
    (delete-file ticktick-snapshots-file))
  (message "TickTick: Snapshots cleared. Next sync will be a full sync."))

;;;###autoload
(defun ticktick-show-sync-status ()
  "Show current sync status and snapshot information."
  (interactive)
  (let ((snapshots (ticktick--get-current-snapshots)))
    (with-output-to-temp-buffer "*TickTick Sync Status*"
      (princ "=== TickTick Sync Status ===\n\n")
      
      (let ((org-snapshot (plist-get snapshots :org))
            (ticktick-snapshot (plist-get snapshots :ticktick))
            (sync-timestamp (plist-get snapshots :sync-timestamp)))
        
        (if sync-timestamp
            (princ (format "Last sync: %s\n\n" 
                          (format-time-string "%Y-%m-%d %H:%M:%S" 
                                            (seconds-to-time sync-timestamp))))
          (princ "No previous sync found\n\n"))
        
        (if org-snapshot
            (let ((org-tasks (plist-get org-snapshot :tasks)))
              (princ (format "Org snapshot: %d tasks\n" (length org-tasks)))
              (princ (format "Org file: %s\n" (plist-get org-snapshot :file)))
              (princ (format "Org timestamp: %s\n" 
                            (format-time-string "%Y-%m-%d %H:%M:%S" 
                                              (seconds-to-time (plist-get org-snapshot :timestamp))))))
          (princ "No org snapshot found\n"))
        
        (princ "\n")
        
        (if ticktick-snapshot
            (let ((tt-tasks (plist-get ticktick-snapshot :tasks)))
              (princ (format "TickTick snapshot: %d tasks\n" (length tt-tasks)))
              (princ (format "TickTick timestamp: %s\n" 
                            (format-time-string "%Y-%m-%d %H:%M:%S" 
                                              (seconds-to-time (plist-get ticktick-snapshot :timestamp))))))
          (princ "No TickTick snapshot found\n"))))))

;;;###autoload
(defun ticktick-force-full-sync ()
  "Force a full bidirectional sync, ignoring snapshots."
  (interactive)
  (ticktick-clear-snapshots)
  (ticktick-sync))

(provide 'ticktick)
;;; ticktick.el ends here

