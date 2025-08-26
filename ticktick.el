;;; ticktick.el --- Org ↔ TickTick with OAuth callback server -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Package-Requires: ((emacs "26.1") (request "0.3.0") (simple-httpd "1.5.0"))
;; Keywords: tools, ticktick, org, tasks, todo
;; URL: https://github.com/your-username/ticktick.el

;;; Commentary:
;; ticktick.el provides two-way sync between TickTick and emacs org-mode,

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

(defcustom ticktick-sync-file (expand-file-name "ticktick.org" ticktick-dir)
  "Path to the org file where all TickTick tasks will be synchronized."
  :type 'file
  :group 'ticktick)

(defcustom ticktick-snapshot-file
  (expand-file-name ".ticktick-snapshot" ticktick-dir)
  "File to store org file snapshots for incremental sync."
  :type 'file
  :group 'ticktick)

(defcustom ticktick--autosync nil
  "If non-nil, automatically sync TickTick when switching buffers or Emacs loses focus."
  :type 'boolean
  :group 'ticktick)

(defcustom ticktick-httpd-port 8080
  "Local port for the OAuth callback server."
  :type 'integer
  :group 'ticktick)

(defcustom ticktick-redirect-uri "http://localhost:8080/ticktick-callback"
  "Redirect URI registered with TickTick. Must match OAuth app settings."
  :type 'string
  :group 'ticktick)

(defvar ticktick-token nil
  "Access token plist for accessing the TickTick API.")

(defvar ticktick-oauth-state nil
  "CSRF-prevention state for the OAuth flow.")

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

;;; Snapshot and incremental sync helpers -----------------------------------

(defun ticktick--save-file-snapshot (file)
  "Save a snapshot of FILE content with timestamp."
  (when (file-exists-p file)
    (ticktick--ensure-dir)
    (let ((snapshot-data
           `((timestamp . ,(float-time))
             (content . ,(with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))
             (hash . ,(with-temp-buffer
                        (insert-file-contents file)
                        (secure-hash 'sha1 (current-buffer)))))))
      (with-temp-file ticktick-snapshot-file
        (insert (prin1-to-string snapshot-data))))))

(defun ticktick--load-file-snapshot ()
  "Load the last saved snapshot of the sync file."
  (when (file-exists-p ticktick-snapshot-file)
    (with-temp-buffer
      (insert-file-contents ticktick-snapshot-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun ticktick--get-file-changes ()
  "Get changes between current org file and stored snapshot.
   Returns (added . (modified . deleted)) task IDs."
  (let* ((snapshot (ticktick--load-file-snapshot))
         (snapshot-content (and snapshot (alist-get 'content snapshot)))
         (current-content (when (file-exists-p ticktick-sync-file)
                            (with-temp-buffer
                              (insert-file-contents ticktick-sync-file)
                              (buffer-string))))
         (snapshot-tasks (when snapshot-content
                           (ticktick--extract-tasks-from-content snapshot-content)))
         (current-tasks (when current-content
                          (ticktick--extract-tasks-from-content current-content)))
         added modified deleted)
    
    (when current-tasks
      ;; Find added tasks (in current but not in snapshot)
      (dolist (task current-tasks)
        (let ((id (plist-get task :id)))
          (if (and id (not (string-empty-p id)))
              ;; Existing task - check if modified
              (let ((snapshot-task (cl-find id snapshot-tasks
                                           :key (lambda (t) (plist-get t :id))
                                           :test #'string=)))
                (when (and snapshot-task
                          (not (string= (plist-get task :hash)
                                       (plist-get snapshot-task :hash))))
                  (push id modified)))
            ;; New task without ID
            (push task added))))
      
      ;; Find deleted tasks (in snapshot but not in current)
      (when snapshot-tasks
        (dolist (task snapshot-tasks)
          (let ((id (plist-get task :id)))
            (when (and id (not (string-empty-p id)))
              (unless (cl-find id current-tasks
                             :key (lambda (t) (plist-get t :id))
                             :test #'string=)
                (push id deleted)))))))
    
    (list (cons 'added added)
          (cons 'modified modified)
          (cons 'deleted deleted))))

(defun ticktick--extract-tasks-from-content (content)
  "Extract task information from org CONTENT string."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (goto-char (point-min))
    (let (tasks)
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) 2) ;; Task level
           (let* ((id (org-entry-get nil "TICKTICK_ID"))
                  (etag (org-entry-get nil "TICKTICK_ETAG"))
                  (title (nth 4 (org-heading-components)))
                  (task-start (org-entry-beginning-position))
                  (task-end (org-entry-end-position))
                  (task-content (buffer-substring-no-properties task-start task-end))
                  (hash (secure-hash 'sha1 task-content)))
             (push `(:id ,id :etag ,etag :title ,title :hash ,hash
                         :start ,task-start :end ,task-end) tasks))))
       nil 'file)
      (reverse tasks))))

;;; Org conversion helpers -----------------------------------------------------

(defun ticktick--task-to-heading (task)
  "Convert TASK plist to an org heading string."
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
            (when content (string-trim content))))
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
              (forward-line)
              (while (looking-at org-planning-line-re)
                (forward-line))
              (when (looking-at ":PROPERTIES:")
                (re-search-forward "^:END:" nil t)
                (forward-line))
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
  "Return non-nil if the current subtree changed since last sync."
  (let* ((etag (org-entry-get nil "TICKTICK_ETAG"))
         (cached (org-entry-get nil "SYNC_CACHE"))
         (body (buffer-substring-no-properties
                (org-entry-beginning-position)
                (org-entry-end-position)))
         (changed (not (string= cached (secure-hash 'sha1 body)))))
    (or (not etag) changed)))

(defun ticktick--update-sync-meta ()
  "Set sync hash and time on current subtree."
  (let ((body (buffer-substring-no-properties (org-entry-beginning-position)
                                              (org-entry-end-position))))
    (org-set-property "LAST_SYNCED" (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
    (org-set-property "SYNC_CACHE" (secure-hash 'sha1 body))))

;;; Sync functions -------------------------------------------------------------

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
                     (save-excursion
                       (goto-char existing-pos)
                       (let ((existing-etag (org-entry-get nil "TICKTICK_ETAG")))
                         (unless (string= existing-etag etag)
                           (delete-region (org-entry-beginning-position)
                                          (org-entry-end-position))
                           (insert (ticktick--task-to-heading task))
                           (ticktick--update-sync-meta))))
                   (save-excursion
                     (goto-char project-pos)
                     (outline-next-heading)
                     (insert (ticktick--task-to-heading task) "\n")
                     (ticktick--update-sync-meta))))))))
       (save-buffer)
       (ticktick--save-file-snapshot ticktick-sync-file)))))

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
     (save-buffer)
     (ticktick--save-file-snapshot ticktick-sync-file))

(defun ticktick-sync-incremental ()
  "Perform incremental sync based on file changes since last snapshot."
  (interactive)
  (let ((changes (ticktick--get-file-changes)))
    (let ((added (alist-get 'added changes))
          (modified (alist-get 'modified changes))
          (deleted (alist-get 'deleted changes)))
      
      (when (or added modified deleted)
        (message "Incremental sync: %d added, %d modified, %d deleted"
                 (length added) (length modified) (length deleted))
        
        ;; Handle deleted tasks first
        (dolist (id deleted)
          (condition-case err
              (progn
                (ticktick-request "DELETE" (format "/open/v1/task/%s" id))
                (message "Deleted remote task: %s" id))
            (error (message "Failed to delete task %s: %s" id (error-message-string err)))))
        
        ;; Handle modified tasks
        (with-current-buffer (find-file-noselect ticktick-sync-file)
          (org-with-wide-buffer
           (dolist (id modified)
             (goto-char (point-min))
             (when (re-search-forward (format ":TICKTICK_ID: %s" (regexp-quote id)) nil t)
               (org-back-to-heading)
               (let* ((task (ticktick--heading-to-task))
                      (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t)))
                 (condition-case err
                     (progn
                       (ticktick-request "POST" (format "/open/v1/task/%s" id)
                                         (append task `(("projectId" . ,project-id))))
                       (ticktick--update-sync-meta)
                       (message "Updated remote task: %s" (alist-get "title" task)))
                   (error (message "Failed to update task %s: %s" id (error-message-string err)))))))
           
           ;; Handle added tasks (new tasks without IDs)
           (dolist (task added)
             (let ((title (plist-get task :title))
                   (start-pos (plist-get task :start)))
               (goto-char start-pos)
               (let* ((task-data (ticktick--heading-to-task))
                      (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t)))
                 (condition-case err
                     (let ((resp (ticktick-request "POST" "/open/v1/task"
                                                   (append task-data `(("projectId" . ,project-id))))))
                       (when resp
                         (org-set-property "TICKTICK_ID" (plist-get resp :id))
                         (org-set-property "TICKTICK_ETAG" (plist-get resp :etag))
                         (ticktick--update-sync-meta)
                         (message "Created remote task: %s" (plist-get resp :title))))
                   (error (message "Failed to create task %s: %s" title (error-message-string err)))))))
           
           (save-buffer)
           (ticktick--save-file-snapshot ticktick-sync-file)))
        
        (message "Incremental sync completed"))
      
      (unless (or added modified deleted)
        (message "No changes detected - sync skipped")))))

(defun ticktick-sync-two-way ()
  "Two-way sync: fetch and push."
  (interactive)
  (ticktick-fetch-to-org)
  (ticktick-push-from-org))

(defun ticktick--autosync ()
  "Autosync if enabled."
  (when ticktick--autosync
    (when (file-exists-p ticktick-sync-file)
      (ignore-errors (ticktick-sync-two-way)))))

;;;###autoload
(defun ticktick-force-full-sync ()
  "Force a complete sync, ignoring incremental changes."
  (interactive)
  (when (file-exists-p ticktick-snapshot-file)
    (delete-file ticktick-snapshot-file)
    (message "Snapshot cleared - forcing full sync"))
  (ticktick-sync-two-way))

;;; Task creation --------------------------------------------------------------

;; (defun ticktick-create-task ()
;;   "Create new task from heading."
;;   (interactive)
;;   (let* ((project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))
;;          (task (ticktick--heading-to-task))
;;          (resp (ticktick-request "POST" "/open/v1/task"
;;                                  (append task `(("projectId" . ,project-id))))))
;;     (when resp
;;       (org-set-property "TICKTICK_ID" (plist-get resp :id))
;;       (org-set-property "TICKTICK_ETAG" (plist-get resp :etag))
;;       (message "Created task: %s" (plist-get resp :title)))))

(add-hook 'focus-out-hook #'ticktick--autosync)
(add-hook 'window-buffer-change-functions (lambda (&rest _) (ticktick--autosync)))

(provide 'ticktick)
;;; ticktick.el ends here

