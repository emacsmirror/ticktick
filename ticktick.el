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

(defcustom ticktick-org-file nil
  "Org file to sync with TickTick. If nil, uses current buffer when syncing."
  :type '(choice (const :tag "Current buffer" nil)
                 (file :tag "Org file"))
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


(provide 'ticktick)
;;; ticktick.el ends here

