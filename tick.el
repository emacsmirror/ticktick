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
         "tasks:write"
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
