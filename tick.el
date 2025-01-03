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
  "Tickel client secret."
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
