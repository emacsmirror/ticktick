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
  "OAuth2 redirect URI for TickTick. It should be http://localhost by default."
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
