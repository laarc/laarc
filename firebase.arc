(= firebase-url* "https://laarrc.firebaseio.com/"
   hn-firebase-url* "https://hacker-news.firebaseio.com/"
   firebase-write* (no (readenv "DEV")))

(defcache firebase-token 600
  (when (file-exists "../firetoken.py")
    (let x (trim:fromstring "" (shell 'python "../firetoken.py"))
      (unless (blank x) x))))

(def firebase-set (path value (o auth firebase-write*) (o url firebase-url*))
  (when (and auth (firebase-token))
    (thread
      (fromstring (tostring:write-json value)
        (shell 'curl '-fsSL '-X 'PUT '-d (string #\@ "-")
               (string url path ".json?access_token=" (firebase-token))))))
  value)

(def firebase-get (path (o auth) (o url firebase-url*))
  (fromstring
    (fromstring ""
      (shell 'curl '-fsSL
             (string url path ".json"
                     (if auth (string "?access_token=" (firebase-token)) ""))))
    (read-json (stdin))))
