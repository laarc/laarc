; HTTP Server.

; To improve performance with static files, set static-max-age*.

(= arcdir* "arc/" logdir* "arc/logs/" staticdir* "static/")

(or= quitsrv* nil breaksrv* nil killreq* (no (readenv "DEV" nil)))

(def serve ((o port 8080))
  (wipe quitsrv*)
  (ensure-srvdirs)
  (map [apply new-bgthread _] pending-bgthreads*)
  (w/socket s port
    (setuid 2) ; XXX switch from root to pg
    (prn "ready to serve port " port)
    (flushout)
    (= currsock* s)
    (until quitsrv*
      (handle-request s breaksrv*)))
  (prn "quit server"))

(def serve1 ((o port 8080))
  (w/socket s port (handle-request s t)))

(def ensure-srvdirs ()
  (map ensure-dir (list arcdir* logdir* staticdir*)))

(or= srv-noisy* nil)

; http requests currently capped at 2 meg by socket-accept

; should threads process requests one at a time? no, then
; a browser that's slow consuming the data could hang the
; whole server.

; wait for a connection from a browser and start a thread
; to handle it. also arrange to kill that thread if it
; has not completed in threadlife* seconds.

(or= threadlife* 30  requests* 0  requests/ip* (table)  
     throttle-ips* (table)  ignore-ips* (table)  spurned* (table))

(def handle-request (s breaksrv)
  (if breaksrv
      (handle-request-1 s)
      (errsafe (handle-request-1 s))))

(def handle-request-1 (s)
  (let (i o ip) (socket-accept s)
    (if (and (or (ignore-ips* ip) (abusive-ip ip))
             (++ (spurned* ip 0)))
        (force-close i o)
        (do (with (th1 nil th2 nil)
              (= th1 (thread
                       (after (handle-request-thread i o ip)
                              (close i o)
                              (kill-thread th2))))
              (= th2 (thread
                       (sleep threadlife*)
                       (when killreq*
                         (unless (dead th1)
                           (prn "srv thread took too long for " ip))
                         (break-thread th1)
                         (force-close i o)))))))))

; Returns true if ip has made req-limit* requests in less than
; req-window* seconds.  If an ip is throttled, only 1 request is 
; allowed per req-window* seconds.  If an ip makes req-limit* 
; requests in less than dos-window* seconds, it is a treated as a DoS
; attack and put in ignore-ips* (for this server invocation).

; To adjust this while running, adjust the req-window* time, not 
; req-limit*, because algorithm doesn't enforce decreases in the latter.

(or= req-times* (table) req-limit* 30 req-window* 10 dos-window* 2 dos-protection* nil)

(def abusive-ip (ip)
  (and dos-protection*
       (only.> (requests/ip* ip) 250)
       (let now (seconds)
         (do1 (if (req-times* ip)
                  (and (>= (qlen (req-times* ip)) 
                           (if (throttle-ips* ip) 1 req-limit*))
                       (let dt (- now (deq (req-times* ip)))
                         (if (< dt dos-window*) (set (ignore-ips* ip)))
                         (< dt req-window*)))
                  (do (= (req-times* ip) (queue))
                      nil))
              (enq now (req-times* ip))))))

(def noisy-header (lines (o label) (o footer "\n"))
  (when srv-noisy*
    (atomic
      (if label (pr label))
      (each line lines
        (prn line))
      (if footer (pr footer))
      (flushout)))
  lines)

(def handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil t0 (msec))
    (after
      (whilet c (unless responded (readc i))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (withs (h (noisy-header (rev lines) (+ "Time: " (moment-ms) "\nHeader: "))
                        (type op args n cooks ip . more) (parseheader h ip))
                  (let t1 (msec)
                    (case type
                      get  (respond o op args cooks ip)
                      post (handle-post i o op args n cooks ip)
                           (respond-err o "Unknown request: " (car lines)))
                    (log-request type op args cooks ip t0 t1)
                    (set responded)))
                (do (push (string (rev line)) lines)
                    (wipe line)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close i o)))
  (harvest-fnids))

(def log-request (type op args cooks ip t0 t1)
  (with (parsetime (- t1 t0) respondtime (- (msec) t1))
    (srvlog 'srv ip 
                 parsetime 
                 respondtime 
                 (if (> (+ parsetime respondtime) 1000) "***" "")
                 type
                 op
                 (let arg1 (car args)
                   (if (caris arg1 "fnid") ""
                       (caris arg1 "X-Arc-Secure") ""
                       arg1))
                 cooks)))

; Could ignore return chars (which come from textarea fields) here by
; (unless (is c #\return) (push c line))

(def handle-post (i o op args n cooks ip)
  (if (no n)
      (respond-err o "Post request without Content-Length.")
      (let line nil
        (whilet c (and (> n 0) (readc i))
          (-- n)
          (push c line)) 
        (let line (string (rev line))
          (noisy-header (list line) "Post Contents: ")
          (respond o op (+ (parseargs line) args) cooks ip)))))

(or= type-header* (table))

(def gen-type-header (ctype)
  (+ "HTTP/1.0 200 OK
Content-Type: "
     ctype
     "
Connection: close"))

(= header* (gen-type-header "text/html; charset=utf-8"))
(= secure-header* "X-Frame-Options: DENY
X-Content-Type-Options: nosniff
X-XSS-Protection: 1; mode=block
Strict-Transport-Security: max-age=31556900"
;Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline' https://www.google.com/recaptcha/ https://www.gstatic.com/recaptcha/ https://cdnjs.cloudflare.com/; frame-src 'self' https://www.google.com/recaptcha/; style-src 'self' 'unsafe-inline'"
)

(map (fn ((k v)) (= (type-header* k) (gen-type-header v)))
     '((image/x-icon "image/x-icon")
       (image/svg "image/svg+xml")
       (image/gif "image/gif")
       (image/jpg "image/jpeg")
       (image/png "image/png")
       (text/plain "text/plain")
       (text/html "text/html; charset=utf-8")
       (text/css  "text/css")
       (text/xml  "text/xml")
       (text/csv  "text/csv")
       (application/json        "application/json")
       (application/javascript  "application/javascript")
       (application/manifest    "application/manifest+json")
       ))

(= rdheader* "HTTP/1.0 302 Moved")

(or= srvops* (table) redirector* (table) optimes* (table) opcounts* (table))

(def save-optime (name elapsed)
  ; this is the place to put a/b testing
  ; toggle a flag and push elapsed into one of two lists
  (++ (opcounts* name 0))
  (unless (optimes* name) (= (optimes* name) (queue)))
  (enq-limit elapsed (optimes* name) 1000))

; For ops that want to add their own headers.  They must thus remember 
; to prn a blank line before anything meant to be part of the page.

(mac defop-raw (name parms . body)
  (w/uniq t1
    `(= (srvops* ',name) 
        (fn ,parms 
          (let ,t1 (msec)
            (do1 (do ,@body)
                 (save-optime ',name (- (msec) ,t1))))))))

(mac defopr-raw (name parms . body)
  `(= (redirector* ',name) t
      (srvops* ',name)     (fn ,parms ,@body)))

(mac defop (name parm . body)
  (w/uniq gs
    `(do (wipe (redirector* ',name))
         (defop-raw ,name (,gs ,parm) 
           (w/stdout ,gs (prn) ,@body)))))

; Defines op as a redirector.  Its retval is new location.

(mac defopr (name parm . body)
  (w/uniq gs
    `(do (set (redirector* ',name))
         (defop-raw ,name (,gs ,parm)
           ,@body))))

;(mac testop (name . args) `((srvops* ',name) ,@args))

(deftem request
  args  nil
  cooks nil
  ip    nil)

(= unknown-msg* "Unknown.")
(or= max-age* (table) static-header* (table) static-max-age* nil)

(def respond (str op args cooks ip)
  (or (hook 'respond str op args cooks ip)
      (w/stdout str
        (let (op args) (parseop op args)
          (iflet f (srvops* op)
                 (let req (the-req*)
                   (= req!op op
                      req!args args)
                   (if (redirector* op)
                       (do (prn rdheader*)
                           (aand (or (hook 'respond-headers str req f t)
                                     (f str req)
                                     "/")
                                 (prn "Location: " it))
                           (prn))
                       (do (prn (aif (static-header* op) (gen-type-header it) header*))
                           (awhen (max-age* op)
                             (prn "Cache-Control: max-age=" it))
                           (when (srvsecure req)
                             (prn secure-header*))
                           (or (hook 'respond-headers str req f nil)
                               (f str req)))))
                 (withs (op (car (tokens op #\?))
                         filetype (static-filetype op))
                   (aif (and filetype (file-exists (string staticdir* op)))
                        (do (prn (or (static-header* op) (type-header* filetype)))
                            (awhen static-max-age*
                              (prn "Cache-Control: max-age=" it))
                            (prn)
                            (w/infile i it
                              (whilet b (readb i)
                                (writeb b str))))
                        (respond-err str unknown-msg*))))))))

(def static-filetype (sym)
  (let fname (str sym)
    (and (~find #\/ fname)
         (case (downcase (last (check (tokens fname #\.) ~single)))
           "ico"  'image/x-icon
           "svg"  'image/svg
           "gif"  'image/gif
           "jpg"  'image/jpg
           "jpeg" 'image/jpg
           "png"  'image/png
           "css"  'text/css
           "txt"  'text/plain
           "htm"  'text/html
           "html" 'text/html
           "arc"  'text/plain
           "csv"  'text/csv
           "xml"  'text/xml
           "js"           'application/javascript
           "json"         'application/json
           "webmanifest"  'application/manifest
           ))))

(def static-src (filename)
  (string "/" filename "?" (shashfile (+ staticdir* filename))))

(seval '(require file/sha1))

(= sha1 seval!sha1)

(def shash (str)
  (sha1 (instring str)))

(defmemo shashfile-1 (filename modtime)
  (w/infile i filename
    (sha1 i)))

(def shashfile (filename)
  (shashfile-1 filename (modtime filename)))

(def respond-err (str msg . args)
  (w/stdout str
    (prn header*)
    (prn)
    (apply pr msg args)))

(def parseop (op args)
  (let xs (tokens op #\/)
    (if (< (len xs) 2)
        (list op args)
        (list (sym (car xs)) (join (list (list "path" (concat (map urldecode (cdr xs)) #\/))) args)))))

(or= the-header* (make-param nil)
     the-req* (make-param nil))

(def srvsecure ((o req (the-req*)))
  (is (arg req "X-Arc-Secure") "1"))

(def parsereq (xs)
  (let (type op args n cooks ip . more) xs
    (the-req* (inst 'request 'type type 'op op 'args args 'cooks cooks 'n n 'ip ip 'more more))
    (++ requests*)
    (++ (requests/ip* ip 0))
    xs))

(def parseheader (lines (o ip))
  (the-header* lines)
  (let (type op args) (parseurl (car lines))
    (parsereq
      (list type
            op
            (+ args
               (rem nil (map [when (begins _ "X-Arc-")
                               (tokens _ [or (whitec _) (is _ #\:)])]
                             (cdr lines))))
            (and (is type 'post)
                 (some (fn (s)
                         (and (begins s "Content-Length:")
                              (errsafe:coerce (cadr (tokens s)) 'int)))
                       (cdr lines)))
            (some (fn (s)
                    (and (or (begins s "Cookie:")
                             (begins s "cookie:"))
                         (parsecookies s)))
                  (cdr lines))
            (or (some (fn (s)
                         (and (begins s "CF-Connecting-IP:")
                              (errsafe:cadr (tokens s))))
                       (cdr lines))
                 ip)))))

; (parseurl "GET /p1?foo=bar&ug etc") -> (get p1 (("foo" "bar") ("ug")))

(def parseurl (s)
  (let (type url) (tokens s)
    (let (base args) (tokens url #\?)
      (list (sym (downcase type))
            (sym (cut base 1))
            (if args
                (parseargs args)
                nil)))))

; I don't urldecode field names or anything in cookies; correct?

(def parseargs (s)
  (map (fn ((k v)) (list k (urldecode v)))
       (map [tokens _ #\=] (tokens s #\&))))

(def parsecookies (s)
  (map [tokens _ #\=] 
       (cdr (tokens s [or (whitec _) (is _ #\;)]))))

(def arg args
  (case (len args)
    0 ((the-req*) 'args)
    1 (apply arg (the-req*) args)
    (let (req key) args
      (alref req!args (str key)))))

; *** Warning: does not currently urlencode args, so if need to do
; that replace v with (urlencode v).

(def reassemble-args (req)
  (aif req!args
       (apply string "?" (intersperse '&
                                      (map (fn ((k v))
                                             (string k '= v))
                                           it)))
       ""))

(or= fns* (table) fnkeys* (table) fnids* (table) timed-fnids* (table))

(def lexval (e)
  (each (id getx setx) e
    (out (if (isa getx 'fn) (getx) getx))))

(mac lexkey () `(lexval (lexenv)))

; count on huge (expt 64 22) size of fnid space to avoid clashes

(def gen-fnid ()
  (sym:rand-string 22))

(def new-fnid ((o k))
  (if k 
      (or= (fnkeys* k) (gen-fnid))
      (gen-fnid)))

(def fnidf (f (o k))
  (atlet key (new-fnid k)
    (= (fns* key) f
       (fnids* key) (list (now) (get-user)))
    (wipe (timed-fnids* key))
    key))

(def timed-fnidf (lasts f (o k))
  (atlet key (new-fnid k)
    (= (fns* key) f
       (timed-fnids* key) (list (seconds) lasts (get-user)))
    (wipe (fnids* key))
    key))

(mac fnid             (f (o k '(lexkey))) `(fnidf ,f ,k))
(mac timed-fnid (lasts f (o k '(lexkey))) `(timed-fnidf ,lasts ,f ,k))

; Within f, it will be bound to the fn's own fnid.  Remember that this is
; so low-level that need to generate the newline to separate from the headers
; within the body of f.

(mac afnid (f)
  `(atlet it (new-fnid (lexkey))
     (= (fns* it) ,f
        (fnids* it) (list (now) (get-user)))
     (wipe (timed-fnids* it))
     it))

;(defop test-afnid req
;  (tag (a href (url-for (afnid (fn (req) (prn) (pr "my fnid is " it)))))
;    (pr "click here")))

; To be more sophisticated, instead of killing fnids, could first 
; replace them with fns that tell the server it's harvesting too 
; aggressively if they start to get called.  But the right thing to 
; do is estimate what the max no of fnids can be and set the harvest 
; limit there-- beyond that the only solution is to buy more memory.

(= fnid-harvest-max*   50000 ; was 20000
   fnid-harvest-ratio* 10
   fnid-hours-max*     6)

(def fnids ((o getter car))
  (map getter (sortable fnids* < car)))

(def kill-fnid (id)
  (wipe (fnids* id) (timed-fnids* id) (fns* id)) t)

(def user-fnids (user)
  (sort (compare > car)
        (each (id (created subj)) fnids*
          (when (is subj user)
            (out (minutes-since created t) id)))))

(def dead-fnids ((o max-hours fnid-hours-max*))
  (+ (each (id (created lasts)) timed-fnids*
       (when (> (since created) lasts)    
         (out id)))
     (each (id (ms user)) fnids*
       (when (>= (hours-since ms t) max-hours)
         (out id)))))

(def harvest-fnids ((o force nil)
                    (o n fnid-harvest-max*)
                    (o r fnid-harvest-ratio*))
  (when (or force (len> fns* n))
    (each id (dead-fnids)
      (kill-fnid id))
    (when (or force (len> fns* n))
      (atlet nharvest (trunc (/ (min n (len fns*)) r))
        (let (kill keep) (split (fnids) nharvest)
          (each id kill
            (kill-fnid id)))))))

(= fnurl* "/x" rfnurl* "/r" rfnurl2* "/y" jfnurl* "/a")

(= todo-msg* (tostring:gentag img src "/todo.gif" width 18 height 18)
   dead-msg* (+ "\nUnknown or expired link. " (tostring:br) todo-msg*))
 
(defop-raw x (str req)
  (w/stdout str 
    (aif (fns* (sym (arg req "fnid")))
         (it req)
         (pr dead-msg*))))

(defopr-raw y (str req)
  (aif (fns* (sym (arg req "fnid")))
       (w/stdout str (it req))
       "deadlink"))

; For asynchronous calls; discards the page.  Would be better to tell
; the fn not to generate it.

(defop-raw a (str req)
  (aif (fns* (sym (arg req "fnid")))
       (tostring (it req))))

(defopr r req
  (aif (fns* (sym (arg req "fnid")))
       (it req)
       "deadlink"))

(defop deadlink req
  (pr dead-msg*))

(def url-for (fnid)
  (string fnurl* "?fnid=" fnid))

(def flinkf (f (o k))
  (string fnurl* "?fnid=" (fnid (fn (req) (prn) (f req)) k)))

(def rflinkf (f (o k))
  (string rfnurl* "?fnid=" (fnid f k)))

(mac flink  (f (o k '(lexkey))) `(flinkf ,f ,k))
(mac rflink (f (o k '(lexkey))) `(rflinkf ,f ,k))
  
; Since it's just an expr, gensym a parm for (ignored) args.

(mac w/link (expr . body)
  `(tag (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(mac w/rlink (expr . body)
  `(tag (a href (rflink (fn (,(uniq)) ,expr)))
     ,@body))

(mac onlink (text . body)
  `(w/link (do ,@body) (pr ,text)))

(mac onrlink (text . body)
  `(w/rlink (do ,@body) (pr ,text)))

; bad to have both flink and linkf; rename flink something like fnid-link

(mac linkf (text parms . body)
  `(tag (a href (flink (fn ,parms ,@body))) (pr ,text)))

(mac rlinkf (text parms . body)
  `(tag (a href (rflink (fn ,parms ,@body))) (pr ,text)))

;(defop top req (linkf 'whoami? (req) (pr "I am " (get-user req))))

;(defop testf req (w/link (pr "ha ha ha") (pr "laugh")))

(mac w/link-if (test expr . body)
  `(tag-if ,test (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(def fnid-field (id)
  (gentag input type 'hidden name 'fnid value id))

; f should be a fn of one arg, which will be http request args.

(def fnformf (f bodyfn (o redir) (o k))
  (tag (form method 'post action (if redir rfnurl2* fnurl*))
    (fnid-field (fnid f k))
    (bodyfn)))

(mac fnform (f bodyfn redir (o k '(lexkey)))
  `(fnformf ,f ,bodyfn ,redir ,k))

; Could also make a version that uses just an expr, and var capture.
; Is there a way to ensure user doesn't use "fnid" as a key?

(mac aform (f . body)
  (w/uniq ga
    `(tag (form method 'post action fnurl*)
       (fnid-field (fnid (fn (,ga)
                           (prn)
                           (,f ,ga))))
       ,@body)))

;(defop test1 req
;  (fnform (fn (req) (prn) (pr req))
;          (fn () (single-input "" 'foo 20 "submit"))))
 
;(defop test2 req
;  (aform (fn (req) (pr req))
;    (single-input "" 'foo 20 "submit")))

; Like aform except creates a fnid that will last for lasts seconds
; (unless the server is restarted).

(mac taform (lasts f . body)
  (w/uniq (gl gf gi ga ge)
    `(withs (,gl ,lasts
             ,ge (lexkey)
             ,gf (fn (,ga) (prn) (,f ,ga)))
       (tag (form method 'post action fnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf ,ge) (fnid ,gf ,ge)))
         ,@body))))

(mac arform (f . body)
  `(tag (form method 'post action rfnurl*)
     (fnid-field (fnid ,f))
     ,@body))

; overlong

(mac tarform (lasts f . body)
  (w/uniq (gl gf ge)
    `(withs (,ge (lexkey) ,gl ,lasts ,gf ,f)
       (tag (form method 'post action rfnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf ,ge) (fnid ,gf ,ge)))
         ,@body))))

(mac aformh (f . body)
  `(tag (form method 'post action fnurl*)
     (fnid-field (fnid ,f))
     ,@body))

(mac arformh (f . body)
  `(tag (form method 'post action rfnurl2*)
     (fnid-field (fnid ,f))
     ,@body))

; only unique per server invocation

(or= unique-ids* (table))

(def unique-id ((o len 32))
  (let id (sym (rand-string (max 5 len)))
    (if (unique-ids* id)
        (unique-id)
        (= (unique-ids* id) id))))

(def srvlog (type . args)
  (w/appendfile o (logfile-name type)
    (w/stdout o (atomic (apply prs (seconds) args) (prn)))))

(def logfile-name (type)
  (string logdir* type "-" (memodate)))

(with (lastasked nil lastval nil)

(def memodate ()
  (let now (seconds)
    (if (or (no lastasked) (> (- now lastasked) 60))
        (= lastasked now lastval (datestring))
        lastval)))

)

(unless (srvops* '||)
  (defop || req (pr "It's alive.")))

(defop topips req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (each ip (let leaders nil 
                   (each (ip n) requests/ip*
                     (when (>= n 1)
                       (insort (compare > requests/ip*)
                               ip
                               leaders)))
                   leaders)
          (let n (requests/ip* ip)
            (row ip n (pr (num (* 10 (/ n requests*)) 1)))))))))

(defop spurned req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (map (fn ((ip n)) (row ip n))
             (sortable spurned* >))))))

; eventually promote to general util

(def sortable (ht (o f >) (o key idfn))
  (sort (compare f key:cadr) (each kv ht (out kv))))


; Background Threads

(or= bgthreads* (table) pending-bgthreads* nil)

(def new-bgthread (id f sec)
  (aif (bgthreads* id) (break-thread it))
  (= (bgthreads* id) (new-thread (fn () 
                                   (while t
                                     (sleep sec)
                                     (f))))))

; should be a macro for this?

(mac defbg (id sec . body)
  `(do (pull [caris _ ',id] pending-bgthreads*)
       (push (list ',id (fn () ,@body) ,sec) 
             pending-bgthreads*)))


; reload changed code each request

(def noisy-reload ()
  (awhen (any:reload)
    (write it)
    (prn)))

(when (readenv "DEV" nil)
  (defhook respond (str op args cooks ips . rest)
    (let (op args) (parseop op args)
      (when (srvops* op)
        (prn op)
        (noisy-reload)))
    nil))

; pull from github periodically

(or= git-pull-time* (readenv "PULL" nil)
     git-pull-count* 0)

(def git-pull-reload ()
  (when (errsafe:git-pull)
    (when (readenv "NUKE")
      (errsafe:git-reset-to-origin t))
    (++ git-pull-count*))
  (errsafe:reload))

(def git-pull-stats ()
  (git-pull-reload)
  (cons git-pull-count* (reload-stats)))

(awhen git-pull-time*
  (defbg git-pull it (git-pull-reload)))


; Idea: make form fields that know their value type because of
; gensymed names, and so the receiving fn gets args that are not
; strings but parsed values.

