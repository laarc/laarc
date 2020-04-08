; Application Server.  Layer inserted 2 Sep 06.

; ideas: 
; def a general notion of apps of which prompt is one, news another
; give each user a place to store data?  A home dir?

; A user is simply a string: "pg". Use /whoami to test user cookie.

(= hpwfile*   "arc/hpw"
   oidfile*   "arc/openids"
   adminfile* "arc/admins"
   cookfile*  "arc/cooks")

(def asv ((o port 8080))
  (load-userinfo)
  (serve port))

(def load-userinfo ()
  (= hpasswords*   (safe-load-table hpwfile*)
     openids*      (safe-load-table oidfile*)
     admins*       (map string (errsafe (readfile adminfile*)))
     cookie->user* (safe-load-table cookfile*))
  (each (k v) cookie->user*
    (= (user->cookie* v) k))
  t)

; idea: a bidirectional table, so don't need two vars (and sets)

(or= cookie->user* (table) user->cookie* (table) user->email* (table) logins* (table))

(def get-ip ((o req (the-req*)))
  (or req!ip "::1"))

(def get-cookie (key (o req (the-req*)))
  (alref req!cooks key))

(def get-user ((o req (the-req*)))
  (let u (aand (get-cookie "user" req)
               (cookie->user* (sym it)))
    (when u (= (logins* u) (get-ip req)))
    u))

(defmemo auth-hash (cookie)
  (shash:string cookie))

(def get-auth ((o user (get-user)))
  (aand user
        (user->cookie* user)
        (auth-hash it)))

(def is-auth (auth (o user (get-user)))
  (is auth (get-auth user)))

(mac when-umatch (user req . body)
  `(if (is ,user (get-user ,req))
       (do ,@body)
       (mismatch-message)))

(def mismatch-message () 
  (prn "Dead link: users don't match."))

(mac when-umatch/r (user req . body)
  `(if (is ,user (get-user ,req))
       (do ,@body)
       "mismatch"))

(defop mismatch req (mismatch-message))

(mac uform (user req after . body)
  `(aform (fn (,req)
            (when-umatch ,user ,req
              ,after))
     ,@body))

(mac urform (user req after . body)
  `(arform (fn (,req)
             (when-umatch/r ,user ,req 
               ,after))
     ,@body))

; Like onlink, but checks that user submitting the request is the
; same it was generated for.  For extra protection could log the 
; username and ip addr of every genlink, and check if they match.

(mac ulink (user text . body)  
  (w/uniq req
    `(linkf ,text (,req) 
       (when-umatch ,user ,req ,@body))))


(defop admin req (admin-gate (get-user req)))

(def admin-gate (u)
  (if (admin u)
      (admin-page u)
      (login-page 'login nil
                  (fn (u ip)  (admin-gate u)))))

(def admin ((o u (get-user))) (and u (mem u admins*)))

(def user-exists (u) (and u (hpasswords* u) u))

(def admin-page (user . msg)
  (whitepage 
    (prbold "Admin: ")
    (hspace 20)
    (pr user " | ")
    (w/link (do (logout-user user)
                (whitepage (pr "Bye " user ".")))
      (pr "logout"))
    (when msg (hspace 10) (map pr msg))
    (br2)
    (aform (fn (req)
             (when-umatch user req
               (with (u (arg req "acct") p (arg req "pw"))
                 (if (or (no u) (no p) (is u "") (is p ""))
                      (pr "Bad data.")
                     (user-exists u)
                      (admin-page user "User already exists: " u)
                      (do (create-acct u p)
                          (admin-page user))))))
      (pwfields "create (server) account"))))

(def cook-user ((o user (get-user)) (o cookie (get-cookie "user")) (o alt))
  (when user
    (let id (if (is cookie t) (new-user-cookie user) cookie)
      (unless alt
        (= (user->cookie* user) id))
      (unless (is (cookie->user* id) user)
        (= (cookie->user* id) user)
        (save-table cookie->user* cookfile*))
      id)))

(def cook-user! ((o user (get-user)) (o cookie (new-user-cookie user)))
  (whenlet c (cook-user user cookie)
    (prcookie c)
    (= (logins* user) (get-ip))
    c))

(def ensure-user ((o user (get-user)))
  (whenlet c (cook-user user)
    ; upgrade old user cookies
    (when (< (len c) 16)
      (cook-user! user))
    user))

(defhook respond-headers (str req f redir)
  (ensure-user)
  nil)

; Unique-ids are only unique per server invocation.

(def new-user-cookie (user)
  (let id (sym:string user "&" (unique-id))
    (if (cookie->user* id) (new-user-cookie user) id)))

(def user-cookies ((o user (get-user)))
  (accum a
    (each (c u) cookie->user*
      (when (is u user)
        (a c)))))

(def logout-user ((o user (get-user)))
  (each c (user-cookies user)
    (wipe (cookie->user* c)))
  (save-table cookie->user* cookfile*)
  (wipe (user->cookie* user))
  (wipe (logins* user)))

(def create-acct (user pw (o email))
  (set (dc-usernames* (downcase user)))
  (set-pw user pw)
  (= (user->email* user) email))

(def disable-acct (user)
  (set-pw user (rand-string 20))
  (logout-user user))

(= bcrypt-work-factor* 10) ; must be >= 10

(def rand-salt ((o work-factor bcrypt-work-factor*))
  (+ "$2b$" (int work-factor) "$" (rand-string 22)))

(def clean-hash (h)
  (last:tokens h))

(def user-pw (user)
  (and user (clean-hash:hpasswords* user)))

(def bcrypt-pw (user pw)
  (aand (<= (len pw) 72) (user-pw user) (is it (bcrypt pw it nil))))

(def sha1-pw (user pw)
  (aand (<= (len pw) 72) (user-pw user) (is it (shash pw))))

(def check-pw (user pw)
  (or (bcrypt-pw user pw)
      (and (sha1-pw user pw)
           (do (set-pw user pw)
               (bcrypt-pw user pw)))))
  
(def set-pw (user pw)
  (= (hpasswords* user) (bcrypt pw (rand-salt)))
  (save-table hpasswords* hpwfile*))

(def hello-page (user ip)
  (whitepage (prs "hello" user "at" ip)))

(defop login req (login-page 'both))

; switch is one of: register, login, both

; afterward is either a function on the newly created username and
; ip address, in which case it is called to generate the next page 
; after a successful login, or a pair of (function url), which means 
; call the function, then redirect to the url.

; classic example of something that should just "return" a val
; via a continuation rather than going to a new page.

(def login-page (switch (o msg nil) (o afterward hello-page))
  (whitepage
    (pagemessage msg)
    (when (in switch 'login 'both)
      (login-form "Login" switch login-handler afterward)
      (hook 'login-form afterward)
      (br2))
    (when (in switch 'register 'both)
      (login-form "Create Account" switch create-handler afterward))))

(def login-form (label switch handler afterward)
  (prbold label)
  (br2)
  (fnform (fn (req) (handler req switch afterward))
          (fn () (pwfields (downcase label)))
          (acons afterward)))

(def login-handler (req switch afterward)
  (logout-user (get-user req))
  (aif (good-login (arg req "acct") (arg req "pw") req!ip)
       (login it req!ip (user->cookie* it) afterward)
       (failed-login switch "Bad login." afterward)))

(def create-handler (req switch afterward)
  (logout-user (get-user req))
  (with (user (arg req "acct") pw (arg req "pw") email (arg req "email"))
    (aif (bad-newacct user pw)
         (failed-login switch it afterward)
         (do (create-acct user pw email)
             (login user req!ip (cook-user! user) afterward)))))

(def login (user ip cookie afterward)
  (= (logins* user) ip)
  (prcookie cookie)
  (if (acons afterward)
      (let (f url) afterward
        (f user ip)
        url)
      (do (prn)
          (afterward user ip))))

(def merge-args (args (o req (the-req*)))
  (let args1 req!args
    (each (k v) args
      (pull [caris _ k] args1)
      (push (list k v) args1))
    (= req!args args1)))

(def failed-login (switch msg afterward)
  (if (acons afterward)
      (let args (copy ((the-req*) 'args))
        (flink (fn ignore (merge-args args) (login-page switch msg afterward))))
      (do (prn)
          (login-page switch msg afterward))))

(def prcookie (cook (o key 'user) (o httponly (is key 'user)))
  (prn:string
    "Set-Cookie: " key "=" cook "; expires=Sun, 17-Jan-2038 19:14:07 GMT"
    (if httponly "; HttpOnly")
    (if (srvsecure) "; Secure")))

(def pwfields ((o label "login"))
  (if (headmatch "create" label)
      (inputs (acct  username 20 (arg "acct") 'plain)
              (pw    password 20 (arg "pw"))
              (email email?   20 (arg "email") 'plain))
      (inputs (acct  username 20 (arg "acct") 'plain 'autofocus)
              (pw    password 20 (arg "pw"))))
  (br)
  (submit label))

(or= good-logins* (queue) bad-logins* (queue))

(def good-login (user pw ip)
  (let record (list (seconds) ip user)
    (if (check-pw user pw)
        (do (cook-user! user)
            (enq-limit record good-logins*)
            user)
        (do (enq-limit record bad-logins*)
            nil))))

(or= dc-usernames* (table))

(def username-taken (user)
  (when (empty dc-usernames*)
    (each (k v) hpasswords*
      (set (dc-usernames* (downcase k)))))
  (dc-usernames* (downcase user)))

(def bad-newacct (user pw)
  (if (no (goodname user 2 15))
       "Usernames can only contain letters, digits, dashes and 
        underscores, and should be between 2 and 15 characters long.  
        Please choose another."
      (username-taken user)
       "That username is taken. Please choose another."
      (or (no pw) (< (len pw) 4))
       "Passwords should be a least 4 characters long.  Please 
        choose another."
       nil))

(def goodname (str (o min 1) (o max nil))
  (and (isa str 'string)
       (>= (len str) min)
       (~find (fn (c) (no (or (alphadig c) (in c #\- #\_))))
              str)
       (isnt (str 0) #\-)
       (or (no max) (<= (len str) max))
       str))

(defop logout req
  (aif (get-user req)
       (do (logout-user it)
           (pr "Logged out."))
       (pr "You were not logged in.")))

(defop whoami req
  (aif (get-user req)
       (if (admin it)
           (prs it 'at req!ip (tostring:write:the-req*))
           (prs it 'at req!ip))
       (do (pr "You are not logged in. ")
           (w/link (login-page 'both) (pr "Log in"))
           (pr "."))))


(= formwid* 60 bigformwid* 80 numwid* 16 formatdoc-url* nil)

; Eventually figure out a way to separate type name from format of 
; input field, instead of having e.g. toks and bigtoks

(def varfield (typ id val)
  (if (in typ 'string 'string1 'url)
       (gentag input type 'text name id value val size formwid*)
      (in typ 'num 'int 'posint 'sym)
       (gentag input type 'text name id value val size numwid*)
      (in typ 'users 'toks)
       (gentag input type 'text name id value (tostring (apply prs val))
                     size formwid*)    
      (is typ 'sexpr)
       (gentag input type 'text name id 
                     value (tostring (map [do (write _) (sp)] val))
                     size formwid*)
      (in typ 'syms 'text 'doc 'mdtext 'mdtext2 'lines 'bigtoks)
       (let text (if (in typ 'syms 'bigtoks)
                      (tostring (apply prs val))
                     (is typ 'lines)
                      (tostring (apply pr (intersperse #\newline val)))
                     (in typ 'mdtext 'mdtext2)
                      (unmarkdown val)
                     (no val)
                      ""
                     val)
         (tag (textarea cols (if (is typ 'doc) bigformwid* formwid*) 
                        rows (needrows text formwid* 4)
                        wrap 'virtual 
                        style (if (is typ 'doc) "font-size:8.5pt")
                        name id)
           (prn) ; needed or 1 initial newline gets chopped off
           (pr text))
         (when (and formatdoc-url* (in typ 'mdtext 'mdtext2))
           (pr " ")
           (tag (font size -2)
             (link "help" formatdoc-url* (gray 175)))))
      (caris typ 'choice)
       (menu id (cdr typ) val)
      (is typ 'yesno)
       (menu id '("yes" "no") (if val "yes" "no"))
      (is typ 'hexcol)
       (gentag input type 'text name id value val)
      (is typ 'time)
       (gentag input type 'text name id value (if val (english-time val) ""))
      (is typ 'date)
       (gentag input type 'text name id value (if val (english-date val) ""))
       (err "unknown varfield type" typ)))

(def text-rows (text wid (o pad 3))
  (+ (trunc (/ (len text) (* wid .8))) pad))

(def needrows (text cols (o pad 0))
  (+ pad (max (+ 1 (count #\newline text))
              (roundup (/ (len text) (- cols 5))))))

(def varline (typ id val (o liveurls))
  (if (in typ 'users 'syms 'toks 'bigtoks)  (apply prs val)
      (is typ 'lines)                       (map prn val)
      (is typ 'yesno)                       (pr (if val 'yes 'no))
      (caris typ 'choice)                   (varline (cadr typ) nil val)
      (is typ 'url)                         (if (and liveurls (valid-url val))
                                                (link val val)
                                                (pr val))
      (text-type typ)                       (pr (or val ""))
                                            (pr val)))

(def text-type (typ) (in typ 'string 'string1 'url 'text 'mdtext 'mdtext2))

; Newlines in forms come back as /r/n.  Only want the /ns. Currently
; remove the /rs in individual cases below.  Could do it in aform or
; even in the parsing of http requests, in the server.

; Need the calls to striptags so that news users can't get html
; into a title or comment by editing it.  If want a form that 
; can take html, just create another typ for it.

(def readvar (typ str (o fail nil))
  (case (carif typ)
    string  (striptags str)
    string1 (if (blank str) fail (striptags str))
    url     (if (blank str) "" (valid-url str) (clean-url str) fail)
    num     (let n (saferead str) (if (number n) n fail))
    int     (let n (saferead str)
              (if (number n) (round n) fail))
    posint  (let n (saferead str)
              (if (and (number n) (> n 0)) (round n) fail))
    text    (striptags str)
    doc     (striptags str)
    mdtext  (md-from-form str)
    mdtext2 (md-from-form str t)                      ; for md with no links
    sym     (or (sym:car:tokens str) fail)
    syms    (map sym (tokens str))
    sexpr   (errsafe (readall str))
    users   (rem [no (goodname _)] (tokens str))
    toks    (tokens str)
    bigtoks (tokens str)
    lines   (lines str)
    choice  (readvar (cadr typ) str)
    yesno   (is str "yes")
    hexcol  (if (hex>color str) str fail)
    time    (or (errsafe (parse-time str)) fail)
    date    (or (errsafe (parse-date str)) fail)
            (err "unknown readvar type" typ)))

; dates should be tagged date, and just redefine <

(def varcompare (typ)
  (if (in typ 'syms 'sexpr 'users 'toks 'bigtoks 'lines 'hexcol)
       (fn (x y) (> (len x) (len y)))
      (is typ 'date)
       (fn (x y)
         (or (no y) (and x (date< x y))))
       (fn (x y)
         (or (empty y) (and (~empty x) (< x y))))))


; (= fail* (uniq))

(def fail* ()) ; coudn't possibly come back from a form
  
; Takes a list of fields of the form (type label value view modify) and 
; a fn f and generates a form such that when submitted (f label newval) 
; will be called for each valid value.  Finally done is called.

(def vars-form (user fields f done (o button "update") (o lasts))
  (taform lasts
          (if (all [no (_ 4)] fields)
              (fn (req))
              (fn (req)
                (when-umatch user req
                  (each (k v) req!args
                    (let name (sym k)
                      (awhen (find [is (cadr _) name] fields)
                        ; added sho to fix bug
                        (let (typ id val sho mod) it
                          (when (and mod v)
                            (let newval (readvar typ v fail*)
                              (unless (is newval fail*)
                                (f name newval))))))))
                  (done))))
     (tab
       (showvars fields))
     (unless (all [no (_ 4)] fields)  ; no modifiable fields
       (br)
       (submit button))))
                
(def showvars (fields (o liveurls))
  (each (typ id val view mod question (o n 1)) (rem empty fields)
    (when view
      (when question
        (tr (repeat n (td)) (td (prn question))))
      (tr (if question
              (repeat n (td))
              (tag (td valign 'top)  (pr id ":")))
          (td (if mod 
                  (varfield typ id val)
                  (varline  typ id val liveurls))))
      (prn))))

; http://daringfireball.net/projects/markdown/syntax

(def md-from-form (str (o nolinks) (o esc))
  (markdown (trim (rem #\return (if esc str (esc-tags str))) 'end) 100 nolinks))

(def markdown (s (o maxurl) (o nolinks))
  (with (ital nil bold nil)
    (tostring
      (forlen i s
        (iflet (newi spaces) (indented-code s i (if (is i 0) 2 0))
               (do (pr  "<p><pre><code>")
                 (let cb (code-block s (- newi spaces 1))
                   (pr cb)
                   (= i (+ (- newi spaces 1) (len cb))))
                 (pr "</code></pre>"))
               (iflet newi (parabreak s i (if (is i 0) 1 0))
                      (do (unless (is i 0) (pr "<p>"))
                          (= i (- newi 1)))
                      (and (is (s i) #\*)
                           (or ital 
                               (atend i s) 
                               (and (~whitec (s (+ i 1)))
                                    (pos #\* s (+ i 1)))))
                       (do (pr (if ital "</i>" "<i>"))
                           (= ital (no ital)))
                      (and (is (s i) #\_)
                           (or bold 
                               (atend i s) 
                               (and (~whitec (s (+ i 1)))
                                    (pos #\_ s (+ i 1)))))
                       (do (pr (if bold "</b>" "<b>"))
                           (= bold (no bold)))
                      (and (no nolinks)
                           (and (or (litmatch "http://" s i) 
                                    (litmatch "https://" s i)
                                    (litmatch "/l/" s i))
                                (or (is i 0) (~in (s (- i 1)) #\" #\>))))
                       (withs (n   (urlend s i)
                               url (clean-url (cut s i n)))
                         (tag (a href url rel 'nofollow)
                           (pr (if (no maxurl) url (ellipsize url maxurl))))
                         (= i (- n 1)))
                       (writec (s i))))))))

(def indented-code (s i (o newlines 0) (o spaces 0))
  (let c (s i)
    (if (nonwhite c)
         (if (and (> newlines 1) (> spaces 1))
             (list i spaces)
             nil)
        (atend i s)
         nil
        (is c #\newline)
         (indented-code s (+ i 1) (+ newlines 1) 0)
         (indented-code s (+ i 1) newlines       (+ spaces 1)))))

; If i is start a paragraph break, returns index of start of next para.

(def parabreak (s i (o newlines 0))
  (let c (s i)
    (if (or (nonwhite c) (atend i s))
        (if (> newlines 1) i nil)
        (parabreak s (+ i 1) (+ newlines (if (is c #\newline) 1 0))))))

; Returns the indices of the next paragraph break in s, if any.

(def next-parabreak (s i)
  (unless (atend i s)
    (aif (parabreak s i) 
         (list i it)
         (next-parabreak s (+ i 1)))))

(def paras (s (o i 0))
  (if (atend i s)
      nil
      (iflet (endthis startnext) (next-parabreak s i)
             (cons (cut s i endthis)
                   (paras s startnext))
             (list (trim (cut s i) 'end)))))


; Returns the index of the first char not part of the url beginning
; at i, or len of string if url goes all the way to the end.

; Note that > immediately after a url (http://foo.com>) will cause
; an odd result, because the > gets escaped to something beginning
; with &, which is treated as part of the url.  Perhaps the answer
; is just to esc-tags after markdown instead of before.

; Treats a delimiter as part of a url if it is (a) an open delimiter
; not followed by whitespace or eos, or (b) a close delimiter 
; balancing a previous open delimiter.

(def urlend (s i (o indelim))
  (let c (s i)
    (if (atend i s)
         (if ((orf punc whitec opendelim) c) 
              i 
             (closedelim c)
              (if indelim (+ i 1) i)
             (+ i 1))
        (if (or (whitec c)
                (and (punc c) (whitec (s (+ i 1))))
                (and ((orf whitec punc) (s (+ i 1)))
                     (or (opendelim c)
                         (and (closedelim c) (no indelim)))))
            i
            (urlend s (+ i 1) (or (opendelim c)
                                  (and indelim (no (closedelim c)))))))))

(def opendelim (c)  (in c #\< #\( #\[ #\{))
 
(def closedelim (c) (in c #\> #\) #\] #\}))


(def code-block (s i)
  (tostring
    (until (let left (- (len s) i 1)
             (or (is left 0)
                 (and (> left 2)
                      (is (s (+ i 1)) #\newline)
                      (nonwhite (s (+ i 2))))))
     (writec (s (++ i))))))

(def unmarkdown (s (o skiplinks))
  (tostring
    (forlen i s
      (if (litmatch "<p>" s i)
           (do (++ i 2) 
               (unless (is i 2) (pr "\n\n")))
          (litmatch "<i>" s i)
           (do (++ i 2) (pr #\*))
          (litmatch "</i>" s i)
           (do (++ i 3) (pr #\*))
          (litmatch "<b>" s i)
           (do (++ i 2) (pr #\_))
          (litmatch "</b>" s i)
           (do (++ i 3) (pr #\_))
          (unless skiplinks
            (litmatch "<a href=" s i))
           (let endurl (posmatch [in _ #\> #\space] s (+ i 9))
             (if endurl
                 (do (pr (cut s (+ i 9) (- endurl 1)))
                     (= i (aif (posmatch "</a>" s endurl)
                               (+ it 3)
                               endurl)))
                 (writec (s i))))
          (litmatch "<pre><code>" s i)
           (awhen (findsubseq "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (writec (s i))))))


(def english-time (min)
  (let n (mod min 720)
    (string (let h (trunc (/ n 60)) (if (is h 0) "12" h))
            ":"
            (let m (mod n 60)
              (if (is m 0) "00"
                  (< m 10) (string "0" m)
                           m))
            (if (is min 0)   " midnight"
                (is min 720) " noon"
                (>= min 720) " pm"
                             " am"))))

(def parse-time (s)
  (let (nums (o label "")) (halve s letter)
    (with ((h (o m 0)) (map int (tokens nums ~digit))
           cleanlabel  (downcase (rem ~alphadig label)))
      (+ (* (if (is h 12)
                 (if (in cleanlabel "am" "midnight")
                     0
                     12)
                (is cleanlabel "am")
                 h
                 (+ h 12))
            60)
          m))))


(= months* '("January" "February" "March" "April" "May" "June" "July"
             "August" "September" "October" "November" "December")
   days* '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(def english-date ((y m d))
  (string d " " (months* (- m 1)) " " y))

(= month-names* (obj "january"    1  "jan"        1
                     "february"   2  "feb"        2
                     "march"      3  "mar"        3
                     "april"      4  "apr"        4
                     "may"        5
                     "june"       6  "jun"        6
                     "july"       7  "jul"        7
                     "august"     8  "aug"        8
                     "september"  9  "sept"       9  "sep"      9
                     "october"   10  "oct"       10
                     "november"  11  "nov"       11
                     "december"  12  "dec"       12))

(def monthnum (s) (month-names* (downcase s)))

; Doesn't work for BC dates.

(def parse-date (s)
  (let nums (date-nums s)
    (if (valid-date nums)
        nums
        (err (string "Invalid date: " s)))))

(def date-nums (s)
  (with ((ynow mnow dnow) (date)
         toks             (tokens s ~alphadig))
    (if (all [all digit _] toks)
         (let nums (map int toks)
           (case (len nums)
             1 (list ynow mnow (car nums))
             2 (iflet d (find [> _ 12] nums)
                        (list ynow (find [isnt _ d] nums) d)
                        (cons ynow nums))
               (if (> (car nums) 31)
                   (firstn 3 nums)
                   (rev (firstn 3 nums)))))
        ([all digit _] (car toks))
         (withs ((ds ms ys) toks
                 d          (int ds))
           (aif (monthnum ms)
                (list (or (errsafe (int ys)) ynow) 
                      it
                      d)
                nil))
        (monthnum (car toks))
         (let (ms ds ys) toks
           (aif (errsafe (int ds))
                (list (or (errsafe (int ys)) ynow) 
                      (monthnum (car toks))
                      it)
                nil))
          nil)))

; To be correct needs to know days per month, and about leap years

(def valid-date ((y m d))
  (and y m d
       (< 0 m 13)
       (< 0 d 32)))

(mac defopl (name parm . body)
  `(defop ,name ,parm
     (if (get-user ,parm)
         (do ,@body) 
         (login-page 'both
                     "You need to be logged in to do that."
                     (list (fn (u ip))
                           (string ',name (reassemble-args ,parm)))))))

(def shellquote (str)
  (+ "'" (multisubst (list (list "'" "'\"'\"'")) str) "'"))

(def shellargs (cmd . args)
  (list (string cmd)
        (map shellquote:string (rem nil args))))

(def shellstring (cmd . args)
  (withs ((cmd args) (apply shellargs cmd args))
    (+ cmd " " (string:intersperse #\space args))))
  
(def shell (cmd . args)
  (w/outstring o
    (w/stderr o
      (w/stdout o
        (system (apply shellstring cmd args))
        (inside o)))))

(def shelllog args
  (withs (ip (get-ip) u (get-user) c (tostring:ppr args))
    (srvlog 'shell ip u c)
    (w/stderr (stdout)
      (system:apply shellstring args))))

(def GET (url)
  (shell "curl" "-fsSL" (clean-url url)))

(def fetch-title (url)
  (let s (GET url)
    (whenlet p1 (posmatch "<title" s)
      (whenlet p2 (posmatch "</title>" s p1)
        (whenlet p3 (posmatch ">" s p1)
          (trim (cut s (+ p3 1) p2)))))))

(seval '(require racket/date))

(def mindate ((o secs (seconds)))
  (cut (rev:timedate secs) 0 5))

(def hourdate ((o secs (seconds)))
  (cut (rev:timedate secs) 0 4))

(defmemo date-seconds ((Y m d (o H 0) (o M 0) (o S 0)))
  (seval!find-seconds S M H d m Y #f))

(defmemo date-yearday (ymd)
  (seval!date-year-day:seval!seconds->date:date-seconds ymd))

(defmemo date-weekday (ymd)
  (seval!date-week-day:seval!seconds->date:date-seconds ymd))

(defmemo date-weekday-name (ymd (o short t))
  (let s (days* (date-weekday ymd))
    (if short (cut s 0 3) s)))

(defmemo month-name (m (o short t))
  (let s (months* (- m 1))
    (if short (cut s 0 3) s)))

(defmemo strftime (fmt (o ts (seconds)))
  ; TODO:
  ; By default, date pads numeric fields with zeroes.  The following
  ; optional flags may follow '%':
  ;
  ; -      (hyphen) do not pad the field
  ;
  ; _      (underscore) pad with spaces
  ;
  ; 0      (zero) pad with zeros
  ;
  ; ^      use upper case if possible
  ;
  ; #      use opposite case if possible
  ;
  ; After  any  flags  comes  an  optional field width, as a decimal
  ; number; then an optional modifier, which is either E to use the
  ; locale's alternate representations if available, or O to use the
  ; locale's alternate numeric symbols if available.
  (withs (secs (trunc ts)
          ns 0 ; todo
          (Y m d H M S) (rev:timedate secs)
          I (aand (mod H 12) (if (is it 0) 12 it))
          fmt (if (begins fmt "+") (cut fmt 1) fmt)
          n (len fmt))
    (apply string
      (accum out
        (forlen i fmt
          (if (~and (is (fmt i) #\%) (< i (- n 1)))
              (out (fmt i))
              (case (fmt (++ i))
; %%     a literal %
                #\% (out "%")
; %a     locale's abbreviated weekday name (e.g., Sun)
                #\a (out (date-weekday-name (list Y m d) t))
; %A     locale's full weekday name (e.g., Sunday)
                #\A (out (date-weekday-name (list Y m d) nil))
; %b     locale's abbreviated month name (e.g., Jan)
                #\b (out (month-name m t))
; %B     locale's full month name (e.g., January)
                #\B (out (month-name m nil))
; %c     locale's date and time (e.g., Thu Mar  3 23:05:25 2005)
                #\c (out (strftime "%a %b %e %H:%M:%S %Y" secs))
; %C     century; like %Y, except omit last two digits (e.g., 20)
                #\C (out (cut (str Y) 0 2))
; %d     day of month (e.g., 01)
                #\d (out (leftpad (str d) 2 "0"))
; %D     date; same as %m/%d/%y
                #\D (out (strftime "%m/%d/%y" secs))
; %e     day of month, space padded; same as %_d
                #\e (out (leftpad (str d) 2 " "))
; %F     full date; same as %Y-%m-%d
                #\F (out (strftime "%Y-%m-%d" secs))
; %g     last two digits of year of ISO week number (see %G)
; %G     year of ISO week number (see %V); normally useful only with %V
                ;#\g (err "todo")
                ;#\G (err "todo")
; %h     same as %b
                #\h (out (month-name m t))
; %H     hour (00..23)
                #\H (out (leftpad (str H) 2 "0"))
; %I     hour (01..12)
                #\I (out (leftpad (str I) 2 "0"))
; %j     day of year (001..366)
                #\j (out (leftpad (str (+ 1 (date-yearday (list Y m d)))) 3 "0"))
; %k     hour, space padded ( 0..23); same as %_H
                #\k (out (leftpad (str H) 2 " "))
; %l     hour, space padded ( 1..12); same as %_I
                #\l (out (leftpad (str I) 2 " "))
; %m     month (01..12)
                #\m (out (leftpad (str m) 2 "0"))
; %M     minute (00..59)
                #\M (out (leftpad (str M) 2 "0"))
; %n     a newline
                #\n (out "\n")
; %N     nanoseconds (000000000..999999999)
                #\N (out (leftpad (str ns) (len "000000000") "0"))
; %p     locale's equivalent of either AM or PM; blank if not known
                #\p (out (if (>= H 12) "PM" "AM"))
; %P     like %p, but lower case
                #\P (out (if (>= H 12) "pm" "am"))
; %q     quarter of year (1..4)
; %r     locale's 12-hour clock time (e.g., 11:11:04 PM)
                #\r (out (strftime "%I:%M:%S %p" secs))
; %R     24-hour hour and minute; same as %H:%M
                #\R (out (strftime "%H:%M" secs))
; %s     seconds since 1970-01-01 00:00:00 UTC
                #\s (out (str secs))
; %S     second (00..60)
                #\S (out (leftpad (str S) 2 "0"))
; %t     a tab
                #\t (out "\t")
; %T     time; same as %H:%M:%S
                #\T (out (strftime "%H:%M:%S" secs))
; %u     day of week (1..7); 1 is Monday
                #\u (out (str (aand (date-weekday (list Y m d)) (if (is it 0) 7 it))))
; %U     week number of year, with Sunday as first day of week (00..53)
                ;#\U (out (str (trunc:/ (date-yearday (list Y m d)) 7)))
; %V     ISO week number, with Monday as first day of week (01..53)
                ;#\V (out (str (trunc:/ (date-yearday (list Y m d)) 7)))
; %w     day of week (0..6); 0 is Sunday
                #\w (out (str (date-weekday (list Y m d))))
; %W     week number of year, with Monday as first day of week (00..53)
; %x     locale's date representation (e.g., 12/31/99)
                #\x (out (strftime "%m/%d/%y" secs))
; %X     locale's time representation (e.g., 23:13:48)
                #\X (out (strftime "%H:%M:%S" secs))
; %y     last two digits of year (00..99)
                #\y (out (cut (str Y) 2))
; %Y     year
                #\Y (out (str Y))
; %z     +hhmm numeric time zone (e.g., -0400)
                #\z (out "-0000")
; %:z    +hh:mm numeric time zone (e.g., -04:00)
                #\: (case (fmt (++ i))
                      #\z (out "-00:00")
; %::z   +hh:mm:ss numeric time zone (e.g., -04:00:00)
                      #\: (case (fmt (++ i))
                            #\z (out "-00:00:00")
; %:::z  numeric time zone with : to necessary precision (e.g., -04, +05:30)
                            #\: (case (fmt (++ i))
                                  #\z (out "-00")
                                  ; unknown
                                  (out "%:::" (fmt i)))))
; %Z     alphabetic time zone abbreviation (e.g., EDT)
                #\Z (out "GMT")
                ; unknown
                (do (out #\% (fmt i))))))))))

(defmemo moment (ms)
  (moment-ms ms))

(defmemo moment-ms ((o ms (msec)))
  (with (secs (trunc (/ ms 1000))
         msecs (mod (trunc ms) 1000))
    (strftime (+ "+%Y-%m-%dT%H:%M:%S." (leftpad msecs 3 "0") "Z") secs)))

(def moment-secs ((o secs (seconds)))
  (moment (* 1000 secs)))

(def rss-date ((o secs (seconds)))
  (strftime "+%a, %d %b %Y %H:%M:%S GMT" secs))

(def send-email (from to subject message)
  (tostring:shell "python2" "../sendmail.py" from to subject message))

; (let ts (seconds)
;   (each c "%aAbBcCdDeFgGhHIjklmMnNpPqrRsStTuUVwWxXyYzZ"
;     (out:string "%" (str c) "\t" (tostring:write:strftime (string "%" (str c)) ts))))
; %%	"%"
; %a	"Sat"
; %A	"Saturday"
; %b	"Feb"
; %B	"February"
; %c	"Sat Feb 16 04:35:48 2019"
; %C	"20"
; %d	"16"
; %D	"02/16/19"
; %e	"16"
; %F	"2019-02-16"
; %g	"%g"
; %G	"%G"
; %h	"Feb"
; %H	"04"
; %I	"04"
; %j	"047"
; %k	" 4"
; %l	" 4"
; %m	"02"
; %M	"35"
; %n	"\n"
; %N	"000000000"
; %p	"AM"
; %P	"am"
; %q	"%q"
; %r	"04:35:48 AM"
; %R	"04:35"
; %s	"1550291748"
; %S	"48"
; %t	"\t"
; %T	"04:35:48"
; %u	"6"
; %U	"%U"
; %V	"%V"
; %w	"6"
; %W	"%W"
; %x	"02/16/19"
; %X	"04:35:48"
; %y	"19"
; %Y	"2019"
; %z	"-0000"
; %Z	"GMT"
