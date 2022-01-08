; News.  2 Sep 06.

; to run news: (nsv), then go to http://localhost:8080
; put usernames of admins, separated by whitespace, in arc/admins

; bug: somehow (+ votedir* nil) is getting evaluated.

(declare 'atstrings t)

(load "firebase.arc")
(load "algolia.arc")

(= site-name*    "laarc"
   site-abbrev*  "LN"
   site-email*   "hi@@laarc.io"
   site-twitter* "theshawwn"
   site-discord* "shawwn#3694"
   discord-url*  "https://discord.gg/qaqkc9z"
   site-url*     "https://www.laarc.io"
   parent-url*   "https://www.laarc.io"
   welcome-url*  "/welcome.html"
   favicon-url*  ""
   site-desc*    "Links for the curious"     ; for rss feed
   site-color*   (color 154 186 170)
   border-color* (color 154 186 170)
   prefer-url*   t)


; Structures

; Could add (html) types like choice, yesno to profile fields.  But not
; as part of deftem, which is defstruct.  Need another mac on top of
; deftem.  Should not need the type specs in user-fields.

(deftem profile
  id         nil
  name       nil
  created    (seconds)
  auth       0
  member     nil
  submitted  nil
  favorites  nil
  votes      nil   ; for now just recent, elts each (time id by sitename dir)
  karma      1
  avg        nil
  weight     .5
  ignore     nil
  email      nil
  verified   nil ; if (is (uvar u email) (uvar u verified)), then email is valid
  about      nil
  showdead   nil
  notify     nil
  noprocrast nil
  firstview  nil
  lastview   nil
  maxvisit   20
  minaway    180
  topcolor   nil
  keys       nil
  delay      0)

(deftem item
  id         nil
  type       nil
  by         nil
  ip         nil
  time       (seconds)
  url        nil
  title      nil
  text       nil
  votes      nil   ; elts each (time ip user type score)
  score      0
  sockvotes  0
  flags      nil
  dead       nil
  deleted    nil
  parts      nil
  parent     nil
  kids       nil
  keys       nil)


; Load and Save

(= newsdir*  "arc/news/"
   storydir* "arc/news/story/"
   profdir*  "arc/news/profile/"
   votedir*  "arc/news/vote/")

(or= votes* (table) profs* (table))

(= initload-users* t)

(def load-news ((o reload))
  (when reload
    (load "news.arc")
    ;(= caching* 0)
    )
  (map ensure-dir (list arcdir* newsdir* storydir* votedir* profdir*))
  (unless stories* (load-items))
  (if (and initload-users* (empty profs*)) (load-users)))

(or= srv-port* (readenv "PORT" 8080))

(def nsv ((o port srv-port*))
  (load-news)
  (asv port))

(def run-news ((o port srv-port*))
  (prn "srv-port* " (= srv-port* port))
  (prn "srv-noisy* " (= srv-noisy* (readenv "NOISY" nil)))
  (prn "caching* " (= caching* (readenv "CACHING" 1)))
  (prn "explicit-flush " (declare 'explicit-flush (readenv "FLUSH" t)))
  (flushout)
  (nsv port))

(def load-users ()
  (pr "load users: ")
  (noisy-each 100 id (dir profdir*)
    (load-user id)))

; For some reason vote files occasionally get written out in a
; broken way.  The nature of the errors (random missing or extra
; chars) suggests the bug is lower-level than anything in Arc.
; Which unfortunately means all lists written to disk are probably
; vulnerable to it, since that's all save-table does.

(def load-user (u)
  (= (votes* u) (load-table (+ votedir* u))
     (profs* u) (temload 'profile (+ profdir* u)))
  u)

; Have to check goodname because some user ids come from http requests.
; So this is like safe-item.  Don't need a sep fn there though.

(def profile (u)
  (or (profs* u)
      (aand (goodname u)
            (file-exists (+ profdir* u))
            (= (profs* u) (temload 'profile it)))))

(def votes (u)
  (or (votes* u)
      (aand (file-exists (+ votedir* u))
            (= (votes* u) (load-table it)))))

(def make-user (name (o email (user->email* name)) (o ip (get-ip)))
  (inst 'profile
        'id       name
        ; For the moment, trust that users aren't giving bogus emails
        ; at signup. No need to force them to verify just yet.
        'email    email
        'verified email
        ; Punish users for giving out their email by spamming them
        ; with cat pictures and NP-hard problems.
        ; https://www.explainxkcd.com/wiki/index.php/287:_NP-Complete
        ; Actually, I've been reluctant to notify by default. But it's
        ; been a surprisingly popular feature, so this might be ok.
        'notify   t))

(def init-user (u (o email (user->email* u)))
  (= (votes* u) (table)
     (profs* u) (make-user u email))
  (save-votes u)
  (save-prof u)
  (newslog (get-ip) u 'noob email)
  u)

; Need this because can create users on the server (for other apps)
; without setting up places to store their state as news users.
; See the admin op in app.arc.  So all calls to login-page from the
; news app need to call this in the after-login fn.

(def ensure-news-user (u)
  (if (profile u) u (init-user u)))

(def save-votes (u) (save-table (votes* u) (+ votedir* u)))

(def save-prof  (u) (save-table (profs* u) (+ profdir* u)) (hook 'save-prof (profs* u)))

(mac uvar (u k) `((profile ,u) ',k))

(mac karma   (u) `(uvar ,u karma))
(mac ignored (u) `(uvar ,u ignore))

; Note that users will now only consider currently loaded users.

(def users ((o f idfn))
  (keep f (keys profs*)))

(def check-key (u k)
  (and u (mem k (uvar u keys))))

(def author (u i) (is u i!by))


(or= stories* nil comments* nil
     items* (table) url->story* (table)
     maxid* 0 initload* 15000)

; The dir expression yields stories in order of file creation time
; (because arc infile truncates), so could just rev the list instead of
; sorting, but sort anyway.

; Note that stories* etc only include the initloaded (i.e. recent)
; ones, plus those created since this server process started.

; Could be smarter about preloading by keeping track of popular pages.

(def load-items ()
  (map rmrf (glob (+ storydir* "*.tmp")))
  (pr "load items: ")
  (with (items (table)
         ids   (sort > (map int (dir storydir*))))
    (if ids (= maxid* (car ids)))
    (noisy-each 100 id (firstn initload* ids)
      (let i (load-item id)
        (push i (items i!type))))
    (= stories*  (rev (merge (compare < !id) items!story items!poll))
       comments* (rev items!comment))
    (hook 'initload items))
  (ensure-topstories))

(def ensure-topstories ()
  (aif (errsafe (readfile1 (+ newsdir* "topstories")))
       (= ranked-stories* (map item it))
       (do (prn "ranking stories.")
           (flushout)
           (gen-topstories))))

(def astory   (i) (is i!type 'story))
(def acomment (i) (is i!type 'comment))
(def apoll    (i) (is i!type 'poll))

(def load-item (id)
  (let i (temload 'item (+ storydir* id))
    (= (items* id) i)
    (awhen (and (astory&live i) (check i!url ~blank))
      (register-url i it))
    i))

; Note that duplicates are only prevented of items that have at some
; point been loaded.

(def register-url (i url)
  (= (url->story* (canonical-url url)) i!id))

; redefined later

(or= stemmable-sites* (table))

(def canonical-url (url)
  (if (stemmable-sites* (sitename url))
      (cut url 0 (pos #\? url))
      url))

(def new-item-id ()
  (do1 (evtil (++ maxid*) [~file-exists (+ storydir* _)])
       (hook 'maxid maxid*)))

(def item (id)
  (or (items* id) (errsafe:load-item id)))

(def kids (i) (map item i!kids))

; For use on external item references (from urls).  Checks id is int
; because people try e.g. item?id=363/blank.php

(def safe-item (id)
  (ok-id&item (if (isa id 'string) (saferead id) id)))

(def ok-id (id)
  (and (exact id) (<= 1 id maxid*)))

(def arg->item (req key)
  (safe-item:saferead (arg req key)))

(def live (i) (nor i!dead i!deleted (flagged i)))

(def save-item (i) (save-table i (+ storydir* i!id)) (hook 'save-item i))

(def kill (i how)
  (when (nor i!dead (mem how i!keys))
    (log-kill i how)
    (wipe (comment-cache* i!id))
    (set i!dead))
  (when (in how 'flagged 'dupe)
    (pushnew how i!keys))
  (save-item i))

(or= kill-log* nil)

(def log-kill (i how)
  (push (list i!id how) kill-log*))

(mac each-loaded-item (var . body)
  (w/uniq g
    `(let ,g nil
       (loop (= ,g maxid*) (> ,g 0) (-- ,g)
         (whenlet ,var (items* ,g)
           ,@body)))))

(def loaded-items (test)
  (accum a (each-loaded-item i (test&a i))))

(def newslog args (apply srvlog 'news args))


; Ranking

; Votes divided by the age in hours to the gravityth power.
; Would be interesting to scale gravity in a slider.

(= gravity* 1.8 timebase* 120 front-threshold* -10
   nourl-factor* 1.0 lightweight-factor* .3 )

(def frontpage-rank (s (o scorefn realscore) (o gravity gravity*))
  (* (/ (let base (- (scorefn s) 0)
          (if (> base 0) (expt base .8) base))
        (expt (/ (+ (item-age s) timebase*) 60) gravity))
     (if (no (in s!type 'story 'poll))  .5
         (blank s!url)                  nourl-factor*
         (lightweight s)                (min lightweight-factor*
                                             (contro-factor s))
                                        (contro-factor s))))

(def contro-factor (s)
  (aif (check (visible-family nil s) [> _ 20])
       (min 1 (expt (/ (realscore s) it) 2))
       1))

(def realscore (i)
  (if (mem 'bury i!keys) -1000 (- i!score i!sockvotes)))

(disktable lightweights* (+ newsdir* "lightweights"))

(def lightweight (s)
  (or s!dead
      (mem 'rally s!keys)  ; title is a rallying cry
      (mem 'image s!keys)  ; post is mainly image(s)
      (lightweights* (sitename s!url))
      (lightweight-url s!url)))

(defmemo lightweight-url (url)
  (in (downcase (last (tokens url #\.))) "png" "jpg" "jpeg"))

(def apath (x)
  (is ((str x) 0) #\/))

(def item-paths (i) (keep apath i!keys))

(def item-age (i) (minutes-since i!time))

(def user-age (u) (minutes-since (uvar u created)))

; Only looks at the 1000 most recent stories, which might one day be a
; problem if there is massive spam.

(def ranked-stories ((o n 500))
  (map !id (firstn n ranked-stories*)))

(def gen-topstories ()
  (= ranked-stories* (rank-stories 180 1000 (memo frontpage-rank)))
  (hook 'save-topstories))

(def save-topstories ()
  (let ids (ranked-stories)
    (writefile ids (+ newsdir* "topstories"))
    (hook 'save-topstories ids)))

(defhook save-topstories ((o ids (ranked-stories)))
  (firebase-set "v0/topstories" ids))

(def rank-stories (n consider scorefn)
  (bestn n (compare > scorefn) (latest-items metastory nil consider)))

; With virtual lists the above call to latest-items could be simply:
; (map item (retrieve consider metastory:item (gen maxid* [- _ 1])))

(def latest-items (test (o stop) (o n))
  (accum a
    (catch
      (down id maxid* 1
        (let i (item id)
          (if (or (and stop (stop i)) (and n (<= n 0)))
              (throw))
          (when (test i)
            (a i)
            (if n (-- n))))))))

; redefined later

(def metastory (i) (and i (in i!type 'story 'poll)))

(def adjust-rank (s (o scorefn frontpage-rank))
  (insortnew (compare > (memo scorefn)) s ranked-stories*)
  (save-topstories))

; If something rose high then stopped getting votes, its score would
; decline but it would stay near the top.  Newly inserted stories would
; thus get stuck in front of it. I avoid this by regularly adjusting
; the rank of a random top story.

(defbg rerank-random 30 (rerank-random))

(def rerank-random ()
  (when ranked-stories*
    (adjust-rank (ranked-stories* (rand (min 50 (len ranked-stories*)))))))

(def rerank-stories ()
  (atomic (= ranked-stories*
             (sort (compare > (memo frontpage-rank))
                   (latest-items metastory)))))

(def subs (i)
  (aand i!keys
        (keep [headmatch "/" (string _)] it)
        (if (mem '/l/private it) it (cons '/l/all it))))

(defmemo match-subs (x)
  (let x (or x "all")
    (apply orf
      (each x (tokens (str x) #\|)
        (let fns (each x (tokens x #\&)
                   (if (begins x "!")
                       (let x (sym (string "/l/" (cut x 1)))
                         (out [~mem x (subs _)]))
                       (let x (sym (string "/l/" x))
                         (out [mem x (subs _)]))))
          (unless (empty fns)
            (out (apply andf fns))))))))

(def substories ((o sub) (o n))
  (latest-items (andf metastory (match-subs sub)) nil n))

(def topstories (user n (o sub) (o threshold front-threshold*))
  (retrieve n
            [and (>= (realscore _) threshold)
                 (cansee user _)]
            (aand (substories sub n)
                  (sort (compare > (memo frontpage-rank)) it))))

(= max-delay* 10)

(def private (i)
  (mem '/l/private superparent.i!keys))

(def cansee (user i)
  (if i!deleted   (admin user)
      i!dead      (or (author user i) (seesdead user))
      (delayed i) (author user i)
      (private i) (or (author user i) (admin user))
      t))

(def isfrom (url i)
  (let u (sitename i!url)
    (and (~empty u) (is url u))))

(let mature (table)
  (def delayed (i)
    (and (no (mature i!id))
         (acomment i)
         (or (< (item-age i) (min max-delay* (uvar i!by delay)))
             (do (set (mature i!id))
                 nil)))))

(def seesdead (user)
  (or (and user (uvar user showdead) (no (ignored user)))
      (editor user)))

(def visible (user is)
  (keep [cansee user _] is))

(def fromsite (url (o is stories*))
  (keep [isfrom url _] is))

(def cansee-descendant (user c)
  (or (cansee user c)
      (some [cansee-descendant user (item _)]
            c!kids)))

(def editor (u)
  (and u (or (admin u) (> (uvar u auth) 0))))

(def member (u)
  (and u (or (admin u) (uvar u member))))


; Page Layout

(= up-url* (static-src "grayarrow.gif")
   down-url* (static-src "graydown.gif")
   logo-url* (static-src "ln.png"))

; redefined later

(def rss-url ((o label))
  (if (is label "comments") "/rsscomments" "/rss"))

(def gen-head (title label)
  (tag head
    (gentag meta name 'viewport content "width=device-width, initial-scale=1.0")
    (gentag meta name 'description             content site-desc*)
    (gentag meta name 'theme-color             content "#@(hexrep sand)")
    (gentag meta name 'msapplication-TileColor content "#@(hexrep orangered)")

    (gentag link rel  'manifest                  href (static-src "site.webmanifest"))
    (gentag link rel  'stylesheet type 'text/css href (static-src "news.css"))
    (gentag link rel  'mask-icon                 href (static-src "safari-pinned-tab.svg") color teal)
    (gentag link rel  "shortcut icon"   href "")

    (gentag link rel  'apple-touch-icon     sizes '180x180 href (static-src "apple-touch-icon.png"))
    (gentag link rel  'icon type 'image/png sizes '32x32   href (static-src "favicon-32x32.png"))
    (gentag link rel  'icon type 'image/png sizes '16x16   href (static-src "favicon-16x16.png"))

    (gentag link rel   'alternate type 'application/rss+xml
                 title 'RSS       href (rss-url label))

    (tag title (pr:eschtml title))
    (tag (script) (pr votejs*))
    (when (in label "place" "/l/place")
      (tag (script src (static-src "place.js")))
      (tag (style) (pr "body { background-color: #@(hexrep sand); }")))))

(mac npage (title label . body)
  `(tag html
     (gen-head ,title ,label)
     (tag body
       (center
         (tag (table id 'hnmain
                     border 0 cellpadding 0 cellspacing 0 width "85%"
                     bgcolor sand)
           ,@body)))))

(or= pagefns* nil)

(mac fulltop (user lid label title whence . body)
  (w/uniq (gu gi gl gt gw)
    `(with (,gu ,user ,gi ,lid ,gl ,label ,gt ,title ,gw ,whence)
       (npage (+ (if ,gt (+ ,gt bar*) "") site-name*) ,gl
         (if (check-procrast ,gu)
             (do (pagetop 'full ,gi ,gl ,gt ,gu ,gw)
                 (hook 'page ,gu ,gl)
                 ,@body)
             (row (procrast-msg ,gu ,gw)))))))

(mac longpage (user t1 lid label title whence . body)
  (w/uniq (gu gt gi)
    `(with (,gu ,user ,gt ,t1 ,gi ,lid)
       (fulltop ,gu ,gi ,label ,title ,whence
         (trtd ,@body)
         (trtd (longfoot ,gu (- (now) ,gt) ,whence))))))

(def longfoot (user elapsed whence)
  (when (in whence "/l/teapots" "/l/teapot" "/l/418")
    (vspace 10)
    (center (tag (img src "/teapot.jpg"))))
  (when (in whence "/l/chess")
    (vspace 10)
    (center
      (chess-board user)))
  (when (in whence "/l/place")
    (vspace 10)
    (center
      (place-board user)))
  (when (in whence "/l/templeos")
    (terry))
  (vspace 10)
  (color-stripe (main-color user))
  (br)
  (center
    (hook 'longfoot)
    (w/bars
      (link "Welcome" welcome-url*)
      (link "Guidelines" "/guidelines.html")
      (link "Bookmarklet" "/bookmarklet.html")
      (link "Feature Requests" "/item?id=230")
      (link "Source" "https://github.com/laarc/laarc")
      (link "API" "https://github.com/laarc/API")
      (link "Contact" "mailto:shawnpresser@@gmail.com")
      (link "Twitter" "https://twitter.com/@site-twitter*")
      (link "Lists" "/lists"))
    (br2)
    (w/bars
      (link "RSS (stories)" "/rss")
      (link "RSS (comments)" "/rsscomments"))
    (search-bar user elapsed whence)
    (admin-bar user elapsed whence)))

(def search-bar (user elapsed whence)
  (br2)
  (tag (form method "get" action "//search.laarc.io/")
    (inputs (q Search 17 nil 'plain))))

(defcache memusage 5
  (repeat 3 (seval!collect-garbage 'major))
  (memory))

(def admin-bar (user elapsed whence)
  (br2)
  (when (admin user)
    (w/bars
      (pr whence)
      (pr (len items*) "/" maxid* " loaded")
      (pr (num (round (/ (memusage) 1000))) " kb")
      (pr (len fns*) " fns")
      (pr (num elapsed) " msec")
      (link "settings" "/newsadmin")
      (link "pages" "/pages")
      (hook 'admin-bar user whence))
    (br2))
  (when (in whence "/l/dev" "/l/programming" "/l/react" "/l/reactnative")
    (prn "<iframe src=\"https://open.spotify.com/embed/user/johanbrook/playlist/2mtlhuFVOFMn6Ho3JmrLc2\" width=\"300\" height=\"380\" frameborder=\"0\" allowtransparency=\"true\" allow=\"encrypted-media\"></iframe>")))

(def color-stripe (c)
  (tag (table width "100%" cellspacing 0 cellpadding 1)
    (tr (tdcolor c))))

(mac shortpage (user lid label title whence . body)
  `(fulltop ,user ,lid ,label ,title ,whence
     (trtd ,@body)))

(mac minipage (label . body)
  `(npage (+ site-name* bar* ,label) ,label
     (pagetop nil nil ,label)
     (trtd ,@body)))

(def msgpage (user msg (o title) (o editurl) (o alert))
  (minipage (or title "Message")
    (awhen alert (pr it) (br2))
    (spanclass admin
      (center
        (awhen editurl
          (when (admin user)
            (underlink "edit" it)
            (br2)))
        (if (len> msg 80)
            (widtable 320 msg)
            (pr msg))))
    (br2)))

;(= (max-age* 'news.css) 86400)   ; cache css in browser for 1 day

; turn off server caching via (= caching* 0) or won't see changes

; only need pre padding because of a bug in Mac Firefox

; Without setting the bottom margin of p tags to 0, 1- and n-para comments
; have different space at the bottom.  This solution suggested by Devin.
; Really am using p tags wrong (as separators rather than wrappers) and the
; correct thing to do would be to wrap each para in <p></p>.  Then whatever
; I set the bottom spacing to, it would be the same no matter how many paras
; in a comment. In this case by setting the bottom spacing of p to 0, I'm
; making it the same as no p, which is what the first para has.

; supplied by pb
;.vote { padding-left:2px; vertical-align:top; }
;.comment { margin-top:1ex; margin-bottom:1ex; color:black; }
;.vote IMG { border:0; margin: 3px 2px 3px 2px; }
;.reply { font-size:smaller; text-decoration:underline !important; }

(= votejs* "
function byId(id) {
  return document.getElementById(id);
}

function vote(node) {
  var v = node.id.split(/_/);   // {'up', '123'}
  var item = v[1];

  // adjust score
  var score = byId('score_' + item);
  var newscore = parseInt(score.innerHTML || '1') + (v[0] == 'up' ? 1 : -1);
  score.innerHTML = newscore + (newscore == 1 ? ' point' : ' points');

  // hide arrows
  byId('up_'   + item).style.visibility = 'hidden';
  byId('down_' + item).style.visibility = 'hidden';

  // ping server
  var ping = new Image();
  ping.src = node.href;

  return false; // cancel browser nav
}")

; Page top

(= sand (color 246 246 239) textgray (gray 130))

(def main-color (user)
  (aif (and user (uvar user topcolor))
       (hex>color it)
       site-color*))

(def pagetop (switch lid label (o title) (o user) (o whence))
; (tr (tdcolor black (vspace 5)))
  (tr (tdcolor (main-color user)
        (tag (table border 0 cellpadding 0 cellspacing 0 width "100%"
                    style "padding:2px")
          (tr (gen-logo)
              (when (is switch 'full)
                (tag (td style "line-height:12pt; height:10px;")
                  (spanclass pagetop
                    (tag b (link site-name* "/l/all"))
                    (hspace 10)
                    (toprow user label))))
             (if (is switch 'full)
                 (tag (td style "text-align:right;padding-right:4px;")
                   (spanclass pagetop (topright user whence)))
                 (tag (td style "line-height:12pt; height:10px;")
                   (spanclass pagetop (tag b (link label "/l/all")))))))))
  (map [_ user] pagefns*)
  (spacerow 10))

(def gen-logo ()
  (tag (td style "width:18px;padding-right:4px")
    (tag (a href parent-url*)
      (tag (img src logo-url* width 18 height 18
                style "border:1px #@(hexrep border-color*) solid;")))))

(= toplabels* '(nil "welcome" "tags" "new" "threads" "comments" ; "discord"
                    "/l/show" "show" "/l/ask" "ask" "/l/place" "place" "*"))

; redefined later

(def toprow (user label)
  (w/bars
    (toplink "tags" "/l" label)
    (toplink "new" "/newest" label)
    (when user
      (toplink "threads" (threads-url user) label))
    (toplink "comments" "/newcomments" label)
    ; (toplink "discord"  discord-url* label)
    (hook 'toprow user label)
    (toplink "ask" "/l/ask" (if (is label "/l/ask") "ask" label))
    (toplink "show" "/l/show" (if (is label "/l/show") "show" label))
    (toplink "place" "/l/place" (if (is label "/l/place") "place" label))
    (link "submit" "/submit")
    (unless (mem label toplabels*)
      (fontcolor white (pr:eschtml label)))))

(def toplink (name dest label)
  (tag-if (is name label) (span class 'topsel)
    (link name dest)))

(def topright (user whence (o showkarma t))
  (when user
    (userlink user user nil)
    (when showkarma (pr  "&nbsp;(@(karma user))"))
    (pr "&nbsp;|&nbsp;"))
  (if user
      (rlinkf 'logout (req)
        (when-umatch/r user req
          (logout-user user)
          whence))
      (onlink "login"
        (login-page 'both nil
                    (list (fn (u ip)
                            (ensure-news-user u)
                            (newslog ip u 'top-login))
                          whence)))))

(def noob (user)
  (and user (< (days-since (uvar user created)) 1)))


; News-Specific Defop Variants

(mac defopt (name parm test msg . body)
  `(defop ,name ,parm
     (if (,test (get-user ,parm))
         (do ,@body)
         (login-page 'both (+ "Please log in" ,msg ".")
                     (list (fn (u ip) (ensure-news-user u))
                           (string ',name (reassemble-args ,parm)))))))

(mac defopg (name parm . body)
  `(defopt ,name ,parm idfn "" ,@body))

(mac defope (name parm . body)
  `(defopt ,name ,parm editor " as an editor" ,@body))

(mac defopa (name parm . body)
  `(defopt ,name ,parm admin " as an administrator" ,@body))

(mac opexpand (definer name parms . body)
  (w/uniq gr
    `(,definer ,name ,gr
       (with (user (get-user ,gr) ip (,gr 'ip))
         (with ,(and parms (mappend [list _ (list 'arg gr (string _))]
                                    parms))
           (newslog ip user ',name ,@parms)
           ,@body)))))

(or= newsop-names* nil)

(mac newsop args
  `(do (pushnew ',(car args) newsop-names*)
       (opexpand defop ,@args)))

(mac newsopr args
  `(do (pushnew ',(car args) newsop-names*)
       (opexpand defopr ,@args)))

(mac adop (name parms . body)
  (w/uniq g
    `(opexpand defopa ,name ,parms
       (let ,g (string ',name)
         (shortpage user nil ,g ,g ,g
           ,@body)))))

(mac edop (name parms . body)
  (w/uniq g
    `(opexpand defope ,name ,parms
       (let ,g (string ',name)
         (shortpage user nil ,g ,g ,g
           ,@body)))))


; News Admin

(defopa newsadmin req
  (let user (get-user req)
    (newslog req!ip user 'newsadmin)
    (newsadmin-page user)))

; Note that caching* is reset to val in source when restart server.

(def nad-fields ()
  `((num      caching         ,caching*                       t t)
    (bigtoks  comment-kill    ,comment-kill*                  t t)
    (bigtoks  comment-ignore  ,comment-ignore*                t t)
    (bigtoks  lightweights    ,(sort < (keys lightweights*))  t t)))

; Need a util like vars-form for a collection of variables.
; Or could generalize vars-form to think of places (in the setf sense).

(def newsadmin-page (user)
  (shortpage user nil nil "newsadmin" "newsadmin"
    (vars-form user
               (nad-fields)
               (fn (name val)
                 (case name
                   caching            (= caching* val)
                   comment-kill       (todisk comment-kill* val)
                   comment-ignore     (todisk comment-ignore* val)
                   lightweights       (todisk lightweights* (memtable val))
                   ))
               (fn () (newsadmin-page user)))
    (br2)
    (aform (fn (req)
             (with (user (get-user req) subject (arg req "id"))
               (if (profile subject)
                   (do (killallby subject)
                       (submitted-page user subject))
                   (admin&newsadmin-page user))))
      (single-input "" 'id 20 "kill all by"))
    (br2)
    (aform (fn (req)
             (let user (get-user req)
               (set-ip-ban user (arg req "ip") t)
               (admin&newsadmin-page user)))
      (single-input "" 'ip 20 "ban ip"))))

(defmemo suggested-title (url)
  (or (fetch-title url) "unknown"))

(newsop suggest-title (url)
  (pr:suggested-title url))


; Users

(newsop user (id)
  (if (only.profile id)
      (user-page user id)
      (pr "No such user.")))

(= (static-header* 'user.json) "application/json")

(newsop user.json (id)
  (aif (only.profile id)
       (write-json (user>json it))
       (pr "null")))

(= (static-header* 'auth.json) "application/json")

(newsop auth.json ()
  (aif (get-auth)
       (write-json (obj auth it))
       (pr "null")))

(= (static-header* 'apple-app-site-association) "application/json")

(defop apple-app-site-association req
  (write-json (obj webcredentials
                   (obj apps (list "B9452FEMTF.com.emilykolar.LaarcIOS")))))

(def user>json (u)
  (obj id        u!id
       created   u!created
       karma     u!karma
       about     u!about
       submitted u!submitted))

(defhook save-prof (u)
  (firebase-set "v0/user/@u!id" (user>json u)))

(def user-page (user subject)
  (let here (user-url subject)
    (shortpage user nil nil (+ "Profile: " subject) here
      (profile-form user subject)
      (br2)
      (hook 'user user subject))))

(def profile-form (user subject)
  (let prof (profile subject)
    (vars-form user
               (user-fields user subject)
               (fn (name val)
                 (when (and (is name 'ignore) val (no prof!ignore))
                   (log-ignore user subject 'profile))
                 (= (prof name) val))
               (fn () (save-prof subject)
                      (user-page user subject)))))

(= topcolor-threshold* 250)

(def user-fields (user subject)
  (withs (e (editor user)
          a (admin user)
          w (is user subject)
          k (and w (> (karma user) topcolor-threshold*))
          u (or a w)
          m (or a (and (member user) w))
          p (profile subject)
          s subject)
  (w/accum
    `(string  user       ,subject                                  t   nil)
    `(string  name       ,(p 'name)                               ,m  ,m)
    `(string  created    ,(text-age:user-age subject)              t   nil)
    `(int     auth       ,(p 'auth)                               ,e  ,a)
    `(yesno   member     ,(p 'member)                             ,a  ,a)
    `(posint  karma      ,(p 'karma)                               t  ,a)
    `(num     avg        ,(p 'avg)                                ,a  nil)
    `(yesno   ignore     ,(p 'ignore)                             ,e  ,e)
    `(num     weight     ,(p 'weight)                             ,a  ,a)
    `(mdtext2 about      ,(p 'about)                               t  ,u)
    `(string  email      ,(p 'email)                              ,u  ,u)
    `(string  verified   ,(p 'verified)                           ,a  ,a)
    (unless (blank p!email)
      (if (isnt p!email p!verified)
          `(string  verify   ,(verify-link s)                     ,u  nil)
          `(yesno   notify   ,(p 'notify)                         ,u  ,u
            "Be notified of replies by email?")))
    `(yesno   showdead   ,(p 'showdead)                           ,u  ,u)
    `(yesno   noprocrast ,(p 'noprocrast)                         ,u  ,u)
    `(string  firstview  ,(p 'firstview)                          ,a   nil)
    `(string  lastview   ,(p 'lastview)                           ,a   nil)
    `(posint  maxvisit   ,(p 'maxvisit)                           ,u  ,u)
    `(posint  minaway    ,(p 'minaway)                            ,u  ,u)
    `(sexpr   keys       ,(p 'keys)                               ,a  ,a)
    `(hexcol  topcolor   ,(or (p 'topcolor) (hexrep site-color*)) ,k  ,k)
    `(int     delay      ,(p 'delay)                              ,u  ,u)
    `(string  password    ,(resetpw-link s)                       ,u  nil "")
    `(string  submissions ,(submissions-link s)                    t  nil "")
    `(string  comments    ,(comments-link s)                       t  nil "")
    `(string  upvoted     ,(+ (upvoted-link s)   " (private)")    ,u  nil "")
    `(string  favorites   ,(+ (favorites-link s) (if u " (shared)" "")) t  nil "")
    )))

(def verify-link (u (o label "verify email"))
  (tostring (underlink label "/verify?u=@u")))

(def resetpw-link (u (o label "reset password"))
  (tostring (underlink label "/resetpw?u=@u")))

(def submissions-link (u (o label "submissions"))
  (tostring (underlink label (submitted-url u))))

(def comments-link (u (o label "comments"))
  (tostring (underlink label (threads-url u))))

(def upvoted-link (u (o label "upvoted"))
  (tostring (underlink label (upvoted-url u))))

(def favorites-link (u (o label "favorites"))
  (tostring (underlink label (favorites-url u))))

(newsop welcome ()
  (pr "Welcome to " site-name* ", " user "!"))

; Verify email

(defopg verify req
  (with (user (get-user req)
         subject (arg req "u"))
    (verify-page user subject)))

(def verify-page (user subject (o msg))
  (let subject (or subject user)
    (if (~or (admin user) (is user subject))
        (pr "Sorry.")
      (minipage "Verify Email"
        (if msg (pr msg) (pr "Verifying email for @subject"))
        (br2)
        (unless msg
          (uform user req (try-verify user subject (arg req "e"))
            (single-input "New email:  " 'e 30 "send verification email"
                          nil (uvar subject email))))))))

(def try-verify (user subject newemail)
  (if (len< newemail 4)
      (verify-page user subject "Emails should be a least 4 characters long.
                          Please choose another.")
      (do (send-verification subject newemail)
          (verify-page user subject "Verification email sent.
                       Please check your inbox."))))

(def send-verification (subject newemail)
  (send-email site-email*
              newemail
              "Please verify your email address on @site-name*"
              (+ "Click here to verify your email: "
                 site-url*
                 (rflink [let u (profile subject)
                           (= u!email newemail
                              u!verified newemail)
                           (save-prof subject)
                           (user-url subject)]))))



; Main Operators

; remember to set caching to 0 when testing non-logged-in

(= caching* 1 perpage* 30 threads-perpage* 10 maxend* 10000)

; Limiting that newscache can't take any arguments except the user.
; To allow other arguments, would have to turn the cache from a single
; stored value to a hash table whose keys were lists of arguments.

(mac newsfn (user time . body)
  (w/uniq gc
    `(let ,gc (cache (fn () (* caching* ,time))
                     (fn () (tostring (let ,user nil ,@body))))
       (fn (,user)
         (if ,user
             (do ,@body)
             (pr (,gc)))))))

(mac newscache (name user time . body)
  `(safeset ,name (newsfn ,user ,time ,@body)))

(newsop news () (newspage user))

(newsop ||   () (newspage user))

;(newsop index.html () (newspage user))

(= lncache* (table))
(= lncache-time* 90)

(newsop l (path)
  (if (empty path)
      (tags-page user)
    ((or (lncache* path)
         (= (lncache* path)
            (newsfn user lncache-time* ()
              (let sub (+ "/l/" path)
                (listpage user (now)
                          (topstories user maxend*
                                      path)
                          sub sub sub)))))
     user)))

(newscache newspage user 90
  (listpage user (now) (topstories user maxend*) nil nil "/l/all"))

(def listpage (user t1 items label title (o url label) (o number t))
  (hook 'listpage user)
  (longpage user t1 nil label title url
    (display-items user items label title url 0 perpage* number)))


(newsop newest () (newestpage user))

; Note: dead/deleted items will persist for the remaining life of the
; cached page.  If this were a prob, could make deletion clear caches.

(newscache newestpage user 40
  (listpage user (now) (newstories user maxend*) "new" "New Links" "newest"))

(def newstories (user n)
  (retrieve n [cansee user _] stories*))

(defhook create-story (s)
  (let ids (map !id (newstories nil 500))
    (firebase-set "v0/newstories" ids)))

(newsop best () (bestpage user))

(newscache bestpage user 1000
  (listpage user (now) (beststories user maxend*) "best" "Top Links"))

; As no of stories gets huge, could test visibility in fn sent to best.

(def beststories (user n)
  (bestn n (compare > realscore) (visible user stories*)))

(def sitestories (user url (o n maxend*))
  (retrieve n [cansee user _] (fromsite url)))

(newsop from (site)
  (let site (clean-url site)
    (listpage user (now) (sitestories user site maxend*) "from" "Submissions from @site")))

(newsop noobstories () (noobspage user stories*))
(newsop noobcomments () (noobspage user comments*))

(def noobspage (user source)
  (listpage user (now) (noobs user maxend* source) "noobs" "New Accounts"))

(def noobs (user n source)
  (retrieve n [and (cansee user _) (bynoob _)] source))

(def bynoob (i)
  (< (- (user-age i!by) (item-age i)) 2880))


(newsop bestcomments () (bestcpage user))

(newscache bestcpage user 1000
  (listpage user (now) (bestcomments user maxend*)
            "best comments" "Best Comments" "bestcomments" nil))

(def bestcomments (user n)
  (bestn n (compare > realscore) (visible user comments*)))

(def stats-from-log (filename)
  (each x (lines:filechars filename)
    (let (s h) (halve x)
      (awhen (aand (saferead s) (if (isa it 'int) it))
        (out it (car:halve (cut h 1)))))))

(def stats-day ((o ymd (date)))
  (let (y m d) ymd
    (let file (string "arc/logs/srv-" y "-" (leftpad m 2) "-" (leftpad d 2))
      (stats-from-log file))))

(def stats-hour (H (o ymd (date)) (o day (stats-day ymd)))
  (withs ((Y m d) ymd
          from (date-seconds (list Y m d H))
          upto (+ from 3600))
    (keep [let (ts) _ (and (>= ts from) (< ts upto))]
          day)))

(def traffic-hour (H (o ymd (date)) (o day (stats-day ymd)))
  (let xs (stats-hour H ymd day)
    (list (len xs) (len:dedup (map cadr xs)))))

(or= traffic* (obj))

(def traffic-day ((o ymd (date)))
  (if (is 24 (len (traffic* ymd)))
      (traffic* ymd)
      (= (traffic* ymd)
         (let (y m d) ymd
           (let day (stats-day ymd)
             (aand (accum a (for i 0 23
                              (let x (traffic-hour i ymd day)
                                (let (requests uniques) x
                                  (when (or (> requests 0) (> uniques 0))
                                    (apply a (strftime "%Y-%m-%d %H:%M GMT" (date-seconds (+ ymd (list i)))) x))))))
                   (rev it)))))))

(defcache plot-traffic 60
  (lines:trim:shell 'sh "bin/plot-traffic.sh"))

(defcache traffic-page 30
  (withs (secs (seconds)
          ymd0 (date secs)
          ymd1 (date (- (date-seconds ymd0) (* 24 60 60)))
          ymd2 (date (- (date-seconds ymd1) (* 24 60 60)))
          ts (strftime "+%Y-%m-%d %H:%M:%S GMT" secs)
          (daily weekly) (pair (plot-traffic)))
    (tostring:minipage "traffic @ts"
      (center
        (pr "recent 48 hours")
        (br2)
        (sptab
          (row "hourly" "requests" "uniques")
          (let predicted nil
            (each (d r u) (firstn 48
                            (+ (traffic-day ymd0)
                               (traffic-day ymd1)
                               (traffic-day ymd2)))
              (if predicted (row d (pr:num r) (pr:num u))
                  (let m (aand (or (saferead (strftime "%M" secs)) 0)
                               (/ 60 (+ 1 it)))
                    (row (pr d " (current)") (pr:num r) (pr:num u))
                    (row (pr d " (predicted)")
                         (pr:num:trunc:* r m)
                         (pr:num:trunc:* u m))
                    (= predicted t))))))
        (vspace 35)
        (color-stripe textgray)
        (vspace 35)
        (pr "daily")
        (br2)
        (each x daily
          (let src (+ "/" (last:tokens x #\/))
            (tag (a href src) (gentag img src src width 900)))
          (br2))
        (sptab
          (row "daily" "requests" "uniques")
          (let predicted nil
            (each (d r u) (map tokens (rev:lines:trim:filechars "static/traffic.csv"))
              (with (r (or (saferead r) 0)
                     u (or (saferead u) 0))
                (if predicted (row d (pr:num r) (pr:num u))
                  (let m (aand (+ (* 60 (or (saferead (strftime "%H" secs)) 0))
                                        (or (saferead (strftime "%M" secs)) 0))
                               (/ (* 60 24) (+ 1 it)))
                    (row (pr d " (current)") (pr:num r) (pr:num u))
                    (row (pr d " (predicted)")
                         (pr:num:trunc:* r m)
                         (pr:num:trunc:* u m))
                    (= predicted t)))))))
        (vspace 35)
        (color-stripe textgray)
        (vspace 35)
        (pr "weekly")
        (br2)
        (each x weekly
          (let src (+ "/" (last:tokens x #\/))
            (tag (a href src) (gentag img src src width 900)))
          (br2))
        (sptab
          (row "weekly" "requests" "uniques")
          (with (predicted nil stats (map tokens (rev:lines:trim:filechars "static/traffic-weekly.csv")))
            (each (d r u) stats
              (with (r (or (saferead r) 0)
                     u (or (saferead u) 0))
                (if predicted (row d (pr:num r) (pr:num u))
                  (let ts (date-seconds (map int (tokens d #\-)))
                    (let m (/ (* 7 24 60 60) (+ 1 (- secs ts)))
                      (row (pr d " (current)") (pr:num r) (pr:num u))
                      (row (pr d " (predicted)")
                           (pr:num:trunc:* r m)
                           (pr:num:trunc:* u m))
                      (= predicted t))))))))
      ))))

(defop traffic req
  (aif (arg "on")
      (minipage "traffic on @it"
        (center
          (sptab
            (row "hourly" "requests" "uniques")
            (each (d r u) (traffic-day (map int (tokens it #\-)))
              (row d (pr:num r) (pr:num u))))))
      (pr:traffic-page)))

(seval '(require racket/os))

(defop uptime req
  (let ((o s "00") (o m "00") (o h "00") (o d "00")) (rev:tokens (trim:shell "ps -o etime= -p" (seval!getpid)) [in _ #\- #\:])
    (pr:string d "d " h "h " m "m " s "s")))

(newsop lists ()
  (longpage user (now) nil "lists" "Lists" "lists"
    (sptab
      (row (link "/votes")        "Recent votes.")
      (row (link "/best")         "Highest voted recent links.")
      (row (link "/active")       "Most active current discussions.")
      (row (link "/bestcomments") "Highest voted recent comments.")
      (row (link "/noobstories")  "Submissions from new accounts.")
      (row (link "/noobcomments") "Comments from new accounts.")
      (row (link "/traffic")      "Hourly, daily, and weekly traffic statistics.")
      (row (link "/uptime")       "How long has racket been running?")
      (when (admin user)
        (map row:link
             '(optimes noobs topips flagged killed badguys badlogins goodlogins)))
      (hook 'listspage user))))


(def upvoted-url (user) (+ "/upvoted?id=" user))

(newsop upvoted (id)
  (if (only.profile id)
      (upvotedpage user id)
      (pr "No such user.")))

(def upvotedpage (user subject)
  (if (or (is user subject) (admin user))
      (listpage user (now)
                (sort (compare < item-age) (voted-items user subject))
                "upvoted"
                "Upvoted items"
                (upvoted-url subject)
                nil)
      (pr "Can't display that.")))

(def voted-items (user subject)
  (keep [and (or (astory _) (acomment _)) (cansee user _)]
        (map item (keys:votes subject))))


; Story Display

(def display-items (user items label title whence
                    (o start 0) (o end perpage*) (o number))
  (zerotable
    (let n start
      (each i (cut items start end)
        (display-item (and number (++ n)) i user whence t)
        (spacerow (if (acomment i) 15 5))))
    (when end
      (let newend (+ end perpage*)
        (when (and (<= newend maxend*) (< end (len items)))
          (spacerow 10)
          (tr (tag (td colspan (if number 2 1)))
              (tag (td class 'title)
                (morelink (aand (- (len items) end)
                                (num it)
                                "@it more...")
                          display-items
                          items label title end newend number))))))))

; This code is inevitably complex because the More fn needs to know
; its own fnid in order to supply a correct whence arg to stuff on
; the page it generates, like logout and delete links.

(def morelink (msg f items label title . args)
  (tag (a href
          (url-for
            (afnid (fn (req)
                     (prn)
                     (with (url  (url-for it)     ; it bound by afnid
                            user (get-user req))
                       (newslog req!ip user 'more label)
                       (longpage user (now) nil label title url
                         (apply f user items label title url args))))))
          rel 'nofollow)
    (pr msg)))

(def display-story (i s user whence)
  (when (or (cansee user s) (s 'kids))
    (tr (display-item-number i)
        (td (votelinks s user whence))
        (titleline s s!url user whence))
    (tr (tag (td colspan (if i 2 1)))
        (tag (td class 'subtext)
          (hook 'itemline s user)
          (itemline s user)
          (editlink s user)
          (when (apoll s) (addoptlink s user))
          (unless i (flaglink s user whence))
          (unless i (favlink s user whence))
          (killlink s user whence)
          ;(blastlink s user whence)
          ;(blastlink s user whence t)
          (deletelink s user whence)
          (when (in s!type 'story 'poll) (commentlink s user))
          (scorelink s user whence)))))

(def display-item-number (i)
  (when i (tag (td align 'right valign 'top class 'title)
            (pr i "."))))

(= follow-threshold* 1)

(def titleline (s url user whence)
  (tag (td class 'title)
    (if (cansee user s)
        (do (deadmark s user)
            (titlelink s url user)
            (awhen (sitename url)
              (spanclass comhead
                (pr " (" )
                (if (admin user)
                    (w/rlink (do (set-site-ban user
                                               it
                                               (case (car (banned-sites* it))
                                                 nil    'ignore
                                                 ignore 'kill
                                                 kill   nil))
                                 whence)
                      (let ban (car (banned-sites* it))
                        (tag-if ban (font color (case ban
                                                  ignore darkred
                                                  kill   darkblue))
                          (pr it))))
                    (link it "/from?site=@it"))
                (pr ") "))))
        (pr (pseudo-text s)))))

(def titlelink (s url user)
  (let toself (blank url)
    (tag (a href (if toself
                      (item-url s!id)
                     (or (live s) (author user s) (editor user))
                      url
                      nil)
            rel  (unless (or toself (> (realscore s) follow-threshold*))
                   'nofollow))
      (pr s!title))))

(def pdflink (url)
  (awhen (vacuumize url)
    (pr " [")
    (link "scribd" it)
    (pr "]")))

(defmemo vacuumize (url)
  (and (or (endmatch ".pdf" url) (endmatch ".PDF" url))
       (+ "http://www.scribd.com/vacuum?url=" url)))

(def pseudo-text (i)
  (if i!deleted "[deleted]" (flagged i) "[flagged]" "[dead]"))

(def deadmark (i user)
  (when (mem 'dupe i!keys)
    (pr " [dupe] "))
  (when (private i)
    (pr " [private] "))
  (when (flagged i)
    (pr " [flagged] "))
  (when (and i!dead (seesdead user))
    (pr " [dead] "))
  (when (and i!deleted (admin user))
    (pr " [deleted] ")))

(= downvote-threshold* 200 downvote-time* 1440)

(= votewid* 14)

(def votelinks (i user whence (o downtoo))
  (center
    (if (and (cansee user i)
             (or (no user)
                 (no ((votes user) i!id))))
         (do (votelink i user whence 'up)
             (if (and downtoo
                      (or (admin user)
                          (< (item-age i) downvote-time*))
                      (canvote user i 'down))
                 (votelink i user whence 'down)
                 ; don't understand why needed, but is, or a new
                 ; page is generated on voting
                 (tag (span id (+ "down_" i!id)))))
        (author user i)
         (do (fontcolor orange (pr "*"))
             (br)
             (hspace votewid*))
        (hspace votewid*))))

; could memoize votelink more, esp for non-logged in users,
; since only uparrow is shown; could straight memoize

; redefined later (identically) so the outs catch new vals of up-url, etc.

(def votelink (i user whence dir)
  (tag (a id      (if user (string dir '_ i!id))
          onclick (if user "return vote(this)")
          href    (vote-url user i dir whence))
    (tag (div class (+ "votearrow" (if (is dir 'down) " rotate180" ""))))))

(def vote-url (user i dir whence)
  (+ "/vote?" "for=" i!id
              "&dir=" dir
              (if user (+ "&by=" user "&auth=" (get-auth user)))
              "&whence=" (urlencode whence)))

(= lowest-score* -4)

; Not much stricter than whether to generate the arrow.  Further tests
; applied in vote-for.

(def canvote (user i dir)
  (and user
       (news-type&live i)
       (or (is dir 'up) (> i!score lowest-score*))
       (no ((votes user) i!id))
       (or (is dir 'up)
           (and (acomment i)
                (> (karma user) downvote-threshold*)
                (no (aand i!parent (author user (item it))))))))

; Need the by argument or someone could trick logged in users into
; voting something up by clicking on a link.  But a bad guy doesn't
; know how to generate an auth arg that matches each user's cookie.

(newsop vote (by for dir auth whence)
  (with (i      (safe-item for)
         dir    (saferead dir)
         whence (if whence (urldecode whence) "news"))
    (if (no i)
         (pr "No such item.")
        (no (in dir 'up 'down))
         (pr "Can't make that vote.")
        (and by (or (isnt by user) (~is-auth auth user)))
         (pr "User mismatch.")
        (no user)
         (login-page 'both "You have to be logged in to vote."
                     (list (fn (u ip)
                             (ensure-news-user u)
                             (newslog ip u 'vote-login)
                             (when (canvote u i dir)
                               (vote-for u i dir)
                               (logvote ip u i)))
                           whence))
        (canvote user i dir)
         (do (vote-for by i dir)
             (logvote ip by i))
         (pr "Can't make that vote."))))

(def itemline (i user)
  (when (cansee user i)
    (when (news-type i) (itemscore i user))
    (byline i user)))

(= show-score-threshold* 1)

(def itemscore (i (o user))
  (tag (span id (+ "score_" i!id))
    (let score (if (is i!type 'pollopt) (realscore i) i!score)
      (when (or (is i!type 'pollopt)
                (> score show-score-threshold*))
        (pr (plural score "point")))))
  (hook 'itemscore i user))

(def item-timestamp (i)
  "@(moment-secs i!time)")

; redefined later

(def byline (i user)
  (pr " by @(tostring (userlink user i!by))")
  (awhen (and (astory i) (tostring (sublinks i)))
    (pr " to @it"))
  (pr " @(tostring (itemlink i (text-age:item-age i))) "))


(def itemlink (i (o label) (o title))
  (link (or label "link") (item-url i!id) nil nil (item-timestamp i)))

(def sublinks (i)
  (each p (item-paths i)
    (link (tostring (fontcolor black (pr:last:tokens p #\/))) p)
    (sp)))

(def user-url (user) (+ "/user?id=" user))

(= show-avg* nil)

(def userlink (user subject (o show-avg t))
  (link (user-name user subject) (user-url subject))
  (awhen (and show-avg* (admin user) show-avg (uvar subject avg))
    (pr " (@(num it 1 t t))")))

(= admin-color* darkblue
   noob-color* (color 60 150 60)
   noob-time* (* 4 1440)) ; 4 days

(def user-name (user subject)
  (if (and (admin user) (admin subject))
       (tostring (fontcolor admin-color* (pr subject)))
      (and (editor user) (ignored subject))
       (tostring (fontcolor darkred (pr subject)))
      (< (user-age subject) noob-time*)
       (tostring (fontcolor noob-color* (pr subject)))
      subject))

(= show-threadavg* nil)

(def commentlink (i user)
  (when (cansee user i)
    (pr bar*)
    (tag (a href (item-url i!id))
      (let n (- (visible-family user i) 1)
        (if (> n 0)
            (do (pr (plural n "comment"))
                (awhen (and show-threadavg* (admin user) (threadavg i))
                  (pr " (@(num it 1 t t))")))
            (pr "discuss"))))))

(def visible-family (user i)
  (+ (if (cansee user i) 1 0)
     (sum [visible-family user (item _)] i!kids)))

(def threadavg (i)
  (only.avg (map [or (uvar _ avg) 1]
                 (rem admin (dedup (map !by (keep live (family i))))))))

(= user-changetime* 120 editor-changetime* 1440)

(or= everchange* (table) noedit* (table))

(def canedit (user i)
  (or (admin user)
      (and (~noedit* i!type)
           (editor user)
           (< (item-age i) editor-changetime*))
      (own-changeable-item user i)))

(def own-changeable-item (user i)
  (and (author user i)
       (~mem 'locked i!keys)
       (no i!deleted)
       (or (everchange* i!type)
           (private i)
           (< (item-age i) user-changetime*))))

(def editlink (i user)
  (when (canedit user i)
    (pr bar*)
    (link "edit" (edit-url i))))

(def addoptlink (p user)
  (when (or (admin user) (author user p))
    (pr bar*)
    (onlink "add choice" (add-pollopt-page p user))))

; reset later

(= flag-threshold* 30 flag-kill-threshold* 1 many-flags* 0)

; Un-flagging something doesn't unkill it, if it's now no longer
; over flag-kill-threshold.  Ok, since arbitrary threshold anyway.

(def flaglink (i user whence)
  (when (and user
             (or (legit-user user)
                 (> (karma user) flag-threshold*)))
    (when (isnt user i!by)
      (pr bar*)
      (w/rlink (do (togglemem user i!flags)
                   (when (and (~mem 'nokill i!keys)
                              (len> i!flags flag-kill-threshold*))
                     (kill i 'flagged))
                   (when (admin user)
                     (if (mem user i!flags)
                         (kill i 'flagged)
                         (do (pull 'flagged i!keys)
                             (save-item i))))
                   whence)
        (pr "@(if (mem user i!flags) 'un)flag")))
    (let label "notice"
      (when (and (admin user) (or (flagged i) (len> i!flags many-flags*)))
        (pr bar* (plural (len i!flags) "flag") " ")
        (w/rlink (do (togglemem 'nokill i!keys)
                     (if (mem 'nokill i!keys) (wipe i!dead))
                     (save-item i)
                     whence)
          (pr (if (mem 'nokill i!keys) "un-notice" "noted")))))))

(def favlink (i user whence)
  (when (and user (cansee user i))
    (pr bar*)
    (w/rlink (do (togglemem i!id (uvar user favorites))
                 (save-prof user)
                 (favorites-url user))
      (pr "@(if (mem i!id (uvar user favorites)) 'un-)favorite"))))

(def killlink (i user whence)
  (when (admin user)
    (pr bar*)
    (w/rlink (do (zap no i!dead)
                 (if i!dead
                     (do (pull 'nokill i!keys)
                         (log-kill i user))
                     (pushnew 'nokill i!keys))
                 (save-item i)
                 whence)
      (pr "@(if i!dead 'un)kill"))))

; Blast kills the submission and bans the user.  Nuke also bans the
; site, so that all future submitters will be ignored.  Does not ban
; the ip address, but that will eventually get banned by maybe-ban-ip.

(def blastlink (i user whence (o nuke))
  (when (and (admin user)
             (or (no nuke) (~empty i!url)))
    (pr bar*)
    (w/rlink (do (toggle-blast i user nuke)
                 whence)
      (prt (if (ignored i!by) "un-") (if nuke "nuke" "blast")))))

(def toggle-blast (i user (o nuke))
  (atomic
    (if (ignored i!by)
        (do (wipe i!dead (ignored i!by))
            (awhen (and nuke (sitename i!url))
              (set-site-ban user it nil)))
        (do (set i!dead)
            (ignore user i!by (if nuke 'nuke 'blast))
            (awhen (and nuke (sitename i!url))
              (set-site-ban user it 'ignore))))
    (if i!dead (log-kill i user))
    (save-item i)
    (save-prof i!by)))

(def candelete (user i)
  (or (admin user) (own-changeable-item user i)))

(def deletelink (i user whence)
  (when (candelete user i)
    (pr bar*)
    (linkf (if i!deleted "undelete" "delete") (req)
      (let user (get-user req)
        (if (candelete user i)
            (del-confirm-page user i whence)
            (prn "You can't delete that."))))))

; Undeleting stories could cause a slight inconsistency. If a story
; linking to x gets deleted, another submission can take its place in
; url->story.  If the original is then undeleted, there will be two
; stories with equal claim to be in url->story.  (The more recent will
; win because it happens to get loaded later.)  Not a big problem.

(def del-confirm-page (user i whence)
  (minipage "Confirm"
    (tab
      ; link never used so not testable but think correct
      (display-item nil i user (flink [del-confirm-page (get-user _) i whence]))
      (spacerow 20)
      (tr (td)
          (td (urform user req
                      (do (when (candelete user i)
                            (= i!deleted (is (arg req "b") "Yes"))
                            (save-item i))
                          whence)
                 (prn "Do you want this to @(if i!deleted 'stay 'be) deleted?")
                 (br2)
                 (but "Yes" "b") (sp) (but "No" "b")))))))

(def scorelink (i user (o whence))
  (when (admin user)
    (pr bar*)
    (pr (aand (frontpage-rank i) (num it 6 t t)))))

(def permalink (story user)
  (when (cansee user story)
    (pr bar*)
    (link "link" (item-url story!id))))

(def logvote (ip user story)
  (newslog ip user 'vote (story 'id) (list (story 'title))))

(def text-age (a (o day t) (o hrs t))
  (tostring
    (if (and day (>= a 1440)) (pr (plural (trunc (/ a 1440)) "day")    " ago")
        (and hrs (>= a   60)) (pr (plural (trunc (/ a 60))   "hour")   " ago")
                    (pr (plural (trunc a)          "minute") " ago"))))


; Voting

; A user needs legit-threshold karma for a vote to count if there has
; already been a vote from the same IP address.  A new account below both
; new- thresholds won't affect rankings, though such votes still affect
; scores unless not a legit-user.

(= legit-threshold* 0 new-age-threshold* 0 new-karma-threshold* 2)

(def legit-user (user)
  (or (editor user)
      (mem 'legit (uvar user keys))))

(def possible-sockpuppet (user)
  (or (ignored user)
      (< (uvar user weight) .5)
      (and (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))))

(= downvote-ratio-limit* .65 recent-votes* nil votewindow* 100)

; Note: if vote-for by one user changes (s 'score) while s is being
; edited by another, the save after the edit will overwrite the change.
; Actual votes can't be lost because that field is not editable.  Not a
; big enough problem to drag in locking.

(def vote-for (user i (o dir 'up))
  (unless (or ((votes user) i!id)
              (and (~live i) (isnt user i!by)))
    (withs (ip   (logins* user)
            vote (list (seconds) ip user dir i!score))
      (unless (or (and (or (ignored user) (check-key user 'novote))
                       (isnt user i!by))
                  (and (is dir 'down)
                       (~editor user)
                       (or (check-key user 'nodowns)
                           (> (downvote-ratio user) downvote-ratio-limit*)
                           ; prevention of karma-bombing
                           (just-downvoted user i!by)))
                  (and (nor (legit-user user)
                            (> (karma user) legit-threshold*))
                       (isnt user i!by)
                       (find [is (cadr _) ip] i!votes))
                  (and (isnt i!type 'pollopt)
                       (biased-voter i vote)))
        (++ i!score (case dir up 1 down -1))
        ; canvote protects against sockpuppet downvote of comments
        (when (and (is dir 'up) (possible-sockpuppet user))
          (++ i!sockvotes))
        (metastory&adjust-rank i)
        (unless (or (author user i)
                    (and (is ip i!ip) (~editor user))
                    (is i!type 'pollopt))
          (++ (karma i!by) (case dir up 1 down -1))
          (save-prof i!by))
        (wipe (comment-cache* i!id)))
      (if (admin user) (pushnew 'nokill i!keys))
      (push vote i!votes)
      (save-item i)
      (push (list (seconds) i!id i!by (sitename i!url) dir)
            (uvar user votes))
      (= ((votes* user) i!id) vote)
      (save-votes user)
      (zap [firstn votewindow* _] (uvar user votes))
      (save-prof user)
      (push (cons i!id vote) recent-votes*))))

; redefined later

(def biased-voter (i vote) nil)

; ugly to access vote fields by position number

(def downvote-ratio (user (o sample 20))
  (ratio [is _.1.3 'down]
         (keep [let by ((item (car _)) 'by)
                 (nor (is by user) (ignored by))]
               (bestn sample (compare > car:cadr) (tablist (votes user))))))

(def just-downvoted (user victim (o n 3))
  (let prev (firstn n (recent-votes-by user))
    (and (is (len prev) n)
         (all (fn ((id sec ip voter dir score))
                (and (author victim (item id)) (is dir 'down)))
              prev))))

; Ugly to pluck out fourth element.  Should read votes into a vote
; template.  They're stored slightly differently in two diff places:
; in one with the voter in the car and the other without.

(def recent-votes-by (user)
  (keep [is _.3 user] recent-votes*))


; Story Submission

(newsop submit ()
  (if user
      (submit-page user nil "" "" t)
      (submit-login-warning nil "" "" t)))

(def submit-login-warning ((o sub) (o url) (o title) (o showtext) (o text))
  (login-page 'both "You have to be logged in to submit."
              (fn (user ip)
                (ensure-news-user user)
                (newslog ip user 'submit-login)
                (submit-page user sub url title showtext text))))

(def clean-sub (x)
  (coerce (+ "/l/" (downcase:last (tokens x #\/))) 'sym))

(def url-input (url)
  (row "url" (input "u" url 50 "ln-url-input"))
  (row "" (underlink "suggest title" nil "suggestTitle();")))

(= submitjs* "
function tlen(el) { var n = el.value.length - 80; el.nextSibling.innerText = n > 0 ? n + ' too long' : ''; }

function suggestTitle() {
  var i = byId('ln-title-input');
  var msg = 'fetching...';
  i.value = msg;
  fetch('/suggest-title?url=' + encodeURIComponent(byId('ln-url-input').value)).then(x => {x.text().then(x => {
    if (i.value === msg) { i.value = x; tlen(i); i.focus(); }})})
  return false; // cancel browser nav
}
")

(def submit-page (user (o sub) (o url) (o title) (o showtext) (o text "") (o msg))
  (minipage "Submit"
    (pagemessage msg)
    (urform user req
            (process-story (get-user req)
                           (clean-sub (arg req "l"))
                           (clean-url (arg req "u"))
                           (striptags (arg req "t"))
                           showtext
                           (and showtext (md-from-form (arg req "x")))
                           req!ip)
      (script submitjs*)
      (tab
        (row "to" (input "l" (or sub "news") 50))
        (row "title" (do (input "t" title 50 "ln-title-input" "tlen(this)" "tlen(this)")
                         (gentag span style "margin-left:10px")))
        (if prefer-url*
            (do (url-input url)
                (when showtext
                  (spacerow 20)
                  ;(row "" "<b>or</b>")
                  (row "text" (textarea "x" 4 50 (only.pr text)))))
            (do (row "text" (textarea "x" 4 50 (only.pr text)))
                (row "" "<b>or</b>")
                (url-input url)))
        (row "" (submit))
        (spacerow 20)
        (row "" submit-instructions*)))))

(= submit-instructions*
   "Leave url blank to submit a question for discussion. The text
   (if any) will appear at the top of the comments page.")

; For use by outside code like bookmarklet.
; http://news.domain.com/submitlink?l=news&u=http://foo.com&t=Foo
; Added a confirm step to avoid xss hacks.

(newsop submitlink (l u t)
  (if user
      (submit-page user l u t t)
      (submit-login-warning l u t)))

(= title-limit* 80
   retry*       "Please try again."
   toolong*     "Please make title < @title-limit* characters."
   bothblank*   "The url and text fields can't both be blank.  Please
                 either supply a url, or if you're asking a question,
                 put it in the text field."
   toofast*     "You're submitting too fast.  Please slow down.  Thanks."
   spammage*    "Stop spamming us.  You're wasting your time.")

; Only for annoyingly high-volume spammers. For ordinary spammers it's
; enough to ban their sites and ip addresses.

(disktable big-spamsites* (+ newsdir* "big-spamsites"))

(def process-story (user sub url title showtext text ip)
  (aif (and (~blank url) (live-story-w/url url))
       (do (vote-for user it)
           (item-url it!id))
       (if (no user)
            (flink [submit-login-warning sub url title showtext text])
           (no (and (or (blank url) (valid-url url))
                    (~blank title)))
            (flink [submit-page user sub url title showtext text retry*])
           (len> title title-limit*)
            (flink [submit-page user sub url title showtext text toolong*])
           ;(and (blank url) (blank text))
            ;(flink [submit-page user sub url title showtext text bothblank*])
           (let site (sitename url)
             (or (big-spamsites* site) (recent-spam site)))
            (flink [msgpage user spammage*])
           (oversubmitting user ip 'story url)
            (flink [msgpage user toofast*])
           (let s (create-story sub url (process-title title) text user ip)
             (story-ban-test user s ip url)
             (when (ignored user) (kill s 'ignored))
             (submit-item user s)
             (maybe-ban-ip s)
             "newest"))))

(def submit-item (user i)
  (push i!id (uvar user submitted))
  (save-prof user)
  (vote-for user i))

(def recent-spam (site)
  (and (caris (banned-sites* site) 'ignore)
       (recent-items [is (sitename _!url) site] 720)))

(def recent-items (test minutes)
  (let cutoff (- (seconds) (* 60 minutes))
    (latest-items test [< _!time cutoff])))

; Turn this on when spam becomes a problem.

(= enforce-oversubmit* nil)

; New user can't submit more than 2 stories in a 2 hour period.
; Give overeager users the key toofast to make limit permanent.

(def oversubmitting (user ip kind (o url))
  (or (check-key user 'rebuff)
      (and enforce-oversubmit*
           (or (check-key user 'toofast)
               (ignored user)
               (< (user-age user) new-age-threshold*)
               (< (karma user) new-karma-threshold*))
           (len> (recent-items [or (author user _) (is _!ip ip)] 180)
                 (if (is kind 'story)
                     (if (bad-user user) 0 1)
                     (if (bad-user user) 1 10))))))

; Note that by deliberate tricks, someone could submit a story with a
; blank title.

(diskvar scrubrules* (+ newsdir* "scrubrules"))

(def process-title (s)
  (let s2 (multisubst scrubrules* s)
    (zap upcase (s2 0))
    s2))

(def live-story-w/url (url)
  (aand (url->story* (canonical-url url)) (check (item it) live)))

(def parse-site (url)
  (rev (tokens (cadr (tokens url [in _ #\/ #\?])) #\.)))

(defmemo sitename (url)
  (and (valid-url url)
       (let toks (parse-site (rem #\space url))
         (if (isa (saferead (car toks)) 'int)
             (tostring (prall toks "" "."))
             (let (t1 t2 t3 . rest) toks
               (if (and (~in t3 nil "www")
                        (or (mem t1 multi-tld-countries*)
                            (mem t2 long-domains*)))
                   (+ t3 "." t2 "." t1)
                   (and t2 (+ t2 "." t1))))))))

(= multi-tld-countries* '("uk" "jp" "au" "in" "ph" "tr" "za" "my" "nz" "br"
                          "mx" "th" "sg" "id" "pk" "eg" "il" "at" "pl"))

(= long-domains* '("blogspot" "wordpress" "livejournal" "blogs" "typepad"
                   "weebly" "posterous" "blog-city" "supersized" "dreamhosters"
                   ; "sampasite"  "multiply" "wetpaint" ; all spam, just ban
                   "eurekster" "blogsome" "edogo" "blog" "com"
                   "ycombinator"))

(def create-story (sub url title text user ip)
  (newslog ip user 'create sub url (list title))
  (let s (inst 'item 'type 'story 'id (new-item-id)
                     'url url 'title title 'text text 'by user 'ip ip)
    (when sub
      (each x (rev (tokens sub [or (whitec _) (in _ #\,)]))
        (pushnew (clean-sub x) s!keys)))
    (unless (mem (clean-sub "private") s!keys)
      (let title (downcase title)
        (if (headmatch "show laarc" title)
            (do (pull (clean-sub "news") s!keys)
                (pushnew (clean-sub "show") s!keys))
            (or (blank url)
                (headmatch "ask laarc" title))
            (do (pull (clean-sub "news") s!keys)
                (pushnew (clean-sub "ask") s!keys)))))
    (save-item s)
    (= (items* s!id) s)
    (unless (blank url) (register-url s url))
    (push s stories*)
    (hook 'create-story s)
    s))


; Bans

(def ignore (user subject cause)
  (set (ignored subject))
  (save-prof subject)
  (log-ignore user subject cause))

(diskvar ignore-log* (+ newsdir* "ignore-log"))

(def log-ignore (user subject cause)
  (todisk ignore-log* (cons (list subject user cause) ignore-log*)))

; Kill means stuff with this substring gets killed. Ignore is stronger,
; means that user will be auto-ignored.  Eventually this info should
; be stored on disk and not in the source code.

(disktable banned-ips*     (+ newsdir* "banned-ips"))   ; was ips
(disktable banned-sites*   (+ newsdir* "banned-sites")) ; was sites

(diskvar  comment-kill*    (+ newsdir* "comment-kill"))
(diskvar  comment-ignore*  (+ newsdir* "comment-ignore"))

(= comment-kill* nil ip-ban-threshold* 3)

(def set-ip-ban (user ip yesno (o info))
  (= (banned-ips* ip) (and yesno (list user (seconds) info)))
  (todisk banned-ips*))

(def set-site-ban (user site ban (o info))
  (= (banned-sites* site) (and ban (list ban user (seconds) info)))
  (todisk banned-sites*))

; Kill submissions from banned ips, but don't auto-ignore users from
; them, because eventually ips will become legit again.

; Note that ban tests are only applied when a link or comment is
; submitted, not each time it's edited.  This will do for now.

(def story-ban-test (user i ip url)
  (site-ban-test user i url)
  (ip-ban-test i ip)
  (hook 'story-ban-test user i ip url))

(def site-ban-test (user i url)
  (whenlet ban (banned-sites* (sitename url))
    (if (caris ban 'ignore) (ignore nil user 'site-ban))
    (kill i 'site-ban)))

(def ip-ban-test (i ip)
  (if (banned-ips* ip) (kill i 'banned-ip)))

(def comment-ban-test (user i ip string kill-list ignore-list)
  (when (some [posmatch _ string] ignore-list)
    (ignore nil user 'comment-ban))
  (when (or (banned-ips* ip) (some [posmatch _ string] kill-list))
    (kill i 'comment-ban)))

; An IP is banned when multiple ignored users have submitted over
; ban-threshold* (currently loaded) dead stories from it.

; Can consider comments too if that later starts to be a problem,
; but the threshold may start to be higher because then you'd be
; dealing with trolls rather than spammers.

(def maybe-ban-ip (s)
  (when (and s!dead (ignored s!by))
    (let bads (loaded-items [and _!dead (astory _) (is _!ip s!ip)])
      (when (and (len> bads ip-ban-threshold*)
                 (some [and (ignored _!by) (isnt _!by s!by)] bads))
        (set-ip-ban nil s!ip t)))))

(def killallby (user)
  (map [kill _ 'all] (submissions user)))

; Only called from repl.

(def kill-whole-thread (c)
  (kill c 'thread)
  (map kill-whole-thread:item c!kids))


; Polls

; a way to add a karma threshold for voting in a poll
;  or better still an arbitrary test fn, or at least pair of name/threshold.
; option to sort the elements of a poll when displaying
; exclusive field? (means only allow one vote per poll)

(= poll-threshold* 20)

(newsop newpoll ()
  (if (and user (> (karma user) poll-threshold*))
      (newpoll-page user)
      (pr "Sorry, you need @poll-threshold* karma to create a poll.")))

(def newpoll-page (user (o title "Poll: ") (o text "") (o opts "") (o msg))
  (minipage "New Poll"
    (pagemessage msg)
    (urform user req
            (process-poll (get-user req)
                          (striptags (arg req "t"))
                          (md-from-form (arg req "x") t)
                          (striptags (arg req "o"))
                          req!ip)
      (tab
        (row "title"   (input "t" title 50))
        (row "text"    (textarea "x" 4 50 (only.pr text)))
        (row ""        "Use blank lines to separate choices:")
        (row "choices" (textarea "o" 7 50 (only.pr opts)))
        (row ""        (submit))))))

(= fewopts* "A poll must have at least two options.")

(def process-poll (user title text opts ip)
  (if (or (blank title) (blank opts))
       (flink [newpoll-page user title text opts retry*])
      (len> title title-limit*)
       (flink [newpoll-page user title text opts toolong*])
      (len< (paras opts) 2)
       (flink [newpoll-page user title text opts fewopts*])
      (atlet p (create-poll (multisubst scrubrules* title) text opts user ip)
        (ip-ban-test p ip)
        (when (ignored user) (kill p 'ignored))
        (submit-item user p)
        (maybe-ban-ip p)
        "newest")))

(def create-poll (title text opts user ip)
  (newslog ip user 'create-poll title)
  (let p (inst 'item 'type 'poll 'id (new-item-id)
                     'title title 'text text 'by user 'ip ip)
    (= p!parts (map get!id (map [create-pollopt p nil nil _ user ip]
                                (paras opts))))
    (save-item p)
    (= (items* p!id) p)
    (push p stories*)
    p))

(def create-pollopt (p url title text user ip)
  (let o (inst 'item 'type 'pollopt 'id (new-item-id)
                     'url url 'title title 'text text 'parent p!id
                     'by user 'ip ip)
    (save-item o)
    (= (items* o!id) o)
    o))

(def add-pollopt-page (p user)
  (minipage "Add Poll Choice"
    (urform user req
            (do (add-pollopt user p (striptags (arg req "x")) req!ip)
                (item-url p!id))
      (tab
        (row "text" (textarea "x" 4 50))
        (row ""     (submit))))))

(def add-pollopt (user p text ip)
  (unless (blank text)
    (atlet o (create-pollopt p nil nil text user ip)
      (++ p!parts (list o!id))
      (save-item p))))

(def display-pollopts (p user whence)
  (each o (visible user (map item p!parts))
    (display-pollopt nil o user whence)
    (spacerow 7)))

(def display-pollopt (n o user whence)
  (tr (display-item-number n)
      (tag (td valign 'top)
        (votelinks o user whence))
      (tag (td class 'comment)
        (tag (div style "margin-top:1px;margin-bottom:0px")
          (if (~cansee user o) (pr (pseudo-text o))
              (~live o)        (spanclass dead
                                 (pr (if (~blank o!title) o!title o!text)))
                               (if (and (~blank o!title) (~blank o!url))
                                   (link o!title o!url)
                                   (fontcolor black (pr o!text)))))))
  (tr (if n (td))
      (td)
      (tag (td class 'default)
        (spanclass comhead
          (itemscore o)
          (editlink o user)
          (killlink o user whence)
          (deletelink o user whence)
          (deadmark o user)))))


; Individual Item Page (= Comments Page of Stories)

(defmemo item-url (id) (+ "/item?id=" id))

(newsop item (id)
  (let s (safe-item id)
    (if (news-type s)
        (do (if s!deleted (note-baditem user ip))
            (item-page user s))
        (do (note-baditem user ip)
            (pr "No such item.")))))

(= (static-header* 'item.json) "application/json")

(newsop item.json (id)
  (let s (safe-item id)
    (if (news-type s)
        (write-json (item>json s user))
        (do (note-baditem user ip)
            (pr "null")))))

(= (static-header* 'maxitem.json) "application/json")

(defop maxitem.json ()
  (write-json maxid*))

(defhook maxid (n)
  (firebase-set "v0/maxitem" maxid*))

(def descendants (i (o user))
  (sum [visible-family user _]
       (map item i!kids)))

(def story-comment-count (i (o user))
  (when (astory i) (descendants i user)))

(def tnil (x) (if x #t nil))
(def tnull (x) (or x 'null))

(def item>json (i (o user))
  (if (or i!deleted (private i))
      (obj id      i!id
           deleted (tnil i!deleted)
           dead    (tnil i!dead)
           private (tnil (private i))
           type    (string i!type)
           time    i!time
           parent  i!parent)
      (obj id      i!id
           dead    (tnil i!dead)
           type    (string i!type)
           kids    i!kids
           keys    (map string i!keys)
           by      i!by
           time    i!time
           title   i!title
           url     i!url
           text    i!text
           parent  i!parent
           score   i!score
           descendants (story-comment-count i user))))

(def item>search (i (o user))
  (if (or i!dead i!deleted (private i))
      (obj objectID       (string i!id)
           parent_id      (tnull i!parent)
           created_at_i   i!time
           created_at     (moment-secs i!time)
           deleted        (tnil i!deleted)
           dead           (tnil i!dead)
           private        (tnil (private i))
           title          'null
           url            'null
           author         'null
           points         'null
           comment_text   'null
           num_comments   'null
           story_id       'null
           story_text     'null
           story_title    'null
           story_url      'null)
    (whenlet s (superparent i)
      (let r (and (no s!deleted) (no s!dead) (isnt s i))
        (obj objectID       (string i!id)
             parent_id      (tnull i!parent)
             created_at_i   i!time
             created_at     (moment-secs i!time)
             deleted        (tnil i!deleted)
             dead           (tnil i!dead)
             private        (tnil (private i))
             title          (tnull i!title)
             url            (tnull i!url)
             author         (tnull i!by)
             points         (tnull i!score)
             comment_text   (tnull:if (acomment i) (tnull i!text))
             num_comments   (tnull:if (astory i) (story-comment-count i user))
             story_id       (tnull:if (isnt s i) s!id)
             story_text     (tnull:if r s!text)
             story_title    (tnull:if r s!title)
             story_url      (tnull:if r s!url)
             _tags          (list (string i!type) "author_@i!by" "story_@s!id"))))))

(defhook save-item (i)
  (firebase-set "v0/item/@i!id" (item>json i))
  (whenlet s (superparent i)
    (algolia-set "Item_production" (item>search i))
    (unless (is s!id i!id)
      (hook 'save-item s))))

(or= baditemreqs* (table) baditem-threshold* 1/100)

; Something looking at a lot of deleted items is probably the bad sort
; of crawler.  Throttle it for this server invocation.

(def note-baditem (user ip)
  (unless (admin user)
    (++ (baditemreqs* ip 0))
    (with (r (requests/ip* ip) b (baditemreqs* ip))
       (when (and (> r 500) (> (/ b r) baditem-threshold*))
         (set (throttle-ips* ip))))))

; redefined later

(def news-type (i) (and i (in i!type 'story 'comment 'poll 'pollopt)))

(def item-page (user i)
  (with (title (and (cansee user i)
                    (or i!title (aand i!text (ellipsize (striptags it)))))
         here (item-url i!id))
    (longpage user (now) nil nil title here
      (tab (display-item nil i user here t)
           (display-item-text i user)
           (when (apoll i)
             (spacerow 10)
             (tr (td)
                 (td (tab (display-pollopts i user here)))))
           (when (and (cansee user i) (or (admin user) (comments-active i)))
             (spacerow 10)
             (row "" (comment-form i user here))))
      (br2)
      (when (and i!kids (commentable i))
        (tab (display-subcomments i user here))
        (br2)))))

(def commentable (i) (in i!type 'story 'comment 'poll))

; By default the ability to comment on an item is turned off after
; 45 days, but this can be overriden with commentable key.

; (= commentable-threshold* (* 60 24 45))

; jan 8 2022: I turned this off (or more specifically, to one century)
; for laarc.io to encourage discussion, even if the conversations are
; old.

(= commentable-threshold* (* 60 24 365 100))


(def comments-active (i)
  (and (live&commentable i)
       (live (superparent i))
       (or (< (item-age i) commentable-threshold*)
           (mem 'commentable i!keys))))


(or= displayfn* (table))

(= (displayfn* 'story)   (fn (n i user here inlist)
                           (display-story n i user here)))

(= (displayfn* 'comment) (fn (n i user here inlist)
                           (display-comment n i user here nil 0 nil inlist)))

(= (displayfn* 'poll)    (displayfn* 'story))

(= (displayfn* 'pollopt) (fn (n i user here inlist)
                           (display-pollopt n i user here)))

(def display-item (n i user here (o inlist))
  ((displayfn* (i 'type)) n i user here inlist))

(def superparent (i (o n))
  (aif (is n 0)
        i
       i!parent
        (superparent (item it) (if n (- n 1)))
       i))

(def display-item-text (s user)
  (when (and (cansee user s)
             (in s!type 'story 'poll)
             ;(blank s!url)
             (~blank s!text))
    (spacerow 2)
    (row "" (spanclass comment (pr s!text)))))


; Edit Item

(def edit-url (i) (+ "/edit?id=" i!id))

(newsop edit (id)
  (let i (safe-item id)
    (if (and i
             (cansee user i)
             (editable-type i)
             (or (news-type i) (admin user) (author user i)))
        (edit-page user i)
        (pr "No such item."))))

(def editable-type (i) (fieldfn* i!type))

(or= fieldfn* (table))

(= (fieldfn* 'story)
   (fn (user s)
     (with (a (admin user)  e (editor user)  x (canedit user s))
       `((string1 title     ,s!title        t ,x)
         (url     url       ,s!url          t ,e)
         (mdtext  text      ,s!text         t ,x)
         ,@(standard-item-fields s a e x)))))

(= (fieldfn* 'comment)
   (fn (user c)
     (with (a (admin user)  e (editor user)  x (canedit user c))
       `((mdtext  text      ,c!text         t ,x)
         ,@(standard-item-fields c a e x)))))

(= (fieldfn* 'poll)
   (fn (user p)
     (with (a (admin user)  e (editor user)  x (canedit user p))
       `((string1 title     ,p!title        t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(= (fieldfn* 'pollopt)
   (fn (user p)
     (with (a (admin user)  e (editor user)  x (canedit user p))
       `((string  title     ,p!title        t ,x)
         (url     url       ,p!url          t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(def standard-item-fields (i a e x)
       `((sexpr   votes     ,i!votes       ,a  nil)
         (int     score     ,i!score        t ,a)
         (num     sockvotes ,i!sockvotes   ,a ,a)
         (yesno   dead      ,i!dead        ,e ,e)
         (yesno   deleted   ,i!deleted     ,a ,a)
         (sexpr   flags     ,i!flags       ,a nil)
         (sexpr   keys      ,i!keys        ,a ,a)
         (string  ip        ,i!ip          ,e  nil)))

; Should check valid-url etc here too.  In fact make a fn that
; does everything that has to happen after submitting a story,
; and call it both there and here.

(def edit-page (user i)
  (let here (edit-url i)
    (shortpage user nil nil "Edit" here
      (tab (display-item nil i user here t)
           (display-item-text i user))
      (br2)
      (vars-form user
                 ((fieldfn* i!type) user i)
                 (fn (name val)
                   (unless (ignore-edit user i name val)
                     (when (and (is name 'dead) val (no i!dead))
                       (log-kill i user))
                     (= (i name) val)))
                 (fn () ;(if (admin user) (pushnew 'locked i!keys))
                        (save-item i)
                        (metastory&adjust-rank i)
                        (wipe (comment-cache* i!id))
                        (edit-page user i)))
      (hook 'edit user i))))

(def ignore-edit (user i name val)
  (and (~admin user)
       (case name
         title (len> val title-limit*)
         dead (mem 'nokill i!keys))))

; Comment Submission

(def comment-login-warning (parent whence (o text))
  (login-page 'both "You have to be logged in to comment."
              (fn (u ip)
                (ensure-news-user u)
                (newslog ip u 'comment-login)
                (addcomment-page parent u whence text))))

(def addcomment-page (parent user whence (o text) (o msg))
  (minipage "Add Comment"
    (pagemessage msg)
    (tab
      (let here (flink [addcomment-page parent (get-user _) whence text msg])
        (display-item nil parent user here t))
      (spacerow 10)
      (row "" (comment-form parent user whence text)))))

(= noob-comment-msg* nil)

; Comment forms last for 30 min (- cache time)

(def comment-form (parent user whence (o text))
  (tarform 1800
           (fn (req)
             (when-umatch/r user req
               (process-comment user parent (arg req "text") req!ip whence)))
    (textarea "text" 6 60
      (aif text (prn (unmarkdown it))))
    (when (and noob-comment-msg* (noob user))
      (br2)
      (spanclass subtext (pr noob-comment-msg*)))
    (br2)
    (submit (if (acomment parent) "reply" "add comment"))))

(= comment-threshold* -20)

; Have to remove #\returns because a form gives you back "a\r\nb"
; instead of just "a\nb".   Maybe should just remove returns from
; the vals coming in from any form, e.g. in aform.

(def process-comment (user parent text ip whence)
  (if (no user)
       (flink [comment-login-warning parent whence text])
      (empty text)
       (flink [addcomment-page parent (get-user _) whence text retry*])
      (oversubmitting user ip 'comment)
       (flink [msgpage user toofast*])
       (atlet c (create-comment parent (md-from-form text) user ip)
         (comment-ban-test user c ip text comment-kill* comment-ignore*)
         (if (bad-user user) (kill c 'ignored/karma))
         (if (dupe-reply c) (kill c 'dupe))
         (submit-item user c)
         (process-reply parent!by c)
         (+ whence "#" (aif (dupe-reply c) it!id c!id)))))

(def dupe-reply (c)
  (find [and (live _)
             (is _!text c!text)
             (is _!by c!by)]
        (siblings c)))

(def process-reply (user c)
  (aand user
        (isnt user c!by)
        (live c)
        (uvar user notify)
        (uvar user email)
        (if (is it (uvar user verified)) it)
        (send-email site-email* it "New reply from @c!by"
                    "@(do site-url*)@(item-url c!id) on: @((superparent c) 'title)")
        t))

(def bad-user (u)
  (or (ignored u) (< (karma u) comment-threshold*)))

(def create-comment (parent text user ip)
  (newslog ip user 'comment (parent 'id))
  (let c (inst 'item 'type 'comment 'id (new-item-id)
                     'text text 'parent parent!id 'by user 'ip ip)
    (save-item c)
    (= (items* c!id) c)
    (push c!id parent!kids)
    (save-item parent)
    (push c comments*)
    c))


; Comment Display

(def display-comment-tree (c user whence (o indent 0) (o initialpar))
  (when (cansee-descendant user c)
    (display-1comment c user whence indent initialpar)
    (display-subcomments c user whence (+ indent 1))))

(def display-1comment (c user whence indent showpar)
  (row (tab (display-comment nil c user whence t indent showpar showpar))))

(def display-subcomments (c user whence (o indent 0))
  (each k (sort (compare > frontpage-rank:item) c!kids)
    (display-comment-tree (item k) user whence indent)))

(def display-comment (n c user whence (o astree) (o indent 0)
                                      (o showpar) (o showon))
  (tr (display-item-number n)
      (when astree (td (hspace (* indent 40))))
      (tag (td valign 'top) (votelinks c user whence t))
      (display-comment-body c user whence astree indent showpar showon)))

; Comment caching doesn't make generation of comments significantly
; faster, but may speed up everything else by generating less garbage.

; It might solve the same problem more generally to make html code
; more efficient.

(= comment-cache* (table) comment-cache-timeout* (table) cc-window* 10000)

(= comments-printed* 0 cc-hits* 0)

(= comment-caching* t)

; Cache comments generated for nil user that are over an hour old.
; Only try to cache most recent 10k items.  But this window moves,
; so if server is running a long time could have more than that in
; cache.  Probably should actively gc expired cache entries.

(def display-comment-body (c user whence astree indent showpar showon)
  (++ comments-printed*)
  (if (and comment-caching*
           astree (no showpar) (no showon)
           (live c)
           (nor (admin user) (editor user) (author user c))
           (< (- maxid* c!id) cc-window*)
           (> (- (seconds) c!time) 60)) ; was 3600
      (pr (cached-comment-body c user whence indent))
      (gen-comment-body c user whence astree indent showpar showon)))

(def cached-comment-body (c user whence indent)
  (or (and (> (or (comment-cache-timeout* c!id) 0) (seconds))
           (awhen (comment-cache* c!id)
             (++ cc-hits*)
             it))
      (= (comment-cache-timeout* c!id)
          (cc-timeout c!time)
         (comment-cache* c!id)
          (tostring (gen-comment-body c user whence t indent nil nil)))))

; Cache for the remainder of the current minute, hour, or day.

(def cc-timeout (t0)
  (let age (- (seconds) t0)
    (+ t0 (if (< age 3600)
               (* (+ (trunc (/ age    60)) 1)    60)
              (< age 86400)
               (* (+ (trunc (/ age  3600)) 1)  3600)
               (* (+ (trunc (/ age 86400)) 1) 86400)))))

(def gen-comment-body (c user whence astree indent showpar showon)
  (tag (td id c!id class 'default)
    (let parent (and (or (no astree) showpar) (c 'parent))
      (tag (div style "margin-top:2px; margin-bottom:-10px; ")
        (spanclass comhead
          (itemline c user)
          (when parent
            (when (cansee user c) (pr bar*))
            (link "parent" (item-url ((item parent) 'id))))
          (editlink c user)
          (killlink c user whence)
          ;(blastlink c user whence)
          (deletelink c user whence)
          ; a hack to check whence but otherwise need an arg just for this
          (unless (or astree (is whence "newcomments"))
            (flaglink c user whence))
          (favlink c user whence)
          (deadmark c user)
          (when showon
            (pr " | on: ")
            (let s (superparent c)
              (link (ellipsize s!title 50) (item-url s!id))))))
      (when (or parent (cansee user c))
        (br))
      (spanclass comment
        (if (~cansee user c)               (pr (pseudo-text c))
            (nor (live c) (author user c)) (spanclass dead (pr c!text))
                                           (fontcolor (comment-color c)
                                             (pr c!text))))
      (when (and astree (cansee user c) (live c))
        (para)
        (tag (font size 1)
          (if (and (~mem 'neutered c!keys)
                   (replyable c indent)
                   (comments-active c))
              (underline (replylink c whence))
              (fontcolor sand (pr "-----"))))))))

; For really deeply nested comments, caching could add another reply
; delay, but that's ok.

; People could beat this by going to the link url or manually entering
; the reply url, but deal with that if they do.

(= reply-decay* 1.8)   ; delays: (0 0 1 3 7 12 18 25 33 42 52 63)

(def replyable (c indent)
  (or (< indent 2)
      (> (item-age c) (expt (- indent 1) reply-decay*))))

(def replylink (i whence (o title 'reply))
  (link title (+ "/reply?id=" i!id "&whence=" (urlencode whence))))

(newsop reply (id whence)
  (with (i      (safe-item id)
         whence (or (only.urldecode whence) "news"))
    (if (only.comments-active i)
        (if user
            (addcomment-page i user whence)
            (login-page 'both "You have to be logged in to comment."
                        (fn (u ip)
                          (ensure-news-user u)
                          (newslog ip u 'comment-login)
                          (addcomment-page i u whence))))
        (pr "No such item."))))

(def comment-color (c)
  (if (> c!score 0) black (grayrange c!score)))

(defmemo grayrange (s)
  (gray (min 230 (round (expt (* (+ (abs s) 2) 900) .6)))))


; Threads

(def threads-url (user) (+ "/threads?id=" user))

(newsop threads (id)
  (if id
      (threads-page user id)
      (pr "No user specified.")))

(def threads-page (user subject)
  (if (profile subject)
      (withs (title (+ subject "'s threads")
              label (if (is user subject) "threads" title)
              here  (threads-url subject))
        (longpage user (now) nil label title here
          (awhen (keep [and (cansee user _) (~subcomment _)]
                       (comments subject maxend*))
            (display-threads user it label title here))))
      (prn "No such user.")))

(def display-threads (user comments label title whence
                      (o start 0) (o end threads-perpage*))
  (tab
    (each c (cut comments start end)
      (row
        (if (acomment c)
            (display-comment-tree c user whence 0 t)
            (tab (display-item nil c user whence t))))
      (spacerow (if (acomment c) 15 5)))
    (when end
      (let newend (+ end threads-perpage*)
        (when (and (<= newend maxend*) (< end (len comments)))
          (spacerow 10)
          (row (tab (tr (td (hspace 0))
                        (td (hspace votewid*))
                        (tag (td class 'title)
                          (morelink (aand (- (len comments) end)
                                          (num it)
                                          "@it more...")
                                    display-threads
                                    comments label title end newend))))))))))

(def submissions (user (o limit))
  (map item (firstn limit (uvar user submitted))))

(def favorites (user (o limit))
  (map item (firstn limit (uvar user favorites))))

(def comments (user (o limit))
  (map item (retrieve limit acomment:item (uvar user submitted))))

(def subcomment (c)
  (some [and (acomment _) (is _!by c!by) (no _!deleted)]
        (ancestors c)))

(def ancestors (i)
  (accum a (trav i!parent a:item self:!parent:item)))

(def siblings (i)
  (map item (aand (item i!parent) (rem i!id it!kids))))

(def favorites-url (user) (+ "/favorites?id=" user))

(newsop favorites (id)
  (if id
      (favorites-page user id)
      (pr "No user specified.")))

(def favorites-page (user subject)
  (if (profile subject)
      (withs (title (+ subject "'s favorites")
              label (if (is user subject) "favorites" title)
              here  (favorites-url subject))
        (longpage user (now) nil label title here
          (awhen (keep [cansee user _]
                       (favorites subject maxend*))
            (display-threads user it label title here))))
      (prn "No such user.")))

; Submitted

(def submitted-url (user) (+ "/submitted?id=" user))

(newsop submitted (id)
  (if id
      (submitted-page user id)
      (pr "No user specified.")))

(def submitted-page (user subject)
  (if (profile subject)
      (with (label (+ subject "'s submissions")
             here  (submitted-url subject))
        (longpage user (now) nil label label here
          (if (or (no (ignored subject))
                  (is user subject)
                  (seesdead user))
              (aif (keep [and (metastory _) (cansee user _)]
                         (submissions subject))
                   (display-items user it label label here 0 perpage* t)))))
      (pr "No such user.")))


; RSS

(= static-header*!rss "application/rss+xml; charset=utf-8")

(newsop rss () (rsspage nil))

(newscache rsspage user 90
  (rss-stories (retrieve (len ranked-stories*) ~private&live ranked-stories*)))

(def rss-stories (stories)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr site-name*))
      (tag link (pr site-url*))
      (tag description (pr site-desc*))
      (each s stories
        (tag item
          (let comurl (+ site-url* (item-url s!id))
            (tag title    (pr (eschtml s!title)))
            (tag link     (pr (if (blank s!url) comurl (eschtml s!url))))
            (tag pubDate  (pr (rss-date s!time)))
            (tag comments (pr comurl))
            (tag description
              (cdata (link "Comments" comurl)))))))))

; RSS comments

(= static-header*!rsscomments "application/rss+xml; charset=utf-8")

(newsop rsscomments () (rsscpage nil))

(newscache rsscpage user 90
  (rss-comments (retrieve (len comments*) ~private&live comments*)))

(def rss-comment-title (c)
  (tostring
    (let p (superparent c 1)
      (if (acomment p)
          (pr c!by " to " p!by bar*)
        (pr c!by bar*)))
    (let s (superparent c)
      (pr (ellipsize s!title 50)))))

(def rss-comments (comments)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr (+ site-name* ": new comments")))
      (tag link (pr (+ site-url* "/newcomments")))
      (tag description (pr (+ site-desc* ": the conversation")))
      (each c comments
        (tag item
          (let comurl (+ site-url* (item-url c!id))
            (tag title    (pr (rss-comment-title c)))
            (tag link     (pr (if (blank c!url) comurl (eschtml c!url))))
            (tag pubDate  (pr (rss-date c!time)))
            (tag comments (pr comurl))
            (tag description
              (cdata
                (pr c!text)
                (br2)
                (pr "on: ")
                (let s (superparent c)
                  (link (ellipsize s!title 50) (+ site-url* (item-url s!id))))))))))))

; memoize RSS pages

(defbg memoize-rss 30 (memoize-rss))

(def memoize-rss ()
  (tostring:rsscpage nil)
  (tostring:rsspage nil)
  ; memoize item dates too.
  (each i ranked-stories*
    (aand i!time (moment-secs i!time)))
  (each c comments*
    (aand c!time (moment-secs c!time)))
  nil)

; User Stats

(newsop leaders () (leaderspage user))

(= nleaders* 20)

(newscache leaderspage user 1000
  (longpage user (now) nil "leaders" "Leaders" "leaders"
    (sptab
      (let i 0
        (each u (firstn nleaders* (leading-users))
          (tr (tdr:pr (++ i) ".")
              (td (userlink user u nil))
              (tdr:pr (karma u))
              (when (admin user)
                (tdr:prt (only.num (uvar u avg) 2 t t))))
          (if (is i 10) (spacerow 30)))))))

(= leader-threshold* 1)  ; redefined later

(def leading-users ()
  (sort (compare > [karma _])
        (users [and (> (karma _) leader-threshold*) (~admin _)])))

(adop editors ()
  (tab (each u (users [is (uvar _ auth) 1])
         (row (userlink user u)))))


(= update-avg-threshold* 0)  ; redefined later

(defbg update-avg 45
  (unless (or (empty profs*) (no stories*))
    (update-avg (rand-user [and (only.> (car (uvar _ submitted))
                                        (- maxid* initload*))
                                (len> (uvar _ submitted)
                                      update-avg-threshold*)]))))

(def update-avg (user)
  (= (uvar user avg) (comment-score user))
  (save-prof user))

(def rand-user ((o test idfn))
  (evtil (rand-key profs*) test))

; Ignore the most recent 5 comments since they may still be gaining votes.
; Also ignore the highest-scoring comment, since possibly a fluff outlier.

(def comment-score (user)
  (aif (check (nthcdr 5 (comments user 50)) [len> _ 10])
       (avg (cdr (sort > (map !score (rem !deleted it)))))
       nil))


; Comment Analysis

; Instead of a separate active op, should probably display this info
; implicitly by e.g. changing color of commentlink or by showing the
; no of comments since that user last looked.

(newsop active () (active-page user))

(newscache active-page user 600
  (listpage user (now) (actives user) "active" "Active Threads"))

(def actives (user (o n maxend*) (o consider 2000))
  (visible user (rank-stories n consider (memo active-rank))))

(= active-threshold* 1500)

(def active-rank (s)
  (sum [max 0 (- active-threshold* (item-age _))]
       (cdr (family s))))

(def family (i) (cons i (mappend family:item i!kids)))


(newsop newcomments () (newcomments-page user))

(newscache newcomments-page user 60
  (listpage user (now) (visible user (firstn maxend* comments*))
            "comments" "New Comments" "newcomments" nil))


; Doc

(defop formatdoc req
  (msgpage (get-user req) formatdoc* "Formatting Options"))

(= formatdoc-url* "formatdoc")

(= formatdoc*
"Blank lines separate paragraphs.
<p> Text after a blank line that is indented by two or more spaces is
reproduced verbatim.  (This is intended for code.)
<p> Text surrounded by asterisks is italicized, if the character after the
first asterisk isn't whitespace.
<p> Urls become links, except in the text field of a submission.<br><br>")


; Noprocrast

(def check-procrast (user)
  (or (no user)
      (no (uvar user noprocrast))
      (let now (seconds)
        (unless (uvar user firstview)
          (reset-procrast user))
        (or (when (< (/ (- now (uvar user firstview)) 60)
                     (uvar user maxvisit))
              (= (uvar user lastview) now)
              (save-prof user)
              t)
            (when (> (/ (- now (uvar user lastview)) 60)
                     (uvar user minaway))
              (reset-procrast user)
              t)))))

(def reset-procrast (user)
  (= (uvar user lastview) (= (uvar user firstview) (seconds)))
  (save-prof user))

(def procrast-msg (user whence)
  (let m (+ 1 (trunc (- (uvar user minaway)
                        (minutes-since (uvar user lastview)))))
    (pr "<b>Get back to work!</b>")
    (para "Sorry, you can't see this page.  Based on the anti-procrastination
           parameters you set in your profile, you'll be able to use the site
           again in " (plural m "minute") ".")
    (para "(If you got this message after submitting something, don't worry,
           the submission was processed.)")
    (para "To change your anti-procrastination settings, go to your profile
           by clicking on your username.  If <tt>noprocrast</tt> is set to
           <tt>yes</tt>, you'll be limited to sessions of <tt>maxvisit</tt>
           minutes, with <tt>minaway</tt> minutes between them.")
    (para)
    (w/rlink whence (underline (pr "retry")))
    ; (hspace 20)
    ; (w/rlink (do (reset-procrast user) whence) (underline (pr "override")))
    (br2)))


; Reset PW

(defopg resetpw req
  (with (user (get-user req)
         subject (arg req "u"))
    (resetpw-page user subject)))

(def resetpw-page (user subject (o msg))
  (let subject (or subject user)
    (if (~or (admin user) (is user subject))
        (pr "Sorry.")
      (minipage "Reset Password"
        (pr "Resetting password for @subject")
        (br2)
        (if msg
             (pr msg)
            (blank (uvar user email))
             (do (pr "Before you do this, please add your email address to your ")
                 (underlink "profile" (user-url user))
                 (pr ". Otherwise you could lose your account if you mistype
                      your new password.")))
        (br2)
        (uform user req (try-resetpw user subject (arg req "p"))
          (single-input "New password: " 'p 20 "reset" t))))))

(def try-resetpw (user subject newpw)
  (if (len< newpw 4)
      (resetpw-page user subject "Passwords should be a least 4 characters long.
                          Please choose another.")
      (do (set-pw subject newpw)
          (newspage user))))

(def send-resetpw (subject email)
  (send-email site-email*
              email
              "@site-name* password recovery"
              (let s (+ "Someone (hopefully you) requested we reset your password for @subject at @(do site-name*). If you want to change it, please visit "
                        site-url*
                        (flink [force-resetpw-page subject])
                        "\n\n"
                        "If not, just ignore this message.\n")
                (when (readenv "DEV" nil)
                  (disp s (stderr)))
                s)))

(def force-resetpw-page (subject (o msg))
  (minipage "Reset Password for @subject"
    (if msg (pr msg))
    (br2)
    (aform (fn (req) (try-force-resetpw subject (arg req "pw")))
      (single-input "New password: " 'pw 20 "reset" t))))

(def try-force-resetpw (subject newpw)
  (if (len< newpw 4)
      (force-resetpw-page subject "Passwords should be a least 4 characters long.
                          Please choose another.")
      (do (set-pw subject newpw)
          (newspage nil))))

(defop forgot req (forgot-page))

(def forgot-page ((o subject (arg "acct")))
  (prbold "Reset your password")
  (br2)
  (aform (fn (req)
           (aand (arg req "acct")
                 (profile it)
                 (unless (blank it!verified)
                   (send-resetpw it!id it!verified)))
           (msgpage nil "Password recovery message sent. If you don't see it, you might want to check your spam folder."))
    (inputs (acct username 20 subject 'plain 'autofocus))
    (br)
    (submit "Send reset email")))

(def forgot-url ((o subject (arg "acct")))
  (if subject (+ "/forgot?acct=" (eschtml subject)) "/forgot"))

(defhook login-form args
  (link "Forgot your password?" (forgot-url)))

; Scrubrules

(defopa scrubrules req
  (scrub-page (get-user req) scrubrules*))

; If have other global alists, generalize an alist edit page.
; Or better still generalize vars-form.

(def scrub-page (user rules (o msg nil))
  (minipage "Scrubrules"
    (when msg (pr msg) (br2))
    (uform user req
           (with (froms (lines (arg req "from"))
                  tos   (lines (arg req "to")))
             (if (is (len froms) (len tos))
                 (do (todisk scrubrules* (map list froms tos))
                     (scrub-page user scrubrules* "Changes saved."))
                 (scrub-page user rules "To and from should be same length.")))
      (pr "From: ")
      (tag (textarea name 'from
                     cols (apply max 20 (map len (map car rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map car rules))))
      (pr " To: ")
      (tag (textarea name 'to
                     cols (apply max 20 (map len (map cadr rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map cadr rules))))
      (br2)
      (submit "update"))))


; Site pages

(def pages-url ((o anchor nil)) (+ "/pages" (aand anchor "#@it")))

(defopa pages req
  (edit-pages-page (get-user req) (arg req "msg")))

(def edit-pages-page (user (o msg nil))
  (minipage "Edit Pages"
    (when msg (pr msg) (br2))

    (urform user req
           (do (todisk guidelines-page* (md-from-form (arg req "guidelines") nil t))
               "/guidelines.html")
      (idtab "guidelines"
        (row (underlink "/guidelines.html"))
        (row (textarea "guidelines" 80 60
               (pr:esc-tags:unmarkdown guidelines-page* t)))
        (row (submit "update /guidelines.html"))))

    (urform user req
           (do (todisk welcome-page* (md-from-form (arg req "welcome") nil t))
               "/welcome.html")
      (idtab "welcome"
        (row (underlink "/welcome.html"))
        (row (textarea "welcome" 80 60
               (pr:esc-tags:unmarkdown welcome-page* t)))
        (row (submit "update /welcome.html"))))

    (urform user req
           (do (todisk bookmarklet-page* (arg req "bookmarklet"))
               "/bookmarklet.html")
      (idtab "bookmarklet"
        (row (underlink "/bookmarklet.html"))
        (row (textarea "bookmarklet" 80 60
               (pr:esc-tags bookmarklet-page*)))
        (row (submit "update /bookmarklet.html"))))))


; Bookmarklet

(diskfile bookmarklet-page* (+ newsdir* "bookmarklet.html") "
<p id=\"first\">
    Thanks to Phil Kast for writing the <a href=\"https://news.ycombinator.com/bookmarklet.html\"><u>original bookmarklet</u></a>.
    <br><br> When you click on the bookmarklet, it will submit the page you're on to Lambda News. To install, drag this link to your browser toolbar:
    <br><br>
</p>

<center>

<!-- <div style=\"margin: auto; padding: 16px; width: 30%; background: #f7f7f7;\"> -->

<a style=\"color: #777; font-size: 2em;\" rel=\"nofollow\" href=\"javascript:q=location.href;if(document.getSelection){d=document.getSelection();}else{d='';}p=document.title;void(open('https://www.laarc.io/submitlink?l=news&u='+encodeURIComponent(q)+'&t='+encodeURIComponent(p),'LambdaNews','toolbar=no,width=700,height=600'));\">
  <u>post to @(do site-abbrev*)</u>
</a>

<br><br>
<br><br> On mobile devices, create a new bookmark, edit it, and replace its url with the following text:
<br><br>

<textarea cols=\"60\" rows=\"7\" wrap=\"virtual\" name=\"about\">javascript:q=location.href;if(document.getSelection){d=document.getSelection();}else{d='';}p=document.title;void(open('https://www.laarc.io/submitlink?l=news&u='+encodeURIComponent(q)+'&t='+encodeURIComponent(p),'LambdaNews','toolbar=no,width=700,height=600'));</textarea>

<br><br> It should look like this:
<br><br>
<img src=\"https://i.imgur.com/J1kFydT.png\" width=\"300px\" />
")

(newsop bookmarklet.html ()
  (msgpage user bookmarklet-page* "Bookmarklet" (pages-url "bookmarklet")))


; Guidelines

(diskfile guidelines-page* (+ newsdir* "guidelines.html") (md-from-form "
_What to Submit_

On-Topic: STEM. Humanities. Humor. Anything intellectually engaging and pro-social.

Off-Topic: That which is flame bait or vacuous.

_In Submissions_

If you submit a link to a video or pdf, please warn readers by appending [video] or [pdf] to the title.

Please submit the original source.

_In Comments_

Be civil. On difficult subjects in particular, you should work hard at being diplomatic. (It helps to picture yourself speaking to a friend.)

When disagreeing, reply to the argument instead of calling names.  \"That is idiotic; 1 + 1 is 2, not 3\" can be shortened to \"1 + 1 is 2, not 3.\"

Assume good faith.

Eschew flamebait.

Please limit your use of uppercase; it looks like shouting and is hard to read.
" nil t))

(newsop guidelines.html ()
  (msgpage user guidelines-page* "Guidelines" (pages-url "guidelines")))

; Welcome

(diskfile welcome-page* (+ newsdir* "welcome.html") (md-from-form
"_Welcome to @(do site-name*)_

<a href=\"/\"><u>@(do site-name*)</u></a> is a bit different from other community sites, and we'd appreciate it if you'd take a minute to read the following as well as the <a href=\"/guidelines.html\"><u>official guidelines</u></a>.

@site-abbrev* is an experiment. As a rule, a community site that becomes popular will decline in quality. Our hypothesis is that this is not inevitable—that by making a conscious effort to resist decline, we can keep it from happening.

Essentially there are two rules here: don't post or upvote crap links, and don't be rude or dumb in comment threads.

A crap link is one that's only superficially interesting. Stories on @site-abbrev* don't have to be about hacking, because good hackers aren't only interested in hacking, but they do have to be deeply interesting.

What does \"deeply interesting\" mean? It means stuff that teaches you about the world. A story about a robbery, for example, would probably not be deeply interesting. But if this robbery was a sign of some bigger, underlying trend, perhaps it could be.

The worst thing to post or upvote is something that's intensely but shallowly interesting: gossip about famous people, funny or cute pictures or videos, partisan political articles, etc. If you let that sort of thing onto a news site, it will push aside the deeply interesting stuff, which tends to be quieter.

The most important principle on @(do site-abbrev*), though, is to make thoughtful comments. Thoughtful in both senses: civil and substantial.

The test for substance is a lot like it is for links. Does your comment teach us anything? There are two ways to do that: by pointing out some consideration that hadn't previously been mentioned, and by giving more information about the topic, perhaps from personal experience.  Whereas comments like \"LOL!\" or worse still, \"That's retarded!\" teach us nothing.

Empty comments can be ok if they're positive.  There's nothing wrong with submitting a comment saying just \"Thanks.\" What we especially discourage are comments that are empty and negative—comments that are mere name-calling.

Which brings us to the most important principle on @(do site-abbrev*): civility. Since long before the web, the anonymity of online conversation has lured people into being much ruder than they'd be in person. So the principle here is: don't say anything you wouldn't say face to face.  This doesn't mean you can't disagree. But disagree without calling names. If you're right, your argument will be more convincing without them.
" nil t))

(newsop welcome.html ()
  (msgpage user welcome-page* "Welcome" (pages-url "welcome")))

(defcache lambdas 300
  (with (acc nil cs (table))
    (each-loaded-item i
      (each k i!keys
        (when (headmatch "/l/" (string k))
          (pushnew k acc)
          (= (cs k) (or (cs k) 0))
          (++ (cs k)))))
    (let r nil
      (each k acc
        (push (list k (cs k)) r))
      (sort (fn (a b) (> a.1 b.1)) r))))

(newscache tags-page user 90
  (longpage user (now) nil "tags" "Tags" "/l"
    (sptab
      (row (underlink "tag" "/l?sort") (underlink "count" "/l"))
      (let tags (lambdas)
        (each (site count) (if (arg "sort") (sort (fn (a b) (< a.0 b.0)) tags) tags)
          (tr (td (pr (link site))) (td count)))))))

; Abuse Analysis

(adop badsites ()
  (sptab
    (row "Dead" "Days" "Site" "O" "K" "I" "Users")
    (each (site deads) (with (banned (banned-site-items)
                              pairs  (killedsites))
                         (+ pairs (map [list _ (banned _)]
                                       (rem (fn (d)
                                              (some [caris _ d] pairs))
                                            (keys banned-sites*)))))
      (let ban (car (banned-sites* site))
        (tr (tdr (when deads
                   (onlink (len deads)
                           (listpage user (now) deads
                                     nil (+ "killed at " site) "badsites"))))
            (tdr (when deads (pr (round (days-since ((car deads) 'time))))))
            (td site)
            (td (w/rlink (do (set-site-ban user site nil) "badsites")
                  (fontcolor (if ban gray.220 black) (pr "x"))))
            (td (w/rlink (do (set-site-ban user site 'kill) "badsites")
                  (fontcolor (case ban kill darkred gray.220) (pr "x"))))
            (td (w/rlink (do (set-site-ban user site 'ignore) "badsites")
                  (fontcolor (case ban ignore darkred gray.220) (pr "x"))))
            (td (each u (dedup (map !by deads))
                  (userlink user u nil)
                  (pr " "))))))))

(defcache killedsites 300
  (let bads (table [each-loaded-item i
                     (awhen (and i!dead (sitename i!url))
                       (push i (_ it)))])
    (with (acc nil deadcount (table))
      (each (site items) bads
        (let n (len items)
          (when (> n 2)
            (= (deadcount site) n)
            (insort (compare > deadcount:car)
                    (list site (rev items))
                    acc))))
      acc)))

(defcache banned-site-items 300
  (table [each-loaded-item i
           (awhen (and i!dead (check (sitename i!url) banned-sites*))
             (push i (_ it)))]))

; Would be nice to auto unban ips whose most recent submission is > n
; days old, but hard to do because of lazy loading.  Would have to keep
; a table of most recent submission per ip, and only enforce bannnedness
; if < n days ago.

(adop badips ()
  (withs ((bads goods) (badips)
          (subs ips)   (sorted-badips bads goods))
    (sptab
      (row "IP" "Days" "Dead" "Live" "Users")
      (each ip ips
        (tr (td (let banned (banned-ips* ip)
                  (w/rlink (do (set-ip-ban user ip (no banned))
                               "badips")
                    (fontcolor (if banned darkred) (pr ip)))))
            (tdr (when (or (goods ip) (bads ip))
                   (pr (round (days-since
                                (max (aif (car (goods ip)) it!time 0)
                                     (aif (car (bads  ip)) it!time 0)))))))
            (tdr (onlink (len (bads ip))
                         (listpage user (now) (bads ip)
                                   nil (+ "dead from " ip) "badips")))
            (tdr (onlink (len (goods ip))
                         (listpage user (now) (goods ip)
                                   nil (+ "live from " ip) "badips")))
            (td (each u (subs ip)
                  (userlink user u nil)
                  (pr " "))))))))

(defcache badips 300
  (with (bads (table) goods (table))
    (each-loaded-item s
      (if (and s!dead (commentable s))
          (push s (bads  s!ip))
          (push s (goods s!ip))))
    (each (k v) bads  (zap rev (bads  k)))
    (each (k v) goods (zap rev (goods k)))
    (list bads goods)))

(def sorted-badips (bads goods)
  (withs (ips  (let ips (rem [len< (bads _) 2] (keys bads))
                (+ ips (rem [mem _ ips] (keys banned-ips*))))
          subs (table
                 [each ip ips
                   (= (_ ip) (dedup (map !by (+ (bads ip) (goods ip)))))]))
    (list subs
          (sort (compare > (memo [badness (subs _) (bads _) (goods _)]))
                ips))))

(def badness (subs bads goods)
  (* (/ (len bads)
        (max .9 (expt (len goods) 2))
        (expt (+ (days-since (aif (car bads) it!time 0))
                 1)
              2))
     (if (len> subs 1) 20 1)))


(edop flagged ()
  (display-selected-items user [retrieve maxend* [len> _!flags 0]  _] "flagged"))

(def flagged (i)
  (and (~mem 'nokill i!keys)
       (or (mem 'flagged i!keys)
           (len> i!flags flag-kill-threshold*))))

(edop killed ()
  (display-selected-items user [retrieve maxend* !dead _] "killed"))

(def display-selected-items (user f whence)
  (display-items user (f stories*) nil nil whence)
  (vspace 35)
  (color-stripe textgray)
  (vspace 35)
  (display-items user (f comments*) nil nil whence))


; Rather useless thus; should add more data.

(adop badguys ()
  (tab (each u (sort (compare > [uvar _ created])
                     (users [ignored _]))
         (row (userlink user u nil)))))

(adop badlogins ()  (logins-page bad-logins*))

(adop goodlogins () (logins-page good-logins*))

(def logins-page (source)
  (sptab (each (time ip user) (firstn 100 (rev (qlist source)))
           (row time ip user))))


; Stats

(adop optimes ()
  (sptab
    (tr (td "op") (tdr "avg") (tdr "med") (tdr "req") (tdr "total"))
    (spacerow 10)
    (each name (sort < newsop-names*)
      (tr (td name)
          (let ms (only.avg (qlist (optimes* name)))
            (tdr:prt (only.round ms))
            (tdr:prt (only.med (qlist (optimes* name))))
            (let n (opcounts* name)
              (tdr:prt n)
              (tdr:prt (and n (round (/ (* n ms) 1000))))))))))

(newsop votes () (votes-page user))

(newscache votes-page user 90
  (longpage user (now) nil "votes" "Votes" "votes"
    (sptab
      (tr (td "") (td "age") (tdr "id") (td "dir") (td "score") (td "") (tdr "voter") (td "") (tdr "author") (td "title"))
      (spacerow 10)
      (let i 0
        (each x (map [let (id (ts ip who dir score)) _
                       (unless (is who item.id!by)
                         (list (text-age (minutes-since ts))
                               id dir (+ 1 score) who item.id!by
                               (if (metastory item.id)
                                    "@item.id!title"
                                   (is item.id!type 'pollopt)
                                    "[@(do item.id!text)]"
                                   "> @(ellipsize item.id!text)")))]
                     (sort (compare > car:cadr) (apply + (map tablist (vals votes*)))))
          (whenlet (age id dir score who by title) x
            (with (age (multisubst '(("hour" "hr") ("minute" "m") (" ago" "") (" " "")) age))
              (row "@(++ i). " age (pr:itemlink item.id id) dir score
                   (tdr (if (or (is user (str who)) (admin user)) (userlink user who) (pr "[hidden]")))
                   (tdr (userlink user by) (pr ":"))
                   (tag (span) (itemlink item.id (multisubst '(("<p>" " ")) title))))
              (spacerow 10))))))))

(adop noobs ()
  (sptab
    (tr (td "") (td "age") (td "id") (td "email") (td "score") (td "votes"))
    (spacerow 10)
    (let i 0
      (each u (sort (compare > !created) (map profile (users)))
        (withs (rank "@(++ i). "
                age (text-age (user-age u!id))
                age (multisubst '(("hour" "hr") ("minute" "m") (" ago" "") (" " "")) age)
                nvotes (len:votes u!id))
          (row rank age (userlink user u!id)
               (unless (blank u!email) (pr:link u!email "mailto:@u!email"))
               u!karma nvotes))))))


(defop topcolors req
  (minipage "Custom Colors"
    (tab
      (each c (dedup (map downcase (trues [uvar _ topcolor] (users))))
        (tr (td c) (tdcolor (hex>color c) (hspace 30)))))))


(or= chess-board* (trim (rem #\return "
rnbqkbnr
pppppppp




PPPPPPPP
RNBQKBNR
")))

(def chess-encode (x)
  (multisubst
    `(("K" "&#9812;")
      ("Q" "&#9813;")
      ("R" "&#9814;")
      ("B" "&#9815;")
      ("N" "&#9816;")
      ("P" "&#9817;")
      ("k" "&#9818;")
      ("q" "&#9819;")
      ("r" "&#9820;")
      ("b" "&#9821;")
      ("n" "&#9822;")
      ("p" "&#9823;")
      (" " "&nbsp;"))
    (string x)))

(def chars (x)
  (accum a
    (each c x
      (a c))))

(def chess ((o board chess-board*))
  (accum a
    (each y (lines chess-board*)
      (a:accum a
        (map a:chess-encode (chars y))))))

(def chess-piece (text (o a) (o b) (o from) (o to))
  (let op (if (~blank from) (+ "from=" from "&to=") "from=")
    (tag (form method 'post action (+ "/chess?" op a "," b))
      (gentag input type 'submit value2 text style "width: 3em;"))))

(def chess-board ((o user) (o from) (o to) (o board chess-board*))
  (idtab "chess"
    (with (i 0 j 0 from (or from "") to (or to ""))
      (each y (chess board)
        (= i 0)
        (++ j)
        (row (chess-piece y.0 (++ i) j from to)
             (chess-piece y.1 (++ i) j from to)
             (chess-piece y.2 (++ i) j from to)
             (chess-piece y.3 (++ i) j from to)
             (chess-piece y.4 (++ i) j from to)
             (chess-piece y.5 (++ i) j from to)
             (chess-piece y.6 (++ i) j from to)
             (chess-piece y.7 (++ i) j from to))))))

(def chess-page (user (o from) (o to) (o board chess-board*))
  (longpage user (now) nil "chess" "Chess" "chess"
    (center
      (chess-board user from to board))))

(def chess-at (x y)
  (+ (* (- y 1) 9) (- x 1)))

(newsop chess (from to)
  (when (and (~blank from) (~blank to))
    (let ((a b) (x y)) (map [map int (tokens _ #\,)] (list from to))
      (swap (chess-board* (chess-at x y))
            (chess-board* (chess-at a b)))
      (wipe (lncache* "chess")))
    (wipe from)
    (wipe to))
  (if (blank from) (wipe to))
  (chess-page user from to))

(defmemo gamma-gray (percent)
  (withs (x (expt (/ percent 1.0) (/ 1.0 2.2))
          n (min 255 (trunc (* x 256)))
          m (min 255 (trunc (* x 300))))
    (color n n m)))

(= place-default-colors*
   (obj #\0 (gamma-gray 0.05)
        #\1 (gamma-gray 0.10)
        #\2 (gamma-gray 0.20)
        #\3 (gamma-gray 0.30)
        #\4 (gamma-gray 0.40)
        #\5 (gamma-gray 0.50)
        #\6 (gamma-gray 0.60)
        #\7 (gamma-gray 0.70)
        #\8 (gamma-gray 0.80)
        #\9 (gamma-gray 0.90)
        #\# (color  27 136  20)
        #\$ (color  27 140 135)
        #\% (color 120 214 208)
        #\@ (color  27 136  20)
        #\B (color  40 173 146)
        #\E (color 255 253  55)
        #\K (color 189   8  28)
        #\M (color 191  18 181)
        #\N (color  96 154  51)
        #\P (color  31 143 240)
        #\Q (color 252 126  40)
        #\R (color 255 178  54)
        #\W (color 139  69  19)
        #\X (color 173 181 189)
        #\Y (color 233 236 239)
        #\Z site-color*
        #\b (color 238 100  92)
        #\c (color 108  80  57)
        #\d (color 253 230 213)
        #\e (color 241 229 214)
        #\f (color 173 152 149)
        #\g (color 189 183 172)
        #\h (color 156 139 110)
        #\i (color 137 100 100)
        #\j (color 205 150  88)
        #\k (color  24 173 241)
        #\l (color 220 198 170)
        #\m (color 209 194 201)
        #\n (color   0   0   0)
        #\o (color 232 224 200)
        #\p (color  54  54  54)
        #\q (color 112  52 142)
        #\r (color 245  54  92)
        #\s (color 230 173 153)
        #\t (color  38  72 121)
        #\u (color 213 240 169)
        #\~ (color 118 245  69)
        #\space white))

(disktable place-colors* (+ newsdir* "place-colors")
  place-default-colors*)

(def place-encode (x)
  (or (place-colors* x) black))

(def place-piece (text (o a) (o b) (o from) (o to) (o bgcol (color 0 255 0)))
  (withs (op (if (~blank from) (+ "from=" from "&to=") "from=")
          url (if (~blank from) "/placeop" "/place")
          whence (if (~blank from) (string "#" from) (string "#" a "," b)))
    (tag (form method 'post action (string url "?" op a "," b) onsubmit "return placeSubmit(this);")
      (gentag input type 'submit value2 text style "background-color: #@(hexrep bgcol);"))))

(= place-submit-url* "/submitlink?l=ask%20place&t=Ask%20laarc:%20what%20should%20we%20draw%20next%3F"
   place-css* "
#place tr    { -webkit-appearance: none; border-radius: 0; display: flex !important; border-collapse: unset; border: 0px; outline: none; padding: 0px; margin: 0px; overflow-wrap: normal; }
#place td    { -webkit-appearance: none; border-radius: 0; display:    inline-block; border-collapse: unset; border: 0px; outline: none; padding: 0px; margin: 0px; }
#place form  { outline: none; margin-block-end: 0px; margin: 0px; padding: 0px; }
#place input { touch-action: manipulation; -webkit-appearance: none; border-radius: 0; outline: none; margin-block-end: 0px; margin: 0px; padding: 0px; height: 1.0em; width: 1.0em; border: 0px; text-shadow: #000 1px 0 10px; color: white; }
"
   place-info* "
If you want to coordinate, come into our @(tostring:underlink 'discord discord-url*), or @(tostring:underlink 'submit place-submit-url*) to @(tostring:underlink '/l/place).
&nbsp;
Click the tiles. The first click selects a color (the tile will be marked with an x).
To clear the selection, click the x again, or click here: @(tostring:underlink 'clear '/place)
")

(def place-board ((o user) (o from) (o to) (o board place-board*))
  (pr:tostring
    (tag (style) (pr place-css*))
    (tag (table id "place" style "table-layout: fixed; width: 100%; overflow: hidden;")
      (tag (tbody style "display: block; max-width: 100vw; overflow: scroll;")
        (each line (lines:trim place-info*)
          (row line))
        (spacerow 10)
        (withs (j -1 from (or from "") to (or to "")
                (((o a -1) (o b -1))) (map [map int (tokens _ #\,)] (list from)))
          (each y (lines board)
            (++ j)
            (tag tr
              (when (is j 0) (td "palette:"))
              (forlen i y
                (tag (td id (string i "," j))
                  (place-piece (if (and (is i a) (is j b)) "x" "") i j from to (place-encode (y i))))))
            (when (is j 0)
              (spacerow 4))
            )))))
  (flushout))

(def place-page (user (o from) (o to) (o board place-board*))
  (longpage user (now) nil "place" "place" "place"
    (center
      (place-board user from to board))))

(def place-at (x y (o board place-board*))
  (let i -1
    (while (> y 0)
      (= i (pos #\newline board (+ i 1)))
      (-- y))
    (+ i x 1)))

(def place-blit (text x y (o board place-board*))
  (let j -1
    (each line (lines text)
      (++ j)
      (forlen i line
        (= (board (place-at (+ x i) (+ y j)))
           (line i))))))

(or= place-events* (queue)
     place-event-id* 0)

(= place-event-limit* 20
   place-event-lasts* 25
   place-event-sleep* 0.5)

(def place-event! (type evt)
  (let e (copy evt)
    (= e!time (str (/ (now) 1000))
       e!type type)
    (atomic
      (= e!id (str (++ place-event-id*)))
      (enq-limit e place-events* place-event-limit*))
    e))

(def place-update (x y (o board place-board*))
  (place-event! "put"
    (obj path (string x "," y)
         data (obj style (obj backgroundColor
                              "#@(hexrep (place-encode (board (place-at x y board))))")))))

(def place-reset ()
  (= place-events* (queue)))

(def place-kill ((o reset))
  (atomic
    (when reset (place-reset))
    (place-event! "kill")))

(newsopr placeop (from to) (placeop from to))
(newsop placeset (from to) (placeop from to))

(def placeop (from to)
  (let ((a b) (x y)) (map [map int (tokens _ #\,)] (list from to))
    (when (errsafe (> y 0))
      (atomic
        (= (place-board* (place-at x y))
           (place-board* (place-at a b)))
        (place-update x y))
      (wipe (lncache* "place")))
    (if (is from to) "/place" (string "/place?from=" (if (is y 0) to from)))))

(= (static-header* 'place.txt) "text/plain")

(defop place.txt req
  (pr place-board*)
  (flushout))

(= (static-header* 'place.json) "application/json")

(defop place.json req (pr:place-json))

(defcache place-json 5
  (tostring:write-json
    (obj board
         (map [map str (chars _)]
              (lines place-board*))
         colors
         (listtab
           (each (c col) place-colors*
             (out (sym:string c)
                  (obj r col!r g col!g b col!b hex "#@(hexrep col)")))))))

(= (static-header* 'place.events) "text/event-stream;
Access-Control-Allow-Origin: *
Cache-Control: no-cache;
X-Accel-Buffering: no")

(defop place.events ()
  (with (seen (obj) ts (now))
    (while (< (since ts t) place-event-lasts*)
      (each x (qlist place-events*)
        (unless (seen x!id)
          (= (seen x!id) t)
          (prn "event: " (or x!type "put"))
          (pr "data: ") (write-json x)
          (prn)
          (prn)
          (flushout)))
      (sleep place-event-sleep*))))

(newsop place (from to)
  (if (blank from) (wipe to))
  (place-page user from to))

(def lorem ()
  ; https://news.ycombinator.com/item?id=15609972
  (trim:shell "@(if (macos?) 'gshuf 'shuf) -n 32 /usr/share/dict/words | tr '\\n' ' '"))

(defcache terry 1 ()
  (awhen (lorem)
    (unless (blank it)
      (prn "\"God says... @it\""))))

(def prize-msg ()
  "You found an easter egg. Message @site-discord* on
  @(tostring:underlink 'discord discord-url*) or email @site-email* to
  claim a prize.")

(newsop test ()
  (msgpage user (prize-msg)))

;; eventually generalize this to handle arbitrary batch jobs while
;; trapping and reporting errors properly. For now, this is ugly but
;; handy to have around for flushing all site data to disk and
;; to firebase.

(or= resave-items-finished* 0 resave-items-total*    0
     resave-items-thread* nil resave-items-errors* nil resave-items-failed* nil)
(= resave-items-throttle* 0.3)

(def resave-items-thread ((o throttle resave-items-throttle*))
  (= resave-items-done*   0   resave-items-total*  0
     resave-items-errors* nil resave-items-failed* nil)
  (each-loaded-item i
    (++ resave-items-total*))
  (each-loaded-item i
    (sleep throttle)
    (on-err (fn (c) (push (list c i) resave-items-errors*))
            (fn () (save-item i) (push i resave-items*)))
    (++ resave-items-done*)))

(def stop-resaving-items ()
  (when resave-items-thread*
    (kill-thread resave-items-thread*)
    (wipe resave-items-thread*)))

(def start-resaving-items ((o throttle resave-items-throttle*))
  (stop-resaving-items)
  (= resave-items-thread* (thread (resave-items-thread throttle))))

(= place-file* (+ newsdir* "place.txt"))

(diskfile place-board* place-file*
"lmostucdefghijEKQRBNPkqrbpXYZWn0123456789KQRBNMkqrbpXYZWn0123456789 KQRBNPkqrbpXYZWn0123456789KQRBNPkqrbpXYZWn0123456789
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 XXX        XXX  XXX        XXX
 YYY        YYY  YYY        YYY
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 XXX        XXX  XXX        XXX
 YYY        YYY  YYY        YYY
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 XXX        XXX  XXX        XXX
 YYY        YYY  YYY        YYY
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR")


(defbg save-place 15 (save-place))

(def save-place ()
  (todisk place-board*)
  (todisk place-colors*))

(def load-place ()
  (let was (copy place-board*)
    (= place-board* (filechars place-file*))
    was))

run-news
