(= tpu-zones*  '((europe-west4-a euw4a)
                 (us-central1-a usc1a)
                 (us-central1-f usc1f))
   tpu-cidr*    '((v3-2048 23)
                  (v3-1024 24)
                  (v3-512 25)
                  (v3-256 26)
                  (v3-128 27)
                  (v3-64 28)
                  (v3-32 29)
                  (v3-8 29)
                  (v2-2048 23)
                  (v2-1024 24)
                  (v2-512 25)
                  (v2-256 26)
                  (v2-128 27)
                  (v2-64 28)
                  (v2-32 29)
                  (v2-8 29)))

(def gcloud-zone-id (zone (o zones tpu-zones*))
  (let zone (sym zone)
    (catch
      (each (name id) zones
        (when (in zone name id)
          (throw id))))))

(def gcloud-zone-name (zone (o zones tpu-zones*))
  (let zone (sym zone)
    (catch
      (each (name id) zones
        (when (in zone name id)
          (throw name))))))

(def gcloud-parse-zone-name (name)
  (or (gcloud-zone-name name)
      (aand (tokens (string name) #\-)
            (map gcloud-zone-name it)
            (find [isa _ 'sym] it))))

(def safeint (x)
  (if (isa x 'int) x
      (aand (saferead:string x) (if (isa it 'int) it))))

(def gcloud-tpu-index (index)
  (aand (safeint index)
        (when (and (exact it) (>= it 0)) it)))

(def tpu-index (p)
  (let id (if (isa p 'table) p!id p)
    (int:last:tokens (str id) #\-)))

(def tpu-zone (p)
  (aand (if (isa p 'table) p!zone p)
        (gcloud-parse-zone-name it)))

(def gcloud-tpu-cidr (accelerator)
  (alref tpu-cidr* (sym accelerator)))

(def tpu-possible-indexes ((o pod? t))
  (if pod?
      (aand (range 0 64)
            (rem 46 it)) ; 10.46.0.0/16 is reserved for non-TPU pods
      (range 0 255)))

(def tpu-used-indexes (zone (o pod? t) (o ps (sorted-tpus)))
  (let zone (sym zone)
    (map tpu-index (keep [and (is (tnil pod?) (tnil:tpu-pod? _))
                              (is (gcloud-zone-id _!zone)
                                  (gcloud-zone-id zone))]
                         ps))))

(def tpu-valid-indexes (zone (o pod? t) (o ps (sorted-tpus)))
  (difference is
              (tpu-possible-indexes pod?)
              (tpu-used-indexes zone pod? ps)))

(def tpu-pod-type? (accelerator)
  (~in (sym accelerator) 'v1-8 'v2-8 'v3-8))

(def tpu-valid-index (index accelerator zone (o pod? (tpu-pod-type? accelerator)) (o ps (sorted-tpus)))
  (find index (tpu-valid-indexes zone pod? ps)))

(def tpu-ip-range (accelerator index)
  (let cidr (gcloud-tpu-cidr accelerator)
    (if (no cidr) (err "Bad accelerator")
        (in (sym accelerator) 'v1-8 'v2-8 'v3-8)
        (cat "10.48." index ".0/" cidr)
        (cat "10." (+ 2 index) ".0.0/" cidr))))

(def tpu-create (index accelerator zone (o preemptible t) (o async t) (o version) (o name))
  (withs (zone-id (gcloud-zone-id zone)
          zone-name (gcloud-zone-name zone)
          tpu-index (tpu-valid-index (gcloud-tpu-index index) accelerator zone-id)
          cidr (gcloud-tpu-cidr accelerator)
          ip-range (tpu-ip-range accelerator index))
    (aif (no tpu-index) (err "Bad TPU index. (Is it already in use, or outside the valid range?)" index)
         (no zone-id) (err "Bad zone id")
         (no zone-name) (err "Bad zone name")
         (no cidr) (err "Bad accelerator")
         (no ip-range) (err "Failed to map IP range")
      (let id (sym (or name (+ "tpu-" accelerator "-" zone-id "-" tpu-index)))
        (tpu-ensure id preemptible)
        (when (tpu-pod? id)
          (tpu-persist id preemptible))
        (shelllog 'gcloud 'compute 'tpus 'create id
               '--zone zone-name
               '--network (+ "tpu-" zone-id)
               '--range ip-range
               '--version (or version "1.15")
               '--accelerator-type accelerator
               (and preemptible '--preemptible)
               (and async '--async))))))

(def tpu-reimage (id version (o async t))
  (withs (p (tpu-parse-info id)
          zone-name (p 'zone))
    (aif (no zone-name) (err "Bad zone name")
      (let id p!id
        (shelllog 'gcloud 'compute 'tpus 'reimage id
               '--zone zone-name
               '--version version
               (and async '--async))))))

(def tpu-parse-info (id (o preemptible t))
  (withs ((prefix type cores zoneid index) (aand (tokens (string id) #\-)
                                                 (if (<= (len it) 3)
                                                     (join (cut it 0 -2) (list "v3" "8") (cut it -2))
                                                     it))
          zone (gcloud-zone-name zoneid)
          zoneid (gcloud-zone-id zoneid)
          index (gcloud-tpu-index index)
          accelerator (sym:string type "-" cores)
          cidr (gcloud-tpu-cidr accelerator))
    (and zone index accelerator cidr
         (obj zone zone
              id id
              type accelerator
              preemptible preemptible))))

(def tpu-delete (name (o zone (gcloud-parse-zone-name name)) (o async t))
  (tpu-unensure (sym name))
  (aand (goodname:string name)
        (gcloud-zone-name zone)
        (do (shelllog 'gcloud 'compute 'tpus 'delete name '--zone it '--quiet (and async '--async))
            t)))

(def tpu-describe (name (o zone (gcloud-parse-zone-name name)))
  (aand (goodname:string name)
        (gcloud-zone-name zone)
        (shell 'tpu-describe name '--zone it)
        (if (headmatch "ERROR: " it) (err it) it)))

(def tpu-preemptible? (name)
  (aand (errsafe:tpu-describe name)
        (yes (posmatch "preemptible: true" it))))

(def tpu-get-version (name (o unknown "1.15"))
  (or (aand (errsafe:tpu-describe name)
            (lines it)
            (find [headmatch "tensorflowVersion:" _] it)
            (last:splitby ": " it)
            (strip it "'"))
      unknown))

(def tpu-get-service-account (name)
  (aand (errsafe:tpu-describe name)
        (lines it)
        (find [headmatch "serviceAccount:" _] it)
        (last:splitby ": " it)))

(defcache tpu-service-account 180 ()
  (catch
    (each p (sorted-tpus)
      (awhen (tpu-get-service-account p!id)
        (throw it)))))

(= (static-header* 'tpu-service-account.json) "application/json")

(newsop tpu-service-account.json ()
  (aif (tpu-service-account)
       (write-json it)
       (pr "null")))

(= (static-header* 'tpu-service-account) "text/plain")

(newsop tpu-service-account ()
  (aif (tpu-service-account)
       (pr it)
       (pr "unknown")))

(def find-tpu (name (o ps (tpus)))
  (find [is _!id (sym name)] ps))

(def get-tpu (name (o k) (o ps (tpus)))
  (aand (find-tpu name ps)
        (if k (it k) it)))

(def tpu-try-recreate (name preemptible version)
  (let status (get-tpu name 'status)
    (if (no status)
        (let p (tpu-parse-info name preemptible)
          (tpu-create (tpu-index p!id) p!type p!zone p!preemptible 'async version p!id))
        (is status "READY") (tpu-unensure name)
        (is status "PREEMPTED")
        (do (srvlog 'shell status name preemptible)
            (tpu-recreate name preemptible (or version (tpu-get-version name)))))))

(or= tpu-recreate* () tpu-persistent* ())

(def tpu-recreations ()
  (union (fn (x y) (is (car x) (car y)))
         tpu-recreate*
         tpu-persistent*))

(def tpu-try-recreations ()
  (each (name preemptible (o version)) (tpu-recreations)
    (tpu-try-recreate name preemptible version))
  (tpu-recreations))

(def tpu-unensure (name)
  (let name (sym name)
    (pull [caris _ name] tpu-recreate*)))

(def tpu-unpersist (name)
  (let name (sym name)
    (pull [caris _ name] tpu-persistent*)))

(def tpu-ensure (name (o preemptible (tpu-preemptible? name)) (o version (tpu-get-version name)))
  (let name (sym name)
    (tpu-unensure name)
    (push (list name (if preemptible 'preemptible) version) tpu-recreate*)))

(def tpu-persist (name (o preemptible (tpu-preemptible? name)))
  (let name (sym name)
    (tpu-unpersist name)
    (push (list name (if preemptible 'preemptible)) tpu-persistent*)))

(def tpu-persists? (name)
  (mem [caris _ name] tpu-persistent*))

(def tpu-persist-link (user name (o preemptible 'lookup) (o whence "/tpus"))
  (if (candelete-tpu user name)
      (let persist (tpu-persists? name)
        (w/rlink (let preemptible (if (is preemptible 'lookup)
                                      (tpu-preemptible? name)
                                    preemptible)
                   (pull [caris _ name] tpu-persistent*)
                   (pull [caris _ name] tpu-recreate*)
                   (unless persist (tpu-persist name preemptible))
                   whence)
          (pr (if persist "un-persist" "persist"))))))

(def tpu-recreate (name (o preemptible (tpu-preemptible? name)) (o version (tpu-get-version name)))
  (tostring:tpu-delete name)
  (tpu-ensure name preemptible version))

(defbg tpu-keepalive 30 ()
  (tpu-try-recreations))

(defope newtpu req
  (let user (get-user req)
    (tpu-create-page user (arg req "msg"))))

(def call-w/tostring (f)
  (if (readenv "DEV")
      (tostring (f))
      (on-err details (fn () (tostring (f))))))

(mac w/tostring body
  `(call-w/tostring (fn () ,@body)))

(def ranges (ls)
  (when ls
    (withs (start (car ls)
            prev (- start 1))
      (+ (each x ls
           (unless (is x (+ prev 1))
             (out start prev)
             (= start x))
           (= prev x))
         (list (list start prev))))))

(def pretty-ranges (ls)
  (aand (map (fn ((a b))
               (if (is a b) a (cat a " to " b)))
             (ranges ls))
        (apply cat (intersperse ", " it))))

(def tpu-create-page (user (o msg nil))
  (minipage "Create TPU"
    (pagemessage msg)
    (uform user req
           (withs (tpu-index (gcloud-tpu-index (arg req "index"))
                   tpu-zone (gcloud-zone-name (arg req "zone"))
                   tpu-cidr (gcloud-tpu-cidr (arg req "accelerator"))
                   tpu-accelerator (arg req "accelerator")
                   tpu-preemptible (readvar 'yesno (arg req "preemptible"))
                   tpu-version (arg req "version"))
             (if (aand tpu-index tpu-zone tpu-cidr tpu-accelerator)
                 (prn "<pre><code>"
                      (w/tostring:tpu-create tpu-index tpu-accelerator tpu-zone tpu-preemptible 'async tpu-version)
                      "</code></pre>")
                 (tpu-create-page user (tostring:prs "Invalid TPU info: "
                                               'tpu-index tpu-index (arg req "index")
                                               'tpu-zone tpu-zone (arg req "zone")
                                               'tpu-cidr tpu-cidr
                                               'tpu-accelerator tpu-accelerator (arg req "accelerator")
                                               'tpu-preemptible tpu-preemptible (arg req "preemptible")
                                               'tpu-version tpu-version (arg req "version")
                                               ))))
      (tab:showvars
        `((int index ,(car:tpu-valid-indexes 'europe-west4-a 'pod) t t)
          ((choice ,@(map car tpu-zones*)) zone europe-west4-a t t)
          ((choice v3-32 v3-128 v3-256 v3-512 v3-1024 v3-2048
                   v2-32 v2-128 v2-256 v2-512 v2-1024 v2-2048
                   v2-8 v3-8)
           accelerator v3-32 t t)
          (yesno preemptible t t t)
          (string version "1.15" t t)))
      (br)
      (submit "create"))
    (prn "Available TPU pod indexes:")
    (sptab
      (each (name id) tpu-zones*
        (whenlet xs (tpu-valid-indexes name 'pod)
          (row (td name) (td:prn:pretty-ranges xs)))))
    (br)
    (prn "Available v2-8 or v3-8 indexes:")
    (sptab
      (each (name id) tpu-zones*
        (whenlet xs (tpu-valid-indexes name nil)
          (row (td name) (td:prn:pretty-ranges xs)))))))

(defope reimagetpu req
  (let user (get-user req)
    (tpu-reimage-page user (arg req "msg"))))

(def tpu-choices ((o ps (sorted-tpus)))
  `(choice ,@(map !id (keep tpu-pod? ps))))

(def tpu-reimage-page (user (o msg nil))
  (minipage "Reimage TPU"
    (pagemessage msg)
    (uform user req
           (withs (tpu-id (sym:arg req "id")
                   tpu-version (arg req "version"))
             (if (aand tpu-id tpu-version)
                 (prn "<pre><code>"
                      (w/tostring:tpu-reimage tpu-id tpu-version)
                      "</code></pre>")
                 (tpu-reimage-page user (tostring:prn "Invalid TPU info: "
                                                      'tpu-id tpu-id (arg req "id")
                                                      'tpu-version tpu-version (arg req "version")))))
           (tab:showvars
        `((,(tpu-choices) id nil t t)
          (string1 version "1.15" t t)))
      (br2)
      (submit "reimage"))))

(def tpu-editor (user)
  (editor user))

(def canrecreate-tpu (user id)
  (and (tpu-editor user) (goodname:string id)))

(def candelete-tpu (user id)
  (and (tpu-editor user) (goodname:string id)))

(def tpu-delete-link (id user (o label "delete") (o whence "/tpus"))
  (when (candelete-tpu user id)
    (linkf label (req)
      (when-umatch user req
        (if (candelete-tpu user id)
            (del-tpu-confirm-page user id whence)
            (prn "You can't delete that."))))))

(def del-tpu-confirm-page (user id whence)
  (minipage "Confirm"
    (tab
      (tr (td)
          (td (urform user req
                     (do (when (is (arg req "b") "Yes")
                           (tpu-unpersist id)
                           (tostring:tpu-delete id))
                         whence)
                 (prn (+ "Do you want " id " to be deleted?"))
                 (br2)
                 (but "Yes" "b") (sp) (but "No" "b")))))))

(def tpu-recreate-link (id user (o whence "/tpus"))
  (when (canrecreate-tpu user id)
    (linkf "recreate" (req)
      (when-umatch user req
        (if (canrecreate-tpu user id)
            (recreate-tpu-confirm-page user id whence)
            (prn "You can't recreate that."))))))

(def recreate-tpu-confirm-page (user id whence)
  (with (preemptible (tpu-preemptible? id)
         version (tpu-get-version id))
    (minipage "Confirm"
      (tab
        (tr (td)
            (td (urform user req
                       (do (when (is (arg req "b") "Yes")
                             (tpu-recreate id preemptible version))
                           whence)
                   (prn "Do you want " id " to be recreated?" (if preemptible "" " (non-preemptible)"))
                   (br2)
                   (but "Yes" "b") (sp) (but "No" "b"))))))))

(def recreate-pods-confirm-page (user whence)
  (minipage "Confirm"
    (tab
      (tr (td)
          (td (urform user req
                     (do (when (is (arg req "b") "Yes")
                           (tpu-recreate-all-pods))
                         whence)
                 (prn "WARNING: This will delete and recreate ALL tpu pods. Proceed?")
                 (br2)
                 (but "Yes" "b") (sp) (but "No" "b")))))))

(def tpu-recreate-pods-link (user (o whence "/tpus"))
  (when (tpu-editor user)
    (underline:linkf "Recreate all TPU pods" (req)
      (when-umatch user req
        (if (tpu-editor user)
            (recreate-pods-confirm-page user whence)
            (prn "You can't recreate all TPU pods."))))))

(def tpu-recreate-all-pods ((o ps (tpus)))
  (map tpu-recreate (map !id (keep tpu-pod? (map cadr (tablist ps))))))

(edop tpulog ()
  (prn "<pre><code>")
  (prn:filechars:logfile-name 'shell)
  (prn "</pre></code>"))

(def tpu-parse-curl (s)
  (withs (parts (tokens (multisubst '((" -H " "|")
                                      (" --compressed" ""))
                                    (trim s))
                        #\|)
          (cmd . headers) (keep [or (posmatch "x-origin:" _)
                                    (posmatch "authorization:" _)
                                    (posmatch "cookie:" _)
                                    (headmatch "curl " _)]
                                parts)
          (url query) (tokens (strip (lstrip cmd "curl ") "'") #\?)
          args (parseargs query)
          )
    (dbg)
    args))

(def tpu-moment ((o secs (seconds)))
   (multisubst '((".000Z" "Z"))
               (moment-secs secs)))

(def tpu-parse-curl (s name zone (o metric 'mem))
  (withs ((cmd . headers) (aand (trim s)
                                (lstrip it "curl ")
                                (rstrip it " --compressed")
                                (splitsby '(" -H ") it)
                                (rem empty it)
                                (map [strip _ "'"] it))
          headers (keep [in (car _) "x-origin" "authorization" "cookie"]
                        (map [splitby ": " _] headers))
          (url query) (tokens cmd #\?)
          args (listtab:parseargs query)
          filters (map [list (car _) (read:cadr _)]
                       (map [splitby "=" _]
                            (splitby " AND " (args "filter"))))
          (o reducer "REDUCE_SUM") (args "aggregation.crossSeriesReducer")
          (o aligner "ALIGN_MEAN") (args "aggregation.perSeriesAligner")
          (o period "+300s")       (args "aggregation.alignmentPeriod")
          (o start-time (- (seconds) (* 24.0 3600.0))) (only.isotime (args "interval.startTime"))
          (o end-time (- (seconds) 0.0))               (only.isotime (args "interval.endTime"))
          token (trim:filechars "bearer.txt")
          )
    (= filters `(("metric.type" ,(case metric
                                   'cpu "tpu.googleapis.com/cpu/utilization"
                                   'mem "tpu.googleapis.com/memory/usage"
                                   'net "tpu.googleapis.com/network/sent_bytes_count"
                                   (err "metric should be 'mem 'cpu or 'net")))
                 ("resource.labels.zone" ,(string zone))
                 ("resource.labels.node_id" ,(string name))))
    (wipe (args "clientRequestId"))
    (wipe (args "key"))
    (withs (result (apply string url "?" (intersperse '& (map (fn ((k v)) (string (urlencode k) '= (urlencode v))) (tablist args))))
            h (fromstring (shell 'curl '-s result '-H (string "Authorization: Bearer " token))
                (read-json)))
      h)))

(def escape (x)
  (tostring:write-json x))

(def tpu-request (metric name (o zone (gcloud-parse-zone-name name)) (o start-time (tpu-moment (- (seconds) (* 24.0 3600.0)))) (o end-time (tpu-moment (seconds))))
  (if (is metric 'net)
      (zip (tpu-request 'sent name zone start-time end-time)
           (tpu-request 'recv name zone start-time end-time))
    (withs (filters `(("metric.type" ,(case metric
                                        'cpu "tpu.googleapis.com/cpu/utilization"
                                        'mem "tpu.googleapis.com/memory/usage"
                                        'sent "tpu.googleapis.com/network/sent_bytes_count"
                                        'recv "tpu.googleapis.com/network/received_bytes_count"
                                        (err "metric should be 'mem 'cpu or 'net")))
                      ("resource.labels.zone" ,(string zone))
                      ("resource.labels.node_id" ,(string name)))
            args `(("aggregation.crossSeriesReducer" "REDUCE_MEAN")
                   ("aggregation.perSeriesAligner" "ALIGN_MEAN")
                   ("aggregation.alignmentPeriod" "+300s")
                   ("interval.startTime" ,start-time)
                   ("interval.endTime" ,end-time)
                   ("filter" ,(apply string (intersperse " AND " (map (fn ((k v)) (string k '= (escape v))) filters)))))
            token (oauth-get-token)
            endpoint "https://monitoring.clients6.google.com/v3/projects/gpt-2-15b-poetry/timeSeries"
            url (apply string endpoint "?" (intersperse '& (map (fn ((k v)) (string (urlencode k) '= (urlencode v))) (tablist args))))
            h (fromstring (shell 'curl '-s url
                                 '-H (string "Authorization: Bearer " token)
                                 '-H "Accept: application/json")
                (read-json)))
        (map [list (isotime _!interval!startTime) _!value!doubleValue]
             h!timeSeries!0!points))))

(def tpu-request-pretty (metric . args)
  (if (is metric 'net)
      (each ((ts sent) (ts recv)) (zip (apply tpu-request 'sent args)
                                       (apply tpu-request 'recv args))
        (out (text-age:minutes-since ts)
             (string (num (/ sent 1024.0 1024.0 1024.0) 2 2 t) " GB out, "
                     (num (/ recv 1024.0 1024.0 1024.0) 2 2 t) " GB in")))
    (each (ts v) (apply tpu-request metric args)
      (out (text-age:minutes-since ts)
           (case metric
             'cpu (string (num v 2 2 t) "%")
             'mem (string (num (/ v 1024.0 1024.0 1024.0) 2 2 t) " GB")
             (sent recv) (string (num (/ v 1024.0 1024.0 1024.0) 2 2 t) " GB " (if (is metric 'recv) 'in 'out))
             (err "Unexpected metric"))))))

(def tpu-request-hours args
  (map (fn ((ts x)) (list (hours-since ts) x)) (apply tpu-request args)))

(def tpu-request-hours-since (hours . args)
  (awhen (apply tpu-request-hours args)
    (and (some [>= (car _) hours] it)
         (keep [< (car _) hours] it))))

(def tpu-bandwidth-since (name (o suffix 'gb) (o hours 18))
  (list (aand (tpu-request-hours-since hours 'sent name) (apply + (map cadr it)))
        (aand (tpu-request-hours-since hours 'recv name) (apply + (map cadr it)))))

(def tpu-unused (name (o cutoff (* 10.0 1024 1024 1024))) ; 10GB
  (let (sent recv) (tpu-bandwidth-since name)
    (and sent recv (< (+ sent recv) cutoff))))

(def tpus-unused ((o ps (sorted-tpus)))
  (keep tpu-unused ps))

(def tpus-unused-noisy ((o ps (sorted-tpus)))
  (noisy-each 1 p ps
    (when (tpu-unused p!id)
      (prn "unused TPU: " p!id)
      (out p))))

(def bytes-to-human (x (o suffix 'gb))
  (aand (sym suffix)
        (if (is it 'kb) (/ x 1024.0)
            (is it 'mb) (/ x (* 1024.0 1024.0))
            (is it 'gb) (/ x (* 1024.0 1024.0 1024.0))
            (is it 'tb) (/ x (* 1024.0 1024.0 1024.0 1024.0))
            (is it 'pb) (/ x (* 1024.0 1024.0 1024.0 1024.0))
            (err "Invalid suffix" suffix))))

; (each ps (sorted-tpus) (prn ps!id " " (map [only.bytes-to-human _ 'gb] (tpu-bandwidth-since ps!id))))
