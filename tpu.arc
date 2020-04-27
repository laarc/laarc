(= tpu-cidr*
   (obj v3-2048 23
        v3-1024 24
        v3-512 25
        v3-256 26
        v3-128 27
        v3-64 28
        v3-32 29
        v2-2048 23
        v2-1024 24
        v2-512 25
        v2-256 26
        v2-128 27
        v2-64 28
        v2-32 29
        ))

(def gcloud-zone-id (zone)
  (let zone (sym zone)
    (if (in zone 'europe-west4-a 'euw4a)
        'euw4a
        (in zone 'us-central1-a 'usc1a)
        'usc1a)))

(def gcloud-zone-name (zone)
  (let zone (sym zone)
    (if (in zone 'europe-west4-a 'euw4a)
        'europe-west4-a
        (in zone 'us-central1-a 'usc1a)
        'us-central1-a)))

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
        (gcloud-zone-name it)))

(def gcloud-tpu-cidr (accelerator)
  (tpu-cidr* (sym accelerator)))

(def tpu-create (index accelerator zone (o preemptible t) (o async t) (o version))
  (withs (index (gcloud-tpu-index index)
          zone-id (gcloud-zone-id zone)
          zone-name (gcloud-zone-name zone)
          cidr (gcloud-tpu-cidr accelerator))
    (aif (no index) (err "Bad TPU index")
         (no zone-id) (err "Bad zone id")
         (no zone-name) (err "Bad zone name")
         (no cidr) (err "Bad accelerator")
      (let id (sym (+ "tpu-" accelerator "-" zone-id "-" index))
        (tpu-ensure id preemptible)
        (tpu-persist id preemptible)
        (shelllog 'gcloud 'compute 'tpus 'create id
               '--zone zone-name
               '--network (+ "tpu-" zone-id)
               '--range (+ "10." (+ 2 index) ".0.0/" cidr)
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

(def tpu-parse-zone-name (name)
  (let name (string name)
    (if (posmatch "-usc1a-" name) 'us-central1-a
        (posmatch "-euw4a-" name) 'europe-west4-a
        (in name "us-central1-a" "usc1a") 'us-central1-a
        (in name "europe-west4-a" "euw4a") 'europe-west4-a)))

(def tpu-parse-info (id (o preemptible t))
  (withs ((prefix type cores zoneid index) (tokens (string id) #\-)
          zone (gcloud-zone-name zoneid)
          zoneid (gcloud-zone-id zoneid)
          index (gcloud-tpu-index index)
          accelerator (sym:string type "-" cores)
          cidr (gcloud-tpu-cidr accelerator))
    (and zone index accelerator cidr
         (obj zone zone
              id (sym:string 'tpu- accelerator '- zoneid '- index)
              type accelerator
              preemptible preemptible))))

(def tpu-delete (name (o zone (tpu-parse-zone-name name)) (o async t))
  (tpu-unensure (sym name))
  (aand (goodname:string name)
        (gcloud-zone-name zone)
        (do (shelllog 'gcloud 'compute 'tpus 'delete name '--zone it '--quiet (and async '--async))
            t)))

(def tpu-describe (name (o zone (tpu-parse-zone-name name)))
  (aand (goodname:string name)
        (gcloud-zone-name zone)
        (shell 'tpu-describe name '--zone it)))

(def tpu-preemptible? (name)
  (aif (tpu-describe name)
       (yes (posmatch "preemptible: true" it))
    (err (string "Bad TPU name " name))))

(def tpu-get-version (name)
  (aand (lines:tpu-describe name)
        (find [headmatch "tensorflowVersion:" _] it)
        (last:splitby ": " it)
        (strip it "'")))

(def tpu-get-service-account (name)
  (aand (lines:tpu-describe name)
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
  (let name (sym name)
    (catch
      (each (i p) ps
        (if (is i name) (throw p))))))

(def get-tpu (name (o k) (o ps (tpus)))
  (aand (find-tpu name ps)
        (if k (it k) it)))

(def tpu-try-recreate (name preemptible version)
  (let status (get-tpu name 'status)
    (if (no status)
        (let p (tpu-parse-info name preemptible)
          (tpu-create (tpu-index p!id) p!type p!zone p!preemptible 'async version))
        (is status "READY") (tpu-unensure name)
        (is status "PREEMPTED")
        (do (srvlog 'shell status name preemptible)
            (tpu-recreate name preemptible (or version (tpu-get-version name)))))))

(or= tpu-recreate* () tpu-persistent* ())

(def tpu-try-recreations ()
  (each (name preemptible (o version)) (+ tpu-recreate* tpu-persistent*)
    (tpu-try-recreate name preemptible version))
  (+ tpu-recreate* tpu-persistent*))

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

(def tpu-create-page (user (o msg nil))
  (minipage "Create TPU"
    (pagemessage msg)
    (uform user req
           (withs (tpu-index (gcloud-tpu-index (arg req "index"))
                   tpu-zone (gcloud-zone-name (arg req "zone"))
                   tpu-cidr (gcloud-tpu-cidr (arg req "accelerator"))
                   tpu-accelerator (arg req "accelerator")
                   tpu-preemptible (readvar 'yesno (arg req "preemptible")))
             (if (aand tpu-index tpu-zone tpu-cidr tpu-accelerator)
                 (do (prn "<pre><code>")
                     (prn:tpu-create tpu-index tpu-accelerator tpu-zone tpu-preemptible)
                     (prn "</code></pre>"))
                 (tpu-create-page user (tostring:prn "Invalid TPU info: "
                                               'tpu-index tpu-index (arg req "index")
                                               'tpu-zone tpu-zone (arg req "zone")
                                               'tpu-cidr tpu-cidr
                                               'tpu-accelerator tpu-accelerator (arg req "accelerator")
                                               'tpu-preemptible tpu-preemptible (arg req "preemptible")
                                               ))))
      (tab:showvars
        `((int index nil t t)
          ((choice europe-west4-a us-central1-a) zone europe-west4-a t t)
          ((choice v3-32 v3-128 v3-256 v3-512 v3-1024 v3-2048
                   v2-32 v2-128 v2-256 v2-512 v2-1024 v2-2048)
           accelerator v3-32 t t)
          (yesno preemptible t t t)))
      (br2)
      (submit "create"))))

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
                 (do (prn "<pre><code>")
                     (prn:tpu-reimage tpu-id tpu-version)
                     (prn "</code></pre>"))
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
  (and (tpu-editor user) (goodname:string id) (tpu-pod? id)))

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

(def zip ls
  (if (no (car ls)) ()
      (cons (map car ls)
            (apply zip (map cdr ls)))))

(def tpu-request (metric name (o zone 'europe-west4-a) (o start-time (tpu-moment (- (seconds) (* 24.0 3600.0)))) (o end-time (tpu-moment (seconds))))
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
  
