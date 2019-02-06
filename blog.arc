; Blog tool example.  20 Jan 08, rev 21 May 09.

; To run:
; arc> (load "blog.arc")
; arc> (bsv)
; go to http://localhost:8080/blog

(= postdir*   "arc/posts/"
   blogtitle* "A Blog")

(or= postid* 0 posts* (table))

(deftem post  id nil  title nil  text nil)

(def load-posts ()
  (each id (map int (dir postdir*))
    (= postid*      (max postid* id)
       (posts* id) (temload 'post (string postdir* id)))))

(def save-post (p) (save-table p (string postdir* p!id)))

(def post (id) (posts* (errsafe:int id)))

(mac blogpage body
  `(whitepage 
     (center
       (widtable 600 
         (tag b (link blogtitle* "blog"))
         (br 3)
         ,@body
         (br 3)
         (w/bars (link "archive")
                 (link "new post" "newpost"))))))

(defop viewpost req (blogop post-page req))

(def blogop (f req)
  (aif (post (arg req "id")) 
       (f (get-user req) it) 
       (blogpage (pr "No such post."))))

(def post-url (p) (string "/viewpost?id=" p!id))

(def post-page (user p) (blogpage (display-post user p)))

(def display-post (user p)
  (tag b (link p!title (post-url p)))
  (when user
    (sp)
    (link "[edit]" (string "/editpost?id=" p!id)))
  (br2)
  (pr p!text))

(defopl newpost req
  (whitepage
    (aform [let u (get-user _)
             (post-page u (addpost u (arg _ "t") (md-from-form:arg _ "b")))]
      (tab (row "title" (input "t" "" 60))
           (row "text"  (textarea "b" 10 80))
           (row ""      (submit))))))

(def addpost (user title text)
  (let p (inst 'post 'id (++ postid*) 'title title 'text text)
    (save-post p)
    (= (posts* p!id) p)))

(defopl editpost req (blogop edit-post-page req))

(def edit-post-page (user p)
  (whitepage
    (vars-form user
               `((string title ,p!title t t)
                 (mdtext text  ,p!text  t t))
               (fn (name val) (= (p name) val))
               (fn () (save-post p)
                      (post-page user p)))))

(defop archive req
  (blogpage
    (tag ul
      (each p (map post (rev (range 1 postid*)))
        (tag li (link p!title (post-url p)))))))

(defop blog req
  (let user (get-user req)
    (blogpage
      (for i 0 4
        (awhen (posts* (- postid* i)) 
          (display-post user it)
          (br 3))))))

(def bsv ()
  (ensure-dir postdir*)
  (load-posts)
  (asv))


