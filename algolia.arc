(= algolia-app-id* "DMX77CEFW8"
   algolia-key-ro* "0e7e64491733b975977b6d1229a3620b"
   algolia-key* "../algolia.json"
   algolia-write* (no (readenv "DEV")))

(defcache algolia-key 600
  (when (file-exists algolia-key*)
    (w/infile i algolia-key*
      (aand (read-json i) (if (isa it 'string) it it!key)))))

(def algolia-set (index value (o auth algolia-write*))
  (withs (app algolia-app-id*
          key (and auth (algolia-key))
          idx (clean-url index)
          id (aand value!objectID (string it) (trim it)))
    (when key
      (if (blank id)
          (err "algolia-set: value must have objectID attribute")
        (thread
          (fromstring (tostring:write-json value)
            (shell 'curl '-fsSL '-X 'PUT '-d (string #\@ "-")
                   '-H (+ "X-Algolia-API-Key: " key)
                   '-H (+ "X-Algolia-Application-Id: " app)
                   (+ "https://" app ".algolia.net/1/indexes/" idx "/" id))))))))

