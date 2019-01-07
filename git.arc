(def git-hash-id (hash)
  (cut hash 0 7))

(def git--read-branch (line)
  (if (headmatch "  " line)
      (git--read-branch (+ "- " line))
      (aand (readall line 3)
            (if (is (last it) '->) (readall line 4) it)
            (cons (is (car it) '*) (cdr it)))))

(def git-branches ()
  (map git--read-branch (lines:shell 'git 'branch '-rv '--all)))

(def git-branch-refs ((o ref))
  (aand (pair:tokens:shell 'git 'show-ref)
        (map (fn ((hash name))
               (list name (git-hash-id hash)))
             it)))

(def git-branch-current ((o name) (o commit) (o short?))
  (trim:shell 'git 'symbolic-ref (if short? '--short) name (or commit "HEAD")))

(def git-branch-commit ((o ref (git-branch-current)))
  (alref (git-branch-refs) ref))

(def git-log ((o commit "HEAD"))
  (shell 'git 'log '-1 commit))

(def git-current-branch-name ()
  (aand (find (fn ((current? . args)) current?) (git-branches))
        (cadr it)))

(def git-clean ((o silent))
  (when (or silent
            (yesno "Warning: about to delete untracked files/folders and uncommitted changes"))
    (shell 'git 'clean '-f '-d '-e ".*")))

; https://stackoverflow.com/questions/2358643/git-discard-all-changes-on-a-diverged-local-branch
(def git-reset-to-origin ((o silent)
                          (o branch (git-current-branch-name))
                          (o remote 'origin))
  (let name (string remote "/" branch)
    (when (or silent
              (yesno (string "Warning: about to reset to " name)))
      (shell 'git 'stash)
      (shell 'git 'reset '--hard name)
      (git-clean silent)
      (unless silent
        (git-print-status)))))

(def git-pull ()
  (aand (tostring:system "rm -f .git/index.lock && git pull >/dev/null && printf 1 || git merge --abort")
        (~headmatch "1" it)))

(def git-print-status ()
  (pr (shell 'git 'status))
  nil)


