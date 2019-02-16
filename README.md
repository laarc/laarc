# To run laarc

```
git clone http://github.com/laarc/laarc
cd laarc
make
mkdir arc
echo "myname" > arc/admins
NOISY=1 DEV=1 rlwrap bin/arc # rlwrap is optional
```

at the arc prompt:

```
((load "news.arc"))
```

go to http://localhost:8080

click on login, and create an account called myname

you should now be logged in as an admin


# To customize News

change the variables at the top of news.arc



# To improve performance

```
(= static-max-age* 7200)    ; browsers can cache static files for 7200 sec

(declare 'direct-calls t)   ; you promise not to redefine fns as tables

(declare 'explicit-flush t) ; you take responsibility for flushing output
                            ; (all existing news code already does)
```

# Production installation

```
# some prereqs
sudo pkg install racket shuf rlwrap

# prereqs for plotting traffic graphs
sudo pkg install moreutils # sponge
sudo pkg install gnuplot

# for email
sudo pkg install python # then install pip
sudo pip install --upgrade google-api-python-client oauth2client

# PULL=10 means `git pull` every 10 seconds
PULL=10 FLUSH=nil bin/laarc
```
