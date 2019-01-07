To run News:

git clone http://github.com/shawwn/arc3.2

cd arc3.2

git checkout ln

make

mkdir arc

echo "myname" > arc/admins

racket -f as.scm

at the arc prompt:

(load "news.arc")

(nsv)

go to http://localhost:8080

click on login, and create an account called myname

you should now be logged in as an admin

manually give at least 10 karma to your initial set of users

don't worry about "user break" messages when restarting News


In production:

racket -f ln.scm


To customize News:

change the variables at the top of news.arc



To improve performance:

(= static-max-age* 7200)    ; browsers can cache static files for 7200 sec

(declare 'direct-calls t)   ; you promise not to redefine fns as tables

(declare 'explicit-flush t) ; you take responsibility for flushing output
                            ; (all existing news code already does)

Production installation:

sudo pkg install racket
sudo pkg install python
# install pip
sudo pip install --upgrade google-api-python-client oauth2client
sudo pkg install shuf # for /terry

