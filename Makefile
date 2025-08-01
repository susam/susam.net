NAME = susam
FQDN = $(NAME).net
MAIL = $(NAME).pal@gmail.com

help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'Targets to run on live server:'
	@echo '  setup             Install Debian packages and CL systems for website.'
	@echo '  https             Reinstall live website and serve with Nginx via HTTPS.'
	@echo '  http              Reinstall live website and serve with Nginx via HTTP.'
	@echo '  rm                Uninstall live website.'
	@echo '  recu              Pull and deploy content updates.'
	@echo '  check-form-live   Check forms work correctly on live website.'
	@echo
	@echo 'Monitoring targets to run on live server:'
	@echo '  follow-log        Follow access logs on live server.'
	@echo '  follow-post       Follow form post logs on live server.'
	@echo '  follow-visit      Follow access logs to watch visitor activity on live server.'
	@echo '  top-visit-get     Filter access logs to find most visited paths.'
	@echo '  top-visit-ref     Filter access logs to find the top referrers for visitors.'
	@echo '  top-get           Filter access logs to find most hit paths.'
	@echo '  top-ref           Filter access logs to find the top referrers.'
	@echo '  post-log          Filter form post logs to find all successful posts.'
	@echo '  count-log         Count hits in each access log file.'
	@echo '  grepl re=PATTERN  Filter all access logs by regular expression pattern.'
	@echo '  grepd re=PATTERN  Filter current access log by regular expression pattern.'
	@echo '  grepv re=PATTERN  Filter visit logs by regular expression pattern.'
	@echo '  lsform            List form data submitted.'
	@echo '  rdform            Read form data submitted.'
	@echo
	@echo 'Low-level targets:'
	@echo '  live              Generate live directory for website.'
	@echo '  site              Generate website.'
	@echo '  dist              Generate website for distribution as zip/tarball.'
	@echo '  serve             Serve _site/ directory via a local HTTP server.'
	@echo '  deep              Put website in a directory deeply nested within _site/.'
	@echo
	@echo 'Development targets:'
	@echo '  clean             Clean temporary files.'
	@echo '  clean-all         Clean temporary files more aggressively.'
	@echo '  opt               Create directories at /opt for testing.'
	@echo '  comment           Create comment file for filename in FILE macro.'
	@echo '  loop              Run a loop to create website directory repeatedly.'
	@echo '  ref               Create reference directories for the current website.'
	@echo '  diff              Create new websites and compare against reference directories.'
	@echo '  test              Test Common Lisp program.'
	@echo '  run-site          Serve website locally via a local HTTP server.'
	@echo '  run-dist          Serve distribution locally via a local HTTP server.'
	@echo '  run-site-deep     Serve website from a subdirectory path.'
	@echo '  run-dist-deep     Serve distribution from a subdirectory path.'
	@echo '  run-form          Run form application locally.'
	@echo '  checks            Run checks on source suitable to be run before push.'
	@echo '  tidy              Run HTML Tidy on the generated website.'
	@echo '  check-links       Check broken links in a locally running website.'
	@echo '  check-paths       Check live website paths and redirects.'
	@echo '  check-form-rate   Check rate-limiting of form.'
	@echo '  check-form-dev    Check forms work correctly in local dev environment.'
	@echo '  pub               Publish updated website on live server and mirror.'
	@echo '  main              Publish main branch on primary server only.'
	@echo '  cu                Publish content updates on primary server only.'
	@echo '  cus               Publish content updates on primary server and restart web server.'
	@echo '  gh                Publish website on GitHub mirror only.'
	@echo '  pull-backup       Pull a backup of cache from live server.'
	@echo
	@echo 'Default target:'
	@echo '  help       Show this help message.'


# Targets for Live Server
# -----------------------

setup: debs cldeps

debs:
	apt-get update
	apt-get -y install nginx certbot sbcl

cldeps:
	rm -rf /opt/cl/
	mkdir -p /opt/cl/form/ /opt/cl/roll/
	set -x; while read -r url; do curl -sSL "$$url" | \
	tar -C /opt/cl/form/ -xz; done < meta/cldeps/form.txt
	set -x; while read -r url; do curl -sSL "$$url" | \
	tar -C /opt/cl/roll/ -xz; done < meta/cldeps/roll.txt

quicklisp:
	rm -rf /opt/quicklisp.lisp /opt/quicklisp
	curl https://beta.quicklisp.org/quicklisp.lisp -o /opt/quicklisp.lisp
	sbcl --load /opt/quicklisp.lisp \
	     --eval '(quicklisp-quickstart:install :path "/opt/quicklisp/")' \
	     --quit
	chown -R www-data:www-data /opt/quicklisp

https: http
	@echo Setting up HTTPS website ...
	mkdir -p /opt/log/cron/
	certbot certonly -n --agree-tos -m '$(MAIL)' --webroot \
	                 -w '/var/www/$(FQDN)' -d '$(FQDN),www.$(FQDN),susam.in'
	(crontab -l | sed '/::::/d'; cat etc/crontab) | crontab
	ln -snf "$$PWD/etc/nginx/https.$(FQDN)" '/etc/nginx/sites-enabled/$(FQDN)'
	systemctl reload nginx
	@echo Done; echo

http: rm live form
	@echo Setting up HTTP website ...
	ln -snf "$$PWD/_live" '/var/www/$(FQDN)'
	ln -snf "$$PWD/etc/nginx/http.$(FQDN)" '/etc/nginx/sites-enabled/$(FQDN)'
	ln -snf "$$PWD/etc/logrotate" /etc/logrotate.d/form
	systemctl reload nginx
	echo 127.0.0.1 '$(NAME)' >> /etc/hosts
	@echo Done; echo

form:
	@echo Setting up form ...
	mkdir -p /opt/cache/ /opt/data/form/ /opt/log/form/
	chown -R www-data:www-data /opt/cache/ /opt/data/form/ /opt/log/form/
	systemctl enable "/opt/susam.net/etc/form.service"
	systemctl daemon-reload
	systemctl start form
	@echo Done; echo

rm: checkroot
	@echo Removing website ...
	rm -f /etc/logrotate/form
	rm -f '/etc/nginx/sites-enabled/$(FQDN)'
	rm -f '/var/www/$(FQDN)'
	systemctl reload nginx
	sed -i '/$(NAME)/d' /etc/hosts
	@echo
	@echo Removing form ...
	-systemctl stop form
	-systemctl disable form
	systemctl daemon-reload
	@echo
	@echo Following crontab entries left intact:
	crontab -l | grep -v "^#" || :
	@echo Done; echo

recu: checkroot
	git checkout cu
	git reset --hard HEAD~5
	git pull
	make live

restart:
	systemctl restart nginx form
	systemctl --no-pager status nginx form

checkroot:
	@echo Checking if current user is root ...
	[ $$(id -u) = 0 ]
	@echo Done; echo

backup:
	tar -caf "/opt/cache/form-$$(date "+%Y-%m-%d_%H-%M-%S").tgz" -C /opt/data/ form/
	ls -1 /opt/cache/form-*.tgz | sort -r | tail -n +100 | xargs rm -vf
	ls -lh /opt/cache/
	df -h /

BOT_RE = tt-rss|bot|netnewswire|AppEngine-Google|HeadlessChrome|FeedFetcher-Google|PetalBot

follow-log:
	sudo tail -F /var/log/nginx/access.log | grep -vE "\.(css|js|ico|png|ttf|woff|xml)|$(BOT_RE)"

follow-post:
	tail -F /opt/log/form/form.log | grep POST

follow-visit:
	if ! [ -f /tmp/lines.txt ]; then echo 0 > /tmp/lines.txt; fi
	tally=0; \
	while true; do \
	  make -s cache-visits FILE=/var/log/nginx/access.log; \
	  prev_lines=$$(cat /tmp/lines.txt); \
	  curr_lines=$$(wc -l < /tmp/visits.txt); \
	  echo "$$curr_lines" > /tmp/lines.txt; \
	  if [ "$$curr_lines" -lt "$$prev_lines" ]; then \
	    prev_lines=0; \
	    echo "Log file truncated; following from the beginning"; \
	  fi; \
	  if [ "$$curr_lines" -ne "$$prev_lines" ]; then \
	    tally=0; \
	    echo; \
	    tail -n +"$$(( $$prev_lines + 1 ))" /tmp/visits.txt; \
	    echo; \
	    echo "[$$(date +"%Y-%m-%d %H:%M:%S")] new visits: $$curr_lines - $$prev_lines = $$(( $$curr_lines - $$prev_lines ))"; \
	  else \
	    tally=$$(( ($$tally + 1) % 5 )); \
	    printf '.'; \
	    if [ "$$tally" = 0 ]; then printf ' '; fi; \
	  fi; \
	  sleep 60; \
	done

top-visit-get: lazy-cache-visits
	grep -o 'GET /[^ ]*' /tmp/visits.txt | sort | uniq -c | sort -nr | nl | less

top-visit-ref: lazy-cache-visits
	awk '{print $$11}' /tmp/visits.txt | grep -vE '^"https?://susam\.net/?' | sort | uniq -c | sort -nr | nl | less

top-get:
	sudo zgrep ' 200 ' /var/log/nginx/access.log* | grep -o 'GET /[^ ]*' | sort | uniq -c | sort -nr | nl | less

top-ref:
	sudo zgrep ' 200 ' /var/log/nginx/access.log* | awk '{print $$11}' | grep -vE '^"https?://susam\.net/?' | sort | uniq -c | sort -nr | nl | less

post-log:
	tail -F /opt/log/form/form.log | grep Written

count-log:
	sudo zgrep -c . /var/log/nginx/access.log* | awk -F : '{printf "%10s  %s\n", $$2, $$1}'

grepl:
	[ -n "$$re" ]
	sudo zgrep -h $(re) /var/log/nginx/access.log*

grepd:
	[ -n "$$re" ]
	sudo grep -h $(re) /var/log/nginx/access.log

grepv: lazy-cache-visits
	[ -n "$$re" ]
	sudo grep -h $(re) /tmp/visits.txt

filter-visitors:
	grep -E 'GET /(favicon\.png|favicon\.ico|feed\.xsl|css) .* "https://susam\.net/' | awk '{print $$1}' | sort -u

filter-visits:
	sudo zgrep -f /tmp/visitors.txt | grep -vE '\.(css|js|ico|png|jpg|gif|ttf|woff|xml|xsl)'

lazy-cache-visits:
	if [ -f /tmp/visits.txt ]; then echo Visits already cached; \
	else make cache-visits FILE=/var/log/nginx/access.log*; fi

cache-visits:
	sudo zgrep -h ' 200 ' $(FILE) | grep -vE "$(BOT_RE)" | make filter-visitors > /tmp/visitors.txt
	sudo zgrep -h ' 200 ' $(FILE) | grep -vE "$(BOT_RE)" | make filter-visits > /tmp/visits.txt

clean-visits:
	rm -f /tmp/visitors.txt /tmp/visits.txt

lsform:
	ls -l /opt/data/form/*.txt | less -F

rdform:
	tail -n +1 /opt/data/form/*.txt | less -F


# Low-Level Targets
# -----------------

live: site roll
	@echo Setting up live directory ...
	mv _live _gone || :
	mv _site _live
	rm -rf _gone
	@echo Done; echo

site: katex
	@echo Generating website ...
	sbcl --noinform --load site.lisp --quit
	@echo Done; echo

dist: katex
	@echo Generating distributable website ...
	sbcl --noinform \
	     --eval '(setf *break-on-signals* t)' \
	     --eval '(defvar *params* (list (cons "index" "index.html")))' \
	     --load site.lisp \
	     --quit
	@echo Done; echo

serve:
	python3 -m http.server -b localhost -d _site/

deep:
	rm -rf _prep/
	mkdir -p _prep/foo/bar/baz/qux/
	mv _site/ _prep/foo/bar/baz/qux/
	mv _prep/ _site/

katex:
	mkdir -p _cache/
	if ! [ -e _cache/katex/ ]; then \
	    echo Downloading KaTeX ...; \
	    curl -sSLo _cache/katex.tar.gz https://github.com/KaTeX/KaTeX/releases/download/v0.16.22/katex.tar.gz; \
	    tar -xvf _cache/katex.tar.gz -C _cache/; \
	else \
	    echo KaTeX is already cached.; \
	fi

mathjax:
	mkdir -p _cache/
	if ! [ -e _cache/mathjax/ ]; then \
	    echo Cloning MathJax ...; \
	    git -C _cache/ clone -b 3.2.0 --depth 1 https://github.com/mathjax/mathjax.git; \
	    rm -rf _cache/mathjax/.git; \
	else \
	    echo MathJax is already cached.; \
	fi

getroll:
	@echo Fetching feeds for roll ...
	rm -rf _cache/roll/
	mkdir -p _cache/roll/
	ua="curl/$$(curl -V | head -n1 | cut -d' ' -f2) (Susam's Blogroll; https://susam.net/roll.html)"; \
	while read -r url; do \
	  domain=$$(echo "$$url" | sed -E 's/.*:\/\/(www\.|feeds\.)?([^/]*)\/.*/\2/'); \
	  echo "Fetching $$url ($$domain) ..."; \
	  curl -sSL -A "$$ua" -m 30 -o _cache/roll/"$$domain".xml "$$url"; \
	done < content/roll.txt
	date > _cache/roll/ok
	@echo Done; echo

devgetroll:
	mv content/roll.txt content/roll.txt.bkp
	echo 'https://susam.net/feed.xml' > content/roll.txt
	make getroll
	mv content/roll.txt.bkp content/roll.txt

roll:
	@echo Generating roll ...
	if ! [ -e _cache/roll/ok ]; then make getroll; fi
	CL_SOURCE_REGISTRY="/opt/cl/roll//" \
	ASDF_OUTPUT_TRANSLATIONS="/opt/cl/:/opt/cache/cl/" \
	sbcl --noinform --load roll.lisp --quit | tee _cache/roll/roll.log
	if [ -e _site/ ]; then cp -v _cache/roll/roll.* _site/; fi
	if [ -e _live/ ]; then cp -v _cache/roll/roll.* _live/; fi
	@echo Done; echo

reroll: getroll roll

devroll:
	@echo Generating development environment roll ...
	if ! [ -e _cache/roll/ok ]; then make getroll; fi
	if ! [ -e _site/ ]; then make dist; fi
	CL_SOURCE_REGISTRY="/opt/cl/roll//" \
	ASDF_OUTPUT_TRANSLATIONS="/opt/cl/:~/cache/cl/" \
	sbcl --noinform \
	     --eval '(defvar *params* (list (cons "index" "index.html")))' \
	     --load roll.lisp \
	     --quit | tee _cache/roll/roll.log
	cp -v _cache/roll/roll.* _site/
	@echo Done; echo


# Development Targets
# -------------------

clean:
	rm -f dist.diff site.diff run.log
	rm -rf _new/

clear: clean
	rm -rf _site/ _ref/

opt:
	sudo mkdir -p /opt/data/form/ /opt/log/form/
	sudo cp raw/opt.lisp /opt/data/form/opt.lisp
	sudo chown -R "$$USER" /opt/data/form/ /opt/log/form/

loop:
	while true; do make dist; sleep 5; done

ref:
	rm -rf _ref/
	mkdir _ref/
	make dist
	mv _site/ _ref/dist/
	make site
	mv _site/ _ref/site/

ddiff:
	make dist
	rm -rf _new/dist/
	mkdir -p _new/
	mv _site/ _new/dist/
	diff -ru _ref/dist/ _new/dist/ > dist.diff || vim dist.diff
	@echo Done; echo

sdiff:
	make site
	rm -rf _new/site/
	mkdir -p _new/
	mv _site/ _new/site/
	diff -ru _ref/site/ _new/site/ > site.diff || vim site.diff
	@echo Done; echo

diff: ddiff sdiff

run-dist: dist serve

run-site: site serve

run-dist-deep: dist deep serve

run-site-deep: site deep serve

serve-form: site
	CL_SOURCE_REGISTRY="/opt/cl/form//" \
	ASDF_OUTPUT_TRANSLATIONS="/opt/cl/:~/cache/cl/" \
	sbcl --load form.lisp

# List metadata.
ls-unlist:
	grep -r 'unlist:' content

ls-tag:
	grep -r 'tag:' content

# Checks
test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

checks: cvsplit check-bre check-comment-files check-copyright check-entities check-tex-content check-newlines check-nginx check-quotes check-rendering check-sentence-space check-tex-site tidy

cvsplit:
	: > content/tree/foss.html
	sed -n 's/CV/FOSS Contributions/; 1,/GitHub/p' content/tree/cv.html >> content/tree/foss.html
	sed -n '/Mastodon/,/<main>/p' content/tree/cv.html >> content/tree/foss.html
	sed -n 's/Open/Free and Open/; /Open Source/,/<\/table>/p' content/tree/cv.html >> content/tree/foss.html
	sed -n '/<\/main>/,$$p' content/tree/cv.html >> content/tree/foss.html
	: > content/tree/talks.html
	sed -n 's/CV/Talks/; 1,/<main>/p' content/tree/cv.html >> content/tree/talks.html
	sed -n '/Talks/,/<\/table>/p' content/tree/cv.html >> content/tree/talks.html
	sed -n '/<\/main>/,$$p' content/tree/cv.html >> content/tree/talks.html

check-bre:
	grep -IErn --exclude invaders.html --exclude cfrs.html --exclude fxyt.html --exclude quickqwerty.html --exclude "*tex-live-packages-in-debian.html" --exclude-dir content/comments --exclude-dir content/tree/code/web 'iz[a-z]' content layout | \
	  grep -vE '\<AUTHorize\>|\<chatgpt\>|\<C\+\+ Optimizing Compiler\>|\<Customize Jenkins\>|\<Dehumanized\>|\<initializer \(6\.7\.8\)|\<journaling and visualization\>|mastering-emacs/ch03.post.html:.*\<[Cc]ustomiz[ae]|\<netizens\>|\<package-initialize\>|\<public synchronized\>|\<Registrant Organization\>|\<ResizableDoubleArray\>|\<[Rr]esized?\>|\<resizing\>|rizon|\<[Ss]ize(d|s|of)?\>|\<sizing\>|wizard|:topic'; [ $$? = 1 ]
	grep -IErn --exclude-dir content/comments 'yze' content layout | \
	  grep -vE '\<StandardAnalyzer\>'; [ $$? = 1 ]
	grep -IErn --exclude cfrs.html --exclude fxyt.html --exclude invaders.html --exclude myrgb.html --exclude --exclude "*tex-live-packages-in-debian.html" --exclude-dir content/comments 'color|center' content layout | \
	  grep -vE '\.center\>|-color\>|\<color:|\<colorforth\>|\<grid center\>|mastering-emacs/ch03.post.html:.*(COLOR|color)|--nocolor\>|\<text-align: center\>|\<textcenter\>'; [ $$? = 1 ]
	sed -n '/Susam Pal/,/date:/p' content/comments/*.html | \
	  grep -E 'iz[a-z]|yze|center|color' | grep -vE '\<color:|\<size\>'; [ $$? = 1 ]
	@echo Done; echo

check-comment-files:
	# Ensure every comment file has a post file.
	ls -1 content/comments/ | while read -r f; do \
	    echo Checking post file for "$$f"; \
		if ! [ -e "content/blog/$$f" ] && ! [ -e "content/maze/$$f" ]; then \
			echo No post file for comment file: "$$f"; exit 1; fi; done
	@echo Done; echo

check-copyright:
	grep -q "&copy; 2005-$$(date +"%Y") Susam Pal" content/tree/cv.html content/tree/foss.html
	@echo Done; echo

check-entities:
	grep -IErn --include='*.html' --exclude=cfrs.html --exclude=fxyt.html --exclude=invaders.html --exclude=myrgb.html --exclude=quickqwerty.html --exclude-dir=content/tree/code/web ' [<>&] ' content | grep -vE ':hover > a'; [ $$? = 1 ]
	@echo Done; echo

check-tex-content:
	# If the non-whitespace character before "\)" is not an
	# alphanumeric character or not an allowed character (e.g., ")",
	# "}", etc.), then it is an error.
	#
	# In particular, we do not want to allow ".  \)", ", \)", etc.
	# But we do want to allow "9 \)", "f(x) \)", "k'", etc.
	grep -IErn '[^])}+\<*0-9A-Za-z] +\\\)' content | grep -vE "' +\\\\)" | grep -vE '\\\( &lt; \\\)|<code>.*\\\).*</code>'; [ $$? = 1 ]
	@echo Done; echo

check-tex-site: dist
	grep --include="*.html" -IErn "\\\)[^- :t'\"<)}]" _site | grep -vE '<code>.*\\\).*</code>'; [ $$? = 1 ]
	@echo Done; echo

check-newlines:
	grep -IErn '(<br>[^&]|\\\[.)' content | grep -vE '\\\[</code>'; [ $$? = 1 ]
	@echo Done; echo

check-nginx:
	# Ensure http.susam.net and https.susam.net are consistent.
	sed -n '1,/limit_req_status/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '1,/limit_req_status/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	sed -n '/server_name [^w]/,/^}/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '/server_name [^w]/,/^}/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	@echo Done; echo

check-quotes:
	grep -r '’' content | grep -vE '‘[a-z-]*\.el’|‘info’'; [ $$? = 1 ]
	@echo Done; echo

check-rendering:
	grep -r --include '*.html' --include '*.xml' --include '*.css' '{{' _site; [ $$? = 1 ]
	@echo Done; echo

check-sentence-space:
	grep --exclude invaders.html --exclude cfrs.html --exclude quickqwerty.html -IErn "[^0-9A-Z.][.?!][])\"']? [A-Z]" content | grep -vE "No soup for you|Mr\. T\.|function!|RET|SPC"; [ $$? = 1 ]
	@#                      ^-----^
	grep -IERn '\. \\' content | grep -vE '<code>|\\left\.'; [ $$? = 1 ]
	grep -IERn '\.  [a-z]' content | grep -vE '\.  freenode'; [ $$? = 1 ]
	grep -IErn 'Mr\.|Ms\.|Mrs\.|Dr\.|vs\.' content | grep -vE 'Mr\. T\.'; [ $$? = 1 ]
	@echo Done; echo

tidy: dist
	find _site -name "*.html" | while read -r page; do \
	  echo Tidying "$$page"; \
	  tidy -q -e --warn-proprietary-attributes no "$$page" || exit 1; \
	done
	@echo Done; echo

check-links:
	@echo "NOTE: Ensure 'make run-site-deep' is running before running this target"; echo
	-wget -r -l 0 --spider -nd -nv http://localhost:8000/ -o run.log
	grep -B1 broken run.log
	@echo Done; echo

check-paths:
	# Blog legacy URL redirects
	curl -sSI https://susam.in/about.html | grep 'Location: https://susam.net/about.html'
	curl -sSI https://susam.net/blog/feed.xml | grep 'Location: https://susam.net/feed.xml'
	# HTTPS redirects
	curl -sSI http://susam.net/fd-100.html | grep 'Location: https://susam.net/fd-100.html'
	curl -sSI http://susam.net/blog/fd-100.html | grep 'Location: https://susam.net/blog/fd-100.html'
	# Main Blog
	curl -sSI https://susam.net/fd-100.html | grep '200 OK'
	curl -sSI https://susam.net/comments/fd-100.html | grep '200 OK'
	@echo Done; echo

check-js:
	npm install --no-save standard eslint-plugin-html
	find . -name "*.html" | grep -v _site/ | xargs npx standard --plugin html
	@echo Done; echo

list-no-meta:
	grep -r --include "*.page.html" -L date: content; echo
	grep -r -l date: --exclude-dir comments content | xargs grep -L tag:; echo
	grep -r --include "????-??-??-*.html" --exclude-dir comments -L tag:; echo
	@echo Done; echo

check-form-rate:
	n=0; while [ $$n -le 60 ]; do printf "$$n $$(date +%H:%M:%S) "; \
	curl -sSI https://susam.net/form/ | head -n 1; n=$$(( $$n + 1 )); \
	sleep 1; done
	@echo Done; echo

check-form-live: checkroot
	make check-form URL=https://susam.net/ SLEEP=15

check-form-dev:
	make check-form URL=http://localhost:4242/ SLEEP=0

check-form:
	rm -f /opt/data/form/comment_* /opt/data/form/subscribe_* /opt/data/form/unsubscribe_*
	# Comment checks
	# --------------
	@echo
	@echo 'Checking successful comment submission with URL ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d url=example.net -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	ls -l /opt/data/form/comment_*
	grep 'foo' /opt/data/form/comment_*
	grep 'alice' /opt/data/form/comment_*
	grep 'example\.net' /opt/data/form/comment_*
	grep 'body' /opt/data/form/comment_*
	rm -f /opt/data/form/comment_*
	@echo
	@echo 'Checking successful comment submission with empty URL ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d url= -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	ls -l /opt/data/form/comment_*
	grep 'foo' /opt/data/form/comment_*
	grep 'alice' /opt/data/form/comment_*
	grep -v 'url:' /opt/data/form/comment_*
	grep 'body' /opt/data/form/comment_*
	rm -f /opt/data/form/comment_*
	@echo
	@echo 'Checking successful comment submission with missing URL ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	ls -l /opt/data/form/comment_*
	grep 'foo' /opt/data/form/comment_*
	grep 'alice' /opt/data/form/comment_*
	grep -v 'url:' /opt/data/form/comment_*
	grep 'body' /opt/data/form/comment_*
	rm -f /opt/data/form/comment_*
	@echo
	@echo 'Checking comment failure due to empty post ...'
	curl -sS '$(URL)form/comment/?post=' -d slug=foo -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/comment_*
	@echo
	@echo 'Checking comment failure due to missing post ...'
	curl -sS '$(URL)form/comment/' -d slug=foo -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/comment_*
	@echo
	@echo 'Checking comment failure due to empty name ...'
	rm -f /opt/data/form/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name= -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/comment_*
	@echo
	@echo 'Checking comment failure due to missing name ...'
	rm -f /opt/data/form/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/comment_*
	@echo
	@echo 'Checking comment failure due to empty comment ...'
	rm -f /opt/data/form/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email= -d comment= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/comment_*
	@echo
	@echo 'Checking comment failure due to missing comment ...'
	rm -f /opt/data/form/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/comment_*
	@echo
	@echo 'Checking comment ignore due to post-slug mismatch ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=bar -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	! ls -l /opt/data/form/comment_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking comment ignore due to invalid key ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email=foo@example.com -d comment=body | grep '<li>' | grep 'Successfully'
	! ls -l /opt/data/form/comment_*
	sleep $(SLEEP)
	@echo
	@echo 'Check comment ignore with missing xkey ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d url=example.net -d comment=body | grep '<li>' | grep 'Successfully'
	! ls -l /opt/data/form/comment_*
	sleep $(SLEEP)
	# Subscribe Checks
	# ----------------
	@echo
	@echo 'Checking successful subscribe ...'
	curl -sS '$(URL)form/subscribe/' -d email=foo@example.com -d name= | grep '<li>' | grep 'Successfully'
	grep 'foo' /opt/data/form/subscribe_*
	rm -f /opt/data/form/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe failure due to empty email ...'
	curl -sS '$(URL)form/subscribe/' -d email= -d name= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe failure due to missing email ...'
	curl -sS '$(URL)form/subscribe/' -d name= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/data/form/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe ignore due to invalid key ...'
	curl -sS '$(URL)form/subscribe/' -d email=foo@example.com -d name=foo | grep '<li>' | grep 'Successfully'
	! ls -l /opt/data/form/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe ignore due to missing key ...'
	curl -sS '$(URL)form/subscribe/' -d email=foo@example.com | grep '<li>' | grep 'Successfully'
	! ls -l /opt/data/form/subscribe_*
	sleep $(SLEEP)
	# Unsubscribe Checks
	# ----------------
	@echo
	@echo 'Checking successful unsubscribe ...'
	curl -sS '$(URL)form/unsubscribe/' -d email=foo@example.com -d name= | grep '<li>' | grep 'Successfully'
	# Done
	# ----
	ls -l /opt/data/form/
	@echo
	@echo Done

post-comment1:
	curl -sS 'localhost:4242/form/comment/?p=foo' -d slug=foo -d name=alice -d email= -d vkeyb=ansi -d comment=body | grep '<li>'

post-comment2:
	curl -sS 'localhost:4242/form/comment/?p=foo' -d slug=foo -d name=alice -d email= -d action= -d comment=body | grep '<li>'

post-subscriber1:
	curl -sS 'localhost:4242/form/subscribe/' -d email=foo@example.com -d name= -d ylang=en-us | grep '<li>'

post-subscriber2:
	curl -sS 'localhost:4242/form/subscribe/' -d email=foo@example.com -d name= -d stack=cadr | grep '<li>'

pub: cu gh

cu:
	git push origin main
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make recu"

cus:
	git push origin main
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make recu restart"

pull-backup:
	mkdir -p ~/bkp/
	ssh susam.net "tar -czf - -C /opt/data/ form/" > ~/bkp/form-$$(date "+%Y-%m-%d_%H-%M-%S").tgz
	ls -lh ~/bkp/

TMP_REV = /tmp/rev.txt
CAT_REV = cat $(TMP_REV)
GIT_SRC = https://github.com/susam/susam.net
GIT_DST = git@github.com:susam/susam.github.io.git
WEB_URL = https://susam.github.io/
TMP_GIT = /tmp/tmpgit
README  = $(TMP_GIT)/README.md

gh: site
	@echo
	@echo 'Creating GitHub mirror ...'
	rm -rf $(TMP_GIT)
	mv _site $(TMP_GIT)
	git rev-parse --short HEAD > $(TMP_REV)
	echo "# Mirror of Susam\'s Website" >> $(README)
	echo >> $(README)
	echo "Automatically generated from [susam/susam.net][GIT_SRC]" >> $(README)
	echo "([$$($(CAT_REV))][GIT_REV])". >> $(README)
	echo >> $(README)
	echo "Visit $(WEB_URL) to view the the mirror." >> $(README)
	echo >> $(README)
	echo "[GIT_SRC]: $(GIT_SRC)" >> $(README)
	echo "[WEB_URL]: $(WEB_URL)" >> $(README)
	echo "[GIT_REV]: $(GIT_SRC)/commit/$$($(CAT_REV))" >> $(README)
	@echo
	@echo 'Pushing GitHub mirror ...'
	cd $(TMP_GIT) && git init
	cd $(TMP_GIT) && git config user.name "Susam Pal"
	cd $(TMP_GIT) && git config user.email susam@susam.net
	cd $(TMP_GIT) && git add .
	cd $(TMP_GIT) && git commit -m "Generated from $(GIT_SRC) - $$($(CAT_REV))"
	cd $(TMP_GIT) && git remote add origin "$(GIT_DST)"
	cd $(TMP_GIT) && git log
	cd $(TMP_GIT) && git push -f origin main
	@echo
	@echo Done
