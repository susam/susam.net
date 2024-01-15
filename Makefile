NAME = susam
FQDN = $(NAME).net
MAIL = $(NAME).pal@gmail.com

help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'Targets to run on live server:'
	@echo '  setup             Install Debian packages and Quicklisp for website.'
	@echo '  https             Reinstall live website and serve with Nginx via HTTPS.'
	@echo '  http              Reinstall live website and serve with Nginx via HTTP.'
	@echo '  rm                Uninstall live website.'
	@echo '  recu              Pull and deploy content updates.'
	@echo '  check-form-live   Check forms work correctly on live website.'
	@echo '  follow-log        Follow access logs on live server.'
	@echo '  follow-post       Follow form post logs on live server.'
	@echo '  post-log          Filter form post logs to find all successful posts.'
	@echo '  top-get-log       Filter access logs to find most popular paths.'
	@echo '  count-log         Count hits in each access log file.'
	@echo
	@echo 'Low-level targets:'
	@echo '  live              Generate live directory for website.'
	@echo '  site              Generate website.'
	@echo '  dist              Generate website for distribution as zip/tarball.'
	@echo
	@echo 'Development targets:'
	@echo '  opt               Create directories at /opt for testing.'
	@echo '  comment           Create comment file for filename in FILE macro.'
	@echo '  loop              Run a loop to create website directory repeatedly.'
	@echo '  test              Test Common Lisp program.'
	@echo '  run-site          Serve website locally via a local HTTP server.'
	@echo '  run-form          Run form application locally.'
	@echo '  checks            Run checks suitable to be run before push.'
	@echo '  check-links       Check broken links in a locally running website.'
	@echo '  check-paths       Check live website paths and redirects.'
	@echo '  check-form-rate   Check rate-limiting of form.'
	@echo '  check-form-dev    Check forms work correctly in local dev environment.'
	@echo '  pub               Publish updated website on live server and mirror.'
	@echo '  web               Publish website on primary server only.'
	@echo '  cu                Publish content updates on primary server only.'
	@echo '  mirror            Publish website on mirror only.'
	@echo '  pull-backup       Pull a backup of cache from live server.'
	@echo
	@echo 'Default target:'
	@echo '  help       Show this help message.'


# Targets for Live Server
# -----------------------

setup:
	apt-get update
	apt-get -y install nginx certbot sbcl
	rm -rf /opt/quicklisp.lisp /opt/quicklisp
	curl https://beta.quicklisp.org/quicklisp.lisp -o /opt/quicklisp.lisp
	sbcl --load /opt/quicklisp.lisp \
	     --eval '(quicklisp-quickstart:install :path "/opt/quicklisp/")' \
	     --quit
	chown -R www-data:www-data /opt/quicklisp

https: http
	@echo Setting up HTTPS website ...
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

follow-log:
	sudo tail -F /var/log/nginx/access.log | grep -vE "\.(css|js|ico|png|woff|xml)|tt-rss|bot|netnewswire|FeedFetcher-Google|AppEngine-Google"

follow-post:
	tail -F /opt/log/form/form.log | grep POST

post-log:
	tail -F /opt/log/form/form.log | grep written

top-get-log:
	sudo zgrep ' 200 ' /var/log/nginx/access.log* | grep -o 'GET /[^ ]*' | sort | uniq -c | sort -nr | nl | less

count-log:
	sudo zgrep -c . /var/log/nginx/access.log* | awk -F : '{printf "%10s  %s\n", $$2, $$1}'


# Low-Level Targets
# -----------------

live: site
	@echo Setting up live directory ...
	mv _live _gone || :
	mv _site _live
	rm -rf _gone
	@echo Done; echo

site: mathjax
	@echo Generating website ...
	sbcl --load site.lisp --quit
	@echo Done; echo

dist: mathjax
	@echo Generating distributable website ...
	sbcl --eval '(defvar *params* (list (cons "index" "index.html")))' --load site.lisp --quit
	@echo Done; echo

mathjax:
	mkdir -p _cache/
	if ! [ -e _cache/mathjax/ ]; then \
	    echo Cloning MathJax ...; \
	    git -C _cache/ clone -b 3.2.0 --depth 1 https://github.com/mathjax/mathjax.git; \
	    rm -rf _cache/mathjax/.git; \
	else \
	    echo MathJax is already cached.; \
	fi


# Development Targets
# -------------------

opt:
	sudo mkdir -p /opt/data/form/ /opt/log/form/
	sudo cp raw/opt.lisp /opt/data/form/opt.lisp
	sudo chown -R "$$USER" /opt/data/form/ /opt/log/form/

comment:
	[ -n "$(FILE)" ]
	touch $$(echo "$(FILE)" | sed 's/\/posts\//\/comments\//')

loop:
	while true; do make dist; sleep 5; done

run-site:
	python3 -m http.server -b localhost -d _site/

run-form: site
	sbcl --load form.lisp

# Checks
test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

checks: check-copyright check-rendering check-sentence-space check-math-punct check-comment-files check-copyright check-nginx

check-copyright:
	grep -q "&copy; 2005-$$(date +"%Y") Susam Pal" static/cv.html
	@echo Done; echo

check-rendering:
	grep -r --include '*.html' --include '*.xml' '{{' _site | head
	@echo Done; echo

check-math-punct:
	! grep -IErn '\\)[^- :t"<)}]' content  | grep -vE 'mastering-emacs'
	@echo Done; echo

check-sentence-space:
	! grep -IErn '[^0-9][.?!]  ' content | grep -vE 'Corporation|20 00'
	@echo Done; echo

check-comment-files:
	# Ensure every comment file has a post file.
	ls -1 content/blog/comments/ | while read -r f; do \
	    echo Checking post file for "$$f"; \
		if ! [ -e "content/blog/posts/$$f" ]; then \
			echo No post file for comment file: "$$f"; exit 1; fi; done
	ls -1 content/cafe/comments/ | while read -r f; do \
	    echo Checking post file for "$$f"; \
		if ! [ -e "content/cafe/posts/$$f" ]; then \
			echo No post file for comment file: "$$f"; exit 1; fi; done
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

check-links:
	@echo "NOTE: Ensure 'make run-site' is running before running this target"; echo
	-wget -r -l 0 --spider -nd -nv http://localhost:8000/ -o run.log
	grep -B1 broken run.log
	@echo Done; echo

check-paths:
	# Blog legacy URL redirects
	curl -sSI http://susam.in/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100/'
	curl -sSI https://susam.in/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100/'
	curl -sSI http://susam.net/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100/'
	curl -sSI http://susam.net/blog/fd-100.html | grep 'Location: https://susam.net/blog/fd-100.html'
	# Main Xlog
	curl -sSI https://susam.net/blog/infosys-tcs-or-wipro.html | grep '200 OK'
	curl -sSI https://susam.net/blog/comments/infosys-tcs-or-wipro.html | grep '200 OK'
	curl -sSI https://susam.net/blog/re-infosys-tcs-or-wipro.html | grep '200 OK'
	curl -sSI https://susam.net/blog/comments/re-infosys-tcs-or-wipro.html | grep '200 OK'
	# Maze
	curl -sSI https://susam.net/maze/c-quine.html | grep '200 OK'
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

pub: web mirror

web:
	git push -f origin main
	ssh -t susam.net "cd /opt/susam.net/ && sudo git checkout main && sudo git reset --hard HEAD~5 && sudo git pull && sudo make live && sudo systemctl restart nginx form && sudo systemctl --no-pager status nginx form"

cu:
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make recu"

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

mirror: site
	@echo
	@echo 'Creating mirror ...'
	rm -rf $(TMP_GIT)
	mv _site $(TMP_GIT)
	git rev-parse --short HEAD > $(TMP_REV)
	echo Mirror of Susam\'s Blog >> $(README)
	echo ====================== >> $(README)
	echo >> $(README)
	echo Automatically generated from [susam/susam.net][GIT_SRC] >> $(README)
	echo "([$$($(CAT_REV))][GIT_REV])". >> $(README)
	echo >> $(README)
	echo Visit $(WEB_URL) to view the the mirror. >> $(README)
	echo >> $(README)
	echo [GIT_SRC]: $(GIT_SRC) >> $(README)
	echo [WEB_URL]: $(WEB_URL) >> $(README)
	echo [GIT_REV]: $(GIT_SRC)/commit/$$($(CAT_REV)) >> $(README)
	@echo
	@echo 'Pushing mirror ...'
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
