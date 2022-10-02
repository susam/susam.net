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
	@echo '  check-form-live   Check forms work correctly on live website.'
	@echo
	@echo 'Low-level targets:'
	@echo '  live              Generate live directory for website.'
	@echo '  site              Generate website.'
	@echo '  dist              Generate website for distribution as zip/tarball.'
	@echo
	@echo 'Development targets:'
	@echo '  loop              Run a loop to create website directory repeatedly.'
	@echo '  test              Test Common Lisp program.'
	@echo '  run-site          Serve website locally via a local HTTP server.'
	@echo '  run-form          Run form application locally.'
	@echo '  check-files       Check that content and config files are well-formed.'
	@echo '  check-links       Check broken links in a locally running website.'
	@echo '  check-paths       Check live website paths and redirects.'
	@echo '  check-form-rate   Check rate-limiting of form.'
	@echo '  check-form-local  Check forms work correctly on local website.'
	@echo '  pub               Publish updated website on live server and mirror.'
	@echo '  force-pub         Publish website on live server after reset, and mirror.'
	@echo '  mirror            Publish website on mirror only.'
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
	systemctl reload nginx
	echo 127.0.0.1 '$(NAME)' >> /etc/hosts
	@echo Done; echo

form:
	@echo Setting up form ...
	mkdir -p /opt/cache
	chown -R www-data:www-data /opt/cache
	systemctl enable "/opt/susam.net/etc/form.service"
	systemctl daemon-reload
	systemctl start form
	@echo Done; echo

rm: checkroot
	@echo Removing website ...
	rm -f '/etc/nginx/sites-enabled/$(FQDN)'
	rm -f '/var/www/$(FQDN)'
	systemctl reload nginx
	sed -i '/$(NAME)/d' /etc/hosts
	#
	@echo Removing form ...
	-systemctl stop form
	-systemctl disable form
	systemctl daemon-reload
	#
	# Following crontab entries left intact:
	crontab -l | grep -v "^#" || :
	@echo Done; echo

checkroot:
	@echo Checking if current user is root ...
	[ $$(id -u) = 0 ]
	@echo Done; echo


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

loop:
	while true; do make dist; sleep 5; done

run-site:
	cd _site && python3 -m http.server

run-form: site
	sbcl --load form.lisp

# Checks
test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

check-files:
	# Ensure every comment file has a post file.
	ls -1 content/blog/comments/ | while read -r f; do \
	    echo Checking post file for "$$f"; \
		if ! [ -e "content/blog/posts/$$f" ]; then \
			echo No post file for comment file: "$$f"; exit 1; fi; done
	ls -1 content/cafe/comments/ | while read -r f; do \
	    echo Checking post file for "$$f"; \
		if ! [ -e "content/cafe/posts/$$f" ]; then \
			echo No post file for comment file: "$$f"; exit 1; fi; done
	# Ensure punctuation goes inside inline-math.
	! grep -IErn '\\)[^ ]' content | grep -vE '\\)(s|th|-|</h[1-6]>|</em>|</li>|\)|:)'
	! grep -IErn '(th|-|</h[1-6]>|:) \\)' content
	# Ensure current year is present in footer.
	grep -q "&copy; 2005-$$(date +"%Y") Susam Pal" static/cv.html
	# Ensure http.susam.net and https.susam.net are consistent.
	sed -n '/types/,/limit/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '/types/,/limit/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	sed -n '/location/,/^}/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '/location/,/^}/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	@echo Done; echo

check-links:
	-wget -r -l 0 --spider -nd -nv http://localhost:8000/ -o run.log
	grep -B1 broken run.log

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

check-form-local:
	make check-form URL=http://localhost:4242/ SLEEP=0

check-form:
	rm -f /opt/cache/comment_* /opt/cache/subscribe_* /opt/cache/unsubscribe_*
	# Comment checks
	# --------------
	@echo
	@echo 'Checking successful comment submission with URL ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d url=example.net -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	ls -l /opt/cache/comment_*
	grep 'foo' /opt/cache/comment_*
	grep 'alice' /opt/cache/comment_*
	grep 'example\.net' /opt/cache/comment_*
	grep 'body' /opt/cache/comment_*
	rm -f /opt/cache/comment_*
	@echo
	@echo 'Checking successful comment submission with empty URL ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d url= -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	ls -l /opt/cache/comment_*
	grep 'foo' /opt/cache/comment_*
	grep 'alice' /opt/cache/comment_*
	grep -v 'url:' /opt/cache/comment_*
	grep 'body' /opt/cache/comment_*
	rm -f /opt/cache/comment_*
	@echo
	@echo 'Checking successful comment submission with missing URL ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	ls -l /opt/cache/comment_*
	grep 'foo' /opt/cache/comment_*
	grep 'alice' /opt/cache/comment_*
	grep -v 'url:' /opt/cache/comment_*
	grep 'body' /opt/cache/comment_*
	rm -f /opt/cache/comment_*
	@echo
	@echo 'Checking comment failure due to empty post ...'
	curl -sS '$(URL)form/comment/?post=' -d slug=foo -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/comment_*
	@echo
	@echo 'Checking comment failure due to missing post ...'
	curl -sS '$(URL)form/comment/' -d slug=foo -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/comment_*
	@echo
	@echo 'Checking comment failure due to empty name ...'
	rm -f /opt/cache/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name= -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/comment_*
	@echo
	@echo 'Checking comment failure due to missing name ...'
	rm -f /opt/cache/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d email= -d comment=body | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/comment_*
	@echo
	@echo 'Checking comment failure due to empty comment ...'
	rm -f /opt/cache/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email= -d comment= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/comment_*
	@echo
	@echo 'Checking comment failure due to missing comment ...'
	rm -f /opt/cache/comment_*
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/comment_*
	@echo
	@echo 'Checking comment ignore due to post-slug mismatch ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=bar -d name=alice -d email= -d comment=body | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/comment_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking comment ignore due to invalid key ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d email=foo@example.com -d comment=body | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/comment_*
	sleep $(SLEEP)
	@echo
	@echo 'Check comment ignore with missing xkey ...'
	curl -sS '$(URL)form/comment/?post=foo' -d slug=foo -d name=alice -d url=example.net -d comment=body | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/comment_*
	sleep $(SLEEP)
	# Subscribe Checks
	# ----------------
	@echo
	@echo 'Checking successful subscribe ...'
	curl -sS '$(URL)form/subscribe/' -d email=foo@example.com -d name= | grep '<li>' | grep 'Successfully'
	grep 'foo' /opt/cache/subscribe_*
	rm -f /opt/cache/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe failure due to empty email ...'
	curl -sS '$(URL)form/subscribe/' -d email= -d name= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe failure due to missing email ...'
	curl -sS '$(URL)form/subscribe/' -d name= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe ignore due to invalid key ...'
	curl -sS '$(URL)form/subscribe/' -d email=foo@example.com -d name=foo | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/subscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe ignore due to missing key ...'
	curl -sS '$(URL)form/subscribe/' -d email=foo@example.com | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/subscribe_*
	sleep $(SLEEP)
	# Unsubscribe Checks
	# ----------------
	@echo
	@echo 'Checking successful unsubscribe ...'
	curl -sS '$(URL)form/unsubscribe/' -d email=foo@example.com -d name= | grep '<li>' | grep 'Successfully'
	grep 'foo' /opt/cache/unsubscribe_*
	rm -f /opt/cache/unsubscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking unsubscribe failure due to empty email ...'
	curl -sS '$(URL)form/unsubscribe/' -d email= -d name= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/unsubscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking unsubscribe failure due to missing email ...'
	curl -sS '$(URL)form/unsubscribe/' -d name= | grep '<li>' | grep 'Invalid'
	! ls -l /opt/cache/unsubscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking unsubscribe ignore due to invalid key ...'
	curl -sS '$(URL)form/unsubscribe/' -d email=foo@example.com -d name=foo | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/unsubscribe_*
	sleep $(SLEEP)
	@echo
	@echo 'Checking subscribe ignore due to missing key ...'
	curl -sS '$(URL)form/unsubscribe/' -d email=foo@example.com | grep '<li>' | grep 'Successfully'
	! ls -l /opt/cache/subscribe_*
	sleep $(SLEEP)
	# Done
	# ----
	ls -l /opt/cache/
	@echo
	@echo Done

pub: web mirror

force-pub: force-web mirror

web:
	git push
	ssh -t susam.net "cd /opt/susam.net/ && sudo git pull && sudo make live && sudo systemctl restart nginx form && sudo systemctl --no-pager status nginx form"

force-web:
	git push -f
	ssh -t susam.net "cd /opt/susam.net/ && sudo git reset --hard HEAD~5 && sudo git pull && sudo make live && sudo systemctl restart nginx form && sudo systemctl --no-pager status nginx form"

TMP_REV = /tmp/rev.txt
CAT_REV = cat $(TMP_REV)
GIT_SRC = https://github.com/susam/susam.net
GIT_DST = https://github.com/susam/susam.github.io
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
	cd $(TMP_GIT) && git remote add origin "$(GIT_DST).git"
	cd $(TMP_GIT) && git log
	cd $(TMP_GIT) && git push -f origin main
	@echo
	@dcho Done
