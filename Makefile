NAME = susam
FQDN = $(NAME).net
MAIL = $(NAME).pal@gmail.com

help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'Publish targets:'
	@echo '  pub         Invoke all publish targets.'
	@echo '  web         Publish website on VPS.'
	@echo '  gh          Publish website on GitHub Pages.'
	@echo
	@echo 'High-level targets:'
	@echo '  setup       Install Debian packages.'
	@echo '  https       Reinstall live website and serve with Nginx via HTTPS.'
	@echo '  http        Reinstall live website and serve with Nginx via HTTP.'
	@echo '  update      Pull latest Git commits and update live website.'
	@echo '  rm          Uninstall live website.'
	@echo
	@echo 'Low-level targets:'
	@echo '  live        Generate live website.'
	@echo '  site        Generate local website.'
	@echo '  pull        Pull latest Git commits but do not update live website.'
	@echo
	@echo 'Test targets:'
	@echo '  test        Test Common Lisp program.'
	@echo '  checks      Check posts for known formatting issues.'
	@echo '  livechecks  Test live website for redirects and hidden posts.'
	@echo
	@echo 'Default target:'
	@echo '  help        Show this help message.'

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

http: rm live spapp
	@echo Setting up HTTP website ...
	ln -snf "$$PWD/_live" '/var/www/$(FQDN)'
	ln -snf "$$PWD/etc/nginx/http.$(FQDN)" '/etc/nginx/sites-enabled/$(FQDN)'
	systemctl reload nginx
	echo 127.0.0.1 '$(NAME)' >> /etc/hosts
	@echo Done; echo

spapp: FORCE
	@echo Setting up spapp ...
	mkdir -p /opt/cache
	chown www-data:www-data /opt/cache
	systemctl enable "/opt/blog/etc/spapp.service"
	systemctl daemon-reload
	systemctl start spapp
	@echo Done; echo

update: pull live

rm: checkroot
	@echo Removing website ...
	rm -f '/etc/nginx/sites-enabled/$(FQDN)'
	rm -f '/var/www/$(FQDN)'
	systemctl reload nginx
	sed -i '/$(NAME)/d' /etc/hosts
	#
	@echo Removing spapp ...
	-systemctl stop spapp
	-systemctl disable spapp
	systemctl daemon-reload
	#
	# Following crontab entries left intact:
	crontab -l | grep -v "^#" || :
	@echo Done; echo

live: site
	@echo Setting up live directory ...
	mv _live _gone || :
	mv _site _live
	ln -snf /opt/maze/_live _live/maze
	rm -rf _gone
	@echo Done; echo

site:
	@echo Generating website ...
	sbcl --script site.lisp
	make mathjax
	@echo Done; echo

dist:
	@echo Generating distributable website ...
	sbcl --eval '(defvar *params* (list (cons "index" "index.html") (cons "maze" "https://susam.net/maze/")))' --script site.lisp
	make mathjax
	@echo Done; echo

mathjax:
	mkdir -p _cache/
	if ! [ -e _cache/mathjax/ ]; then \
	    echo Cloning MathJax ...; \
	    git -C _cache/ clone -b 3.2.0 --depth 1 https://github.com/mathjax/mathjax.git; \
	else \
	    echo Using cached MathJax ...; \
	fi
	mkdir _site/js/
	cp -R _cache/mathjax/ _site/js/mathjax/

runapp:
	sbcl --load spapp.lisp

loop:
	while true; do make dist; sleep 5; done

pull:
	@echo Pulling new changes ...
	git fetch
	if [ "$$(git rev-parse HEAD)" = "$$(git rev-parse "@{u}")" ]; then \
		echo; echo No new changes; echo; false; \
	fi
	git merge
	@echo Done; echo

checkroot:
	@echo Checking if current user is root ...
	[ $$(id -u) = 0 ]
	@echo Done; echo

clean:
	find . -name "__pycache__" -exec rm -r {} +
	find . -name "*.pyc" -exec rm {} +

FORCE:


pub: push web gh

push:
	git push

web:
	ssh -t susam.net "cd /opt/blog/; sudo git pull; sudo make live; sudo systemctl restart spapp"


# GitHub Pages Mirror

TMP_REV = /tmp/rev.txt
CAT_REV = cat $(TMP_REV)
GIT_SRC = https://github.com/susam/susam.net
GIT_DST = https://github.com/susam/susam.github.io
WEB_URL = https://susam.github.io/
TMP_GIT = /tmp/tmpgit
README  = $(TMP_GIT)/README.md

gh: site
	#
	# Create mirror.
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
	#
	# Push mirror.
	cd $(TMP_GIT) && git init
	cd $(TMP_GIT) && git config user.name "Susam Pal"
	cd $(TMP_GIT) && git config user.email susam@susam.net
	cd $(TMP_GIT) && git add .
	cd $(TMP_GIT) && git commit -m "Generated from $(GIT_SRC) - $$($(CAT_REV))"
	cd $(TMP_GIT) && git remote add origin "$(GIT_DST).git"
	cd $(TMP_GIT) && git log
	cd $(TMP_GIT) && git push -f origin main

# Checks
test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

checks:
	# Ensure every comment file has a post file.
	ls -1 content/comments/ | while read -r f; do \
		if ! [ -e "content/blog/$$f" ] && ! [ -e "content/xlog/$$f" ]; then \
			echo No post file for comment file: "$$f"; exit 1; fi; done
	# Ensure punctuation goes inside inline-math.
	! grep -IErn '\\)[^ ]' content | grep -vE '\\)(s|th|-|</h[1-6]>|</em>|</li>\)|:)'
	! grep -IErn '(th|-|</h[1-6]>|:) \\)' content
	# Ensure current year is present in footer.
	grep -q "&copy; 2005-$$(date +"%Y") Susam Pal" static/cv.html
	# Ensure http.susam.net and https.susam.net are consistent.
	sed -n '/location/,/^}/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '/location/,/^}/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	@echo Done; echo

livechecks:
	# Blog legacy URL redirects
	curl -sSI http://susam.in/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100/'
	curl -sSI https://susam.in/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100/'
	curl -sSI http://susam.net/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100/'
	curl -sSI http://susam.net/blog/fd-100.html | grep 'Location: https://susam.net/blog/fd-100.html'
	curl -sSI https://susam.net/blog/fd-100/ | grep 'Location: https://susam.net/blog/fd-100.html'
	# Main Xlog
	curl -sSI https://susam.net/blog/infosys-tcs-or-wipro.html | grep '200 OK'
	curl -sSI https://susam.net/blog/comments/infosys-tcs-or-wipro.html | grep '200 OK'
	curl -sSI https://susam.net/blog/re-infosys-tcs-or-wipro.html | grep '200 OK'
	curl -sSI https://susam.net/blog/comments/re-infosys-tcs-or-wipro.html | grep '200 OK'
	# Main Blog redirects
	curl -sSI https://susam.net/blog/universal-palindrome-day/ | grep 'Location: https://susam.net/blog/global-palindrome-day.html'
	# Maze
	curl -sSI https://susam.net/maze/c-quine.html | grep '200 OK'
	# Maze to Blog redirects
	curl -sSI https://susam.net/maze/fd-100.html | grep 'Location: https://susam.net/blog/fd-100.html'
	# Maze Xlog
	curl -sSI https://susam.net/maze/paradox.html | grep '200 OK'
	curl -sSI https://susam.net/maze/comments/paradox.html | grep '200 OK'

appchecks: checkroot
	curl https://susam.net/app/comment/?post=foo -d slug=foo -d name=alice -d email= -d comment=body
	curl https://susam.net/app/subscribe/ -d email=foo-subscribe@example.com
	curl https://susam.net/app/unsubscribe/ -d email=foo-unsubscribe@example.com
	ls -l /opt/cache
	cat /opt/cache/comment_foo_$$(date +"%Y-%m-%d")_*.txt
	grep -h foo /opt/cache/*subscribe_$$(date +"%Y-%m-%d")_*.txt
	rm /opt/cache/comment_foo_$$(date +"%Y-%m-%d")_*.txt
	grep -l foo /opt/cache/*subscribe_$$(date +"%Y-%m-%d")_*.txt | xargs rm
	ls -l /opt/cache/
