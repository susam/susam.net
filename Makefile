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
	@echo '  cure              Publish content updates on primary server and restart web server.'
	@echo '  gh                Publish website on GitHub mirror only.'
	@echo '  cb                Publish website on GitHub mirror only.'
	@echo '  pull-backup       Pull a backup of cache from live server.'
	@echo
	@echo 'Default target:'
	@echo '  help              Show this help message.'


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
	    tally=$$(( $$tally + 1 )); \
	    printf '.'; \
	    if [ $$(( $$tally % 5 )) = 0 ]; then printf " $$tally "; fi; \
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

checks: \
  cvsplit \
  check-abbrev-spacing \
  check-bre-and \
  check-bre-or \
  check-bre-respectively \
  check-bre-spell-iz \
  check-bre-spell-yze \
  check-bre-spell-color \
  check-bre-spell-center \
  check-bre-spell-license \
  check-bre-thatis \
  check-comment-files \
  check-copyright \
  check-entities \
  check-general-thatis \
  check-tex-end \
  check-tex-ltgt \
  check-tex-site \
  check-newline \
  check-nginx \
  check-quote \
  check-rendering \
  check-sentence-spacing \
  tidy

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

cat-my-text:
	find content \( -name '*.html' -o -name '*.txt' \) \
	  ! -path 'content/guestbook/guestbook.html' \
	  ! -path '*/comments/*' \
	  ! -path 'content/licence/mit.html' \
	  -exec cat {} + > /tmp/cat
	sed -n '/name: Susam/,/date:/p' content/comments/*.html >> /tmp/cat

cat-all-text:
	find content -type f \
	  ! -name '*.gif' \
	  ! -name '*.ico' \
	  ! -name '*.jpg' \
	  ! -name '*.ly' \
	  ! -name '*.midi' \
	  ! -name '*.mp3' \
	  ! -name '*.mp4' \
	  ! -name '*.ogg' \
	  ! -name '*.pdf' \
	  ! -name '*.png' \
	  -exec cat {} + > /tmp/cat

# Check that mid-sentence abbrevations are followed by a single space.
check-abbrev-spacing: cat-all-text
	sed ' \
	  s/, and Hello World, from the future\.  freenode is/x/g; \
	  s/IRC\.  freenode is FOSS\.  freenode is freedom\./x/g; \
	  s/I have a confusion\.  the calculation that was giving/x/g; \
	' /tmp/cat > /tmp/tr
	grep -En '\. {2,}[a-z]' /tmp/tr | tee /tmp/err || true
	! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Check that there is no serial comma before 'and'.
check-bre-and: cat-my-text
	tr -s ' \n' ' ' < /tmp/cat | \
	sed -e 's/<[^>]*>//g' -e '\
	  s/, And Jill came tumbling after/x)/g; \
	  s/, and .,.\.) For example, in a statement like/x/g; \
	  s/, and 11/x/g; \
	  s/, and 9[., ]/x/g; \
	  s/, and Hello World, from the future/x/g; \
	  s/, and I.ll do my best to/x/g; \
	  s/, and \/n\/ or \/ŋ\//x/g; \
	  s/, and \\( \\tau \\)/x/g; \
	  s/, and \\( \\theta \\)/x/g; \
	  s/, and a great number of contorted trees/x/g; \
	  s/, and after a while we began to take their complaints/x/g; \
	  s/, and arithmetic expansion/x/g; \
	  s/, and at the scale of the UID/x/g; \
	  s/, and consider what I wanted to/x/g; \
	  s/, and effectively subverts/x/g; \
	  s/, and even deeper, to the well-being of the world/x/g; \
	  s/, and finally/x/g; \
	  s/, and in the days before ubiquitous computing/x/g; \
	  s/, and keeping fun in the house/x/g; \
	  s/, and language that each implementation documents/x/g; \
	  s/, and log\.brigg\./x/g; \
	  s/, and odd, outlandish, swampy trees/x/g; \
	  s/, and off-by-one errors\. The punchline/x/g; \
	  s/, and second, as a homophone/x/g; \
	  s/, and suggested that never knowing the exact answer/x/g; \
	  s/, and symbolic forms often replaces the conceptual/x/g; \
	  s/, and the absurdity of including off-by-one errors/x/g; \
	  s/, and the man.s response that he has quit drinking/x/g; \
	  s/, and the third orders a quarter of a beer/x/g; \
	  s/, and their inclusion in a list of "hard things"/x/g; \
	  s/, and therefore cannot create an off-by-one error/x/g; \
	  s/, and they are not self-sustaining --- they depend/x/g; \
	  s/, and we should be open to all interpretations/x/g; \
	  s/, and we will have a much more careful review process/x/g; \
	' > /tmp/tr
	grep -Ei ', and([^a-z]|$$)' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}, and([^a-z]|$$).{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure there is no serial comma before 'or'.
check-bre-or: cat-my-text
	tr -s ' \n' ' ' < /tmp/cat | \
	sed -e 's/<[^>]*>//g' -e '\
	  s/, or [0-9.]* beers/x/g; \
	  s/, or a play on words/x/g; \
	  s/, or a comment, the characters \/\/ introduce a comment/x/g; \
	  s/, or calling a function that does any of those operations/x/g; \
	  s/, or most simply l[ag]/x/g; \
	  s/, or other behavior where this International Standard/x/g; \
	  s/, or pinhole camera/x/g; \
	  s/, or simply move on to other subjects and forget/x/g; \
	  s/, or spelling is bound to contain at least one eror/x/g; \
	  s/, or the Poincar&eacute; conjecture, really matter?/x/g; \
	  s/, or typographical error/x/g; \
	' > /tmp/tr
	grep -Ei ', or([^a-z]|$$)' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}, or([^a-z]|$$).{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure there is no comma before 'respectively'.
check-bre-respectively: cat-my-text
	tr -s ' \n' ' ' < /tmp/cat | grep -Ei ', respectively' > /tmp/err || true
	grep -Eino '.{0,30}, respectively.{0,30}|$$)' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure '-ize' spellings do not occur.
check-bre-spell-iz: cat-my-text
	sed ' \
	  s/0ZTIz/x/g; \
	  s/<code>[^<]*<\/code>/x/g; \
	  s/<em>The Customize Interface<\/em>/x/g; \
	  s/Customize Jenkins/x/g; \
	  s/ELIZA/x/g; \
	  s/Erase Customizations/x/g; \
	  s/I apologize for any confusion/x/g; \
	  s/I apologize for misunderstanding the joke/x/g; \
	  s/I apologize for the error/x/g; \
	  s/M-x customize[-a-z]* RET/x/g; \
	  s/Optimizing Compiler/x/g; \
	  s/Registrant Organization/x/g; \
	  s/ResizableDoubleArray/x/g; \
	  s/Revert This Session.s Customizations/x/g; \
	  s/Size$$/x/g; \
	  s/Size[^a-z]/x/g; \
	  s/Sizing[^a-z]/x/g; \
	  s/Undo Edits in Customization Buffer/x/g; \
	  s/[^A-Za-z]AUTHorize[^A-Za-z]/x/g; \
	  s/[^A-Za-z]Dehumanized$$/x/g; \
	  s/[^A-Za-z][Hh]orizontal[^A-Za-z]/x/g; \
	  s/[^A-Za-z][Rr]esize/x/g; \
	  s/[^A-Za-z][Ss]ize$$/x/g; \
	  s/[^A-Za-z][Ss]ize[^A-Za-z]/x/g; \
	  s/[^A-Za-z][Ss]ize[ds]$$/x/g; \
	  s/[^A-Za-z][Ss]ize[ds][^A-Za-z]/x/g; \
	  s/[^A-Za-z]box-sizing[^A-Za-z]/x/g; \
	  s/[^A-Za-z]horizon/x/g; \
	  s/[^A-Za-z]netizens[^A-Za-z]/x/g; \
	  s/[^A-Za-z]quiz/x/g; \
	  s/[^A-Za-z]resizing[^A-Za-z]/x/g; \
	  s/[^A-Za-z]seize[^A-Za-z]/x/g; \
	  s/[^A-Za-z]sizeof[^A-Za-z]/x/g; \
	  s/[_-]SIZE/x/g; \
	  s/initializer (6.7.8)/x/g; \
	  s/izz/x/g; \
	  s/package-initialize/x/g; \
	  s/public synchronized/x/g; \
	  s/traumatized by Java-esque/x/g; \
	  s/[^A-Za-z]verizon/x/g; \
	  s/[^A-Za-z]wizards\{0,1\}[^A-Za-z]/x/g; \
	' /tmp/cat > /tmp/tr
	grep -Ei 'iz[a-z]' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}iz[a-z].{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure '-yze' spellings do not occur.
check-bre-spell-yze: cat-my-text
	sed ' \
	  s/Field\.Index\.ANALYZED/x/g; \
	  s/StandardAnalyzer/x/g; \
	' /tmp/cat > /tmp/tr
	grep -in 'yze' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}yze.{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure 'color' spelling does not occur.
check-bre-spell-color: cat-my-text
	sed ' \
	  s/<code>[^<]*<\/code>/x/g; \
	  s/<em>Supported colors<\/em>/x/g; \
	  s/color: #/x/g; \
	  s/color: linear-gradient/x/g; \
	  s/href="[^"]*"/x/g; \
	  s/id="[^"]*"/x/g; \
	  s/prefers-color-scheme/x/g; \
	  s/src="[^"]*"/x/g; \
	  s/style="[^"]*"/x/g; \
	  s/style\.accentColor/x/g; \
	  s/style\.color/x/g; \
	' /tmp/cat > /tmp/tr
	grep -Ei 'color' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}color.{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure 'center' spelling does not occur.
check-bre-spell-center: cat-my-text
	sed ' \
	  s/align-items: center/x/g; \
	  s/style\.[A-Za-z]* = .center./x/g; \
	  s/text-align: center/x/g; \
	  s/class="[^"]*"/x/g; \
	  s/style="[^"]*"/x/g; \
	' /tmp/cat > /tmp/tr
	grep -Ei 'center' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}center.{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure 'license' spelling does not occur.
check-bre-spell-license: cat-my-text
	sed ' \
	  s/"credits" or "license" for more information/x/g; \
	  s/<code>[^<]*<\/code>/x/g; \
	  s/[^A-Za-z]LICENSE\.md[^A-Za-z]/x/g; \
	' /tmp/cat > /tmp/tr
	grep -Ei 'license' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}license.{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure 'i.e.' and 'e.g.' are not followed by comma.
check-bre-thatis: cat-my-text
	grep -E '(i\.e\.|e\.\g.),' /tmp/cat | tee /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure each comment file has a corresponding post file.
check-comment-files:
	ls -1 content/comments/ | \
	while read -r f; do \
	  echo "Checking post file for $$f"; \
	  if ! [ -e "content/blog/$$f" ] && ! [ -e "content/maze/$$f" ]; then \
	    echo "ERROR: No post file for comment file: $$f" && exit 1; \
	  fi; done
	@echo Done; echo

# Ensure copyright footers have the current year.
check-copyright:
	grep -q "&copy; 2005-$$(date +"%Y") Susam Pal" content/tree/cv.html content/tree/foss.html
	@echo Done; echo

# Ensure special HTML symbols do not occur without being encoded as entities.
check-entities:
	find content -name '*.html' -exec cat {} + | \
	sed -e '\
	  /<script>/,/<\/script>/d; \
	  /<style>/,/<\/style>/d; \
	' | \
	tr -s ' \n' ' ' | \
	sed -e 's/<[^>]*>//g' -e '\
	  s/<!--/x/g; \
	  s/-->/x/g; \
	  s/&#[0-9]*;/x/g; \
	  s/&#x[0-9A-Fa-f]*;/x/g; \
	  s/&[0-9A-Za-z]*;/x/g; \
	' > /tmp/tr
	grep -Ei '[<>&]' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}[<>&].{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure that 'i.e.' and 'e.g.' are preceded by commas.
check-general-thatis:
	find content \( -name '*.html' -o -name '*.txt' \) -exec cat {} + | \
	tr -s ' \n' ' ' > /tmp/tr
	grep -Ei '[^,] (i\.e\.|e\.g\.)' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}[^,] (i\.e\.|e\.g\.).{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure '\)' is not preceded by punctuation.  For example, '.  \)'
# and ', \)' are considered errors.  However, certain symbols are
# allowed before '\)' as they commonly appear in mathematical
# expressions within LaTeX delimiters.  For example, ') \)' and '} \)'
# are permitted.
check-tex-end:
	find content -name '*.html' -exec cat {} + | \
	tr -s ' \n' ' ' | \
	sed 's/<code>[^<]*<\/code>/x/g' | \
	tr -s ' \n' ' ' | \
	sed "s/' \\\\)/\\\\prime \\\\)/g" > /tmp/tr
	grep -Ei '[^])}+*0-9A-Za-z] \\\)' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}[^])}+*0-9A-Za-z] \\\).{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

NX = NX=$$(printf '\nx'); NL=$${NX%x}
NL = \\$${NL}

# Ensure that \lt and \gt is used in LaTeX instead of &lt; and &gt;.
check-tex-ltgt:
	# Match \( =>     Regex \\(     => Shell \\\\(.
	# Match \[ =>     Regex \\\[    => Shell \\\\\\[.
	# Match \] =>     Regex \\]     => Shell \\\\].
	# Match \begin => Regex \\begin => Shell \\\\begin.
	$(NX); find content -name '*.html' -exec cat {} + | \
	tr -s ' \n' ' ' | \
	sed "\
	  s/<code>[^<]*<\/code>/x/g; \
	  s/\\\\(/$(NL)&/g; \
	  s/\\\\)/&$(NL)/g; \
	  s/\\\\\\[/$(NL)&/g; \
	  s/\\\\]/&$(NL)/g; \
	  s/\\\\begin{[^}]*}/$(NL)&/g; \
	  s/\\\\end{[^}]*}/&$(NL)/g; \
	" | grep -Ei '\\\(|\\\)|\\\[|\\\]|\\begin|\\end' > /tmp/tr
	grep '&[lg]t;' /tmp/tr > /tmp/err || true
	cat /tmp/err
	! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure that sentence-ending punctuation does not appear in the
# generated website, as web browsers may place such punctuation on the
# next line, which looks visually odd.  The site generator moves all
# sentence-ending punctuation inside LaTeX delimiters so it always
# remains attached to the mathematical expression.
#
# For example, '\).'  and '\)?'  are not permitted in the generated
# website.  However, '\)</td>', '\)}', and '\)rd' are permitted.
check-tex-site: dist
	find _site -name '*.html' \
	  ! -path '_site/nq.html' \
	  -exec cat {} + | \
	sed -e 's/<code>[^<]*<\/code>/x/g' \
	    -e "s/\\\\)'/\\\\\\\\prime)/g" \
	    -e '\
	  s/\\)<\/td>/x/g; \
	  s/\\)[:)}<-]/x/g; \
	  s/\\)\\n/x/g; \
	  s/\\)rd/x/g; \
	  s/\\)th/x/g; \
	' > /tmp/tr
	grep -Ei '\\\)[^ ]' /tmp/tr > /tmp/err || true
	cat /tmp/err
	! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure '<br>', '\[' and '\]' are followed by newline.
check-newline:
	find content -name '*.html' \
	  ! -path 'content/tree/nq.html' \
	  -exec cat {} + | \
	sed ' \
	  s/\\\[<\/code>/x/g; \
	  s/\\]<\/code>/x/g; \
	' > /tmp/tr
	grep -Ei '(<br>.|\\\[.|\\].)' /tmp/tr > /tmp/err || true
	cat /tmp/err
	! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure that the configurations for the short-lived HTTP website and
# the long-lived HTTPS website are consistent.  The HTTP site is
# short-lived because it only runs during initial setup to enable
# domain validation while generating the TLS certificate for the HTTPS
# site.
check-nginx:
	sed -n '1,/limit_req_status/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '1,/limit_req_status/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	sed -n '/server_name [^w]/,/^}/p' etc/nginx/http.susam.net > /tmp/http.susam.net
	sed -n '/server_name [^w]/,/^}/p' etc/nginx/https.susam.net > /tmp/https.susam.net
	diff -u /tmp/http.susam.net /tmp/https.susam.net
	echo "$@: PASS"

# Ensure curly quotes do not occur.
check-quote:
	grep -r '[‘’“”]'  content; [ $$? = 1 ]
	echo "$@: PASS"

# Ensure there are no stray template placeholders in the generated
# website.
check-rendering:
	grep -r --include '*.html' --include '*.xml' --include '*.css' '{{' _site; [ $$? = 1 ]
	echo "$@: PASS"

# Ensure double spaces between sentences.
check-sentence-spacing: cat-all-text
	awk '{printf "%s  ", $$0}' /tmp/cat | \
	sed -e " \
	  s/  *'On Painting': '[^']*'/x/g; \
	  s/  *'The Code Book': '[^']*'/x/g; \
	  s/  *'Treasure Island': '[^']*'/x/g; \
	  s/  *Control: '[^']*'/x/g; \
	  s/  *Cosmos: '[^']*'/x/g; \
	  s/  *Cosmos: '[^']*'/x/g; \
	  s/  *Grip: '[^']*'/x/g; \
	  s/  *Relativity: '[^']*'/x/g; \
	  s/  *Sentences: '[^']*'/x/g; \
	  s/  *Test: '[^']*'/x/g; \
	  s/  *Words: '[^']*'/x/g; \
	  s/ ? '/x/g; \
	" -e ' \
	  s/<code>[^<]*<\/code>/x/g; \
	  s/<kbd>[^<]*<\/kbd>/x/g; \
	  s/<samp>[^<]*<\/samp>/x/g; \
	' -e ' \
	  s/  *Hyperspace: "[^"]*"/x/g; \
	  s/  *Sentences: "[^"]*"/x/g; \
	  s/  *Test: "[^"]*"/x/g; \
	  s/ ? [0-9A-Za-z`(-]/x/g; \
	  s/(n - 1)! \\,/x/g; \
	  s/0B66:[^<]*<\/samp>/x/g; \
	  s/117C:[^<]*<\/samp>/x/g; \
	  s/<!-- [0-9][0-9]*\./<!-- x/g; \
	  s/<!-- title: [^>]*[?.] -->/<!-- x -->/g; \
	  s/? "/x/g; \
	  s/Mr\./x/g; \
	  s/[a-z]\. [a-z]/x/g; \
	  s/\( [A-Z]\.\)\{1,3\}/ x/g; \
	  s/\.\.\./x/g; \
	  s/\\left\./x/g; \
	  s/e\.g\./x/g; \
	  s/ed\. [a-z(]/x/g; \
	  s/etc\. [a-z]/x/g; \
	  s/i\.e\./x/g; \
	' > /tmp/tr
	grep -Ei '[.?!] [^ ]' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}[.?!] [^ ].{0,30}' /tmp/err || true
	! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Run HTML tidy on the website.
tidy: dist
	find _site -name "*.html" | while read -r page; do \
	  echo Tidying "$$page"; \
	  sed 's/ method="dialog"//' "$$page" > /tmp/tmp.html; \
	  tidy -q -e --warn-proprietary-attributes no /tmp/tmp.html || exit 1; \
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


# Commit changes and publish website across all mirrors.
copub: co pub

# Publish website across all mirrors.
pub: cu gh cb

# Commit changes to the content update ('cu') branch.
co:
	git checkout cu
	git status
	@echo
	@echo 'Press ENTER to proceed to commit.  Press Ctrl+C to cancel.' && read
	@echo
	git add -p
	git commit --amend --reset-author

# Publish the content update ('cu') branch to the web server.
cu:
	git push origin main
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make recu"

# Publish the content update ('cu') branch and restart the web server.
cure:
	git push origin main
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make recu restart"

pull-backup:
	mkdir -p ~/bkp/
	ssh susam.net "tar -czf - -C /opt/data/ form/" > ~/bkp/form-$$(date "+%Y-%m-%d_%H-%M-%S").tgz
	ls -lh ~/bkp/

GIT_SRC = https://github.com/susam/susam.net

mirror: site
	@echo Creating mirror ...
	rm -rf /tmp/mirror/
	mv _site /tmp/mirror/
	git rev-parse --short HEAD > /tmp/rev.txt
	printf '%b' "# Mirror of Susam's Website\n\n\
Automatically generated from \
commit [$$(cat /tmp/rev.txt)]($(GIT_SRC)/commit/$$(cat /tmp/rev.txt)) \
of <$(GIT_SRC)>.\n\n\
Visit <$(WEB_URL)> to visit the mirror.\n\n\
Visit <https://susam.net/> to visit the original website.\n" > /tmp/mirror/README.md
	cd /tmp/mirror/ && git init
	cd /tmp/mirror/ && git config user.name "Susam Pal"
	cd /tmp/mirror/ && git config user.email susam@susam.net
	cd /tmp/mirror/ && git add .
	cd /tmp/mirror/ && git commit -m "Generated from $(GIT_SRC)"
	cd /tmp/mirror/ && git log
	cat /tmp/mirror/README.md
	@echo Done; echo

push-mirror:
	make mirror GIT_DST=$(GIT_DST) WEB_URL=$(WEB_URL)
	@echo Publishing mirror to $(WEB_URL) ...
	cd /tmp/mirror/ && git remote remove origin || :
	cd /tmp/mirror/ && git remote add origin "$(GIT_DST)"
	cd /tmp/mirror/ && git push -f origin main
	@echo Done; echo

gh:
	make push-mirror \
	  GIT_DST=git@github.com:susam/susam.github.io.git \
	  WEB_URL=https://susam.github.io/

cb:
	make push-mirror \
	  GIT_DST=ssh://git@codeberg.org/susam/pages.git \
	  WEB_URL=https://susam.codeberg.page/
