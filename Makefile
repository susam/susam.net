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
	@echo '  cu                Pull and deploy content updates.'
	@echo '  restart           Restart Nginx and the form service.'
	@echo '  check-form-live   Check forms on the live website.'
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
	@echo '  ls                List form data submitted.'
	@echo '  rd                Read form data submitted.'
	@echo
	@echo 'Local targets that SSH to the live server:'
	@echo '  scu               Publish content updates and deploy them.'
	@echo '  scure             Publish content updates, deploy and restart services.'
	@echo '  sbkp              Pull a backup of form data from the live server.'
	@echo '  sfs               Redownload the file store and deploy the site.'
	@echo '  sync              Copy local source to the live server and deploy it.'
	@echo
	@echo 'Local publishing targets:'
	@echo '  cc                Set up remotes and publish branches.'
	@echo '  mirror            Publish a generated website mirror.'
	@echo
	@echo 'Low-level targets:'
	@echo '  live              Generate live directory for website.'
	@echo '  site              Generate website.'
	@echo '  fs                Download file store.'
	@echo '  rmfs              Remove the downloaded file store.'
	@echo '  refs              Remove and redownload the file store.'
	@echo '  dist              Generate website for distribution as zip/tarball.'
	@echo '  serve             Serve _site/ directory via a local HTTP server.'
	@echo '  deep              Put website in a directory deeply nested within _site/.'
	@echo
	@echo 'Development targets:'
	@echo '  clean             Clean temporary files.'
	@echo '  opt               Create directories at /opt for testing.'
	@echo '  loop              Run a loop to create website directory repeatedly.'
	@echo '  lsc               List comment counts in descending order.'
	@echo '  ref               Create reference directories for the current website.'
	@echo '  diff              Create new websites and compare against reference directories.'
	@echo '  profile           Profile website generation.'
	@echo '  run-site          Serve website locally via a local HTTP server.'
	@echo '  run-dist          Serve distribution locally via a local HTTP server.'
	@echo '  run-site-deep     Serve website from a subdirectory path.'
	@echo '  run-dist-deep     Serve distribution from a subdirectory path.'
	@echo '  run-form          Run form application locally.'
	@echo
	@echo 'Checks to run in development environment:'
	@echo '  test              Test the Common Lisp program.'
	@echo '  chk               Run source checks suitable before push.'
	@echo '  tidy              Run HTML Tidy on the generated website.'
	@echo '  xmllint           Run XML Linter on the generated website.'
	@echo '  check-links       Check links in a locally running website.'
	@echo '  check-form-rate   Check live form rate limiting from any host.'
	@echo '  check-form-dev    Check forms in the local dev environment.'
	@echo
	@echo 'Default target:'
	@echo '  help              Show this help message.'


# Targets To Run On Live Server
# -----------------------------

setup: debs cldeps

debs:
	apt-get update
	apt-get -y install nginx certbot sbcl time

cldeps: fs
	rm -rf /opt/cl/
	mkdir -p /opt/cl/form/
	cp -R _fs/opt/cl/form/. /opt/cl/form/

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
	ln -snf "$$PWD/etc/logrotate" /etc/logrotate.d/susam
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
	rm -f /etc/logrotate.d/susam
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

cu: checkroot
	git checkout cu
	git reset --hard HEAD~5
	git pull origin cu
	make live

restart:
	systemctl restart nginx form
	systemctl --no-pager status nginx form

check-form-live: checkroot
	make check-form URL=https://susam.net/ SLEEP=15

checkroot:
	@echo Checking if current user is root ...
	[ $$(id -u) = 0 ]
	@echo Done; echo


# Monitoring Targets To Run On Live Server
# ----------------------------------------

BOT_RE = AppEngine-Google|Amazonbot|Barkrowler|ChatGPT|Bot|HeadlessChrome|InfoHubFetcher|FeedFetcher-Google|OpenClaw|RSS

NOISE_RE = \.(css|js|ico|png|ttf|woff|xml)|robots\.txt|$(BOT_RE)

nono:
	grep -vE "$(NOISE_RE)"

follow-log:
	sudo tail -F /var/log/nginx/access.log | grep -vE "$(NOISE_RE)"

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

ls:
	ls -l /opt/data/form/*.txt | less -F

rd:
	tail -vn +1 /opt/data/form/*.txt | less -F


# Local Targets That SSH To The Live Server
# -----------------------------------------

scu:
	git push origin main
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make cu"

scure:
	git push origin main
	git push -f origin cu
	ssh -t susam.net "cd /opt/susam.net/ && sudo make cu restart"

sbkp:
	mkdir -p ~/bkp/
	ssh susam.net "tar -czf - -C /opt/data/ form/" > ~/bkp/form-$$(date "+%Y-%m-%d_%H-%M-%S").tgz
	ls -lh ~/bkp/

sfs:
	ssh -t susam.net "cd /opt/susam.net/ && sudo make refs cu"

sync:
	tar --format=ustar -czf /tmp/susam.net.tgz -X .gitignore --exclude=.git .
	scp /tmp/susam.net.tgz susam.net:/tmp/susam.net.tgz
	ssh -t susam.net " \
	  sudo rm -rf /tmp/susam.net/ && \
	  sudo mkdir -p /tmp/susam.net/ /opt/susam.net/ && \
	  sudo tar -xf /tmp/susam.net.tgz -C /tmp/susam.net/ && \
	  sudo cp -R /tmp/susam.net/. /opt/susam.net/ && \
	  cd /opt/susam.net/ && sudo make live"


# Local Publishing Targets
# ------------------------

cc:
	git remote remove cc || :
	git remote add cc git@codeberg.org:susam/$(FQDN).git
	git remote set-url cc --add git@github.com:susam/$(FQDN).git
	git remote remove origin || :
	git remote add origin git@github.com:susam/$(FQDN).git
	git remote -v
	git fetch
	git push origin main
	git push cc main --tags
	git push -fu origin cu
	git push -f cc cu

gitsite: site
	@echo Creating mirror ...
	rm -rf /tmp/mirror/
	mv _site /tmp/mirror/
	cd /tmp/mirror/ && git init
	cd /tmp/mirror/ && git config user.name "Susam Pal"
	cd /tmp/mirror/ && git config user.email susam@susam.net
	cd /tmp/mirror/ && git add .
	cd /tmp/mirror/ && git commit -m "Mirror website"
	cd /tmp/mirror/ && git log
	@echo Done; echo

mirror: gitsite
	@echo Publishing mirror to $(WEB_URL) ...
	cd /tmp/mirror/ && git remote remove origin || :
	cd /tmp/mirror/ && git remote add origin git@github.com:susam/susam.github.io.git
	cd /tmp/mirror/ && git remote set-url origin --add git@codeberg.org:susam/pages.git
	cd /tmp/mirror/ && git push -f origin main
	@echo Done; echo


# Low-Level Targets
# -----------------

live: site
	@echo Setting up live directory ...
	mv _live _gone || :
	mv _site _live
	rm -rf _gone
	@echo Done; echo

site: fs
	@echo Generating website ...
	# The following command intentionally ends with a semicolon to force
	# make to execute it through the shell.  This keeps 'time' output
	# consistent with that of 'make dist'.
	time sbcl --script site.lisp;
	@echo Done; echo

profile: fs
	@echo Profiling website generation ...
	sbcl --non-interactive --eval '(defvar *site-mode* nil)' --load site.lisp --eval '(sb-profile:profile "CL-USER")' --eval '(main)' --eval '(sb-profile:report :limit 10)'
	@echo Done; echo

dist: fs
	@echo Generating distributable website ...
	time sbcl --noinform \
	          --eval '(setf *break-on-signals* t)' \
	          --eval '(defvar *params* (list (cons "index" "index.html")))' \
	          --script site.lisp 2>&1 | tee dist.log
	@[ -s dist.log ] && \
	  echo '============================================================' && \
	  grep -C10 Backtrace dist.log && \
	  echo '============================================================'; :
	! grep -q Backtrace dist.log
	@echo Done; echo

serve:
	python3 -m http.server -b localhost -d _site/

deep:
	rm -rf _prep/
	mkdir -p _prep/foo/bar/baz/qux/
	mv _site/ _prep/foo/bar/baz/qux/
	mv _prep/ _site/

fs: _fs/

_fs/:
	echo Downloading FS ...
	curl -L https://github.com/susam/fs/archive/main.tar.gz | tar -xz
	mv fs-main/ _fs/
	du -sh _fs/

rmfs:
	rm -rf _fs/

refs: rmfs fs


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

lsc:
	(find content/tree/cm -name '*.disc.html' && find content/tree -maxdepth 1 -name '*.talk.html') | \
	while read -r f; do \
	  echo $$(grep '<!-- date:' "$$f" "$$f" | wc -l) "$$f"; \
	done | sort -rn | less -F

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

ls-unlist:
	grep -r 'unlist:' content

ls-tag:
	grep -r 'tag:' content


# Checks To Run In Development Environment
# ----------------------------------------

check-form-rate:
	n=0; while [ $$n -le 60 ]; do printf "$$n $$(date +%H:%M:%S) "; \
	curl -sSI https://susam.net/form/ | head -n 1; n=$$(( $$n + 1 )); \
	sleep 1; done
	@echo Done; echo

test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

chk: \
  cvsplit \
  check-abbrev-spacing \
  check-sentence-spacing \
  check-bre-respectively \
  check-copyright \
  check-entities \
  check-tex-site \
  check-newline \
  check-nginx \
  check-pre \
  check-quote \
  check-rendering \
  tidy \
  xmllint

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

pc:
	sed -nE '/<pre>/,/<\/pre>/{ /(<pre>|<\/pre>)/d; p; }' content/tree/pc.html > content/tree/pc.txt

cat-my-text:
	find content \( -name '*.html' -o -name '*.txt' \) \
	  ! -name '*.disc.html' \
	  ! -name '*.talk.html' \
	  ! -path 'content/licence/mit.html' \
	  -exec cat {} + > /tmp/cat
	sed -n '/name: Susam/,/date:/p' content/tree/cm/*.disc.html content/tree/*.talk.html >> /tmp/cat

cat-all-text:
	find content -type f \
	  ! -name '.DS_Store' \
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
	  s/" \. "/x/g; \
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

# Ensure there is no comma before 'respectively'.
check-bre-respectively: cat-my-text
	tr -s ' \n' ' ' < /tmp/cat | grep -Ei ', respectively' > /tmp/err || true
	grep -Eino '.{0,30}, respectively.{0,30}|$$)' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

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
	sed -e 's/<[^<>]*>//g' -e '\
	  s/C:\\>lynx susam\.net/x/g; \
	  s/<!--/x/g; \
	  s/-->/x/g; \
	  s/&#[0-9]*;/x/g; \
	  s/&#x[0-9A-Fa-f]*;/x/g; \
	  s/&[0-9A-Za-z]*;/x/g; \
	' > /tmp/tr
	grep -Ei '[<>&]' /tmp/tr > /tmp/err || true
	grep -Eino '.{0,30}([<>&]).{0,30}' /tmp/err || true
	@! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

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
	  s/\\\[1.5em/x/g; \
	  s/\\\[1em/x/g; \
	  s/\\\[<\/code>/x/g; \
	  s/\\]<\/code>/x/g; \
	  s/\\]\\\\]\/g,/x/g; \
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

# Check that <pre> elements contain <code> or <samp> elements.
check-pre:
	find content -name '*.html' \
	  ! -path 'content/tree/cm/eql.disc.html' \
	  ! -path 'content/tree/cfrs.html' \
	  ! -path 'content/tree/code/web/miller-rabin-speed-test.html' \
	  ! -path 'content/tree/code/test/lorem.page.html' \
	  ! -path 'content/tree/eql.post.html' \
	  ! -path 'content/tree/fxyt.html' \
	  ! -path 'content/tree/pc.html' \
	  -exec cat {} + | \
	grep -E '</?pre>' | \
	grep -vE '<pre><code>|<pre><samp>|</code></pre>|</samp></pre>' > /tmp/err || true
	cat /tmp/err
	! [ -s /tmp/err ] && echo "$@: PASS" || (echo "$@: ERROR" && false)

# Ensure curly quotes do not occur.
check-quote:
	grep -r '[‘’“”]'  content; [ $$? = 1 ]
	echo "$@: PASS"

# Ensure there are no stray template placeholders in the generated
# website.
check-rendering:
	grep -r --include '*.html' --include '*.xml' --include '*.css' '{{' _site; [ $$? = 1 ]
	echo "$@: PASS"

# Run HTML tidy on the website.
tidy: dist
	find _site -name "*.html" | \
	grep -v '_site/css-fizz-buzz\.html' | \
	grep -v '_site/code/web/css-fizz-buzz-ol\.html' | \
	grep -v '_site/nested-code-fences.html' | \
	while read -r page; do \
	  echo Tidying "$$page"; \
	  sed ' \
	    s/ method="dialog"//; \
	    s/<ol type="[^"]*">/<ol>/; ' \
	  "$$page" > /tmp/tmp.html; \
	  tidy -q -e --warn-proprietary-attributes no /tmp/tmp.html || exit 1; \
	done
	@echo Done; echo

xmllint: dist
	find _site -name "*.xml" | \
	while read -r xml; do \
	  echo Linting "$$xml"; \
	  xmllint --noout "$$xml"; \
	done
	@echo Done; echo

check-links:
	@echo "NOTE: Ensure 'make run-site-deep' is running before running this target"; echo
	sed -i .bkp 's/$${jsURL}/wander.js/' _site/foo/bar/baz/qux/_site/wander/index.html
	-wget -r -l 0 --spider -nd -nv http://localhost:8000/ -o run.log
	grep -B1 broken run.log
	@echo Done; echo

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
