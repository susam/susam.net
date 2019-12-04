NAME = susam
FQDN = $(NAME).in
MAIL = $(NAME)@$(FQDN)

help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'High-level targets:'
	@echo '  setup   Install Debian packages.'
	@echo '  https   Reinstall live website and serve with Nginx via HTTPS.'
	@echo '  http    Reinstall live website and serve with Nginx via HTTP.'
	@echo '  update  Pull latest Git commits and update live website.'
	@echo '  rm      Uninstall live website.'
	@echo '  local   Generate local website and serve with Python.'
	@echo
	@echo 'Low-level targets:'
	@echo '  live    Generate live website but do not serve.'
	@echo '  site    Generate local website but do not serve.'
	@echo '  pull    Pull latest Git commits but do not update live website.'
	@echo
	@echo 'Default target:'
	@echo '  help    Show this help message.'

setup:
	apt-get update
	apt-get -y install nginx certbot uwsgi uwsgi-plugin-python3

https: http
	@echo Setting up HTTPS website ...
	certbot certonly -n --agree-tos -m '$(MAIL)' --webroot \
	                 -w '/var/www/$(FQDN)' -d '$(FQDN),www.$(FQDN)'
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
	uwsgi --version
	mkdir -p /opt/live
	mkdir -p /opt/cache
	chown www-data:www-data /opt/cache
	ln -snf "$$PWD" '/opt/live/$(FQDN)' # Symlink for spapp.service
	systemctl enable "$$PWD/etc/spapp.service"
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
	rm -f '/opt/live/$(FQDN)'
	rm -f /tmp/spapp.sock # Created by sapp.service
	#
	# Following crontab entries left intact:
	crontab -l | grep -v "^#" || :
	@echo Done; echo

local: site
	@echo Serving website locally ...
	if python3 -c "import http.server" 2> /dev/null; then \
		echo Running Python3 http.server ...; \
		cd _site && python3 -m http.server; \
	elif python -c "import http.server" 2> /dev/null; then \
		echo Running Python http.server ...; \
		cd _site && python -m http.server; \
	elif python -c "import SimpleHTTPServer" 2> /dev/null; then \
		echo Running Python SimpleHTTPServer ...; \
		cd _site && python -m SimpleHTTPServer; \
	else \
		echo Cannot find http.server or SimpleHTTPServer.;  \
	fi
	@echo Done; echo

live: site
	@echo Setting up live directory ...
	mv _live _gone || :
	mv _site _live
	rm -rf _gone
	@echo Done; echo

site:
	@echo Generating website ...
	python3 -m py.makesite
	@echo Done; echo

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


# GitHub Pages Mirror

TMP_REV = /tmp/rev.txt
CAT_REV = cat $(TMP_REV)
GIT_SRC = https://github.com/susam/susam.in
GIT_DST = https://github.com/susam/susam.github.io
WEB_URL = https://github.susam.io/
TMP_GIT = /tmp/tmpgit
README  = $(TMP_GIT)/README.md

ghmirror: site
	#
	## Create mirror.
	rm -rf $(TMP_GIT)
	mv _site $(TMP_GIT)
	git rev-parse --short HEAD > $(TMP_REV)
	echo Mirror of Susam\'s Blog >> $(README)
	echo ====================== >> $(README)
	echo >> $(README)
	echo Automatically generated from [susam/susam.in][GIT_SRC] >> $(README)
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
	cd $(TMP_GIT) && git config user.email susam@susam.in
	cd $(TMP_GIT) && git add .
	cd $(TMP_GIT) && git commit -m "Generated from $(GIT_SRC) - $$($(CAT_REV))"
	cd $(TMP_GIT) && git remote add origin "$(GIT_DST).git"
	cd $(TMP_GIT) && git log
	cd $(TMP_GIT) && git push -f origin master

# SMTP
SMTP_USER = susam
SMTP_FWDS = susam.pal@gmail.com, susampal@protonmail.com

checkpass:
	@echo Checking if smtppass has been provided ...
	[ -n "$(smtppass)" ]
	@echo Done; echo

smtp: checkroot checkpass rmsmtp
	#
	@echo Installing SMTP server ...
	apt-get update
	apt-get -y install exim4
	#
	@echo Editing configuration ...
	sed -e "s/\(dc_eximconfig_configtype\)=.*/\1='internet'/" \
	    -e "s/\(dc_other_hostnames\)=.*/\1='$(FQDN)'/" \
	    -e "s/\(dc_local_interfaces\)=.*/\1=''/" \
	    /etc/exim4/update-exim4.conf.conf > /tmp/update-exim4.conf.conf
	mv /tmp/update-exim4.conf.conf /etc/exim4
	#
	@echo Setting mailname ...
	echo "$(FQDN)" > /etc/mailname
	#
	@echo Setting aliases ...
	echo "$(SMTP_USER): $(SMTP_FWDS)" >> /etc/aliases
	echo "root: $(SMTP_FWDS)" >> /etc/aliases
	#
	@echo Adding macros ...
	( \
	    echo MAIN_TLS_ENABLE = true; \
	    echo AUTH_SERVER_ALLOW_NOTLS_PASSWORDS = false; \
	) > /etc/exim4/exim4.conf.localmacros
	#
	@echo Editing template ...
	sed -e "/plain_server/,/endif/ s/# //" \
	    /etc/exim4/exim4.conf.template > /tmp/exim4.conf.template
	mv /tmp/exim4.conf.template /etc/exim4
	#
	@echo Creating password ...
	echo "$(SMTP_USER):$$(mkpasswd -m sha-512 "$(smtppass)")" \
	     > /etc/exim4/passwd
	chown root:Debian-exim /etc/exim4/passwd
	chmod 640 /etc/exim4/passwd
	# Following commands are based on the content of:
	# /usr/share/doc/exim4-base/examples/exim-gencert
	#
	@echo Generating certificate ...
	openssl req -x509 -newkey rsa:2048 \
	        -keyout /etc/exim4/exim.key -out /etc/exim4/exim.crt \
		-days 1095 -nodes -subj '/CN=$(FQDN)'
	chown root:Debian-exim /etc/exim4/exim.key /etc/exim4/exim.crt
	chmod 640 /etc/exim4/exim.key /etc/exim4/exim.crt
	#
	@echo Generating configuration ...
	update-exim4.conf
	#
	@echo Restarting SMTP server ...
	systemctl restart exim4
	#
	@echo Sending test message ...
	echo hello, world -- Makefile | mail -s "hello, world" susam@susam.in
	@echo Done; echo

rmsmtp:
	#
	# Remove files we created, so that apt-get purge can get rid of
	# /etc/exim4 directory.
	@echo Removing SMTP server and configuration ...
	rm -f /etc/exim4/exim4.conf.localmacros /etc/exim4/passwd \
	      /etc/exim4/exim.crt /etc/exim4/exim.key
	#
	apt-get -y purge exim4 exim4-base
	#
	# Remove packages installed due to "Recommends" field.
	apt-get -y purge libfribidi0 psmisc
	apt-get -y autoremove --purge
	#
	# Remove files that apt-get purge did not remove.
	rm -f /etc/mailname /etc/aliases
	@echo Done; echo

# Checks
checks:
	# Ensure punctuation goes inside inline-math.
	! grep -IErn '\\)[^ ]' content | grep -vE '\\)(th|-|</h[1-6]>|\)|:)'
	! grep -IErn '(th|-|</h[1-6]>|:) \\)' content
