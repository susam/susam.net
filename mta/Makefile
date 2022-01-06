FQDN = susam.in
SMTP_ROOT = root
SMTP_USER = susam
SMTP_FWDS = foo@example.com, bar@example.com

smtp: checkroot checkpass rmsmtp
	# --------------------------------------------------------------
	# Installing SMTP server ...
	# --------------------------------------------------------------
	apt-get update
	apt-get -y install exim4
	# --------------------------------------------------------------
	# Editing configuration ...
	# --------------------------------------------------------------
	sed -e "s/\(dc_eximconfig_configtype\)=.*/\1='internet'/" \
	    -e "s/\(dc_other_hostnames\)=.*/\1='$(FQDN)'/" \
	    -e "s/\(dc_local_interfaces\)=.*/\1=''/" \
	    /etc/exim4/update-exim4.conf.conf > /tmp/update-exim4.conf.conf
	mv /tmp/update-exim4.conf.conf /etc/exim4
	# --------------------------------------------------------------
	# Setting mailname for security notifications ...
	# --------------------------------------------------------------
	echo "$(FQDN)" > /etc/mailname
	# --------------------------------------------------------------
	# Setting aliases for email forwarding ...
	# --------------------------------------------------------------
	echo "$(SMTP_USER): $(SMTP_FWDS)" >> /etc/aliases
	echo "$(SMTP_ROOT): $(SMTP_FWDS)" >> /etc/aliases
	# --------------------------------------------------------------
	# Enabling AUTH PLAIN ...
	# --------------------------------------------------------------
	sed -e "/plain_server/,/endif/ s/# //" \
	    /etc/exim4/exim4.conf.template > /tmp/exim4.conf.template
	mv /tmp/exim4.conf.template /etc/exim4
	echo "$(SMTP_USER):$$(mkpasswd -m sha-512 "$(smtppass)")" \
	     > /etc/exim4/passwd
	chown root:Debian-exim /etc/exim4/passwd
	chmod 640 /etc/exim4/passwd
	# --------------------------------------------------------------
	# Enabling STARTTLS ...
	# --------------------------------------------------------------
	echo MAIN_TLS_ENABLE = yes >> /etc/exim4/exim4.conf.localmacros
	openssl req -x509 -newkey rsa:2048 \
	        -keyout /etc/exim4/exim.key -out /etc/exim4/exim.crt \
		-days 1095 -nodes -subj '/CN=$(FQDN)'
	chown root:Debian-exim /etc/exim4/exim.key /etc/exim4/exim.crt
	chmod 640 /etc/exim4/exim.key /etc/exim4/exim.crt
	# --------------------------------------------------------------
	# Enabling DKIM ...
	# --------------------------------------------------------------
	echo DKIM_CANON = relaxed >> /etc/exim4/exim4.conf.localmacros
	echo DKIM_SELECTOR = s1 >> /etc/exim4/exim4.conf.localmacros
	echo DKIM_DOMAIN = $(FQDN) >> /etc/exim4/exim4.conf.localmacros
	echo DKIM_PRIVATE_KEY = /etc/exim4/exim.key >> \
	     /etc/exim4/exim4.conf.localmacros
	openssl pkey -in /etc/exim4/exim.key -pubout \
	             -outform DER | base64 -w 0 > /tmp/publickey
	# --------------------------------------------------------------
	# Restarting Exim 4 ...
	# --------------------------------------------------------------
	update-exim4.conf
	systemctl restart exim4
	# --------------------------------------------------------------
	# Sending test email ...
	# --------------------------------------------------------------
	echo hello, world -- Makefile | \
	    mail -a "From: root@susam.in" -s "hello, world" susam@susam.in
	# --------------------------------------------------------------
	# Generating DNS records ...
	# --------------------------------------------------------------
	@echo "Add the following TXT records to DNS:"
	@echo
	@echo "@ TXT v=spf1 mx -all"
	@echo "s1._domainkey TXT p=$$(cat /tmp/publickey)"
	@echo "_dmarc TXT v=DMARC1; p=reject; adkim=s; aspf=s"
	@echo
	@echo Done; echo

rmsmtp: checkroot
	# --------------------------------------------------------------
	# Removing Exim 4 ...
	# --------------------------------------------------------------
	rm -f /etc/exim4/exim4.conf.localmacros /etc/exim4/passwd
	rm -f /etc/exim4/exim.crt /etc/exim4/exim.key
	apt-get -y purge exim4 exim4-base
	# --------------------------------------------------------------
	# Removing packages installed due to "Recommends" field ...
	# --------------------------------------------------------------
	apt-get -y purge libfribidi0 psmisc
	apt-get -y autoremove --purge
	# --------------------------------------------------------------
	# Removing files that apt-get purge did not remove ...
	# --------------------------------------------------------------
	rm -f /etc/mailname /etc/aliases
	@echo Done; echo

checkroot:
	@echo Checking if current user is root ...
	[ $$(id -u) = 0 ]
	@echo Done; echo

checkpass:
	@echo Checking if smtppass has been provided ...
	[ -n "$(smtppass)" ]
	@echo Done; echo
