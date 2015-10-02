#!/usr/bin/bash

AWK=/usr/bin/awk
SED=/usr/bin/sed

USER=dalmatiner
GROUP=$USER

case $2 in
    PRE-INSTALL)
        #if grep '^Image: base64 1[34].[1234].*$' /etc/product
	#then
	#    echo "Image version supported"
	#else
	#    echo "This image version is not supported please use the base64 13.2.1 image."
	#    exit 1
        #fi
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating dalmatinerfe group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating dalmatinerfe user ...
            useradd -g $GROUP -d /var/db/dalmatinerfe -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/dalmatinerfe
        chown -R $USER:$GROUP /var/db/dalmatinerfe
        mkdir -p /var/log/dalmatinerfe/sasl
        chown -R $USER:$GROUP /var/log/dalmatinerfe
        if [ -d /tmp/dalmatinerfe ]
        then
            chown -R $USER:$GROUP /tmp/dalmatinerfe
        fi
        ;;
    POST-INSTALL)
        echo Importing service ...
        svccfg import /opt/local/dalmatinerfe/share/dfe.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`
        CONFFILE=/opt/local/dalmatinerfe/etc/dalmatinerfe.conf
        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        else
            echo "Config file exists you might need to update settings"
            #echo "Merging old file with new template, the original can be found in ${CONFFILE}.old."
            #/opt/local/dalmatinerfe/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
            #    mv ${CONFFILE} ${CONFFILE}.old &&
            #    mv ${CONFFILE}.new ${CONFFILE}
        fi
        ;;
esac
