FROM adoptopenjdk:14-jre-hotspot
COPY target/fractl-0.1.4-standalone.jar /fractl-runtime.jar
COPY library/config.edn /config.edn
RUN mkdir /library
COPY library/notification.fractl /library/notification.fractl
COPY library/logging.fractl /library/logging.fractl
COPY library/ledger.fractl /library/ledger.fractl
COPY library/app_catalog.fractl /library/app_catalog.fractl
COPY library/policy.fractl /library/policy.fractl
COPY library/catalog.fractl /library/catalog.fractl
COPY library/authentication.fractl /library/authentication.fractl
COPY library/identity.fractl /library/identity.fractl
COPY library/model.fractl /library/model.fractl
CMD ["java", "-jar", "fractl-runtime.jar", "-c", "config.edn", "library/model.fractl"]