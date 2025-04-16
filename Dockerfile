FROM maven:3.8.1-openjdk-17-slim
RUN mkdir -p /usr/local/newrelic
COPY newrelic/newrelic.yml /usr/local/newrelic
COPY newrelic/newrelic.jar /usr/local/newrelic
COPY target/runner-shipment-services-0.0.1.jar /usr/local/lib/runner-shipment-services-0.0.1.jar
EXPOSE 8080

