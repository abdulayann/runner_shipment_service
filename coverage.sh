#!/bin/bash


# echo "Download Jacoco Agent"
# curl "https://repo1.maven.org/maven2/org/jacoco/org.jacoco.agent/0.8.7/org.jacoco.agent-0.8.7-runtime.jar" -o "./jacocoagent.jar"
# curl "https://repo1.maven.org/maven2/org/jacoco/org.jacoco.cli/0.8.7/org.jacoco.cli-0.8.7-nodeps.jar" -o "./jacococli.jar"

echo "Start Server"
java  -jar -Dprops_profile=buildtest /usr/local/lib/runner-shipment-services-0.0.1.jar


