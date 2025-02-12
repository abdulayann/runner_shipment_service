package com.dpw.runner.shipment.services.config;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.flywaydb.core.Flyway;
import org.flywaydb.core.api.Location;
import org.flywaydb.core.api.MigrationInfo;
import org.flywaydb.core.api.callback.Callback;
import org.flywaydb.core.api.callback.Context;
import org.flywaydb.core.api.callback.Event;
import org.flywaydb.core.api.resource.LoadableResource;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class FlywaySQLLogger implements Callback {

    private final ObjectProvider<Flyway> flywayProvider;

    public FlywaySQLLogger(ObjectProvider<Flyway> flywayProvider) {
        this.flywayProvider = flywayProvider;
    }

    @Override
    public boolean supports(Event event, Context context) {
        return event == Event.BEFORE_MIGRATE;
    }

    @Override
    public boolean canHandleInTransaction(Event event, Context context) {
        return false;
    }

    @Override
    public void handle(Event event, Context context) {
        log.info("========== Checking Pending Flyway Migrations ==========");

        Flyway flyway = flywayProvider.getIfAvailable();
        if (flyway == null) {
            log.warn("Flyway bean is not available yet. Skipping SQL logging.");
            return;
        }

        try {
            MigrationInfo[] pendingMigrations = flyway.info().pending();

            if (pendingMigrations.length == 0) {
                log.info("No pending migrations! The database is up to date.");
            } else {
                log.info("Pending Migrations:");
                Arrays.stream(pendingMigrations).forEach(migration -> {
                    log.info("Version: {} - Description: {}", migration.getVersion(), migration.getDescription());
                    logMigrationSQL(flyway, migration.getScript());
                });
            }
        } catch (Exception e) {
            log.error("Error retrieving pending migrations", e);
        }

        log.info("========== Flyway Migration Execution Will Now Start ==========");
    }

    private void logMigrationSQL(Flyway flyway, String scriptName) {
        try {
            List<String> locations = Arrays.stream(flyway.getConfiguration().getLocations())
                    .map(Location::getDescriptor).toList();

            LoadableResource resource = null;
            for (String location : locations) {
                ClassPathResource classPathResource = new ClassPathResource(location.replace("classpath:", "") + "/" + scriptName);
                if (classPathResource.exists()) {
                    resource = new LoadableResource() {
                        @Override
                        public Reader read() {
                            try {
                                return new InputStreamReader(classPathResource.getInputStream(), StandardCharsets.UTF_8);
                            } catch (IOException e) {
                                log.error("Error reading migration script '{}'", scriptName, e);
                                return null; // Prevents exception propagation
                            }
                        }

                        @Override
                        public String getAbsolutePath() {
                            return classPathResource.getPath();
                        }

                        @Override
                        public String getAbsolutePathOnDisk() {
                            return "";
                        }

                        @Override
                        public String getFilename() {
                            return classPathResource.getFilename();
                        }

                        @Override
                        public String getRelativePath() {
                            return "";
                        }
                    };
                    break; // Stop searching once found
                }
            }

            if (resource == null) {
                log.warn("No SQL file found for migration: {}", scriptName);
                return;
            }

            // Read and log SQL content safely
            try (BufferedReader reader = new BufferedReader(resource.read())) {
                if (reader == null) {
                    log.warn("Skipping SQL logging for '{}' due to previous read error.", scriptName);
                    return;
                }
                String sqlContent = reader.lines().collect(Collectors.joining("\n"));
                log.info("SQL Content of '{}':\n{}", scriptName, sqlContent);
            } catch (Exception e) {
                log.error("Error reading SQL content for {}", scriptName, e);
            }
        } catch (Exception e) {
            log.error("Unexpected error while processing SQL logging for {}", scriptName, e);
        }
    }

    @Override
    public String getCallbackName() {
        return "FlywaySQLLogger";
    }
}