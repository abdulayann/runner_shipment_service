package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.notification.service.INotificationService;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
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
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class FlywaySQLLogger implements Callback {

    private final ObjectProvider<Flyway> flywayProvider;
    private final ApplicationContext applicationContext;
    private final Environment environment;

    @Value("${flyway.logs.email.to}")
    private String toEmailsProperty;

    @Value("${flyway.logs.email.cc}")
    private String ccEmailsProperty;

    @Value("${flyway.logs.email.enabled}")
    private boolean migrationEnabled;
    @Value("${flyway.logs.email.subject}")
    private String mailSubject;

    @Value("${current.sprint.name}")
    private String currentSprint;

    public FlywaySQLLogger(ObjectProvider<Flyway> flywayProvider, ApplicationContext applicationContext, Environment environment) {
        this.flywayProvider = flywayProvider;
        this.applicationContext = applicationContext;
        this.environment = environment;
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

        if (!migrationEnabled) {
            log.info("Flyway migration email logging is disabled for this environment.");
            return;
        }

        log.info("========== Checking Pending Flyway Migrations ==========");

        Flyway flyway = flywayProvider.getIfAvailable();
        if (flyway == null) {
            log.warn("Flyway bean is not available. Skipping SQL logging.");
            return;
        }

        try {
            MigrationInfo[] pendingMigrations = flyway.info().pending();
            if (pendingMigrations.length == 0) {
                log.info("No pending migrations found.");
                return;
            }

            INotificationService notificationService = applicationContext.getBean(INotificationService.class);
            List<String> toEmails = new ArrayList<>(Arrays.asList(toEmailsProperty.split(",")));
            List<String> ccEmails = new ArrayList<>(Arrays.asList(ccEmailsProperty.split(",")));

            String activeProfiles = String.join(", ", environment.getActiveProfiles());
            String environmentName = environment.getProperty("spring.application.name", "Unknown Environment");

            // Build subject line with key details
            String emailSubject = String.join(" | ",
                    mailSubject,
                    activeProfiles.toUpperCase(),
                    environmentName,
                    currentSprint
            );

            StringBuilder emailContent = new StringBuilder();
            emailContent.append("""
                        <html>
                        <head>
                            <style>
                                body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                                h3 { color: #0056b3; }
                                pre { background: #f4f4f4; padding: 10px; border-left: 3px solid #0073e6; overflow-x: auto; }
                                hr { border: 1px solid #ddd; margin: 20px 0; }
                                .container { max-width: 800px; margin: auto; padding: 20px; border: 1px solid #ddd; background: #fff; }
                                .footer { font-size: 12px; color: #777; margin-top: 20px; }
                            </style>
                        </head>
                        <body>
                            <div class='container'>
                    """);

            for (MigrationInfo migration : pendingMigrations) {
                String sqlContent = fetchSQLScript(flyway, migration.getScript());

                emailContent.append(String.format("""
                                    <h3>Migration Version: %s</h3>
                                    <p><strong>Description:</strong> %s</p>
                                    <p><strong>Script Name:</strong> %s</p>
                                    <pre>%s</pre>
                                    <hr>
                                """, migration.getVersion(), migration.getDescription(),
                        migration.getScript(), sqlContent));
            }

            emailContent.append("""
                            </div>
                        </body>
                        </html>
                    """);

            // Send email with the formatted subject and content
            notificationService.sendEmail(emailContent.toString(), emailSubject, toEmails, ccEmails);

        } catch (Exception e) {
            log.error("Error retrieving pending migrations", e);
        }

        log.info("========== Flyway Migration Execution Will Now Start ==========");
    }

    private String fetchSQLScript(Flyway flyway, String scriptName) {
        try {
            List<String> locations = Arrays.stream(flyway.getConfiguration().getLocations()).map(Location::getDescriptor).toList();

            for (String location : locations) {
                ClassPathResource resource = new ClassPathResource(location.replace("classpath:", "") + "/" + scriptName);
                if (resource.exists()) {
                    return readResourceContent(resource);
                }
            }

            log.warn("No SQL file found for migration: {}", scriptName);
            return "SQL script not found: " + scriptName;
        } catch (Exception e) {
            log.error("Unexpected error while processing SQL logging for {}", scriptName, e);
            return "Unexpected error for script: " + scriptName;
        }
    }

    private String readResourceContent(ClassPathResource resource) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(resource.getInputStream(), StandardCharsets.UTF_8))) {
            return reader.lines().collect(Collectors.joining("\n"));
        } catch (IOException e) {
            log.error("Error reading SQL content for {}", resource.getFilename(), e);
            return "Error reading SQL for: " + resource.getFilename();
        }
    }

    @Override
    public String getCallbackName() {
        return "FlywaySQLLogger";
    }
}