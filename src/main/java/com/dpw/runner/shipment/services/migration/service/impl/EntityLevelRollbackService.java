package com.dpw.runner.shipment.services.migration.service.impl;



import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.sql.DataSource;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Slf4j
@Service
@Generated
@SuppressWarnings("all")
@EnableAsync
public class EntityLevelRollbackService {


    @PersistenceContext
    private EntityManager entityManager;

    public List<?> listSchemas() {
        return entityManager.createNativeQuery("SELECT schema_name FROM information_schema.schemata")
                .getResultList();
    }

    @Autowired
    private DataSource dataSource;

    @Autowired
    private JdbcTemplate jdbcTemplate;
    private static final Pattern SCHEMA = Pattern.compile("^[A-Za-z0-9_]{1,64}$");
    private static final Pattern TENANT = Pattern.compile("^[A-Za-z0-9_\\-]{1,128}$");

    @Transactional(rollbackFor = Exception.class)
    @Async
    public void executeSqlFromFile(String tenantId, String schema) {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(Objects.requireNonNull(getClass().getResourceAsStream("/db/migration/entity_to_entity_migration.sql")))
        )) {

            String sql = reader.lines().collect(Collectors.joining("\n"));
            for (String statement : sql.split(";")) {
                if (!statement.trim().isEmpty()) {
                    if (tenantId != null && schema != null) {

                        requireValid(schema, SCHEMA, "schema");
                        requireValid(tenantId, TENANT, "tenantId");

                        String parsed = statement
                                .replace("__TENANT_ID__",tenantId )
                                .replace("__SCHEMA__", schema);

                        log.info("Executing: {}", LoggerHelper.sanitizeForLogs(parsed));
                        jdbcTemplate.execute(parsed);

                    } else {
                        log.error("❌ tenantId and schema cannot be null");
                    }
                }
            }
            log.info("✅ SQL script executed successfully.");
        } catch (Exception e) {
            log.error("❌ Error executing SQL script", e);
            throw new RuntimeException(e);
        }
    }

    @Transactional(rollbackFor = Exception.class)
    @Async
    public String backupEntity(Integer tenantId) {
        try (
                BufferedReader reader = new BufferedReader(
                        new InputStreamReader(Objects.requireNonNull(getClass().getResourceAsStream("/db/migration/backup_schema.sql")))
                );
                Connection conn = dataSource.getConnection();
                Statement stmt = conn.createStatement()
        ) {
            // Read the full DO $$ ... $$ block as a single statement
            String sql = reader.lines().collect(Collectors.joining("\n")).trim();

            String schema = String.format("public_%s_%s", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyddMM_HHmm")), tenantId);
            if (!sql.isEmpty()) {
                sql = sql.replaceAll("__SCHEMA__", schema);
                log.info("Executing PL/pgSQL block:\n{}", sql);
                stmt.execute(sql);  // Execute the full DO block
            }

            log.info("✅ PL/pgSQL script executed successfully");
            return schema;
        } catch (Exception e) {
            log.error("❌ Error executing PL/pgSQL script", e);
            throw new RuntimeException(e); // Triggers rollback
        }
    }

    private static void requireValid(String val, Pattern p, String name) {
        if (!p.matcher(val).matches())
            throw new IllegalArgumentException("Invalid " + name);
    }

}
