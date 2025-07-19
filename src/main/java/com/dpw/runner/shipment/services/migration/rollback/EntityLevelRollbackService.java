package com.dpw.runner.shipment.services.migration.rollback;


import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
@Service
public class EntityLevelRollbackService {


    @PersistenceContext
    private EntityManager entityManager;

    public List<?> listSchemas() {
        return entityManager.createNativeQuery("SELECT schema_name FROM information_schema.schemata")
                .getResultList();
    }

    @Transactional(rollbackFor = Exception.class)
    public void executeSqlFromFile(String tenantId, String schema) {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(Objects.requireNonNull(getClass().getResourceAsStream("/db/migration/entity_to_entity_migration.sql"))))) {
            String sql = reader.lines().collect(Collectors.joining("\n"));
            for (String statement : sql.split(";")) {
                if (!statement.trim().isEmpty()) {
                    log.info("Executing Statement {}", statement);
                    statement = statement.replaceAll("__TENANT_ID__", tenantId).replaceAll("__SCHEMA__", schema);
                    entityManager.createNativeQuery(statement).executeUpdate();
                }
            }
            System.out.println("SQL script executed successfully.");
        } catch (Exception e) {
            log.error("failing to restore ", e);
            throw new RuntimeException(e);
        }
    }
}
