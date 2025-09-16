package com.dpw.runner.shipment.services.migration.utils;

import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

@Service
@Slf4j
@Generated
public class ContractIdMapUtil {

    @Autowired
    private IApplicationConfigService applicationConfigService;

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private final Map<String, Map<String, String>> contractMap;
    private static final String CONTRACT_FILE_NAME = "ContractIdsJsonMap.json";

    public ContractIdMapUtil() {
        Map<String, Map<String, String>> tempMap = new HashMap<>();
        try (InputStream inputStream = new ClassPathResource(CONTRACT_FILE_NAME).getInputStream()) {
            tempMap = objectMapper.readValue(
                    inputStream,
                    new TypeReference<Map<String, Map<String, String>>>() {}
            );
        } catch (IOException e) {
            log.error("Failed to load contract map from JSON file in resources", e);
            throw new IllegalStateException("Could not load contract map", e);
        }
        contractMap = tempMap;
    }

    public String getParentContractId(String contractId, String contractType, String currentEnvironment) {
        if (Strings.isNullOrEmpty(contractId) || Strings.isNullOrEmpty(contractType) || Strings.isNullOrEmpty(currentEnvironment)) {
            return null;
        }

        String env = currentEnvironment.trim().toUpperCase();
        String type = contractType.trim().toUpperCase();
        String key = env + "_" + type;

        // First check AppConfig
        String appConfigParentContractId = getParentContractIdFromAppConfig(contractId, env, type);
        if (appConfigParentContractId != null) {
            log.info("ParentContractId fetched from AppConfig is {}", appConfigParentContractId);
            return appConfigParentContractId;
        }

        // Fallback to local JSON
        log.info("Fetching local contract config map for key "+ key);
        Map<String, String> innerContractMap = contractMap.get(key);
        if (innerContractMap != null) {
            String parentContractId = innerContractMap.get(contractId);
            log.info("ParentContractId fetched from Local contractMap is {}", parentContractId);
            return parentContractId;
        }
        log.error("No contract map found for key {} in local contract config map: ", key);
        return null;
    }

    private String getParentContractIdFromAppConfig(String contractId, String env, String type) {
        String appConfigContractMapString = applicationConfigService.getValue("CONTRACT_MAP");
        if (Strings.isNullOrEmpty(appConfigContractMapString)) {
            return null;
        }

        try {
            Map<String, Map<String, String>> appConfigContractMap = objectMapper.readValue(
                    appConfigContractMapString,
                    new TypeReference<Map<String, Map<String, String>>>() {}
            );
            String key = env + "_" + type;
            log.info("Fetching AppConfig map for key {}", key);
            Map<String, String> innerAppConfigContractMap = appConfigContractMap.get(key);
            if(innerAppConfigContractMap != null) {
                String parentContractId = innerAppConfigContractMap.get(contractId);
                log.info("ParentContractId {} fetched from AppConfig contractMap is for key {}", parentContractId, key);
                return parentContractId;
            }
            log.error("No contract map found for key {} in AppConfig contract config map: ", key);
            return null;
        } catch (Exception e) {
            log.error("Failed to fetch contract map from AppConfig: {}", e.getMessage());
            throw new IllegalArgumentException(e);
        }
    }
}
