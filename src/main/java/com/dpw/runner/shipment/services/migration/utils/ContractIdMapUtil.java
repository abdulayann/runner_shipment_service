package com.dpw.runner.shipment.services.migration.utils;

import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
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
    private static final String path = "src/main/java/com/dpw/runner/shipment/services/migration/utils/ContractIdsJsonMap.json";

    public ContractIdMapUtil() throws IOException {
        Map<String, Map<String, String>> tempMap = new HashMap<>();
        try {
            tempMap = objectMapper.readValue(
                    new File(path),
                    new TypeReference<Map<String, Map<String, String>>>() {}
            );
        } catch (IOException e) {
            log.error("Failed to load contract map from JSON file: {}", path, e);
            // Optionally: throw new RuntimeException(e);
        }
        contractMap = tempMap;
    }

    public String getParentContractId(String contractId, String contractType, String currentEnvironment) {
        if (contractId == null || contractType == null || currentEnvironment == null) {
            return null;
        }

        String env = currentEnvironment.trim().toUpperCase();
        String type = contractType.trim().toUpperCase();
        String key = env + "_" + type;

        // First check AppConfig
        String appConfigParentContractId = getParentContractIdFromAppConfig(contractId, env, type);
        if (appConfigParentContractId != null) {
            return appConfigParentContractId;
        }

        // Fallback to local JSON
        Map<String, String> innerContractMap = contractMap.get(key);
        if (innerContractMap == null) {
            throw new IllegalArgumentException("No contract map found for key: " + key);
        }
        return innerContractMap.get(contractId);
    }

    private String getParentContractIdFromAppConfig(String contractId, String env, String type) {
        String appConfigContractMapString = applicationConfigService.getValue(env + "_" + type);
        if (appConfigContractMapString == null || appConfigContractMapString.isBlank()) {
            return null;
        }

        try {
            Map<String, String> appConfigContractMap = objectMapper.readValue(
                    appConfigContractMapString,
                    new TypeReference<Map<String, String>>() {}
            );
            return appConfigContractMap.get(contractId);
        } catch (JsonProcessingException e) {
            log.error("Failed to parse contract map from AppConfig for key: {}_{}", env, type, e);
            return null;
        }
    }
}
