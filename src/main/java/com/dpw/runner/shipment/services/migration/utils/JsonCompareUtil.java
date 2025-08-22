package com.dpw.runner.shipment.services.migration.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.flipkart.zjsonpatch.JsonDiff;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class JsonCompareUtil {

    private static final ObjectMapper mapper = new ObjectMapper();

    private JsonCompareUtil() {}

    /**
     * Compare two JSON strings and log the differences.
     */
    public static boolean compareAndLog(String jsonOld, String jsonNew) {
        try {
            JsonNode oldNode = mapper.readTree(jsonOld);
            JsonNode newNode = mapper.readTree(jsonNew);

            JsonNode diff = JsonDiff.asJson(oldNode, newNode);

            if (diff.isEmpty()) {
                log.info("No differences found between JSONs.");
                return false;
            } else {
                log.info("Differences found:\n{}", diff.toPrettyString());
                return true;
            }
        } catch (Exception e) {
            log.error("Error while comparing JSONs", e);
            return false;
        }
    }
}
