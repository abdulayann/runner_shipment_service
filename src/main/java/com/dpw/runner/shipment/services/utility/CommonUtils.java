package com.dpw.runner.shipment.services.utility;

import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utility.Constants.*;

public class CommonUtils {

    private static final Logger LOG = LoggerFactory.getLogger(CommonUtils.class);
    private static final String resourcePath = String.format("%s%s", System.getProperty("user.dir"), "/src/main/resources/");
    private static ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);

    public static List<Object> getAclPermissionList() {
        File jsonInputFile = new File(String.format("%s%s", resourcePath, "acl.json"));
        List<Object> jsonData = new ArrayList<>();
        Map<String, Object> obj = new HashMap<>();
        InputStream is;
        LOG.info("Loaded input file : {}", jsonInputFile);

        try {
            is = new FileInputStream(jsonInputFile);
            JsonReader reader = Json.createReader(is);
            JsonArray permission = reader.readArray();
            reader.close();
            for (int i = 0; i < permission.size(); i++) {
                JsonObject temp = permission.getJsonObject(i);
                obj.put(temp.get("transportMode").toString(), temp.get("direction"));
            }

            jsonData = Arrays.asList(permission.toArray());
            LOG.info("Total permissions scanned : {}", permission.size());
        } catch (Exception e) {
            LOG.error("Failed to read input json file", e);
        }
        return jsonData;
    }

    public static JsonObject readJson(String FilePath) {
        File jsonInputFile = new File(FilePath);
        JsonObject jsonData = null;
        Map<String, Object> obj = new HashMap<>();
        InputStream is;

        try {
            is = new FileInputStream(jsonInputFile);
            JsonReader reader = Json.createReader(is);
            JsonObject object = reader.readObject();
            jsonData = object;
            reader.close();
        } catch (Exception e) {
            LOG.error("Failed to read input json file", e);
        }

        return jsonData;
    }

    public static List<FilterCriteria> generateFilterCriteriaFromPermissions(List<String> permissionList) {

        List<FilterCriteria> criterias = new ArrayList<>();
        HashSet<String> permissionSet = new HashSet<>();

        for (String permission : permissionList) {
            List<FilterCriteria> innerFilters = new ArrayList();
            JsonObject criteriaObj = readJson(String.format("%s%s", resourcePath, "permissions.json")).getJsonObject(permission);
            HashMap<String, String> criteriaMap = new HashMap<>();
            String transportMode = criteriaObj.getString(TRANSPORT_MODE);
            String direction = criteriaObj.getString(DIRECTION);
            String shipmentType = criteriaObj.getString(SHIPMENT_TYPE).toString();
            criteriaMap.put(TRANSPORT_MODE, transportMode);
            criteriaMap.put(DIRECTION, direction);
            criteriaMap.put(SHIPMENT_TYPE, shipmentType);
            HashMap<String, Boolean> criteriaAppenderMap = checkCriteriaAppender(criteriaMap, permissionSet);


            if (!transportMode.equals(ALL)) {
                if (criteriaAppenderMap.get(TRANSPORT_MODE))
                    innerFilters.add(constructCriteria(TRANSPORT_MODE, transportMode, "=", null));

                if (!direction.equals(ALL)) {
                    if (criteriaAppenderMap.get(DIRECTION))
                        innerFilters.add(constructCriteria(DIRECTION, direction, "=", "and"));

                    if (!shipmentType.equals(ALL)) {
                        if (criteriaAppenderMap.get(SHIPMENT_TYPE))
                            innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", "and"));
                    } else {
                        permissionSet.add(transportMode + direction + "*");
                    }
                } else {
                    permissionSet.add(transportMode + "*");
                }
            } else {
                permissionSet.add("*");
            }

            if (!innerFilters.isEmpty() && !permissionSet.contains("*")) {
                criterias.add(FilterCriteria.builder().innerFilter(innerFilters)
                        .logicOperator(criterias.isEmpty() ? null : "or").build());
            }
        }

        return criterias;
    }

    private static FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

    private static HashMap<String, Boolean> checkCriteriaAppender(HashMap<String, String> input, HashSet<String> permissionSet) {
        // This will tell us level wise whether to add permissions for current permission object
        HashMap<String, Boolean> criteriaAppenderMap = (HashMap<String, Boolean>) input.entrySet()
                .stream()
                .collect(Collectors.toMap(e -> e.getKey(),
                        e -> false));

        String level0 = input.get(TRANSPORT_MODE);
        String level1 = level0.concat(input.get(DIRECTION));
        String level2 = level1.concat(input.get(SHIPMENT_TYPE));

        if (!permissionSet.contains("*")) {
            if (!permissionSet.contains(level0 + "*")) {
                criteriaAppenderMap.put(TRANSPORT_MODE, true);
                if (!permissionSet.contains(level1 + "*")) {
                    criteriaAppenderMap.put(DIRECTION, true);
                    if (!permissionSet.contains(level2 + "*")) {
                        criteriaAppenderMap.put(SHIPMENT_TYPE, true);
                    }
                }
            }
        }

        return criteriaAppenderMap;
    }

}
