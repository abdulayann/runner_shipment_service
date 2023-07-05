package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

public class CommonUtils {

    private static final Logger LOG = LoggerFactory.getLogger(CommonUtils.class);
    private static final String resourcePath = String.format("%s%s", System.getProperty("user.dir"), "/src/main/resources/");

    public static List<FilterCriteria> generateFilterCriteriaFromPermissions(List<String> permissionList) {

        List<FilterCriteria> criterias = new ArrayList<>();
        HashSet<String> permissionSet = new HashSet<>();

        for (String permission : permissionList) {
            List<FilterCriteria> innerFilters = new ArrayList();
            HashMap<String, String> criteriaMap = new HashMap<>();
            // De-construct permission string into individual elements and strip the last element
            List<String> parameterList = Arrays.stream(permission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("list"))
                    .collect(Collectors.toList());
            String transportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
            String direction = getParameterFromPermission(DIRECTION_INDEX, parameterList);
            String shipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
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

    private static String getParameterFromPermission(int parameterIndex, List<String> permission) {
        String parameterValue = ALL;
        if (parameterIndex < permission.size()) {
            parameterValue = permission.get(parameterIndex);
        }
        return parameterValue;
    }
    public static ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator){
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(0);
        request.setLimit(Integer.MAX_VALUE);


        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }
}
