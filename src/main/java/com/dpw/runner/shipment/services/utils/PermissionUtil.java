package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructCriteria;
import static com.dpw.runner.shipment.services.utils.V1PermissionMapUtil.getPermissionName;

/**
 * Util class for leveraging common methods related to permissions
 * Creating filter criteria from input list of permission
 */
public class PermissionUtil {

    public static List<FilterCriteria> generateFilterCriteriaFromPermissions(List<String> permissionList) {

        List<FilterCriteria> criterias = new ArrayList<>();
        HashSet<String> permissionSet = new HashSet<>();

        for (String permission : permissionList) {
            List<FilterCriteria> innerFilters = new ArrayList();
            HashMap<String, String> criteriaMap = new HashMap<>();
            String v1MappedPermission = getPermissionName(permission);
            if(v1MappedPermission == null)
                continue;
            // De-construct permission string into individual elements and strip the last element
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("list"))
                    .collect(Collectors.toList());
            String transportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
            String direction = getParameterFromPermission(DIRECTION_INDEX, parameterList);
            String shipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
            String domesticType = getParameterFromPermission(IS_DOMESTIC_INDEX, parameterList);
            criteriaMap.put(TRANSPORT_MODE, transportMode);
            criteriaMap.put(DIRECTION, direction);
            criteriaMap.put(SHIPMENT_TYPE, shipmentType);
            criteriaMap.put(IS_DOMESTIC, domesticType);
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
                    if (!shipmentType.equals(ALL)) {
                        innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", "and"));
                    }
                    else {
                        permissionSet.add(transportMode + "*");
                    }
                }
            } else {
                // Transport mode : ALL ( example ImportShipmentList || AllShipmentList )
                if(!direction.equals(ALL)) {
                    innerFilters.add(constructCriteria(DIRECTION, direction, "=", null));
                    if(!shipmentType.equals(ALL)){
                        innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", null));
                    }
                } else if(!shipmentType.equals(ALL)){
                    innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", null));
                } else {
                    permissionSet.add("*");
                }
            }
            // Appending criteria for Domestic or International shipments
            if(domesticType.equals(DOMESTIC) || domesticType.equals(INTERNATIONAL)){
                innerFilters.add(constructCriteria(IS_DOMESTIC, domesticType.equals(DOMESTIC), "=", "and"));
            }

            if (!innerFilters.isEmpty() && !permissionSet.contains("*")) {
                criterias.add(FilterCriteria.builder().innerFilter(innerFilters)
                        .logicOperator(criterias.isEmpty() ? null : "or").build());
            }
        }

        return criterias;
    }

    private static HashMap<String, Boolean> checkCriteriaAppender(HashMap<String, String> input, HashSet<String> permissionSet) {
        // This will tell us level wise whether to add permissions for current permission object
        // In case of validating isDomestic field, criteriaAppenderMap is explicitly set to true for levels
        // so that we don't miss any filter criteria.
        HashMap<String, Boolean> criteriaAppenderMap = (HashMap<String, Boolean>) input.entrySet()
                .stream()
                .collect(Collectors.toMap(e -> e.getKey(),
                        e -> false));

        String level0 = input.get(TRANSPORT_MODE);
        String level1 = level0.concat(input.get(DIRECTION));
        String level2 = level1.concat(input.get(SHIPMENT_TYPE));
        boolean isDomesticPresent = !input.get(IS_DOMESTIC).equals(ALL);

        if (!permissionSet.contains("*")) {
            if (!permissionSet.contains(level0 + "*") || isDomesticPresent) {
                criteriaAppenderMap.put(TRANSPORT_MODE, true);
                if (!permissionSet.contains(level1 + "*") || isDomesticPresent) {
                    criteriaAppenderMap.put(DIRECTION, true);
                    if (!permissionSet.contains(level2 + "*") || isDomesticPresent) {
                        criteriaAppenderMap.put(SHIPMENT_TYPE, true);
                    }
                }
            }
        }

        return criteriaAppenderMap;
    }

    public static String getParameterFromPermission(int parameterIndex, List<String> permission) {
        // Setting Default value as all if unable to find value in permission keys
        String parameterValue = ALL;
        if (parameterIndex < permission.size()) {
            parameterValue = permission.get(parameterIndex);
        }
        return parameterValue;
    }
}
