package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructCriteria;

/**
 * Util class for leveraging common methods related to permissions
 * Creating filter criteria from input list of permission
 */
public class PermissionUtil {
    private PermissionUtil(){}

    /**
     * We receive LIST_PERMISSION set of the user permissions , since the permission structure have been updated now
     * the system only has VIEW permission as far as this method is concerned;
     * Internally we are bifurcating that view permission into List and Retrieve
     * @param permissionList : VIEW permissions from user token
     * @param isShipment : boolean value for identifying shipment or consolidation
     * @return List<FilterCriteria> based upon available view permission
     */
    public static List<FilterCriteria> generateFilterCriteriaFromPermissions(List<String> permissionList, Boolean isShipment) {

        List<FilterCriteria> criterias = new ArrayList<>();
        HashSet<String> permissionSet = new HashSet<>();

        List<String> mappedPermission = V1PermissionMapUtil.getPermissionNames(permissionList);

        for (String v1MappedPermission : mappedPermission) {
            List<FilterCriteria> innerFilters = new ArrayList<>();
            HashMap<String, String> criteriaMap = new HashMap<>();
            if(v1MappedPermission == null)
                continue;

            // De-construct permission string into individual elements and strip the last element
            List<String> parameterList = Arrays.stream(v1MappedPermission.toLowerCase().split(DELIMITER))
                    .filter(e -> !e.contains("list"))
                    .toList();
            String transportMode = getParameterFromPermission(TRANSPORT_MODE_INDEX, parameterList);
            String direction = getParameterFromPermission(DIRECTION_INDEX, parameterList);
            String shipmentType = getParameterFromPermission(SHIPMENT_TYPE_INDEX, parameterList);
            String domesticType = getParameterFromPermission(IS_DOMESTIC_INDEX, parameterList);
            criteriaMap.put(TRANSPORT_MODE, transportMode);
            populateCriteriaMapForDirectionAndShipmentType(isShipment, criteriaMap, direction, shipmentType);
            criteriaMap.put(IS_DOMESTIC, domesticType);
            HashMap<String, Boolean> criteriaAppenderMap = checkCriteriaAppender(criteriaMap, permissionSet, isShipment);

            if (!transportMode.equals(ALL)) {
                prepareCriteriaForSpecificTransportMode(isShipment, criteriaAppenderMap, innerFilters, transportMode, direction, shipmentType, permissionSet);
            } else {
                // Transport mode : ALL ( example ImportShipmentList || AllShipmentList )
                prepareCriteriaForAllTransportMode(isShipment, direction, innerFilters, shipmentType, permissionSet);
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

    private static void populateCriteriaMapForDirectionAndShipmentType(Boolean isShipment, HashMap<String, String> criteriaMap, String direction, String shipmentType) {
        if(Boolean.TRUE.equals(isShipment))
            criteriaMap.put(DIRECTION, direction);
        else
            criteriaMap.put(SHIPMENT_TYPE, direction);
        if(Boolean.TRUE.equals(isShipment))
            criteriaMap.put(SHIPMENT_TYPE, shipmentType);
        else
            criteriaMap.put(CONTAINER_CATEGORY, shipmentType);
    }

    private static void prepareCriteriaForSpecificTransportMode(Boolean isShipment, HashMap<String, Boolean> criteriaAppenderMap, List<FilterCriteria> innerFilters, String transportMode, String direction, String shipmentType, HashSet<String> permissionSet) {
        if (Boolean.TRUE.equals(criteriaAppenderMap.get(TRANSPORT_MODE)))
            innerFilters.add(constructCriteria(TRANSPORT_MODE, transportMode, "=", null));

        if (!direction.equals(ALL)) {
            if(Boolean.TRUE.equals(isShipment))
            {
                if (Boolean.TRUE.equals(criteriaAppenderMap.get(DIRECTION)))
                    innerFilters.add(constructCriteria(DIRECTION, direction, "=", "and"));
            }
            else
            {
                if(Boolean.TRUE.equals(criteriaAppenderMap.get(SHIPMENT_TYPE)))
                    innerFilters.add(constructCriteria(SHIPMENT_TYPE, direction, "=", "and"));
            }

            prepareCriteriaForShipmentType(isShipment, criteriaAppenderMap, innerFilters, transportMode, direction, shipmentType, permissionSet);
        } else {
            prepareCriteriaForAllDirection(isShipment, shipmentType, innerFilters, permissionSet, transportMode);
        }
    }

    private static void prepareCriteriaForShipmentType(Boolean isShipment, HashMap<String, Boolean> criteriaAppenderMap, List<FilterCriteria> innerFilters, String transportMode, String direction, String shipmentType, HashSet<String> permissionSet) {
        if (!shipmentType.equals(ALL)) {
            if(Boolean.TRUE.equals(isShipment))
            {
                if (Boolean.TRUE.equals(criteriaAppenderMap.get(SHIPMENT_TYPE)))
                    innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", "and"));
            }
            else
            {
                if (Boolean.TRUE.equals(criteriaAppenderMap.get(CONTAINER_CATEGORY)))
                    innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", "and"));
            }
        } else {
            permissionSet.add(transportMode + direction + "*");
        }
    }

    private static void prepareCriteriaForAllDirection(Boolean isShipment, String shipmentType, List<FilterCriteria> innerFilters, HashSet<String> permissionSet, String transportMode) {
        if (!shipmentType.equals(ALL)) {
            if(Boolean.TRUE.equals(isShipment))
                innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", "and"));
            else
                innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", "and"));
        }
        else {
            permissionSet.add(transportMode + "*");
        }
    }

    private static void prepareCriteriaForAllTransportMode(Boolean isShipment, String direction, List<FilterCriteria> innerFilters, String shipmentType, HashSet<String> permissionSet) {
        if(!direction.equals(ALL)) {
            if(Boolean.TRUE.equals(isShipment))
                innerFilters.add(constructCriteria(DIRECTION, direction, "=", null));
            else
                innerFilters.add(constructCriteria(SHIPMENT_TYPE, direction, "=", null));
            if(!shipmentType.equals(ALL)){
                if(Boolean.TRUE.equals(isShipment))
                    innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", null));
                else
                    innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", null));
            }
        } else if(!shipmentType.equals(ALL)){
            if(Boolean.TRUE.equals(isShipment))
                innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", null));
            else
                innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", null));
        } else {
            permissionSet.add("*");
        }
    }

    private static HashMap<String, Boolean> checkCriteriaAppender(HashMap<String, String> input, HashSet<String> permissionSet, Boolean isShipment) {
        // This will tell us level wise whether to add permissions for current permission object
        // In case of validating isDomestic field, criteriaAppenderMap is explicitly set to true for levels
        // so that we don't miss any filter criteria.
        HashMap<String, Boolean> criteriaAppenderMap = (HashMap<String, Boolean>) input.entrySet()
                .stream()
                .collect(Collectors.toMap(e -> e.getKey(),
                        e -> false));

        String level0 = input.get(TRANSPORT_MODE);
        String level1 = null;
        if(Boolean.TRUE.equals(isShipment))
            level1 = level0.concat(input.get(DIRECTION));
        else
            level1 = level0.concat(input.get(SHIPMENT_TYPE));
        String level2 = null;
        if(Boolean.TRUE.equals(isShipment))
            level2 = level1.concat(input.get(SHIPMENT_TYPE));
        else
            level2 = level1.concat(input.get(CONTAINER_CATEGORY));
        boolean isDomesticPresent = !input.get(IS_DOMESTIC).equals(ALL);

        if (!permissionSet.contains("*") && (!permissionSet.contains(level0 + "*") || isDomesticPresent)) {
            populateCriteriaAppenderMap(permissionSet, isShipment, criteriaAppenderMap, level1, isDomesticPresent, level2);
        }

        return criteriaAppenderMap;
    }

    private static void populateCriteriaAppenderMap(HashSet<String> permissionSet, Boolean isShipment, HashMap<String, Boolean> criteriaAppenderMap, String level1, boolean isDomesticPresent, String level2) {
        criteriaAppenderMap.put(TRANSPORT_MODE, true);
        if (!permissionSet.contains(level1 + "*") || isDomesticPresent) {
            if(Boolean.TRUE.equals(isShipment))
                criteriaAppenderMap.put(DIRECTION, true);
            else
                criteriaAppenderMap.put(SHIPMENT_TYPE, true);
            if (!permissionSet.contains(level2 + "*") || isDomesticPresent) {
                if(Boolean.TRUE.equals(isShipment))
                    criteriaAppenderMap.put(SHIPMENT_TYPE, true);
                else
                    criteriaAppenderMap.put(CONTAINER_CATEGORY, true);
            }
        }
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
