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
            List<FilterCriteria> innerFilters = new ArrayList();
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
            if(isShipment)
                criteriaMap.put(DIRECTION, direction);
            else
                criteriaMap.put(SHIPMENT_TYPE, direction);
            if(isShipment)
                criteriaMap.put(SHIPMENT_TYPE, shipmentType);
            else
                criteriaMap.put(CONTAINER_CATEGORY, shipmentType);
            criteriaMap.put(IS_DOMESTIC, domesticType);
            HashMap<String, Boolean> criteriaAppenderMap = checkCriteriaAppender(criteriaMap, permissionSet, isShipment);

            if (!transportMode.equals(ALL)) {
                if (criteriaAppenderMap.get(TRANSPORT_MODE))
                    innerFilters.add(constructCriteria(TRANSPORT_MODE, transportMode, "=", null));

                if (!direction.equals(ALL)) {
                    if(isShipment)
                    {
                        if (criteriaAppenderMap.get(DIRECTION))
                            innerFilters.add(constructCriteria(DIRECTION, direction, "=", "and"));
                    }
                    else
                    {
                        if(criteriaAppenderMap.get(SHIPMENT_TYPE))
                            innerFilters.add(constructCriteria(SHIPMENT_TYPE, direction, "=", "and"));
                    }

                    if (!shipmentType.equals(ALL)) {
                        if(isShipment)
                        {
                            if (criteriaAppenderMap.get(SHIPMENT_TYPE))
                                innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", "and"));
                        }
                        else
                        {
                            if (criteriaAppenderMap.get(CONTAINER_CATEGORY))
                                innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", "and"));
                        }
                    } else {
                        permissionSet.add(transportMode + direction + "*");
                    }
                } else {
                    if (!shipmentType.equals(ALL)) {
                        if(isShipment)
                            innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", "and"));
                        else
                            innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", "and"));
                    }
                    else {
                        permissionSet.add(transportMode + "*");
                    }
                }
            } else {
                // Transport mode : ALL ( example ImportShipmentList || AllShipmentList )
                if(!direction.equals(ALL)) {
                    if(isShipment)
                        innerFilters.add(constructCriteria(DIRECTION, direction, "=", null));
                    else
                        innerFilters.add(constructCriteria(SHIPMENT_TYPE, direction, "=", null));
                    if(!shipmentType.equals(ALL)){
                        if(isShipment)
                            innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", null));
                        else
                            innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", null));
                    }
                } else if(!shipmentType.equals(ALL)){
                    if(isShipment)
                        innerFilters.add(constructCriteria(SHIPMENT_TYPE, shipmentType, "=", null));
                    else
                        innerFilters.add(constructCriteria(CONTAINER_CATEGORY, shipmentType, "=", null));
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
        if(isShipment)
            level1 = level0.concat(input.get(DIRECTION));
        else
            level1 = level0.concat(input.get(SHIPMENT_TYPE));
        String level2 = null;
        if(isShipment)
            level2 = level1.concat(input.get(SHIPMENT_TYPE));
        else
            level2 = level1.concat(input.get(CONTAINER_CATEGORY));
        boolean isDomesticPresent = !input.get(IS_DOMESTIC).equals(ALL);

        if (!permissionSet.contains("*")) {
            if (!permissionSet.contains(level0 + "*") || isDomesticPresent) {
                criteriaAppenderMap.put(TRANSPORT_MODE, true);
                if (!permissionSet.contains(level1 + "*") || isDomesticPresent) {
                    if(isShipment)
                        criteriaAppenderMap.put(DIRECTION, true);
                    else
                        criteriaAppenderMap.put(SHIPMENT_TYPE, true);
                    if (!permissionSet.contains(level2 + "*") || isDomesticPresent) {
                        if(isShipment)
                            criteriaAppenderMap.put(SHIPMENT_TYPE, true);
                        else
                            criteriaAppenderMap.put(CONTAINER_CATEGORY, true);
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
