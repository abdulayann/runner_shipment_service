package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@Component
@SuppressWarnings("java:S3008")
public class PermissionsContext {
    private PermissionsContext(){}
    private static ThreadLocal<Map<String,List<String>>> Permissions = new InheritableThreadLocal<>();

    public static List<String> getPermissions(String key) {
        return Permissions.get().get(key);
    }

    public static void setPermissions(List<String> userPermissions) {
        List<String> shipmentListPermission = new ArrayList<>();
        List<String> shipmentRetrievePermission = new ArrayList<>();
        List<String> shipmentCreatePermission = new ArrayList<>();
        List<String> shipmentUpdatePermission = new ArrayList<>();
        List<String> shipmentCancelPermission = new ArrayList<>();
        List<String> consolidationListPermission = new ArrayList<>();
        List<String> consolidationRetrievePermission = new ArrayList<>();
        List<String> consolidationCreatePermission = new ArrayList<>();
        List<String> consolidationUpdatePermission = new ArrayList<>();
        List<String> consolidationCancelPermission = new ArrayList<>();
        List<String> carrierBookingCreate = new ArrayList<>();
        List<String> carrierBookingView = new ArrayList<>();

        for (String permission : userPermissions) {
            // Older permission context setting
            setShipmentPermissions(permission, shipmentListPermission, shipmentRetrievePermission, shipmentCreatePermission, shipmentUpdatePermission);
            setConsolidationPermissions(permission, consolidationListPermission, consolidationRetrievePermission, consolidationCreatePermission, consolidationUpdatePermission);
            setCarrierBookingPermissions(permission, carrierBookingCreate, carrierBookingView);


            // context setup for new permissions
            if(permission.startsWith("Operation")) {
                // Shipment permissions grouping
                populatePermissionList(shipmentListPermission, shipmentRetrievePermission, shipmentCreatePermission, shipmentUpdatePermission, shipmentCancelPermission, permission, SHIPMENTS_PERMISSION_KEY);
                // Consolidation permissions grouping
                populatePermissionList(consolidationListPermission, consolidationRetrievePermission, consolidationCreatePermission, consolidationUpdatePermission, consolidationCancelPermission, permission, CONSOLIDATIONS_PERMISSION_KEY);
            }
        }

        Permissions.set(Map.ofEntries(
                Map.entry(SHIPMENT_LIST_PERMISSION, shipmentListPermission),
                Map.entry(SHIPMENT_RETRIEVE_PERMISSION, shipmentRetrievePermission),
                Map.entry(SHIPMENT_CREATE_PERMISSION, shipmentCreatePermission),
                Map.entry(SHIPMENT_UPDATE_PERMISSION, shipmentUpdatePermission),
                Map.entry(SHIPMENT_CANCEL_PERMISSION, shipmentCancelPermission),

                Map.entry(CONSOLIDATION_LIST_PERMISSION, consolidationListPermission),
                Map.entry(CONSOLIDATION_RETRIEVE_PERMISSION, consolidationRetrievePermission),
                Map.entry(CONSOLIDATION_CREATE_PERMISSION, consolidationCreatePermission),
                Map.entry(CONSOLIDATION_UPDATE_PERMISSION, consolidationUpdatePermission),
                Map.entry(CONSOLIDATION_CANCEL_PERMISSION, consolidationCancelPermission),

                Map.entry(CARRIER_BOOKING_CREATE, carrierBookingCreate),
                Map.entry(CARRIER_BOOKING_VIEW, carrierBookingView)
        ));
    }

    private static void setCarrierBookingPermissions(String permission, List<String> carrierBookingCreate, List<String> carrierBookingView) {
        if(permission.equals(CARRIER_BOOKING_CREATE))
            carrierBookingCreate.add(CARRIER_BOOKING_CREATE);
        if(permission.equals(CARRIER_BOOKING_VIEW))
            carrierBookingView.add(CARRIER_BOOKING_VIEW);
    }

    private static void setConsolidationPermissions(String permission, List<String> consolidationListPermission, List<String> consolidationRetrievePermission, List<String> consolidationCreatePermission, List<String> consolidationUpdatePermission) {
        if(permission.endsWith(CONSOLIDATION_LIST_PERMISSION))
            consolidationListPermission.add(permission);
        if(permission.endsWith(CONSOLIDATION_RETRIEVE_PERMISSION))
            consolidationRetrievePermission.add(permission);
        if(permission.endsWith(CONSOLIDATION_CREATE_PERMISSION))
            consolidationCreatePermission.add(permission);
        if(permission.endsWith(CONSOLIDATION_UPDATE_PERMISSION))
            consolidationUpdatePermission.add(permission);
    }

    private static void setShipmentPermissions(String permission, List<String> shipmentListPermission, List<String> shipmentRetrievePermission, List<String> shipmentCreatePermission, List<String> shipmentUpdatePermission) {
        if(permission.endsWith(SHIPMENT_LIST_PERMISSION))
            shipmentListPermission.add(permission);
        if(permission.endsWith(SHIPMENT_RETRIEVE_PERMISSION))
            shipmentRetrievePermission.add(permission);
        if(permission.endsWith(SHIPMENT_CREATE_PERMISSION))
            shipmentCreatePermission.add(permission);
        if(permission.endsWith(SHIPMENT_UPDATE_PERMISSION))
            shipmentUpdatePermission.add(permission);
    }

    /**
     *
     * @param listPermissionList : ListPermission Collection
     * @param retrievePermissionList : RetrievePermission Collection
     * @param createPermissionList : CreatePermission Collection
     * @param updatePermissionList : UpdatePermission Collection
     * @param cancelPermissionList : CancelPermission Collection
     * @param userPermission : current permission that needs to be evaluated
     * @param entity : SHIPMENT or CONSOLIDATION
     */
    private static void populatePermissionList(List<String> listPermissionList, List<String> retrievePermissionList, List<String> createPermissionList, List<String> updatePermissionList, List<String> cancelPermissionList, String userPermission, String entity) {
        if(userPermission.endsWith(VIEW_PERMISSION) && userPermission.contains(entity)) {
            listPermissionList.add(userPermission);
            retrievePermissionList.add(userPermission);
        }
        if(userPermission.endsWith(MODIFY_PERMISSION) && userPermission.contains(entity)) {
            updatePermissionList.add(userPermission);
            listPermissionList.add(userPermission);
            retrievePermissionList.add(userPermission);
        }
        if(userPermission.endsWith(CANCEL_PERMISSION) && userPermission.contains(entity)) {
            cancelPermissionList.add(userPermission);
            listPermissionList.add(userPermission);
            retrievePermissionList.add(userPermission);
        }
        if(userPermission.endsWith(CREATE_PERMISSION) && userPermission.contains(entity)) {
            createPermissionList.add(userPermission);
            updatePermissionList.add(userPermission);
            listPermissionList.add(userPermission);
            retrievePermissionList.add(userPermission);
        }
    }

    public static void removePermissions(){
        Permissions.remove();
    }

}