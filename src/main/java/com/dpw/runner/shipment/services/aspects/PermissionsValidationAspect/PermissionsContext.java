package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@Component
public class PermissionsContext {
    private PermissionsContext(){}
    private static ThreadLocal<Map<String,List<String>>> Permissions = new InheritableThreadLocal<>();

    public static List<String> getPermissions(String key) {
        return Permissions.get().get(key);
    }

    public static void setPermissions(List<String> UserPermissions) {
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

        for (String permission : UserPermissions) {
            // Shipment permissions grouping
            if(permission.endsWith(SHIPMENT_LIST_PERMISSION) || (permission.endsWith(VIEW_PERMISSION) && permission.contains(SHIPMENTS))) {
                shipmentListPermission.add(permission);
            }
            if(permission.endsWith(SHIPMENT_RETRIEVE_PERMISSION) || (permission.endsWith(VIEW_PERMISSION) && permission.contains(SHIPMENTS))) {
                shipmentRetrievePermission.add(permission);
            }
            if(permission.endsWith(SHIPMENT_UPDATE_PERMISSION) || (permission.endsWith(MODIFY_PERMISSION) && permission.contains(SHIPMENTS))) {
                shipmentUpdatePermission.add(permission);
                shipmentListPermission.add(permission);
                shipmentRetrievePermission.add(permission);
            }
            if(permission.endsWith(CANCEL_PERMISSION) && permission.contains(SHIPMENTS)) {
                shipmentCancelPermission.add(permission);
                shipmentListPermission.add(permission);
                shipmentRetrievePermission.add(permission);
            }
            if(permission.endsWith(SHIPMENT_CREATE_PERMISSION) || (permission.endsWith(CREATE_PERMISSION)) && permission.contains(SHIPMENTS)) {
                shipmentCreatePermission.add(permission);
                shipmentUpdatePermission.add(permission);
                shipmentListPermission.add(permission);
                shipmentRetrievePermission.add(permission);
            }

            // Consolidation permission grouping
            if(permission.endsWith(CONSOLIDATION_LIST_PERMISSION) || permission.endsWith(VIEW_PERMISSION))
                consolidationListPermission.add(permission);
            if(permission.endsWith(CONSOLIDATION_RETRIEVE_PERMISSION) || permission.endsWith(VIEW_PERMISSION))
                consolidationRetrievePermission.add(permission);
            if(permission.endsWith(CONSOLIDATION_CREATE_PERMISSION) || permission.endsWith(CREATE_PERMISSION))
                consolidationCreatePermission.add(permission);
            if(permission.endsWith(CONSOLIDATION_UPDATE_PERMISSION) || permission.endsWith(MODIFY_PERMISSION))
                consolidationUpdatePermission.add(permission);
            if(permission.endsWith(CANCEL_PERMISSION) && permission.contains(CONSOLIDATION))
                consolidationCancelPermission.add(permission);

            // CarrierBooking permission grouping
            if(permission.equals(CARRIER_BOOKING_CREATE))
                carrierBookingCreate.add(CARRIER_BOOKING_CREATE);
            if(permission.equals(CARRIER_BOOKING_VIEW))
                carrierBookingView.add(CARRIER_BOOKING_VIEW);
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
    public static void removePermissions(){
        Permissions.remove();
    }

}