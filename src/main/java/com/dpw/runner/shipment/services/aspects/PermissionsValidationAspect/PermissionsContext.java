package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;

import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@Component
public class PermissionsContext {
    private static ThreadLocal<Map<String,List<String>>> Permissions = new InheritableThreadLocal<>();

    public static List<String> getPermissions(String key) {
        return Permissions.get().get(key);
    }

    public static void setPermissions(List<String> UserPermissions) {
        List<String> shipmentListPermission = new ArrayList<>();
        List<String> shipmentRetrievePermission = new ArrayList<>();
        List<String> consolidationListPermission = new ArrayList<>();
        List<String> consolidationRetrievePermission = new ArrayList<>();

        for (String permission : UserPermissions) {
            if(permission.endsWith(SHIPMENT_LIST_PERMISSION))
                shipmentListPermission.add(permission);
            if(permission.endsWith(SHIPMENT_RETRIEVE_PERMISSION))
                shipmentRetrievePermission.add(permission);
            if(permission.endsWith(CONSOLIDATION_LIST_PERMISSION))
                consolidationListPermission.add(permission);
            if(permission.endsWith(CONSOLIDATION_RETRIEVE_PERMISSION))
                consolidationRetrievePermission.add(permission);
        }

        Permissions.set(Map.ofEntries(
                Map.entry(SHIPMENT_LIST_PERMISSION, shipmentListPermission),
                Map.entry(SHIPMENT_RETRIEVE_PERMISSION, shipmentRetrievePermission),
                Map.entry(CONSOLIDATION_LIST_PERMISSION, consolidationListPermission),
                Map.entry(CONSOLIDATION_RETRIEVE_PERMISSION, consolidationRetrievePermission)
        ));
    }
    public static void removePermissions(){
        Permissions.remove();
    }

}