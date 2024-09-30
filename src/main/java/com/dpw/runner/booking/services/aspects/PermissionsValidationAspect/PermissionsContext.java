package com.dpw.runner.booking.services.aspects.PermissionsValidationAspect;

import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.booking.services.commons.constants.Constants.*;

@Component
public class PermissionsContext {
    private PermissionsContext(){}
    private static ThreadLocal<Map<String,List<String>>> Permissions = new InheritableThreadLocal<>();

    public static List<String> getPermissions(String key) {
        return Permissions.get().get(key);
    }

    public static void setPermissions(List<String> UserPermissions) {
        List<String> carrierBookingCreate = new ArrayList<>();
        List<String> carrierBookingView = new ArrayList<>();

        for (String permission : UserPermissions) {
            if(permission.equals(CARRIER_BOOKING_CREATE))
                carrierBookingCreate.add(CARRIER_BOOKING_CREATE);
            if(permission.equals(CARRIER_BOOKING_VIEW))
                carrierBookingView.add(CARRIER_BOOKING_VIEW);
        }

        Permissions.set(Map.ofEntries(
                Map.entry(CARRIER_BOOKING_CREATE, carrierBookingCreate),
                Map.entry(CARRIER_BOOKING_VIEW, carrierBookingView)
        ));
    }
    public static void removePermissions(){
        Permissions.remove();
    }

}