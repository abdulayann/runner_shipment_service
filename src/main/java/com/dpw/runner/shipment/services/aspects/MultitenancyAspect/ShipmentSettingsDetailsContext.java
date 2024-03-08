package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.stereotype.Component;

@Component
public class ShipmentSettingsDetailsContext {
    private ShipmentSettingsDetailsContext(){}

    private static ThreadLocal<ShipmentSettingsDetails> tenantSettingsThreadLocal =
      new InheritableThreadLocal<>();

    public static ShipmentSettingsDetails getCurrentTenantSettings() {
        return tenantSettingsThreadLocal.get();
    }

    public static void setCurrentTenantSettings(ShipmentSettingsDetails shipmentSettingsDetails) {
         tenantSettingsThreadLocal.set(shipmentSettingsDetails);
    }

    public static void remove() {
        tenantSettingsThreadLocal.remove();
    }

}
