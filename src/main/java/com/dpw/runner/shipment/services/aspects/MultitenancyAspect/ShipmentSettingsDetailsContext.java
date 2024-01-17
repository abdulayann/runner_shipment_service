package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import java.util.Optional;

@Component
public class ShipmentSettingsDetailsContext {

    private static ThreadLocal<ShipmentSettingsDetails> tenantSettingsThreadLocal =
      new InheritableThreadLocal<>();

    public static ShipmentSettingsDetails getCurrentTenantSettings() {
        return tenantSettingsThreadLocal.get();
    }

    public static void setCurrentTenantSettings(ShipmentSettingsDetails shipmentSettingsDetails) {
         tenantSettingsThreadLocal.set(shipmentSettingsDetails);
    }

}
