package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

public class ShipmentVersionContext {
    private ShipmentVersionContext() {
    }

    private static ThreadLocal<Boolean> v3Enabled = new InheritableThreadLocal<>();

    public static void markV3() {
        v3Enabled.set(true);
    }

    public static boolean isV3() {
        return v3Enabled.get() != null && v3Enabled.get();
    }
}
