package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

public class IgnoreAutoTenantPopulationContext {

    private static final ThreadLocal<Boolean> ignoreAutoTenantPopulation = new InheritableThreadLocal<>();

    private IgnoreAutoTenantPopulationContext() {
    }

    public static Boolean getContext() {
        return Boolean.TRUE.equals(ignoreAutoTenantPopulation.get());
    }

    public static void setContext(Boolean flag) {
        ignoreAutoTenantPopulation.set(flag);
    }

    public static void clearContext() {
        ignoreAutoTenantPopulation.remove();
    }

}