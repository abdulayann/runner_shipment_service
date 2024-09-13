package com.dpw.runner.shipment.services.aspects.sync;


import java.util.Objects;

public class SyncingContext {
    private SyncingContext(){}
    private static final ThreadLocal<Boolean> currentContext = new ThreadLocal<>();

    public static Boolean getContext() {
        return Objects.isNull(currentContext.get()) ? Boolean.TRUE : currentContext.get();
    }

    public static void setContext(Boolean context) {
        currentContext.set(context);
    }

    public static void removeContext() {
        currentContext.remove();
    }
}
