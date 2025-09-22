package com.dpw.runner.shipment.services.projection;

public interface ShippingConsoleNoProjection {
    Long getId();

    String getEntityId();

    class NullShippingConsoleNoProjection implements ShippingConsoleNoProjection {

        @Override
        public Long getId() {
            return null;
        }

        @Override
        public String getEntityId() {
            return null;
        }

    }
}
