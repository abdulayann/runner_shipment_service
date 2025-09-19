package com.dpw.runner.shipment.services.projection;

public interface ShippingConsoleIdProjection {
    Long getId();

    Long getEntityId();

    class NullShippingConsoleNoProjection implements ShippingConsoleIdProjection {

        @Override
        public Long getId() {
            return null;
        }

        @Override
        public Long getEntityId() {
            return null;
        }

    }
}
