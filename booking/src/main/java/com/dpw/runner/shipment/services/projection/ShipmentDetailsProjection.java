package com.dpw.runner.shipment.services.projection;

public interface ShipmentDetailsProjection {

    Integer getTenantId();

    String getHblNumber();

    String getShipmentId();

    class NullShipmentDetailsProjection implements ShipmentDetailsProjection {

        @Override
        public Integer getTenantId() {
            return null;
        }

        @Override
        public String getHblNumber() {
            return null;
        }

        @Override
        public String getShipmentId() {
            return null;
        }

    }
}