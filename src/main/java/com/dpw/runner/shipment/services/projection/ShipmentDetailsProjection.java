package com.dpw.runner.shipment.services.projection;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public interface ShipmentDetailsProjection {

    Integer getTenantId();
    String getHblNumber();
    String getShipmentId();
    String getShipmentType();
    String getTransportMode();
    Long getId();
    String getShipmentNumber();
    Long getContainerId();

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

        @Override
        public String getShipmentType() {
            return null;
        }

        @Override
        public String getTransportMode() {
            return null;
        }

        @Override
        public Long getId() {
            return null;
        }

        @Override
        public String getShipmentNumber() {
            return null;
        }

        @Override
        public Long getContainerId() {
            return null;
        }

    }
}