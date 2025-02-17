package com.dpw.runner.shipment.services.projection;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public interface ShipmentDetailsProjection {

    Integer getTenantId();

    String getHblNumber();

    String getShipmentId();

    String getShipmentType(); // shipment_type

    String getTransportMode(); // transport_mode

    Long getId();

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

    }
}