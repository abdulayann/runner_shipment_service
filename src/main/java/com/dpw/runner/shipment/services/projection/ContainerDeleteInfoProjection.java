package com.dpw.runner.shipment.services.projection;

public interface ContainerDeleteInfoProjection {

    Long getContainerId();

    String getContainerNumber();

    String getShipmentId();

    String getPacks();

    class NullContainerDeleteInfoProjection implements ContainerDeleteInfoProjection {

        @Override
        public Long getContainerId() {
            return null;
        }

        @Override
        public String getContainerNumber() {
            return null;
        }

        @Override
        public String getShipmentId() {
            return null;
        }

        @Override
        public String getPacks() {
            return null;
        }
    }

}
