package com.dpw.runner.shipment.services.projection;

public interface ContainerDeleteInfoProjection {

    Long getContainerId();

    String getContainerCode();

    String getContainerNumber();

    String getShipmentId();

    String getPacks();

    String getPacksType();

    class NullContainerDeleteInfoProjection implements ContainerDeleteInfoProjection {

        @Override
        public Long getContainerId() {
            return null;
        }

        @Override
        public String getContainerCode() {
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

        @Override
        public String getPacksType() {
            return null;
        }
    }

}
