package com.dpw.runner.shipment.services.projection;

public interface ConsolidationDetailsProjection {
    Integer getTenantId();

    String getConsolidationNumber();

    String getMawb();

    String getBol();

    class NullConsolidationDetailsProjection implements ConsolidationDetailsProjection {
        @Override
        public Integer getTenantId() {
            return null;
        }

        @Override
        public String getConsolidationNumber() {
            return null;
        }

        @Override
        public String getMawb() {
            return null;
        }

        @Override
        public String getBol() {
            return null;
        }
    }
}