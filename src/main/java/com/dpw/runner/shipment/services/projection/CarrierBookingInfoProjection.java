package com.dpw.runner.shipment.services.projection;

import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public interface CarrierBookingInfoProjection {
    String getBookingStatus();

    String getBookingNo();
    String getSiStatus();

    class NullCarrierBookingInfoProjection implements CarrierBookingInfoProjection {

        @Override
        public String getBookingStatus() {
            return null;
        }

        @Override
        public String getBookingNo() {
            return null;
        }

        @Override
        public String getSiStatus() {
            return null;
        }

    }
}
