package com.dpw.runner.shipment.services.dto.trackingservice;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TrackingServiceLiteContainerResponse {

    private List<LiteContainer> containers;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class LiteContainer {

        private String containerNumber;
        private String identifierType;
        private String identifierValue;
        private String type;
        private String size;
        private String typeIsoCode;
        private Object bolNumber;
        private Object bookingNumber;
        private Object sealNumber;
        private Object marks;
        private Object incoterm;
        private Object shipper;
        private Object consignee;
        private String weight;
        private String weightUom;
        private String numberOfPackages;
        private String packageType;
        private Object reeferTemperature;
        private Object commodity;
        private Object latitude;
        private Object longitude;
        private Object location;
        private Object locationUpdateTime;
    }

}
