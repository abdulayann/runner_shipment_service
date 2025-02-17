package com.dpw.runner.shipment.services.dto.trackingservice;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@JsonInclude(JsonInclude.Include.ALWAYS)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UniversalTrackingPayload {
    private String carrier;
    private String masterBill;
    private String bookingReferenceNumber;
    private String runnerReferenceNumber;
    private String referenceNumberType;
    private String entityType;
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<EntityDetail> entityDetails;
    private List<ShipmentDetail> shipmentDetails;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class UniversalEventsPayload {
        private String bookingReferenceNumber;
        private String runnerReferenceNumber;
        private String referenceNumberType;
        private List<EventDetail> events;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class EventDetail {
        private Long id;
        private String eventCode;
        private String description;
        private LocalDateTime estimated;
        private LocalDateTime actual;
        private String placeName;
    }

    @Data
    public static class ContainerDet {
        private String containerNumber;
        @JsonProperty("CreatedDate")
        private LocalDateTime CreatedDate;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class EntityDetail {
        private String trackingNumber;
        private String allocationDate;
        private BigDecimal grossWeight;
        private String grossWeightUom;
        private Boolean isEmpty;
        private Boolean isReefer;
        private Boolean isShipperOwned;
        private String containerTypeCode;
        private String mode;
        private Long containerCount;
        private String descriptionOfGoods;
        private Long noofPackages;
        private BigDecimal netWeight;
        private String netWeightUom;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ShipmentDetail {
        private String originName;
        private String destinationName;
        private String destinationCountry;
        private String originPortCode;
        private String destinationPortCode;
        private String serviceMode;
        private String estimatedPickupDate;
        private String bookingCreationDate;
        private String houseBill;
        private String shipmentType;
        private String countryCode;
    }
}
