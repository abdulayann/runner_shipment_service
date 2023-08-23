package com.dpw.runner.shipment.services.dto.request.platformBooking;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.BookingChargesRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.BookingContainerRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.BookingPackingRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.BookingRoutingsRequest;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking Request Model for platform")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class PlatformToRunnerCustomerBookingRequest implements IRunnerRequest {
    @JsonProperty("status")
    private BookingStatus bookingStatus;
    private PartiesRequest customer;
    private Boolean isCustomerFreeText;
    private PartiesRequest consignor;
    private Boolean isConsignorFreeText;
    private PartiesRequest consignee;
    private Boolean isConsigneeFreeText;
    private PartiesRequest notifyParty;
    private Boolean isNotifyPartyFreeText;
    private String customerEmail;
    private String bookingNumber;
    private LocalDateTime bookingDate;
    private String incoTerms;
    private String origin;
    private String destination;
    @JsonProperty("pol")
    private String originPort;
    @JsonProperty("pod")
    private String destinationPort;
//    private CarrierDetailRequest carrierDetails;
    private String transportType;
    private String cargoType;
    private String direction;
    private String shippingLine;
    private String vessel;
    @JsonProperty("voyageNumber")
    private String voyage;
    private Integer quantity;
    private String quantityUnit;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private BigDecimal chargeable;
    private String chargeableUnit;
    private UUID contractId;
    @JsonProperty("containers")
    private List<BookingContainerRequest> containersList;
    @JsonProperty("packs")
    private List<BookingPackingRequest> packingList;
    @JsonProperty("routings")
    private List<BookingRoutingsRequest> routingList;
    @JsonProperty("charges")
    private List<PlatformBookingChargesRequest> bookingCharges;
}
