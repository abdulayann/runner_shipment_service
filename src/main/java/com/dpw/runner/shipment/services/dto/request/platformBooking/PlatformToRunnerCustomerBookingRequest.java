package com.dpw.runner.shipment.services.dto.request.platformBooking;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@ApiModel("Customer Booking Request Model for platform")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class PlatformToRunnerCustomerBookingRequest implements IRunnerRequest {
    @JsonProperty("status")
    private BookingStatus bookingStatus;
    private BookingSource source;
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
    private String contractId;
    private String contractStatus;
    private Boolean isSingleUsageContract;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    @JsonProperty("containers")
    private List<BookingContainerRequest> containersList;
    @JsonProperty("packs")
    private List<BookingPackingRequest> packingList;
    @JsonProperty("routings")
    private List<BookingRoutingsRequest> routingList;
    @JsonProperty("charges")
    private List<PlatformBookingChargesRequest> bookingCharges;
    private String serviceMode;
    @JsonProperty("parentContractId")
    private String parentContractId;
    @JsonProperty("primarySalesAgentEmail")
    private String primarySalesAgentEmail;
    @JsonProperty("secondarySalesAgentEmail")
    private String secondarySalesAgentEmail;
    @JsonProperty("salesBranch")
    private String salesBranch;
    @JsonProperty("minTransitHours")
    private String minTransitHours;
    @JsonProperty("maxTransitHours")
    private String maxTransitHours;
    @JsonProperty("isDg")
    private Boolean isDg;
    private String shipmentReferenceNumber;
    private List<NotesRequest> notesList;
    private String integrationSource;
}
