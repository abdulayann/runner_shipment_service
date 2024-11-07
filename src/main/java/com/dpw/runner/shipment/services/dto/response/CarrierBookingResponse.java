package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Carrier Booking Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private CarrierBookingStatus status;
    private ShippingInstructionResponse shippingInstruction;
    private String bookingId;
    private String bookingNumber;
    private String bol;
    private String forwarderRefNumber;
    private Integer sourceTenantId;
    private String transportMode;
    private String serviceType;
    private String bookingOffice;
    private String modeOfBooking;
    private CarrierDetailResponse carrierDetails;
    private String placeOfCarrierReceipt;
    private String placeOfCarrierDelivery;
    private String requestedTransshipment;
    private String prohibitedTransshipment;
    private LocalDateTime latesetDeliveryDate;
    private LocalDateTime earliestDepartureDate;
    private Boolean isLinked;
    private Boolean isTempControlled;
    private String shipperRefNumber;
    private String contractNumber;
    private String purchaseOrderNumber;
    private UUID consolidationGuid;
    private Boolean isOverride;
    private String referenceNo;
    private List<PartiesResponse> consolidationAddresses;
    private Boolean isSplitBooking;
    private String parentBookingNumber;
    private String splitBookingSequence;
    private String carrierComments;
    private String customerComments;
    private List<BookingCarriageResponse> bookingCarriagesList;
    private List<BookingPaymentResponse> bookingPaymentsList;
    private List<ContainerResponse> containersList;
    private List<PackingResponse> packingList;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private LocalDateTime estimatedTerminalCutoff;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime bookingCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime latestFullEquDeliveredToCarrier;
    private LocalDateTime earliestDropOffFullEquToCarrier;
    private LocalDateTime earliestEmptyEquPickUp;
    private List<EventsResponse> eventsList;
    private Integer lockedBy;
    private Boolean isLocked;
    private Map<String, String> masterData;
}
