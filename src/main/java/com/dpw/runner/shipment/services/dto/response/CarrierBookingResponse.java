package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Carrier Booking Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingResponse implements IRunnerResponse {
    private Long id;
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
    private CarrierDetailRequest carrierDetails;
    private String placeOfCarrierReceipt;
    private String placeOfCarrierDelivery;
    private String requestedTransshipment;
    private String prohibitedTransshipment;
    private LocalDateTime latesetDeliveryDate;
    private LocalDateTime earliestDepartureDate;
    private boolean isLinked;
    private boolean isTempControlled;
    private String shipperRefNumber;
    private String contractNumber;
    private String purchaseOrderNumber;
    private UUID consolidationGuid;
    private boolean isOverride;
    private String referenceNo;
    private List<PartiesRequest> consolidationAddresses;
    private boolean isSplitBooking;
    private String parentBookingNumber;
    private String splitBookingSequence;
    private String carrierComments;
    private String customerComments;
    private List<BookingCarriageResponse> bookingCarriagesList;
    private List<BookingPaymentResponse> bookingPaymentsList;
    private List<ContainerResponse> containersList;
    private List<PackingResponse> packingList;
    private List<OrderInfoResponse> orderIds;
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
    private boolean isLocked;
}
