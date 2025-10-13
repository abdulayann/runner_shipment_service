package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingListResponse implements IRunnerResponse {
    private Long id;
    private String type= CarrierBookingConstants.CARRIER_BOOKING_TYPE;
    private String status;
    private String bookingNo;
    private Integer tenantId;
    private String carrierBookingNo;
    private String carrierBlNo;
    private Long entityId;
    private String entityNumber;
    private String entityType;
    private String contractNo;
    private String serviceType;
    private String bookingOffice;
    private String bookingComment;
    private String carrierComment;
    // Relations
    private PartiesResponse requester;
    private PartiesResponse shipper;
    private PartiesResponse consignee;
    private PartiesResponse forwardingAgent;
    private List<PartiesResponse> additionalParties;
    private PartiesResponse pickupFrom;
    private PartiesResponse deliveryTo;
    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private String otherInternalEmails;
    private String otherExternalEmails;
    private ShippingInstructionStatus siStatus;
    private VerifiedGrossMassStatus vgmStatus;
    private SailingInformationResponse sailingInformation;

    private Map<String, String> tenantMasterData;
    private String createdBy;
    private LocalDateTime createdAt;
    private String updatedBy;
    private LocalDateTime updatedAt;
    private String createByUserEmail;
    private String submitByUserEmail;
}
