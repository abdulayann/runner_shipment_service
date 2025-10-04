package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionResponse implements IRunnerResponse {

    private Long id;
    private String status;
    private String bookingStatus;
    private String bookingNo;
    private PartiesResponse contract;
    private PartiesResponse shipper;
    private PartiesResponse consignee;
    private PartiesResponse forwardingAgent;
    private PartiesResponse requestor;
    private List<PartiesResponse> additionalParties;
    private BigDecimal shipperDeclaredValue;
    private String shipperDeclaredValueCurrency;
    private String blReleaseOffice;
    private LocalDateTime dateOfIssue;
    private String carrierBlNo;
    private String carrierBookingNo;
    private String entityType;
    private Long entityId;
    private String entityNumber;
    private String blComments;
    private Integer noOfFreightCopies;
    private Integer noOfUnFreightCopies;
    private Integer nonNegoFreightCopies;
    private Integer nonNegoUnFreightCopies;
    private ShippingInstructionType shippingInstructionType;
    private String serviceType;
    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private List<FreightDetailResponse> freightDetailList;
    private List<CommonPackageResponse> commonPackagesList;
    private List<CommonContainerResponse> commonContainersList;
    private List<ReferenceNumberResponse> referenceNumbers;
    private SailingInformationResponse sailingInformation;
    private List<ShippingInstructionContainerWarningResponse> containerDiff;
    private List<ShippingInstructionContainerWarningResponse> packageDiff;
    private String createdBy;
    private LocalDateTime createdAt;
    private String updatedBy;
    private LocalDateTime updatedAt;
    private String createByUserEmail;
    private String submitByUserEmail;
    private String comments;
    private String vgmStatus;
}
