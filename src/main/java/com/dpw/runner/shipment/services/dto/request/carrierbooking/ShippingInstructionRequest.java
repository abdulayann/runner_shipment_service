package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionRequest implements Serializable {
    private Long id;
    private String status;
    private String carrierBlNo;
    private String carrierBookingNo;
    private EntityType entityType;
    @NotNull(message = "Entity Id can not be null")
    @Min(value = 1, message = "Entity id can not be zero/negative")
    private Long entityId;
    private String entityNumber;
    @NotBlank(message = "Service type can not be empty")
    private String serviceType;
    private BigDecimal shipperDeclaredValue;
    private String shipperDeclaredValueCurrency;
    private String blReleaseOffice;
    private LocalDateTime dateOfIssue;
    @NotNull(message = "Shipping instruction can not be null")
    private ShippingInstructionType shippingInstructionType;
    @Size(max = 10000, message = "BL Comments cannot exceed 10,000 characters")
    private String blComments;
    private Integer noOfFreightCopies;
    private Integer noOfUnFreightCopies;
    private Integer nonNegoFreightCopies;
    private Integer nonNegoUnFreightCopies;
    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private String otherInternalEmails;
    private String otherExternalEmails;
    private PartiesRequest contract;
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    private PartiesRequest forwardingAgent;
    private PartiesRequest notifyParty;
    private PartiesRequest requestor;
    private List<PartiesRequest> additionalParties;
    private List<FreightDetailRequest> freightDetailList;
    private List<CommonPackageRequest> commonPackagesList;
    private List<CommonContainerRequest> commonContainersList;
    private List<ReferenceNumberRequest> referenceNumbers;
    private SailingInformationRequest sailingInformation;
}

