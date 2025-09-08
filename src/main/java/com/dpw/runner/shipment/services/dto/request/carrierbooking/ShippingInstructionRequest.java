package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionEntityType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionRequest {
    private String status;
    private String carrierBlNo;
    private String carrierBookingNo;
    private ShippingInstructionEntityType entityType;
    private Long entityId;
    private String entityNumber;
    private String blComments;
    private Integer noOfFreightCopies;
    private Integer noOfUnFreightCopies;
    private Integer nonNegoFreightCopies;
    private Integer nonNegoUnFreightCopies;

    private PartiesRequest contract;
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    private PartiesRequest forwardingAgent;

    private List<PartiesRequest> additionalParties;
    private List<FreightDetailRequest> freightDetailList;
    private List<CommonPackageRequest> commonPackagesList;
    private List<CommonContainerRequest> commonContainersList;
    private List<ReferenceNumberRequest> referenceNumbers;
    private SailingInformationRequest sailingInformation;
}

