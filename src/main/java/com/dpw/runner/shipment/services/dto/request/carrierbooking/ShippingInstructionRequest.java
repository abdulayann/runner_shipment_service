package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionEntityType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionRequest {
    private String status;
    private String mblNo;
    private String carrierBookingNo;
    private ShippingInstructionEntityType entityType;
    private Long entityId;
    private String entityNumber;
    private String blComments;

    private PartiesRequest contract;
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    private PartiesRequest forwardingAgent;

    private List<PartiesRequest> additionalParties;
    private List<FreightDetailRequest> freightDetails;
    private List<CommonPackageRequest> commonPackages;
    private List<CommonContainerRequest> commonContainers;
    private List<ReferenceNumberRequest> referenceNumberRequests;
    private SailingInformationRequest sailingInformationRequest;
}

