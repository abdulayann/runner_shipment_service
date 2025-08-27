package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionResponse {

    private Long id;

    private String status;

    private PartiesResponse contract;
    private PartiesResponse shipper;
    private PartiesResponse consignee;
    private PartiesResponse forwardingAgent;
    private List<PartiesResponse> additionalParties;

    private String mblNo;
    private String carrierBookingNo;
    private String entityType;
    private Long entityId;
    private String entityNumber;
    private String blComments;

    private List<FreightDetailResponse> freightDetails;
    private List<CommonPackageResponse> commonPackages;
    private List<CommonContainerResponse> commonContainers;


}
