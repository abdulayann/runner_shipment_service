package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionInttraRequest implements IRunnerResponse {
    private String messageStatus;
    private LocalDateTime dateTime; //202508180550 format
    private List<FreightDetailResponse> freightDetailList;
    private BigDecimal shipperDeclaredValue;
    private String shipperDeclaredValueCurrency;
    private String blReleaseOffice;
    private LocalDateTime dateOfIssue;
    private String carrierBookingNo;
    private String entityNumber;
    private String blComments;
    private Integer totalNoOfPackages;
    private Double totalGrossWeight;
    private Double totalGrossVolume;
    private Integer totalNumberOfEquipments; //figure out how to fill
    private SailingInformationResponse sailingInformation;
    private ShippingInstructionType shippingInstructionType;
    private Integer noOfFreightCopies;
    private Integer noOfUnFreightCopies;
    private PartiesResponse shipper;
    private PartiesResponse consignee;
    private PartiesResponse requestor;
    private PartiesResponse forwardingAgent;
    List<CommonContainerResponse> commonContainersList;
    List<CommonPackageResponse> commonPackagesList;
    private String carrierScacCode;
    private String carrierDescription;
}
