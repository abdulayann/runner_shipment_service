package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;

@Data
@ApiModel("Awb Shipment Info Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbShipmentInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private String shipperName;
    private String shipperAddress;
    private String shipperAddress2;
    @MasterData(type = MasterDataType.COUNTRIES)
    private String shipperCountry;
    private String shipperCountryName;
    @MasterData(type = MasterDataType.COUNTRY_STATES, cascade = Constants.SHIPPER_COUNTRY)
    private String shipperState;
    private String shipperCity;
    private String shipperZipCode;
    private String shipperPhone;
    private String shipperReferenceNumber;
    private String shipperContactName;
    private String shipperTaxRegistrationNumber;
    private OtherPartyInfo shipperPartyInfo;
    private String consigneeName;
    private String consigneeAddress;
    private String consigneeAddress2;
    @MasterData(type = MasterDataType.COUNTRIES)
    private String consigneeCountry;
    private String consigneeCountryName;
    @MasterData(type = MasterDataType.COUNTRY_STATES, cascade = Constants.CONSIGNEE_COUNTRY)
    private String consigneeState;
    private String consigneeCity;
    private String consigneeZipCode;
    private String consigneePhone;
    private String consigneeReferenceNumber;
    private String consigneeContactName;
    private String consigneeTaxRegistrationNumber;
    private OtherPartyInfo consigneePartyInfo;
    private String issuingAgentName;
    private String issuingAgentAddress;
    private String issuingAgentAddress2;
    @MasterData(type = MasterDataType.COUNTRIES)
    private String issuingAgentCountry;
    private String issuingAgentCountryName;
    @MasterData(type = MasterDataType.COUNTRY_STATES, cascade = Constants.ISSUING_AGENT_COUNTRY)
    private String issuingAgentState;
    private String issuingAgentCity;
    private String issuingAgentZipCode;
    private String issuingAgentPhone;
    private String issuingAgentContactName;
    private String issuingAgentTaxRegistrationNumber;
    private OtherPartyInfo issuingAgentPartyInfo;
    private String iataCode;
    private String noOfCopies;
    private String status;
    private String awbNumber;
    @UnlocationData
    private String originAirport;
    @UnlocationData
    private String destinationAirport;
    private String firstCarrier;
    private Integer abrId;
    private String agentCASSCode;
    private String shipperAccountNumber;
    private String consigneeAccountNumber;
    private String accountNumber;
}
