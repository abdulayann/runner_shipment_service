package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Notify Party Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbNotifyPartyInfo implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String type;
    private String orgCode;
    //TODO - Please Move this to parties org - Tapan
    private Integer orgId;
    private Long notifyOrgId;
    private String name;
    private String address;
    private Integer addressId;
    @MasterData(type = MasterDataType.COUNTRIES)
    private String country;
    private String countryName;
    @MasterData(type = MasterDataType.COUNTRY_STATES, cascade = Constants.AWB_COUNTRY)
    private String state;
    private String city;
    private String zipCode;
    private String phone;
    private Boolean isShipmentCreated;
    private UUID guid;
}
