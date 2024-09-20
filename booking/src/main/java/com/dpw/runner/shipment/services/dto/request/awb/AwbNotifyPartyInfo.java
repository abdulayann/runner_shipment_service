package com.dpw.runner.shipment.services.dto.request.awb;

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
public class AwbNotifyPartyInfo implements Serializable {
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
    private Boolean isShipmentCreated;
    private UUID guid;
}
