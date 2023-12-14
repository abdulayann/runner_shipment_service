package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Notify Party Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbNotifyPartyInfo {
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
