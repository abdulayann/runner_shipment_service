package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

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
    private Integer orgId;
    private Long notifyOrgId;
    private String name;
    private String address;
    private Integer addressId;
}
