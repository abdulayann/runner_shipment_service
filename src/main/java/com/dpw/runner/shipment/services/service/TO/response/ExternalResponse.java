package com.dpw.runner.shipment.services.service.TO.response;

import com.dpw.runner.shipment.services.entity.enums.StatusType;
import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ExternalResponse {
    public String tid;
    public StatusType status = StatusType.SUBMITTED;
    public String  message;
    public DescartesResponse descartesResponse;
    public String xmlPayload;
}
