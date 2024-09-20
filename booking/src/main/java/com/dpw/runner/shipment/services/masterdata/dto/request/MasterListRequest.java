package com.dpw.runner.shipment.services.masterdata.dto.request;

import lombok.*;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MasterListRequest {
    public String ItemValue;
    public String ItemType;
    public String Cascade;
}
