package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IMasterDataBaseEntity;
import lombok.*;

import java.util.Map;
@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PartiesData implements IMasterDataBaseEntity {
    private String type;
    private String orgCode;
    private String addressCode;
    private Map<String, Object> orgData;
    private Map<String, Object> addressData;
}
