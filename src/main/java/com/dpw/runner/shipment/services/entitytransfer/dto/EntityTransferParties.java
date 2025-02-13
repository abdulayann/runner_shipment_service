package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferParties implements IEntityTranferBaseEntity {
    private String type;
    private String orgCode;
    private String addressCode;
    private String orgId;
    private String addressId;
    private Map<String, Object> orgData;
    private Map<String, Object> addressData;
    private String countryCode;
}
