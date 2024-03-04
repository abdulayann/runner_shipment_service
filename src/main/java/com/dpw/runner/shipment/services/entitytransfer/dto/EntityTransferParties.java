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
    public String type;
    public String orgCode;
    public String addressCode;
    private Map<String, Object> orgData;
    private Map<String, Object> addressData;
}
