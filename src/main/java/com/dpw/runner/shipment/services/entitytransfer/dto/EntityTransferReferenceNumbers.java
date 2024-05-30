package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferReferenceNumbers implements IEntityTranferBaseEntity {
    public String countryOfIssue;
    public String type;
    public String referenceNumber;
    private Map<String, EntityTransferMasterLists> masterData;
}