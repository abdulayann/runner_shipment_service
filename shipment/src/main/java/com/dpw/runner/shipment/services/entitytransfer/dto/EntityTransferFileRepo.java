package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferFileRepo implements IEntityTranferBaseEntity {
    public String fileName;
    public String path;
    public String docType;
    public Boolean clientEnabled;
    public Boolean isPosted;
    public String eventCode;
}