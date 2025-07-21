package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;


@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferNotes implements IEntityTranferBaseEntity {
    private String text;
    private Long entityId;
    private String entityType;
    private String insertUserDisplayName;
    private Boolean isPublic;
    private Boolean isActive;
    private String label;
    private String assignedTo;
    private Boolean isReadOnly;
}
