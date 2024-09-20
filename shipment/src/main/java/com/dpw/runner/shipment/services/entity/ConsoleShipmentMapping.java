package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import lombok.*;

import javax.persistence.*;

@Entity
@Getter
@Setter
@Table(name = "console_shipment_mapping")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConsoleShipmentMapping extends BaseEntity {
    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "is_attachment_done")
    private Boolean isAttachmentDone;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "request_type")
    private ShipmentRequestedType requestedType;
}
