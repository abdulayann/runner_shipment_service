package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

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

    @Column(name = "request_type")
    private Integer requestedType;
}
