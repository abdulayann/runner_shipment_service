package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "shipment_order")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class ShipmentOrder extends BaseEntity {
    @Column(name = "order_guid")
    private UUID orderGuid;

    @Column(name = "order_number")
    private String orderNumber;

    @Column(name = "shipment_id")
    private Long shipmentId;
}
