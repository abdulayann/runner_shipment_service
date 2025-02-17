package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "shipments_containers_mapping")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ShipmentsContainersMapping extends BaseEntity {
    @Column(name = "container_id")
    private Long containerId;

    @Column(name = "shipment_id")
    private Long shipmentId;
}
