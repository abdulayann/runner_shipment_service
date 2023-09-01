package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import javax.persistence.*;

@Entity
@Getter
@Setter
@Table(name = "shipments_containers_mapping")
@AllArgsConstructor
@NoArgsConstructor
public class ShipmentsContainersMapping extends BaseEntity {
        @Column(name = "container_id")
        private Long containerId;

        @Column(name = "shipment_id")
        private Long shipmentId;
}
