package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "container_charges_mapping")
@AllArgsConstructor
@NoArgsConstructor
public class ContainerChargesMapping extends BaseEntity {
    @Column(name = "container_id")
    private Long containerId;

    @Column(name = "charge_id")
    private Long chargeId;
}
