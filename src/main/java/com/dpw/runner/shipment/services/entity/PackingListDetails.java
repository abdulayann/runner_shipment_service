package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Data
@Setter
@Getter
@Table(name = "container_packs_mapping")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class PackingListDetails extends BaseEntity {
    @Column(name = "container_id")
    private Long containerId;

    @Column(name = "pack_id")
    private Long packId;
}
