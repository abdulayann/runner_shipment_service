package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import org.hibernate.annotations.Generated;
import org.hibernate.annotations.GenerationTime;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Data
@Table(name = "sequence_incrementor")
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE sequence_incrementor SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class SequenceIncrementor extends BaseEntity {

    @Generated(GenerationTime.INSERT)
    @Column(name = "shipment_increment_id")
    Integer shipmentIncrementId;

    @Column(name = "entity_id")
    Long entityId;
}
